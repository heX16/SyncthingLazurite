unit uSyncthingManager;

{$mode objfpc}{$H+}

// Директива компилятора для отключения исправления бага Windows с выводом процессов
// Определите в проекте для отключения cmd.exe перенаправления stdout/stderr:
// {$DEFINE DISABLE_WINDOWS_OUTPUT_FIX}
// По умолчанию исправление ВКЛЮЧЕНО для совместимости с Windows

interface

uses
  Classes, SysUtils, UTF8Process, ExtCtrls,
  syncthing_api, fpjson, XMLRead, DOM, LazUTF8, LazFileUtils;

// Вспомогательная функция для поиска XML узлов
function GetXmlNode(var Node: TDOMNode; Name: WideString): boolean;

type
  // Состояния процессов Syncthing
  TProcessState = (
    psUnknown,           // Неизвестное состояние
    psStopped,          // Процессы остановлены
    psStarting,         // Запуск процессов
    psRunning,          // Процессы запущены и работают
    psStopping,         // Остановка процессов
    psError            // Ошибка процесса
  );

  // События процесса
  TProcessEvent = procedure(Sender: TObject; State: TProcessState) of object;
  TConsoleOutputEvent = procedure(Sender: TObject; const Line: UTF8String) of object;

  { TSyncthingManager }

  TSyncthingManager = class(TSyncthingAPI)
  private
    FProcessSyncthing: TProcessUTF8;

    FHomePath: UTF8String;
    FExecPath: UTF8String;

    FProcessState: TProcessState;

    FTimerReadOutput: TTimer;
    FTimerCheckProcess: TTimer;

    FOutputBuffer: UTF8String;

    FOnProcessStateChanged: TProcessEvent;
    FOnConsoleOutput: TConsoleOutputEvent;
    procedure InitializeProcesses;
    procedure InitializeTimers;

    procedure TimerReadOutputTimer(Sender: TObject);
    procedure TimerCheckProcessTimer(Sender: TObject);

    procedure ProcessTerminated(Sender: TObject);
    procedure ReadProcessOutput;
    procedure ProcessOutputLine(const Line: UTF8String);
    procedure ProcessStateChanged(NewState: TProcessState);
    procedure AddToConsole(const Text: UTF8String);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetConfigPath: UTF8String;
    procedure ExtractConfigSettings;
    procedure LoadConfigFromDisk; virtual;

    procedure SetHomePath(const Path: UTF8String); virtual;
    procedure SetExecPath(const Path: UTF8String); virtual;

    function StartSyncthingProcess: Boolean; virtual;
    function StopSyncthingProcess: Boolean; virtual;
    function StopAllProcesses: Boolean; virtual;

    function IsProcessRunning: Boolean; virtual;
    function GetProcessInfo: string; virtual;

    // Свойства
    property HomePath: UTF8String read FHomePath write SetHomePath;
    property ExecPath: UTF8String read FExecPath write SetExecPath;
    property ProcessState: TProcessState read FProcessState;

    // События
    property OnProcessStateChanged: TProcessEvent read FOnProcessStateChanged write FOnProcessStateChanged;
    property OnConsoleOutput: TConsoleOutputEvent read FOnConsoleOutput write FOnConsoleOutput;

  end;

  { TSyncthingManagerWithSupport }
  // Extended version with support process functionality
  // Preserved for potential future use

  TSyncthingManagerWithSupport = class(TSyncthingManager)
  private
    FProcessSupport: TProcessUTF8;
    procedure InitializeSupportProcess;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function StartSupportProcess: Boolean; virtual;
    function StopAllProcesses: Boolean; override;
  end;


implementation

uses
  Forms,
  ulogging,
  DateUtils,
  process,
  TypInfo,
  LCLTranslator, LConvEncoding;

const
  PROCESS_START_TIMEOUT_MS = 5000;
  PROCESS_CHECK_INTERVAL_MS = 100;
  PROCESS_CHECK_ITERATIONS = PROCESS_START_TIMEOUT_MS div PROCESS_CHECK_INTERVAL_MS;

{ TSyncthingManager }

constructor TSyncthingManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FProcessState := psUnknown;

  InitializeProcesses;
  InitializeTimers;
  LoadConfigFromDisk;
end;

destructor TSyncthingManager.Destroy;
begin
  StopAllProcesses;

  if Assigned(FProcessSyncthing) then
    FreeAndNil(FProcessSyncthing);

  if Assigned(FTimerReadOutput) then
    FreeAndNil(FTimerReadOutput);

  if Assigned(FTimerCheckProcess) then
    FreeAndNil(FTimerCheckProcess);

  inherited Destroy;
end;

procedure TSyncthingManager.InitializeProcesses;
begin
  FProcessSyncthing := TProcessUTF8.Create(nil);
  FProcessSyncthing.Options := [poUsePipes];
  FProcessSyncthing.ShowWindow := swoHIDE;
end;

procedure TSyncthingManager.InitializeTimers;
begin
  FTimerReadOutput := TTimer.Create(Self);
  FTimerReadOutput.Enabled := False;
  FTimerReadOutput.Interval := 100;
  FTimerReadOutput.OnTimer := @TimerReadOutputTimer;

  FTimerCheckProcess := TTimer.Create(Self);
  FTimerCheckProcess.Enabled := False;
  FTimerCheckProcess.Interval := 1000;
  FTimerCheckProcess.OnTimer := @TimerCheckProcessTimer;
end;


procedure TSyncthingManager.TimerReadOutputTimer(Sender: TObject);
begin
  ReadProcessOutput;
end;

procedure TSyncthingManager.TimerCheckProcessTimer(Sender: TObject);
var
  NewState: TProcessState;
begin
  NewState := TProcessState.psUnknown;
  if FProcessSyncthing.Running then
  begin
    if FProcessState = psStarting then
      NewState := psRunning
    else if FProcessState <> psRunning then
      NewState := psRunning;
  end
  else if FProcessState in [psRunning, psStarting] then
  begin
    NewState := psStopped;
  end
  else
    NewState := FProcessState;

  if NewState <> FProcessState then
    ProcessStateChanged(NewState);
end;


procedure TSyncthingManager.ProcessTerminated(Sender: TObject);
begin
  if Sender = FProcessSyncthing then
  begin
    if FProcessState = psRunning then
      ProcessStateChanged(psStopped);
  end;
end;

procedure TSyncthingManager.ReadProcessOutput;
var
  Line: RawByteString;
  BytesRead: LongInt;
  p: Integer;
  LineStr: UTF8String;
  conv_ok: boolean;
const
  LE = LineEnding;
begin
  if not Assigned(FProcessSyncthing) or not FProcessSyncthing.Running then
    Exit;

  if Assigned(FProcessSyncthing.Output) then
  begin
    BytesRead := FProcessSyncthing.Output.NumBytesAvailable;
    if BytesRead > 0 then
    begin
      SetLength(Line, BytesRead);
      FProcessSyncthing.Output.ReadBuffer(Line[1], BytesRead);

      FOutputBuffer := FOutputBuffer + Line;

      // Разбиваем на строки
      p := Pos(LE, FOutputBuffer);
      while p <> 0 do
      begin
        LineStr := Copy(FOutputBuffer, 1, p - 1);
        FOutputBuffer := Copy(FOutputBuffer, p + Length(LE), Length(FOutputBuffer) - p);

        LineStr := ConvertEncodingToUTF8(LineStr, GetConsoleTextEncoding(), conv_ok);

        ProcessOutputLine(LineStr);

        p := Pos(LE, FOutputBuffer);
      end;
    end;
  end;
end;

procedure TSyncthingManager.ProcessOutputLine(const Line: UTF8String);
begin
  // TODO: AddToConsole??? - зачем это надо? у нас уже есть OnConsoleOutput
  AddToConsole(Line);

  if Assigned(FOnConsoleOutput) then
    FOnConsoleOutput(Self, Line);
end;

procedure TSyncthingManager.ProcessStateChanged(NewState: TProcessState);
var
  OldState: TProcessState;
begin
  OldState := FProcessState;
  FProcessState := NewState;

  DebugLog(Format('Process state changed: %s -> %s',
    [GetEnumName(TypeInfo(TProcessState), Ord(OldState)),
     GetEnumName(TypeInfo(TProcessState), Ord(NewState))]));

  case FProcessState of
    psStarting, psRunning:
      begin
        FTimerReadOutput.Enabled := True;
        FTimerCheckProcess.Enabled := True;
      end;
    psStopped, psError:
      begin
        FTimerReadOutput.Enabled := False;
        FTimerCheckProcess.Enabled := False;
      end;
  end;

  if Assigned(FOnProcessStateChanged) then
    FOnProcessStateChanged(Self, FProcessState);
end;


procedure TSyncthingManager.ExtractConfigSettings;
var
  filename: UTF8String;
  FDoc: TXMLDocument;
  NPtr: TDOMNode;
  APIKeyValue: UTF8String;
begin
  if FHomePath = '' then
    Exit;

  filename := GetConfigPath;
  if not FileExistsUTF8(filename) then
    Exit;

  try
    FDoc := nil;
    ReadXMLFile(FDoc, UTF8ToSys(filename));

    if FDoc = nil then Exit;

    NPtr := FDoc;

    if GetXmlNode(NPtr, 'configuration') and
       GetXmlNode(NPtr, 'gui') and
       GetXmlNode(NPtr, 'apikey') and
       GetXmlNode(NPtr, '#text') then
    begin
      APIKeyValue := UTF8String(NPtr.NodeValue);
      if APIKeyValue <> '' then
      begin
        DebugLog('API key loaded from config.xml');
        SetAPIKey(APIKeyValue);
      end;
    end;

  finally
    if Assigned(FDoc) then
      FDoc.Free;
  end;
end;

function TSyncthingManager.GetConfigPath: UTF8String;
begin
  if FHomePath = '' then
    Result := ''
  else
    Result := FHomePath + PathDelim + 'config.xml';
end;

procedure TSyncthingManager.LoadConfigFromDisk;
begin
  if FHomePath <> '' then
  begin
    ExtractConfigSettings;
  end;
end;

procedure TSyncthingManager.SetHomePath(const Path: UTF8String);
begin
  FHomePath := Path;
end;

procedure TSyncthingManager.SetExecPath(const Path: UTF8String);
begin
  FExecPath := Path;
end;

function TSyncthingManager.StartSyncthingProcess: Boolean;
var
  StartTime: TDateTime;
  i: Integer;
begin
  Result := False;

  if FExecPath = '' then
  begin
    DebugLog('Cannot start Syncthing: executable path not set');
    ProcessStateChanged(psError);
    Exit;
  end;

  if FProcessState = psRunning then
  begin
    DebugLog('Syncthing process already running');
    Exit(True);
  end;

  ProcessStateChanged(psStarting);

  try
    {$IFDEF WINDOWS}
    {$IFNDEF DISABLE_WINDOWS_OUTPUT_FIX}
    // Используем cmd.exe для перенаправления вывода (исправление бага Windows)
    // Включено по умолчанию для совместимости
    FProcessSyncthing.Executable := GetEnvironmentVariableUTF8('WINDIR') + '\system32\cmd.exe';
    FProcessSyncthing.Parameters.Text :=
      '/C "' + FExecPath + ' ' +
      '--no-browser ' +
      '--home=' + FHomePath + ' ' +
      '| find /v " empty_find_just_for_redirect_stdio_00686558 " "';
    {$ELSE}
    FProcessSyncthing.Executable := FExecPath;
    FProcessSyncthing.Parameters.Text :=
      '--no-browser ' +
      '--home=' + FHomePath;
    {$ENDIF}
    {$ELSE}
    FProcessSyncthing.Executable := FExecPath;
    FProcessSyncthing.Parameters.Clear;
    FProcessSyncthing.Parameters.Add('--home=' + FHomePath);
    FProcessSyncthing.Parameters.Add('--no-browser');
    {$ENDIF}

    FProcessSyncthing.Execute;

    for I := 1 to PROCESS_CHECK_ITERATIONS do
    begin
      if FProcessSyncthing.Running then
        Break;

      Sleep(PROCESS_CHECK_INTERVAL_MS);
      Application.ProcessMessages;
    end;

    if FProcessSyncthing.Running then
    begin
      DebugLog('Syncthing process started successfully');
      ProcessStateChanged(psRunning);

      Connect;
      Result := True;
    end
    else
    begin
      DebugLog('Failed to start Syncthing process. Exit code: ' + IntToStr(FProcessSyncthing.ExitCode));
      ProcessStateChanged(psError);
    end;

  except
    on E: Exception do
    begin
      DebugLog('Exception while starting Syncthing: ' + E.Message);
      ProcessStateChanged(psError);
    end;
  end;
end;

function TSyncthingManager.StopSyncthingProcess: Boolean;
begin
  Result := False;

  if not FProcessSyncthing.Running then
  begin
    DebugLog('Syncthing process not running');
    Exit(True);
  end;

  ProcessStateChanged(psStopping);

  try
    if IsOnline then
    begin
      API_Get('system/shutdown', nil, '');
      Sleep(2000);
    end;

    if FProcessSyncthing.Running then
      FProcessSyncthing.Terminate(0);

    Sleep(1000);

    if not FProcessSyncthing.Running then
    begin
      DebugLog('Syncthing process stopped successfully');
      ProcessStateChanged(psStopped);
      Disconnect; // Отключаемся от API
      Result := True;
    end
    else
    begin
      DebugLog('Failed to stop Syncthing process');
      ProcessStateChanged(psError);
    end;

  except
    on E: Exception do
    begin
      DebugLog('Exception while stopping Syncthing: ' + E.Message);
      ProcessStateChanged(psError);
    end;
  end;
end;

function TSyncthingManager.StopAllProcesses: Boolean;
begin
  StopSyncthingProcess;
  Result := not FProcessSyncthing.Running;
end;

function TSyncthingManager.IsProcessRunning: Boolean;
begin
  Result := FProcessSyncthing.Running;
end;

function TSyncthingManager.GetProcessInfo: string;
begin
  Result := Format('Process: %s, State: %s, Running: %s',
    [FExecPath,
     GetEnumName(TypeInfo(TProcessState), Ord(FProcessState)),
     BoolToStr(FProcessSyncthing.Running, True)]);
end;

procedure TSyncthingManager.AddToConsole(const Text: UTF8String);
begin
  if Assigned(FOnConsoleOutput) then
    FOnConsoleOutput(Self, Text);
end;

// Вспомогательная функция для поиска XML узлов
function GetXmlNode(var Node: TDOMNode; Name: WideString): boolean;
var
  N: TDOMNode;
begin
  Result := False;
  if Node <> nil then
  begin
    N := Node.FindNode(Name);
    if N <> nil then
    begin
      Node := N;
      Result := True;
    end;
  end;
end;

{ TSyncthingManagerWithSupport }

constructor TSyncthingManagerWithSupport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeSupportProcess;
end;

destructor TSyncthingManagerWithSupport.Destroy;
begin
  if Assigned(FProcessSupport) then
    FreeAndNil(FProcessSupport);
  inherited Destroy;
end;

procedure TSyncthingManagerWithSupport.InitializeSupportProcess;
begin
  FProcessSupport := TProcessUTF8.Create(nil);
  FProcessSupport.Options := [poNoConsole];
  FProcessSupport.ShowWindow := swoHIDE;
end;

function TSyncthingManagerWithSupport.StartSupportProcess: Boolean;
begin
  Result := False;

  if HomePath = '' then
  begin
    DebugLog('Cannot start support process: home path not set');
    Exit;
  end;

  {$IFDEF WINDOWS}
  FProcessSupport.Executable := ExecPath;
  FProcessSupport.Parameters.Text := '--home=' + HomePath;
  {$ELSE}
  {$ENDIF}

  try
    FProcessSupport.Execute;
    Result := FProcessSupport.Running;
    if Result then
      DebugLog('Support process started successfully')
    else
      DebugLog('Failed to start support process');
  except
    on E: Exception do
    begin
      DebugLog('Exception while starting support process: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TSyncthingManagerWithSupport.StopAllProcesses: Boolean;
begin
  inherited StopAllProcesses;

  if FProcessSupport.Running then
  begin
    FProcessSupport.Terminate(0);
    DebugLog('Support process stopped');
  end;

  Result := not IsProcessRunning and not FProcessSupport.Running;
end;

end.
