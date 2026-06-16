unit uSyncthingManager;

{$mode objfpc}{$H+}

// Compiler directive to disable the Windows process output bugfix.
// Define in the project to disable cmd.exe stdout/stderr redirection:
{$DEFINE DISABLE_WINDOWS_OUTPUT_FIX}
// By default the fix is ENABLED for Windows compatibility.

interface

uses
  Classes, SysUtils, UTF8Process, AsyncProcess, ExtCtrls,
  syncthing_api, XMLRead, DOM, LazUTF8, LazFileUtils;

// Helper function for finding XML nodes
function GetXmlNode(var Node: TDOMNode; Name: WideString): boolean;

type
  // Syncthing process states
  TProcessState = (
    psUnknown,           // Unknown state
    psStopped,          // Processes stopped
    psStarting,         // Starting processes
    psRunning,          // Processes running
    psStopping,         // Stopping processes
    psError            // Process error
  );

  // Process events
  TProcessEvent = procedure(Sender: TObject; State: TProcessState) of object;
  TConsoleOutputEvent = procedure(Sender: TObject; const Line: UTF8String) of object;

  // Kinds of process errors to provide more context in ProcessStateChanged
  TProcessErrorKind = (
    pekNone,                 // No error
    pekStartTimeout,         // Process did not start within timeout
    pekStartExecuteException,// Exception while calling Execute()
    pekStartConfigError,     // Start failed due to configuration error (e.g. missing exec path)
    pekStopFailed,           // Process did not stop after shutdown/terminate
    pekStopException         // Exception while stopping process
  );

  // Information about a failed process start
  TStartFailureInfo = record
    ErrorMessage: UTF8String;  // Exception text or empty
    ExecPath: UTF8String;      // Intended executable path
    IsException: Boolean;      // True if the error came from an Execute exception
    OSLastError: LongInt;      // Last OS error code (if any)
    Process: TAsyncProcess;    // Process instance
  end;

  // Start failure event
  TStartFailedEvent = procedure(Sender: TObject; const Info: TStartFailureInfo) of object;

  { TSyncthingManager }

  TSyncthingManager = class(TSyncthingAPI)
  private
    FProcessSyncthing: TAsyncProcess;

    FHomePath: UTF8String;
    FExecPath: UTF8String;

    // Invariant: FProcessState must be changed only inside ProcessStateChanged();
    // do not assign FProcessState directly in other methods.
    FProcessState: TProcessState;

    FTimerCheckProcess: TTimer;

    // Startup helper: number of timer ticks left before we treat start as failed
    FStartChecksRemaining: Integer;

    FOutputBuffer: RawByteString;

    FOnProcessStateChanged: TProcessEvent;
    FOnConsoleOutput: TConsoleOutputEvent;
    FOnStartFailed: TStartFailedEvent;
    procedure InitializeProcesses;
    procedure InitializeTimers;

    procedure ProcessReadData(Sender: TObject);
    procedure TimerCheckProcessTimer(Sender: TObject);

    procedure ProcessTerminated(Sender: TObject);
    procedure ReadProcessOutput;
    procedure ProcessOutputLine(const Line: UTF8String); virtual;
    procedure ProcessStateChanged(NewState: TProcessState;
      AErrorKind: TProcessErrorKind = pekNone;
      const AErrorMessage: UTF8String = ''); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetConfigPath: UTF8String;
    procedure ExtractConfigSettings;
    procedure LoadConfigFromDisk; virtual;

    procedure SetHomePath(const Path: UTF8String); virtual;
    procedure SetExecPath(const Path: UTF8String); virtual;

    procedure StartSyncthingProcess; virtual;
    function StopSyncthingProcess: Boolean; virtual;
    function StopAllProcesses: Boolean; virtual;

    function IsProcessRunning: Boolean; virtual;

    // Properties
    property HomePath: UTF8String read FHomePath write SetHomePath;
    property ExecPath: UTF8String read FExecPath write SetExecPath;
    property ProcessState: TProcessState read FProcessState;

    // Events
    property OnProcessStateChanged: TProcessEvent read FOnProcessStateChanged write FOnProcessStateChanged;
    property OnConsoleOutput: TConsoleOutputEvent read FOnConsoleOutput write FOnConsoleOutput;
    property OnStartFailed: TStartFailedEvent read FOnStartFailed write FOnStartFailed;

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

  // Timer intervals in milliseconds
  PROCESS_STATE_CHECK_INTERVAL_MS = 1000;   // Process state check interval

  // Wait times in milliseconds
  API_SHUTDOWN_WAIT_MS = 2000;              // Wait after API shutdown call
  PROCESS_TERMINATE_WAIT_MS = 1000;         // Wait after Terminate

{ TSyncthingManager }

constructor TSyncthingManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FProcessState := psUnknown;

  InitializeProcesses;
  InitializeTimers;
end;

destructor TSyncthingManager.Destroy;
begin
  StopAllProcesses;

  if Assigned(FProcessSyncthing) then
    FreeAndNil(FProcessSyncthing);

  if Assigned(FTimerCheckProcess) then
    FreeAndNil(FTimerCheckProcess);

  inherited Destroy;
end;

procedure TSyncthingManager.InitializeProcesses;
begin
  FProcessSyncthing := TAsyncProcess.Create(nil);
  FProcessSyncthing.Options := [poUsePipes, poStderrToOutPut];
  FProcessSyncthing.ShowWindow := swoHIDE;
  // Subscribe to events: data ready and process terminated
  FProcessSyncthing.OnReadData := @ProcessReadData; // use read handler
  FProcessSyncthing.OnTerminate := @ProcessTerminated;
end;

procedure TSyncthingManager.InitializeTimers;
begin
  FTimerCheckProcess := TTimer.Create(Self);
  FTimerCheckProcess.Enabled := False;
  FTimerCheckProcess.Interval := PROCESS_STATE_CHECK_INTERVAL_MS;
  FTimerCheckProcess.OnTimer := @TimerCheckProcessTimer;
end;


procedure TSyncthingManager.ProcessReadData(Sender: TObject);
begin
  ReadProcessOutput;
end;

procedure TSyncthingManager.TimerCheckProcessTimer(Sender: TObject);
var
  NewState: TProcessState;
  Info: TStartFailureInfo;
begin
  // Timer is used only during startup phase (psStarting)
  if FProcessState <> psStarting then
  begin
    FTimerCheckProcess.Enabled := False;
    Exit;
  end;

  // If process is running at OS level, promote to psRunning and stop timer
  if FProcessSyncthing.Running then
  begin
    ProcessStateChanged(psRunning);
    Exit;
  end;

  // Process is not running yet, track timeout
  if FStartChecksRemaining > 0 then
  begin
    Dec(FStartChecksRemaining);
    if FStartChecksRemaining = 0 then
    begin
      // Mark startup timeout error; ProcessStateChanged will log details
      ProcessStateChanged(psError, pekStartTimeout,
        'Process did not start within timeout');

      if Assigned(FOnStartFailed) then
      begin
        FillChar(Info, SizeOf(Info), 0);
        Info.ErrorMessage := '';
        Info.ExecPath := FExecPath;
        Info.IsException := False;
        Info.OSLastError := GetLastOSError;
        Info.Process := FProcessSyncthing;
        FOnStartFailed(Self, Info);
      end;
    end;
  end;
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
  Chunk: RawByteString;
  BytesRead: LongInt;
  p: Integer;
  RawLine: RawByteString;
  UtfLine: UTF8String;
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
      SetLength(Chunk, BytesRead);
      FProcessSyncthing.Output.ReadBuffer(Chunk[1], BytesRead);

      FOutputBuffer := FOutputBuffer + Chunk;

      // Split buffer into lines using the system line separator
      p := Pos(LE, FOutputBuffer);
      while p <> 0 do
      begin
        RawLine := Copy(FOutputBuffer, 1, p - 1);
        FOutputBuffer := Copy(FOutputBuffer, p + Length(LE), Length(FOutputBuffer) - p);

        UtfLine := ConvertEncodingToUTF8(RawLine, GetConsoleTextEncoding(), conv_ok);
        if not conv_ok then
        begin
          DebugLog('Failed to convert console output line to UTF-8; passing raw bytes as UTF-8');
          UtfLine := UTF8String(RawLine);
        end;

        ProcessOutputLine(UtfLine);

        p := Pos(LE, FOutputBuffer);
      end;
    end;
  end;
end;

procedure TSyncthingManager.ProcessOutputLine(const Line: UTF8String);
begin
  if Assigned(FOnConsoleOutput) then
    FOnConsoleOutput(Self, Line);
end;

procedure TSyncthingManager.ProcessStateChanged(NewState: TProcessState;
  AErrorKind: TProcessErrorKind; const AErrorMessage: UTF8String);
var
  OldState: TProcessState;
  TailUtf: UTF8String;
  conv_ok: boolean;
begin
  OldState := FProcessState;
  DebugLog(Format('Process state changed: %s -> %s',
    [GetEnumName(TypeInfo(TProcessState), Ord(OldState)),
     GetEnumName(TypeInfo(TProcessState), Ord(NewState))]));
  FProcessState := NewState;

  case FProcessState of
    psStarting:
      begin
        // Initialize startup timeout counter and enable timer
        FStartChecksRemaining := PROCESS_START_TIMEOUT_MS div PROCESS_STATE_CHECK_INTERVAL_MS;
        FTimerCheckProcess.Enabled := True;
      end;
    psRunning:
      begin
        // Startup finished, timer no longer needed
        FTimerCheckProcess.Enabled := False;
        // Log successful start with PID when we transition from starting phase
        if (OldState = psStarting) and Assigned(FProcessSyncthing) and FProcessSyncthing.Running then
          DebugLog(Format('Syncthing process started successfully (PID=%d)', [FProcessSyncthing.ProcessID]));
        // Automatically connect to Syncthing API after process is running
        try
          if not IsOnline then
            Connect;
        except
          on E: Exception do
            DebugLog('Exception while auto-connecting after process start: ' + E.Message);
        end;
      end;
    psStopped, psError:
      begin
        FTimerCheckProcess.Enabled := False;
        // Flush remaining buffer as the final line if any data is left
        if Length(FOutputBuffer) > 0 then
        begin
          // Try to convert and emit the tail as a line
          // Important: avoid losing data when there is no trailing line break
          TailUtf := ConvertEncodingToUTF8(FOutputBuffer, GetConsoleTextEncoding(), conv_ok);
          if not conv_ok then
          begin
            DebugLog('Failed to convert trailing console output to UTF-8; passing raw bytes as UTF-8');
            TailUtf := UTF8String(FOutputBuffer);
          end;
          ProcessOutputLine(TailUtf);
          FOutputBuffer := '';
        end;

        // Log additional diagnostics for error states based on error kind
        if (FProcessState = psError) then
        begin
          case AErrorKind of
            pekStartTimeout:
              begin
                DebugLog('Failed to start Syncthing process. Process is not running after timeout.');
                DebugLog('Start diagnostics: ' +
                  Format('Executable="%s"; Params="%s"; OSLastError=%d (%s); ExitCode=%d',
                    [FProcessSyncthing.Executable,
                     UTF8String(FProcessSyncthing.Parameters.Text),
                     GetLastOSError,
                     UTF8String(SysErrorMessage(GetLastOSError)),
                     FProcessSyncthing.ExitCode]));
              end;
            pekStartExecuteException, pekStartConfigError,
            pekStopFailed, pekStopException:
              begin
                if AErrorMessage <> '' then
                  DebugLog('Process error details: ' + AErrorMessage);
              end;
          else
            ; // no extra diagnostics
          end;
        end;
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

procedure TSyncthingManager.StartSyncthingProcess;
var
  Info: TStartFailureInfo;
  {$IFDEF WINDOWS}
  {$IFNDEF DISABLE_WINDOWS_OUTPUT_FIX}
  InnerCmd: UTF8String;
  {$ENDIF}
  {$ENDIF}
begin

  if FExecPath = '' then
  begin
    DebugLog('Cannot start Syncthing: executable path not set');
    ProcessStateChanged(psError, pekStartConfigError,
      'Executable path not set');
    Exit;
  end;

  if FProcessState = psRunning then
  begin
    DebugLog('Syncthing process already running');
    Exit;
  end;

  ProcessStateChanged(psStarting);

  try
    {$IFDEF WINDOWS}
    {$IFNDEF DISABLE_WINDOWS_OUTPUT_FIX}
    // Windows + output fix: launch via cmd.exe and pass the full command as a single second parameter
    FProcessSyncthing.Executable := GetEnvironmentVariableUTF8('WINDIR') + '\system32\cmd.exe';
    FProcessSyncthing.Parameters.Clear;
    FProcessSyncthing.Parameters.Add('/C');
    InnerCmd := '"' + FExecPath + '" --no-browser --home="' +
      FHomePath +
      '" | find /v " empty_find_just_for_redirect_stdio_00686558 "';
    FProcessSyncthing.Parameters.Add(InnerCmd);
    {$ELSE}
    // Windows without fix: launch directly with separate parameters
    FProcessSyncthing.Executable := FExecPath;
    FProcessSyncthing.Parameters.Clear;
    FProcessSyncthing.Parameters.Add('--no-browser');
    FProcessSyncthing.Parameters.Add('--home=' + FHomePath);
    {$ENDIF}
    {$ELSE}
    FProcessSyncthing.Executable := FExecPath;
    FProcessSyncthing.Parameters.Clear;
    FProcessSyncthing.Parameters.Add('--no-browser');
    FProcessSyncthing.Parameters.Add('--home=' + FHomePath);
    {$ENDIF}

    FProcessSyncthing.Execute;

  except
    on E: Exception do
    begin
      DebugLog('Exception while starting Syncthing: ' + E.Message);
      ProcessStateChanged(psError, pekStartExecuteException,
        UTF8String(E.Message));
      if Assigned(FOnStartFailed) then
      begin
        FillChar(Info, SizeOf(Info), 0);
        Info.ErrorMessage := UTF8String(E.Message);
        Info.ExecPath := FExecPath;
        Info.IsException := True;
        Info.OSLastError := GetLastOSError;
        Info.Process := FProcessSyncthing;
        FOnStartFailed(Self, Info);
      end;
    end;
  end;
end;

function TSyncthingManager.StopSyncthingProcess: Boolean;
var
  err: Integer;
begin
  Result := False;

  if not FProcessSyncthing.Running then
  begin
    DebugLog('Syncthing process not running');
    ProcessStateChanged(psStopped);
    Exit(True);
  end;

  ProcessStateChanged(psStopping);

  try
    if IsOnline then
    begin
      API_Post('system/shutdown', nil, '');
      Sleep(API_SHUTDOWN_WAIT_MS);
    end;

    if FProcessSyncthing.Running then
      FProcessSyncthing.Terminate(0);

    Sleep(PROCESS_TERMINATE_WAIT_MS);

    if not FProcessSyncthing.Running then
    begin
      DebugLog('Syncthing process stopped successfully');
      ProcessStateChanged(psStopped);
      Disconnect; // Disconnect from API
      Result := True;
    end
    else
    begin
      DebugLog('Failed to stop Syncthing process');
      err := GetLastOSError;
      ProcessStateChanged(
        psError,
        pekStopFailed,
        'Process did not stop after terminate, OSLastError=' + UTF8String(IntToStr(err))
      );
    end;

  except
    on E: Exception do
    begin
      DebugLog('Exception while stopping Syncthing: ' + E.Message);
      ProcessStateChanged(psError, pekStopException, UTF8String(E.Message));
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





// Helper function for finding XML nodes
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
  FProcessSupport.Parameters.Clear;
  FProcessSupport.Parameters.Add('--home=' + HomePath);
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
