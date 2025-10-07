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
    // Процессы
    FProcessSyncthing: TProcessUTF8;
    FProcessSupport: TProcessUTF8;

    // Конфигурация
    FHomePath: UTF8String;
    FExecPath: UTF8String;
    FConfigPath: UTF8String;
    FConfigData: TJSONObject;

    // Состояния процессов
    FProcessState: TProcessState;

    // Таймеры
    FTimerReadOutput: TTimer;
    FTimerCheckProcess: TTimer;

    // Буфер вывода
    FOutputBuffer: UTF8String;

    // События
    FOnProcessStateChanged: TProcessEvent;
    FOnConsoleOutput: TConsoleOutputEvent;

    // Внутренние методы
    procedure InitializeProcesses;
    procedure InitializeTimers;
    procedure ConfigureProcesses;

    procedure TimerReadOutputTimer(Sender: TObject);
    procedure TimerCheckProcessTimer(Sender: TObject);

    procedure ProcessTerminated(Sender: TObject);
    procedure ReadProcessOutput;
    procedure ProcessOutputLine(const Line: UTF8String);
    procedure ProcessStateChanged(NewState: TProcessState);
    procedure AddToConsole(const Text: UTF8String);

  protected
    // Переопределенные методы TSyncthingAPI
    procedure SetEndpoint(const Host: UTF8String; Port: Integer; UseTLS: Boolean); override;
    procedure SetAPIKey(const Key: UTF8String); override;

  public
    // Конструктор/Деструктор
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Конфигурация
    function ReadAPIKeyFromConfig: string; virtual;
    function ReadConfigFromFile: TJSONObject; virtual;
    procedure LoadConfigFromDisk; virtual;

    procedure SetHomePath(const Path: UTF8String); virtual;
    procedure SetExecPath(const Path: UTF8String); virtual;

    // Управление процессами
    function StartSyncthingProcess: Boolean; virtual;
    function StartSupportProcess: Boolean; virtual;
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


implementation

uses
  Forms,
  ulogging,
  DateUtils,
  process,
  TypInfo,
  LCLTranslator, LConvEncoding;

// Константы для таймаутов процессов
const
  PROCESS_START_TIMEOUT_MS = 5000;  // Максимальное время ожидания запуска процесса (мс)
  PROCESS_CHECK_INTERVAL_MS = 100;  // Интервал проверки состояния процесса (мс)
  PROCESS_CHECK_ITERATIONS = PROCESS_START_TIMEOUT_MS div PROCESS_CHECK_INTERVAL_MS; // Количество итераций

{ TSyncthingManager }

constructor TSyncthingManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Инициализация состояний
  FProcessState := psUnknown;
  FConfigData := nil;

  // Создание процессов
  InitializeProcesses;

  // Создание таймеров
  InitializeTimers;

  // Загрузка конфигурации по умолчанию
  LoadConfigFromDisk;

  // Чтение API-ключа из конфигурации
  if FHomePath <> '' then
  begin
    SetAPIKey(ReadAPIKeyFromConfig);
  end;
end;

destructor TSyncthingManager.Destroy;
begin
  // Остановка всех процессов перед уничтожением
  StopAllProcesses;

  // Освобождение ресурсов процессов
  if Assigned(FProcessSyncthing) then
    FreeAndNil(FProcessSyncthing);

  if Assigned(FProcessSupport) then
    FreeAndNil(FProcessSupport);

  // Освобождение конфигурации
  if Assigned(FConfigData) then
    FreeAndNil(FConfigData);

  // Освобождение таймеров
  if Assigned(FTimerReadOutput) then
    FreeAndNil(FTimerReadOutput);

  if Assigned(FTimerCheckProcess) then
    FreeAndNil(FTimerCheckProcess);

  inherited Destroy;
end;

procedure TSyncthingManager.InitializeProcesses;
begin
  // Создание основного процесса Syncthing
  FProcessSyncthing := TProcessUTF8.Create(nil);
  FProcessSyncthing.Options := [poUsePipes];
  FProcessSyncthing.ShowWindow := swoHIDE;

  // Создание вспомогательного процесса
  FProcessSupport := TProcessUTF8.Create(nil);
  FProcessSupport.Options := [poNoConsole];
  FProcessSupport.ShowWindow := swoHIDE;

  ConfigureProcesses;
end;

procedure TSyncthingManager.InitializeTimers;
begin
  // Таймер чтения вывода процессов
  FTimerReadOutput := TTimer.Create(Self);
  FTimerReadOutput.Enabled := False;
  FTimerReadOutput.Interval := 100; // 100ms
  FTimerReadOutput.OnTimer := @TimerReadOutputTimer;

  // Таймер проверки состояния процессов
  FTimerCheckProcess := TTimer.Create(Self);
  FTimerCheckProcess.Enabled := False;
  FTimerCheckProcess.Interval := 1000; // 1s
  FTimerCheckProcess.OnTimer := @TimerCheckProcessTimer;
end;

procedure TSyncthingManager.ConfigureProcesses;
begin
  // Конфигурация процессов будет выполнена в FillSyncthingExecPath
end;

procedure TSyncthingManager.SetEndpoint(const Host: UTF8String; Port: Integer; UseTLS: Boolean);
begin
  inherited SetEndpoint(Host, Port, UseTLS);

  // Синхронизация с настройками процессов
  if FHomePath <> '' then
    ConfigureProcesses;
end;

procedure TSyncthingManager.SetAPIKey(const Key: UTF8String);
begin
  inherited SetAPIKey(Key);

  // API-ключ также сохраняется локально для записи в конфигурацию
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
  // Проверка состояния процессов
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

  // Если состояние изменилось, уведомляем
  if NewState <> FProcessState then
    ProcessStateChanged(NewState);
end;


procedure TSyncthingManager.ProcessTerminated(Sender: TObject);
begin
  // Процесс завершился, обновляем состояние
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

      // Добавляем к буферу
      FOutputBuffer := FOutputBuffer + Line;

      // Разбиваем на строки
      p := Pos(LE, FOutputBuffer);
      while p <> 0 do
      begin
        LineStr := Copy(FOutputBuffer, 1, p - 1);
        FOutputBuffer := Copy(FOutputBuffer, p + Length(LE), Length(FOutputBuffer) - p);

        // Конвертируем кодировку
        LineStr := ConvertEncodingToUTF8(LineStr, GetConsoleTextEncoding(), conv_ok);

        // Обрабатываем строку
        ProcessOutputLine(LineStr);

        p := Pos(LE, FOutputBuffer);
      end;
    end;
  end;
end;

procedure TSyncthingManager.ProcessOutputLine(const Line: UTF8String);
begin
  // Добавляем в консоль
  AddToConsole(Line);

  // Вызываем событие
  if Assigned(FOnConsoleOutput) then
    FOnConsoleOutput(Self, Line);
end;

procedure TSyncthingManager.ProcessStateChanged(NewState: TProcessState);
var
  OldState: TProcessState;
begin
  OldState := FProcessState;
  FProcessState := NewState;

  // Логируем смену состояния
  DebugLog(Format('Process state changed: %s -> %s',
    [GetEnumName(TypeInfo(TProcessState), Ord(OldState)),
     GetEnumName(TypeInfo(TProcessState), Ord(NewState))]));

  // Останавливаем/запускаем таймеры в зависимости от состояния
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

  // Вызываем событие
  if Assigned(FOnProcessStateChanged) then
    FOnProcessStateChanged(Self, FProcessState);
end;

function TSyncthingManager.ReadAPIKeyFromConfig: string;
var
  filename: UTF8String;
  FDoc: TXMLDocument;
  NPtr: TDOMNode;
begin
  Result := '';

  if FHomePath = '' then
    Exit;

  filename := FHomePath + PathDelim + 'config.xml';

  if not FileExistsUTF8(filename) then
    Exit;

  try
    FDoc := nil;
    ReadXMLFile(FDoc, UTF8ToSys(filename));

    NPtr := FDoc;
    if GetXmlNode(NPtr, 'configuration') and
       GetXmlNode(NPtr, 'gui') and
       GetXmlNode(NPtr, 'apikey') and
       GetXmlNode(NPtr, '#text') then
    begin
      Result := UTF8String(NPtr.NodeValue);
    end;
  finally
    if Assigned(FDoc) then
      FDoc.Free;
  end;

  // Если не нашли в файле, возвращаем пустую строку
  // API-ключ должен передаваться извне через SetAPIKey
end;

function TSyncthingManager.ReadConfigFromFile: TJSONObject;
var
  filename: UTF8String;
  FDoc: TXMLDocument;
  ConfigStr: UTF8String;
begin
  Result := nil;

  if FHomePath = '' then
    Exit;

  filename := FHomePath + PathDelim + 'config.xml';

  if not FileExistsUTF8(filename) then
    Exit;

  try
    FDoc := nil;
    ReadXMLFile(FDoc, UTF8ToSys(filename));

    // Для простоты конвертируем XML в JSON (в реальности может быть сложнее)
    // Это упрощенная версия - в будущем можно реализовать полный парсер
    ConfigStr := '<config>Конфигурация загружена</config>';

    if ConfigStr <> '' then
      Result := TJSONObject(GetJSON(ConfigStr));
  finally
    if Assigned(FDoc) then
      FDoc.Free;
  end;
end;

procedure TSyncthingManager.LoadConfigFromDisk;
begin
  // Загружаем пути из параметров (должны быть установлены извне)
  // SetHomePath и SetExecPath должны вызываться до этого метода

  // Читаем API-ключ из конфигурации
  if FHomePath <> '' then
  begin
    SetAPIKey(ReadAPIKeyFromConfig);
  end;

  // Загружаем дополнительные настройки конфигурации
  if FHomePath <> '' then
    FConfigData := ReadConfigFromFile;
end;

procedure TSyncthingManager.SetHomePath(const Path: UTF8String);
begin
  if FHomePath <> Path then
  begin
    FHomePath := Path;
    FConfigPath := FHomePath + PathDelim + 'config.xml';

    // Переконфигурируем процессы при изменении пути
    ConfigureProcesses;

    DebugLog('Home path changed to: ' + FHomePath);
  end;
end;

procedure TSyncthingManager.SetExecPath(const Path: UTF8String);
begin
  if FExecPath <> Path then
  begin
    FExecPath := Path;

    // Переконфигурируем процессы при изменении пути к исполняемому файлу
    ConfigureProcesses;

    DebugLog('Exec path changed to: ' + FExecPath);
  end;
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
    // Обычный запуск без исправления бага (отключено директивой)
    FProcessSyncthing.Executable := FExecPath;
    FProcessSyncthing.Parameters.Text :=
      '--no-browser ' +
      '--home=' + FHomePath;
    {$ENDIF}
    {$ELSE}
    // Linux/macOS версия
    FProcessSyncthing.Executable := FExecPath;
    FProcessSyncthing.Parameters.Clear;
    FProcessSyncthing.Parameters.Add('--home=' + FHomePath);
    FProcessSyncthing.Parameters.Add('--no-browser');
    {$ENDIF}

    // Запускаем процесс
    FProcessSyncthing.Execute;

    // Ждем запуска процесса с фиксированным количеством итераций
    for I := 1 to PROCESS_CHECK_ITERATIONS do
    begin
      if FProcessSyncthing.Running then
        Break; // Процесс запустился, выходим из цикла

      Sleep(PROCESS_CHECK_INTERVAL_MS);
      Application.ProcessMessages;
    end;

    if FProcessSyncthing.Running then
    begin
      DebugLog('Syncthing process started successfully');
      ProcessStateChanged(psRunning);

      // Подключаемся к API после успешного запуска процесса
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

function TSyncthingManager.StartSupportProcess: Boolean;
begin
  Result := False;

  if FHomePath = '' then
  begin
    DebugLog('Cannot start support process: home path not set');
    Exit;
  end;

  {$IFDEF WINDOWS}
  FProcessSupport.Executable := FExecPath; // Предполагаем, что есть syncthing-inotify.exe рядом
  FProcessSupport.Parameters.Text := '--home=' + FHomePath;
  {$ELSE}
  // Для Linux может потребоваться другая утилита
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
    // Сначала пытаемся graceful shutdown через API
    if IsOnline then
    begin
      // Отправляем команду shutdown через REST API
      API_Get('system/shutdown', nil, '');
      Sleep(2000); // Ждем 2 секунды
    end;

    // Если процесс все еще работает, принудительно завершаем
    if FProcessSyncthing.Running then
      FProcessSyncthing.Terminate(0);

    // Ждем завершения
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
  // Останавливаем основной процесс
  StopSyncthingProcess;

  // Останавливаем вспомогательный процесс
  if FProcessSupport.Running then
  begin
    FProcessSupport.Terminate(0);
    DebugLog('Support process stopped');
  end;

  Result := not FProcessSyncthing.Running and not FProcessSupport.Running;
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

end.
