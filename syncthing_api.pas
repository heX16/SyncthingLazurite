unit syncthing_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpjson,
  AsyncHttp,
  ExtCtrls;

type
  // Finite state machine for the core
  TSyncthingState = (
    ssOffline,        // No active connection; idle/offline state
    ssConnecting,     // Connecting to Syncthing and performing initial sync
    ssOnline,         // Connected and operational; long-polling is active
    ssDisconnecting   // Graceful disconnect in progress
  );

  // Action to take after a long-polling drop
  TLongPollingDropAction = (
    lpdaDefault,  // Use default internal strategy (e.g., retry with backoff)
    lpdaRetry,    // Explicitly keep retrying to restore long-polling
    lpdaAbort     // Abort session and transition to offline
  );

  // Event types (callbacks)
  TNotifyEventObj = procedure(Sender: TObject) of object;
  TConnectErrorEvent = procedure(Sender: TObject; const MessageText: UTF8String) of object;
  TEventJsonEvent = procedure(Sender: TObject; Event: TJSONObject) of object;
  // Fired when a branch in the in-memory JSON tree was modified; Path is a JSON path
  TTreeChangedEvent = procedure(Sender: TObject; const Path: UTF8String) of object;
  TBeforeAfterTreeModifyEvent = procedure(Sender: TObject) of object;
  TLongPollingDropHandler = function(Sender: TObject): TLongPollingDropAction of object;

!!!
для унификации можно добавить типы:
  - массив массивов (таблица, двумерный массив) с перечислением endpoints в котором указаны пути, обработчики (для большиства одинаковые), и чтото еще если требуется
  - enum с перечислением endpoints


  { TSyncthingApiV2 }

  // Core class for interacting with Syncthing (REST + Event API) and maintaining JSON tree
  TSyncthingApiV2 = class(TComponent)
  private
    FState: TSyncthingState;
    FRoot: TJSONObject;            // Single in-memory JSON tree root
    FAPIKey: UTF8String;           // X-API-Key
    FHost: UTF8String;             // Syncthing host
    FPort: Integer;                // Syncthing port
    FUseTLS: Boolean;              // Use HTTPS if true
    FServerURL: UTF8String;        // Built base URL (scheme://host:port/), see `BuildServerURL`

    FHTTP: TAsyncHTTP;             // REST client
    FHTTPEvents: TAsyncHTTP;       // Long-polling client

    FEventsLastId: Int64;          // Last seen event id
    FLongPollingRestartIntervalSec: Integer; // Periodic restart interval
    FLongPollingRestartTimer: TTimer;

    // Events (callbacks)
    FOnBeforeConnect: TNotifyEventObj;
    FOnConnected: TNotifyEventObj;
    FOnConnectError: TConnectErrorEvent;
    FOnBeforeDisconnect: TNotifyEventObj;
    FOnDisconnectedByUser: TNotifyEventObj;
    FOnHardDisconnect: TNotifyEventObj;
    FOnLongPollingDrop: TLongPollingDropHandler;
    FOnBeforeLongPollingRestart: TNotifyEventObj;
    FOnEvent: TEventJsonEvent;
    FOnTreeChanged: TTreeChangedEvent;
    FOnBeforeTreeModify: TBeforeAfterTreeModifyEvent;
    FOnAfterTreeModify: TBeforeAfterTreeModifyEvent;
    // Internal helpers
    procedure HttpAddHeader(Request: THttpRequest; Sender: TObject);
    procedure EventsDisconnected(Request: THttpRequest; Sender: TObject);
    function ParseJson(Request: THttpRequest; out Json: TJSONData): boolean;
    procedure SetAtPath(var RootObj: TJSONObject; const JsonPath: UTF8String; NewData: TJSONData);
    // REST callbacks
    !!! rename to CB_CheckOnline
    procedure CB_Ping(Request: THttpRequest);

    !!!: я думаю все эти функции можно унифицировать до одной функции.
    procedure CB_SystemConfig(Request: THttpRequest);
    procedure CB_SystemConnections(Request: THttpRequest);
    procedure CB_StatsDevice(Request: THttpRequest);
    procedure CB_StatsFolder(Request: THttpRequest);
    procedure CB_SystemStatus(Request: THttpRequest);
    procedure CB_SystemVersion(Request: THttpRequest);
    // Events callbacks
    procedure CB_Events(Request: THttpRequest);
    // REST helper
    procedure API_Get(const Api: UTF8String; Callback: THttpRequestCallbackFunction; const QueueKey: UTF8String = '');

  protected
    { Builds base server URL from host/port and scheme }
    procedure BuildServerURL; virtual;
    { Creates and configures HTTP clients for REST and long-polling }
    procedure ConfigureHttpClients; virtual;

    // JSON tree helpers
    { Creates default JSON root object used before initial sync }
    function CreateDefaultRoot: TJSONObject; virtual;
    { Notifies listeners and prepares to modify the JSON tree }
    procedure BeginTreeModify; virtual;
    { Notifies listeners and finalizes JSON tree modification }
    procedure EndTreeModify; virtual;
    { Replaces a branch at JsonPath with NewData in the root tree }
    procedure ReplaceRootBranch(const JsonPath: UTF8String; NewData: TJSONData); virtual;

    // Initial sync via REST API
    { Performs initial synchronization by loading required REST resources }
    procedure PerformInitialSync; virtual;
    { Loads /rest/config (or equivalent) into the tree }
    procedure LoadSystemConfig; virtual;
    { Loads /rest/system/connections into the tree }
    procedure LoadSystemConnections; virtual;
    { Loads /rest/stats/device into the tree }
    procedure LoadStatsDevice; virtual;
    { Loads /rest/stats/folder into the tree }
    procedure LoadStatsFolder; virtual;
    { Loads /rest/system/status into the tree }
    procedure LoadSystemStatus; virtual;
    { Loads /rest/system/version into the tree }
    procedure LoadSystemVersion; virtual;

    // Long-polling lifecycle
    { Starts event long-polling listener }
    procedure StartLongPolling; virtual;
    { Stops event long-polling listener }
    procedure StopLongPolling; virtual;
    { Restarts event long-polling listener (periodic or on drop) }
    procedure RestartLongPolling; virtual;
    { Timer handler to periodically restart long-polling }
    procedure LongPollingRestartTimerHandler(Sender: TObject); virtual;

    // Long-polling processing
    { Parses and processes events array returned by Event API }
    procedure HandleLongPollingResponse(EventsArray: TJSONArray); virtual;
    { Dispatches single event to user handler before internal integration }
    procedure ProcessEvent(EventObj: TJSONObject); virtual;
    { Integrates single event data into the JSON tree }
    procedure IntegrateEvent(EventObj: TJSONObject); virtual;
    { Notifies listeners that a tree branch at Path was changed }
    procedure NotifyTreeChanged(const Path: UTF8String); virtual;

  public
    { Creates the core object without starting any network activity }
    constructor Create(AOwner: TComponent); override;
    { Frees resources and detaches clients }
    destructor Destroy; override;

    // Connection control
    { Connects to a running Syncthing instance and performs initial sync }
    procedure Connect; virtual;
    { Gracefully disconnects and stops long-polling }
    procedure Disconnect; virtual;

    // Configuration
    { Sets endpoint (host, port, TLS flag) and rebuilds base URL }
    procedure SetEndpoint(const Host: UTF8String; Port: Integer; UseTLS: Boolean); virtual;
    { Sets X-API-Key for authenticated requests }
    procedure SetAPIKey(const Key: UTF8String); virtual;
    { Sets periodic restart interval for long-polling (0 to disable) }
    procedure SetLongPollingRestartInterval(Seconds: Integer); virtual;

    // State and data
    { Returns true when FSM is online }
    function IsOnline: Boolean; virtual;
    { Current finite state machine state }
    property State: TSyncthingState read FState;
    { In-memory JSON root with all synchronized data }
    property Root: TJSONObject read FRoot;
    { Last seen event id (from Event API) }
    property EventsLastId: Int64 read FEventsLastId;

    // Callbacks
    { Called before starting connection procedure }
    property OnBeforeConnect: TNotifyEventObj read FOnBeforeConnect write FOnBeforeConnect;
    { Called after successful connection and initial sync }
    property OnConnected: TNotifyEventObj read FOnConnected write FOnConnected;
    { Called if connection or initial sync fails }
    property OnConnectError: TConnectErrorEvent read FOnConnectError write FOnConnectError;
    { Called before starting disconnect procedure }
    property OnBeforeDisconnect: TNotifyEventObj read FOnBeforeDisconnect write FOnBeforeDisconnect;
    { Called after user-requested disconnect finishes }
    property OnDisconnectedByUser: TNotifyEventObj read FOnDisconnectedByUser write FOnDisconnectedByUser;
    { Called when connection is lost and recovery failed }
    property OnHardDisconnect: TNotifyEventObj read FOnHardDisconnect write FOnHardDisconnect;
    { Called when long-polling drops; return action to take }
    property OnLongPollingDrop: TLongPollingDropHandler read FOnLongPollingDrop write FOnLongPollingDrop;
    { Called before periodic long-polling restart }
    property OnBeforeLongPollingRestart: TNotifyEventObj read FOnBeforeLongPollingRestart write FOnBeforeLongPollingRestart;
    { Called on every received raw event (before integration) }
    property OnEvent: TEventJsonEvent read FOnEvent write FOnEvent;
    { Called after integration when a JSON tree path was updated }
    property OnTreeChanged: TTreeChangedEvent read FOnTreeChanged write FOnTreeChanged;
    { Called before modifying the JSON tree }
    property OnBeforeTreeModify: TBeforeAfterTreeModifyEvent read FOnBeforeTreeModify write FOnBeforeTreeModify;
    { Called after modifying the JSON tree }
    property OnAfterTreeModify: TBeforeAfterTreeModifyEvent read FOnAfterTreeModify write FOnAfterTreeModify;
  end;

implementation
uses
  jsonparser;

{ TSyncthingApiV2 }

constructor TSyncthingApiV2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := ssOffline;
  FRoot := CreateDefaultRoot;
  FAPIKey := '';
  FHost := '127.0.0.1';
  FPort := 8384;
  FUseTLS := False;
  FServerURL := '';
  FHTTP := nil;
  FHTTPEvents := nil;
  FEventsLastId := 0;
  FLongPollingRestartIntervalSec := 0;
  FLongPollingRestartTimer := nil;
end;

destructor TSyncthingApiV2.Destroy;
begin
  if Assigned(FLongPollingRestartTimer) then
    FreeAndNil(FLongPollingRestartTimer);
  if Assigned(FHTTP) then
    FreeAndNil(FHTTP);
  if Assigned(FHTTPEvents) then
    FreeAndNil(FHTTPEvents);
  if Assigned(FRoot) then
    FreeAndNil(FRoot);
  inherited Destroy;
end;

procedure TSyncthingApiV2.BuildServerURL;
begin
  if FUseTLS then
    FServerURL := Format('https://%s:%d/', [FHost, FPort])
  else
    FServerURL := Format('http://%s:%d/', [FHost, FPort]);
end;

procedure TSyncthingApiV2.ConfigureHttpClients;
begin
  if not Assigned(FHTTP) then
    FHTTP := TAsyncHTTP.Create;
  FHTTP.ConnectTimeout := 1000;
  FHTTP.RetryCount := 1;
  FHTTP.IOTimeout := 1000;
  FHTTP.KeepConnection := True;
  FHTTP.OnOpened := @HttpAddHeader;

  if not Assigned(FHTTPEvents) then
    FHTTPEvents := TAsyncHTTP.Create;
  FHTTPEvents.ConnectTimeout := 1000;
  FHTTPEvents.RetryCount := 0;
  FHTTPEvents.IOTimeout := 61000;
  FHTTPEvents.KeepConnection := True;
  FHTTPEvents.OnOpened := @HttpAddHeader;
  FHTTPEvents.OnDisconnected := @EventsDisconnected;

  if (FLongPollingRestartIntervalSec > 0) then
  begin
    if not Assigned(FLongPollingRestartTimer) then
    begin
      FLongPollingRestartTimer := TTimer.Create(Self);
      FLongPollingRestartTimer.Enabled := False;
      FLongPollingRestartTimer.OnTimer := @LongPollingRestartTimerHandler;
    end;
    FLongPollingRestartTimer.Interval := FLongPollingRestartIntervalSec * 1000;
  end
  else if Assigned(FLongPollingRestartTimer) then
    FLongPollingRestartTimer.Enabled := False;
end;

function TSyncthingApiV2.CreateDefaultRoot: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

procedure TSyncthingApiV2.BeginTreeModify;
begin
  if Assigned(FOnBeforeTreeModify) then
    FOnBeforeTreeModify(Self);
end;

procedure TSyncthingApiV2.EndTreeModify;
begin
  if Assigned(FOnAfterTreeModify) then
    FOnAfterTreeModify(Self);
end;

procedure TSyncthingApiV2.ReplaceRootBranch(const JsonPath: UTF8String; NewData: TJSONData);
begin
  if not Assigned(FRoot) then
    FRoot := CreateDefaultRoot;
  BeginTreeModify;
  try
    SetAtPath(FRoot, StringReplace(JsonPath, '/', '.', [rfReplaceAll]), NewData);
    NotifyTreeChanged(JsonPath);
  finally
    EndTreeModify;
  end;
end;

procedure TSyncthingApiV2.PerformInitialSync;
begin !!!
  API_Get('config', @CB_SystemConfig, 'config');
  API_Get('system/connections', @CB_SystemConnections, 'system/connections');
  API_Get('stats/device', @CB_StatsDevice, 'stats/device');
  API_Get('stats/folder', @CB_StatsFolder, 'stats/folder');
  API_Get('system/status', @CB_SystemStatus, 'system/status');
  API_Get('system/version', @CB_SystemVersion, 'system/version');
end;

procedure TSyncthingApiV2.LoadSystemConfig;
begin
  API_Get('config', @CB_SystemConfig, 'config');
end;

procedure TSyncthingApiV2.LoadSystemConnections;
begin
  API_Get('system/connections', @CB_SystemConnections, 'system/connections');
end;

procedure TSyncthingApiV2.LoadStatsDevice;
begin
  API_Get('stats/device', @CB_StatsDevice, 'stats/device');
end;

procedure TSyncthingApiV2.LoadStatsFolder;
begin
  API_Get('stats/folder', @CB_StatsFolder, 'stats/folder');
end;

procedure TSyncthingApiV2.LoadSystemStatus;
begin
  API_Get('system/status', @CB_SystemStatus, 'system/status');
end;

procedure TSyncthingApiV2.LoadSystemVersion;
begin
  API_Get('system/version', @CB_SystemVersion, 'system/version');
end;

procedure TSyncthingApiV2.StartLongPolling;
begin
  if (FState <> ssOnline) then Exit;
  if not Assigned(FHTTPEvents) then Exit;
  if FHTTPEvents.RequestInQueue('polling') then Exit;

  if (Assigned(FLongPollingRestartTimer)) and (FLongPollingRestartIntervalSec > 0) then
    FLongPollingRestartTimer.Enabled := True;

  FHTTPEvents.Get(
    FServerURL + 'rest/events?since=' + IntToStr(FEventsLastId) + '&limit=10&timeout=60',
    @CB_Events,
    '',
    'polling'
  );
end;

procedure TSyncthingApiV2.StopLongPolling;
begin
  if Assigned(FLongPollingRestartTimer) then
    FLongPollingRestartTimer.Enabled := False;
  if Assigned(FHTTPEvents) then
    FHTTPEvents.AbortActiveConnection();
end;

procedure TSyncthingApiV2.RestartLongPolling;
begin
  StopLongPolling;
  StartLongPolling;
end;

procedure TSyncthingApiV2.LongPollingRestartTimerHandler(Sender: TObject);
begin
  if Assigned(FOnBeforeLongPollingRestart) then
    FOnBeforeLongPollingRestart(Self);
  RestartLongPolling;
end;

procedure TSyncthingApiV2.HandleLongPollingResponse(EventsArray: TJSONArray);
var
  i: Integer;
  ev: TJSONObject;
  id: Int64;
begin
  if not Assigned(EventsArray) then Exit;
  for i := 0 to EventsArray.Count - 1 do
  begin
    if (EventsArray.Items[i] is TJSONObject) then
    begin
      ev := TJSONObject(EventsArray.Items[i]);
      id := ev.Get('id', -1);
      if (id >= 0) and (id > FEventsLastId) then
        FEventsLastId := id;
      ProcessEvent(ev);
    end;
  end;
end;

procedure TSyncthingApiV2.ProcessEvent(EventObj: TJSONObject);
var
  eventType: UTF8String;
begin
  if not Assigned(EventObj) then Exit;
  if Assigned(FOnEvent) then
    FOnEvent(Self, EventObj);

  eventType := EventObj.Get('type', '');

  if (eventType = 'ConfigSaved') then
    LoadSystemConfig
  else if (eventType = 'DeviceConnected') or (eventType = 'DeviceDisconnected') or
          (eventType = 'DevicePaused') or (eventType = 'DeviceResumed') then
  begin
    LoadSystemConnections;
    LoadStatsDevice;
  end
  else if (eventType = 'FolderSummary') or (eventType = 'FolderErrors') or
          (eventType = 'FolderPaused') or (eventType = 'FolderResumed') or
          (eventType = 'FolderCompletion') then
    LoadStatsFolder
  else if (eventType = 'StateChanged') then
    LoadSystemStatus;
end;

procedure TSyncthingApiV2.IntegrateEvent(EventObj: TJSONObject);
begin
  // Placeholder: integrate fragments from EventObj.Find('data') into Root if needed
end;

procedure TSyncthingApiV2.NotifyTreeChanged(const Path: UTF8String);
begin !!!
  if Assigned(FOnTreeChanged) then
    FOnTreeChanged(Self, Path);
end;

procedure TSyncthingApiV2.Connect;
begin
  FSM_Event(ssConnecting);
  
  if Assigned(FOnBeforeConnect) then
    FOnBeforeConnect(Self);

  FSM_Process();:
    BuildServerURL;
    ConfigureHttpClients;
    API_Get('system/ping', @CB_Ping, 'system/ping');
end;

procedure TSyncthingApiV2.Disconnect;
begin
  FSM_Event(ssDisconnecting);

  if Assigned(FOnBeforeDisconnect) then
    FOnBeforeDisconnect(Self);

  FSM_Process();:
    StopLongPolling;
    FState := ssOffline;

  if Assigned(FOnDisconnectedByUser) then
    FOnDisconnectedByUser(Self);
end;

procedure TSyncthingApiV2.SetEndpoint(const Host: UTF8String; Port: Integer; UseTLS: Boolean);
begin
  FHost := Host;
  FPort := Port;
  FUseTLS := UseTLS;
  FAPIKey := Key; !!!
  BuildServerURL;
end;

procedure TSyncthingApiV2.SetAPIKey(const Key: UTF8STRING);
begin!!!
end;

procedure TSyncthingApiV2.SetLongPollingRestartInterval(Seconds: Integer);
begin
  FLongPollingRestartIntervalSec := Seconds;
  ConfigureHttpClients;
end;

function TSyncthingApiV2.IsOnline: Boolean;
begin
  Result := (FState = ssOnline);
end;

// Internal helpers and callbacks

procedure TSyncthingApiV2.HttpAddHeader(Request: THttpRequest; Sender: TObject);
begin
  if Pos('X-API-Key:', Request.HeadersRaw) = 0 then
  begin
    if Request.HeadersRaw <> '' then
      Request.HeadersRaw := 'X-API-Key: ' + FAPIKey + LineEnding + Request.HeadersRaw
    else
      Request.HeadersRaw := 'X-API-Key: ' + FAPIKey;
  end;
end;

procedure TSyncthingApiV2.EventsDisconnected(Request: THttpRequest; Sender: TObject);
var
  action: TLongPollingDropAction;
begin
  action := lpdaDefault;
  if Assigned(FOnLongPollingDrop) then
    action := FOnLongPollingDrop(Self);

  case action of
  !!!???
    lpdaAbort:
      begin
        FState := ssOffline;
        if Assigned(FOnHardDisconnect) then
          FOnHardDisconnect(Self);
      end;
  else
    RestartLongPolling;
  end;
end;

function TSyncthingApiV2.ParseJson(Request: THttpRequest; out Json: TJSONData): boolean;
var
  Parser: TJSONParser;
  Data: TJSONData;
  MS: TMemoryStream;
begin
  Result := False;
  Json := nil;
  if (Request = nil) then Exit;
  if Request.Status = 499 then Exit;
  if (Request.Response <> nil) and (Request.Response.Size > 0) then
  begin
    Parser := nil;
    MS := TMemoryStream.Create;
    try
      Request.Response.Position := 0;
      !!! - копирование необязательно, можно использовать Request.Response.
      но для этого нужно дописать в AsyncHttp.pas, чтобы была защита от освобождения объекта если он еще в обработке
      MS.CopyFrom(Request.Response, Request.Response.Size);
      MS.Position := 0;
      Parser := TJSONParser.Create(MS, [joUTF8]);
      try
        Data := Parser.Parse;
      except
        on EJSONParser do Data := nil;
      end;
      if Data <> nil then
      begin
        Json := Data;
        Result := True;
      end;
    finally
      if Parser <> nil then Parser.Free;
      MS.Free;
    end;
  end;
end;

procedure TSyncthingApiV2.SetAtPath(var RootObj: TJSONObject; const JsonPath: UTF8String; NewData: TJSONData);
var
  segs: TStringList;
  i, idx: Integer;
  cur: TJSONObject;
  key: UTF8String;
  nextObj: TJSONObject;
  dotPath: UTF8String;
begin
  if not Assigned(RootObj) then
    RootObj := TJSONObject.Create;

  ??? что тут происходит и почему так сложно?

  dotPath := StringReplace(JsonPath, '/', '.', [rfReplaceAll]);

  segs := TStringList.Create;
  try
    segs.Delimiter := '.';
    segs.StrictDelimiter := True;
    segs.DelimitedText := StringReplace(dotPath, '.', ',', [rfReplaceAll]);
    if segs.Count = 0 then Exit;

    cur := RootObj;
    for i := 0 to segs.Count - 2 do
    begin
      key := segs[i];
      idx := cur.IndexOfName(key);
      if idx = -1 then
      begin
        nextObj := TJSONObject.Create;
        cur.Add(key, nextObj);
        cur := nextObj;
      end
      else
      begin
        if cur.Items[idx] is TJSONObject then
          cur := TJSONObject(cur.Items[idx])
        else
        begin
          cur.Delete(idx);
          nextObj := TJSONObject.Create;
          cur.Add(key, nextObj);
          cur := nextObj;
        end;
      end;
    end;

    key := segs[segs.Count - 1];
    idx := cur.IndexOfName(key);
    if idx <> -1 then
      cur.Delete(idx);
    cur.Add(key, NewData);
  finally
    segs.Free;
  end;
end;

procedure TSyncthingApiV2.API_Get(const Api: UTF8String; Callback: THttpRequestCallbackFunction; const QueueKey: UTF8String);
var
  key: UTF8String;
  plainKey: UTF8String;
  qPos: SizeInt;
begin
  if not Assigned(FHTTP) then Exit;
  !!! зачем QueueKey? - можно его убрать и всегда использовать key.
  можно в эту функцию добавить флаг чтобы не использовать QueueKey - чтобы запрос всегда выполнялся.

  AsyncHTTP - можно добавить флаг чтобы очищать очередь от ненужных дублирующихся запросов. Мне кажется это отличная идея.
  key := QueueKey;
  if key = '' then
    key := Api;
  plainKey := key;
  qPos := Pos('?', plainKey);
  !!! больше того. мне кажется чистить plainKey тоже не обязательно - пусть ключ будет более унильным если это требуется
  if qPos > 0 then
    plainKey := Copy(plainKey, 1, qPos - 1);
  FHTTP.Get(FServerURL + 'rest/' + Api, Callback, '', plainKey);
end;

procedure TSyncthingApiV2.CB_Ping(Request: THttpRequest);
begin
  if (Request <> nil) and (Request.Status = 200) and (Request.Connected) then
  begin
    FSM_Event(ssOnline);!!!
    FState := ssOnline;
    PerformInitialSync;
    if Assigned(FOnConnected) then
      FOnConnected(Self);
    StartLongPolling;
  end
  else
  begin
    FSM_Event(ssOffline);!!!
    FState := ssOffline;
    if Assigned(FOnConnectError) then
      FOnConnectError(Self, 'Ping failed or not connected');
  end;
end;

procedure TSyncthingApiV2.CB_SystemConfig(Request: THttpRequest);
var
  j: TJSONData;
begin
  !!! все эти функции можно унифицироват до одной функции.
  if ParseJson(Request, j) then
  begin
    ReplaceRootBranch('config', j);
  end;
end;

procedure TSyncthingApiV2.CB_SystemConnections(Request: THttpRequest);
var
  j: TJSONData;
begin
  if ParseJson(Request, j) then
  begin
    ReplaceRootBranch('system/connections', j);
  end;
end;

procedure TSyncthingApiV2.CB_StatsDevice(Request: THttpRequest);
var
  j: TJSONData;
begin
  if ParseJson(Request, j) then
  begin
    ReplaceRootBranch('stats/device', j);
  end;
end;

procedure TSyncthingApiV2.CB_StatsFolder(Request: THttpRequest);
var
  j: TJSONData;
begin
  if ParseJson(Request, j) then
  begin
    ReplaceRootBranch('stats/folder', j);
  end;
end;

procedure TSyncthingApiV2.CB_SystemStatus(Request: THttpRequest);
var
  j: TJSONData;
begin
  if ParseJson(Request, j) then
  begin
    ReplaceRootBranch('system/status', j);
  end;
end;

procedure TSyncthingApiV2.CB_SystemVersion(Request: THttpRequest);
var
  j: TJSONData;
begin
  if ParseJson(Request, j) then
  begin
    ReplaceRootBranch('system/version', j);
  end;
end;

procedure TSyncthingApiV2.CB_Events(Request: THttpRequest);
var
  j: TJSONData;
  a: TJSONArray;
begin
  j := nil;
  if ParseJson(Request, j) then
  try
    if (j is TJSONArray) then
    begin
      a := TJSONArray(j);
      HandleLongPollingResponse(a);
    end;
  finally
    if Assigned(j) then
      j.Free;
  end;
  StartLongPolling;
end;

end.


