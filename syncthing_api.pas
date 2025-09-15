unit syncthing_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, StrUtils,
  fpjson,
  AsyncHttp,
  ExtCtrls;

type
  // Finite state machine for the core
  TSyncthingFSM_State = (
    ssOffline,        // No active connection; idle/offline state
    ssConnectingInitAndPing,     // Connecting to Syncthing
    ssConnectingPingWait, // Connecting to Syncthing
    ssConnectingWaitData,
    ssOnline,         // Connected; long-polling is active
    ssOnlinePaused,   // Connected; long-polling is offline
    ssOnlineUnstable, // Connected; long-polling has an error and is offline
    ssDisconnecting   // Graceful disconnect in progress
  );

  // ###
  TSyncthingFSM_Command = (
    ssCmdNone,
    ssCmdConnect,
    ssCmdDisconnect,
    ssCmdPause,
    ssCmdPauseRelease,
    ssCmdConnectingPingAck,
    ssCmdConnectingPingFault,
    ssCmdConnectingAcceptedData,
    // TODO: add  `TimerConnectingTimeout`
    ssCmdConnectingTimeout,
    ssCmdLongPollingTimeToAutoReconnect,
    ssCmdLongPollingDisconnected,
    ssCmdLongPollingTimerRestore,
    ssCmdLongPollingError
  );

  // Event types (callbacks)
  TNotifyEventObj = procedure(Sender: TObject) of object;
  TConnectErrorEvent = procedure(Sender: TObject; const MessageText: UTF8String) of object;
  TEventJsonEvent = procedure(Sender: TObject; Event: TJSONObject) of object;
  // Fired when a branch in the in-memory JSON tree was modified; Path is a JSON path
  TTreeChangedEvent = procedure(Sender: TObject; const Path: UTF8String) of object;
  TBeforeAfterTreeModifyEvent = procedure(Sender: TObject) of object;
  TLongPollingDropHandler = procedure(Sender: TObject) of object;

  // Endpoints enum for REST initial sync
  TSyncthingEndpointId = (
    epConfig,
    epSystemConnections,
    epStatsDevice,
    epStatsFolder,
    epSystemStatus,
    epSystemVersion
  );

const
  // Typed constant containing all endpoint ids
  SyncthingEndpointsBasic: array of TSyncthingEndpointId = (
    epConfig,
    epSystemConnections,
    epStatsDevice,
    epStatsFolder,
    epSystemStatus,
    epSystemVersion
  );

type
  { TSyncthingAPI }

  // Core class for interacting with Syncthing (REST + Event API) and maintaining JSON tree
  TSyncthingAPI = class(TComponent)
  private
    FState: TSyncthingFSM_State;
    FRoot: TJSONObject;            // Single in-memory JSON tree root
    FAPIKey: UTF8String;           // X-API-Key
    FHost: UTF8String;             // Syncthing host
    FPort: Integer;                // Syncthing port
    FStateCommand: TSyncthingFSM_Command;
    FUseTLS: Boolean;              // Use HTTPS if true
    FServerURL: UTF8String;        // Built base URL (scheme://host:port/), see `BuildServerURL`

    FHTTP: TAsyncHTTP;             // REST client
    FHTTPEvents: TAsyncHTTP;       // Long-polling client

    FEventsLastId: Int64;          // Last seen event id
    FLongPollingRestartIntervalSec: Integer; // Periodic restart interval
    FLongPollingRestartTimer: TTimer;
    FLongPollingSelfRestartFlag: boolean; // flag that we ourselves caused the pooling restart

    FConnectTimeout: Integer; // Global connect timeout for both clients
    FIOTimeout: Integer;      // Global IO timeout for both clients

    FTimerConnectingTimeout: TTimer;
    FTimerLongPollingErrorRestore: TTimer;

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
    // State process. (FSM)
    procedure FSM_Process();
    // Internal helpers
    procedure HttpAddHeader(Request: THttpRequest; Sender: TObject);
    procedure LongPollingDisconnected(Request: THttpRequest; Sender: TObject);
    procedure LongPollingError(Request: THttpRequest; Sender: TObject);
    procedure OnRestAPIQueueEmpty(Sender: TObject);
    procedure ConnectingTimeoutTimerHandler(Sender: TObject);
    procedure LongPollingErrorRestoreTimerHandler(Sender: TObject);

    // JSON Utils

    function ParseJson(Request: THttpRequest; out Json: TJSONData): boolean;
    procedure SetAtPath(var RootObj: TJSONObject; const JsonPath: UTF8String; NewData: TJSONData);
    procedure FindAndCreatePathInJsonTree(var RootObj: TJSONObject;
      const JsonPath: UTF8String; out Parent: TJSONObject; out
      Target: TJSONData; out TargetName: UTF8String);

    // REST callbacks
    procedure CB_CheckOnline(Request: THttpRequest);
    procedure CB_HandleEndpoint(Request: THttpRequest);
    // Events callbacks
    procedure CB_Events(Request: THttpRequest);
    // REST helper
    procedure API_Get(
      const Api: UTF8String;
      Callback: THttpRequestCallbackFunction;
      userString: string);
    // Build endpoints table
    procedure InitEndpointTable;
    // Maps endpoint id to callback handler
    function GetEndpointCallback(Id: TSyncthingEndpointId): THttpRequestCallbackFunction; virtual;
    // Maps endpoint id to "JSON Tree" storage path
    function GetEndpointJsonTreePath(Id: TSyncthingEndpointId): UTF8String; virtual;

  protected
    { Builds base server URL from host/port and scheme }
    procedure BuildServerURL; virtual;
    { Creates and configures HTTP clients for REST and long-polling }
    procedure ConfigureHttpClients(); virtual;

    // JSON tree helpers
    { Creates default JSON root object used before initial sync }
    function CreateDefaultRoot: TJSONObject; virtual;
    { Notifies listeners and prepares to modify the JSON tree }
    procedure BeginTreeModify(); virtual;
    { Notifies listeners and finalizes JSON tree modification }
    procedure EndTreeModify(); virtual;
    { Replaces a branch in the "JSON Tree" with NewData }
    procedure JSONTreeNewData(const JsonPath: UTF8String; NewData: TJSONData); virtual;

    { Load all data from REST resources to JSON tree }
    procedure LoadAllData(); virtual;
    {
    Loads a single endpoint identified by Id to JSON tree
    }
    procedure LoadEndpoint(Id: TSyncthingEndpointId); virtual;

    // Long-polling lifecycle
    { Starts event long-polling listener }
    procedure StartLongPolling(); virtual;
    { Stops event long-polling listener }
    procedure StopLongPolling(); virtual;
    { Restarts event long-polling listener (periodic or on drop) }
    procedure RestartLongPolling(); virtual;
    { Timer handler to periodically restart long-polling }
    procedure LongPollingRestartTimerHandler(Sender: TObject); virtual;

    // Long-polling processing
    { Parses and processes events array returned by Event API }
    procedure HandleLongPollingResponse(EventsArray: TJSONArray); virtual;
    { Dispatches single event to user handler before internal integration }
    procedure ProcessEvent(EventObj: TJSONObject); virtual;
    { Integrates single event data into the JSON tree }
    procedure IntegrateEvent(const EventObj: TJSONObject); virtual;
    { Notifies listeners that a tree branch at Path was changed }
    procedure NotifyTreeChanged(const Path: UTF8String); virtual;

  public
    { Creates the core object without starting any network activity }
    constructor Create(AOwner: TComponent); override;
    { Frees resources and detaches clients }
    destructor Destroy; override;

    // Connection control
    { Connects to a running Syncthing instance and performs initial sync }
    procedure Connect(); virtual;
    { Gracefully disconnects and stops long-polling }
    procedure Disconnect(); virtual;
    procedure Pause(); virtual;
    procedure PauseRelease(); virtual;

    // Configuration
    { Sets endpoint (host, port, TLS flag) and rebuilds base URL }
    procedure SetEndpoint(const Host: UTF8String; Port: Integer; UseTLS: Boolean); virtual;
    { Sets X-API-Key for authenticated requests }
    procedure SetAPIKey(const Key: UTF8String); virtual;
    { Sets periodic restart interval for long-polling (0 to disable) }
    procedure SetLongPollingRestartInterval(Seconds: Integer); virtual;
    procedure SetConnectTimeout(Value: Integer); virtual;
    procedure SetIOTimeout(Value: Integer); virtual;
    procedure SetConnectingTimeout(Value: Integer); virtual;
    function GetConnectingTimeout: Integer; virtual;

    // Properties
    property ConnectTimeout: Integer read FConnectTimeout write SetConnectTimeout;
    property IOTimeout: Integer read FIOTimeout write SetIOTimeout;
    property ConnectingTimeout: Integer read GetConnectingTimeout write SetConnectingTimeout;

    // State and data

    { Returns true when FSM is online }
    function IsOnline: Boolean; virtual;
    { Maps endpoint id to REST URI (under /rest/) }
    class function GetEndpointURI(Id: TSyncthingEndpointId): UTF8String; static;
    { Current finite state machine state }
    property State: TSyncthingFSM_State read FState;
    { Current finite state machine command }
    property Command: TSyncthingFSM_Command read FStateCommand write FStateCommand;
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
  jsonscanner,
  jsonparser;

procedure TSyncthingAPI.FindAndCreatePathInJsonTree(
  var RootObj: TJSONObject; const JsonPath: UTF8String; 
  out Parent: TJSONObject; out Target: TJSONData; out TargetName: UTF8String);
var
  parts: TStringDynArray;
  i: Integer;
begin
  if (JsonPath = '') or (JsonPath[1] = '/') or (JsonPath[Length(JsonPath)] = '/') then
    raise Exception.Create('Invalid JSON path format');

  parts := SplitString(JsonPath, '/');

  Parent := nil;
  Target := RootObj;

  for i := Low(parts) to High(parts) do
  begin
    // Step into the target
    Parent := TJSONObject(Target);

    TargetName := parts[i];
    Target := TJSONObject(Parent.Find(TargetName));

    // Remove invalid object
    // if object present and is not a last object in path
    if Assigned(Target) and ((i < High(parts)) and not (Target is TJSONObject)) then
    begin
      Parent.Delete(TargetName);
      Target := nil;
    end;

    // Add new object to continue path (if needed)
    if not Assigned(Target) then
    begin
      Parent.Add(TargetName, TJSONObject.Create());
      Target := TJSONObject(Parent.Find(TargetName));
    end;
  end;
end;



procedure TSyncthingAPI.FSM_Process();
  function ProcessCmdDisconnect(): boolean;
  begin
    Result := false;
    if (Command = ssCmdDisconnect) and Assigned(FOnBeforeDisconnect) then
      FOnBeforeDisconnect(Self);
    if Command = ssCmdDisconnect then
    begin
      FState:=ssDisconnecting;
      Result := true; // continue;
    end;
  end;

begin
  while True do
  begin
    case FState of

      ssConnectingInitAndPing:
        begin
          BuildServerURL();
          ConfigureHttpClients();
          // Start initial ping; CB_CheckOnline drives next transitions
          API_Get('system/ping', @CB_CheckOnline, '');
          FTimerConnectingTimeout.Enabled := True;
          FState:=ssConnectingPingWait;
        end;

      ssConnectingPingWait:
        begin
          if (Command = ssCmdConnectingPingFault) or
             (Command = ssCmdConnectingTimeout) or
             (Command = ssCmdDisconnect)
          then
          begin
            FState:=ssOffline;
            FHTTP.CancelAll();
          end;
          if Command = ssCmdConnectingPingAck then
          begin
            FState:=ssConnectingWaitData;
            LoadAllData();
          end;
        end;

      ssConnectingWaitData:
        begin
          if Command = ssCmdConnectingAcceptedData then
          begin
            if FHTTP.QueueCount = 0 then
            begin
              // All data loaded, ready to "online status" ('.')\
              FState:=ssOnline;
              FTimerConnectingTimeout.Enabled := False;

              Command := ssCmdLongPollingTimeToAutoReconnect;
              if Assigned(FOnConnected) then
                FOnConnected(Self);

              continue;
            end;
          end;
          if (Command = ssCmdConnectingTimeout) or
             (Command = ssCmdDisconnect)
          then
          begin
            FState:=ssOffline;
            FHTTP.CancelAll();
          end;
        end;

      ssOnline:
        begin
          if ProcessCmdDisconnect() then
            continue;

          if Command = ssCmdLongPollingError then
          begin
            // goto "Unstable"
            FState:=ssOnlineUnstable;
            FTimerLongPollingErrorRestore.Enabled:=true;
            continue;
          end;
          if Command = ssCmdLongPollingDisconnected then
          begin
            if not FHTTPEvents.RequestInQueueCold('pooling') and
               not FLongPollingSelfRestartFlag
            then
              StartLongPolling();
          end;
          if Command = ssCmdLongPollingTimeToAutoReconnect then
          begin
            RestartLongPolling();
          end;
          if Command = ssCmdPause then
          begin
            StopLongPolling();
            FState:=ssOnlinePaused;
          end;
          if Command = ssCmdLongPollingTimerRestore then
          begin
            FTimerLongPollingErrorRestore.Enabled:=false;
          end;
        end;

      ssOnlineUnstable:
        begin
          if ProcessCmdDisconnect() then
            continue;
          if Command = ssCmdLongPollingTimerRestore then
          begin
            if FHTTPEvents.RequestInQueue('pooling') then
            begin
              // goto "Online"
              FState:=ssOnline;
              continue;
            end
            else
              StartLongPolling();
          end;
          if Command = ssCmdPause then
          begin
            StopLongPolling();
            FState:=ssOnlinePaused;
            continue;
          end;
        end;

      ssOnlinePaused:
      begin
        if ProcessCmdDisconnect() then
          continue;

        if Command = ssCmdPauseRelease then
        begin
          FState:=ssOnline;
          StartLongPolling();
        end;
      end;

      ssDisconnecting:
        begin
          FTimerConnectingTimeout.Enabled := False;
          StopLongPolling();
          FHTTP.CancelAll();
          FState := ssOffline;

          if Assigned(FOnDisconnectedByUser) then
             FOnDisconnectedByUser(Self);
        end;

      ssOffline:
        begin
          FTimerConnectingTimeout.Enabled := False;

          if Command = ssCmdConnect then
          begin
            if Assigned(FOnBeforeConnect) then
              FOnBeforeConnect(Self);

            if Command = ssCmdConnect then
            begin
              FState := ssConnectingInitAndPing;
              continue;
            end;
          end;
        end;

    end; // case

    break;
    Command:=ssCmdNone;
  end; // while

end;

class function TSyncthingAPI.GetEndpointURI(Id: TSyncthingEndpointId): UTF8String;
begin
  case Id of
    epConfig:            Exit('config');
    epSystemConnections: Exit('system/connections');
    epStatsDevice:       Exit('stats/device');
    epStatsFolder:       Exit('stats/folder');
    epSystemStatus:      Exit('system/status');
    epSystemVersion:     Exit('system/version');
  end;
  Exit('invalid_endpoint');
end;

function TSyncthingAPI.GetEndpointJsonTreePath(Id: TSyncthingEndpointId): UTF8String;
begin
  // Copy of endpoint URI
  Result := GetEndpointURI(Id);
end;

{ TSyncthingAPI }

constructor TSyncthingAPI.Create(AOwner: TComponent);
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
  FLongPollingRestartIntervalSec := 60;
  FLongPollingRestartTimer := nil;
  FConnectTimeout := 1000;
  FIOTimeout := 1000;
  FTimerConnectingTimeout := TTimer.Create(Self);
  FTimerConnectingTimeout.Enabled := False;
  FTimerConnectingTimeout.OnTimer := @ConnectingTimeoutTimerHandler;
  FTimerConnectingTimeout.Interval:= 5000;
  FTimerLongPollingErrorRestore := TTimer.Create(Self);
  FTimerLongPollingErrorRestore.Enabled := False;
  FTimerLongPollingErrorRestore.OnTimer := @LongPollingErrorRestoreTimerHandler;
  FTimerLongPollingErrorRestore.Interval := 5000;
  InitEndpointTable;
end;

destructor TSyncthingAPI.Destroy;
begin
  if Assigned(FLongPollingRestartTimer) then
    FreeAndNil(FLongPollingRestartTimer);
  if Assigned(FTimerConnectingTimeout) then
    FreeAndNil(FTimerConnectingTimeout);
  if Assigned(FTimerLongPollingErrorRestore) then
    FreeAndNil(FTimerLongPollingErrorRestore);
  if Assigned(FHTTP) then
    FreeAndNil(FHTTP);
  if Assigned(FHTTPEvents) then
    FreeAndNil(FHTTPEvents);
  if Assigned(FRoot) then
    FreeAndNil(FRoot);
  inherited Destroy;
end;

procedure TSyncthingAPI.BuildServerURL;
begin
  if FUseTLS then
    FServerURL := Format('https://%s:%d/', [FHost, FPort])
  else
    FServerURL := Format('http://%s:%d/', [FHost, FPort]);
end;

procedure TSyncthingAPI.ConfigureHttpClients;
begin
  if not Assigned(FHTTP) then
    FHTTP := TAsyncHTTP.Create;
  FHTTP.ConnectTimeout := FConnectTimeout;
  FHTTP.RetryCount := 1;
  FHTTP.IOTimeout := FIOTimeout;
  FHTTP.KeepConnection := True;
  FHTTP.OnOpened := @HttpAddHeader;
  FHTTP.OnQueueEmpty := @OnRestAPIQueueEmpty;

  if not Assigned(FHTTPEvents) then
    FHTTPEvents := TAsyncHTTP.Create;
  FHTTPEvents.ConnectTimeout := FConnectTimeout;
  FHTTPEvents.RetryCount := 0;
  FHTTPEvents.IOTimeout := FLongPollingRestartIntervalSec * 1000 + FIOTimeout;
  FHTTPEvents.KeepConnection := True;
  FHTTPEvents.OnOpened := @HttpAddHeader;
  FHTTPEvents.OnDisconnected := @LongPollingDisconnected;
  FHTTPEvents.OnError := @LongPollingError;

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

function TSyncthingAPI.CreateDefaultRoot: TJSONObject;
var
  ep: TSyncthingEndpointId;
  rootObj: TJSONObject;
begin
  // Create root and pre-create empty branches for known endpoints
  rootObj := TJSONObject.Create;
  for ep in SyncthingEndpointsBasic do
    SetAtPath(rootObj, GetEndpointJsonTreePath(ep), TJSONObject.Create);
  Result := rootObj;
end;

procedure TSyncthingAPI.BeginTreeModify;
begin
  if Assigned(FOnBeforeTreeModify) then
    FOnBeforeTreeModify(Self);
end;

procedure TSyncthingAPI.EndTreeModify;
begin
  if Assigned(FOnAfterTreeModify) then
    FOnAfterTreeModify(Self);
end;

procedure TSyncthingAPI.JSONTreeNewData(const JsonPath: UTF8String; NewData: TJSONData);
begin
  BeginTreeModify;
  try
    if NewData is TJSONData then
    begin
      //
    end;


    SetAtPath(FRoot, JsonPath, NewData);
    NotifyTreeChanged(JsonPath);
  finally
    EndTreeModify;
  end;
end;

procedure TSyncthingAPI.LoadAllData();
var
  ep: TSyncthingEndpointId;
begin
  // Iterate endpoints and call unified handler
  for ep in SyncthingEndpointsBasic do
    LoadEndpoint(ep);
end;

procedure TSyncthingAPI.LoadEndpoint(Id: TSyncthingEndpointId);
begin
  // Note: pass "JSON target" path via UserString, see `CB_HandleEndpoint`

  API_Get(
    GetEndpointURI(Id), // REST API Endpoint
    GetEndpointCallback(Id), // callback: usually `CB_HandleEndpoint`
    GetEndpointURI(Id)  // userString: send JSON-tree target path
  );
end;

procedure TSyncthingAPI.CB_HandleEndpoint(Request: THttpRequest);
var
  j: TJSONData;
begin
  if (Request <> nil) then
  begin
    if Request.UserString = '' then Exit;
    if ParseJson(Request, j) then
    begin
      JSONTreeNewData(Request.UserString, j);
    end;
  end;
end;

procedure TSyncthingAPI.StartLongPolling();
begin
  if (Assigned(FLongPollingRestartTimer)) and (FLongPollingRestartIntervalSec > 0) then
  begin
    FLongPollingRestartTimer.Enabled := False;
    FLongPollingRestartTimer.Enabled := True;
  end;

  if FHTTPEvents.RequestInQueue('polling') then
    FHTTPEvents.CancelAll();

  FHTTPEvents.Get(
    FServerURL + 'rest/events?since=' + IntToStr(FEventsLastId) + '&limit=10&timeout=60',
    @CB_Events,
    '',
    'polling'
  );
end;

procedure TSyncthingAPI.StopLongPolling;
begin
  if Assigned(FLongPollingRestartTimer) then
    FLongPollingRestartTimer.Enabled := False;

  if Assigned(FHTTPEvents) then
    FHTTPEvents.CancelAll();
end;

procedure TSyncthingAPI.RestartLongPolling;
begin
  FLongPollingSelfRestartFlag := true;
  StopLongPolling();
  StartLongPolling();
  FLongPollingSelfRestartFlag := false;
end;

procedure TSyncthingAPI.LongPollingRestartTimerHandler(Sender: TObject);
begin
  Command:=ssCmdLongPollingTimeToAutoReconnect;

  if Assigned(FOnBeforeLongPollingRestart) then
    FOnBeforeLongPollingRestart(Self);

  FSM_Process();
end;

procedure TSyncthingAPI.ConnectingTimeoutTimerHandler(Sender: TObject);
begin
  Command:=ssCmdConnectingTimeout;
  FSM_Process();
end;

procedure TSyncthingAPI.LongPollingErrorRestoreTimerHandler(Sender: TObject);
begin
  Command:=ssCmdLongPollingTimerRestore;
  FSM_Process();
end;

procedure TSyncthingAPI.HandleLongPollingResponse(EventsArray: TJSONArray);
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

procedure TSyncthingAPI.ProcessEvent(EventObj: TJSONObject);
var
  eventType: UTF8String;
begin
  if not Assigned(EventObj) then Exit;
  if Assigned(FOnEvent) then
    FOnEvent(Self, EventObj);

  eventType := EventObj.Get('type', '');

  if (eventType = 'ConfigSaved') then
    LoadEndpoint(epConfig)
  else if (eventType = 'DeviceConnected') or (eventType = 'DeviceDisconnected') or
          (eventType = 'DevicePaused') or (eventType = 'DeviceResumed') then
  begin
    LoadEndpoint(epSystemConnections);
    LoadEndpoint(epStatsDevice);
  end
  else if (eventType = 'FolderSummary') or (eventType = 'FolderErrors') or
          (eventType = 'FolderPaused') or (eventType = 'FolderResumed') or
          (eventType = 'FolderCompletion') then
    LoadEndpoint(epStatsFolder)
  else if (eventType = 'StateChanged') then
    LoadEndpoint(epSystemStatus);
end;

procedure TSyncthingAPI.IntegrateEvent(const EventObj: TJSONObject);
begin
  // Placeholder: integrate fragments from EventObj.Find('data') into Root if needed
  Assert(EventObj <> nil);
end;

procedure TSyncthingAPI.NotifyTreeChanged(const Path: UTF8String);
begin
  if Assigned(FOnTreeChanged) then
    FOnTreeChanged(Self, Path);
end;

procedure TSyncthingAPI.Connect;
begin
  Self.Command:=ssCmdConnect;
  FSM_Process();
end;

procedure TSyncthingAPI.Disconnect;
begin
  Command:=ssCmdDisconnect;
  FSM_Process();
end;

procedure TSyncthingAPI.Pause();
begin
  if IsOnline then
  begin
    Command:=ssCmdPause;
    FSM_Process();
  end;
end;

procedure TSyncthingAPI.PauseRelease();
begin
  if IsOnline then
  begin
    Command:=ssCmdPauseRelease;
    FSM_Process();
  end;
end;

procedure TSyncthingAPI.SetEndpoint(const Host: UTF8String; Port: Integer; UseTLS: Boolean);
begin
  FHost := Host;
  FPort := Port;
  FUseTLS := UseTLS;
  BuildServerURL;
end;

procedure TSyncthingAPI.SetAPIKey(const Key: UTF8String);
begin
  FAPIKey := Key;
end;

procedure TSyncthingAPI.SetLongPollingRestartInterval(Seconds: Integer);
begin
  FLongPollingRestartIntervalSec := Seconds;
  ConfigureHttpClients;
end;

procedure TSyncthingAPI.SetConnectTimeout(Value: Integer);
begin
  FConnectTimeout := Value;
  if Assigned(FHTTP) then
    FHTTP.ConnectTimeout := Value;
  if Assigned(FHTTPEvents) then
    FHTTPEvents.ConnectTimeout := Value;
end;

procedure TSyncthingAPI.SetIOTimeout(Value: Integer);
begin
  FIOTimeout := Value;
  if Assigned(FHTTP) then
    FHTTP.IOTimeout := Value;
  if Assigned(FHTTPEvents) then
    FHTTPEvents.IOTimeout := FLongPollingRestartIntervalSec * 1000 + Value;
end;

procedure TSyncthingAPI.SetConnectingTimeout(Value: Integer);
begin
  if Assigned(FTimerConnectingTimeout) then
    FTimerConnectingTimeout.Interval := Value;
end;

function TSyncthingAPI.GetConnectingTimeout: Integer;
begin
  if Assigned(FTimerConnectingTimeout) then
    Result := FTimerConnectingTimeout.Interval
  else
    Result := 0;
end;

function TSyncthingAPI.IsOnline: Boolean;
begin
  Result := (State = ssOnline) or (State = ssOnlinePaused) or (State = ssOnlineUnstable);
end;

// Internal helpers and callbacks

procedure TSyncthingAPI.HttpAddHeader(Request: THttpRequest; Sender: TObject);
begin
  if Pos('X-API-Key:', Request.HeadersRaw) = 0 then
  begin
    if Request.HeadersRaw <> '' then
      Request.HeadersRaw := 'X-API-Key: ' + FAPIKey + LineEnding + Request.HeadersRaw
    else
      Request.HeadersRaw := 'X-API-Key: ' + FAPIKey;
  end;
end;

procedure TSyncthingAPI.LongPollingDisconnected(Request: THttpRequest; Sender: TObject);
begin
  Command:=ssCmdLongPollingDisconnected;

  if Assigned(FOnLongPollingDrop) then
    FOnLongPollingDrop(Self);

  FSM_Process();
end;

procedure TSyncthingAPI.LongPollingError(Request: THttpRequest; Sender: TObject
  );
begin
  Command:=ssCmdLongPollingError;
  FSM_Process();
end;

procedure TSyncthingAPI.OnRestAPIQueueEmpty(Sender: TObject);
begin
  if Self.State = ssConnectingWaitData then
  begin
    Command:=ssCmdConnectingAcceptedData;
    FSM_Process();
  end;
end;

function TSyncthingAPI.ParseJson(Request: THttpRequest; out Json: TJSONData): boolean;
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
      // TODO: ParseJson. remove `MS: TMemoryStream`
      // I think in the future this can be removed,
      // there were problems with AV,
      // but I hope I fixed this bug
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

procedure TSyncthingAPI.SetAtPath(var RootObj: TJSONObject; const JsonPath: UTF8String; NewData: TJSONData);
var
  parent: TJSONObject;
  targetData: TJSONData;
  targetName: UTF8String;
begin
  FindAndCreatePathInJsonTree(RootObj, JsonPath, parent, targetData, targetName);

  // remove the old branch and adding the new branch
  parent.Delete(targetName);
  parent.Add(targetName, NewData);
end;

procedure TSyncthingAPI.API_Get(const Api: UTF8String;
  Callback: THttpRequestCallbackFunction; userString: string);
begin
  if not Assigned(FHTTP) then Exit;

  FHTTP.Get(
    FServerURL + 'rest/' + Api, // url
    Callback,                    // callback
    '',                          // headers
    Api,                    // operationName
    nil,                         // userObject
    userString,                         // userString
    True                         // clearDuplicates
  );
end;

procedure TSyncthingAPI.InitEndpointTable;
begin
  // Currently not needed since we resolve callback via GetEndpointCallback
end;

function TSyncthingAPI.GetEndpointCallback(Id: TSyncthingEndpointId): THttpRequestCallbackFunction;
begin
  // Default to unified handler; specialize here if any endpoint needs custom logic
  (*
  case Id of
    // Example of specialization (none at the moment):
    epSystemStatus: Exit(@CB_SystemStatus);
  end;
  *)
  Exit(@CB_HandleEndpoint);
end;

procedure TSyncthingAPI.CB_CheckOnline(Request: THttpRequest);
begin
  if (Request <> nil) and (Request.Status = 200) and (Request.Succeeded) then
  begin
    // Is online
    Command:=ssCmdConnectingPingAck;
    FSM_Process();
  end
  else
  begin
    Command:=ssCmdConnectingPingFault;
    FSM_Process();
    if Assigned(FOnConnectError) then
      FOnConnectError(Self, 'Ping failed or not connected');
  end;
end;

procedure TSyncthingAPI.CB_Events(Request: THttpRequest);
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
  // TODO: remove it !!!
  // кажется это просто не нужно, мы потом поймаем событие дисконект.
  // или тут нужно вызывать FSM с особой командой
  StartLongPolling;
end;

end.


