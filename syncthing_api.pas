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
  TSyncthingFSM_State = (
    ssOffline,        // No active connection; idle/offline state
    ssConnectingPingSend,     // Connecting to Syncthing
    ssConnectingPingWait, // Connecting to Syncthing
    ssConnectingLoadData,
    ssOnline,         // Connected and operational; long-polling is active
    ssDisconnecting   // Graceful disconnect in progress
  );

  // ###
  TSyncthingFSM_Command = (
    ssCmdNone,
    ssCmdConnect,
    ssCmdDisconnect,
    ssCmdConnectingConfirmed,
    ssCmdConnectingFault,
    ssCmdConnectingAcceptedData,
    ssCmdConnectingTimeout
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
  { TSyncthingApiV2 }

  // Core class for interacting with Syncthing (REST + Event API) and maintaining JSON tree
  TSyncthingApiV2 = class(TComponent)
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
    function ParseJson(Request: THttpRequest; out Json: TJSONData): boolean;
    procedure SetAtPath(var RootObj: TJSONObject; const JsonPath: UTF8String; NewData: TJSONData);
    // REST callbacks
    procedure CB_CheckOnline(Request: THttpRequest);
    procedure CB_HandleEndpoint(Request: THttpRequest);
    // Events callbacks
    procedure CB_Events(Request: THttpRequest);
    // REST helper
    procedure API_Get(const Api: UTF8String; Callback: THttpRequestCallbackFunction);
    // Build endpoints table
    procedure InitEndpointTable;
    // Maps endpoint id to callback handler
    function SyncthingEndpointCallback(Id: TSyncthingEndpointId): THttpRequestCallbackFunction; virtual;
    // Maps endpoint id to JSON storage path (root path to replace)
    function SyncthingEndpointJsonPath(Id: TSyncthingEndpointId): UTF8String; virtual;

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
    { Loads a single endpoint identified by Id }
    procedure LoadEndpoint(Id: TSyncthingEndpointId); virtual;

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
    { Maps endpoint id to REST URI (under /rest/) }
    class function SyncthingEndpointURI(Id: TSyncthingEndpointId): UTF8String; static;
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



procedure TSyncthingApiV2.FSM_Process();
begin
  while True do
  begin
    case FState of

      ssConnectingPingSend:
        begin
          BuildServerURL;
          ConfigureHttpClients;
          // Start initial ping; CB_CheckOnline drives next transitions
          API_Get('system/ping', @CB_CheckOnline);
          FState:=ssConnectingPingWait;
        end;

      ssConnectingPingWait:
        begin
          if Command = ssCmdDisconnect then
          begin
            FState:=ssOffline;
          end;
          if Command = ssCmdConnectingFault then
          begin
            FState:=ssOffline;
          end;
          if Command = ssCmdConnectingConfirmed then
          begin
            FState:=ssConnectingLoadData;
            PerformInitialSync;
            continue;
          end;
        end;

      ssConnectingLoadData:
        begin
          if Command = ssCmdConnectingAcceptedData then
          begin
            if FHTTP.QueueCount = 0 then
            begin
              if Assigned(FOnConnected) then
                FOnConnected(Self);

              FState:=ssOnline;

              StartLongPolling();
            end;
          end;
          if Command = ssCmdDisconnect then
          begin
            FState:=ssOffline;
          end;
          if Command = ssCmdConnectingTimeout then
          begin
            FState:=ssOffline;
          end;
        end;

      ssOnline:
        begin
          if (Command = ssCmdDisconnect) and Assigned(FOnBeforeDisconnect) then
            FOnBeforeDisconnect(Self);

          if Command = ssCmdDisconnect then
          begin
            FState:=ssDisconnecting;
            continue;
          end;
        end;

      ssDisconnecting:
        begin
          StopLongPolling();
          FHTTP.CancelAll();
          FState := ssOffline;

          if Assigned(FOnDisconnectedByUser) then
             FOnDisconnectedByUser(Self);
        end;

      ssOffline:
        begin
          if Command = ssCmdConnect then
          begin
            if Assigned(FOnBeforeConnect) then
              FOnBeforeConnect(Self);

            if Command = ssCmdConnect then
            begin
              FState := ssConnectingPingSend;
              continue;
            end;
          end;
        end;

    end; // case

    break;
    Command:=ssCmdNone;
  end; // while

end;

class function TSyncthingApiV2.SyncthingEndpointURI(Id: TSyncthingEndpointId): UTF8String;
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

function TSyncthingApiV2.SyncthingEndpointJsonPath(Id: TSyncthingEndpointId): UTF8String;
begin
  // Copy of endpoint URI
  Result := SyncthingEndpointURI(Id);
end;

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
  InitEndpointTable;
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
  FHTTPEvents.OnDisconnected := @LongPollingDisconnected;

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
var
  ep: TSyncthingEndpointId;
  rootObj: TJSONObject;
begin
  // Create root and pre-create empty branches for known endpoints
  rootObj := TJSONObject.Create;
  for ep in SyncthingEndpointsBasic do
    SetAtPath(rootObj, SyncthingEndpointJsonPath(ep), TJSONObject.Create);
  Result := rootObj;
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
var
  ep: TSyncthingEndpointId;
begin
  // Iterate endpoints and call unified handler; pass JSON path via UserString
  for ep in SyncthingEndpointsBasic do
    LoadEndpoint(ep);
end;
procedure TSyncthingApiV2.LoadEndpoint(Id: TSyncthingEndpointId);
begin
  API_Get(SyncthingEndpointURI(Id), @CB_HandleEndpoint);
end;

procedure TSyncthingApiV2.CB_HandleEndpoint(Request: THttpRequest);
var
  j: TJSONData;
begin
  if (Request <> nil) then
  begin
    if Request.UserString = '' then Exit;
    if ParseJson(Request, j) then
    begin
      ReplaceRootBranch(Request.UserString, j);
    end;
  end;
end;

procedure TSyncthingApiV2.StartLongPolling;
begin
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

procedure TSyncthingApiV2.IntegrateEvent(EventObj: TJSONObject);
begin
  // Placeholder: integrate fragments from EventObj.Find('data') into Root if needed
end;

procedure TSyncthingApiV2.NotifyTreeChanged(const Path: UTF8String);
begin
  if Assigned(FOnTreeChanged) then
    FOnTreeChanged(Self, Path);
end;

procedure TSyncthingApiV2.Connect;
begin
  Self.Command:=ssCmdConnect;
  FSM_Process();
end;

procedure TSyncthingApiV2.Disconnect;
begin
  Command:=ssCmdDisconnect;
  FSM_Process();
end;

procedure TSyncthingApiV2.SetEndpoint(const Host: UTF8String; Port: Integer; UseTLS: Boolean);
begin
  FHost := Host;
  FPort := Port;
  FUseTLS := UseTLS;
  BuildServerURL;
end;

procedure TSyncthingApiV2.SetAPIKey(const Key: UTF8String);
begin
  FAPIKey := Key;
end;

procedure TSyncthingApiV2.SetLongPollingRestartInterval(Seconds: Integer);
begin
  FLongPollingRestartIntervalSec := Seconds;
  ConfigureHttpClients;
end;

function TSyncthingApiV2.IsOnline: Boolean;
begin
  Result := (State = ssOnline);
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

procedure TSyncthingApiV2.LongPollingDisconnected(Request: THttpRequest; Sender: TObject);
begin
  if Assigned(FOnLongPollingDrop) then
    FOnLongPollingDrop(Self);

  // TODO: !!!
  (*if
  // MOVE TO FSM
  case action of
  // !!!???
    lpdaAbort:
      begin
        FSM_Event(ssOffline);
        if Assigned(FOnHardDisconnect) then
          FOnHardDisconnect(Self)
        else
          RestartLongPolling;
      end;
  else
    RestartLongPolling;
  end;*)
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

  // !!! ??? что тут происходит и почему так сложно?

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

procedure TSyncthingApiV2.API_Get(const Api: UTF8String; Callback: THttpRequestCallbackFunction);
var
  key: UTF8String;
  plainKey: UTF8String;
begin
  if not Assigned(FHTTP) then Exit;

  FHTTP.Get(
    FServerURL + 'rest/' + Api, // url
    Callback,                    // callback
    '',                          // headers
    Api,                    // operationName
    nil,                         // userObject
    Api,                         // userString
    True                         // clearDuplicates
  );
end;

procedure TSyncthingApiV2.InitEndpointTable;
begin
  // Currently not needed since we resolve callback via SyncthingEndpointCallback
end;

function TSyncthingApiV2.SyncthingEndpointCallback(Id: TSyncthingEndpointId): THttpRequestCallbackFunction;
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

procedure TSyncthingApiV2.CB_CheckOnline(Request: THttpRequest);
begin
  if (Request <> nil) and (Request.Status = 200) and (Request.Connected) then
  begin
    // Is online
    Command:=ssCmdConnectingConfirmed;
    FSM_Process();
  end
  else
  begin
    Command:=ssCmdConnectingFault;
    FSM_Process();
    if Assigned(FOnConnectError) then
      FOnConnectError(Self, 'Ping failed or not connected');
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


