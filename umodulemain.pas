unit uModuleMain;

{$mode objfpc}{$H+}

interface

uses
  RegExpr,
  AsyncHttp,
  Classes, SysUtils, FileUtil, Controls, ExtCtrls, Menus, ActnList,
  VirtualTrees,
  fpjson,
  UniqueInstance,
  syncthing_api,
  uSyncthingManager,
  uLogging;

resourcestring
  cLanguageName = 'cLanguageName';
  cLanguageNameEng = 'cLanguageNameEng';
  cProgramName = 'SyncthingLazurite';
  cStrLocal = ' (local)';

type

  { TModuleMain }

  TModuleMain = class(TDataModule)
    actCopySelectedDevID: TAction;
    actAbout: TAction;
    actCloseGUI: TAction;
    actConnect: TAction;
    actDisconnect: TAction;
    actDisconnectAndStop: TAction;
    actConnectOrStart: TAction;
    actStopSyncthing: TAction;
    actRestartApp: TAction;
    actStopAndExit: TAction;
    actShowConsole: TAction;
    actShowEvents: TAction;
    actSetStateStop: TAction;
    actSetStateRun: TAction;
    actShowWeb: TAction;
    actShowRestView: TAction;
    actShowOptions: TAction;
    ActionListGUI: TActionList;
    imgTreeViewIcon: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    mnView: TMenuItem;
    mnShowWeb: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    mnStateStop: TMenuItem;
    mnStateRun: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnRestart: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mnServer: TMenuItem;
    menuTrayIcon: TPopupMenu;
    mnStopAndExit: TMenuItem;
    menuDevList: TPopupMenu;
    TimerUpdate: TTimer;
    TimerInit: TTimer;
    TrayIcon: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    procedure actAboutExecute(Sender: TObject);
    procedure actCloseGUIExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actCopySelectedDevIDExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actShowConsoleExecute(Sender: TObject);
    procedure actShowEventsExecute(Sender: TObject);
    procedure actShowOptionsExecute(Sender: TObject);
    procedure actShowRestViewExecute(Sender: TObject);
    procedure actShowWebExecute(Sender: TObject);
    procedure actConnectOrStartExecute(Sender: TObject);
    procedure actDisconnectAndStopExecute(Sender: TObject);
    procedure actStopAndExitExecute(Sender: TObject);
    procedure actRestartAppExecute(Sender: TObject);
    procedure actStopSyncthingExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure InitEvent(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FSyncthingAPI: TSyncthingManager; // Core Syncthing Manager instance (created manually)

    procedure AddLineToEventsLog(const Line: string);
    function MapHttpErrorCodeToText(const Code: Integer): string;
    procedure Syn_OnConnected(Sender: TObject);
    procedure Syn_OnConnectError(Sender: TObject; StatusCode: Integer);
    procedure Syn_OnEvent(Sender: TObject; Event: TJSONObject);
    procedure Syn_OnTreeChanged(Sender: TObject; EndpointId: TSyncthingEndpointId; const Path: UTF8String);
    procedure Syn_OnStateChanged(Sender: TObject; NewState: TSyncthingFSM_State);

    // Manager-specific events
    procedure Mgr_OnProcessStateChanged(Sender: TObject; {%H-}State: TProcessState);
    procedure Mgr_OnConsoleOutput(Sender: TObject; const Line: UTF8String);
    procedure Mgr_OnStartFailed(Sender: TObject; const Info: TStartFailureInfo);

    // Helpers
    procedure LoadManagerSettingsFromOptions;
    procedure UpdateActionsEnabled;
    function GetPortFromOptions: Integer;
  public
    Initialized: Boolean;

    // Returns display text for folder node by index using current JSON tree
    function GetFolderDisplayText(const Index: Integer): string;
    // Returns display text for device node by index using current JSON tree
    function GetDeviceDisplayText(const Index: Integer): string;
    // Returns icon index for device node (0=offline,1=online,2=paused)
    function GetDeviceIconIndex(const Index: Integer): Integer;
    // Returns hint text for device node
    function GetDeviceHint(const Index: Integer): string;
    function BuildOnlineDevicesHint: string;
    // Returns system version string from JSON tree (empty if not available)
    function GetSystemVersionString: string;
    // Returns API key from options (empty if not set)
    function GetAPIKey: string;
  end;

var
  ModuleMain: TModuleMain;

implementation

{$R *.lfm}

uses
  uModulePreInit_i18n,
  syncthing_api_utils,
  LCLIntf, //OpenURL
  Clipbrd,
  DateUtils,
  Forms,
  FormAbout,
  uSyncthingTypes,
  uFormJsonView,
  uFormOptions,
  uFormMain,
  Graphics,
  TypInfo,
  LCLType,
  LCLTranslator,
  DefaultTranslator;


{ TModuleMain }

procedure TModuleMain.InitEvent(Sender: TObject);
begin
  // Initialization program
  Self.TimerInit.Enabled:=False;

  if not Self.Initialized then
  begin
    // Code of initialization

    // Empty

    Self.Initialized := True;
  end;
end;

procedure TModuleMain.AddLineToEventsLog(const Line: string);
var
  timeStr: string;
begin
  // Add time in HH:MM:SS format to the beginning of the line
  timeStr := FormatDateTime('hh:nn:ss', Now);
  frmMain.listEvents.Items.Insert(0, '[' + timeStr + '] ' + Line);
  while frmMain.listEvents.Items.Count > 100 do
    frmMain.listEvents.Items.Delete(frmMain.listEvents.Items.Count - 1);
end;

procedure TModuleMain.TrayIconDblClick(Sender: TObject);
begin
  frmMain.WindowState := wsNormal;
  frmMain.Show();
  frmMain.SetFocus();
end;

procedure TModuleMain.actShowOptionsExecute(Sender: TObject);
var
  modalRes: Integer;
  langFile: string;
  langCode: string;
  langDir: string;
begin
  modalRes := frmOptions.ShowModal();
  if modalRes = mrOK then
  begin
    langFile := frmOptions.cbLanguages.Text;
    if langFile <> '' then
    begin
      langCode := GetLangCode(langFile);
      langDir := GetLangDir;
      SetDefaultLang(langCode, langDir);
    end;
  end;
end;

procedure TModuleMain.actCopySelectedDevIDExecute(Sender: TObject);
var
  //i: PVirtualNode;
  //d: TDevInfo;
  CpText: string;
begin
  CpText := '';
  // TODO: WIP - actCopySelectedDevID
  // for i in frmMain.treeDevices.SelectedNodes() do begin
  //   if Dev[i^.Index] then
  //   begin
  //     CpText := CpText + #13 + d.Id;
  //   end;
  // end;
  if (Length(CpText)>1) and (CpText[1]=#13) then
    Delete(CpText, 1, 1);
  Clipboard.AsText:=CpText;
end;

procedure TModuleMain.actConnectExecute(Sender: TObject);
var
  host: UTF8String;
  port: Integer;
  useTLS: Boolean;
  apiKey: UTF8String;
begin
  // Configure endpoint and API key from Options (no Core dependency)
  host := '127.0.0.1';
  port := GetPortFromOptions;
  //TODO: add support TLS
  useTLS := false;
  if Assigned(frmOptions) then
    apiKey := UTF8String(frmOptions.edAPIKey.Text)
  else
    apiKey := '';

  FSyncthingAPI.SetEndpoint(host, port, useTLS);
  if apiKey <> '' then
    FSyncthingAPI.SetAPIKey(apiKey);

  // Connect to Syncthing
  FSyncthingAPI.Connect();
end;

procedure TModuleMain.actDisconnectExecute(Sender: TObject);
begin
  FSyncthingAPI.Disconnect();
end;

procedure TModuleMain.actShowConsoleExecute(Sender: TObject);
begin
  if frmMain.edConsole.Visible then
  begin
    frmMain.edConsole.Visible := false;
    frmMain.SplitterBottom1.Visible := false;
  end else
  begin
    frmMain.SplitterBottom1.Visible := true;
    frmMain.edConsole.Visible := true;
  end;
end;

procedure TModuleMain.actShowEventsExecute(Sender: TObject);
begin
  if frmMain.grpEvents.Visible then
  begin
    frmMain.grpEvents.Visible := false;
    frmMain.SplitterEventsBottom.Visible := false;
  end else
  begin
    frmMain.SplitterEventsBottom.Visible := true;
    frmMain.grpEvents.Visible := true;
  end;
end;

procedure TModuleMain.actAboutExecute(Sender: TObject);
var f: TfrmAbout;
begin
  f := TfrmAbout.Create(self);
  f.ShowModal();
  FreeAndNil(f);
end;

procedure TModuleMain.actCloseGUIExecute(Sender: TObject);
begin
  Application.Terminate();
end;

procedure TModuleMain.actShowRestViewExecute(Sender: TObject);
begin
  frmJSONView.Show();
  frmJSONView.SetFocus();
end;

procedure TModuleMain.actShowWebExecute(Sender: TObject);
var
  port: Integer;
begin
  // Open Web UI using options (defaults to http://127.0.0.1:8384/)
  // Note: TLS is not used at this stage
  // TODO: need update this code - build URL from real data
  port := GetPortFromOptions;
  OpenURL(Format('http://127.0.0.1:%d/', [port]));
end;

procedure TModuleMain.DataModuleCreate(Sender: TObject);
begin
  //todo: !!! i18n WIP. - нужно подумать как сделать. инструкция для GPT: спроси меня прежде чем писать код.
  {strs:=TStringList.Create;
  GetFiles(ExtractFilePath(Application.ExeName)+'languages' + PathDelim, strs, '*.po');
  frmMain.cbLanguage.Items.Assign(strs);
  strs.Free;}
  //SetDefaultLang('ru');
  //SetDefaultLang(GetOSLanguage()); // - just use DefaultTranslator module
  //frmMain.LanguageChanged();


  // Create Syncthing Manager instance manually (do not place on form)
  FSyncthingAPI := TSyncthingManager.Create(Self);
  // Bind API event handlers
  FSyncthingAPI.OnConnected := @Syn_OnConnected;
  FSyncthingAPI.OnConnectError := @Syn_OnConnectError;
  FSyncthingAPI.OnEvent := @Syn_OnEvent;
  FSyncthingAPI.OnTreeChanged := @Syn_OnTreeChanged;
  FSyncthingAPI.OnStateChanged := @Syn_OnStateChanged;
  // Bind Manager-specific handlers
  FSyncthingAPI.OnProcessStateChanged := @Mgr_OnProcessStateChanged;
  FSyncthingAPI.OnConsoleOutput := @Mgr_OnConsoleOutput;
  FSyncthingAPI.OnStartFailed := @Mgr_OnStartFailed;

  UpdateActionsEnabled;
end;

procedure TModuleMain.DataModuleDestroy(Sender: TObject);
begin
  // Free Syncthing API instance
  if Assigned(FSyncthingAPI) then
  begin
    // TODO: WIP... - я думаю останавливать процессы не нужно.
    // FSyncthingAPI.StopAllProcesses;
    FreeAndNil(FSyncthingAPI);
  end;
end;

procedure TModuleMain.Syn_OnConnected(Sender: TObject);
begin
  // TODO: add UI updates or logging on successful connection
end;

function TModuleMain.MapHttpErrorCodeToText(const Code: Integer): string;
begin
  case Code of
    HTTPErrorCode_SocketHostNotFound:
      Exit('Host not found');
    HTTPErrorCode_SocketCreationFailed:
      Exit('Socket creation failed');
    HTTPErrorCode_SocketConnectFailed:
      Exit('Socket connect failed');
    HTTPErrorCode_SocketConnectTimeout:
      Exit('Connect timeout - syncthing is disabled or unavailable');
    HTTPErrorCode_SocketIOTimeout:
      Exit('Socket I/O timeout');
    HTTPErrorCode_UnknownException:
      Exit('Unknown exception in HTTP client');
    HTTPErrorCode_HTTPClientException:
      Exit('HTTP client exception');
    HTTPErrorCode_Disconnected:
      Exit('Connection was disconnected');
    HTTPErrorCode_KeepAliveEnded:
      Exit('Keep-Alive connection was closed by server');
  else
    if (Code >= 100) and (Code <= 599) then
      Exit('HTTP status ' + IntToStr(Code))
    else
      Exit('Unknown error (' + IntToStr(Code) + ')');
  end;
end;

procedure TModuleMain.Syn_OnConnectError(Sender: TObject; StatusCode: Integer);
var
  msg: string;
begin
  msg := MapHttpErrorCodeToText(StatusCode);
  AddLineToEventsLog('Connect error: ' + msg + '  [' + IntToStr(StatusCode) + ']');
end;

procedure TModuleMain.Syn_OnEvent(Sender: TObject; Event: TJSONObject);
var
  j2: TJSONData;
  s, info: UTF8String;
  id: Int64;
  eventType, eventName: UTF8String;
  jsonStr: UTF8String;
begin
  // Build short human-readable line for visual log
  info := '';
  j2 := Event.FindPath('data.folder');
  if j2 <> nil then
    info := 'folder:' + j2.AsString;

  id := Event.Get('id', 0);
  eventType := Event.Get('type', '');
  eventName := Event.Get('name', '');

  s := Format('id:%d;  type:%s  name:%s  %s', [id, eventType, eventName, info]);

  // Append compact JSON dump (truncated to 255 chars)
  try
    jsonStr := Event.AsJSON;
    // Use AsJSON for compact single-line representation
    // Fallback to FormatJSON if needed
    if Length(jsonStr) = 0 then
      jsonStr := Event.FormatJSON();
    if Length(jsonStr) > 255 then
      SetLength(jsonStr, 255);
    s := s + '  json:' + jsonStr;
  except
    on E: Exception do
      s := s + '  json:<error>';
  end;

  // Add to visual log
  AddLineToEventsLog(s);
end;

procedure TModuleMain.Syn_OnTreeChanged(Sender: TObject; EndpointId: TSyncthingEndpointId; const Path: UTF8String);
  procedure ChangedEndpoint(EndpointId: TSyncthingEndpointId);
  begin
    // React to JSON tree updates: adjust devices tree node count when devices config changes
    case EndpointId of
      epConfig_Devices, epStats_Device:
      begin
        // We are already in UI thread; update directly
        frmMain.treeDevices.RootNodeCount := FSyncthingAPI.config_devices.Count;
        //frmMain.treeDevices.Repaint;
        frmMain.treeDevices.Invalidate();
        // Update tray hint when device data changes
        TrayIcon.Hint := BuildOnlineDevicesHint();
        {$IfDef DEBUG}
        AddLineToEventsLog('UPD: Devices');
        {$EndIf}
      end;
      epConfig_Folders, epStats_Folder:
      begin
        frmMain.treeFolders.RootNodeCount := FSyncthingAPI.config_folders.Count;
        //frmMain.treeFolders.Repaint;
        frmMain.treeFolders.Invalidate();
        {$IfDef DEBUG}
        AddLineToEventsLog('UPD: Folders');
        {$EndIf}
      end;
    end;
  end;
var
  ep: TSyncthingEndpointId;
  {$IfDef DEBUG}
  logStr: string;
  {$EndIf}
begin
  {$IfDef DEBUG}
  DebugLog('FSyncthingAPI.OnTreeChanged: ' +
    GetEnumName(TypeInfo(TSyncthingEndpointId), Ord(EndpointId)));

  // Add TreeChanged entry to visual log with JSON path
  logStr := 'TreeChanged: ' + GetEnumName(TypeInfo(TSyncthingEndpointId), Ord(EndpointId)) +
    '  JSONTreePath:' + Path;
  AddLineToEventsLog(logStr);
  {$EndIf}

  if EndpointId <> epConfig then
    ChangedEndpoint(EndpointId)
  else
  begin
    // For epConfig notify all related config endpoints
    for ep in SyncthingEndpointsConfig do
      ChangedEndpoint(ep);
  end;
end;

procedure TModuleMain.Syn_OnStateChanged(Sender: TObject; NewState: TSyncthingFSM_State);
var
  {$IfDef DEBUG}
  s: string;
  {$EndIf}
  c: TColor;
  statusMessage: string;
begin
  self.actConnect.Enabled:=true;
  self.actDisconnect.Enabled:=true;

  {$IfDef DEBUG}
  // Print new FSM state to console
  s := GetEnumName(TypeInfo(TSyncthingFSM_State), Ord(NewState));
  DebugLog('FSyncthingAPI.OnStateChanged: ' + s);
  
  // Add status change to events log
  statusMessage := 'Status changed: ' + s;
  AddLineToEventsLog(statusMessage);
  {$EndIf}

  // Change status circle color depending on state
  case NewState of
    ssOffline: begin
      c := clGray; // was used previously for offline
      self.actDisconnect.Enabled:=false;
    end;

    ssConnectingInitAndPing, ssConnectingPingWait, ssConnectingWaitData:
      c := clYellow; // connecting states

    ssOnline, ssOnlineLongPollingWait: begin
      c := TColor($00B000);  // online
      self.actConnect.Enabled:=false;
    end;

    ssOnlinePaused:
      c := clSilver; // paused

    ssOnlineUnstable, ssOnlineUnstableLongPollingWait:
      c := clRed;    // unstable/error

    ssDisconnecting:
      c := clYellow;   // disconnecting
  else
    c := clPurple;
  end;
  frmMain.shStatusCircle.Brush.Color := c;
  UpdateActionsEnabled;
end;

function TModuleMain.GetFolderDisplayText(const Index: Integer): string;
var
  obj: TJSONObject;
  nameField: TJSONData;
  dirField: TJSONData;
  nameStr: string;
  pathStr: string;
begin
  Result := '';
  if (Index >= 0) and (Index < FSyncthingAPI.config_folders.Count) then
  begin
    obj := FSyncthingAPI.config_folders.Objects[Index];
    if obj <> nil then
    begin
      nameField := obj.Find('label');
      if (nameField = nil) or (nameField.AsString = '') then
        nameField := obj.Find('id');

      if (nameField <> nil) and (nameField.JSONType = jtString) then
        nameStr := nameField.AsString
      else
        nameStr := '(error)';

      begin
        // TODO: делать эту проверку самому не нужно - syncthing делает ее, можно смотреть особый параметр в jsontree
        dirField := obj.Find('path');
        if (dirField <> nil) and (dirField.JSONType = jtString) then
          pathStr := dirField.AsString
        else
          pathStr := '';

        if (pathStr <> '') and (not DirectoryExists(pathStr)) then
          Result := 'NOT FOUND! ' + nameStr
        else
          Result := nameStr;
      end;
    end;
  end;
end;

function TModuleMain.GetDeviceDisplayText(const Index: Integer): string;
var
  obj: TJSONObject;
  nameField: TJSONData;
  nameStr: string;
begin
  Result := '';
  if (Index >= 0) and (Index < FSyncthingAPI.config_devices.Count) then
  begin
    obj := FSyncthingAPI.config_devices.Objects[Index];
    if obj <> nil then
    begin
      nameField := obj.Find('name');
      if (nameField = nil) or (nameField.JSONType <> jtString) or (Trim(nameField.AsString) = '') then
        nameField := obj.Find('deviceID');
      if (nameField <> nil) and (nameField.JSONType = jtString) then
        nameStr := nameField.AsString
      else
        nameStr := '';

      Result := nameStr;
    end;
  end;
end;

function TModuleMain.GetDeviceIconIndex(const Index: Integer): Integer;
var
  dev: TJSONObject;
  pausedField: TJSONData;
  deviceId: string;
  connObj: TJSONObject;
  connected: Boolean;
begin
  // Defaults to offline
  Result := 0;
  if (Index < 0) or (Index >= FSyncthingAPI.config_devices.Count) then Exit;

  dev := FSyncthingAPI.config_devices.Objects[Index];
  if dev = nil then Exit;

  // paused flag from config
  pausedField := dev.Find('paused');
  if (pausedField <> nil) and (pausedField.JSONType = jtBoolean) and pausedField.AsBoolean then
    Exit(2);

  // check connection status in system.connections if available in tree root
  deviceId := dev.Get('deviceID', '');
  connected := False;
  if (deviceId <> '') then
  begin
    connObj := FSyncthingAPI.TreeRoot.FindPath('system.connections.connections.' + deviceId) as TJSONObject;
    if connObj <> nil then
      connected := connObj.Get('connected', False);
  end;

  if connected then
    Result := 1
  else
    Result := 0;
end;

function TModuleMain.GetDeviceHint(const Index: Integer): string;
var
  dev: TJSONObject;
  nameStr, deviceId, addrStr: string;
  lastSeenStr: string;
  lsData: TJSONData;
  addresses: TJSONArray;
  p: integer;
begin
  Result := '';
  if (Index < 0) or (Index >= FSyncthingAPI.config_devices.Count) then Exit;

  dev := FSyncthingAPI.config_devices.Objects[Index];
  if dev = nil then Exit;

  nameStr := dev.Get('name', '');
  if nameStr = '' then
    nameStr := dev.Get('deviceID', '');
  deviceId := dev.Get('deviceID', '');

  // try to take first address from config
  addrStr := '';
  addresses := dev.Arrays['addresses'];
  if (addresses <> nil) and (addresses.Count > 0) then
    addrStr := addresses.Strings[0];

  // lastSeen from JSON tree at stats.device.<id>.lastSeen (use value as-is)
  lastSeenStr := '';
  if deviceId <> '' then
  begin
    lsData := FSyncthingAPI.TreeRoot.FindPath('stats.device.' + deviceId + '.lastSeen');
    if (lsData <> nil) and (lsData.JSONType = jtString) then
      lastSeenStr := lsData.AsString;
  end;

  Result := nameStr;
  if lastSeenStr <> '' then
  begin
    // Format lastSeen: replace 'T' with space and cut timezone suffix starting with '+'
    lastSeenStr := StringReplace(lastSeenStr, 'T', ' ', []);
    p := Pos('+', lastSeenStr);
    if p > 0 then
      lastSeenStr := Copy(lastSeenStr, 1, p - 1);
    Result := Result + LineEnding + 'lastSeen: ' + lastSeenStr;
  end;
  if addrStr <> '' then
    Result := Result + LineEnding + 'Address: ' + addrStr;
end;

function TModuleMain.BuildOnlineDevicesHint: string;
const
  MaxItemsInHint = 5;
var
  i, shown: Integer;
  onlineCount: Integer;
  dev: TJSONObject;
  nameStr: string;
  addrStr: string;
  addresses: TJSONArray;
  deviceId: string;
  connected: Boolean;
  connObj: TJSONObject;
begin
  onlineCount := 0;
  shown := 0;
  Result := '';
  for i := 0 to FSyncthingAPI.config_devices.Count - 1 do
  begin
    dev := FSyncthingAPI.config_devices.Objects[i];
    if dev = nil then
      continue;

    // determine connection state via system.connections
    deviceId := dev.Get('deviceID', '');
    connected := False;
    if deviceId <> '' then
    begin
      connObj := FSyncthingAPI.TreeRoot.FindPath('system.connections.connections.' + deviceId) as TJSONObject;
      if connObj <> nil then
        connected := connObj.Get('connected', False);
    end;

    if connected then
    begin
      Inc(onlineCount);
      if shown < MaxItemsInHint then
      begin
        nameStr := dev.Get('name', '');
        if nameStr = '' then
          nameStr := deviceId;

        addrStr := '';
        addresses := dev.Arrays['addresses'];
        if (addresses <> nil) and (addresses.Count > 0) then
          addrStr := addresses.Strings[0];

        if (addrStr <> '') and IsLocalIP(addrStr) then
          nameStr := nameStr + cStrLocal;

        Result := Result + nameStr + LineEnding;
        Inc(shown);
      end;
    end;
  end;

  if onlineCount > MaxItemsInHint then
    Result := Result + '...' + LineEnding;

  if onlineCount = 0 then
    Result := cProgramName
  else
    Result :=
           cProgramName + LineEnding +
           'Online ' + IntToStr(onlineCount) + ' devices:' + LineEnding +
           Result;
end;

function TModuleMain.GetSystemVersionString: string;
var
  d: TJSONData;
  ver: string;
begin
  // Read version from in-memory JSON tree: system.version.version
  d := FSyncthingAPI.TreeRoot.FindPath('system.version.version');
  if (d <> nil) and (d.JSONType = jtString) then
    ver := d.AsString
  else
    ver := '';
  Result := ver;
end;

function TModuleMain.GetAPIKey: string;
begin
  if Assigned(frmOptions) then
    Result := frmOptions.edAPIKey.Text
  else
    Result := '';
end;

function TModuleMain.GetPortFromOptions: Integer;
const
  DEFAULT_PORT = 8384;
  MIN_PORT = 1;
  MAX_PORT = 65535;
var
  portStr: string;
  portValue: Integer;
begin
  Result := DEFAULT_PORT;
  
  if Assigned(frmOptions) then
  begin
    portStr := Trim(frmOptions.edPortNumber.Text);
    if portStr <> '' then
    begin
      portValue := StrToIntDef(portStr, -1);
      if (portValue >= MIN_PORT) and (portValue <= MAX_PORT) then
        Result := portValue;
    end;
  end;
end;

procedure TModuleMain.Mgr_OnProcessStateChanged(Sender: TObject; State: TProcessState);
begin
  UpdateActionsEnabled;
end;

procedure TModuleMain.Mgr_OnConsoleOutput(Sender: TObject; const Line: UTF8String);
begin
  // TODO: над этой функцией нужно поработать. кжется тут дублирование кода и лишний функционал.
  //       `Length(Line) > 255` - вот это я думаю можно перенести? или оставить... подумать
  if frmMain.edConsole.Lines.Count > 500 then
    frmMain.edConsole.Lines.Delete(0);
  frmMain.edConsole.Lines.Add(string(Line));
  // Also duplicate a compact line into events log
  if Length(Line) > 255 then
    AddLineToEventsLog(Copy(string(Line), 1, 255) + '...')
  else
    AddLineToEventsLog(string(Line));
end;

procedure TModuleMain.Mgr_OnStartFailed(Sender: TObject; const Info: TStartFailureInfo);
var
  msg: UTF8String;
begin
  msg := 'Failed to start Syncthing';
  if Info.IsException and (Info.ErrorMessage <> '') then
    msg := msg + ': ' + Info.ErrorMessage
  else if Info.OSLastError <> 0 then
    msg := msg + ' (OS err=' + UTF8String(IntToStr(Info.OSLastError)) + ')';
  AddLineToEventsLog(string(msg));
end;

procedure TModuleMain.LoadManagerSettingsFromOptions;
var
  host: UTF8String;
  port: Integer;
  useTLS: Boolean;
  apiKey: UTF8String;
  execPath, homePath: UTF8String;
begin
  host := '127.0.0.1';
  port := GetPortFromOptions;
  useTLS := false;

  if Assigned(frmOptions) then
  begin
    execPath := UTF8String(frmOptions.edPathToExecWithFilename.Text);
    homePath := UTF8String(frmOptions.edPathToConfigDir.Text);
    apiKey := UTF8String(frmOptions.edAPIKey.Text);
  end
  else
  begin
    execPath := '';
    homePath := '';
    apiKey := '';
  end;

  if execPath <> '' then
    FSyncthingAPI.ExecPath := execPath;
  if homePath <> '' then
    FSyncthingAPI.HomePath := homePath;
  if apiKey <> '' then
    FSyncthingAPI.SetAPIKey(apiKey)
  else
    FSyncthingAPI.LoadConfigFromDisk;

  FSyncthingAPI.SetEndpoint(host, port, useTLS);
end;

procedure TModuleMain.UpdateActionsEnabled;
var
  running: Boolean;
  online: Boolean;
begin
  running := False;
  if Assigned(FSyncthingAPI) then
    running := FSyncthingAPI.IsProcessRunning;

  online := False;
  if Assigned(FSyncthingAPI) then
    online := FSyncthingAPI.IsOnline;

  self.actConnect.Enabled := not online;
  self.actDisconnect.Enabled := online;
  self.actConnectOrStart.Enabled := not running;
  self.actDisconnectAndStop.Enabled := running;
  self.actStopAndExit.Enabled := running;
end;

procedure TModuleMain.actConnectOrStartExecute(Sender: TObject);
begin
  if not FSyncthingAPI.IsProcessRunning then
  begin
    LoadManagerSettingsFromOptions;
    FSyncthingAPI.StartSyncthingProcess;
  end
  else
    actConnectExecute(nil);
end;

procedure TModuleMain.actDisconnectAndStopExecute(Sender: TObject);
begin
  FSyncthingAPI.Disconnect;
  FSyncthingAPI.StopSyncthingProcess;
end;

procedure TModuleMain.actStopAndExitExecute(Sender: TObject);
begin
  FSyncthingAPI.Disconnect;
  FSyncthingAPI.StopSyncthingProcess;
  Application.Terminate;
end;

procedure TModuleMain.actRestartAppExecute(Sender: TObject);
begin
  FSyncthingAPI.Disconnect;
  FSyncthingAPI.StopSyncthingProcess;
  LoadManagerSettingsFromOptions;
  FSyncthingAPI.StartSyncthingProcess;
end;

procedure TModuleMain.actStopSyncthingExecute(Sender: TObject);
begin
  // Request graceful shutdown via REST API (no forced terminate)
  if Assigned(FSyncthingAPI) and FSyncthingAPI.IsOnline then
  begin
    FSyncthingAPI.API_Post('system/shutdown', nil, '');
    AddLineToEventsLog('Shutdown requested via API');
  end
  else
    AddLineToEventsLog('Cannot shutdown via API: not online');
end;

end.

