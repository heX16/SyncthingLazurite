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
  SyncObjs,
  syncthing_api,
  uLogging;

resourcestring
  cStrLocal = ' (local)';

type

  { TModuleMain }

  TModuleMain = class(TDataModule)
    actCopySelectedDevID: TAction;
    actAbout: TAction;
    actCloseGUI: TAction;
    actConnect: TAction;
    actDisconnect: TAction;
    actShowConsole: TAction;
    actShowEvents: TAction;
    actSetStateStop: TAction;
    actSetStateRun: TAction;
    actShowWeb: TAction;
    actShowRestView: TAction;
    actShowOptions: TAction;
    ActionListGUI: TActionList;
    ImageTrayIcons: TImageList;
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
    miExit: TMenuItem;
    MenuItem22: TMenuItem;
    menuDevList: TPopupMenu;
    TimerUpdate: TTimer;
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
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FSyncthingAPI: TSyncthingAPI; // Core Syncthing API instance (created manually)

    LockJSONTree: TCriticalSection; // Critical section for JSON tree updates
    procedure Syn_OnConnected(Sender: TObject);
    procedure Syn_OnEvent(Sender: TObject; Event: TJSONObject);
    procedure Syn_OnTreeChanged(Sender: TObject; EndpointId: TSyncthingEndpointId; const Path: UTF8String);
    procedure Syn_OnBeforeTreeModify(Sender: TObject);
    procedure Syn_OnAfterTreeModify(Sender: TObject);
    procedure Syn_OnStateChanged(Sender: TObject; NewState: TSyncthingFSM_State);
  public
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
  end;

var
  ModuleMain: TModuleMain;

implementation

{$R *.lfm}

uses
  syncthing_api_utils,
  LCLIntf, //OpenURL
  Clipbrd,
  DateUtils,
  uModuleCore,
  Forms,
  FormAbout,
  uSyncthingTypes,
  uFormJsonView,
  uFormOptions,
  uFormMain,
  Graphics,
  TypInfo;

{ TModuleMain }

procedure TModuleMain.TrayIconDblClick(Sender: TObject);
begin
  frmMain.WindowState := wsNormal;
  frmMain.Show();
  frmMain.SetFocus();
end;

procedure TModuleMain.actShowOptionsExecute(Sender: TObject);
begin
  frmOptions.ShowModal();
end;

procedure TModuleMain.actCopySelectedDevIDExecute(Sender: TObject);
var
  i: PVirtualNode;
  d: TDevInfo;
  CpText: string;
begin
  CpText := '';
  // migration: commented Core usage
  // for i in frmMain.treeDevices.SelectedNodes() do begin
  //   if Core.MapDevInfo.GetValue(Core.ListDevInfo[i^.Index], d) then begin
  //     CpText := CpText + #13 + d.Id;
  //   end;
  // end;
  if (Length(CpText)>1) and (CpText[1]=#13) then
    Delete(CpText, 1, 1);
  Clipboard.AsText:=CpText;
end;

procedure TModuleMain.actConnectExecute(Sender: TObject);
begin
  //TODO: add support TLS
  // Configure endpoint and API key from Core settings
  FSyncthingAPI.SetEndpoint(
    Core.SyncthigHost,
    Core.SyncthigPort,
    false
  );
  FSyncthingAPI.SetAPIKey(Core.APIKey);

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
begin
  //TODO: !!! make function `Core.SyncthigServer()`
  //OpenURL(Core.SyncthigServer());
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


  // Create Syncthing API instance manually (do not place on form)
  LockJSONTree := TCriticalSection.Create;
  FSyncthingAPI := TSyncthingAPI.Create(Self);
  // Bind event handlers
  FSyncthingAPI.OnConnected := @Syn_OnConnected;
  FSyncthingAPI.OnEvent := @Syn_OnEvent;
  FSyncthingAPI.OnTreeChanged := @Syn_OnTreeChanged;
  FSyncthingAPI.OnBeforeTreeModify := @Syn_OnBeforeTreeModify;
  FSyncthingAPI.OnAfterTreeModify := @Syn_OnAfterTreeModify;
  FSyncthingAPI.OnStateChanged := @Syn_OnStateChanged;
end;

procedure TModuleMain.DataModuleDestroy(Sender: TObject);
begin
  // Free Syncthing API instance
  if Assigned(FSyncthingAPI) then
    FreeAndNil(FSyncthingAPI);
  if Assigned(LockJSONTree) then
    FreeAndNil(LockJSONTree);
end;

procedure TModuleMain.Syn_OnConnected(Sender: TObject);
begin
  // TODO: add UI updates or logging on successful connection
end;

procedure TModuleMain.Syn_OnEvent(Sender: TObject; Event: TJSONObject);
begin
  // TODO: process raw event or forward to UI/log
end;

procedure TModuleMain.Syn_OnTreeChanged(Sender: TObject; EndpointId: TSyncthingEndpointId; const Path: UTF8String);
  procedure ChangedEndpoint(EndpointId: TSyncthingEndpointId);
  begin
    // React to JSON tree updates: adjust devices tree node count when devices config changes
    case EndpointId of
      epConfig_Devices:
      begin
        // We are already in UI thread; update directly
        frmMain.treeDevices.RootNodeCount := FSyncthingAPI.config_devices.Count;
        frmMain.treeDevices.Invalidate();
      end;
      epConfig_Folders:
      begin
        frmMain.treeFolders.RootNodeCount := FSyncthingAPI.config_folders.Count;
        frmMain.treeFolders.Invalidate();
        // Update tray hint when folders data changes
        TrayIcon.Hint := BuildOnlineDevicesHint();
      end;
    end;
  end;
var
  ep: TSyncthingEndpointId;
begin
  DebugLog('FSyncthingAPI.OnTreeChanged: ' + 
    GetEnumName(TypeInfo(TSyncthingEndpointId), Ord(EndpointId)));

  if EndpointId <> epConfig then
    ChangedEndpoint(EndpointId)
  else
  begin
    // For epConfig notify all related config endpoints
    for ep in SyncthingEndpointsConfig do
      ChangedEndpoint(ep);
  end;
end;

procedure TModuleMain.Syn_OnBeforeTreeModify(Sender: TObject);
begin
  if Assigned(LockJSONTree) then
    LockJSONTree.Enter;
end;

procedure TModuleMain.Syn_OnAfterTreeModify(Sender: TObject);
begin
  if Assigned(LockJSONTree) then
    LockJSONTree.Leave;
end;

procedure TModuleMain.Syn_OnStateChanged(Sender: TObject; NewState: TSyncthingFSM_State);
var
  s: string;
  c: TColor;
begin
  // Print new FSM state to console
  s := GetEnumName(TypeInfo(TSyncthingFSM_State), Ord(NewState));
  DebugLog('FSyncthingAPI.OnStateChanged: ' + s);
  // Change status circle color depending on state
  case NewState of
    ssOffline:
      c := clGray; // was used previously for offline
    ssConnectingInitAndPing, ssConnectingPingWait, ssConnectingWaitData:
      c := clYellow; // connecting states
    ssOnline:
      c := TColor($00B000);  // online
    ssOnlinePaused:
      c := clSilver; // paused
    ssOnlineUnstable:
      c := clRed;    // unstable/error
    ssDisconnecting:
      c := clYellow;   // disconnecting
  else
    c := clPurple;
  end;
  frmMain.shStatusCircle.Brush.Color := c;
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
  addrField: TJSONData;
  nameStr: string;
  addrStr: string;
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

      addrField := obj.Find('addresses');
      if (addrField <> nil) and (addrField.JSONType = jtArray) and (TJSONArray(addrField).Count > 0) then
        addrStr := TJSONArray(addrField).Strings[0]
      else
        addrStr := '';

      if addrStr <> '' then
        Result := nameStr + ' ' + addrStr
      else
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

  Result := 'Online ' + IntToStr(onlineCount) + ' devices:' + LineEnding + Result;
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

end.

