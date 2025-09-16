unit uModuleMain;

{$mode objfpc}{$H+}

interface

uses
  RegExpr,
  AsyncHttp,
  Classes, SysUtils, FileUtil, Controls, ExtCtrls, Menus, ActnList,
  VirtualTrees,
  UniqueInstance,
  syncthing_api;

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
    procedure actSetStateRunExecute(Sender: TObject);
    procedure actSetStateStopExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FSyncthingAPI: TSyncthingAPI; // Core Syncthing API instance (created manually)
    procedure Syn_OnConnected(Sender: TObject);
    procedure Syn_OnEvent(Sender: TObject; Event: TJSONObject);
    procedure Syn_OnTreeChanged(Sender: TObject; const Path: UTF8String);
  public

  end;

var
  ModuleMain: TModuleMain;

implementation

{$R *.lfm}

uses
  LCLIntf, //OpenURL
  Clipbrd,
  DateUtils,
  fpjson,
  uModuleCore,
  Forms,
  FormAbout,
  uSyncthingTypes,
  uFormJsonView,
  uFormOptions,
  uFormMain;

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
  for i in frmMain.treeDevices.SelectedNodes() do begin
    if Core.MapDevInfo.GetValue(Core.ListDevInfo[i^.Index], d) then begin
      CpText := CpText + #13 + d.Id;
    end;
  end;
  if (Length(CpText)>1) and (CpText[1]=#13) then
    Delete(CpText, 1, 1);
  Clipboard.AsText:=CpText;
end;

procedure TModuleMain.actConnectExecute(Sender: TObject);
begin
  // Configure endpoint and API key from Core settings
  FSyncthingAPI.SetEndpoint(
    Core.SyncthigHost,
    Core.SyncthigPort,
    Pos('https://', LowerCase(Core.SyncthigServer)) = 1
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
  OpenURL(Core.SyncthigServer);
end;

procedure TModuleMain.actSetStateRunExecute(Sender: TObject);
begin
  if (Core.State = stStopped) or (Core.State = stStopping) then
  begin
    Core.State := stLaunching;
    // TODO: WIP
  end;
end;

procedure TModuleMain.actSetStateStopExecute(Sender: TObject);
begin
  if (Core.State = stWork) or (Core.State = stLaunching) then
  begin
    Core.State := stStopping;
    // TODO: WIP
  end;
end;

procedure TModuleMain.DataModuleCreate(Sender: TObject);
begin
  // Create Syncthing API instance manually (do not place on form)
  FSyncthingAPI := TSyncthingAPI.Create(Self);
  // Bind event handlers
  FSyncthingAPI.OnConnected := @Syn_OnConnected;
  FSyncthingAPI.OnEvent := @Syn_OnEvent;
  FSyncthingAPI.OnTreeChanged := @Syn_OnTreeChanged;
end;

procedure TModuleMain.DataModuleDestroy(Sender: TObject);
begin
  // Free Syncthing API instance
  if Assigned(FSyncthingAPI) then
    FreeAndNil(FSyncthingAPI);
end;

procedure TModuleMain.Syn_OnConnected(Sender: TObject);
begin
  // TODO: add UI updates or logging on successful connection
end;

procedure TModuleMain.Syn_OnEvent(Sender: TObject; Event: TJSONObject);
begin
  // TODO: process raw event or forward to UI/log
end;

procedure TModuleMain.Syn_OnTreeChanged(Sender: TObject; const Path: UTF8String);
begin
  // TODO: react to JSON tree updates (refresh views as needed)
end;

end.

