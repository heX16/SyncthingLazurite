unit uModuleMain;

{$mode objfpc}{$H+}

interface

uses
  RegExpr,
  AsyncHttp,
  Classes, SysUtils, FileUtil, Controls, ExtCtrls, Menus, ActnList,
  VirtualTrees,
  UniqueInstance;

resourcestring
  cStrLocal = ' (local)';

type

  { TModuleMain }

  TModuleMain = class(TDataModule)
    actCopySelectedDevID: TAction;
    actAbout: TAction;
    actCloseGUI: TAction;
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
    procedure actCopySelectedDevIDExecute(Sender: TObject);
    procedure actShowConsoleExecute(Sender: TObject);
    procedure actShowEventsExecute(Sender: TObject);
    procedure actShowOptionsExecute(Sender: TObject);
    procedure actShowRestViewExecute(Sender: TObject);
    procedure actShowWebExecute(Sender: TObject);
    procedure actSetStateRunExecute(Sender: TObject);
    procedure actSetStateStopExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    procedure httpUpdateDeviceStat(Request: THttpRequest);
    procedure httpUpdateFolderStat(Request: THttpRequest);
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

// todo: move to Core
procedure TModuleMain.httpUpdateDeviceStat(Request: THttpRequest);
var
  JData: TJSONData;
  ij: TJSONEnum;
  s: ansistring;
  dt: TDateTime;
  d: TDevInfo;
begin
  if HttpRequestToJson(Request, JData) then
  try
    // enum all device
    for ij in JData do
    begin
      // find in device list.
      if Core.MapDevInfo.GetValue(ij.Key, d) then
      begin
        // update data
        if ij.Value.FindPath('lastSeen')<>nil then
        begin
          s := ij.Value.GetPath('lastSeen').AsString;
          if JsonStrToDateTime(s, dt) then
            d.LastSeen:=dt;
        end;
        //todo: use 'mutable' ptr
        Core.MapDevInfo[ij.Key] := d;
      end;
    end;
  finally
    FreeAndNil(JData);
  end;
end;

// todo: move to Core
procedure TModuleMain.httpUpdateFolderStat(Request: THttpRequest);
var
  JData: TJSONData;
  ij: TJSONEnum;
begin
  if HttpRequestToJson(Request, JData) then
  try
    // TODO: WIP...
  finally
    JData.Free();
  end;
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
  end;
end;

procedure TModuleMain.actSetStateStopExecute(Sender: TObject);
begin
  if (Core.State = stWork) or (Core.State = stLaunching) then
  begin
    Core.State := stStopping;
  end;
end;

procedure TModuleMain.DataModuleCreate(Sender: TObject);
begin
end;

// TODO: Move to core
procedure TModuleMain.TimerUpdateTimer(Sender: TObject);
var
  i: Core.MapDevInfo.TIterator;
  OnlineCount: integer;
  OnlineList: string;
  DeviceName: string;
  DeviceAddr: string;
const
  MaxItemsInHint = 5;
begin

  if Core.IsOnline and not Core.aiohttp.RequestInQueue('system/connections') then
    Core.API_Get('system/connections', @Core.httpUpdateConnections);

  if Core.IsOnline and not Core.aiohttp.RequestInQueue('stats/folder') then
    Core.API_Get('stats/folder', @httpUpdateFolderStat);

  if Core.IsOnline and not Core.aiohttp.RequestInQueue('stats/device') then
    Core.API_Get('stats/device', @httpUpdateDeviceStat);

  i := Core.MapDevInfo.Iterator();
  OnlineCount := 0;
  OnlineList := '';
  if i <> nil then
    try
      repeat
        if i.GetMutable()^.Connected then begin
          inc(OnlineCount);
          if OnlineCount <= MaxItemsInHint then
          begin
            DeviceName := i.Data.Value.Name;
            DeviceAddr := i.Data.Value.Address;
            if IsLocalIP(DeviceAddr) then
              DeviceName := DeviceName + cStrLocal;
            OnlineList := OnlineList + DeviceName + #13;
          end;
        end;
      until not i.Next;
    finally
      FreeAndNil(i);
    end;
  if OnlineCount > MaxItemsInHint then
    OnlineList := OnlineList + '...' + #13;
  TrayIcon.Hint:='Online ' + IntToStr(OnlineCount) + ' devices:' + #13 + OnlineList;
end;

end.

