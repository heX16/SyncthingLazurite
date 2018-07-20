unit uModuleMain;

{$mode objfpc}{$H+}

interface

uses
  RegExpr,
  AsyncHttp,
  Classes, SysUtils, FileUtil, Controls, ExtCtrls, Menus, ActnList,
  UniqueInstance;

type

  { TModuleMain }

  TModuleMain = class(TDataModule)
    actShowRestView: TAction;
    actShowOptions: TAction;
    ActionListGUI: TActionList;
    ImageTryIcons: TImageList;
    imgJSON: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnServer: TMenuItem;
    menuTrayIcon: TPopupMenu;
    miExit: TMenuItem;
    miShow: TMenuItem;
    TimerUpdate: TTimer;
    TrayIcon: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    procedure actShowOptionsExecute(Sender: TObject);
    procedure actShowRestViewExecute(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    httpUpdateDeviceInProc: boolean;
    httpUpdateFolderInProc: boolean;

    procedure httpUpdateDevice(Query: THttpQuery);
    procedure httpUpdateFolder(Query: THttpQuery);
  public

  end;

var
  ModuleMain: TModuleMain;

implementation

{$R *.lfm}

uses
  DateUtils,
  fpjson,
  uModuleCore,
  Forms,
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

procedure TModuleMain.httpUpdateDevice(Query: THttpQuery);
var
  JData: TJSONData;
  ij: TJSONEnum;
  s, onl: ansistring;
  n: UTF8String;
  dt: TDateTime;
var d: TDevInfo;
begin
  //todo: change logic - first update MapDevInfo and after update DevicesItems
  if HttpQueryToJson(Query, JData) then
  try
    frmMain.DevicesItems.Clear();
    for ij in JData do
    begin
      onl:='';
      if ij.Value.FindPath('lastSeen')<>nil then
      begin
        s := ij.Value.GetPath('lastSeen').Value;
        if JsonStrToDateTime(s, dt) then
          if IncSecond(dt, 120) > Now() then
            onl := '[ONLINE] ';
      end;
      // find normal name
      if Core.MapDevInfo.GetValue(ij.Key, d) then
      begin
        n := d.Name;
        //todo: update data
      end else
        n := ij.Key;
      frmMain.DevicesItems.Add(onl + n);
    end;
    frmMain.treeDevices.RootNodeCount:=JData.Count;
    frmMain.treeDevices.Invalidate(); //todo: <-optimize
  finally
    JData.Free();
  end;
end;

procedure TModuleMain.httpUpdateFolder(Query: THttpQuery);
var
  JData: TJSONData;
  ij: TJSONEnum;
begin
  if HttpQueryToJson(Query, JData) then
  try
    frmMain.FoldersItems.Clear();
    for ij in JData do
      frmMain.FoldersItems.Add(ij.Key);
    frmMain.treeFolders.RootNodeCount:=JData.Count;
  finally
    JData.Free();
  end;
end;

procedure TModuleMain.actShowOptionsExecute(Sender: TObject);
begin
  frmOptions.ShowModal();
end;

procedure TModuleMain.actShowRestViewExecute(Sender: TObject);
begin
  frmJSONView.Show();
  frmJSONView.SetFocus();
end;

procedure TModuleMain.TimerUpdateTimer(Sender: TObject);
begin
  if not httpUpdateDeviceInProc and Core.IsOnline then
    Core.aiohttp.Get(Core.SyncthigServer+'rest/stats/device', @httpUpdateDevice, '', @httpUpdateDeviceInProc);
  if not httpUpdateFolderInProc and Core.IsOnline then
    Core.aiohttp.Get(Core.SyncthigServer+'rest/stats/folder', @httpUpdateFolder, '', @httpUpdateFolderInProc);
end;

end.

