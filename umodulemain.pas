unit uModuleMain;

{$mode objfpc}{$H+}

interface

uses
  AsyncHttp,
  Classes, SysUtils, FileUtil, Controls, ExtCtrls, Menus, ActnList,
  UniqueInstance;

type

  { TModuleMain }

  TModuleMain = class(TDataModule)
    actShowOptions: TAction;
    ActionListGUI: TActionList;
    ImageTryIcons: TImageList;
    imgJSON: TImageList;
    menuTrayIcon: TPopupMenu;
    miExit: TMenuItem;
    miShow: TMenuItem;
    TimerUpdate: TTimer;
    TrayIcon: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    procedure actShowOptionsExecute(Sender: TObject);
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
  RegExpr,
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
  dt: TDateTime;
  re: TRegExpr;
begin
  if HttpQueryToJson(Query, JData) then
  try
    frmMain.DevicesItems.Clear();
    for ij in JData do
    begin
      onl:='';
      re := TRegExpr.Create('(\d+)-(\d+)-(\d+)T(\d+):(\d+):(\d+).*');
      if ij.Value.FindPath('lastSeen')<>nil then
      begin
        s := ij.Value.GetPath('lastSeen').Value;
        if JsonStrToDateTime(s, dt) then
          if IncSecond(dt, 120) > Now() then
            onl := '[ONLINE] ';
      end;
      frmMain.DevicesItems.Add(onl + ij.Key);
    end;
    frmMain.treeDevices.RootNodeCount:=JData.Count;
    frmMain.treeDevices.Invalidate(); //todo: <-optimize
  finally
    re.Free();
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

procedure TModuleMain.TimerUpdateTimer(Sender: TObject);
begin
  if not httpUpdateDeviceInProc and Core.Online then
  begin
    Core.aiohttp.Get(Core.SyncthigServer+'rest/stats/device', @httpUpdateDevice, '', @httpUpdateDeviceInProc);
    Core.aiohttp.Get(Core.SyncthigServer+'rest/stats/folder', @httpUpdateFolder, '', @httpUpdateFolderInProc);
  end;
end;

end.

