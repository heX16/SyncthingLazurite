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
  fpjson,
  uModuleCore,
  Forms,
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
begin
  (* example:
    {
      "HJBNI74-LB5I7ND-IDXKOJH-CMM5KM3-AR4BMVB-XOXIJAB-FSYCOFN-3EBH7AS" : {
        "lastSeen" : "1970-01-01T03:00:00+03:00"
      },
      ...
  *)
  if HttpQueryToJson(Query, JData) then
  try
    frmMain.lbDevices.Clear();
    for ij in JData do
      frmMain.lbDevices.AddItem(ij.Key, nil);
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
    begin
      frmMain.FoldersItems.Add(ij.Key);
    end;
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

