unit uModuleMain;

{$mode objfpc}{$H+}

interface

uses
  RegExpr,
  AsyncHttp,
  Classes, SysUtils, FileUtil, Controls, ExtCtrls, Menus, ActnList,
  VirtualTrees,
  UniqueInstance;

type

  { TModuleMain }

  TModuleMain = class(TDataModule)
    actCopySelectedDevID: TAction;
    actAbout: TAction;
    actShowRestView: TAction;
    actShowOptions: TAction;
    ActionListGUI: TActionList;
    ImageTrayIcons: TImageList;
    imgJSON: TImageList;
    imgTreeViewIcon: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
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
    miShow: TMenuItem;
    menuDevList: TPopupMenu;
    TimerUpdate: TTimer;
    TrayIcon: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    procedure actAboutExecute(Sender: TObject);
    procedure actCopySelectedDevIDExecute(Sender: TObject);
    procedure actShowOptionsExecute(Sender: TObject);
    procedure actShowRestViewExecute(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    httpUpdateConnectionsInProc: boolean;
    httpUpdateDeviceStatInProc: boolean;
    httpUpdateFolderStatInProc: boolean;

    procedure httpUpdateDeviceStat(Query: THttpQuery);
    procedure httpUpdateFolderStat(Query: THttpQuery);
  public

  end;

var
  ModuleMain: TModuleMain;

implementation

{$R *.lfm}

uses
  Clipbrd,
  DateUtils,
  fpjson,
  uModuleCore,
  Forms,
  FormAbout,
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

procedure TModuleMain.httpUpdateDeviceStat(Query: THttpQuery);
var
  JData: TJSONData;
  ij: TJSONEnum;
  s: ansistring;
  dt: TDateTime;
  d: TDevInfo;
begin
  //todo: httpUpdateDeviceStat - move to Core!
  if HttpQueryToJson(Query, JData) then
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

procedure TModuleMain.httpUpdateFolderStat(Query: THttpQuery);
var
  JData: TJSONData;
  ij: TJSONEnum;
begin
  //todo: httpUpdateDevice - move to Core!
  if HttpQueryToJson(Query, JData) then
  try
    {
    frmMain.FoldersItems.Clear();
    for ij in JData do
      frmMain.FoldersItems.Add(ij.Key);
    frmMain.treeFolders.RootNodeCount:=JData.Count;
    }
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

procedure TModuleMain.actAboutExecute(Sender: TObject);
var f: TfrmAbout;
begin
  f := TfrmAbout.Create(self);
  f.ShowModal();
  FreeAndNil(f);
end;

procedure TModuleMain.actShowRestViewExecute(Sender: TObject);
begin
  frmJSONView.Show();
  frmJSONView.SetFocus();
end;

procedure TModuleMain.TimerUpdateTimer(Sender: TObject);
var
  i: Core.MapDevInfo.TIterator;
  OnlineCount: integer;
begin

  if not httpUpdateConnectionsInProc and Core.IsOnline then
    Core.aiohttp.Get(Core.SyncthigServer+'rest/system/connections', @Core.httpUpdateConnections, '', @httpUpdateConnectionsInProc);

  if not httpUpdateFolderStatInProc and Core.IsOnline then
    Core.aiohttp.Get(Core.SyncthigServer+'rest/stats/folder', @httpUpdateFolderStat, '', @httpUpdateFolderStatInProc);

  //todo: уменьшить кол-во обращений
  if not httpUpdateDeviceStatInProc and Core.IsOnline then
    Core.aiohttp.Get(Core.SyncthigServer+'rest/stats/device', @httpUpdateDeviceStat, '', @httpUpdateDeviceStatInProc);

  i := Core.MapDevInfo.Iterator();
  OnlineCount := 0;
  if i <> nil then
    try
      repeat
        if i.GetMutable()^.Connected then
          inc(OnlineCount);
      until not i.Next;
    finally
      FreeAndNil(i);
    end;
  TrayIcon.Hint:='Online ' + IntToStr(OnlineCount);
end;

end.

