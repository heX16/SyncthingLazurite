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
    actCloseGUI: TAction;
    actStateStop: TAction;
    actStateRun: TAction;
    actShowWeb: TAction;
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
    MenuItem13: TMenuItem;
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
    miShow: TMenuItem;
    menuDevList: TPopupMenu;
    TimerUpdate: TTimer;
    TrayIcon: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    procedure actAboutExecute(Sender: TObject);
    procedure actCloseGUIExecute(Sender: TObject);
    procedure actCopySelectedDevIDExecute(Sender: TObject);
    procedure actShowOptionsExecute(Sender: TObject);
    procedure actShowRestViewExecute(Sender: TObject);
    procedure actShowWebExecute(Sender: TObject);
    procedure actStateRunExecute(Sender: TObject);
    procedure actStateStopExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    httpUpdateConnectionsInProc: boolean;
    httpUpdateDeviceStatInProc: boolean;
    httpUpdateFolderStatInProc: boolean;
    httpEventsInProc: boolean;

    // https://docs.syncthing.net/rest/events-get.html
    // https://docs.syncthing.net/dev/events.html#event-types
    // LocalChangeDetected,RemoteChangeDetected
    procedure httpEvents(Query: THttpQuery);

    procedure httpUpdateDeviceStat(Query: THttpQuery);
    procedure httpUpdateFolderStat(Query: THttpQuery);
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

procedure TModuleMain.httpEvents(Query: THttpQuery);
var
  JData: TJSONData;
  j: TJSONObject;
  j2: TJSONData;
  e: TJSONEnum;
  s: UTF8String;
begin
  //TODO: BUG 2022. AV. здесь падает. - дальше вызов FFFF происходит. че такое, хз...
  if HttpQueryToJson(Query, JData) then
  try
    for e in JData do
    begin
      //todo: httpEvents WIP!!!
      j := e.Value as TJSONObject;
      Core.EventsLastId := j.Get('globalID', 0);
      s := IntToStr(Core.EventsLastId) +' '+ j.Get('type', '');
      j2 := j.FindPath('data.folder');
      if j2<>nil then
        s := s + ' folder: "' + j2.AsString + '"' else
        s := s + ' // ' + j.AsJSON;
      frmMain.listEvents.Lines.Insert(0, s);

      //TODO: frmMain.listEvents.CaretPos - must by slide. code not working...
      frmMain.listEvents.CaretPos.SetLocation(Point(0, frmMain.listEvents.Lines.Count-1));

      while frmMain.listEvents.Lines.Count > 100 do
        frmMain.listEvents.Lines.Delete(frmMain.listEvents.Lines.Count-1);
    end;
    //ParseEvents...
    //...EventsLastId:=...
  finally
    FreeAndNil(JData);
  end;
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

procedure TModuleMain.actStateRunExecute(Sender: TObject);
begin
  if (Core.State = stStopped) or (Core.State = stStopping) then
  begin
    Core.State := stLaunching;
  end;
end;

procedure TModuleMain.actStateStopExecute(Sender: TObject);
begin
  if (Core.State = stWork) or (Core.State = stLaunching) then
  begin
    Core.State := stStopping;
  end;
end;

procedure TModuleMain.DataModuleCreate(Sender: TObject);
begin
end;

procedure TModuleMain.TimerUpdateTimer(Sender: TObject);
var
  i: Core.MapDevInfo.TIterator;
  OnlineCount: integer;
  OnlineList: string;
const
  MaxItemsInHint = 5;
begin

  if not httpUpdateConnectionsInProc and Core.IsOnline then
    Core.aiohttp.Get(Core.SyncthigServer+'rest/system/connections', @Core.httpUpdateConnections, '', @httpUpdateConnectionsInProc);

  if not httpUpdateFolderStatInProc and Core.IsOnline then
    Core.aiohttp.Get(Core.SyncthigServer+'rest/stats/folder', @httpUpdateFolderStat, '', @httpUpdateFolderStatInProc);

  //todo: уменьшить кол-во обращений
  if not httpUpdateDeviceStatInProc and Core.IsOnline then
    Core.aiohttp.Get(Core.SyncthigServer+'rest/stats/device', @httpUpdateDeviceStat, '', @httpUpdateDeviceStatInProc);

  try
    //TODO: BUG 2022. AV. - здесь падает. отсюда начинается вызов проблемной цепочки.
    if not httpEventsInProc and Core.IsOnline then
      Core.aiohttp.Get(Core.SyncthigServer+'rest/events'+'?'+
        'since='+IntToStr(Core.EventsLastId)+'&'+
        'limit='+IntToStr(10)+'&'+
        'timeout='+IntToStr(0),//+'&'+
        //'events=LocalChangeDetected,RemoteChangeDetected',
        @httpEvents, '', @httpEventsInProc);
  except
    //TODO: BUG 2022. AV. поставил заглушку. посмотрим...
    frmMain.lbExcDetected.Visible:=true;
  end;



  i := Core.MapDevInfo.Iterator();
  OnlineCount := 0;
  OnlineList := '';
  if i <> nil then
    try
      repeat
        if i.GetMutable()^.Connected then begin
          inc(OnlineCount);
          if OnlineCount <= MaxItemsInHint then
            OnlineList := OnlineList + i.Data.Value.Name + #13;
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

