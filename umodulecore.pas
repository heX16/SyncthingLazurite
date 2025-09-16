unit uModuleCore;

{$mode objfpc}{$H+}

interface

uses
  AsyncHttp,
  uSyncthingTypes,
  Dialogs,
  fpjson,
  XMLRead, DOM,
  LazUTF8,
  Classes, SysUtils, UTF8Process, ExtCtrls, ActnList, Forms;

type
  TCoreState = (
    stUnknown,
    // execute fault
    stFaultAndDisconnected,
    // detect: connect to present syncthing or execure syncthing
    stConnectOrStart,
    // syncthing disabled
    stStopped,
    // GUI will paused, wait disconnecting the long polling
    stDisconnectingGUI,
    // syncthinc still working, but GUI paused
    stDisconnectedGUI,
    // execute syncthinc app
    stLaunching,
    // wait ack from syncthinc app
    stLaunchingWait,
    // online - syncthinc working, GUI working
    stWork,
    // shutdown and start (restart)
    stShutdownAndStart,
    // shutdown and start, wait ack (restart)
    stShutdownAndStartWait,
    // send stop command
    stStopping,
    // wait stopping ack
    stStoppingWait);


type

  TAddConsoleLine = procedure (Line: UTF8String) of object;

  { TCore }

  //todo: extract real core code to 'model'(or 'control') and 'utils'
  TCore = class(TDataModule)
    actConnect: TAction;
    actStartOrConnect: TAction;
    actUpdateData: TAction;
    actStopAndExit: TAction;
    actInit: TAction;
    actReloadConfig: TAction;
    actRunSupportProc: TAction;
    actTerminate: TAction;
    actRestart: TAction;
    actStop: TAction;
    actStartAndConnect: TAction;
    ActionList: TActionList;
    ProcessSyncthing: TProcessUTF8;
    ProcessSupport: TProcessUTF8;
    TimerEventListen: TTimer;
    TimerEventCheckTimeout: TTimer;
    TimerEventProcess: TTimer;
    TimerAfterStartCheck: TTimer;
    TimerInit: TTimer;
    TimerCheckOnline: TTimer;
    TimerReadStdOutput: TTimer;
    TimerStartOnStart: TTimer;
    TimerUpdate: TTimer;
    procedure actConnectExecute(Sender: TObject);
    procedure actInitExecute(Sender: TObject);
    procedure actStartOrConnectExecute(Sender: TObject);
    procedure actStopAndExitExecute(Sender: TObject);
    procedure actRestartExecute(Sender: TObject);
    procedure actRunSupportProcExecute(Sender: TObject);
    procedure actStartAndConnectExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actTerminateExecute(Sender: TObject);
    procedure actUpdateDataExecute(Sender: TObject);

    procedure DataModuleDestroy(Sender: TObject);
    procedure TimerAfterStartCheckTimer(Sender: TObject);

    procedure TimerInitTimer(Sender: TObject);
    procedure TimerStartOnStartTimer(Sender: TObject);
    procedure TimerReadStdOutputTimer(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
  private
    OutputChankStr: UTF8String;
  public
    procedure httpUpdateConnections(Request: THttpRequest);
    procedure httpUpdateDeviceStat(Request: THttpRequest);
  public

    // Flag that core is inited
    Inited: boolean;

    // State (FSM)
    //TODO: WIP!
    State: TCoreState;

    // flag - checked 'syncthing is work'
    OnlineTested: boolean;

    Terminated: boolean;

    // REST API access
    aiohttp: TAsyncHTTP;

    // REST API pooling
    aiohttpLongPolling: TAsyncHTTP;

    APIKey: string;
    SyncthigExecPath: UTF8String;

    // data dir. / config dir.
    SyncthigHome: UTF8String;

    SyncthigParam: UTF8String;
    SyncthigServer: UTF8String;
    SyncthigHost: UTF8String;
    SyncthigPort: integer;

    EventsLastId: int64;

    procedure Init(); virtual;
    procedure Done(); virtual;
    procedure EventOnline(); virtual;
    procedure EventOffline(); virtual;

    function GetSyncthigExecPath(): UTF8String; virtual;
    function GetSyncthigHomePath(): UTF8String; virtual;
    function ReadAPIKeyFromCfg(): string; virtual;
    procedure FillSyncthingExecPath();
    procedure FillSupportExecPath();

    procedure StartAndConnect();
    procedure Stop();

    procedure EventProcess(event: TJSONObject); virtual;

    function MakeOnlineHint(): string;

    // Returns true if current state implies Syncthing is online/working
    function Online(): boolean;
    // Returns true if FSM is in a transitional state
    function InTransition(): boolean;

    procedure ReadStdOutput(Proc: TProcessUTF8; AddProc: TAddConsoleLine; var TextChank: UTF8String);
    procedure AddStringToConsole(Str: UTF8String);
  end;

var
  Core: TCore;

function HttpRequestToJson(Request: THttpRequest; out Json: TJSONData): boolean;

implementation

uses
  LCLTranslator, // i18n
  uFormOptions,
  uFormMain,
  uModuleMain,
  httpsend, {Synacode,}
  synautil,
  Graphics,
  LConvEncoding,
  LazFileUtils,
  jsonparser,
  jsonscanner;

{$R *.lfm}

function TCore.Online(): boolean;
begin
  // Minimal logic: online only when fully working
  Result := (self.State = stWork);
end;

function TCore.InTransition(): boolean;
begin
  case self.State of
    stLaunching,
    stLaunchingWait,
    stConnectOrStart,
    stShutdownAndStart,
    stShutdownAndStartWait,
    stStopping,
    stStoppingWait,
    stDisconnectingGUI:
      Result := true;
  else
    Result := false;
  end;
end;

function HttpRequestToJson(Request: THttpRequest; out Json: TJSONData): boolean;
var
  JN: TJSONParser;
  JData: TJSONData;
  MS: TMemoryStream;
begin
  Result := false;
  Json := nil;
  if (Request = nil) then Exit;
  if Request.Status = 499 then Exit;

  if (Request.Response <> nil) and (Request.Response.Size > 0) then
  begin
    JN := nil;
    MS := TMemoryStream.Create;
    try
      // Copy response into a local stream to decouple from Request.Response
      Request.Response.Position := 0;
      MS.CopyFrom(Request.Response, Request.Response.Size);
      MS.Position := 0;

      JN := TJSONParser.Create(MS, [joUTF8]);
      try
        JData := JN.Parse();
      except
        on EJSONParser do JData := nil;
      end;
      if JData <> nil then
      begin
        Json := JData;
        Result := true;
      end;
    finally
      if JN <> nil then
        JN.Free();
      MS.Free;
    end;
  end;
end;

{ TCore }

procedure TCore.httpUpdateConnections(Request: THttpRequest);
var
  JData, j2: TJSONData;
  ij: TJSONEnum;
  d: TDevInfo;
begin
  (*
  if HttpRequestToJson(Request, JData) then
  try
    j2 := JData.GetPath('connections');
    // enum all device
    for ij in j2 do
    begin
      // find in device list.
      if self.MapDevInfo.GetValue(ij.Key, d) then
      begin
        // update data
        d.Connected:=ij.Value.GetPath('connected').AsBoolean;
        d.Paused:=ij.Value.GetPath('paused').AsBoolean;
        d.Address:=ij.Value.GetPath('address').AsString;
        //todo: use 'mutable' ptr
        // write to map
        self.MapDevInfo[ij.Key] := d;
      end;
    end;
    // OFF: frmMain.treeDevices.RootNodeCount:=j2.Count;
    // OFF: frmMain.treeDevices.Invalidate(); //todo: <-optimize
  finally
    FreeAndNil(JData);
  end;
  *)
end;

procedure TCore.httpUpdateDeviceStat(Request: THttpRequest);
var
  JData: TJSONData;
  ij: TJSONEnum;
  s: ansistring;
  dt: TDateTime;
  d: TDevInfo;
begin
  (*
  if HttpRequestToJson(Request, JData) then
  try
    // enum all device
    for ij in JData do
    begin
      // find in device list.
      if self.MapDevInfo.GetValue(ij.Key, d) then
      begin
        // update data
        if ij.Value.FindPath('lastSeen')<>nil then
        begin
          s := ij.Value.GetPath('lastSeen').AsString;
          if JsonStrToDateTime(s, dt) then
            d.LastSeen:=dt;
        end;
        //todo: use 'mutable' ptr
        // write to map
        self.MapDevInfo[ij.Key] := d;
      end;
    end;
  finally
    FreeAndNil(JData);
  end;
  *)
end;


function GetXml(var Node:TDOMNode; Name: WideString): boolean;
var N:TDOMNode;
begin
  Result := false;
  if Node <> nil then
  begin
    N := Node.FindNode(Name);
    if N <> nil then
    begin
      Node:=N;
      Result:=true;
    end;
  end;
end;

function TCore.ReadAPIKeyFromCfg: string;
var
  //key: string;
  filename: UTF8String;
  FDoc: TXMLDocument;
  NPtr: TDOMNode;
begin
  try
    FDoc := nil;
    filename := GetSyncthigHomePath() + '\config.xml';
    if FileExistsUTF8(filename) then
      ReadXMLFile(FDoc, UTF8ToSys(filename));

    NPtr := FDoc;
    if GetXml(NPtr, 'configuration') then
      if GetXml(NPtr, 'gui') then
        if GetXml(NPtr, 'apikey') then
          if GetXml(NPtr, '#text') then
          begin
            Result := AnsiString(NPtr.NodeValue);
          end;
  finally
    if Assigned(FDoc) then
      FreeAndNil(FDoc);
  end;

  if Result = '' then
    Result := frmOptions.edAPIKey.Text;
end;

procedure TCore.StartAndConnect();
begin
  //TODO: добавить поддержку "подключения" к уже запущенному процессу (без консоли разумеется)
  // если процесс не запущен тогда запускаем его
  if not ProcessSyncthing.Running then
  begin
    // we are launching a new process
    self.State := stLaunching;
    FillSyncthingExecPath();
    ProcessSyncthing.Execute();
    TimerAfterStartCheck.Enabled:=True;
    if actRunSupportProc.Checked then
    begin
      FillSupportExecPath();
      ProcessSupport.Execute();
    end;
  end
  else
  begin
    // TODO: process is already running - ...???...
  end;
end;

procedure TCore.Stop();
begin
  // move state to stopping, final state will be set by httpCheckOnline
  self.State := stStopping;
  //aiohttp.Post('rest/system/shutdown', '', nil);
  if ProcessSupport.Active then
    ProcessSupport.Terminate(0);
  //ProcessSyncthing.Terminate(0);
end;

procedure TCore.EventProcess(event: TJSONObject);
var
  j2: TJSONData;
  s, info: UTF8String;
begin
  // https://docs.syncthing.net/dev/events.html#event-types

  info := '';
  j2 := event.FindPath('data.folder');
  if j2 <> nil then
  begin
    info := 'folder:' + j2.AsString;
  end
  else
  begin
    info := 'json_dump:' + event.AsJSON;
  end;

  s := Format('id:%d;  type:%s  name:%s  %s', [
    event.Get('id', 0),
    event.Get('type', ''),
    event.Get('name', ''),
    info
  ]);

  // OFF: frmMain.listEvents.Items.Insert(0, s);

  // OFF: while frmMain.listEvents.Items.Count > 100 do
  // OFF:   frmMain.listEvents.Items.Delete(frmMain.listEvents.Items.Count-1);

  // OFF: frmMain.listEvents.ItemIndex := frmMain.listEvents.Items.Count-1;
end;

procedure TCore.ReadStdOutput(Proc: TProcessUTF8;
  AddProc: TAddConsoleLine; var TextChank: UTF8String);
var
  line, TmpStr: RawByteString;
  p, BytesRead: LongInt;
  conv_ok: boolean;
const
  LE = LineEnding; //todo: check in linux
begin
  try
    if Assigned(Proc.Output) then
    begin
      //todo: (ProcessSyncthing.Running) ...
      BytesRead:=Proc.Output.NumBytesAvailable;
      if (BytesRead > 0) then
      begin
        SetLength(TmpStr, BytesRead);
        Proc.Output.ReadBuffer(TmpStr[1], BytesRead);
        TmpStr := TextChank + TmpStr;
        line := '';
        p := Pos(LE, TmpStr);
        while p <> 0 do
        begin
          // get line
          line := Copy(TmpStr, 1, p-1);
          // cut line from text
          TmpStr := Copy(TmpStr, p+Length(LE), Length(TmpStr) - p);
          // normalize codepage from console (uses LConvEncoding)
          line := ConvertEncodingToUTF8(line, GetConsoleTextEncoding(), conv_ok);
          // send line
          AddProc(line);
          // find again
          p := Pos(LE, TmpStr);
        end;
        TextChank := TmpStr;
      end;
    end;
  except
    //ShowMessage('FAIL!');
  end;
end;

procedure TCore.TimerReadStdOutputTimer(Sender: TObject);
begin
  ReadStdOutput(ProcessSyncthing, @AddStringToConsole, OutputChankStr);
end;

procedure TCore.TimerUpdateTimer(Sender: TObject);
begin
  self.TimerUpdate.Enabled:=false;
  actUpdateData.Execute();
end;

function TCore.MakeOnlineHint(): string;
(*
var
  i: Core.MapDevInfo.TIterator;
  OnlineCount: integer;
  OnlineList: string;
  DeviceName: string;
  DeviceAddr: string;
const
  MaxItemsInHint = 5;
*)
begin
  (*
  i := self.MapDevInfo.Iterator();
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
              DeviceName := DeviceName + ' (local)';
            OnlineList := OnlineList + DeviceName + #13;
          end;
        end;
      until not i.Next;
    finally
      FreeAndNil(i);
    end;
  if OnlineCount > MaxItemsInHint then
    OnlineList := OnlineList + '...' + #13;

  Result := 'Online ' + IntToStr(OnlineCount) + ' devices:' + #13 + OnlineList;
  *)
end;

procedure TCore.TimerInitTimer(Sender: TObject);
begin
  if TimerInit.Enabled then
  begin
    TimerInit.Enabled:=false;
    actInit.Execute();
  end;
end;

procedure TCore.TimerStartOnStartTimer(Sender: TObject);
begin
  if OnlineTested then
  begin
    TimerStartOnStart.Enabled:=false;
    if not Online() then
      StartAndConnect();
  end;
end;

procedure TCore.actRestartExecute(Sender: TObject);
var i: integer;
begin
  actStop.Execute();

  //todo: плохое решение! - надо делать проверку и отложенный старт
  for i:=1 to 10*5 do begin
    sleep(100); // sleep 5 second
    Application.ProcessMessages();
  end;

  actStartAndConnect.Execute();
end;

procedure TCore.actStopAndExitExecute(Sender: TObject);
begin
  actStop.Execute();
  Application.Terminate();
end;

procedure TCore.actInitExecute(Sender: TObject);
begin
  self.Init();

  //todo: i18n move to module main
  {strs:=TStringList.Create;
  GetFiles(ExtractFilePath(Application.ExeName)+'languages' + PathDelim, strs, '*.po');
  frmMain.cbLanguage.Items.Assign(strs);
  strs.Free;}
  SetDefaultLang('ru');
  //SetDefaultLang(GetOSLanguage()); // - just use DefaultTranslator module
  //frmMain.LanguageChanged();
end;

procedure TCore.actStartOrConnectExecute(Sender: TObject);
begin
  // TODO: WIP...
end;

procedure TCore.actConnectExecute(Sender: TObject);
begin
  TimerCheckOnline.Enabled:=true;
end;

procedure TCore.actRunSupportProcExecute(Sender: TObject);
begin
  // empty - it is just options
end;

procedure TCore.actStartAndConnectExecute(Sender: TObject);
begin
  StartAndConnect();
end;

procedure TCore.actStopExecute(Sender: TObject);
begin
  Stop();
end;

procedure TCore.actTerminateExecute(Sender: TObject);
begin
  ProcessSupport.Terminate(0);
  ProcessSyncthing.Terminate(0);
end;

procedure TCore.actUpdateDataExecute(Sender: TObject);
begin
  if self.Online() then
    ModuleMain.TrayIcon.Hint := Core.MakeOnlineHint()
  else
    ModuleMain.TrayIcon.Hint := '';
end;

procedure TCore.DataModuleDestroy(Sender: TObject);
begin
  Done();
end;

procedure TCore.TimerAfterStartCheckTimer(Sender: TObject);
begin
  TimerAfterStartCheck.Enabled:=False;
  if not ProcessSyncthing.Running then
  begin
    ShowMessage('Run fail. Exit code: '+IntToStr(ProcessSyncthing.ExitCode)+
      #13+
      '(press [Ctrl]+[C] to copy full error text)'+#13+
      'Run:'+#13+
      ProcessSyncthing.Executable + ' ' + ProcessSyncthing.Parameters.Text
    );
  end else
  begin
    TimerCheckOnline.Enabled:=true;
  end;
end;

procedure TCore.Init();
begin
  self.State := stUnknown;

  aiohttp := TAsyncHTTP.Create;
  aiohttp.ConnectTimeout:=1000;
  aiohttp.RetryCount:=1;
  aiohttp.IOTimeout:=1000;
  aiohttp.KeepConnection:=true;

  aiohttpLongPolling := TAsyncHTTP.Create;
  aiohttpLongPolling.ConnectTimeout:=1000;
  aiohttpLongPolling.RetryCount:=0;
  aiohttpLongPolling.IOTimeout:=61000;
  aiohttpLongPolling.KeepConnection:=true;


  SyncthigHost:='127.0.0.1';
  SyncthigPort:=8384;
  SyncthigServer:=Format('http://%s:%d/', [SyncthigHost, SyncthigPort]);

  SyncthigExecPath:=GetSyncthigExecPath();
  SyncthigHome:=GetSyncthigHomePath();
  APIKey:=ReadAPIKeyFromCfg();

  // TODO: migrate
  //TimerStartOnStart.Enabled:=frmOptions.chRunSyncOnStart.Checked;
  //TimerCheckOnline.Enabled:=frmOptions.chRunSyncOnStart.Checked;

  Inited := true;
end;

procedure TCore.Done();
var
  i: TMapFolderInfo.TIterator;
  i2: TMapDevInfo.TIterator;
begin
  Inited := false;

  Terminated := true;
  // Stop timers related to long-poll and general activity
  TimerCheckOnline.Enabled:=false;
  TimerEventListen.Enabled:=false;
  TimerEventProcess.Enabled:=false;
  FreeAndNil(aiohttp);
  FreeAndNil(aiohttpLongPolling);
end;

procedure TCore.EventOnline();
begin
  // OFF: frmMain.shStatusCircle.Brush.Color:=clGreen;
  actUpdateData.Execute();
  // start listening events when we became online
  self.TimerEventListen.Enabled:=true;
end;

procedure TCore.EventOffline();
begin
  // OFF: frmMain.shStatusCircle.Brush.Color:=clPurple;
  // stop listening events when offline
  self.TimerEventListen.Enabled:=false;
end;

procedure TCore.AddStringToConsole(Str: UTF8String);
begin
  if frmMain.edConsole.Lines.Count > 100 then
    frmMain.edConsole.Lines.Delete(0);
  frmMain.edConsole.Lines.Add(Str);
end;

function TCore.GetSyncthigExecPath: UTF8String;
begin
  //OLD: result := 'D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing.exe';
  result := frmOptions.edPathToExecWithFilename.Text;
end;

function TCore.GetSyncthigHomePath: UTF8String;
begin
  //OLD: result := 'h:\Dat\syncthing\';
  result := frmOptions.edPathToConfigDir.Text;
end;

procedure TCore.FillSyncthingExecPath();
begin
  {$IFDEF WINDOWS}

  if frmOptions.chUseProxyOutputForFixBug.Checked then
  begin
    {Note:
       In windows has a bug - std output pipe is empty (for an unknown reason).
       In other program all working good (There is an error only in Syncthing).
       For fix this bug need redirect pipe stream over other program.
       (for redirect i am using 'find' program)
       (my enviroment: Win7 x64, Syncthing v0.14.27 for Windows (32 bit).
    }
    ProcessSyncthing.Executable :=
      GetEnvironmentVariable('WINDIR')+'\system32\cmd.exe';
    ProcessSyncthing.Parameters.Text :=
      '/C "' + SyncthigExecPath + ' ' +
      //'--no-console '+
      '--no-browser '+
      '--home=' + SyncthigHome + ' ' +
      ' | find /v " empty_find_just_for_redirect_stdio_00686558 " "';
  end else
  begin
    ProcessSyncthing.Executable := SyncthigExecPath;
    ProcessSyncthing.Parameters.Text :=
      //'--no-console '+
      '--no-browser '+
      '--home=' + SyncthigHome;
  end;

  {$ELSE}
  //todo: WIP: FillSyncthingExecPath linux
  ProcessSyncthing.Executable := FindSyncthigPath();
  ProcessSyncthing.Parameters.Clear;
  ProcessSyncthing.Parameters.Add('--home=' + SyncthigHome);
  ProcessSyncthing.Parameters.Add('--no-browser');
  {$ENDIF}
end;

procedure TCore.FillSupportExecPath();
begin
  //todo: WIP: FillSupportExecPath
  ProcessSupport.Executable := 'D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing-inotify.exe';
  ProcessSupport.Parameters.Text := '--home=' + SyncthigHome;
end;

end.

