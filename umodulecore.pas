unit uModuleCore;

{$mode objfpc}{$H+}

(*
procedure TfrmMain.httpTest(Query: THttpQuery);
begin
  //example: aiohttp.Get('http://ya.ru', @httpTest);
  if Query.CallState = httpLoad then
    ShowMessage(IntToStr(Query.Status));
end;


http://wiki.freepascal.org/Executing_External_Programs
http://wiki.freepascal.org/Executing_External_Programs/ru

%WINDIR%\system32
%comspec% (C:/windows/system32/cmd.exe)

WORK: /C "D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing.exe | find /v " FAKE_FILTER_STRING_FOR_FIND_COMMAND_AS_MIRROR_REDIRECT " "

C:\Windows\System32\cmd.exe
/C "D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing.exe"
/C "D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing_hex_disk.bat"

-------------

function GetConsoleWindow: HWND; stdcall; external kernel32;

procedure ShowSelfConsoleWindow;
begin
  //show the console window
  ShowWindow(GetConsoleWindow(), SW_SHOW);
end;

procedure HideSelfConsoleWindow;
begin
  //hide the console window
  ShowWindow(GetConsoleWindow(), SW_HIDE);
end;

-------------


System Endpoints
    GET /rest/system/config
    GET /rest/system/config/insync
    GET /rest/system/connections
    GET /rest/system/debug
    GET /rest/system/discovery
    GET /rest/system/error
    GET /rest/system/log
    GET /rest/system/ping
    GET /rest/system/status
    GET /rest/system/upgrade
    GET /rest/system/version

    POST /rest/system/ping
    POST /rest/system/reset
    POST /rest/system/restart
    POST /rest/system/shutdown
    POST /rest/system/upgrade

Database Endpoints
    GET /rest/db/browse
    GET /rest/db/completion
    GET /rest/db/file
    GET /rest/db/ignores
    GET /rest/db/need
    GET /rest/db/status

Event Endpoints
    GET /rest/events
    GET /rest/events/disk

Statistics Endpoints
    GET /rest/stats/device
      {
        "HJBNI74-LB5I7ND-IDXKOJH-CMM5KM3-AR4BMVB-XOXIJAB-FSYCOFN-3EBH7AS" : {
          "lastSeen" : "1970-01-01T03:00:00+03:00"
        },
        ...
    GET /rest/stats/folder

Misc Services Endpoints
    GET /rest/svc/deviceid
    GET /rest/svc/lang
    GET /rest/svc/random/string
    GET /rest/svc/report







    -----------------------------

    detect “Up to Date”:
    https://forum.syncthing.net/t/syncthing-is-the-best-help-me-detect-up-to-date-with-rest/10968/5

    /rest/db/completion?folder=src&device=[specific_device_id]

    If I do that, I get:

    {
      "completion": 99.14897162188355,
      "globalBytes": 78764706,
      "needBytes": 670310,
      "needDeletes": 0
    }

    This means I have to iterate over every directory and every device to see problems!

    In particular,

    /rest/db/status?folder=src

    shows:

    {
      "globalBytes": 78764706,
      "globalDeleted": 3188,
      "globalDirectories": 977,
      "globalFiles": 4999,
      "globalSymlinks": 12,
      "ignorePatterns": false,
      "inSyncBytes": 78764706,
      "inSyncFiles": 4999,
      "invalid": "",
      "localBytes": 78764706,
      "localDeleted": 3147,
      "localDirectories": 977,
      "localFiles": 4999,
      "localSymlinks": 12,
      "needBytes": 0,
      "needDeletes": 0,
      "needDirectories": 0,
      "needFiles": 0,
      "needSymlinks": 0,
      "sequence": 44122,
      "state": "idle",
      "stateChanged": "2017-11-17T18:55:14.3305827-08:00",
      "version": 44122
    }

*)

interface

uses
  //Dialogs, //todo: TEMP
  AsyncHttp,
  fpjson,
  XMLRead, DOM,
  LazFileUtils, LazUTF8,
  Classes, SysUtils, UTF8Process, ExtCtrls, ActnList, Forms;

type

  TAddConsoleLine = procedure (Line: UTF8String) of object;

  { TCore }

  //todo: extract real core code to 'model'(or 'control') and 'utils'
  TCore = class(TDataModule)
    actExit: TAction;
    actInit: TAction;
    actPause: TAction;
    actRunSupportProc: TAction;
    actTerminate: TAction;
    actRestart: TAction;
    actStop: TAction;
    actStart: TAction;
    ActionList: TActionList;
    ProcessSyncthing: TProcessUTF8;
    ProcessSupport: TProcessUTF8;
    TimerPause: TTimer;
    TimerInit: TTimer;
    TimerPing: TTimer;
    TimerReadStdOutput: TTimer;
    procedure actInitExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actRestartExecute(Sender: TObject);
    procedure actRunSupportProcExecute(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actTerminateExecute(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TimerInitTimer(Sender: TObject);
    procedure TimerPauseTimer(Sender: TObject);
    procedure TimerPingTimer(Sender: TObject);
    procedure TimerReadStdOutputTimer(Sender: TObject);
  private
    OutputChankStr: UTF8String;
  public
    Online: boolean;
    Terminated: boolean;
    aiohttp: TAsyncHTTP;

    APIKey: string;
    SyncthigExecPath: UTF8String;

    // data dir. / config dir.
    SyncthigHome: UTF8String;

    SyncthigParam: UTF8String;
    SyncthigServer: UTF8String;

    httpPingInProc: boolean;

    procedure Init;
    procedure Done;
    function GetSyncthigExecPath: UTF8String; virtual;
    function GetSyncthigHome: UTF8String; virtual;
    function GetAPIKey: string; virtual;
    procedure FillSyncthingExecPath;
    procedure FillSupportExecPath;

    procedure Start;
    procedure Stop;

    procedure ReadStdOutput(Proc: TProcessUTF8; AddProc: TAddConsoleLine; var TextChank: UTF8String);
    procedure AddStringToConsole(Str: UTF8String);

    function SendJSON(const RESTPath: string; const DataForSend: TStrings = nil): Boolean;

    // call from other thread!
    procedure aiohttpAddHeader(Query: THttpQueryBase);

    procedure httpPing(Query: THttpQuery);
  end;

var
  Core: TCore;

function HttpQueryToJson(Query: THttpQuery; out Json: TJSONData): boolean;

function JsonStrToDateTime(Str: AnsiString; out dt: TDateTime): boolean;

implementation

uses
  LCLTranslator, // i18n
  uFormOptions,
  uFormMain,
  httpsend, {Synacode,}
  synautil,
  Graphics,
  LConvEncoding,
  RegExpr, // ParseJsonDateTime
  dateutils, // ParseJsonDateTime
  jsonparser,
  jsonscanner;

{$R *.lfm}

function JsonStrToDateTime(Str: AnsiString; out dt: TDateTime): boolean;
var re: TRegExpr;
begin
  //todo: move to global singlton
  re := TRegExpr.Create('(\d+)-(\d+)-(\d+)T(\d+):(\d+):(\d+).*');
  Result := false;
  try
    if re.Exec(Str) then
    begin
      Result := TryEncodeDateTime(
        StrToInt(re.Match[1]),
        StrToInt(re.Match[2]),
        StrToInt(re.Match[3]),
        StrToInt(re.Match[4]),
        StrToInt(re.Match[5]),
        StrToInt(re.Match[6]),
        0,
        dt
        );
    end;
  finally
    re.Free();
  end;
end;

function HttpQueryToJson(Query: THttpQuery; out Json: TJSONData): boolean;
var
  JN: TJSONParser;
  JData: TJSONData;
begin
  Result := false;
  Json := nil;
  if (Query.ReadyState=httpDone) and (Query.Status=200) then
  begin
    try
      if Query.Response.Size>0 then
      begin
        JData:=nil;
        JN := nil;
        JN := TJSONParser.Create(Query.Response, [joUTF8]);
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
      end;
    finally
      if JN<>nil then
        JN.Free();
    end;
  end;
end;

{ TCore }

function TCore.SendJSON(const RESTPath: string; const DataForSend: TStrings
  ): Boolean;
var
  HTTP: THTTPSend;
  URL: string;
  s: UTF8String;
begin
  //todo: REMOVE OLD METHOD - migrate to asyncHttp
  Result := false;
  URL := SyncthigServer + RESTPath;
  HTTP := THTTPSend.Create;
  HTTP.Headers.Add('X-API-Key: '+APIKey);
  try
    if Assigned(DataForSend) then
      for s in DataForSend do
        WriteStrToStream(HTTP.Document, s);
    Result := HTTP.HTTPMethod('POST', URL);
    //if Result then
    //  Response.LoadFromStream(HTTP.Document);
  finally
    HTTP.Free;
  end;
end;

procedure TCore.aiohttpAddHeader(Query: THttpQueryBase);
begin
  Query.SetRequestHeader('X-API-Key', self.APIKey);
end;

procedure TCore.httpPing(Query: THttpQuery);
begin
  if not Terminated then
    if Query.ReadyState=httpDone then
    begin
      //todo: check ping result
      if Query.Status <> 200 then
      begin
        Online := false;
        frmMain.shStatusCircle.Brush.Color:=clPurple;
      end else
      begin
        Online := true;
        frmMain.shStatusCircle.Brush.Color:=clGreen
      end;
    end else
    begin
      Online := false;
      frmMain.shStatusCircle.Brush.Color:=clRed;
    end;
end;

//todo: TEMP!!!!!!!!!
function DoFill(Node:TDOMNode): widestring;
var
  i: integer;
begin
  Result := '';
  //if not Assigned(Node) then exit;
  for i:=0 to Node.ChildNodes.Count - 1 do
  begin
    Result := Result + Node.ChildNodes[i].NodeName + ', ';
    Result := Result + ' (' + DoFill(Node.ChildNodes[i]) + '), ';
  end;
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

function TCore.GetAPIKey: string;
var
  //key: string;
  filename: UTF8String;
  FDoc: TXMLDocument;
  NPtr: TDOMNode;
begin
  try
    FDoc := nil;
    filename := GetSyncthigHome() + '\config.xml';
    if FileExistsUTF8(filename) then
      ReadXMLFile(FDoc, UTF8ToSys(filename));

    NPtr := FDoc;
    if GetXml(NPtr, 'configuration') then
      if GetXml(NPtr, 'gui') then
        if GetXml(NPtr, 'apikey') then
          if GetXml(NPtr, '#text') then
          begin
            Result := NPtr.NodeValue;
          end;
  finally
    if Assigned(FDoc) then
      FreeAndNil(FDoc);
  end;

  //todo: WIP: GetAPIKey
  if Result = '' then
    Result := frmOptions.edAPIKey.Text;
end;

procedure TCore.Start;
begin
  if not ProcessSyncthing.Running then
  begin
    FillSyncthingExecPath();
    ProcessSyncthing.Execute();
    if actRunSupportProc.Checked then
    begin
      FillSupportExecPath();
      ProcessSupport.Execute();
    end;
  end;
end;

procedure TCore.Stop;
begin
  //todo: WIP!!!! - чет не заработал POST метод...
  //todo: OLD:
  SendJSON('rest/system/shutdown');
  //aiohttp.Post('rest/system/shutdown', '', nil);
  ProcessSupport.Terminate(0);
  //ProcessSyncthing.Terminate(0);
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

procedure TCore.TimerInitTimer(Sender: TObject);
begin
  if TimerInit.Enabled then
  begin
    TimerInit.Enabled:=false;
    actInit.Execute();
  end;
end;

procedure TCore.TimerPauseTimer(Sender: TObject);
begin
  TimerPause.Enabled:=false;
  actStart.Execute();
end;

procedure TCore.actRestartExecute(Sender: TObject);
begin
  actStop.Execute();
  actStart.Execute();
end;

procedure TCore.actExitExecute(Sender: TObject);
begin
  actStop.Execute();
  Application.Terminate;
end;

procedure TCore.actPauseExecute(Sender: TObject);
begin
  actStop.Execute();
  TimerPause.Enabled:=true;
end;

procedure TCore.actInitExecute(Sender: TObject);
begin
  Core.Init();

  //todo: i18n move to module main
  {strs:=TStringList.Create;
  GetFiles(ExtractFilePath(Application.ExeName)+'languages' + PathDelim, strs, '*.po');
  frmMain.cbLanguage.Items.Assign(strs);
  strs.Free;}
  SetDefaultLang('ru');
  //SetDefaultLang(GetOSLanguage()); // - just use DefaultTranslator module
  //frmMain.LanguageChanged();
end;

procedure TCore.actRunSupportProcExecute(Sender: TObject);
begin
  // empty - it is just options
end;

procedure TCore.actStartExecute(Sender: TObject);
begin
  Start();
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

procedure TCore.DataModuleDestroy(Sender: TObject);
begin
  Done();
end;

procedure TCore.TimerPingTimer(Sender: TObject);
begin
  if not httpPingInProc and not Terminated then
    aiohttp.Get(SyncthigServer+'rest/system/ping', @httpPing, '', @httpPingInProc);
end;

procedure TCore.Init;
begin
  //todo: WIP: INIT
  Online := false;
  aiohttp := TAsyncHTTP.Create(false);
  aiohttp.OnOpened:=@aiohttpAddHeader;
  SyncthigServer:='http://127.0.0.1:8384/';
  SyncthigExecPath:=GetSyncthigExecPath();
  SyncthigHome:=GetSyncthigHome();
  APIKey:=GetAPIKey();

  TimerPing.Enabled:=true;
end;

procedure TCore.Done;
begin
  Terminated := true;
  TimerPing.Enabled:=false;
  FreeAndNil(aiohttp);
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
  result := frmOptions.edPathToExec.Text;
end;

function TCore.GetSyncthigHome: UTF8String;
begin
  //OLD: result := 'h:\Dat\syncthing\';
  result := frmOptions.edPathToConfig.Text;
end;

procedure TCore.FillSyncthingExecPath;
begin
  {$IFDEF WINDOWS}
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
    //'-no-console '+
    '-no-browser '+
    '-home=' + SyncthigHome + ' ' +
    ' | find /v " FAKE_FILTER_STRING_FOR_FIND_REDIRECT_COMMAND " "';
  {$ELSE}
  //todo: WIP: FillSyncthingExecPath linux
  ProcessSyncthing.Executable := FindSyncthigPath();
  ProcessSyncthing.Parameters.Clear;
  ProcessSyncthing.Parameters.Add('-home=' + SyncthigHome);
  ProcessSyncthing.Parameters.Add('-no-browser');
  {$ENDIF}
end;

procedure TCore.FillSupportExecPath;
begin
  //todo: WIP: FillSupportExecPath
  ProcessSupport.Executable := 'D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing-inotify.exe';
  ProcessSupport.Parameters.Text := '-home=' + SyncthigHome;
end;

end.

