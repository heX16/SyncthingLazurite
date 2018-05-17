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
    GET /rest/stats/folder

Misc Services Endpoints
    GET /rest/svc/deviceid
    GET /rest/svc/lang
    GET /rest/svc/random/string
    GET /rest/svc/report

*)

interface

uses
  AsyncHttp,
  Classes, SysUtils, FileUtil, UTF8Process, ExtCtrls, ActnList, Forms,
  UniqueInstance;

type

  TAddConsoleLine = procedure (Line: UTF8String) of object;

  { TCore }

  //todo: extract real core code to 'model'(or 'control') and 'utils'
  TCore = class(TDataModule)
    actExit: TAction;
    actInit: TAction;
    actRunSupportProc: TAction;
    actTerminate: TAction;
    actRestart: TAction;
    actStop: TAction;
    actStart: TAction;
    ActionList: TActionList;
    ProcessSyncthing: TProcessUTF8;
    ProcessSupport: TProcessUTF8;
    TimerInit: TTimer;
    TimerPing: TTimer;
    TimerReadStdOutput: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure actInitExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actRestartExecute(Sender: TObject);
    procedure actRunSupportProcExecute(Sender: TObject);
    procedure actStartExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actTerminateExecute(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TimerInitTimer(Sender: TObject);
    procedure TimerPingTimer(Sender: TObject);
    procedure TimerReadStdOutputTimer(Sender: TObject);
  private
    OutputChankStr: UTF8String;
  public
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

    function GetHTTPText(const RESTPath: string; Response: TStrings): Boolean;
    function SendJSON(const RESTPath: string; const DataForSend: TStrings = nil): Boolean;

    procedure httpPing(Query: THttpQuery);
  end;

var
  Core: TCore;

implementation

uses
  uFormOptions,
  uFormMain,
  httpsend, {Synacode,}
  synautil,
  Graphics,
  LConvEncoding,
  fpjson, jsonparser, jsonscanner;

{$R *.lfm}

{ TCore }

function TCore.GetHTTPText(const RESTPath: string; Response: TStrings): Boolean;
var
  HTTP: THTTPSend;
  URL: string;
begin
  URL := SyncthigServer + RESTPath;
  HTTP := THTTPSend.Create;
  HTTP.Headers.Add('X-API-Key: '+APIKey);
  try
    Result := HTTP.HTTPMethod('GET', URL);
    if Result then
      Response.LoadFromStream(HTTP.Document);
  finally
    HTTP.Free;
  end;
end;

function TCore.SendJSON(const RESTPath: string; const DataForSend: TStrings
  ): Boolean;
var
  HTTP: THTTPSend;
  URL: string;
  s: UTF8String;
begin
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

procedure TCore.httpPing(Query: THttpQuery);
begin
  if not Terminated then
    if Query.CallState=httpLoadStart then
      Query.SetRequestHeader('X-API-Key', Core.APIKey);
    if Query.CallState=httpLoad then
    begin
      //todo: check ping result
      if Query.Status <> 200 then
        frmMain.shStatusCircle.Brush.Color:=clPurple else
        frmMain.shStatusCircle.Brush.Color:=clGreen
    end else
      frmMain.shStatusCircle.Brush.Color:=clRed;
end;

function TCore.GetAPIKey: string;
begin
  //todo: WIP: GetAPIKey
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
  SendJSON('rest/system/shutdown');
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

procedure TCore.actInitExecute(Sender: TObject);
begin
  Core.Init();
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
  if not httpPingInProc then
    aiohttp.Get(SyncthigServer+'rest/system/ping', @httpPing, @httpPingInProc);
end;

procedure TCore.Init;
begin
  //todo: WIP: INIT
  aiohttp := TAsyncHTTP.Create(false);
  SyncthigServer:='http://127.0.0.1:8384/';
  SyncthigExecPath:=GetSyncthigExecPath();
  SyncthigHome:=GetSyncthigHome();
  APIKey:=GetAPIKey();

  //todo: TimerPing.Enabled:=true;
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

