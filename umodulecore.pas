unit uModuleCore;

{$mode objfpc}{$H+}

(*
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
  Classes, SysUtils, FileUtil, UTF8Process, ExtCtrls;

type

  TAddConsoleLine = procedure (Line: UTF8String) of object;

  { TCore }

  TCore = class(TDataModule)
    ProcessSyncthing: TProcessUTF8;
    ProcessNotify: TProcessUTF8;
    timerPing: TTimer;
    TimerReadStdOutput: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure timerPingTimer(Sender: TObject);
    procedure TimerReadStdOutputTimer(Sender: TObject);
  private
    OutputChankStr: UTF8String;
  public
    APIKey: string;
    SyncthigPath: UTF8String;
    SyncthigHome: UTF8String;
    SyncthigParam: UTF8String;
    SyncthigServer: UTF8String;


    procedure Init;
    function FindSyncthigPath: UTF8String;
    procedure FillSyncthingExecPath;
    procedure FillNotifyExecPath;
    function ReadAPIKey: string;

    procedure Start;
    procedure Stop;

    procedure ReadStdOutput(Proc: TProcessUTF8; AddProc: TAddConsoleLine; var TextChank: UTF8String);
    procedure AddStringToConsole(Str: UTF8String);
    function GetHTTPText(const RESTPath: string; Response: TStrings): Boolean;
    function SendJSON(const RESTPath: string; const DataForSend: TStrings = nil): Boolean;
  end;

var
  Core: TCore;

implementation

uses
  uFormMain,
  httpsend, {Synacode,}
  synautil,
  Graphics,
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

function TCore.ReadAPIKey: string;
begin
  Result := 'MJzE2GL2vOvaFydjM05ocRM64SZ0DR1-';
  //todo: WIP: ReadAPIKey
end;

procedure TCore.Start;
begin
  if not ProcessSyncthing.Running then
  begin
    FillSyncthingExecPath();
    ProcessSyncthing.Execute();
    FillNotifyExecPath();
    ProcessNotify.Execute();
  end;
end;

procedure TCore.Stop;
begin
  SendJSON('rest/system/shutdown');
  ProcessNotify.Terminate(0);
  //ProcessSyncthing.Terminate(0);
end;

procedure TCore.ReadStdOutput(Proc: TProcessUTF8; AddProc: TAddConsoleLine;
  var TextChank: UTF8String);
var
  line, TmpStr: RawByteString;
  p, BytesRead: LongInt;
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
        p := Pos(LE, TmpStr);
        line := TextChank;
        while p <> 0 do
        begin
          line := line + Copy(TmpStr, 1, p-1);
          TmpStr := Copy(TmpStr, p+Length(LE), Length(TmpStr) - p);
          //line:=UTF8Encode(line);
          AddProc(line);
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

procedure TCore.DataModuleCreate(Sender: TObject);
begin
  Core.Init();
end;

procedure TCore.timerPingTimer(Sender: TObject);
var
  strs: TStrings;
  ok: boolean;
begin
  ok := false;
  try
    strs := TStringList.Create;
    ok := GetHTTPText('rest/system/ping', strs);
    //todo: check ping result
  finally
    strs.Free;
  end;
  if ok then
  begin
    //todo: check ping result
    frmMain.shStatusCircle.Brush.Color:=clGreen;
  end else
    frmMain.shStatusCircle.Brush.Color:=clRed;
end;

procedure TCore.Init;
begin
  //todo: WIP: INIT
  SyncthigServer:='http://127.0.0.1:8384/';
  SyncthigPath:=FindSyncthigPath();
  APIKey:=ReadAPIKey();
  SyncthigHome:='h:\Dat\syncthing\';
end;

procedure TCore.AddStringToConsole(Str: UTF8String);
begin
  if frmMain.edConsole.Lines.Count > 100 then
    frmMain.edConsole.Lines.Delete(0);
  frmMain.edConsole.Lines.Add(Str);
end;

function TCore.FindSyncthigPath: UTF8String;
begin
  //todo: WIP: FindSyncthigPath
  result := 'D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing.exe';
end;

procedure TCore.FillSyncthingExecPath;
begin
  {$IFDEF Win32}
  {Note:
     in windows has a bug - std output pipe is empty (for an unknown reason).
     in other program all working good (There is an error only in Syncthing).
     for fix this bug need redirect pipe stream over other program.
     (for redirect i am using 'find' program)
     (my enviroment: Win7 x64, Syncthing v0.14.27 for Windows (32 bit).
  }
  ProcessSyncthing.Executable :=
    GetEnvironmentVariable('WINDIR')+'\system32\cmd.exe';
  ProcessSyncthing.Parameters.Text :=
    '/C "' + SyncthigPath + ' ' +
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

procedure TCore.FillNotifyExecPath;
begin
  //todo: WIP: FillNotifyExecPath
  ProcessNotify.Executable := 'D:\NetDrive\AppsPortableHex\Programs\_Net\syncthing\syncthing-inotify.exe';
  ProcessNotify.Parameters.Text := '-home=' + SyncthigHome;
end;

end.

