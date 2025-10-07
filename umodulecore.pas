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

  TAddConsoleLine = procedure (Line: UTF8String) of object;

  { TCore }

  //todo: extract real core code to 'model'(or 'control') and 'utils'
  TCore = class(TDataModule)
    ActionList: TActionList;
    TimerAfterStartCheckRunError: TTimer;
    TimerInit: TTimer;

    procedure TimerAfterStartCheckRunErrorTimer(Sender: TObject);
    procedure TimerInitTimer(Sender: TObject);
  private
    OutputChankStr: UTF8String;
  public
    // Flag that core is inited
    Inited: boolean;

    APIKey: string;
    SyncthigExecPath: UTF8String;

    // data dir. / config dir.
    SyncthigHome: UTF8String;

    SyncthigParam: UTF8String;
    SyncthigHost: UTF8String;
    SyncthigPort: integer;

    function GetSyncthigExecPath(): UTF8String; virtual;
    function GetSyncthigHomePath(): UTF8String; virtual;
    function ReadAPIKeyFromCfg(): string; virtual;
    procedure FillSyncthingExecPath();
    procedure FillSupportExecPath();


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



procedure TCore.TimerInitTimer(Sender: TObject);
begin
  if TimerInit.Enabled then
  begin
    TimerInit.Enabled:=false;

    // TODO: temp, удалить выставление ru
    SetDefaultLang('ru');

    SyncthigHost:='127.0.0.1';
    SyncthigPort:=8384;

    // TODO: WIP...
    SyncthigExecPath:=GetSyncthigExecPath();
    SyncthigHome:=GetSyncthigHomePath();
    APIKey:=ReadAPIKeyFromCfg();

    if frmOptions.chRunSyncOnStart.Checked then
    begin
      ModuleMain.actConnect.Execute;
    end;
  end;
end;

procedure TCore.TimerAfterStartCheckRunErrorTimer(Sender: TObject);
begin
  TimerAfterStartCheckRunError.Enabled:=False;
  if not ProcessSyncthing.Running then
  begin
    ShowMessage('Run fail. Exit code: '+IntToStr(ProcessSyncthing.ExitCode)+
      #13+
      '(press [Ctrl]+[C] to copy full error text)'+#13+
      'Run:'+#13+
      ProcessSyncthing.Executable + ' ' + ProcessSyncthing.Parameters.Text
    );
  end;
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

