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
  { TCore }

  //todo: extract real core code to 'model'(or 'control') and 'utils'
  TCore = class(TDataModule)
    ActionList: TActionList;
    TimerAfterStartCheckRunError: TTimer;
    TimerInit: TTimer;

    procedure TimerAfterStartCheckRunErrorTimer(Sender: TObject);
    procedure TimerInitTimer(Sender: TObject);
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

    procedure AddStringToConsole(Str: UTF8String);
  end;

var
  Core: TCore;

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
  LazFileUtils;

{$R *.lfm}


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

    // GPT: устаревший код.
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
  // GPT: устаревший код. нужен в качестве примера как генерировать сообщение о ошибке при запуске.
  (*
  if not ProcessSyncthing.Running then
  begin
    ShowMessage('Run fail. Exit code: '+IntToStr(ProcessSyncthing.ExitCode)+
      #13+
      '(press [Ctrl]+[C] to copy full error text)'+#13+
      'Run:'+#13+
      ProcessSyncthing.Executable + ' ' + ProcessSyncthing.Parameters.Text
    );
  end;
  *)
end;

procedure TCore.AddStringToConsole(Str: UTF8String);
begin
  if frmMain.edConsole.Lines.Count > 100 then
    frmMain.edConsole.Lines.Delete(0);
  frmMain.edConsole.Lines.Add(Str);
end;

function TCore.GetSyncthigExecPath: UTF8String;
begin
  result := frmOptions.edPathToExecWithFilename.Text;
end;

function TCore.GetSyncthigHomePath: UTF8String;
begin
  result := frmOptions.edPathToConfigDir.Text;
end;

procedure TCore.FillSyncthingExecPath();
begin
  // GPT: устаревший код. нужен только как пример. будет удален в будущем.
  (*
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
  *)
end;

end.

