unit FormAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  AsyncHTTP,
  ExtCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    lbAuthor: TLabel;
    lbSyncthingVersion: TLabeledEdit;
    lbTranslateAuthor: TLabel;
    lbNameAndVer: TLabel;
    lbRepURL: TLabeledEdit;
    lbAboutText: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure httpGetVer(Request: THttpRequest);
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  uModuleCore,
  SysUtils,
  fpjson;

{$R *.lfm}

{ TfrmAbout }
function GetMyVersion(VerCount: Byte = 4):string;
type
  TVerInfo=packed record
    OtherBytes: array[0..47] of byte; // ненужные нам 48 байт
    Minor,Major,Build,Release: word; // а тут версия
  end;
var
  s:TResourceStream;
  v:TVerInfo;
begin
  result := '';
  try
    s:=TResourceStream.Create(HInstance,'#1',RT_VERSION); // достаём ресурс
    if s.Size>0 then begin
      s.Read(v,SizeOf(v)); // читаем нужные нам байты
      // версия:
      if VerCount >= 1 then
        Result := Result + IntToStr(v.Major);
      if VerCount >= 2 then
        Result := Result + '.' + IntToStr(v.Minor);
      if VerCount >= 3 then
        Result := Result + '.' + IntToStr(v.Release);
      if VerCount >= 4 then
        Result := Result + '.' + IntToStr(v.Build);
    end;
    s.Free;
  except;
  end;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lbNameAndVer.Caption:='Simple Syncthing';
  lbNameAndVer.Caption:=lbNameAndVer.Caption + '   ver.'+GetMyVersion(4);
  lbAuthor.Caption:='Author: heXor';
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  Core.API_Get('system/version', @httpGetVer);
end;

procedure TfrmAbout.httpGetVer(Request: THttpRequest);
var 
  j: TJSONData;
  versionStr: string;
begin
  if Core.Online() then
  begin
    if HttpRequestToJson(Request, j) then
  try
    // Extract version information from Syncthing response
    // JSON structure: {"arch": "amd64", "longVersion": "...", "os": "darwin", "version": "v0.10.27+3-gea8c3de"}
    if j.FindPath('version') <> nil then
      versionStr := j.GetPath('version').AsString
    else
      versionStr := 'Unknown';
    
    // Display version in the form
    lbSyncthingVersion.Text := versionStr;
    finally
      FreeAndNil(j);
    end;
  end else
  begin
    // Syncthing is offline - show offline status
    lbSyncthingVersion.Text := 'Unknown (Offline)';
  end;
end;

end.

