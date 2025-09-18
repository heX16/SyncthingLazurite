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
    Button1: TButton;
    Image1: TImage;
    lbAuthor: TLabel;
    lbProgramVer: TLabel;
    lbSyncthingVersion: TLabeledEdit;
    lbSyncthingWebSite: TLabeledEdit;
    lbTranslateAuthor1: TLabel;
    lbProgramName: TLabel;
    lbRepURL: TLabeledEdit;
    lbAboutText: TMemo;
    lbTranslateAuthor2: TLabel;
    Shape1: TShape;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    { public declarations }
  end;

// var frmAbout: TfrmAbout; - disabled, used a dynamic creation

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  uModuleMain,
  SysUtils,
  uUtils,
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
  lbProgramName.Caption:='SyncthingLazurite';
  lbProgramVer.Caption:='v' + uUtils.StripAfterChar(GetMyVersion(4), '.', true);
  lbAuthor.Caption:='Author: [ heXor ]';
end;

procedure TfrmAbout.Button1Click(Sender: TObject);
begin
  self.Close();
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  // Read version from in-memory JSON tree (already present by default)
  lbSyncthingVersion.Text := ModuleMain.GetSystemVersionString();
  if lbSyncthingVersion.Text = '' then
    lbSyncthingVersion.Text := 'Unknown';
end;

end.

