unit FormAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type
  TArrayOfWord = array[0..3] of Word;

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Button1: TButton;
    Image1: TImage;
    lbAuthor: TLabel;
    lbProgramVer: TLabel;
    lbProgramVerBuild: TLabel;
    lbProgramVerDate: TLabel;
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
  fpjson;

{$R *.lfm}

{ TfrmAbout }
function GetMyVersion(): TArrayOfWord;
type
  TVerInfo=packed record
    OtherBytes: array[0..47] of byte; // ненужные нам 48 байт
    Minor,Major,Build,Release: word; // а тут версия
  end;
var
  s:TResourceStream;
  v:TVerInfo;
begin
  Result[0] := 0;
  Result[1] := 0;
  Result[2] := 0;
  Result[3] := 0;
  
  try
    s:=TResourceStream.Create(HInstance,'#1',RT_VERSION); // достаём ресурс
    if s.Size>0 then begin
      s.Read(v,SizeOf(v)); // читаем нужные нам байты
      // версия:
      Result[0] := v.Major;
      Result[1] := v.Minor;
      Result[2] := v.Release;
      Result[3] := v.Build;
    end;
    s.Free;
  except;
  end;
end;

function GetProgramVersion(): string;
var
  versionNumbers: TArrayOfWord;
begin
  versionNumbers := GetMyVersion();
  Result := IntToStr(versionNumbers[0]);
  Result := Result + '.' + IntToStr(versionNumbers[1]);
end;

function GetBuildNumber(): string;
begin
  Result := IntToStr(GetMyVersion()[3]); // returning build number
end;

function GetDateFromVersion(): string;
const
  MonthNames: array[1..12] of string = (
    'january', 'february', 'march', 'april', 'may', 'june',
    'july', 'august', 'september', 'october', 'november', 'december'
  );
var
  dateNum: Word;
  monthNum: Byte;
  year: Word;
begin
  dateNum := GetMyVersion()[2]; // Release field contains year and month
  
  // Extract month and year from number like 2510
  year := (dateNum div 100) + 2000; // Convert 2-digit year to 4-digit year (25 -> 2025)
  monthNum := dateNum mod 100;
  
  // Check month range
  if (monthNum >= 1) and (monthNum <= 12) then
    // format: 2025-10-XX (october)
    Result := IntToStr(year) + '-' + IntToStr(monthNum) + '-XX (' + MonthNames[monthNum] + ')' 
  else
    Result := IntToStr(year) + '-XX-XX (' + IntToStr(monthNum) + ')';
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lbProgramName.Caption:='SyncthingLazurite';
  lbProgramVer.Caption:='v' + GetProgramVersion(); // Показываем только две цифры версии
  lbProgramVerBuild.Caption:='Build: ' + GetBuildNumber(); // Показываем номер билда
  lbProgramVerDate.Caption:='Date: ' + GetDateFromVersion(); // Показываем дату в формате мм.гг
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

