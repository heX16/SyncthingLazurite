unit uModulePreInit_i18n;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniPropStorage;

type

  { TModulePreInit_i18n }

  TModulePreInit_i18n = class(TDataModule)
    IniPropStorageConfig: TIniPropStorage;
    procedure DataModuleCreate(Sender: TObject);
    procedure LoadLang();
  private

  public

  end;

var
  ModulePreInit_i18n: TModulePreInit_i18n;

function GetLangDir: string;
function GetLangCode(const LangFile: string): string;

implementation

uses
  Forms,
  TypInfo,
  LCLType,
  LCLTranslator,
  DefaultTranslator;

{$R *.lfm}

function GetLangDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'languages';
end;

function GetLangCode(const LangFile: string): string;
const
  prefix = 'SyncthingLazurite.';
var
  code: string;
begin
  code := ChangeFileExt(LangFile, '');
  if Copy(code, 1, Length(prefix)) = prefix then
    Delete(code, 1, Length(prefix));
  Result := code;
end;

{ TModulePreInit_i18n }

procedure TModulePreInit_i18n.DataModuleCreate(Sender: TObject);
begin
  // Initialization before creating all forms
  LoadLang();
end;

procedure TModulePreInit_i18n.LoadLang();
var
  Language, langDir: string;
begin
  Language := IniPropStorageConfig.ReadString('cbLanguages_Text', '');
  langDir := GetLangDir();
  if Language <> '' then
    SetDefaultLang(GetLangCode(Language), langDir)
  else
    SetDefaultLang('en', langDir);

end;

end.

