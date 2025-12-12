unit uFormOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLConf, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, StdCtrls, XMLPropStorage, IniPropStorage,
  DefaultTranslator, uget_os_language;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    BitBtn1: TBitBtn;
    btnSelectDirConfig: TButton;
    btnSelectFileExec: TButton;
    chAutoGetAPIKey: TCheckBox;
    chConnectOnStartup: TCheckBox;
    chUseProxyOutputForFixBug: TCheckBox;
    chRunSyncthingOnStartup: TCheckBox;
    cbLanguages: TComboBox;
    edPortNumber: TLabeledEdit;
    edPathToConfigDir: TLabeledEdit;
    edPathToExecWithFilename: TLabeledEdit;
    edAPIKey: TLabeledEdit;
    IniPropStorageConfig: TIniPropStorage;
    lbLanguage: TLabel;
    lbLanguage_Fixed: TLabel;
    procedure chRunSyncthingOnStartupChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure LoadLanguagesList;

  public

  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.chRunSyncthingOnStartupChange(Sender: TObject);
begin
  if chRunSyncthingOnStartup.Checked then
    chConnectOnStartup.Checked := True;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  LoadLanguagesList;
end;

procedure TfrmOptions.LoadLanguagesList;
var
  files: TStringList;
  langDir: string;
  i: Integer;
  currentLangFilename: string;
  idx: Integer;
begin
  // Fill combo with available localization files from /languages folder
  langDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'languages';

  currentLangFilename := cbLanguages.Text;
  // TODO: WIP - use the `GetOSLanguage`
  if currentLangFilename = '' then
    currentLangFilename := 'SyncthingLazurite.en.po';
    // WIP: currentLangFilename := Format('SyncthingLazurite.%s.po', [ GetOSLanguage .... ;

  files := TStringList.Create;
  try
    files.Sorted := True;
    files.Duplicates := dupIgnore;

    if DirectoryExists(langDir) then
    begin
      FindAllFiles(files, langDir, 'SyncthingLazurite.*.po', False);
    end;

    cbLanguages.Items.BeginUpdate;
    try
      cbLanguages.Items.Clear;
      for i := 0 to files.Count - 1 do
      begin
        cbLanguages.Items.Add(ExtractFileName(files[i]));
      end;

      idx := cbLanguages.Items.IndexOf(currentLangFilename);
      if idx >= 0 then
        cbLanguages.ItemIndex := idx
      else
        cbLanguages.ItemIndex := cbLanguages.Items.IndexOf('SyncthingLazurite.en.po');

    finally
      cbLanguages.Items.EndUpdate;

      cbLanguages.Style:=csDropDownList;
    end;
  finally
    files.Free;
  end;
end;

end.

