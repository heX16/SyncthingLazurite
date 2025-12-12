unit uFormOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLConf, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, StdCtrls, XMLPropStorage, IniPropStorage;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    BitBtn1: TBitBtn;
    btnSelectDirConfig: TButton;
    btnSelectFileExec: TButton;
    chAutoGetAPIKey: TCheckBox;
    chConnectOnStart: TCheckBox;
    chUseProxyOutputForFixBug: TCheckBox;
    chRunSyncOnStart: TCheckBox;
    cbLanguages: TComboBox;
    edPortNumber: TLabeledEdit;
    edPathToConfigDir: TLabeledEdit;
    edPathToExecWithFilename: TLabeledEdit;
    edAPIKey: TLabeledEdit;
    IniPropStorageConfig: TIniPropStorage;
    lbLanguage: TLabel;
    lbLanguage_Fixed: TLabel;
    procedure chRunSyncOnStartChange(Sender: TObject);
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

procedure TfrmOptions.chRunSyncOnStartChange(Sender: TObject);
begin
  if chRunSyncOnStart.Checked then
    chConnectOnStart.Checked := True;
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
begin
  // Fill combo with available localization files from /languages folder
  langDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'languages';

  files := TStringList.Create;
  try
    files.Sorted := True;
    files.Duplicates := dupIgnore;

    if DirectoryExists(langDir) then
    begin
      FindAllFiles(files, langDir, 'SyncthingLazurite.*.po', False);
      FindAllFiles(files, langDir, 'SyncthingLazurite.*.pot', False);
    end;

    cbLanguages.Items.BeginUpdate;
    try
      cbLanguages.Items.Clear;
      for i := 0 to files.Count - 1 do
        cbLanguages.Items.Add(ExtractFileName(files[i]));

      if (cbLanguages.Items.Count > 0) and (cbLanguages.ItemIndex < 0) then
        cbLanguages.ItemIndex := 0;
    finally
      cbLanguages.Items.EndUpdate;
    end;
  finally
    files.Free;
  end;
end;

end.

