unit uFormOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLConf, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, StdCtrls, XMLPropStorage, IniPropStorage,
  DefaultTranslator, uget_os_language, uLangUtils;

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
    destructor Destroy; override;
  private
    procedure LoadLanguagesList;

  public
    FileNameList: TStringList;

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

  if self.edPathToExecWithFilename.Text = '---' then
  begin
    {$IFDEF Windows}
    self.edPathToExecWithFilename.Text := '.\syncthing.exe'
    {$ELSE}
    self.edPathToExecWithFilename.Text := './syncthing'
    {$ENDIF}
  end;
end;

destructor TfrmOptions.Destroy;
begin
  FreeAndNil(FileNameList);
  inherited Destroy;
end;

procedure TfrmOptions.LoadLanguagesList;
var
  files: TStringList;
  langDir: string;
  i: Integer;
  currentLangFilename: string;
  idx: Integer;
  head: string;
  langNative, langEng: string;
begin
  // Fill combo with available localization files from /languages folder
  langDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'languages';

  if FileNameList = nil then
    FileNameList := TStringList.Create;
  FileNameList.Clear;

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
        // Read a small head of the file and extract language names
        head := ReadFirstBytesUtf8Safe(files[i], 0, 10);
        if ExtractLanguageNames(head, langNative, langEng) and (langNative <> '') then
          cbLanguages.Items.Add(langNative + ' / ' + langEng)
        else
          cbLanguages.Items.Add(ExtractFileName(files[i])); // fallback

        FileNameList.Add(ExtractFileName(files[i]));
      end;

      idx := FileNameList.IndexOf(currentLangFilename);
      if idx >= 0 then
        cbLanguages.ItemIndex := idx
      else
        cbLanguages.ItemIndex := FileNameList.IndexOf('SyncthingLazurite.en.po');

    finally
      cbLanguages.Items.EndUpdate;

      cbLanguages.Style:=csDropDownList;
    end;
  finally
    files.Free;
  end;
end;

end.

