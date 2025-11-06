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
    chAuto: TCheckBox;
    chConnectOnStart: TCheckBox;
    chUseProxyOutputForFixBug: TCheckBox;
    chRunSyncOnStart: TCheckBox;
    edPortNumber: TLabeledEdit;
    edPathToConfigDir: TLabeledEdit;
    edPathToExecWithFilename: TLabeledEdit;
    edAPIKey: TLabeledEdit;
    IniPropStorageConfig: TIniPropStorage;
    procedure chRunSyncOnStartChange(Sender: TObject);
  private

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

end.

