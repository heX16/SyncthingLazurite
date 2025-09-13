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
    chUseProxyOutputForFixBug: TCheckBox;
    chScheduleIntervalMode: TCheckBox;
    chRunSyncOnStart: TCheckBox;
    edInterval: TComboBox;
    edPathToConfigDir: TLabeledEdit;
    edPathToExecWithFilename: TLabeledEdit;
    edAPIKey: TLabeledEdit;
    grpSchedule: TGroupBox;
    IniPropStorageConfig: TIniPropStorage;
    lbSchedulePauseBetweenRuns: TLabel;
    procedure chScheduleIntervalModeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  lbSchedulePauseBetweenRuns.Enabled:=chScheduleIntervalMode.Checked;
end;

procedure TfrmOptions.chScheduleIntervalModeChange(Sender: TObject);
begin
  lbSchedulePauseBetweenRuns.Enabled:=chScheduleIntervalMode.Checked;
end;

end.

