unit uFormOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLConf, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, StdCtrls, XMLPropStorage;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    BitBtn1: TBitBtn;
    btnSelectDirConfig: TButton;
    btnSelectFileExec: TButton;
    chAuto: TCheckBox;
    chUseProxyOutputForFixBug: TCheckBox;
    chIntervalMode: TCheckBox;
    chRunSyncOnStart: TCheckBox;
    edInterval: TComboBox;
    edPathToConfig: TLabeledEdit;
    edPathToExec: TLabeledEdit;
    edAPIKey: TLabeledEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    XMLPropStorageConfig: TXMLPropStorage;
    procedure chIntervalModeChange(Sender: TObject);
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
  Label1.Enabled:=chIntervalMode.Checked;
end;

procedure TfrmOptions.chIntervalModeChange(Sender: TObject);
begin
  Label1.Enabled:=chIntervalMode.Checked;
end;

end.

