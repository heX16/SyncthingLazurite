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
    chRunSyncOnStart: TCheckBox;
    edPathToConfig: TLabeledEdit;
    edPathToExec: TLabeledEdit;
    edAPIKey: TLabeledEdit;
    XMLPropStorageConfig: TXMLPropStorage;
  private

  public

  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.lfm}

end.

