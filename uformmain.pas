unit uFormMain;

//todo: if API key is not valid - analize this error

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  //SynHighlighterJScript,
  //SynEdit,
  uModuleCore, VirtualTrees, Forms, Controls,
  StdCtrls, ExtCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    btnOptions: TButton;
    edConsole: TMemo;
    grpFolders: TGroupBox;
    grpDevices: TGroupBox;
    lbDevices: TListBox;
    Panel1: TPanel;
    shStatusCircle: TShape;
    Splitter1: TSplitter;
    treeFolders: TVirtualStringTree;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miShowClick(Sender: TObject);
    procedure treeFoldersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
  public
    FoldersItems: TStrings;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uModuleMain;

{$R *.lfm}


{ TfrmMain }

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  Core.Start();
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  Core.Stop();
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Core.Done();
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FoldersItems := TStringList.Create();
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Module must be destroy at first
  if Assigned(ModuleMain) then
    FreeAndNil(ModuleMain);
  FreeAndNil(FoldersItems);
end;

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
    Hide();
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.miShowClick(Sender: TObject);
begin
  //todo: make action
  ModuleMain.TrayIconDblClick(nil);
end;

procedure TfrmMain.treeFoldersGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  CellText := FoldersItems[Node^.Index];
end;

end.
