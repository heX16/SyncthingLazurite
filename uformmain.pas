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
  StdCtrls, ExtCtrls, Graphics;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnShowWeb: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnOptions: TButton;
    edConsole: TMemo;
    grpEvents: TGroupBox;
    grpFolders: TGroupBox;
    grpDevices: TGroupBox;
    lbExcDetected: TLabel;
    listEvents: TMemo;
    Panel1: TPanel;
    shStatusCircle: TShape;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    treeFolders: TVirtualStringTree;
    treeDevices: TVirtualStringTree;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miShowClick(Sender: TObject);
    procedure treeDevicesGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure treeDevicesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure treeDevicesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure treeFoldersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
  public
  end;

var
  frmMain: TfrmMain;

resourcestring
  cOffline = 'Offline';
  cDays = 'days';
  cAddress = 'Address';

implementation

uses
  dateutils,
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

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Module must be destroy at first
  if Assigned(ModuleMain) then
    FreeAndNil(ModuleMain);
end;

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
    Hide();
end;

procedure TfrmMain.miShowClick(Sender: TObject);
begin
  //todo: make action
  ModuleMain.TrayIconDblClick(nil);
end;

procedure TfrmMain.treeDevicesGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  d: TDevInfo;
begin
  HintText := '';
  if Core.MapDevInfo.GetValue(Core.ListDevInfo[Node^.Index], d) then
  begin
    HintText := HintText + d.Id;
    if (not d.Connected) and (DaysBetween(d.LastSeen, Now()) < 31*6) then
      HintText :=  HintText + #13 + cOffline + ': ' + IntToStr(DaysBetween(d.LastSeen, Now())) + ' ' + cDays;
    if d.Connected then
      HintText :=  HintText + #13 + cAddress + ': ' + d.Address;
  end;
end;

procedure TfrmMain.treeDevicesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  d: TDevInfo;
begin
  ImageIndex:=0;
  if Core.MapDevInfo.GetValue(Core.ListDevInfo[Node^.Index], d) then
  begin
    if d.Connected then
      ImageIndex:=1;

    if d.Paused then
      ImageIndex:=2;
  end;
end;

procedure TfrmMain.treeDevicesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  d: TDevInfo;
begin
  if Node^.Index < Core.ListDevInfo.Count then
    CellText := Core.MapDevInfo[Core.ListDevInfo[Node^.Index]].Name else
    CellText := 'ERROR';
end;

procedure TfrmMain.treeFoldersGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Node^.Index < Core.ListFolderInfo.Count then
    CellText := Core.MapFolderInfo[Core.ListFolderInfo[Node^.Index]].Name else
    CellText := 'ERROR';
end;

end.
