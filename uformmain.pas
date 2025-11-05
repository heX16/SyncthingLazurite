unit uFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Math,
  SysUtils,
  VirtualTrees, Forms, Controls,
  StdCtrls, ExtCtrls, Graphics, IniPropStorage;

const
  cDevListAdditionalHeight = 8;

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
    IniPropStoragePos: TIniPropStorage;
    listEvents: TListBox;
    Panel1: TPanel;
    shStatusCircle: TShape;
    SplitterBottom1: TSplitter;
    SplitterLeft: TSplitter;
    SplitterEventsBottom: TSplitter;
    treeFolders: TVirtualStringTree;
    treeDevices: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure listEventsClick(Sender: TObject);
    procedure miShowClick(Sender: TObject);
    procedure treeDevicesGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure treeDevicesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure treeDevicesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure treeDevicesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure treeDevicesMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure treeDevicesResize(Sender: TObject);
    procedure treeFoldersGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
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
  uSyncthingTypes,
  uModuleMain,
  Clipbrd;

{$R *.lfm}


{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Self.treeDevices.RootNodeCount:=0;
  Self.treeFolders.RootNodeCount:=0;
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

procedure TfrmMain.listEventsClick(Sender: TObject);
var
  idx: Integer;
begin
  // Copy selected event line to clipboard on double-click
  idx := listEvents.ItemIndex;
  if idx >= 0 then
    Clipboard.AsText := listEvents.Items[idx];
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
  item: TDevInfo;
begin
  HintText := ModuleMain.GetDeviceHint(Node^.Index);
end;

procedure TfrmMain.treeDevicesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  item: TDevInfo;
begin
  ImageIndex:=0;
  if Column = 0 then
    ImageIndex := ModuleMain.GetDeviceIconIndex(Node^.Index);
end;

procedure TfrmMain.treeFoldersGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  item: TFolderInfo;
begin
  ImageIndex:=0;
  // TODO: WIP treeFoldersGetImageIndex
  // if Folder[Node^.Index] then
  // begin
  //   (*
  //   if not item.DirectoryExists() then
  //     ImageIndex:=1;
  //
  //   if item.Paused then
  //     ImageIndex:=2;
  //   *)
  // end;
end;

procedure TfrmMain.treeDevicesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Column = 0 then
    CellText := ModuleMain.GetDeviceDisplayText(Node^.Index)
  else
    CellText := '';
end;

procedure TfrmMain.treeDevicesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  // Multiline example:
  // MultilineDemo.pas `TNodeForm.MLTreeInitNode`

  // For nodes that need multiline text
  Include(InitialStates, ivsMultiline);
end;

procedure TfrmMain.treeDevicesMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  if (Node <> nil) and Sender.MultiLine[Node] then
  begin
    TargetCanvas.Font := Sender.Font;
    NodeHeight := (Sender as TVirtualStringTree).ComputeNodeHeight(
        TargetCanvas, Node, 0, ModuleMain.GetDeviceDisplayText(Node^.Index) + ' ___') + cDevListAdditionalHeight;
  end;
  // else use default height
end;

procedure TfrmMain.treeDevicesResize(Sender: TObject);
begin
  with Sender as TVirtualStringTree do
  begin
    Header.Columns[0].Width:=Max(ClientWidth, Header.Columns[0].MinWidth);
  end;
end;


procedure TfrmMain.treeFoldersGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  // Read from SyncthingAPI JSON pointers via module helper
  CellText := ModuleMain.GetFolderDisplayText(Node^.Index);
end;

end.
