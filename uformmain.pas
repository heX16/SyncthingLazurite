unit uFormMain;

//todo: if API key is not valid - analize this error

{$mode objfpc}{$H+}

{
Dependency:

Test:
* https://github.com/esvignolo/delphi-rest-client-api
  в этой библиотеке нет асинхронности,
  но зато есть парсинг

* https://github.com/libevent/libevent
  библиотека для C
  очень мощная и вроде простая
  в теории ее можно прикрутить.
  оссобено после моих тестов,
  где я обнаружил потенциальную возможность
  скомпилировать C файлы, и затем их obj
  файлы исползовать напрямую в lazarus.
  правда когда 10 лет назад мы это тестировали
  там был баг, причем на уровне lazarus.

Dependency old:
* LNet https://github.com/almindor/lnet
  пришлось отказаться - там утечка памяти (issue создал - #32).
  асинхронная.
  надо попробовать пофиксить утечку - кажется это будет проще,
  тем все остальные варианты.

* Python
  затея хорошая, но он дает слишком много сложностей.
  код по конвертированию данных
  по сложности такойже как парсинг данных из json.
  но учитывая что на Lazarus нет Async HTTP _вообще_
  то кажется что Python это единственный простой вариант.

}

interface

uses
  Classes,
  Math,
  SysUtils,
  //SynHighlighterJScript,
  //SynEdit,
  uModuleCore, VirtualTrees, Forms, Controls,
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
  uModuleMain;

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
  // WIP
  ImageIndex:=0;
  // migration: commented Core usage
  // if Core.MapFolderInfo.GetValue(Core.ListFolderInfo[Node^.Index], item) then
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
    // migration: commented Core usage
    // NodeHeight := (Sender as TVirtualStringTree).ComputeNodeHeight(
    //     TargetCanvas, Node, 0, Core.ListDev_GetText(Node^.Index) + ' ___') + cDevListAdditionalHeight;
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
