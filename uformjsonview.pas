unit uFormJsonView;

{$mode objfpc}{$H+}

interface

uses
  uModuleCore,
  AsyncHttp, fpjson,
  Classes, SysUtils, Forms, Controls, StdCtrls,
  ExtCtrls, ComCtrls, Menus;

type

  { TfrmJSONView }

  TfrmJSONView = class(TForm)
    btnHintUpd: TButton;
    edJSONView: TMemo;
    imgJSON: TImageList;
    edHintFieldsList: TLabeledEdit;
    listEndpoints: TListBox;
    MainMenu: TMainMenu;
    mnShowHintPanel: TMenuItem;
    mnCollapseAll: TMenuItem;
    mnExpandAll: TMenuItem;
    panelHintFieldsList: TPanel;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TimerUpdateTreeView: TTimer;
    treeJsonData: TTreeView;
    procedure btnHintUpdClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure listGetAPIClick(Sender: TObject);
    procedure mnShowHintPanelClick(Sender: TObject);
    procedure TimerUpdateTreeViewTimer(Sender: TObject);

  private

  public
    hintList: TStringList;
    endpoint: string;
    json: TJSONData;

    procedure httpGetAPItoTree(Request: THttpRequest);

    procedure ShowJSONDocument(TV: TTreeView; DataSource: TJSONData;
      Compact: boolean = False; SortObjectMembers: boolean = False);

  end;


procedure JSONDataAddToTreeView(TV: TTreeView; AParent: TTreeNode;
      Data: TJSONData; Compact: boolean = False;
      SortObjectMembers: boolean = False);

procedure JSONDataAddToTreeView_OnAddingNode(TV: TTreeView; AParent: TTreeNode;
      ACurrent: TTreeNode;
      Data: TJSONData);

var
  frmJSONView: TfrmJSONView;

implementation

uses
  Dialogs,
  jsonparser, jsonscanner;

{$R *.lfm}

procedure TfrmJSONView.httpGetAPItoTree(Request: THttpRequest);
var
  JData: TJSONData;
  i: integer;
var
  node: TTreeNode;
begin
  if Self.json <> nil then
     FreeAndNil(Self.json);
  if HttpRequestToJson(Request, JData) then
    Self.json := JData;
  Self.TimerUpdateTreeView.Enabled:=true;
end;

procedure TfrmJSONView.TimerUpdateTreeViewTimer(Sender: TObject);
begin
  Self.TimerUpdateTreeView.Enabled:=false;
  if Self.json = nil then
    Self.json := TJSONData.Create;

  edJSONView.Text := Self.json.FormatJSON();
  ShowJSONDocument(Self.treeJsonData, Self.json, True);
end;


// Returns image type code for a given TJSONtype
function JSONTypeToImageIndex(const Value: TJSONtype): Integer;
begin
  case Value of
    jtUnknown: Result := -1;
    jtNumber:  Result := 8;
    jtString:  Result := 9;
    jtBoolean: Result := 7;
    jtNull:    Result := 6;
    jtArray:   Result := 17;
    jtObject:  Result := 16;
  else
    Result := -1;
  end;
end;

procedure JSONDataAddToTreeView(TV: TTreeView; AParent: TTreeNode;
  Data: TJSONData; Compact: boolean; SortObjectMembers: boolean);
var
  N, N2: TTreeNode;
  I: integer;
  D: TJSONData;
  S: TStringList;
  NWasAdded: boolean;
begin
  if not Assigned(Data) then
    exit;
  if Compact and (AParent <> nil) then
    begin
      N := AParent;
      NWasAdded := False;
    end
  else
    begin
      N := TV.Items.AddChild(AParent, '');
      NWasAdded := True;
    end;

  case Data.JSONType of
    jtArray,
    jtObject:
    begin
      S := TStringList.Create;
      try
        // collect json items to string list
        for I := 0 to Data.Count - 1 do
          if Data.JSONtype = jtArray then
            // name as "array index"
            S.AddObject(IntToStr(I), Data.items[i])
          else
            // name as "dict key"
            S.AddObject(TJSONObject(Data).Names[i], Data.items[i]);

        // sort string list
        if SortObjectMembers and (Data.JSONType = jtObject) then
          S.Sort;

        // enum string list and add to tree view
        for I := 0 to S.Count - 1 do
        begin
          N2 := TV.Items.AddChild(N, S[i]);
          D := TJSONData(S.Objects[i]);

          N2.ImageIndex := JSONTypeToImageIndex(D.JSONType);
          N2.SelectedIndex := JSONTypeToImageIndex(D.JSONType);

          // notify about newly added node
          JSONDataAddToTreeView_OnAddingNode(TV, N, N2, D);

          // recursion! ->
          JSONDataAddToTreeView(TV, N2, D, Compact, SortObjectMembers);
        end
      finally
        FreeAndNil(S);
      end;
    end;
    jtNull:
      N.Text := N.Text + ': ' + 'Null';
    else
      N.Text := N.Text + ': ' + Data.AsString;
      //if (Data.JSONType=jtString) then C:='"'+C+'"';
  end;

  N.ImageIndex := JSONTypeToImageIndex(Data.JSONType);
  N.SelectedIndex := JSONTypeToImageIndex(Data.JSONType);
  N.Data := Data;

  // notify only if this node was actually added here (not reused in Compact mode)
  if NWasAdded then
    JSONDataAddToTreeView_OnAddingNode(TV, AParent, N, Data);
end;

procedure JSONDataAddToTreeView_OnAddingNode(TV: TTreeView; AParent: TTreeNode;
  ACurrent: TTreeNode; Data: TJSONData);
begin

end;

procedure TfrmJSONView.listGetAPIClick(Sender: TObject);
begin
  if listEndpoints.ItemIndex>=0 then
  begin
    self.endpoint := Trim(listEndpoints.Items[listEndpoints.ItemIndex]);

    // Remove everything after " (" including the bracket and space
    if Pos(' (', self.endpoint) > 0 then
      self.endpoint := Copy(self.endpoint, 1, Pos(' (', self.endpoint) - 1);

    Core.API_Get(self.endpoint, @httpGetAPItoTree);
  end;
end;

procedure TfrmJSONView.btnHintUpdClick(Sender: TObject);
begin
  // WIP...!!!!!!!!!!!!!!!!!!!!!!
end;

procedure TfrmJSONView.FormCreate(Sender: TObject);
begin
  //WIP
end;

procedure TfrmJSONView.FormDestroy(Sender: TObject);
begin
  //WIP
end;

procedure TfrmJSONView.mnShowHintPanelClick(Sender: TObject);
begin
  panelHintFieldsList.Visible := not panelHintFieldsList.Visible;
end;

procedure TfrmJSONView.ShowJSONDocument(TV: TTreeView; DataSource: TJSONData;
  Compact: boolean; SortObjectMembers: boolean);
var  
  node: TTreeNode;
begin
  TV.Items.BeginUpdate;
  try
    TV.Items.Clear;
    JSONDataAddToTreeView(TV, nil, DataSource, Compact, SortObjectMembers);

    Self.treeJsonData.FullCollapse();
    node := Self.treeJsonData.Items.GetFirstNode;
    if node <> nil then
    begin
      node.Expand(False);
      while Assigned(node) do
      begin
        // enum only root nodes
        node := node.GetNextSibling;
        if node <> nil then
        begin
          node.Expand(False);
        end;
      end;
    end;
  finally
    TV.Items.EndUpdate;
  end;
end;


end.

