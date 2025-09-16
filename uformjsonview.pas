unit uFormJsonView;

{$mode objfpc}{$H+}

interface

uses
  uModuleCore,
  syncthing_api,
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
    mnCopyExpandRecursive: TMenuItem;
    mnCopyValueText: TMenuItem;
    mnCopyValue: TMenuItem;
    mnCopyName: TMenuItem;
    mnCopyJsonRecursive: TMenuItem;
    mnCopyPath: TMenuItem;
    mnTestConnect: TMenuItem;
    mnTestJson: TMenuItem;
    mnTestGrp: TMenuItem;
    mnShowHintPanel: TMenuItem;
    mnCollapseAll: TMenuItem;
    mnExpandAll: TMenuItem;
    panelHintFieldsList: TPanel;
    PopupMenuJson: TPopupMenu;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TimerUpdateTreeView: TTimer;
    treeJsonData: TTreeView;
    procedure btnHintUpdClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure listGetAPIClick(Sender: TObject);
    procedure mnCopyExpandRecursiveClick(Sender: TObject);
    procedure mnCopyJsonRecursiveClick(Sender: TObject);
    procedure mnCopyNameClick(Sender: TObject);
    procedure mnCopyPathClick(Sender: TObject);
    procedure mnCopyValueClick(Sender: TObject);
    procedure mnCopyValueTextClick(Sender: TObject);
    procedure mnShowHintPanelClick(Sender: TObject);
    procedure mnTestConnectClick(Sender: TObject);
    procedure mnTestJsonClick(Sender: TObject);
    procedure TimerUpdateTreeViewTimer(Sender: TObject);

  private
    // tmp
    sync_api: TSyncthingAPI;

  public
    hintList: TStringList;
    endpoint: string;
    json: TJSONData;

    procedure httpGetAPItoTree(Request: THttpRequest);

    procedure ShowJSONDocument(TV: TTreeView; DataSource: TJSONData;
      SortObjectMembers: boolean = False);

  end;


procedure JSONDataAddToTreeView(TV: TTreeView; AParent: TTreeNode;
      Data: TJSONData;
      SortObjectMembers: boolean = False);

procedure JSONDataAddToTreeView_OnAddingNode(TV: TTreeView; AParent: TTreeNode;
      ACurrent: TTreeNode;
      Data: TJSONData);

var
  frmJSONView: TfrmJSONView;

implementation

uses
  Dialogs,
  jsonparser, jsonscanner, StrUtils, Clipbrd;

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
    Self.json := TJSONObject.Create;

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

// Formats JSON value into compact inline string for hint display
function JSONValueToInlineText(const Value: TJSONData): string;
begin
  case Value.JSONType of
    jtNull:    Result := 'Null';
    jtArray:   Result := '[...]';
    jtObject:  Result := '{...}';
  else
    Result := Value.AsString;
  end;
end;

procedure JSONDataAddToTreeView(TV: TTreeView; AParent: TTreeNode;
  Data: TJSONData; SortObjectMembers: boolean);
var
  N, N2: TTreeNode;
  I: integer;
  D: TJSONData;
  S: TStringList;
  NWasAdded: boolean;
begin
  if not Assigned(Data) then
    exit;
  if (AParent <> nil) then
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
          N2.Data:=D;

          // notify about newly added node
          JSONDataAddToTreeView_OnAddingNode(TV, N, N2, D);

          // recursion! ->
          JSONDataAddToTreeView(TV, N2, D, SortObjectMembers);
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
var
  Obj: TJSONObject;
  i, idx: Integer;
  hintName, appended, valueTxt: string;
  valData: TJSONData;
begin
  // Append hint values to object nodes label if their fields are listed in hintList
  if (Data <> nil)
    and (Data.JSONType = jtObject)
    and Assigned(ACurrent)
    and (ACurrent.Text <> '')
    and (frmJSONView.hintList.Count > 0) then
  begin
    Obj := TJSONObject(Data);
    appended := '';
    for i := 0 to frmJSONView.hintList.Count - 1 do
    begin
      hintName := Trim(frmJSONView.hintList[i]);
      if hintName = '' then
        Continue;
      idx := Obj.IndexOfName(hintName);
      if idx >= 0 then
      begin
        valData := Obj.Items[idx];
        valueTxt := JSONValueToInlineText(valData);
        if appended <> '' then
          appended := appended + ', ';
        appended := appended + hintName + ':' + valueTxt;
      end;
    end;
    if appended <> '' then
      ACurrent.Text := ACurrent.Text + ' (' + appended + ')';
  end;
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

procedure TfrmJSONView.mnCopyExpandRecursiveClick(Sender: TObject);
begin
  // expand tree of current item in treeJsonData
  if Assigned(treeJsonData.Selected) then
    treeJsonData.Selected.Expand(True);
end;

procedure TfrmJSONView.mnCopyJsonRecursiveClick(Sender: TObject);
begin
  // copy json as text of current item in treeJsonData
  // with sub items - recursive
  if Assigned(treeJsonData.Selected) and Assigned(treeJsonData.Selected.Data) then
    Clipboard.AsText := TJSONData(treeJsonData.Selected.Data).FormatJSON()
  else
    Clipboard.AsText := '';
end;

procedure TfrmJSONView.mnCopyNameClick(Sender: TObject);
var
  node: TTreeNode;
  nameOnly: string;
  p: Integer;
begin
  // copy name of element of current item in treeJsonData
  // if name not present - copy empty string
  node := treeJsonData.Selected;
  nameOnly := '';
  if Assigned(node) then
  begin
    nameOnly := node.Text;
    // cut value suffix like ": value"
    p := Pos(':', nameOnly);
    if p > 0 then
      nameOnly := Trim(Copy(nameOnly, 1, p - 1));
    // cut appended hints like " (field1:..., field2:...)"
    p := Pos(' (', nameOnly);
    if p > 0 then
      nameOnly := Trim(Copy(nameOnly, 1, p - 1));
  end;
  Clipboard.AsText := nameOnly;
end;

procedure TfrmJSONView.mnCopyPathClick(Sender: TObject);
var
  node, cur: TTreeNode;
  parts: TStringList;
  token, path, t: string;
  p: Integer;
  i, idx: Integer;
begin
  // copy path of current item in treeJsonData
  node := treeJsonData.Selected;
  path := '';
  if Assigned(node) then
  begin
    parts := TStringList.Create;
    try
      cur := node;
      while Assigned(cur) do
      begin
        token := cur.Text;
        // get name only (before ':' and before ' (')
        p := Pos(':', token);
        if p > 0 then
          token := Trim(Copy(token, 1, p - 1));
        p := Pos(' (', token);
        if p > 0 then
          token := Trim(Copy(token, 1, p - 1));
        token := Trim(token);
        if token <> '' then
          parts.Add(token);
        cur := cur.Parent;
      end;
      // build path from root to selected
      for i := parts.Count - 1 downto 0 do
      begin
        t := parts[i];
        if TryStrToInt(t, idx) then
        begin
          // array index style: previous part + [idx]
          if path = '' then
            path := '[' + t + ']'
          else
            path := path + '[' + t + ']';
        end
        else
        begin
          if path = '' then
            path := t
          else
            path := path + '.' + t;
        end;
      end;
    finally
      parts.Free;
    end;
  end;
  Clipboard.AsText := path;
end;

procedure TfrmJSONView.mnCopyValueClick(Sender: TObject);
var
  node: TTreeNode;
  s: string;
  data: TJSONData;
begin
  // copy value of element of current item in treeJsonData
  // if value not present - copy empty string
  node := treeJsonData.Selected;
  s := '';
  if Assigned(node) and Assigned(node.Data) then
  begin
    data := TJSONData(node.Data);
    case data.JSONType of
      jtString,
      jtNumber,
      jtBoolean:
        s := data.AsString;
      jtNull:
        s := '';
    else
      s := '';
    end;
  end;
  Clipboard.AsText := s;
end;

procedure TfrmJSONView.mnCopyValueTextClick(Sender: TObject);
var
  node: TTreeNode;
begin
  // copy JSON literal (quoted string, numbers, booleans, null; objects/arrays as compact JSON)
  node := treeJsonData.Selected;
  if Assigned(node) and Assigned(node.Data) then
    Clipboard.AsText := TJSONData(node.Data).AsJSON
  else
    Clipboard.AsText := '';
end;

procedure TfrmJSONView.btnHintUpdClick(Sender: TObject);
var
  parts: specialize TArray<String>;
  s,s_for: string;
begin
  hintList.Clear;

  // Split by comma into dynamic array
  parts := SplitString(edHintFieldsList.Text, ',');

  // Normalize and copy non-empty, unique tokens
  for s_for in parts do
  begin
    s := Trim(s_for);
    if s <> '' then
      if hintList.IndexOf(s) < 0 then
        hintList.Add(s);
  end;

  // Re-render the JSON tree to apply hint annotations
  ShowJSONDocument(Self.treeJsonData, Self.json, True);
end;

procedure TfrmJSONView.FormCreate(Sender: TObject);
begin
  hintList := TStringList.Create;
  json := nil;
end;

procedure TfrmJSONView.FormDestroy(Sender: TObject);
begin
  FreeAndNil(hintList);
  FreeAndNil(json);
end;

procedure TfrmJSONView.mnShowHintPanelClick(Sender: TObject);
begin
  panelHintFieldsList.Visible := not panelHintFieldsList.Visible;
end;

procedure TfrmJSONView.mnTestConnectClick(Sender: TObject);
begin
  // TODO: remove me
  sync_api := TSyncthingAPI.Create(self);
  sync_api.SetAPIKey(Core.APIKey);
  sync_api.SetEndpoint('127.0.0.1', 8384, false);
  sync_api.Connect();
end;

procedure TfrmJSONView.mnTestJsonClick(Sender: TObject);
begin
  ShowJSONDocument(self.treeJsonData, sync_api.TreeRoot, false);
end;

procedure TfrmJSONView.ShowJSONDocument(TV: TTreeView; DataSource: TJSONData;
  SortObjectMembers: boolean);
var  
  node: TTreeNode;
begin
  TV.Items.BeginUpdate;
  try
    TV.Items.Clear;
    JSONDataAddToTreeView(TV, nil, DataSource, SortObjectMembers);

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

