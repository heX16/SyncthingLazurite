unit uFormJsonView;

{$mode objfpc}{$H+}

interface

uses
  uModuleCore,
  AsyncHttp, fpjson,
  Classes, SysUtils, Forms, Controls, StdCtrls,
  ExtCtrls, ComCtrls;

type

  { TfrmJSONView }

  TfrmJSONView = class(TForm)
    edJSONView: TMemo;
    listGetAPI: TListBox;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    treeJsonData: TTreeView;
    procedure listGetAPIClick(Sender: TObject);
  private

  public
    procedure httpGetAPItoTree(Request: THttpRequest);
    procedure ShowJSONDocument(TV: TTreeView; DataSource: TJSONData;
      Compact: boolean = False; SortObjectMembers: boolean = False);
    procedure ShowJSONData(TV: TTreeView; AParent: TTreeNode;
      Data: TJSONData; Compact: boolean = False;
      SortObjectMembers: boolean = False);
  end;

var
  frmJSONView: TfrmJSONView;

implementation

uses
  jsonparser, jsonscanner;

{$R *.lfm}

procedure TfrmJSONView.httpGetAPItoTree(Request: THttpRequest);
var
  JData: TJSONData;
begin
  if HttpRequestToJson(Request, JData) then
  try
    edJSONView.Text := JData.FormatJSON();
    ShowJSONDocument(treeJsonData, JData, True);
    treeJsonData.FullExpand();
  finally
    JData.Free();
  end;
end;

procedure TfrmJSONView.ShowJSONData(TV: TTreeView; AParent: TTreeNode;
  Data: TJSONData; Compact: boolean; SortObjectMembers: boolean);

const
  ImageTypeMap: array[TJSONtype] of integer =
    //      jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject
    (-1, 8, 9, 7, 6, 5, 4);

var
  N, N2: TTreeNode;
  I: integer;
  D: TJSONData;
  S: TStringList;

begin
  if not Assigned(Data) then
    exit;
  if Compact and (AParent <> nil) then
    N := AParent
  else
    N := TV.Items.AddChild(AParent, '');

  case Data.JSONType of
    jtArray,
    jtObject:
    begin
      S := TStringList.Create;
      try
        // collect json items to string list
        for I := 0 to Data.Count - 1 do
          if Data.JSONtype = jtArray then
            S.AddObject(IntToStr(I), Data.items[i])
          else
            S.AddObject(TJSONObject(Data).Names[i], Data.items[i]);

        // sort string list
        if SortObjectMembers and (Data.JSONType = jtObject) then
          S.Sort;

        // enum string list and add to tree view
        for I := 0 to S.Count - 1 do
        begin
          N2 := TV.Items.AddChild(N, S[i]);
          D := TJSONData(S.Objects[i]);
          N2.ImageIndex := ImageTypeMap[D.JSONType];
          N2.SelectedIndex := ImageTypeMap[D.JSONType];
          // recursion! ->
          ShowJSONData(TV, N2, D, Compact, SortObjectMembers);
        end
      finally
        S.Free;
      end;
    end;
    jtNull:
      N.Text := N.Text + ': ' + 'Null';
    else
      N.Text := N.Text + ': ' + Data.AsString;
      //if (Data.JSONType=jtString) then C:='"'+C+'"';
  end;

  N.ImageIndex := ImageTypeMap[Data.JSONType];
  N.SelectedIndex := ImageTypeMap[Data.JSONType];
  N.Data := Data;
end;

procedure TfrmJSONView.listGetAPIClick(Sender: TObject);
var endpoint: string;
begin
  if listGetAPI.ItemIndex>=0 then
  begin
    endpoint := listGetAPI.Items[listGetAPI.ItemIndex];

    // Remove everything after " (" including the bracket and space
    if Pos(' (', endpoint) > 0 then
      endpoint := Copy(endpoint, 1, Pos(' (', endpoint) - 1);

    Core.API_Get(endpoint, @httpGetAPItoTree);
  end;
end;

procedure TfrmJSONView.ShowJSONDocument(TV: TTreeView; DataSource: TJSONData;
  Compact: boolean; SortObjectMembers: boolean);
begin
  with TV.Items do
  begin
    BeginUpdate;
    try
      TV.Items.Clear;
      ShowJSONData(TV, nil, DataSource, Compact, SortObjectMembers);
      with TV do
        if (Items.Count > 0) and Assigned(Items[0]) then
        begin
          Items[0].Expand(False);
          Selected := Items[0];
        end;
    finally
      EndUpdate;
    end;
  end;
end;


end.

