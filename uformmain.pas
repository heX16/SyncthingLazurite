unit uFormMain;

//todo: if API key is not valid - analize this error

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  //SynHighlighterJScript,
  //SynEdit,
  AsyncHttp,
  uModuleCore, fpjson, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, ComCtrls, Buttons;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    btnOptions: TButton;
    edConsole: TMemo;
    edJSONView: TMemo;
    imgJSON: TImageList;
    listGetAPI: TListBox;
    miShow: TMenuItem;
    miExit: TMenuItem;
    Panel1: TPanel;
    menuTrayIcon: TPopupMenu;
    shStatusCircle: TShape;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TrayIcon: TTrayIcon;
    treeJsonData: TTreeView;
    procedure btnGetAPIClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miShowClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private

    procedure httpGetAPItoTree(Query: THttpQuery);
  public
    procedure ShowJSONDocument(TV: TTreeView; DataSource: TJSONData;
      Compact: boolean = False; SortObjectMembers: boolean = False);
    procedure ShowJSONData(TV: TTreeView; AParent: TTreeNode;
      Data: TJSONData; Compact: boolean = False;
      SortObjectMembers: boolean = False);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  //fpjson,
  uFormOptions,
  jsonparser, jsonscanner;

{$R *.lfm}


{ TfrmMain }

procedure TfrmMain.httpGetAPItoTree(Query: THttpQuery);
var
  StrResponse: TStringList;
  JN: TJSONParser;
  JData: TJSONData;
  strm: TMemoryStream;
begin
  writeln(Query.CallState, Core.APIKey);
  if Query.CallState=httpLoadStart then
    Query.SetRequestHeader('X-API-Key', Core.APIKey)
  else if Query.CallState=httpLoad then
  begin
    StrResponse := TStringList.Create();
    try
      writeln(Query.Status);
      strm := Query.Response;
      if strm.Size>0 then
      begin
        StrResponse.LoadFromStream(Query.Response);
        JData:=nil;
        JN:=nil;
        edJSONView.Text := StrResponse.Text;
        JN := TJSONParser.Create(StrResponse.Text, [joUTF8]);
        try
          JData := JN.Parse();
        except
          on EJSONParser do JData := nil;
        end;
        if JData <> nil then
        begin
          edJSONView.Text := JData.FormatJSON();
          ShowJSONDocument(treeJsonData, JData, True);
          treeJsonData.FullExpand();
        end;
      end;
    finally
      if JData<>nil then
        JData.Free();
      if JN<>nil then
        JN.Free();
      StrResponse.Free();
    end;
  end;
end;

procedure TfrmMain.btnGetAPIClick(Sender: TObject);
var s:TStrings;
begin
  s:=TStringList.Create;
  Core.GetHTTPText('rest/'+listGetAPI.Items[listGetAPI.ItemIndex], s);
  ShowMessage(s.Text);

  Core.aiohttp.Get(Core.SyncthigServer+'rest/'+listGetAPI.Items[listGetAPI.ItemIndex], @httpGetAPItoTree);
end;

procedure TfrmMain.btnOptionsClick(Sender: TObject);
begin
  frmOptions.ShowModal();
end;

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

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
    Hide;
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.miShowClick(Sender: TObject);
begin
  TrayIconDblClick(nil);
end;

procedure TfrmMain.TrayIconDblClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
  SetFocus;
end;



procedure TfrmMain.ShowJSONData(TV: TTreeView; AParent: TTreeNode;
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

procedure TfrmMain.ShowJSONDocument(TV: TTreeView; DataSource: TJSONData;
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
