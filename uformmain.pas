unit uFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  //SynHighlighterJScript,
  //SynEdit,
  uModuleCore,
  fpjson,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Button1: TButton;
    btnStart: TButton;
    btnStop: TButton;
    edConsole: TMemo;
    edJSONView: TMemo;
    imgJSON: TImageList;
    miShow: TMenuItem;
    miExit: TMenuItem;
    Panel1: TPanel;
    menuTrayIcon: TPopupMenu;
    Splitter1: TSplitter;
    TrayIcon: TTrayIcon;
    treeJsonData: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miShowClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private

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
  jsonparser, jsonscanner;

{$R *.lfm}


{ TfrmMain }

procedure TfrmMain.Button1Click(Sender: TObject);
var
  Response: TStringList;
  JN: TJSONParser;
  JData: TJSONData;
begin
  Response := TStringList.Create;

  try
    if Core.GetHTTPText('rest/system/config', Response) then
    begin
      //edJSONView.Text:=Response.Text;
      JN := TJSONParser.Create(Response.Text, [joUTF8]);
      JData := JN.Parse();
      edJSONView.Text := JData.FormatJSON();
      ShowJSONDocument(treeJsonData, JData, True);
      //Response.SaveToFile('c:\response.txt');
    end;
  finally
    JData.Free;
    Response.Free;
    JN.Free;
  end;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  Core.Start();
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  Core.Stop();
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
  SCaption = 'JSON Viewer';
  SEmpty = 'Empty document';
  //SArray = '[%d]';
  //SObject = '[%d]';
  SArray   = 'Array (%d elements)';
  SObject  = 'Object (%d members)';
  SNull = 'null';

const
  ImageTypeMap: array[TJSONtype] of integer =
    //      jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject
    (-1, 8, 9, 7, 6, 5, 4);

var
  N, N2: TTreeNode;
  I: integer;
  D: TJSONData;
  C: string;
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
      If (Data.JSONType=jtArray) then
        C:=SArray
      else
        C:=SObject;
      C:=Format(C,[Data.Count]);
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
          ShowJSONData(TV, N2, D, Compact, SortObjectMembers);
        end
      finally
        S.Free;
      end;
    end;
    jtNull:
      C := SNull;
    else
      C := Data.AsString;
      //if (Data.JSONType=jtString) then C:='"'+C+'"';
  end;

  if Assigned(N) then
  begin
    //if jtArray, jtObject
    if N.Text = '' then
      N.Text := C
    else
      N.Text := N.Text + ': ' + C;
    N.ImageIndex := ImageTypeMap[Data.JSONType];
    N.SelectedIndex := ImageTypeMap[Data.JSONType];
    N.Data := Data;
  end;
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
