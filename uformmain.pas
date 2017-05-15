unit uFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  //SynHighlighterJScript,
  //SynEdit,
  uModuleCore,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Button1: TButton;
    btnStart: TButton;
    btnStop: TButton;
    edConsole: TMemo;
    edJSONView: TMemo;
    miShow: TMenuItem;
    miExit: TMenuItem;
    Panel1: TPanel;
    menuTrayIcon: TPopupMenu;
    Splitter1: TSplitter;
    TrayIcon: TTrayIcon;
    procedure Button1Click(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miShowClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  fpjson, jsonparser, jsonscanner;

{$R *.lfm}


{ TfrmMain }

procedure TfrmMain.Button1Click(Sender: TObject);
var
  Response: TStringList;
  JN: TJSONParser;
begin
  Response := TStringList.Create;

  try
    if Core.GetHTTPText('rest/system/config', Response) then
    begin
      //edJSONView.Text:=Response.Text;
      JN := TJSONParser.Create(Response.Text, [joUTF8]);
      edJSONView.Text:=JN.Parse.FormatJSON();
      //Response.SaveToFile('c:\response.txt');
    end;
  finally
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
  WindowState:=wsNormal;
  Show;
  SetFocus;
end;

end.
