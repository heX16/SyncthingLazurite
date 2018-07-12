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
    grpDevices: TGroupBox;
    ListBox1: TListBox;
    Panel1: TPanel;
    shStatusCircle: TShape;
    Splitter1: TSplitter;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miShowClick(Sender: TObject);
  private
  public
  end;

var
  frmMain: TfrmMain;

implementation

uses
  uModuleMain,
  //fpjson,
  httpsend,
  uFormOptions,
  jsonparser, jsonscanner;

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

end.
