unit uModuleMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, ExtCtrls, Menus, ActnList,
  UniqueInstance;

type

  { TModuleMain }

  TModuleMain = class(TDataModule)
    actShowOptions: TAction;
    ActionListGUI: TActionList;
    ImageTryIcons: TImageList;
    imgJSON: TImageList;
    menuTrayIcon: TPopupMenu;
    miExit: TMenuItem;
    miShow: TMenuItem;
    TrayIcon: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    procedure actShowOptionsExecute(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private

  public

  end;

var
  ModuleMain: TModuleMain;

implementation

{$R *.lfm}

uses
  Forms,
  uFormOptions,
  uFormMain;

{ TModuleMain }

procedure TModuleMain.TrayIconDblClick(Sender: TObject);
begin
  frmMain.WindowState := wsNormal;
  frmMain.Show();
  frmMain.SetFocus();
end;

procedure TModuleMain.actShowOptionsExecute(Sender: TObject);
begin
  frmOptions.ShowModal();
end;

end.

