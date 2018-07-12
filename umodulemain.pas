unit uModuleMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, ExtCtrls, Menus, UniqueInstance;

type

  { TModuleMain }

  TModuleMain = class(TDataModule)
    ImageTryIcons: TImageList;
    imgJSON: TImageList;
    menuTrayIcon: TPopupMenu;
    miExit: TMenuItem;
    miShow: TMenuItem;
    TrayIcon: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
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
  uFormMain;

{ TModuleMain }

procedure TModuleMain.TrayIconDblClick(Sender: TObject);
begin
  frmMain.WindowState := wsNormal;
  frmMain.Show();
  frmMain.SetFocus();
end;

end.

