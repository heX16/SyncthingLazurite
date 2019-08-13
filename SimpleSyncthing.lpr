program SimpleSyncthing;

{$ifdef DEBUG}
  {$apptype console}
{$endif}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uFormMain, laz_synapse, uniqueinstance_package, uModuleCore,
  AsyncHttp, uFormOptions, uModuleMain, uformjsonview, hashmapstr, FormAbout
  { you can add units after this };

{$R *.res}

begin
  Application.Scaled:=True;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TCore, Core);
  Application.CreateForm(TModuleMain, ModuleMain);
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmJSONView, frmJSONView);
  Application.Run;
end.

