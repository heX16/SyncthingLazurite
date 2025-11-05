program SyncthingLazurite;

{$ifdef DEBUG}
  {$apptype console}
{$endif}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uFormMain, laz_synapse, uniqueinstance_package, AsyncHttp,
  uFormOptions, uModuleMain, uformjsonview, hashmapstr, FormAbout,
  usyncthingtypes, syncthing_api, uSyncthingManager, uLogging;

{$R *.res}

begin
  Application.Scaled:=True;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TModuleMain, ModuleMain);
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmJSONView, frmJSONView);
  Application.Run;
end.

