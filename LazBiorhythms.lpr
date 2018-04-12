program LazBiorhythms;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, formLazBiorhythms, formhelp, formLicence,
  LazBiorhythmsUntils, formAbout, formOptions
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmLazBiorhythms, frmLazBiorhythms);
  Application.CreateForm(TfrmHelp, frmHelp);
  Application.CreateForm(TfrmLicence, frmLicence);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.Run;
end.

