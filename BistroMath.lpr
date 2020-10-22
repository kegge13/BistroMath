program BistroMath;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, lazcontrols, runtimetypeinfocontrols, WellForm,
  KlemLogo;

{$R *.res}

begin
  {$if declared(useHeapTrace)}
   useHeapTrace             := False;
  {$endIf}
  RequireDerivedFormResource:= True;
  Application.Scaled        := True;
  Application.Initialize;
  Application.CreateForm(TAnalyseForm, AnalyseForm);
  Application.Run;
end.

