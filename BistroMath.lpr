program BistroMath;
{$I BistroMath_opt.inc}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  cmem,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, tachartlazaruspkg, lazcontrols, runtimetypeinfocontrols,
  WellForm,
  KlemLogo;

{$R *.res}

begin
{$IFDEF HEAPTRACE}
 {$IFDEF DEBUG}
  {$IFDEF HEAPTRACE_REPORT}
  if FileExists('bistromath.trc') then
    DeleteFile('bistromath.trc');
  SetHeapTraceOutput('bistromath.trc');
  {$ENDIF}
 {$ENDIF DEBUG}
{$ELSE}
 {$if declared(useHeapTrace)}
  useHeapTrace            := False;
 {$endIf}
{$ENDIF HEAPTRACE}
RequireDerivedFormResource:= True;
Application.Scaled        := True;
Application.Initialize;
Application.CreateForm(TAnalyseForm, AnalyseForm);
Application.Run;
end.

