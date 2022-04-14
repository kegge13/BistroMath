unit KlemLogo; {c Theo van Soest 24/02/2021}
{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TAboutBox = class(TForm)
    Panel      : TPanel;
    ProductName: TStaticText;
    Version    : TLabel;
    Copyright  : TLabel;
    Comments   : TMemo;
    OKButton   : TButton;
    Timer      : TTimer;
    constructor Create         (AOwner : TComponent);                           override;
    procedure   FormKeyPress   (Sender : TObject;
                                var Key: Char);
    procedure   FormMouseMove  (Sender : TObject;
                                Shift  : TShiftState;
                                X,Y    : Integer);
    procedure   TimerFade      (Sender : TObject);
    procedure   CommentsKeyDown(Sender : TObject;
                                var Key: Word;
                                Shift  : TShiftState);
   private
    Xold,Yold  : Integer;
  end;

var AboutBox        : TAboutBox;
    BMBuildNumber   : Integer;

implementation

uses StrUtils, LCLversion,
     Math,
     TObaseDef,TOtools,TOnumparser;

{$R *.lfm}

var LogoStrings: array[1..15] of String=(
      'This software is FREE and may not be sold or distributed on a commercial basis. '+
        'Distribution through media or internet is only allowed with written permission of the author.',
      '     Theo van Soest',
      '     bistromath@kegge13.nl',
      '------------------------------',
      '',
      'What is BistroMath?',
      'The program hooks up to the chain of clipboard viewers and thus analyses all '+
        'text based information. When a measurement is copied from '+
        'any of the supported systems, the data will be found as valid profile data. '+
        'For a inline/crossline scan always the correct edge positions are detected, '+
        'including profiles fully outside the main axis, wedged and FFF fields.',
      'Any profile may be stored as reference. When available and if configured, '+
        'the measurement will be divided by the reference profile. '+
        'Inline/crossline reference profiles are resampled to the measurement grid '+
        'and aligned with the measurement. Use F1 to get help.',
      'The user and only the user is responsible for all decisions based on this software.',
      '',
      'Why is it called BistroMath?',
      'Douglas Adams stated that numbers, just like time and space, are not absolute and called this bistromathics. DON''T PANIC',
      '',
      'It acknowledges that profile analysis does not give absolute results. Moreover, BistroMath serves a lot of different data formats.',
      'See the help file for more details.');

    LogoCnt,InsertPos: Integer;


{30/10/2020 auto insertion of date}
{24/02/2021 insert LCL/FPC version}
constructor TAboutBox.Create(AOwner: TComponent);
var dt: TDateTime;
    p : toTNumParser;
    s : String;
begin
inherited;
LogoCnt              := Length(LogoStrings);
InsertPos            := Comments.Lines.Count;
Comments .ParentFont := False;
Comments .Font.Color := clBlack;                                                //why isn't this working???
p                    := toTNumParser.Create;
p        .CurrentLine:= {$I %DATE%};
dt                   := p.NextDate;                                             //this procedure gives maximum flexibility
s                    := FormatDateTime('DD/MM/YYYY',dt);
Version  .Caption    := Format('version %s (build %d)   %s, generated with Lazarus v%s / FreePascal compiler v%s',[GetAppVersionString(False),BMBuildNumber,s,lcl_version,{$INCLUDE %FPCVERSION%}]);
CopyRight.Caption    := '© Theo van Soest, 2005 - '+RightStr(CharSetTrimAll(csComplete-csNumeric,s),4);
p.Free;
end; {~init}


procedure TAboutBox.FormMouseMove(Sender: TObject;
                                  Shift : TShiftState;
                                  X,Y   : Integer);
begin
if Abs(X-Xold)+Abs(Y-Yold)>2 then
  AlphaBlendValue:= Min(AlphaBlendValue+4,255);
Xold:= X;
Yold:= Y;
end; {~formmousemove}


procedure TAboutBox.FormKeyPress(Sender : TObject;
                                 var Key: Char);
begin
ModalResult:= mrOk;
end; {~formkeypress}


procedure TAboutBox.TimerFade(Sender: TObject);
begin
if LogoCnt>0 then
  begin
  Panel.Color:= Color;
  Comments.Lines.Insert(InsertPos,LogoStrings[LogoCnt]);
  Dec(LogoCnt);
  end
else
  begin
  AlphaBlendValue:= Max(AlphaBlendValue-1,1);
  if AlphaBlendValue<10 then
    ModalResult:= mrOk;
  end;
end; {~timerfade}


procedure TAboutBox.CommentsKeyDown(Sender :TObject;
                                    var Key:Word;
                                    Shift  :TShiftState);
begin
AlphaBlendValue:= Min(AlphaBlendValue+1,255);
end;


begin
BMBuildNumber:= AppBuildNumber;
end.
