unit TOtools; {© Theo van Soest, Lazarus 2.0.10/FPC 3.2.0: 2019 - 06/10/2020}
{$mode objfpc}{$h+}
{$BOOLEVAL OFF,WARN SYMBOL_PLATFORM OFF}

(*
=================================================================================
 This library is original work of Theo van Soest.
 It is published under the GNU Lesser General Public License v3 (LGPL-3.0).
 https://tldrlegal.com/license/gnu-lesser-general-public-license-v3-%28lgpl-3%29
=================================================================================
*)

interface                     {07/04/2020}

uses {$IFDEF Windows}
      Windows,
     {$ENDIF}
     {$IFDEF Unix}
      CLasses,
     {$ENDIF}
      SysUtils, Graphics,
      TObaseDef;

type CharSet=set of Char;


type
  UnitsType=(no_Units,Units_in_numerator,Units_in_denominator);

function  MemAvail                                          : Int64;
function  InvertColor(const Color                 :TColor;
                      LimitToBW                   :Boolean =False      ): TColor;
function  GetAppVersionString(IncludeBuildInfo    :Boolean =True;
                              MinorDigits         :Byte    =2;
                              IncludePosRelease   :Boolean =False;
                              IncludeAnyRelease   :Boolean =False      ): String;
function  AppBuildNumber                                                : Integer;
function  Stg2Int(const S                         :String              ): Integer;
function  Stg2Ext(const S                         :String              ): Extended;
function  FloatFormat(AValue                      :Extended;
                      SignificantDigits           :Word    =1          ): String;
function  NeededDecimals(AValue                   :Extended;
                         SignificantDigits        :Word    =1;
                         IncludeLeadingZeros      :Boolean =True       ): Word;
function  Num2Stg(const Value                     :Extended;
	                Field,Deci                :ShortInt;
                  Fill                            :Char    =#32        ): String;   overload;
function  Num2Stg(const Value                     :Int64;
                  Field                           :ShortInt=0;
                  Fill                            :Char    =#32        ): String;   overload;
procedure Set_Field_Deci(Value                    :Extended;
       		               var Field,Deci     :ShortInt;                          {eventuele startwaarden}
                         Use_Input_Width          :BOOLEAN             );             {t => ondergrens field,deci}
function CharSetReplaceAll(ACharSet               :CharSet;
                           ANewChar               :Char;
                           AString                :String              ): String;
function CharSetTrimAll(ACharSet                  :CharSet;
                        AString                   :String              ): String;
function CharSetToString(ACharSet                 :CharSet             ): String;
function CountChars(AString,CharList              :String              ): Integer;  overload;
function CountChars(AString                       :String;
                    ACharSet                      :CharSet             ): Integer;  overload;
{$IFDEF Windows}
procedure ShowTaskBar  (bShow                     :Boolean =True       );
procedure EnableTaskBar(bShow                     :Boolean =True       );
{$ENDIF}
function  StripCharSet (const S                   :String;
                        const TrimSet             :CharSet=csWhiteSpace): String;
procedure WaitLoop(const ms                       :Comp                );
function  MakeRect(const Left,Top,
                         Width,Height             :Integer             ): TRect;
function  InRect(const Apoint                     :TPoint;
                 const ARect                      :TRect               ): Boolean;
procedure MakeWinBounds(const ARect               :TRect;
                        var Left,Top,
                        Width,Height              :Integer             );
function GetNumCPU                                                      : Integer;
function CheckDecimalPointSeparator(AutoCorrect   :Boolean=True        ): Boolean;
function GetCommonAppdataRoot                                           : String;

var CommonAppData:String='c:\programdata\';

//----------------------------------------------------------------------------

implementation

uses FileInfo,
     {$IFDEF Unix}
      utf8process,
     {$ENDIF}
     Forms, Math, StrUtils, Registry;

{$IFDEF VER140}
{Delphi 6: compatibiliteit naar Delphi 7-code}
  procedure GetLocaleFormatSettings(ActualLCID           :LCID;
                                    var FormatSettings:TFormatSettings);
  var i: Integer;
  begin
  FormatSettings.CurrencyString              := CurrencyString;
  FormatSettings.CurrencyFormat              := CurrencyFormat;
  FormatSettings.NegCurrFormat               := NegCurrFormat;
  FormatSettings.ThousandSeparator           := ThousandSeparator;
  FormatSettings.DecimalSeparator            := DecimalSeparator;
  FormatSettings.CurrencyDecimals            := CurrencyDecimals;
  FormatSettings.DateSeparator               := DateSeparator;
  FormatSettings.ShortDateFormat             := ShortDateFormat;
  FormatSettings.LongDateFormat              := LongDateFormat;
  FormatSettings.TimeSeparator               := TimeSeparator;
  FormatSettings.TimeAMString                := TimeAMString;
  FormatSettings.TimePMString                := TimePMString;
  FormatSettings.ShortTimeFormat             := ShortTimeFormat;
  FormatSettings.LongTimeFormat              := LongTimeFormat;
  for i:= 1 to 12 do
    begin
    FormatSettings.ShortMonthNames[i]        := ShortMonthNames[i];
    FormatSettings.LongMonthNames[i]         := LongMonthNames[i];
    end;
  for i:= 1 to 7 do
    begin
    FormatSettings.ShortDayNames[i]          := ShortDayNames[i];
    FormatSettings.LongDayNames[i]           := LongDayNames[i];
    FormatSettings.EraNames[i]               := EraNames[i];
    FormatSettings.EraYearOffsets[i]         := EraYearOffsets[i];
    end;
  FormatSettings.SysLocale                   := SysLocale;
  FormatSettings.TwoDigitYearCenturyWindow:= TwoDigitYearCenturyWindow;
  FormatSettings.ListSeparator               := ListSeparator;
  end;

  procedure SetLocaleFormatSettings(FormatSettings:TFormatSettings);
  var i: Integer;
  begin
  CurrencyString              := FormatSettings.CurrencyString;
  CurrencyFormat              := FormatSettings.CurrencyFormat;
  NegCurrFormat               := FormatSettings.NegCurrFormat;
  ThousandSeparator           := FormatSettings.ThousandSeparator;
  DecimalSeparator            := FormatSettings.DecimalSeparator;
  CurrencyDecimals            := FormatSettings.CurrencyDecimals;
  DateSeparator               := FormatSettings.DateSeparator;
  ShortDateFormat             := FormatSettings.ShortDateFormat;
  LongDateFormat              := FormatSettings.LongDateFormat;
  TimeSeparator               := FormatSettings.TimeSeparator;
  TimeAMString                := FormatSettings.TimeAMString;
  TimePMString                := FormatSettings.TimePMString;
  ShortTimeFormat             := FormatSettings.ShortTimeFormat;
  LongTimeFormat              := FormatSettings.LongTimeFormat;
  for i:= 1 to 12 do
    begin
    ShortMonthNames[i]        := FormatSettings.ShortMonthNames[i];
    LongMonthNames[i]         := FormatSettings.LongMonthNames[i];
    end;
  for i:= 1 to 7 do
    begin
    ShortDayNames[i]          := FormatSettings.ShortDayNames[i];
    LongDayNames[i]           := FormatSettings.LongDayNames[i];
    EraNames[i]               := FormatSettings.EraNames[i];
    EraYearOffsets[i]         := FormatSettings.EraYearOffsets[i];
    end;
  SysLocale                   := FormatSettings.SysLocale;
  TwoDigitYearCenturyWindow:= FormatSettings.TwoDigitYearCenturyWindow;
  ListSeparator               := FormatSettings.ListSeparator;
  end;

  function StrToDateTime(ATimeString       :String;
                         AFormatSettings:TFormatSettings): TDateTime;
  var SFormatSettings:TFormatSettings;
  begin
  GetLocaleFormatSettings(SysLocale.DefaultLCID,SFormatSettings);
  SetLocaleFormatSettings(AFormatSettings);
  Result:= SysUtils.StrToDateTime(ATimeString);
  SetLocaleFormatSettings(SFormatSettings);
  end;
{$ENDIF}


{10/06/2020 FPC adaptations}
function InvertColor(const Color:TColor;
                     LimitToBW  :Boolean=False): TColor;
var r,g,b: byte;
begin
r:= Red(Abs(Color));
g:= Green(Abs(Color));
b:= Blue(Abs(Color));
if LimitToBW then
  begin
  if (r+g+b)>384 then result:= clBlack
  else                result:= clWhite;
  end
else                  result:= TColor((Color and $FF000000)+RGBTocolor(255-r,255-g,255-b));
end; {invertcolor}


{18/07/2015}
{03/08/2016 MinorDigits, stringarray implementation}
{05/06/2020 alternative version by Alan Chamberlain, corrected by TvS}
{30/06/2020 add dot before buildinfo}
function GetAppVersionString(IncludeBuildInfo :Boolean=True;
                             MinorDigits      :Byte   =2;
                             IncludePosRelease:Boolean=False;
                             IncludeAnyRelease:Boolean=False): String;
var VersionInfo: TVersionInfo;
begin
Result:= '';
try
  VersionInfo:= TVersionInfo.Create;
  VersionInfo.Load(HINSTANCE);
  Result:= IntToStr(VersionInfo.FixedInfo.FileVersion[0]) + '.' +
     Format('%0.*d',[MinorDigits,VersionInfo.FixedInfo.FileVersion[1]]);
  if (IncludeAnyRelease or (IncludePosRelease and (VersionInfo.FixedInfo.FileVersion[2]<>0))) then
     Result:= Result + '.' + IntToStr(VersionInfo.FixedInfo.FileVersion[2]);
  if IncludeBuildInfo then
     Result:= Result + '.' + IntToStr(VersionInfo.FixedInfo.FileVersion[3]);
 finally
   if assigned(VersionInfo) then
     VersionInfo.Free;
 end;
end; {getappversionstring}


{05/06/2020 alternative version by Alan Chamberlain}
function AppBuildNumber: Integer;                        //ACC 6/5/2020
var VersionInfo: TVersionInfo;
begin
Result:= 0;
try
  VersionInfo:= TVersionInfo.Create;
  VersionInfo.Load(Application.Handle);
  Result:= VersionInfo.FixedInfo.FileVersion[3];
 finally
   if assigned(VersionInfo) then VersionInfo.Free;
 end;
end; {appbuildnumber}


{06/10/2020 escape from fundamentals}
function CharSetTrimAll(ACharSet:CharSet;
                        AString :String): String;
var i: Integer;
begin
Result:= AString;
i:= Length(AString);
while i>0 do
  begin
  Dec(i);
  if Result.Chars[i] in ACharSet then
    Result:= Result.Remove(i,1);
  end;
end;

{06/10/2020 escape from fundamentals}
function CharSetReplaceAll(ACharSet:CharSet;
                           ANewChar:Char;
                           AString :String ): String;
var c: Char;
begin
Result:= AString;
for c:= #0 to #255 do
  if c in ACharSet then
    Result:= Result.Replace(c,ANewChar{,[rfReplaceAll]});
end;


{06/10/2020 escape from fundamentals}
function CharSetToString(ACharSet:CharSet): String;
var c: Char;
begin
Result:= '';
for c:= #0 to #255 do
  if c in ACharSet then
    Result:= Result+c;
end;


function CountChars(AString :String;
                    ACharSet:CharSet): Integer;
begin
Result:= CountChars(AString,CharSetToString(ACharSet));
end;


{06/10/2020 alternative for fundamentals}
function CountChars(AString,CharList:String): Integer;
var i: Integer;
begin
Result:= 0;
i     := CharList.Length;
while i>0 do
  begin
  Dec(i);
  Result:= Result+AString.CountChar(CharList.Chars[i]);                         //zero-based
  end;
end;


function Stg2Int(const S:String): Integer;
begin
Val(S,Result);
end;


function Stg2Ext(const S:String): Extended;
begin
Val(S,Result);
end;


function StripCharSet(const S         :String;
                      const TrimSet:CharSet=csWhiteSpace): String;
var i: Integer;
begin
Result:= S;
i:= Length(Result);
while i>0 do
  begin
  if Result[i] in TrimSet then Delete(Result,i,1);
  Dec(i);
  end;
end; {stripcharset}


procedure WaitLoop(const ms:Comp);
var i: Comp;
begin
i:= TimeStampToMSecs(DateTimeToTimeStamp(Now));
while TimeStampToMSecs(DateTimeToTimeStamp(Now))-i<ms do
  begin
  Sleep(1);
  Application.ProcessMessages;
  end;
end; {waitloop}


function MemAvail: Int64;
var HS: THeapStatus;
begin
Hs:= GetHeapStatus;
Result:= HS.TotalFree;
end; {memavail}


{17/04/2013, 25/02/2015}
{01/02/2018 NeededDecimals}
{21/02/2020}
//https://lazarus-ccr.sourceforge.io/docs/rtl/sysutils/decimalseparator.html
function FloatFormat(AValue              :Extended;
                     SignificantDigits:Word=1  ): String;
var Stg: String;
begin
if AValue=0 then Stg:= '0'
else             Stg:= Format('%0.*f',[NeededDecimals(AValue,SignificantDigits),AValue]);
if DefaultFormatSettings.DecimalSeparator<>'.' then Result:= AnsiReplaceStr(Stg,'.',DefaultFormatSettings.DecimalSeparator)
else                                                Result:= Stg;
end; {floatformat}


{01/02/2018}
{21/08/2018: applying the rounding might rond up to higher decade (9.9999 -> 10.00)}
{05/09/2018 IncludeLeadingZeros}
{09/12/2019 added zero protection}
function NeededDecimals(AValue                :Extended;
                        SignificantDigits     :Word=1;
                        IncludeLeadingZeros:Boolean=True): Word;
var LogValue: Extended;
begin
if AValue=0 then
  Result:= SignificantDigits-Min(1,SignificantDigits)
else
  begin
  AValue     := Abs(AValue);
  try
    LogValue:= Log10(AValue);
   except
    LogValue:= 0;
   end;
  if IncludeLeadingZeros then Logvalue:= Max(0.5,LogValue);
  Result:= SignificantDigits-Min(SignificantDigits,Ceil(LogValue));
  if AValue>1e-5 then
    try
      AValue:= Round(AValue*IntPower(10,Result));
      if AValue>0 then
          if Trunc(Log10(AValue))>Trunc(LogValue)+Result then
            Dec(Result);
     except
     end;
  end;
end; {neededdecimals}


{17/03/2000, 25/02/2015}
function Num2Stg(const Value:Extended;
                 Field,Deci    :ShortInt;
                 Fill          :Char=#32): String;
var i,j: ShortInt;
    Stg: String;
begin
i:= 0; j:= 0;
Set_Field_Deci(Value,i,j,False);  Field:= Max(Field,i);  Deci:= Max(Deci,j);
Stg:= Format('%*.*f',[Field,Deci,Value]);
if Fill<>#32 then Stg:= AnsiReplaceStr(Stg,#32,Fill);
if DefaultFormatSettings.DecimalSeparator<>'.' then Result:= AnsiReplaceStr(Stg,'.',DefaultFormatSettings.DecimalSeparator)
else                                                Result:= Stg;
end; {num2stg}


{17/03/2000}
function  Num2Stg(const Value:Int64;
		              Field         :ShortInt=0;
                  Fill          :Char=#32): String;
var Stg: String;
begin
Stg:= Format('%*d',[Field,Value]);
if Fill<>#32 then Stg:= AnsiReplaceStr(Stg,#32,Fill);
Result:= Stg;
end; {num2stg}


{05/09/95}
procedure Set_Field_Deci(Value             :Extended;
       		            	 var Field,Deci    :ShortInt;
                   			 Use_Input_Width:BOOLEAN);
                         var I,J: ShortInt;
begin
if NOT Use_Input_Width then begin  Field:= 1;  Deci:= 0;  end; {else ondergrens}
I:= 1;  J:= 1;
if ABS(Value)>1E-9 then
  begin
  Value:= Log10(ABS(Value));
  if Value<0 then I:= -1;
  I:= Trunc(Value+I); {waarde van orde 'naar boven' afgerond}
{ if I<0 then Dec(I,Deci);    deci significante decimalen}
  end;
if Deci>0 then begin  Deci:= Max(Deci,-I);  Inc(J);  end;  {orde<0  ==> -i>0}
Field:= Max(Max(Abs(Field),I),Max(0,Deci)+J);
end; {set_field_deci}


function MakeRect(const Left,Top,Width,Height:Integer): TRect;
begin
Result.Left     := Left;
Result.Top      := Top;
Result.Right    := Left+Width ;
Result.Bottom:= Top +Height;
end; {makerect}


function InRect(const Apoint:TPoint;
                const ARect    :TRect): Boolean;
begin
with APoint,Arect do Result:= (x>=Left) and (x<=Right) and (y>=Top) and (y<=Bottom);
end; {inrect}


procedure MakeWinBounds (const ARect                   :TRect;
                         var   Left,Top,Width,Height:Integer);
begin
Left     := ARect.Left;
Top      := Arect.Top;
Width    := (ARect.Right -Left);
Height:= (ARect.Bottom-Top );
end; {~makewinbounds}


{$IFDEF Windows}
procedure ShowTaskBar(bShow:Boolean=True);
begin
if bShow then ShowWindow(FindWindow('Shell_TrayWnd',nil), SW_SHOWNA)
else          ShowWindow(FindWindow('Shell_TrayWnd',nil), SW_HIDE  );
end; {showtaskbar}


procedure EnableTaskBar(bShow:Boolean=True);
begin
if bShow then EnableWindow(FindWindow('Shell_TrayWnd',nil),TRUE )
else          EnableWindow(FindWindow('Shell_TrayWnd',nil),FALSE);
end; {enabletaskbar}
{$ENDIF}


function GetNumCPU: Integer;
var Reg: TRegistry;
begin
Reg:= TRegistry.Create(KEY_READ);
try
  Result        := 1;
  Reg.RootKey:= HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('\SYSTEM\ControlSet001\Control\Session Manager\Environment',False) then
     begin
     val(Reg.ReadString('NUMBER_OF_PROCESSORS'),Result);
     end
  else if Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Session Manager',False) then
     Result:= Reg.ReadInteger('RegisteredProcessors')
  else Result:= 1;
 finally
    FreeAndNil(Reg);
 end;
end; {getnumcpu}


function GetCommonAppdataRoot: String;
var Reg: TRegistry;
    i,j: Integer;
    Stg: String;
begin
Reg:= TRegistry.Create(KEY_READ);
try
  Result        := CommonAppData;
  Reg.RootKey:= HKEY_LOCAL_MACHINE;
  if Reg.OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders',False) then
    Result      := Reg.ReadString('Common AppData');
 finally
    FreeAndNil(Reg);
 end;
repeat
  i:= Pos('%',Result);
  if i>0 then
    begin
    Delete(Result,i,1);
    j:= Pos('%',Result);
    if j>0 then
      begin
      Stg:= Copy(Result,i,j-i);
      Stg:= GetEnvironmentVariable(Stg);
      Delete(Result,i,j);
      Insert(Stg,Result,i);
      end;
    end;
until i=0;
end; {getcommonappdataroot}


{24/02/2015}
function CheckDecimalPointSeparator(AutoCorrect:Boolean=True): Boolean;
var Reg: TRegistry;
begin
Reg:= TRegistry.Create(KEY_READ or KEY_SET_VALUE);
try
  Result        := False;
  Reg.RootKey:= HKEY_CURRENT_USER;
  if Reg.OpenKey('\Control Panel\International',False) then
    begin
    Result:= Reg.ReadString('sDecimal')='.';
    if (not Result) and AutoCorrect then
      begin
      Reg.WriteString('sDecimal','.');
      Result:= Reg.ReadString('sDecimal')='.';
      end;
    end;
 finally
    FreeAndNil(Reg);
 end;
end; {checkdecimalpointseparator}

end.
