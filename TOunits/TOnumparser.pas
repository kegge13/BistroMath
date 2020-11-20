unit TOnumparser;  {© Theo van Soest 2015-16/11/2020}
{$I TOnumparser_opt.inc}
{$R-}

(*
=================================================================================
 This library is original work of Theo van Soest.
 It is published under the GNU Lesser General Public License v3 (LGPL-3.0).
 https://tldrlegal.com/license/gnu-lesser-general-public-license-v3-%28lgpl-3%29
=================================================================================
*)

interface

uses Classes,Math,
     TObaseDef;

const
  toTDefNegSign    =['-'        ];
  toTDefPosSign    =['+'        ];
  toTDefDecPoint   =[',','.'    ];
  toTDefExponent   =['e','E','^'];
  toDefMonths      ='jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec';
  toDefNumMonths   =12;

type
  toTTestChars     =#33..#127;
  toTCharSet       =set of toTTestChars;
  toMonthsArr      =array[1..toDefNumMonths] of String;
  toMDYhmsOrder    =string[6];

  toCharToNumAlias =record
                      toAlias: Char;
                      toNum  : String;
                    end;

  {14/02/2016 replaced tmemorystream with tstringstream}
  {11/06/2016 added property CurrentLinePos}
  {29/09/2016 removed FileClose, FFileOpened, GetLineCount, FMemIO}
  {30/03/2017 Tab2Space}
  {17/01/2018 interfaceto delimiter, added delimitercountcheck}
  {13/10/2020 AutoSetDecPoint}
  {16/11/2020 added FTopLineNr, SetTop, ResetTop}
  toTNumParser=class(TObject)
  private
    FAliasValues     : array of toCharToNumAlias;
    FConversionResult: Boolean;
    FConversionString: String;
    FDecPointSet     : toTCharSet;
    FDelimiterCount  : Integer;
    FErrorString     : String;
    FExp             : Integer;
    FExponentSet     : toTCharSet;
    FFileName        : String;
    FFileTime        : TDateTime;
    FLastLineOk      : String;
    FLastLineOkNr    : Integer;
    FLeft            : Integer;
    FLeftConv        : Integer;
    FLine            : String;
    FLineCount       : Integer;
    FLineNr          : Integer;                                                 //1-based; zero-based internal
    FTopLineNr       : Integer;
    FNegSignSet      : toTCharSet;
    FNumberSet       : toTCharSet;
    FPosSignSet      : toTCharSet;
    FRight           : Integer;
    FRightConv       : Integer;
    FSearchText      : String;
    FStatusProc      : toExtMsgProc;
    FStrings         : TStringList;
    FPreLoaded       : Boolean;
    function    FindNum(IntegersOnly:Boolean=False): Boolean;
    function    GetRemainderOfLine                 : String;
    function    GetErrorString                     : String;
    function    GetStringsStatus                   : Boolean;
    procedure   SetLineNr(ANumber:Integer);
    procedure   SetPreload(AState:Boolean);
    procedure   SetCurrentLine(Stg:String);
    procedure   ParseMessage(const AMessage:String);
  public
    Months: toMonthsArr;
    constructor Create(StringList               :TStrings         =nil           );  reintroduce;  overload;
    constructor Create(AFileNameOrList          :String;
                       IsCommaSepList           :Boolean          =False;
                       ADelimiter               :Char             =','           );  reintroduce;  overload;
    procedure   Assign(AStream                  :TStream;
                       IgnorePreload            :Boolean          =False         );  overload;
    procedure   Assign(StringList               :TStrings;
                       IgnorePreload            :Boolean          =False         );  overload;
    procedure   Assign(AFileName                :String                          );  overload;
    procedure   Assign(AFileName                :String;
                       ARawStream               :TStream;
                       EmptyRawStreamWhenRead   :Boolean          =True          );  overload;
    procedure   Clear;
    procedure   SetLineOk;
    procedure   SetMonths         (CommaSepStg  :string           =toDefMonths   );
    procedure   SetNumberChars    (NumberChars  :toTCharSet       =csNumeric     );
    procedure   SetNegSignChars   (NegSignChars :toTCharSet       =toTDefNegSign );
    procedure   SetPosSignChars   (PosSignChars :toTCharSet       =toTDefPosSign );
    procedure   SetDecPointChars  (DecPointChars:toTCharSet       =toTDefDecPoint);  overload;
    procedure   SetDecPointChars  (DecPointChars:String                          );  overload;
    procedure   SetExponentChars  (ExponentChars:toTCharSet       =toTDefExponent);
    procedure   SetDelimiter      (ADelimiter   :Char             =','           );
    procedure   SetStatusProcedure(StatusProc   :toExtMsgProc=nil);
    procedure   SetAliasValue     (AAlias       :Char;
                                   AValue       :String);
    procedure   AutoSetDecPoint   (DecPointChars:toTCharSet       =toTDefDecPoint);  overload;
    procedure   AutoSetDecPoint   (DecPointChars:String                          );  overload;
    function    Search(ASearchText       :String;
                       CaseInSensitive   :Boolean=False;
                       FromCurrentLine   :Boolean=False;
                       NotFoundMessage   :Boolean=True   ): Boolean;        overload;
    function    Search(ASearchText       :String;
                       OptionalText      :String;
                       CaseInSensitive   :Boolean=False;
                       FromCurrentLine   :Boolean=False;
                       NotFoundMessage   :Boolean=True   ): Boolean;        overload;
    procedure   SetTop(ALineNr           :Integer);
    procedure   ResetTop;
    function    GotoTop(ReadFirstLine    :Boolean=False;
                        Tab2Space        :Boolean=True   ): Boolean;
    function    GotoLeft                                  : Boolean;
    function    NextLine(MoveLineOk      :Boolean=False;
                         Tab2Space       :Boolean=True   ): Boolean;
    function    CountNumbers(IntegersOnly:Boolean=False  ): Word;
    function    CommaText                                 : String;
    function    NextFloat(MaxPositive    :Extended=1e20;
                          MinPositive    :Extended=0;
                          NegativeAllowed:Boolean =True  ): Extended;
    function    NextInteger(MinLimit:Integer=-MaxInt;
                            MaxLimit:Integer= MaxInt     ): Integer;
    function    NextMonth                                 : Integer;
    function    NextDate(DateOrder:toMDYhmsOrder='dmyHMS';
                         AM_order :toMDYhmsOrder='mdyHMS'): TDateTime;
    function    NextChar                                  : Char;
    function    NextString(StopChar :Char)                : String;        overload;
    function    NextString(StopChars:toTCharSet)          : String;        overload;
    function    NextString(NumChar  :Integer)             : String;        overload;
    function    GetConversionResult                       : Boolean;
    function    GetEOFstatus                              : Boolean;
    function    GetString(Index:Integer)                  : String;
    function    GetDelimiter                              : Char;
    function    DelimiterCountCheck                       : Boolean;
    destructor  Destroy;                                                 override;
  published
    property    ConversionFirstPos   :Integer     read FLeftConv;
    property    ConversionLastPos    :Integer     read FRightConv;
    property    ConversionResult     :Boolean     read GetConversionResult;
    property    ConversionString     :String      read FConversionString;
    property    CurrentLine          :String      read FLine              write SetCurrentLine;     //1-based; zero-based internal
    property    CurrentLineNumber    :Integer     read FLineNr            write SetLineNr;          //1-based; zero-based internal
    property    CurrentLinePos       :Integer     read FLeft;
    property    Delimiter            :Char        read GetDelimiter       write SetDelimiter;
    property    DelimiterCount       :Integer     read FDelimiterCount;
    property    ErrorString          :String      read GetErrorString     write FErrorString;
    property    EndOfFile            :Boolean     read GetEOFstatus;
    property    FileDate             :TDateTime   read FFileTime;
    property    FileInMemory         :Boolean     read GetStringsStatus;
    property    LineCount            :Integer     read FLineCount;
    property    LastLineOk           :String      read FLastLineOk;
    property    LastLineOkNumber     :Integer     read FLastLineOkNr;
    property    PreLoaded            :Boolean     read FPreLoaded         write SetPreLoad;
    property    FileName             :String      read FFileName;
    property    RemainderOfLine      :String      read GetRemainderOfLine;
    property    SearchText           :String      read FSearchText;
    property    Strings              :TStringList read FStrings;
  end;


function SelfTest: Boolean;

implementation

uses SysUtils,StrUtils,DateUtils,
     TOtools,TOmath;

const
     toNumParser_nolines=-1;
resourcestring
     toNumParser_notfound ='"%s" not found (%s)';


{16/11/2020 FTopLineNr}
constructor toTNumParser.Create(StringList:TStrings=nil);
begin
inherited Create;
FStrings         := TStringList.Create;
FLineNr          := -1;
FTopLineNr       := -1;
FLastLineOkNr    := 0;
FLine            := '';
FLineCount       := toNumParser_nolines;
FLastLineOk      := '';
FConversionResult:= False;
FConversionString:= '';
FFileTime        := Date;
FPreLoaded       := False;
SetMonths;
SetNumberChars;
SetNegSignChars;
SetPosSignChars;
SetDecPointChars;
SetExponentChars;
if StringList<>nil then Assign(StringList);
end; {~create}


{29/09/2016 combined file/commaseplist}
{17/01/2018 support for alternative delimiter and delimitercountcheck}
{06/10/2020 fundamentals alternative}
constructor toTNumParser.Create(AFileNameOrList :String;
                                IsCommaSepList  :Boolean=False;
                                ADelimiter      :Char   =','   );
begin
Create(nil);
if IsCommaSepList then
  begin
  Delimiter         := ADelimiter;
  FStrings.CommaText:= AFileNameOrList;
  FLineCount        := FStrings.Count;
  FDelimiterCount   := AFileNameOrList.CountChar(Delimiter);
  end
else
  Assign(AFileNameOrList);
end; {~create}


{17/01/2018}
procedure toTNumParser.SetDelimiter(ADelimiter:Char=',');
begin
FStrings.Delimiter:= ADelimiter;
end; {~setdelimter}


{09/12/2015}
{16/11/2020 FTopLineNr}
procedure toTNumParser.Clear;
begin
if assigned(FStrings) then FStrings.Clear;
FLineNr   := 0;
FTopLineNr:= 0;
FLineCount:= 0;
FPreLoaded:= False;
FFileName := '';
end; {~clear}


{09/12/2015}
procedure toTNumParser.SetPreload(AState:Boolean);
begin
FPreLoaded:= AState and (Strings.Count>0);
end; {~setpreload}


{29/09/2016 removed FMemIO}
procedure toTNumParser.SetLineNr(ANumber:Integer);
begin
ANumber:= Max(FTopLineNr,ANumber);
FLineNr:= Min(Pred(ANumber),Pred(FStrings.Count));
repeat until (ANumber=FLineNr) or (not NextLine);
end; {~setlinenr}


procedure toTNumParser.SetAliasValue(AAlias:Char; AValue:String);
var i,j: Integer;
begin
i:= 0;
j:= Length(FAliasValues);
if j>0 then while (i<j) and (FAliasValues[i].toAlias<>AAlias) do Inc(i);
if i<j then
  begin
  if AValue='' then
    begin
    Dec(j);
    FAliasValues[i]:= FAliasValues[j];
    Setlength(FAliasValues,j);
    end;
  end
else
  begin
  SetLength(FAliasValues,Succ(j));
  FAliasValues[j].toAlias:= AAlias;
  FAliasValues[j].toNum  := AValue;
  end;
end; {~setaliasvalue}

{01/04/2020 addr(FStatusProc) changed to FStatusProc}
procedure toTNumParser.ParseMessage(const AMessage:String);
begin
if FStatusProc<>nil then FStatusProc(AMessage);
end; {~parsemessage}


procedure toTNumParser.SetCurrentLine(Stg:String);
begin
FLine := Stg;
FLeft := 0;
FRight:= 0;
end; {~setcurrentline}


{16/12/2015 direct load from stringstream added}
{16/11/2020 FTopLineNr}
procedure toTNumParser.Assign(AStream      :TStream;
                              IgnorePreload:Boolean=False);
begin
FLineNr   := 0;
FTopLineNr:= 0;
if assigned(AStream) and (not (PreLoaded and IgnorePreload and (LineCount=0))) then
  begin
  AStream.Position:= 0;
  FStrings.Clear;
  FStrings.LoadFromStream(AStream);
  FLineCount:= FStrings.Count;
  end
end; {~assign}


{09/12/2015: ignorepreload}
{16/11/2020 FTopLineNr}
procedure toTNumParser.Assign(StringList   :TStrings;
                              IgnorePreload:Boolean=False);
var AStream: TMemoryStream;
begin
FLineNr   := 0;
FTopLineNr:= 0;
if assigned(StringList) and (not (PreLoaded and IgnorePreload and (LineCount=0))) then
  begin
  AStream:= TMemoryStream.Create;
  StringList.SaveToStream(AStream);
  AStream.Position:= 0;
  FStrings.Clear;
  FStrings.LoadFromStream(AStream);
  AStream.Free;
  FLineCount:= FStrings.Count;
  end
end; {~assign}


{29/09/2016 FStream, removed FMemIO}
{16/11/2020 FTopLineNr}
procedure toTNumParser.Assign(AFileName:String);
begin
Clear;
FLineNr      := 0;
FTopLineNr   := 0;
FLineCount   := toNumParser_nolines;
FLastLineOkNr:= 0;
FlastLineOk  := '';
if (Length(AFileName)>0) and FileExists(AFileName) then
  begin
  FFileName:= AFileName;
  FFileTime:= FileDateToDateTime(FileAge(AFileName));
  FStrings.Clear;
  FStrings.LoadFromFile(AFileName);                                             //IgnoreEncoding=True (FPC 3.2.0)
  FLineCount:= FStrings.Count;
  end;
end; {~assign}


{19/05/2020 new variant}
procedure toTNumParser.Assign(AFileName             :String;
                              ARawStream            :TStream;
                              EmptyRawStreamWhenRead:Boolean=True);
begin
if ARawStream.Size>0 then
  begin
  FFileName:= AFileName;
  Assign(ARawStream,True);
  if EmptyRawStreamWhenRead then
    ARawStream.Size:= 0;
  end
else
  Assign(AFileName);
end; {~assign}


procedure toTNumParser.SetLineOk;
begin
if FLastLineOkNr<FLineNr then
  begin
  FLastLineOk  := FLine;
  FLastLineOkNr:= FLineNr;
  end;
end; {~setlineok}


function toTNumParser.GetStringsStatus: Boolean;
begin
Result:= FLineCount>toNumParser_nolines;
end; {~getstringsstatus}


function toTNumParser.GetErrorString: String;
begin
if Length(FErrorString)=0 then Result:= '?'
else                           Result:= FErrorString;
end; {~geterrorstrings}


{01/06/2020 initialise stg}
function toTNumParser.CommaText: String;
var Stg: String;
begin
Stg:= '';
if GetStringsStatus then
  Stg:= Strings.CommaText
else if GotoTop then
  begin
  while NextLine do Stg:= Stg+','+FLine;
  GotoTop;
  end;
Result:= Stg;
end; {~commatext}


procedure toTNumParser.SetMonths(CommaSepStg:string=toDefMonths);
var i: Integer;
    s: array of String;
begin
s:= CommaSepStg.Split(Delimiter);                                               //zero-based
i:= Min(Length(s),toDefNumMonths);
while i>0 do
  begin
  Months[i]:= s[i-1];                                                           //1-based
  Dec(i);
  end;
end; {~setmonths}


procedure toTNumParser.SetNumberChars(NumberChars:toTCharSet=csNumeric);
begin
if NumberChars<>[] then FNumberSet:= NumberChars;
end; {~setnumberchars}


procedure toTNumParser.SetNegSignChars(NegSignChars:toTCharSet=toTDefNegSign);
begin
{if NegSignChars<>[] then }FNegSignSet:= NegSignChars;
end; {~setnegsignchars}


procedure toTNumParser.SetPosSignChars(PosSignChars:toTCharSet=toTDefPosSign);
begin
{if PosSignChars<>[] then }FPosSignSet:= PosSignChars;
end; {~setpossignchars}


{13/10/2020}
procedure toTNumParser.SetDecPointChars(DecPointChars:String);
var s: toTCharSet;
    i: Integer;
begin
s:= [];
i:= Length(DecPointChars);
while i>0 do
  begin
  s:= s+[DecPointChars[i]];
  Dec(i);
  end;
SetDecPointChars(s);
end; {~setdecpointchars}


procedure toTNumParser.SetDecPointChars(DecPointChars:toTCharSet=toTDefDecPoint);
begin
if DecPointChars<>[] then
  FDecPointSet:= DecPointChars;
end; {~setdecpointchars}


procedure toTNumParser.SetExponentChars(ExponentChars:toTCharSet=toTDefExponent);
begin
if ExponentChars<>[] then
  FExponentSet:= ExponentChars;
end; {~setnumberchars}


{13/10/2020 new}
procedure toTNumParser.AutoSetDecPoint(DecPointChars:toTCharSet=toTDefDecPoint);
var c,r: Char;
    n,m: Integer;
begin
m:= 0;
for c:= Low(toTTestChars) to High(toTTestChars) do
  if c in DecPointChars then
    begin
    n:= FStrings.Text.CountChar(c);
    if n>m then
      begin
      m:= n;
      r:= c;
      end;
    end;
if m>0 then
  FDecPointSet:= [r]
else
  FDecPointSet:= DecPointChars;
end; {~autosetdecpoint}


procedure toTNumParser.AutoSetDecPoint(DecPointChars:String);
begin
SetDecPointChars(DecPointChars);
AutoSetDecPoint(FDecPointSet);
end; {~autosetdecpoint}

procedure toTNumParser.SetStatusProcedure(StatusProc:toExtMsgProc=nil);
begin
FStatusProc:= StatusProc;
end; {~setstatusprocedure}


function toTNumParser.Search(ASearchText    :String;
                             CaseInSensitive:Boolean=False;
                             FromCurrentLine:Boolean=False;
                             NotFoundMessage:Boolean=True): Boolean;
begin
Result:= Search(ASearchText,'',CaseInSensitive,FromCurrentLine,NotFoundMessage);
end; {~search}


{$push}{$warn 5092 off}
{12/10/2020 if not found, return to start line of search}
function toTNumParser.Search(ASearchText    :String;
                             OptionalText   :String;
                             CaseInSensitive:Boolean=False;
                             FromCurrentLine:Boolean=False;
                             NotFoundMessage:Boolean=True): Boolean;
var Found,Ok: Boolean;
    i,j     : Integer;
    Stg     : String;
begin
Found      := Length(ASearchtext)=0;
FLeft      := 0;
Ok         := True;
i          := 1;
FSearchText:= ASearchText;
if CaseInSensitive then
  begin
  ASearchText := AnsiLowerCase(ASearchText);
  OptionalText:= AnsiLowerCase(OptionalText);
  end;
repeat
  if FromCurrentLine then FromCurrentLine:= False
  else                    Ok             := NextLine;
  if Ok and (not Found) then
    begin
    Stg:= FLine;
    if CaseInsensitive then
      Stg:= AnsiLowerCase(Stg);
    i    := Pos(ASearchText,Stg);
    Found:= i>0;
    if Found and (Length(OptionalText)>0) then
      begin
      j:= Pos(ASearchText+OptionalText,Stg);
      if j=i then
        ASearchText:= ASearchText+OptionalText
      end;
    end;
until Found or (not Ok);
Ok:= Ok and Found;
if Ok then
  begin
  SetLineOk;
  FLeft        := i+Length(ASearchText);
  while (FLine[FLeft]=#32) and (FLeft<Length(FLine)) do
    Inc(FLeft);
  end
else
  begin
  if NotFoundMessage then
    ParseMessage(Format(toNumParser_notfound,[ASearchText,LastLineOk]));
  FLineNr:= FLastLineOkNr+1;
  NextLine;
  end;
Result:= Ok;
end; {~search}
{$pop}


{16/11/2020 set "top" at some line number}
procedure toTNumParser.SetTop(ALineNr:Integer);
begin
FTopLineNr:= Clip(ALineNr-1,0,FStrings.Count);
FLineNr   := Clip(FLineNr,FTopLineNr,FStrings.Count);
if FLastLineOkNr<>FLineNr then
  GotoTop;
end; {~settop}


{16/11/2020 set "top" at start of file}
procedure toTNumParser.ResetTop;
begin
SetTop(0);
end; {~resettop}


function toTNumParser.GotoTop(ReadFirstLine:Boolean=False;
                              Tab2Space    :Boolean=True): Boolean;
begin
FLineNr      := FTopLineNr;
FLine        := '';
FLastLineOk  := '';
FLastLineOkNr:= FTopLineNr;
Result       := (not ReadFirstLine) or NextLine(False,Tab2Space);
end; {~gototop}


function toTNumParser.GotoLeft: Boolean;
begin
FLeft := 0;
FRight:= 0;
Result:= FLineNr>0;
end; {~gotoleft}


{12/10/2020 set EOF also for FLineNr exceeding FStrings.Count although this should not happen}
function toTNumParser.GetEOFstatus: Boolean;
begin
Result:= FLineNr>=FStrings.Count;
end; {~geteofstatus}


{09/10/2020 THelperString Replace}
function toTNumParser.NextLine(MoveLineOk:Boolean=False;
                               Tab2Space :Boolean=True): Boolean;
begin
FLineNr:= Max(0,FLineNr);                                                       //1-based; zero-based internal
Result := FLineNr<FStrings.Count;
if Result then
  begin
  if Tab2Space then FLine:= FStrings.Strings[FLineNr].Replace(chTab,chSpace)
  else              FLine:= FStrings.Strings[FLineNr];                          //tabs are converted to '#' for not understood reasons
  ParseMessage(FLine);
  Inc(FLineNr);
  FLeft := 0;
  FRight:= 0;
  if MoveLineOk or (FLastLineOkNr=0) then
    SetLineOk;
  end;
end; {~nextline}


{06/10/2020 fundamentals alternative}
function toTNumParser.GetRemainderOfLine: String;
var s: String;
begin
if FLeft>0 then s:= Copy(FLine,FLeft,Length(FLine)-FLeft+1)
else            s:= FLine;
Result:= s.Trim;
end; {~getremainderofline}


function toTNumParser.GetConversionResult: Boolean;
begin
if FConversionResult then
  SetLineOk;
Result:= FConversionResult;
end; {~getconversionresult}


{$IFDEF new_findnum}
{07/01/2018 completely rewritten}
{08/01/2018 set formal DecimalSeparator in interpreted string}
{28/04/2020 DefaultFormatSettings.DecimalSeparator}
function toTNumParser.FindNum(IntegersOnly:Boolean=False): Boolean;
var a_cnt,i,j,lastpos: Integer;
    Ok               : Boolean;
    a,s,SignSet      : toTCharSet;
    curr,next        : Char;
    Stg              : String;

  function Test(c:Char;
                t:toTCharSet): Boolean;
  begin
  Result:= c in t;
  end;

  procedure SetCurrNext(n:Integer);
  begin
  curr:= Stg[n];
  next:= Stg[n+1];
  end;

  function RunCharSet(p:Integer;
                      t:toTCharSet): Integer;
  var q: Integer;
  begin
  q:= p;
  while (Stg[p] in t) and (p<=lastpos) do Inc(p);
  SetCurrNext(p);
  Result:= p-q;
  if Result>0 then
    begin
    FRight           := q+Result+ifthen(Test(curr,t),0,-1);
    FConversionString:= Copy(Stg,q,Result);
    end;
  end;

  function RunDigits(p:Integer): Integer;
  begin
  Result:= RunCharSet(p,FNumberSet);
  end;

begin
//IntegersOnly:= False;
//FLine:= '-1.1^-4';
Stg    := FLine+#0;
lastpos:= Length(Stg);
i      := 0;
a_cnt  := Length(FAliasValues);
a      := [];
while (i<a_cnt) do
  begin
  a:= a+[FAliasValues[i].toAlias];
  Inc(i);
  end;
SignSet:= FNegSignSet + FPosSignSet;
if IntegersOnly then s:= []
else                 s:= FDecPointSet + FExponentSet;
s    := a + s + FNumberSet + SignSet;
Ok   := False;
FLeft:= Max(1,FLeft);
try
  while (not Ok) and (FLeft<lastpos) do
    begin
    while (FLeft<lastpos) and (not Test(Stg[FLeft],s)) do Inc(FLeft); {jump to first relevant char}
    FRight:= Max(FLeft,FRight);
    SetCurrNext(FLeft);
    if (a_cnt>0) and (Curr in a) then {alias found}
      begin
      Ok               := True;
      FRight           := FLeft;
      FConversionString:= FAliasValues[FLeft].toNum;
      end  {alias}
    else
      begin  {no alias}
      i:= 0;
      if Curr in SignSet then
        begin
        i:= RunCharSet(FLeft,SignSet); {check for more signs}
        if i>1 then
          begin
          Fleft:= FLeft+i-1;
          i    := 1;
          end;
        end;
      if IntegersOnly then
        Ok:= (RunDigits(FLeft+i)>0)
      else
        begin {floating point}
        if Test(curr,FDecPointSet) then                        {starts with dec point}
          begin
          Stg[FLeft]:= DefaultFormatSettings.DecimalSeparator; {set formal dec point symbol}
          if Test(next,FDecPointSet) then Fleft:= FLeft+1+1    {repeating dec point, discard all results, move FLeft}
          else                            Inc(i);
          Ok:= (RunDigits(FLeft+i)>0);
          end;
        if not Ok then
          begin
          Ok:= (RunCharSet(FLeft+i,FNumberSet+FDecPointSet)>0) and (CountChars(FConversionString,CharSetToString(FDecPointSet))<=1);
          if Ok then
            begin
            j:= FConversionString.IndexOfAny(CharSetToString(FDecPointSet).ToCharArray);                    //zero-based
            if j>=0 then
              Stg[FLeft+i+j]:= DefaultFormatSettings.DecimalSeparator;          //set formal dec point symbol
            end;
          end;
        if Ok and (curr in FExponentSet) and (FRight<lastpos) then
          begin
          i     := Succ(FRight); {points to exponent}
          FExp  := i;
          Stg[i]:= 'e';
          if Test(next,SignSet) then Inc(i);
          RunDigits(Succ(i)); {if no digits are found FRight stays in front of exponent}
          end;
        end; {floating point}
      if not Ok then
        Fleft:= Succ(FRight);
      end; {else}
    end; {while}
 except
  Ok:= False;
 end; {try}
if Ok then
  begin
  FLeftConv        := FLeft;
  FRightConv       := FRight;
  FConversionString:= Copy(Stg,FLeft,Succ(FRight-FLeft))
  end
else
  begin
  FLeft            := Min(Fleft,Length(FLine));
  FLeftConv        := FLeft;
  FRightConv       := FLeft;
  FConversionString:= '';
  end;
Result:= Ok;
end; {~findnum}

{$ELSE}

{11/06/2016 clear FConversionString on fail}
{20/03/2017 exception handling}
function toTNumParser.FindNum(IntegersOnly:Boolean=False): Boolean;
type phases=0..8;
var i,j        : Integer;
    Ok         : Boolean;
    a,s,SignSet: toTCharSet;
    curr,next  : Char;
    Stg        : String;
    phase      : phases;  {[-/+] [n] [.] [n] [e][-/+]n | alias }
                          {  1    2   3   4   5   6  7     8   }
    function Test(c:Char;
                  t:toTCharSet): Boolean;
    begin
    Result:= c in t;
    end;

    function RunDigits(p:Integer): Integer;
    var q: Integer;
    begin
    q:= p;
    while Test(Stg[p],FNumberSet) do Inc(p);
    Result:= p-q;
    end;

    procedure SetCurrNext(n:Integer);
    begin
    curr:= Stg[n];
    next:= Stg[n+1];
    Ok  := Ok and Test(curr,s+FExponentSet);
    end;

begin
Ok := False;
Stg:= FLine+#0;
i  := 0;
a  := [];
j:= Length(FAliasValues);
while (i<j) do
  begin
  a:= a+[FAliasValues[i].toAlias];
  Inc(i);
  end;
SignSet:= FNegSignSet + FPosSignSet;
if IntegersOnly then s:= []
else                 s:= FDecPointSet + FExponentSet;
s:= a + s + FNumberSet + SignSet;
i:= 0;
FLeft:= Max(0,Pred(FLeft));
try
  repeat
    if IntegersOnly then phase:= 6
    else                 phase:= 0;
    FExp:= 0;
    while (not Ok) and (FLeft<Length(Stg)) do
      begin
      Inc(FLeft);
      Ok:= Test(Stg[FLeft],s);
      end;
    if Ok then i:= FLeft;
    SetCurrNext(i);
    while Ok and (phase<7) do
      begin
      if Test(curr,a) then
        begin
        phase:= 8;
        repeat
          Dec(j);
         until curr=FAliasValues[j].toAlias;
        end;
      if Test(curr,SignSet) then
        begin
        if Test(curr,FNegSignSet) then stg[i]:= '-'
        else                           stg[i]:= '+';
        if phase in [0,5] then
          Inc(phase)
        else if phase<>6 then
          begin
          Ok:= False;
          Dec(i);
          end;
        if Test(next,FDecPointSet) then
          begin
          Inc(i);
          Stg[i]:= DecimalSeparator;
          phase := 2;
          SetCurrNext(i);
          end;
        end;
      if Test(curr,FDecPointSet) then
        begin
        Stg[i]:= DecimalSeparator;
        if (phase in [0,2]) and Test(next,FNumberSet) then Inc(phase)
        else begin  i:= -1;  Ok:= False;  end;
        end
      else if Test(curr,FExponentSet) then
        begin
        Stg[i]:= 'e';
        if (phase in [2,4]) and Test(next,SignSet + FNumberSet) then
           begin
           if phase=2 then phase:= 5
           else            Inc(phase);
           if Test(next,FNumberSet) then Inc(phase);
           end
        else begin  i:= -1;  Ok:= False;  end;
        FExp:= i;
        end;
      if Ok and (phase in [0..3,6,7]) then
        begin
        if (phase=6) and Test(curr,FNumberSet) then next:= curr;
        if Test(next,FNumberSet) then
          begin
          Inc(i,RunDigits(i+1));
          if phase=0 then phase:= 2
          else            phase:= Min(phase+1,7);
          end
        else
          if Test(next,SignSet) then
            begin
            FRight:= i+1;
            Phase := 7;
            end
        else
          if phase>0 then Ok:= False;
        end;
      Inc(i);
      SetCurrNext(i);
      end;
    FRight:= i-1;
    Ok:= (phase in [0,2,4,7,8]) and (FRight>=FLeft);
  until Ok or (FLeft=Length(Stg));
  Ok:= (FRight>Fleft) or (Stg[FLeft] in toTDefNumChars + a);
 except
  Ok   := False;
  phase:= 0;
 end;
if Ok then
  begin
  if phase=8 then FConversionString:= FAliasValues[j].toNum
  else            FConversionString:= CopyRange(Stg,FLeft,FRight);
  FLeftConv := FLeft;
  FRightConv:= FRight;
  end
else FConversionString:= '';
Result:= Ok;
end; {~findnum}
{$ENDIF}

function toTNumParser.CountNumbers(IntegersOnly:Boolean=False): Word;
begin
FLeft := 0;
Result:= 0;
while FindNum(IntegersOnly) do
  begin
  FLeft:= Succ(FRight);
  Inc(Result);
  end;
FLeft:= 0;
end; {~countnumbers}


{20/03/2017 range check}
function toTNumParser.NextFloat(MaxPositive    :Extended=1e20;
                                MinPositive    :Extended=0;
                                NegativeAllowed:Boolean=True   ): Extended;
{$IFDEF SMART_ROUNDING}
const SignificantDecimals=9;
var Scaling: Extended;
{$ENDIF}
begin
FConversionResult:= FindNum;
Result           := 0;
if FConversionResult then
  try
     FLeft            := Min(Length(FLine),Succ(FRight));
     Result           := StrToFloat(ConversionString);
    {$IFDEF SMART_ROUNDING}
     if (Result<>0) and (Round(Result)<>Result) then
       begin
       Scaling:= Power(10,SignificantDecimals-Ceil(Log10(Abs(Result))));
       Result := Round(Result*Scaling)/Scaling;
       end;
    {$ENDIF}
     FConversionResult:= ((Result>MinPositive) and (Result     <MaxPositive)) or
                         ( NegativeAllowed     and (Abs(Result)<MaxPositive));
  except
    FConversionResult:= False;
  end;
end; {~nextfloat}


{20/03/2017 range check}
function toTNumParser.NextInteger(MinLimit:Integer=-MaxInt;
                                  MaxLimit:Integer= MaxInt): Integer;
begin
FConversionResult:= FindNum(True);
Result           := 0;
if FConversionResult then
  try
     FLeft            := Succ(FRight);
     Result           := Round(StrToFloat(ConversionString));
     FConversionResult:= Inrange(Result,MinLimit,MaxLimit);
  except
    FConversionResult:= False;
  end;
end; {~nextinteger}


function toTNumParser.NextChar: Char;
begin
FLeft:= Max(1,FLeft);
if FLeft<=Length(FLine) then
  begin
  Result:= FLine[FLeft];
  Inc(FLeft);
  Inc(FRight);
  end
else
  Result:= #0;
end; {~nextchar}


{06/10/2020 fundamentals alternative}
function toTNumParser.NextString(StopChar:Char): String;
begin
if FLeft=0 then
  Inc(FLeft);                                                                   //FLeft is 1-based
FRight:= FLine.IndexOf(StopChar,FLeft);                                         //zero-based
Result:= FLine.SubString(Pred(FLeft),FRight-FLeft).Trim;                        //zero-based
FLeft := Succ(FRight);
end; {~nextstring}

{24/02/2015}
{06/10/2020 fundamentals alternative}
function toTNumParser.NextString(StopChars:toTCharSet): String;
begin
if FLeft=0 then
  Inc(FLeft);
FRight:= Succ(FLeft);
while (FRight<Length(FLine)) and (not (FLine[FRight] in StopChars)) do          //1-based
  Inc(FRight);
Result:= FLine.SubString(Pred(FLeft),FRight-FLeft).Trim;                        //zero-based
FLeft := FRight;
end; {~nextstring}


{06/10/2020 fundamentals alternative}
function toTNumParser.NextString(NumChar:Integer): String;
begin
if FLeft=0 then
  Inc(FLeft);                                                                   //FLeft is 1-based
NumChar:= Min(NumChar,Length(FLine)-FLeft+1);
Result := FLine.SubString(Pred(FLeft),NumChar).Trim;                            //zero-based
Inc(FLeft,NumChar);
end; {~nextstring}


{$push}{$warn 5057 off}
{09/10/2020 SoundEx test, works also for 'mei'/'may'/'mai'}
function toTNumParser.NextMonth: Integer;
var i,k: Integer;

    procedure FindMatch(var APos,AMonth:Integer);
    var p,n,r: Integer;
        s,c  : String;
    begin
    s     := FLine.ToLower;
    AMonth:= -1;
    APos  := Length(s)-3;
    n     := Max(0,Pred(FLeft));
    for p:= 1 to toDefNumMonths do
      begin
      c:= Months[p].ToLower.Chars[0];
      r:= s.IndexOf(c,n);                                                       //zero-based
      if InRange(r,0,APos) then
        begin
        if SoundEx(Months[p].ToLower)=SoundEx(s.SubString(r,Length(Months[p]))) then
          begin
          AMonth:= p;
          APos  := r;
          end;
        end;
      end;
    end;

begin
FindMatch(k,i);                                                                 //initialises i,k
FConversionResult:= i>0;
if FConversionResult then
  begin
  FLeft := k;
  FRight:= k+Length(Months[i]);
  end
else
  i:= NextInteger;
Result:= i;
end; {~nextmonth}
{$pop}

{$push}{$warn 5057 off}
{03/06/2020 process only dmy results when >0}
function toTNumParser.NextDate(DateOrder:toMDYhmsOrder='dmyHMS';
                               AM_order :toMDYhmsOrder='mdyHMS'): TDateTime;
var i,j,n,tmpL,tmpR: Word;
    av             : array[1..7] of Integer;
    ab             : array[1..7] of Boolean;
    b              : Boolean;
    tmpNeg         : toTCharSet;
    s              : String;
begin
tmpNeg     := FNegSignSet;
FNegSignSet:= [];
b          := True;
s          := LowerCase(RemainderOfLine);
FillChar(av,SizeOf(av),0);
FillChar(ab,SizeOf(ab),0);
i          := Pos(' am',s);
j          := Pos(' pm',s);
if i+j>0 then
  begin
  s:= AM_order;
  if j>0 then av[7]:= 12;
  end
else s:= DateOrder;
for i:= 1 to length(DateOrder) do
  begin
  j:= Pos(s[i],'dmyHMS');                                                       //1-based
  b:= b and (j>0);
  if b then
    begin
    tmpL:= FLeft;
    n   := abs(NextInteger);
    tmpR:= FRight;
    if s[i]='m' then
      begin
      FLeft:= tmpL;
      av[j]:= abs(NextMonth);
      if FRight>tmpR then                                                       //found at different position
        FLeft:= tmpL;
      end
    else
      av[j]:= n;
    b    := b and FConversionResult;
    ab[j]:= b;
    end;
  end;
if b then
  try
    if ((av[1]<1) and ab[1]) or ((av[2]<1) and ab[2]) then
      Result:= 0
    else
      begin
      if ab[3] then
        begin
        if av[3]<  70 then Inc(av[3], 100);
        if av[3]<1900 then Inc(av[3],1900);
        end;
      b:= TryEncodeDate(av[3],av[2],av[1],Result);
      end;
    if b then Result:= Result+(av[4]+av[7]+(av[5]+av[6]/60)/60)/24
    else      Result:= 0;
   except
    Result:= Now;
    b     := False;
   end
else
  Result:= Now;
FNegSignSet      := tmpNeg;
FConversionResult:= b;
end; {~nextdate}
{$pop}

function toTNumParser.GetString(Index:Integer):String;
begin
if Index in [0..Pred(LineCount)] then
  begin
  if FileInMemory then
    Result:= FStrings[Index]
  else
    begin
    Inc(Index);
    while FLineNr<Index do
      NextLine;
    Result:= CurrentLine;
    end;
  end
else Result:= '';
end; {~getstring}


{17/01/2018}
function toTNumParser.GetDelimiter: Char;
begin
Result:= FStrings.Delimiter;
end; {~getdelimter}


{17/01/2018}
function toTNumParser.DelimiterCountCheck: Boolean;
begin
Result:= Succ(DelimiterCount)=LineCount;
end; {~delimitercountcheck}


destructor toTNumParser.Destroy;
begin
SetStatusProcedure(nil);
Setlength(FAliasValues,0);
FStrings.Free;
inherited;
end; {~destroy}


function SelfTest: Boolean;
var Parser: toTNumParser;
begin
Parser:= toTNumParser.Create;
with Parser do
  begin
  CurrentLine:= ' 1.0 -2 -4.321e-1- 04feb2011 06:00 PM';
  Result:= (NextFloat=1) and (NextInteger=-2) and (NextFloat=-0.4321) and
           (NextDate('dmyHM','dmyHM')=(EncodeDate(2011,2,4))+0.75);
  Free;
  end;
end; {selftest}

end.

