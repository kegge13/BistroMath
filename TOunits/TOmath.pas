unit TOmath;   {© Theo van Soest, 20/12/2015-20/06/2022  FPC 3.2.0: 09/02/2021}
{$MODE DELPHI}
{$I TOmath_opt.inc}
interface      {09/02/2021}

{$IFDEF uTExtendedX87}
uses uTExtendedX87,
{$ENDIF}

type
  MathExt     = {$IFDEF uTExtendedX87} TExtendedX87 {$ELSE} Extended {$ENDIF};
  NumericType = (nByte,nShortInt,                                    { 8 bits unsigned/signed}
                 nWord,nSmallInt,                                    {16 bits unsigned/signed}
                 nCardinal,nLongWord,nInteger,nLongInt,              {32 bits unsigned/signed}
                 nInt64,                                             {64 bits signed}
                 nSingle,                                            { 4 bytes}
                 nReal48,                                            { 6 bytes}
                 nReal,nDouble,nCurrency,nComp,                      { 8 bytes}
                 nExtended);                                         {10 bytes}
 AdressRec    = record case Boolean of
                 False:(Pt:Pointer);
                 True :(Lw:LongWord);
                end;

 Dividers     = 2..9;
 DividerSet   = set of Dividers;
 StatsDataType= Double;
 TStatsBuffer = array of StatsDataType;
 TStatsSampler= class(TObject)
  private
    FS_x           : array[1..2] of StatsDataType;
    FSNx           : Integer;
    FSmin,FSmax    : StatsDataType;
    FShi,FSlo,FSout: Integer;
    FSXbuffer      : TStatsBuffer;
    FSNbuffer      : Integer;
   function    Get_Mean           : StatsDataType;
   function    Get_SumSquares_Mean: StatsDataType;
   function    Get_VarianceEst    : StatsDataType;
   function    Get_Variance       : StatsDataType;
   function    Get_StdEst         : StatsDataType;
   function    Get_StdDev         : StatsDataType;
   function    Get_CL             : StatsDataType;
   function    Get_Median         : StatsDataType;
   function    Get_ReducedRange   : StatsDataType;
   function    Get_Outlier        : StatsDataType;
   procedure   SetIndex;
  public
   constructor Create(MinMaxBufferSize:Integer=0);
   procedure   Initialize;                              virtual;
   procedure   Add_X(X:StatsDataType);                  virtual;
   procedure   Del_X(X:StatsDataType);                  virtual;
   function    InDataRange(X      :StatsDataType;
                           Limited:Boolean=True): Boolean;
   destructor  Destroy;                                 override;
   property Samples        :Integer       read FSNx;
   property Mean           :StatsDataType read Get_Mean;
   property SumSquaresMean :StatsDataType read Get_SumSquares_Mean;
   property VarianceEst    :StatsDataType read Get_VarianceEst;
   property Variance       :StatsDataType read Get_Variance;
   property StdDevEst      :StatsDataType read Get_StdEst;
   property StdDev         :StatsDataType read Get_StdDev;
   property Median         :StatsDataType read Get_Median;
   property NoOutlier_range:StatsDataType read Get_ReducedRange;
   property Outlier        :StatsDataType read Get_Outlier;
   property ConfidenceLimit:StatsDataType read Get_CL;
   property Xlow           :StatsDataType read FSmin;
   property Xhigh          :StatsDataType read FSmax;
   property IndexRangeHigh :Integer       read FShi;
   property IndexRangeLow  :Integer       read FSlo;
  end;

 THistogramSampler= class(TStatsSampler)
  private
    FNumBins     : Cardinal;
    FBins        : array of Integer;
    FBinSize     : StatsDataType;
    FBinLo,FBinHi: StatsDataType;
    FLargestBin  : Cardinal;
    FChanged     : Boolean;
   function  GetLargestBin                       : Cardinal;
   function  GetLargestBinPos                    : StatsDataType;
   function  GetBin        (ABin  :Cardinal     ): Integer;
   function  GetBinFraction(ABin  :Cardinal     ): StatsDataType;
   function  GetCountAbove (ABin  :Cardinal     ): Cardinal;
   function  GetCountBelow (ABin  :Cardinal     ): Cardinal;
   function  GetBinPos     (ABin  :Cardinal     ): StatsDataType;
   procedure SetLow        (ALevel:StatsDataType);
   procedure SetHigh       (ALevel:StatsDataType);
  public
   constructor Create(LowestLevel,HighestLevel:StatsDataType;
                      ANumBins                :Cardinal=0;
                      ExpectedSamples         :Integer =0);
   procedure   Initialize;                                     override;
   procedure   SetNumBins(ANumBins:Cardinal);
   procedure   Add_X(X:StatsDataType);                         override;
   procedure   Del_X(X:StatsDataType);                         override;
   function    CalcBinNumber(X:StatsDataType): Cardinal;
   destructor  Destroy;                                        override;
   property Samples;
   property Mean;
   property SumSquaresMean;
   property VarianceEst;
   property Variance;
   property StdDevEst;
   property StdDev;
   property ConfidenceLimit;
   property Xlow;
   property Xhigh;
   property BinCounts[Index:Cardinal]  : Integer       read GetBin;
   property BinFraction[Index:Cardinal]: StatsDataType read GetBinFraction;
   property NumBins                    : Cardinal      read FNumBins    write SetNumBins;
   property BinSize                    : StatsDataType read FBinSize;
   property LargestBin                 : Cardinal      read GetLargestBin;
   property LargestBinValue            : StatsDataType read GetLargestBinPos;
   property BinRangeLow                : StatsDataType read FBinLo      write SetLow;
   property BinRangeHigh               : StatsDataType read FBinHi      write SetHigh;
   property CountAbove[Index:Cardinal] : Cardinal      read GetCountAbove;
   property CountBelow[Index:Cardinal] : Cardinal      read GetCountBelow;
  end;


  {20/12/2015}
  {27/08/2020 added Valid}
  TQuadFitReport=record
                  Valid          : Boolean;
                  Xtop,Ytop      : Extended;
                  Xmin,Xmax      : Extended;
                  Ymin,Ymax      : Extended;
                  Xavg,Yavg      : Extended;
                  Qofs,Qlin,Qquad: Extended;
                  Points         : Integer;
                 end;
 {21/05/2020 internal data support uTExtendedX87}
 TQuadFit  = class(TObject)
  private
    FXbuffer,FYbuffer  : TStatsBuffer;
    FS_x               : array[1..4] of MathExt;
    FS_xy              : array[0..2] of MathExt;
    FLZ,FLL,
    FQZ,FQL,FQQ,
    FMinX,FMaxX,
    FMinY,FMaxY        : MathExt;
    FNx                : Integer;
    FNbuffer           : Integer;
    FCalc              : Boolean;
    FValid,FMinMaxValid: Boolean;
   function    Calculate   : Boolean;
   function    Get_TopX    : Extended;
   function    Get_TopY    : Extended;
   function    Get_LZero   : Extended;
   function    Get_LLin    : Extended;
   function    Get_Zero    : Extended;
   function    Get_Lin     : Extended;
   function    Get_Quad    : Extended;
   function    Get_AverageX: Extended;
   function    Get_AverageY: Extended;
   function    Get_RangeX  : Extended;
   function    Get_RangeY  : Extended;
   function    Get_Report  : TQuadFitReport;
  public
   constructor Create(MinMaxBufferSize:Integer=0);
   procedure   Initialize;
   procedure   Add_XY(X,Y      :Extended     );
   procedure   Del_XY(X,Y      :Extended     );
   function    FitLin(X        :Extended     ): Extended;
   function    FitQuad(X       :Extended;
                       FallBack:Boolean=False): Extended;
   destructor  Destroy;                                    override;
   property TopX     :Extended       read Get_TopX;
   property TopY     :Extended       read Get_TopY;
   property Offset   :Extended       read Get_Zero;
   property Linear   :Extended       read Get_Lin;
   property Quadratic:Extended       read Get_Quad;
   property LinOffset:Extended       read Get_LZero;
   property LinLinear:Extended       read Get_LLin;
   property RangeX   :Extended       read Get_RangeX;
   property RangeY   :Extended       read Get_RangeY;
   property Nx       :Integer        read FNx;
   property AverageX :Extended       read Get_AverageX;
   property AverageY :Extended       read Get_AverageY;
   property FitValid :Boolean        read Calculate;
   property Report   :TQuadFitReport read Get_Report;
  end;


 {21/05/2020 internal data support uTExtendedX87}
 TLinFit   = class(TObject)
  private
    FS_x      : array[1..2] of MathExt;
    FS_y,FS_xy: MathExt;
    FZ,FL     : MathExt;
    FNx       : Integer;
    FCalc     : Boolean;
   function    Calculate   : Boolean;
   function    Get_AverageX: Extended;
   function    Get_AverageY: Extended;
   function    Get_Zero    : Extended;
   function    Get_Lin     : Extended;
  public
   constructor Create;
   procedure   Initialize;
   procedure   Add_XY(X,Y:Extended);
   procedure   Del_XY(X,Y:Extended);
   function    Fit(X:Extended): Extended;
   property Offset   :Extended read Get_Zero;
   property Linear   :Extended read Get_Lin;
   property AverageX :Extended read Get_AverageX;
   property AverageY :Extended read Get_AverageY;
   property Nx       :Integer  read FNx;
   property FitValid :Boolean  read FCalc;
  end;

//--------------------------------------------------------

function  RoundDeci(const Value   :Extended;
                    const Decimals:Integer                ): Extended;
function  Clip(const Value    :Extended;
               const Low,High :Extended                   ): Extended;     overload; {$IFDEF UseInline}inline;{$ENDIF}
function  Clip(const Value    :LongInt;
               const Low,High :LongInt                    ): LongInt;      overload; {$IFDEF UseInline}inline;{$ENDIF}
function  Clip(const Value    :Int64;
               const Low,High :Int64                      ): Int64;        overload; {$IFDEF UseInline}inline;{$ENDIF}
function  ClipByte(const Value:LongInt                    ): LongInt;      overload; {$IFDEF UseInline}inline;{$ENDIF}
function  ClipByte(const Value:Int64                      ): Int64;        overload; {$IFDEF UseInline}inline;{$ENDIF}
function  ClipWord(const Value:LongInt                    ): LongInt;      overload; {$IFDEF UseInline}inline;{$ENDIF}
function  ClipWord(const Value:Int64                      ): Int64;        overload; {$IFDEF UseInline}inline;{$ENDIF}
function  ClipLongWord(const Value:Int64                  ): LongWord;               {$IFDEF UseInline}inline;{$ENDIF}
function  CenteredRandom(Range:Double                     ): Double;
function  VectorLength(const X ,Y             :Extended   ): Extended;            overload;
function  VectorLength(const X ,Y ,Z          :Extended   ): Extended;            overload;
function  VectorLength(const X1,Y1,   X2,Y2   :Extended   ): Extended;            overload;
function  VectorLength(const X1,Y1,Z1,X2,Y2,Z2:Extended   ): Extended;            overload;

procedure Vector2Cartesian(const VectorLength,Angle:Extended;
                           var X,Y                 :Extended);                overload;
procedure Vector2Cartesian(const VectorLength,Angle:Extended;
                           var X,Y                 :Integer);                 overload;
(*function  MakeRGBcolor(CircleDegrees:Word;
                       Radius       :Byte): TColor;  *)

procedure QuadFit(var XYset;                   {zero based counting}
                  XYtype      :NumericType;
                  var Z,L,Q   :Extended;       {y=Z+L.x+Q.x.x}
                  Num_El      :LongWord;
                  C1          :Extended=0;     {1-dim: x:= c1+c2*0..numel-1}
                  C2          :Extended=1;     {2-dim: x-c1, y-c2 }
                  OneDimension:Boolean =True;
                  Step_El     :Word    =1);

function Det2(const a,b,
                    c,d:MathExt): MathExt;

function Det3(const a,b,c,
                    d,e,f,
                    g,h,i:MathExt): MathExt;

function NiceStep(Value         :Extended;
                  DecadeDividers:DividerSet=[2,5];
                  SmallerValueOk:Boolean=True;
                  LargerValueOk :Boolean=True): Extended;

function LineDistance2Origin(Gain,Offset:Extended): Extended;                 overload;
function LineDistance2Origin(X1,Y1,X2,Y2:Extended): Extended;                 overload;

function IsPrime(ANumber:Int64): Boolean;

{$IFDEF SelfTest}
function SelfTest:Boolean;
{$ENDIF}


implementation

uses Math, SysUtils;

const
  tomath_OutlierRatio:Extended=         3;
  MaxByte                     =       $ff;
  MaxWord                     =     $ffff;
  MaxLongWord                 = $ffffffff;
  StatsNoresult               =  1.1e4000;  {check with type of StatsDataType}


{$IFDEF SMART_ROUNDING}
function SmartRound(AValue:MathExt): MathExt;
const SignificantDecimals=9;
var   Scaling: MathExt;
begin
if (AValue<>0) and (Round(AValue)<>AValue) then
  begin
  Scaling:= Power(10,SignificantDecimals-Ceil(Log10(Abs(AValue))));
  AValue := Round(AValue*Scaling)/Scaling;
  end;
Result:= AValue;
end;
{$ENDIF}


//-----tstatsampler---------------------------------------------------------

constructor TStatsSampler.Create(MinMaxBufferSize:Integer=0);
begin
inherited Create;
Initialize;
FSNbuffer:= System.Abs(MinMaxBufferSize);
end; {~create}


{30/10/2019 initialisation of FSXbuffer needed}
procedure TStatsSampler.Initialize;
begin
FillChar(FS_x ,SizeOf(FS_x) ,0);
FSNx :=  0;
FShi :=  0;
FSlo :=  0;
FSout:= -1;
FSmin:= MaxDouble;
FSmax:= MinDouble;
Setlength(FSXbuffer,0);
end; {~initialize}


procedure TStatsSampler.Add_X(X:StatsDataType);
var Term: StatsDataType;
    i,j : Integer;
begin
Term:= 1;
try
  if FSNbuffer>0 then {keep sorted}
    begin
    FSout:= -1;
    i    := Pred(Length(FSXbuffer));
    j    := 0;
    while (j<=i) and (FSXbuffer[j]<X) do Inc(j); {fxbuffer[j]>x or j=i}
    if Length(FSXbuffer)<FSNbuffer then {enlarge buffer when allowed}
      begin
      SetLength(FSXbuffer,Succ(Length(FSXbuffer)));
      Inc(i);
      end;
    while i>j do {move out, starting at j, move (previous) upper element out anyway}
      begin
      FSXbuffer[i]:= FSXbuffer[Pred(i)];
      Dec(i);
      end;
    FSXbuffer[j]:= X;
    end; {fnbuffer>0}
  Inc(FSNx);
  for i:= 1 to 2 do
    begin
    Term   := Term*X;
    FS_x[i]:= FS_x[i]+Term;
    end;
  if FSNx=1 then
    begin
    FSmin:= X;  FSmax:= X;
    end
  else if   X>FSMax then FSmax:= X
    else if X<FSmin then FSmin:= X;
 except
  on EMathError do Initialize;
 end;
SetIndex;
end; {~add_x}


procedure TStatsSampler.Del_X(X:StatsDataType);
var Term: StatsDataType;
    i,j : Integer;
begin
Term:= 1;
try
  if FSNbuffer>0 then {keep order}
    begin
    FSout:= -1;
    i   := Pred(Length(FSXbuffer));
    j   := 0;
    while (j<i) and (FSXbuffer[j]<>X) do Inc(j); {find element with value X (if any)}
    if FSXbuffer[j]=X then {elemnt found}
      begin
      while j<i do {move following elements one down}
        begin
        FSXbuffer[j]:= FSXbuffer[Succ(j)];
        Inc(j);
        end;
      SetLength(FSXbuffer,i); {remove last element}
      FSmin:= FSXbuffer[0];
      FSmax:= FSXbuffer[Pred(i)];
      end;
    end; {fnbuffer>0}
  Dec(FSNx);
  for i:= 1 to 2 do
    begin
    Term   := Term*X;
    FS_x[i]:= FS_x[i]-Term;
    end;
 except
  on EMathError do Initialize;
 end;
SetIndex;
end; {~del_x}


procedure TStatsSampler.SetIndex;
begin
Fslo:= 0;
if Samples<=FSNbuffer then FShi:= Pred(Length(FSXbuffer))
else                       FShi:= 0;
end; {~setindex}


function TStatsSampler.Get_Median: StatsDataType;
var i: Integer;
begin
i:= Length(FSXbuffer);
if i>0 then
  begin
  Result:= FSXbuffer[i div 2];
  if (not Odd(i)) then Result:= (FSXbuffer[Pred(i div 2)]+Result)/2;
  end
else Result:= 0;
end; {~get_median}


{$push}{$warn 5091 off: setps not initialised}
function TStatsSampler.Get_ReducedRange: StatsDataType;
var i,eStepIndex : Integer;
    d,mStep,eStep: StatsDataType;
    Steps        : TStatsBuffer;
begin
FSlo:= 0;
FShi:= Pred(Length(FSXbuffer));
if Fshi>2 then
  begin
  SetLength(Steps,FShi); {calculate difference between each adjacent pair}
  for i:= 0 to Pred(FShi) do
    Steps[i]:= FSXbuffer[Succ(i)]-FSXbuffer[i];
  i:= FShi div 2;
  if Odd(FShi) then mStep:= (Steps[i]+Steps[Succ(i)])/2 {median}
  else              mStep:=  Steps[i];
  if mStep=0 then mStep:= 1;
  eStep     := 0;
  eStepIndex:= 0;
  for i:= 0 to Pred(FShi) do {find extreme large or small step}
    begin
    d:= Steps[i]/mStep;
    if (d>0) and (d<1) then d:= 1/d;
    if d>eStep then
      begin
      eStep     := d;
      eStepIndex:= i;
      end;
    end;
  if eStep>tomath_OutlierRatio then
    begin
    eStep:= Steps[eStepIndex];
    if      FSXbuffer[1   ]-FSXbuffer[0         ]=eStep then begin  FSout:=    0;  Inc(FSlo);  end
    else if FSXbuffer[FShi]-FSXbuffer[Pred(FShi)]=eStep then begin  FSout:= FShi;  Dec(FShi);  end;
    end;
  Finalize(Steps);
  Result:= FSXbuffer[FShi]-FSXbuffer[FSlo];
  end
else
  Result:= 0;
end; {~get_reducedrange}
{$pop}


function TStatsSampler.Get_Outlier: StatsDataType;
begin
if FSout=-1 then Result:= StatsNoresult
else             Result:= FSXbuffer[FSout];
end; {~get_outlier}


function TStatsSampler.InDataRange(X      :StatsDataType;
                                   Limited:Boolean=True): Boolean;
var r: StatsDataType;
begin
if Samples>1 then
  begin
  if Limited then
    begin
    r     := Succ(Samples)*Get_ReducedRange/(2*Samples);
    Result:= InRange(X,Median-r,Median+r);
    end
  else
    Result:= InRange(X,FSmin,FSmax)
  end
else
  Result:= False;
end; {~inrange}


function TStatsSampler.Get_Mean: StatsDataType;
begin
if Samples>0 then Result:= FS_x[1]/Samples
else              Result:= 0;
end; {~get_mean}


{18/06/2022 protect against negative calculation value due to equal data input}
function TStatsSampler.Get_SumSquares_Mean: StatsDataType;
begin
if Samples>0 then Result:= Max(0,FS_x[2]-Sqr(FS_x[1])/Samples)
else              Result:= 0;
end; {~get_sumsquares_mean}


{20/06/2022 threshold as extra condition}
function TStatsSampler.Get_VarianceEst: StatsDataType;
begin
if (Samples>1) and (FSmax>FSmin) then Result:= SumSquaresMean/Pred(Samples)
else                                  Result:= 0;
end; {~get_varianceest}


function TStatsSampler.Get_StdEst: StatsDataType;
begin
Result:= SqRt(VarianceEst);
end; {~get_stdest}


function TStatsSampler.Get_Variance: StatsDataType;
begin
if Samples>1 then Result:= SumSquaresMean/Samples
else              Result:= 0;
end; {~get_variance}


function TStatsSampler.Get_StdDev: StatsDataType;
begin
Result:= SqRt(Variance);
end; {~get_stddev}


function TStatsSampler.Get_CL: StatsDataType;
begin
Result:= abs(Mean)+1.5*StdDevEst;
end; {~get_cl}


destructor TStatsSampler.Destroy;
begin
Finalize(FSXbuffer);
Inherited;
end; {~destroy}


//-----thistogramsampler---------------------------------------------------------


{29/10/2019 inherited create needs data}
{29/07/2020 safety catch}
constructor THistogramSampler.Create(LowestLevel,HighestLevel:StatsDataType;
                                     ANumBins                :Cardinal=0;
                                     ExpectedSamples         :Integer=0);
begin
if HighestLevel-LowestLevel<1e-8 then
  HighestLevel:= HighestLevel+1;
FBinHi:= HighestLevel;
SetLow(LowestLevel);
SetHigh(HighestLevel);
NumBins:= Max(3,ifthen(ANumBins>0,ANumBins,Round(SqRt(ExpectedSamples))));
inherited Create(ExpectedSamples);                                              //calls initialize
end; {~create}


procedure THistogramSampler.Initialize;
var i: Cardinal;
begin
if Samples>0 then
  begin
  Inherited;
  for i:= 0 to Pred(FNumBins) do FBins[i]:= 0;
  end;
if FNumBins>0 then
  FBinSize:= (FBinHi-FBinLo)/FNumBins;
FLargestBin:= 0;
FChanged   := False;
end; {~initialize}


procedure THistogramSampler.SetLow(ALevel:StatsDataType);
begin
FBinLo:= Math.Min(ALevel,FBinHi-1e-10);
Initialize;
end; {~setlow}


procedure THistogramSampler.SetHigh(ALevel:StatsDataType);
begin
FBinHi:= Math.Max(FBinLo+1e-10,ALevel);
Initialize;
end; {~sethigh}


procedure THistogramSampler.SetNumBins(ANumBins:Cardinal);
begin
FNumBins:= Max(1,ANumBins);
SetLength(FBins,FNumBins);
Initialize;
end; {~setnumbins}


function THistogramSampler.CalcBinNumber(X:StatsDataType): Cardinal;
begin
Result:= Min(Max(0,Trunc((X-FBinLo)/FBinSize)),Pred(FNumBins));
end; {~calcbinnumber}


function THistogramSampler.GetBinPos(ABin:Cardinal): StatsDataType;
begin
Result:= FBinLo+FBinSize*(0.5+Max(0,Min(ABin,Pred(FNumBins))));
end; {~getbinpos}


function THistogramSampler.GetBin(ABin:Cardinal): Integer;
begin
if InRange(ABin,0,Pred(FNumBins)) then Result:= FBins[ABin]
else                                   Result:= 0;
end; {~getbin}


function THistogramSampler.GetBinFraction(ABin:Cardinal): StatsDataType;
begin
Result:= GetBin(ABin)/Samples;
end; {~getbinfraction}


function THistogramSampler.GetLargestBinPos: StatsDataType;
begin
Result:= GetBinPos(GetLargestBin);
end; {~getlargestbinpos}


{04/09/2020 more efficient implementation, using FChanged}
function THistogramSampler.GetLargestBin: Cardinal;
var i,j: Cardinal;
begin
if FChanged then
  begin
  i:= 0;
  for j:= 0 to Pred(FNumBins) do
    if FBins[j]>FBins[i] then
      i:= j;
  Result:= i;
  end
else
  Result:= FLargestBin;
end; {~getlargestbin}


function THistogramSampler.GetCountAbove(ABin:Cardinal): Cardinal;
var n: Cardinal;
begin
Result:= 0;
n     := Pred(FNumBins);
while (ABin<n) do
  begin
  Inc(ABin);
  Inc(Result,FBins[ABin]);
  end;
end; {~getcountabove}


function THistogramSampler.GetCountBelow(ABin:Cardinal): Cardinal;
begin
Result:= 0;
ABin  := Min(ABin,FNumBins-1);
while (ABin>0) do
  begin
  Dec(ABin);
  Inc(Result,FBins[ABin]);
  end;
end; {~getcountbelow}


procedure THistogramSampler.Add_X(X:StatsDataType);
begin
Inherited;
Inc(FBins[CalcBinNumber(X)]);
FChanged:= True;
end; {~add_x}


procedure THistogramSampler.Del_X(X:StatsDataType);
begin
Inherited;
Dec(FBins[CalcBinNumber(X)]);
FChanged:= True;
end; {~del_x}


destructor THistogramSampler.Destroy;
begin
Finalize(FBins);
Inherited;
end; {~destroy}


//----TQuadFit--------------------------

constructor TQuadFit.Create(MinMaxBufferSize:Integer=0);
begin
inherited Create;
Initialize;
FNbuffer:= System.Abs(MinMaxBufferSize);
end; {~create}


procedure TQuadFit.Initialize;
begin
Finalize(FXbuffer);
Finalize(FYbuffer);
FillChar(FS_x ,SizeOf(FS_x) ,0);
FillChar(FS_xy,SizeOf(FS_xy),0);
FNx         := 0;
FCalc       := False;
FValid      := True;
FMinMaxValid:= False;
end; {~initialize}


{21/05/2020 internal data support uTExtendedX87}
procedure TQuadFit.Add_XY(X,Y:Extended);
var Term: MathExt;
    i,j : word;
   {$IFDEF SMART_ROUNDING}
    _X,_Y: MathExt;
   {$ENDIF}
begin
try
  FMinMaxValid:= True;
  if FNx=0 then
    begin
    FMinX:= X;  FMaxX:= X;
    FMinY:= Y;  FMaxY:= Y;
    end
  else
    begin
    FMinX:= Math.Min(FMinX,X);  FMaxX:= Math.Max(FMaxX,X);
    FMinY:= Math.Min(FMinY,Y);  FMaxY:= Math.Max(FMaxY,Y);
    end;
  if FNbuffer>0 then
    begin
    i:= Length(FXbuffer);
    if i=FNbuffer then
      begin
      Dec(i);
      FXbuffer:= copy(FXbuffer,1,i);
      FYbuffer:= copy(FYbuffer,1,i);
      end;
    j:= Succ(i);
    SetLength(FXbuffer,j);  FXbuffer[i]:= X;
    SetLength(FYbuffer,j);  FYbuffer[i]:= Y;
    end; {fnbuffer>0}
  Inc(FNx);
  Term:= 1;
 {$IFDEF SMART_ROUNDING}
  _X:= SmartRound(X);
  _Y:= SmartRound(Y);
  for i:= 1 to 4 do
    begin
    Term   := Term*_X;
    FS_x[i]:= FS_x[i]+Term;
    end;
  Term    := _Y;
  FS_xy[0]:= FS_xy[0]+Term;
  for i:= 1 to 2 do
    begin
    Term    := Term*_X;
    FS_xy[i]:= FS_xy[i]+Term;
    end;
 {$ELSE}
  for i:= 1 to 4 do
    begin
    Term   := Term*X;
    FS_x[i]:= FS_x[i]+Term;
    end;
  Term    := Y;
  FS_xy[0]:= FS_xy[0]+Term;
  for i:= 1 to 2 do
    begin
    Term    := Term*X;
    FS_xy[i]:= FS_xy[i]+Term;
    end;
 {$ENDIF}
 except
  on EMathError do Initialize;
 end;
FCalc := False;
FValid:= True;
end; {~add_xy}


{21/05/2020 internal data support uTExtendedX87}
procedure TQuadFit.Del_XY(X,Y:Extended);
var Term: MathExt;
    i,j : Integer;
   {$IFDEF SMART_ROUNDING}
    _X,_Y: MathExt;
   {$ENDIF}
begin
try
  if Length(FXbuffer)>0 then
    begin
    i:= 0;
    j:= Pred(Length(FXbuffer));
    while (i<j) and ((FXbuffer[i]<>X) or (FYbuffer[i]<>Y)) do Inc(i);
    while i<j do
      begin
      FXbuffer[i]:= FXbuffer[Succ(i)];
      FYbuffer[i]:= FYbuffer[Succ(i)];
      Inc(i);
      end;
    FMinX:= FXbuffer[0];  FMaxX:= FXbuffer[0];  SetLength(FXbuffer,j);
    FMinY:= FYbuffer[0];  FMaxY:= FYbuffer[0];  SetLength(FYbuffer,j);
    while j>0 do
      begin
      Dec(j);
      FMinX:= Math.Min(FXbuffer[j],FMinX);  FMaxX:= Math.Max(FXbuffer[j],FMaxX);
      FMinY:= Math.Min(FYbuffer[j],FMinY);  FMaxY:= Math.Max(FYbuffer[j],FMaxY);
      end;
    end {fnbuffer>0}
  else FMinMaxValid:= False;
  Dec(FNx);
  Term:= 1;
  {$IFDEF SMART_ROUNDING}
   _X:= SmartRound(X);
   _Y:= SmartRound(Y);
  for i:= 1 to 4 do
    begin
    Term   := Term*_X;
    FS_x[i]:= FS_x[i]-Term;
    end;
  Term    := _Y;
  FS_xy[0]:= FS_xy[0]-Term;
  for i:= 1 to 2 do
    begin
    Term    := Term*_X;
    FS_xy[i]:= FS_xy[i]-Term;
    end;
 {$ELSE}
  for i:= 1 to 4 do
    begin
    Term   := Term*X;
    FS_x[i]:= FS_x[i]-Term;
    end;
  Term    := Y;
  FS_xy[0]:= FS_xy[0]-Term;
  for i:= 1 to 2 do
    begin
    Term    := Term*X;
    FS_xy[i]:= FS_xy[i]-Term;
    end;
 {$ENDIF}
 except
  on EMathError do Initialize;
 end;
FCalc := False;
FValid:= True;
end; {~del_xy}


{21/05/2020 internal data support uTExtendedX87}
function TQuadFit.Calculate: Boolean;
var t: MathExt;
begin
if (not FCalc) and FValid then
  begin
  if FNx>1 then
    begin
    t     := FS_x[2] - FS_x[1]*FS_x[1]/FNx;
    FValid:= t<>0;
    if FValid then
      begin
      FLL:= (FS_xy[1] - FS_x[1]*FS_xy[0]/FNx) / t;
      FLZ:= (FS_xy[0] - FLL*FS_x[1])/FNx;
      if FNx>2 then
        begin
        t:= Det3(FNx,      FS_x[1],  FS_x[2],
                 FS_x[1],  FS_x[2],  FS_x[3],
                 FS_x[2],  FS_x[3],  FS_x[4]);
        FValid:= t<>0;
        if FValid then
          try
            FLL:= (FS_xy[1] - FS_x[1]*FS_xy[0]/FNx) / (FS_x[2] - FS_x[1]*FS_x[1]/FNx);
            FLZ:= (FS_xy[0] - FLL*FS_x[1])/FNx;
            FQZ:= Det3(FS_xy[0], FS_x[1],  FS_x[2],
                       FS_xy[1], FS_x[2],  FS_x[3],
                       FS_xy[2], FS_x[3],  FS_x[4])  / t;

            FQL:= Det3(FNx    ,  FS_xy[0], FS_x[2],
                       FS_x[1],  FS_xy[1], FS_x[3],
                       FS_x[2],  FS_xy[2], FS_x[4])  / t;

            FQQ:= Det3(FNx    ,  FS_x[1],  FS_xy[0],
                       FS_x[1],  FS_x[2],  FS_xy[1],
                       FS_x[2],  FS_x[3],  FS_xy[2]) / t;
          except
            on EMathError do begin  FLZ:= 0;  FLL:= 0;  FQZ:= 0;  FQL:= 0;  FQQ:= 0;  end;
          end; {try}
        end {fnx>2}
      else
        begin  {fnx=2}
        FQZ:= FLZ;  FQL:= FLL;  FQQ:= 0;
        end;
      end; {fvalid}
    FCalc:= FValid;
    end
  else {fNx<=1}
    begin
    FCalc:= True;
    FLZ  := FS_xy[0];  FLL:= 0;  FQZ:= FLZ;  FQL:= 0;  FQQ:= 0;
    end;
  end;
Result:= FCalc;
end; {~calculate}


function TQuadFit.Get_Zero: Extended;
begin
if Calculate then Result:= FQZ
else              Result:= 0;
end; {~get_zero}


function TQuadFit.Get_Lin: Extended;
begin
if Calculate then Result:= FQL
else              Result:= 0;
end; {~get_lin}


function TQuadFit.Get_Quad: Extended;
begin
if Calculate then Result:= FQQ
else              Result:= 0;
end; {~get_quad}


function TQuadFit.Get_LZero: Extended;
begin
if Calculate then Result:= FLZ
else              Result:= 0;
end; {~get_lzero}


function TQuadFit.Get_TopX: Extended;
begin
if Calculate and (FQQ<>0) then Result:= -FQL/(2*FQQ)
else                           Result:= 0;
end; {~get_topx}


function TQuadFit.Get_TopY: Extended;
begin
if Calculate and (FQQ<>0) then Result:= FitQuad(TopX)
else                           Result:= 0;
end; {~get_topy}


function TQuadFit.Get_LLin: Extended;
begin
if Calculate then Result:= FLL
else              Result:= 0;
end; {~get_llin}


function TQuadFit.Get_RangeX: Extended;
begin
if FNx=0 then Result:= 0
else          Result:= FMaxX-FMinX;
end; {~get_rangex}


function TQuadFit.Get_RangeY: Extended;
begin
if FNx=0 then Result:= 0
else          Result:= FMaxY-FMinY;
end; {~get_rangeY}


function TQuadFit.FitLin(X:Extended): Extended;
begin
Calculate;
{$IFDEF SMART_ROUNDING}
Result:= FLZ+SmartRound(X)*FLL;
{$ELSE}
Result:= FLZ+X*FLL;
{$ENDIF}
end; {~fitlin}


{21/05/2020 internal data support uTExtendedX87}
function TQuadFit.FitQuad(X       :Extended;
                          FallBack:Boolean=False): Extended;
var i,j      : Integer;
    Xtmp,Ytmp: MathExt;
    S        : TStatsSampler;
    L        : TLinFit;
   {$IFDEF SMART_ROUNDING}
    _X       : MathExt;
   {$ENDIF}

  function FindMin(excluded:integer=-1): integer;
  var i  : Integer;
      m,n: MathExt;
  begin
  m     := StatsNoresult;
  Result:= 0;
  for i:= 0 to Pred(Length(FXbuffer)) do
    if i<>excluded then
      begin
      n:= abs(X-FXbuffer[i]);
      if n<m then
        begin
        m     := n;
        Result:= i;
        end;
      end; {for}
  L.Add_XY(FXbuffer[Result],FYbuffer[Result]);
  end;  {findmin}

begin
Calculate;
{$IFDEF SMART_ROUNDING}
_X:= SmartRound(X);
Result:= FQZ+(FQL+FQQ*_X)*_X;
{$ELSE}
Result:= FQZ+(FQL+FQQ*X)*X;
{$ENDIF}
if FallBack and FMinMaxValid and (Length(FXbuffer)>0) and InRange(X,FMinX,FMaxX) then
  begin
  S:= TStatsSampler.Create(Length(FXbuffer));
  for i:= 0 to Pred(Length(FXbuffer)) do S.Add_X(FYbuffer[i]);
  Fallback:= (S.Samples>3) and ((not S.InDataRange(Result)) or (S.OutLier<>StatsNoresult));
  if Fallback then
    begin
    Ytmp:= S.Outlier;
    i   := Pred(Length(FXbuffer));
    j   := -1;
    if      FYbuffer[0]=Ytmp then j:= 0  {zoek outlier op: moet aan rand liggen}
    else if FYbuffer[i]=Ytmp then j:= i;
    if j>=0 then
      begin
      Xtmp:= FXbuffer[j];
      Del_XY(Xtmp,Ytmp);
      S.Del_X(Ytmp);
      Result  := Offset+(Linear+Quadratic*X)*X;
      Fallback:= not S.InDataRange(Result);
      if Fallback then
        begin
        L:= TLinFit.Create;
        FindMin(FindMin);
        Result:= L.Offset+L.Linear*X;
        L.Free;
        end;
      Add_XY(Xtmp,Ytmp);
      S.Add_X(Ytmp);
      end;
    end;  {fallback}
  S.Free;
  end;
end; {~fitquad}


function TQuadFit.Get_AverageX: Extended;
begin
if Calculate then Result:= FS_x[1]/FNx
else              Result:= 0;
end; {~get_averagey}


function TQuadFit.Get_AverageY: Extended;
begin
if Calculate then Result:= FS_xy[0]/FNx
else              Result:= 0;
end; {~get_averagey}


function TQuadFit.Get_Report: TQuadFitReport;
begin
with Result do
  begin
  Valid := FValid;
  Xtop  := TopX;
  Ytop  := TopY;
  Xmin  := FMinX;
  Ymin  := FMinY;
  Xmax  := FMaxX;
  Ymax  := FMaxY;
  Xavg  := AverageX;
  Yavg  := AverageY;
  Qofs  := Offset;
  Qlin  := Linear;
  Qquad := Quadratic;
  Points:= Nx;
  end;
end; {~get_report}


destructor TQuadFit.Destroy;
begin
Finalize(FXbuffer);
Finalize(FYbuffer);
Inherited;
end; {~destroy}


//------------------------------

constructor TLinFit.Create;
begin
inherited;
Initialize;
end; {~create}


{11/12/2015: added FS_y:= 0}
procedure TLinFit.Initialize;
begin
FillChar(FS_x ,SizeOf(FS_x) ,0);
FS_xy       := 0;
FS_y        := 0;
FNx         := 0;
FCalc       := False;
end; {~initialize}


{21/05/2020 internal data support uTExtendedX87}
procedure TLinFit.Add_XY(X,Y:Extended);
var tx: MathExt;
    i : Integer;
begin
tx:= 1;
for i:= 1 to 2 do
  begin
  tx     := tx*X;
  FS_x[i]:= FS_x[i]+tx;
  end;
FS_y := FS_y +Y;
FS_xy:= FS_xy+X*Y;
FCalc:= False;
Inc(FNx);
end; {~add_xy}


{21/05/2020 internal data support uTExtendedX87}
procedure TLinFit.Del_XY(X,Y:Extended);
var tx: MathExt;
    i : Integer;
begin
tx:= 1;
for i:= 1 to 2 do
  begin
  tx     := tx*X;
  FS_x[i]:= FS_x[i]-tx;
  end;
FS_y := FS_y -Y;
FS_xy:= FS_xy-X*Y;
FCalc:= False;
Dec(FNx);
end; {~del_xy}


{21/05/2020 internal data support uTExtendedX87}
function TLinFit.Calculate: Boolean;
var t: MathExt;
begin
if (not FCalc) and (FNx>0) then
  try
    if FNx>1 then
      begin
      t:= FS_x[2] - FS_x[1]*FS_x[1]/FNx;
      Fcalc:= t<>0;
      if FCalc then
        begin
        FL := (FS_xy - FS_x[1]*FS_y/FNx) / t;
        FZ := (FS_y - FL*FS_x[1])/FNx;
        end;
      end
    else
      begin
      FCalc:= True;
      FL   := 0;
      FZ   := FS_y;
      end;
  except
    on EZeroDivide do begin  FZ:= 0;  FL:= 0;  end;
  end;
Result:= FCalc;
end; {~calculate}


function TLinFit.Get_Zero: Extended;
begin
if Calculate then Result:= FZ
else              Result:= 0;
end; {~get_zero}


function TLinFit.Get_Lin: Extended;
begin
if Calculate then Result:= FL
else              Result:= 0;
end; {~get_lin}


function TLinFit.Get_AverageX: Extended;
begin
if Calculate then Result:= FS_x[1]/FNx
else              Result:= 0;
end; {~get_averagey}


function TLinFit.Get_AverageY: Extended;
begin
if Calculate then Result:= FS_y/FNx
else              Result:= 0;
end; {~get_averagey}


function TLinFit.Fit(X:Extended): Extended;
begin
Result:= Offset+Linear*X;
end; {~fit}


//-----procedures-and functions------------------------------------------------

function  RoundDeci(const Value   : Extended;
                    const Decimals: Integer): Extended;
var t: Extended;
begin
t     := Power(10,Decimals);
Result:= Round(Value*t)/t;
end;


function  Clip(const Value   :Extended;
               const Low,High:Extended): Extended;
begin
if      Value < Low  then Result:= Low
else if Value > High then Result:= High
else                      Result:= Value;
end;


function Clip(const Value   :LongInt;
              const Low,High:LongInt): LongInt;
begin
if      Value < Low  then Result:= Low
else if Value > High then Result:= High
else                      Result:= Value;
end;

function Clip(const Value   :Int64;
              const Low,High:Int64): Int64;
begin
if      Value < Low  then Result:= Low
else if Value > High then Result:= High
else                      Result:= Value;
end;


function ClipByte(const Value:LongInt): LongInt;
begin
if      Value < 0 then       Result:= 0
else if Value > MaxByte then Result:= MaxByte
else                         Result:= Value;
end;


function ClipByte(const Value:Int64): Int64;
begin
if      Value < 0       then Result:= 0
else if Value > MaxByte then Result:= MaxByte
else                         Result:= Value;
end;


function ClipWord(const Value:LongInt): LongInt;
begin
if      Value < 0       then Result:= 0
else if Value > MaxWord then Result:= MaxWord
else                         Result:= Value;
end;


function ClipWord(const Value: Int64): Int64;
begin
if      Value < 0       then Result:= 0
else if Value > MaxWord then Result:= MaxWord
else                         Result:= Value;
end;


function ClipLongWord(const Value: Int64): LongWord;
begin
if      Value < 0           then Result:= 0
else if Value > MaxLongWord then Result:= MaxLongWord
else                             Result:= LongWord(Value);
end;


function CenteredRandom(Range:Double): Double;
begin
Result:= (Random-0.5)*Range;
end; {centeredrandom}


// 2-by-2 determinant
function Det2(const a,b,
                    c,d:MathExt): MathExt;
begin
Result:= a*d - b*c
end; {det2}


function Det3(const a,b,c,
                    d,e,f,
                    g,h,i:MathExt): MathExt;
begin
Result:= a*Det2(e,f, h,i) - b*Det2(d,f, g,i) + c*Det2(d,e, g,h)
end; {det3}


{18/09/95}
function FetchValue(var XYset;           {zero-based counting}
                    XYtype      :NumericType;
                    X           :LongWord;
                    Y           :LongWord=0;
                    OneDimension:Boolean=True): Extended;
var a:AdressRec;
begin
a.Pt:= @XYset;
if not OneDimension then
  X:= (X Shl 1)+Y;
with a do
  case XYtype of
    nByte              : begin  Inc(Lw,X*SizeOf(Byte));      Result:= Byte(Pt^);      end;
    nShortInt          : begin  Inc(Lw,X*SizeOf(ShortInt));  Result:= ShortInt(Pt^);  end;
    nWord              : begin  Inc(Lw,X*SizeOf(Word));      Result:= Word(Pt^);      end;
    nSmallInt          : begin  Inc(Lw,X*SizeOf(SmallInt));  Result:= SmallInt(Pt^);  end;
    nInteger ,nLongInt : begin  Inc(Lw,X*SizeOf(Integer));   Result:= Integer(Pt^);   end;
    nCardinal,nLongWord: begin  Inc(Lw,X*SizeOf(LongWord));  Result:= LongWord(Pt^);  end;
    nInt64             : begin  Inc(Lw,X*SizeOf(Int64));     Result:= Int64(Pt^);     end;
    nReal48            : begin  Inc(Lw,X*SizeOf(Real48));    Result:= Real48(Pt^);    end;
    nSingle            : begin  Inc(Lw,X*SizeOf(Single));    Result:= Single(Pt^);    end;
    nReal    ,nDouble  : begin  Inc(Lw,X*SizeOf(Double));    Result:= Double(Pt^);    end;
    nCurrency          : begin  Inc(Lw,X*SizeOf(Currency));  Result:= Currency(Pt^);  end;
    nComp              : begin  Inc(Lw,X*SizeOf(Comp));      Result:= Comp(Pt^);      end;
    nExtended          : begin  Inc(Lw,X*SizeOf(Extended));  Result:= Extended(Pt^);  end;
   else                                                      Result:= 0;
  end;
end; {fetchvalue}


{02/02/2000}
function VectorLength(const X,Y:Extended): Extended;
var i: Extended;
begin
i:= SqRt(Sqr(X)+Sqr(Y));
if X<0 then
  i:= -i;
Result:= i;
end; {vectorlength}


function VectorLength(const X,Y,Z:Extended): Extended;
begin
Result:= SqRt(Sqr(X)+Sqr(Y)+Sqr(Z));
end; {vectorlength}


function  VectorLength(const X1,Y1,X2,Y2:Extended): Extended;
begin
Result:= VectorLength(X2-X1,Y2-Y1);
end; {vectorlength}


function  VectorLength(const X1,Y1,Z1,X2,Y2,Z2:Extended): Extended;
begin
Result:= VectorLength(X2-X1,Y2-Y1,Z2-Z1);
end; {vectorlength}


{17/03/2000}
procedure Vector2Cartesian(const VectorLength,Angle:Extended;
                           var X,Y                 :Extended);
begin
X:= VectorLength*Cos(Angle);  Y:= VectorLength*Sin(Angle);
end; {}


{17/03/2000}
procedure Vector2Cartesian(const VectorLength,Angle:Extended;
                           var X,Y                 :Integer);
begin
X:= Round(VectorLength*Cos(Angle));  Y:= Round(VectorLength*Sin(Angle));
end; {}


(*
function MakeRGBcolor(CircleDegrees:Word;
                      Radius       :Byte): TColor;
const Xred=0;  Yred=255;  Xgreen=128;  Ygreen=-180;  Xblue=-128;  Yblue=-180;
var Red,Green,Blue: LongWord;
    X,Y           : Integer;
begin
Vector2Cartesian(Radius,2*Pi*CircleDegrees/360,X,Y);
Red   := 255-Min(Round(VectorLength(X-Xred  ,Y-Yred  )),255);
Green := 255-Min(Round(VectorLength(X-Xgreen,Y-Ygreen)),255);
Blue  := 255-Min(Round(VectorLength(X-Xblue ,Y-Yblue )),255);
Result:= (Blue Shl 8)+(Green Shl 4)+Red;
end; {}
*)


{02/08/95}
procedure QuadFit(var XYset;                   {zero based counting}
                  XYtype      :NumericType;
             		  var Z,L,Q   :Extended;       {y=Z+L.x+Q.x.x}
                  Num_El      :LongWord;
                  C1          :Extended=0;     {1-dim: x:= c1+c2*0..numel-1}
                  C2          :Extended=1;     {2-dim: x-c1, y-c2 }
                  OneDimension:Boolean =True;
                  Step_El     :Word    =1);
var QFO: TQuadFit;
    X,Y: Extended;
    i  : LongInt;
begin
QFO:= TQuadFit.Create;
with QFO DO
  begin
  if Step_El=0 then
    Step_El:= 1;
  i:= Num_El*Step_El;
  repeat
    Dec(i,Step_El);
    if OneDimension then
      begin
      X:= C1+i*C2;
      Y:= FetchValue(XYset,XYtype,i);
      end
    else
      begin
      X:= FetchValue(XYset,XYtype,i,0,False)-C1;
      Y:= FetchValue(XYset,XYtype,i,1,False)-C2;
      end;
    Add_XY(X,Y);
  until i=0;
  Z:= Offset;  L:= Linear;  Q:= Quadratic;
  Free;
  end; {with}
end; {quadfit}

(*
function DECsingle_2_Real(VAR FourBytes): REAL;
var R,E: Extended;
begin
with DoubleWord(FourBytes) DO
  if Pt=nil then DECsingle_2_Real:= 0
  else
    begin
    E:= (Hw AND $7F80) DIV 128; {bit 14..7 woord 1}
    R:= (($0080+(Hw AND $007F))*65536.0 + Lw)  *  Exp((E-152)*Ln(2));
    {(hidden bit ~ bit  6..0 woord 1 ~ woord 2)  *2^(e-128)/2^24}
    if Hw AND $8000=$8000 then R:= -R;                  {bit 15}
    DECsingle_2_Real:= R;
    end;
end; {decsingle_2_real}

PROCEDURE Real_2_DECsingle(R     :Extended;
                           VAR FourBytes);
VAR E: INTEGER;
    L: LongInt;
begin
DoubleWord(FourBytes).Pt:= NIL;
if R<>0 then with DoubleWord(FourBytes) DO
  begin
  if R<0 then begin  R:= -R;  Hw:= $8000;  end;                           {bit 15}
  R:= Ln(R)/Ln(2);  E:= Trunc(R);                 {kies: r < 2^e => e>ln(r)/ln(2)}
  if R>0 then Inc(E);
  L:= ROUND(Exp((R-E+24)*Ln(2)));      {0,5ÛR<1 en L=R*2^24 (24 bits getal maken)}
  Hw:= Hw + ((E+128) Shl 7) + ((L DIV $FFFF) AND $7F);
  {Sign~Expo~Fraction(22..16) | shl(7,E+128) -> bit 7..14 , shr(16,L)->(1)+7 bits}
  Lw:= L AND $FFFF;                                              {laagste 16 bits}
  end;
end; {real_2_decsingle}
*)


function NiceStep(Value         :Extended;
                  DecadeDividers:DividerSet=[2,5];
                  SmallerValueOk:Boolean=True;
                  LargerValueOk :Boolean=True): Extended;
var MinDif,Dif          : Extended;
    i,MinStep,Vexp,VSign: Integer;

  procedure TestDif(TestStep:Integer);
  begin
  if ((TestStep<Value) and SmallerValueOk) or ((TestStep>Value) and LargerValueOk) then
       Dif:= abs(Value/TestStep-1);
  if Dif<MinDif then
    begin
    MinDif := Dif;
    MinStep:= TestStep;
    end;
  end;

begin
if Value=0 then Result:= 0
else
  begin
  Vsign:= Sign(Value);
  Value:= Abs(Value);
  VExp:= 0;
  while Value<1 do
    begin
    Dec(Vexp);
    Value:= Value*10;
    end;
  while Value>10 do
    begin
    Inc(Vexp);
    Value:= Value/10;
    end;
  MinDif := 10;
  MinStep:= 1;
  Dif    := MinDif;
  TestDif(10);
  for i:= 2 to 9 do if i in DecadeDividers then
    TestDif(i);
  Result:= VSign*MinStep*Power(10,VExp);
  end;
end; {nicestep}


function LineDistance2Origin(Gain,Offset:Extended): Extended;
begin
Result:= Abs(Offset)/VectorLength(Gain,1);
end; {linedistance2origin}


function LineDistance2Origin(X1,Y1,X2,Y2:Extended): Extended;
var Gain:Extended;
begin
if X1=X2 then
  Result:= Abs(X1)
else
  begin
  Gain:= (Y2-Y1)/(X2-X1);
  Result:= LineDistance2Origin(Gain,Y1-(Gain*X1));
  end;
end; {linedistance2origin}


{31/12/2017}
function IsPrime(ANumber:Int64): Boolean;
var i: Int64;
begin
if ANumber<=1                                  then Result:= False
else if ANumber<=3                             then Result:= True
else if (ANumber mod 2=0) or (ANumber mod 3=0) then Result:= false
else
  begin
  i:= 5;
  Result:= True;
  while (i*i<ANumber) and Result do
    begin
    Result:= (ANumber mod i>0) and (ANumber mod (i + 2)>0);
    i:= i+6;
    end;
  end;
end; {isprime}


{21/05/2020 internal data support uTExtendedX87}
{$IFDEF SelfTest}
function SelfTest: Boolean;
const N=5;
     {$IFDEF uTExtendedX87}
      tDelta=1e-11;
     {$ENDIF}
var Q    : TQuadFit;
    L    : TLinFit;
    a,b,c: MathExt;
    i    : Integer;
    ok   : Boolean;
    S    : TStatsSampler;
begin
a:= 1;  b:= 2;  c:= 3;
S:= TStatsSampler.Create(2);
with S do
  begin
  Add_X(-1);  Add_X(-2);  Add_X(-3);
  ok:= (Mean=-2) and (SumSquaresMean=2) and (VarianceEst=1) and (ConfidenceLimit=3.5);
  Free;
  end;
ok:= ok and (NiceStep(0.23,[2,5])=0.2) and (NiceStep(0.23,[2,5],False,True)=0.5);
L:= TLinFit.Create;
with L do
  begin
  for i:= 1 to 4 do
    Add_XY(i,a+b*i);
 {$IFDEF uTExtendedX87}
  //Ok:= Ok and (Abs(Offset-a)<tDelta) and (Abs(Linear-b)<tDelta);
  Ok:= Ok and (Offset=a) and (Linear=b);
 {$ELSE}
  Ok:= Ok and (Offset=a) and (Linear=b);
 {$ENDIF}
  Del_XY(2,a+b*2);
 {$IFDEF uTExtendedX87}
  Ok:= Ok and (Abs(Offset-a)<tDelta) and (Abs(Linear-b)<tDelta) and (Abs(Fit(4)-a-b*4)<tDelta);
  //Ok:= Ok and (Offset=a) and (Linear=b) and (Fit(4)=a+b*4);
 {$ELSE}
  Ok:= Ok and (Offset=a) and (Linear=b) and (Fit(4)=a+b*4);
 {$ENDIF}
  Free;
  end;
Q:= TQuadFit.Create(N);
with Q do
  begin
  for i:= 1 to Pred(N) do
    Add_XY(i,100+i);
  Add_XY(N,100+2*N);
  for i:= 1 to N do
    Ok:= Ok and (FitQuad(i,true)=100+i);                              //fallback test
  for i:= 1 to Pred(N) do
    Del_XY(i,100+i);
  Del_XY(N,100+2*N);
  for i:= 1 to 5 do
    Add_XY(i,a+(b+c*i)*i);
  Del_XY(4,a+(b+c*4)*4);
 {$IFDEF uTExtendedX87}
  //Ok:= Ok and (Abs(Offset-a)<tDelta) and (Abs(Linear-b)<tDelta) and (Abs(Quadratic-c)<tDelta) and (Abs(FitQuad(4)-a-(b+c*4)*4)<tDelta);
  Ok:= Ok and (Offset=a) and (Linear=b) and (Quadratic=c) and (FitQuad(4)=a+(b+c*4)*4);
 {$ELSE}
  Ok:= Ok and (Offset=a) and (Linear=b) and (Quadratic=c) and (FitQuad(4)=a+(b+c*4)*4);
 {$ENDIF}
  Initialize;
  for i:= 1 to 5 do
    Add_XY(i,a+b*i);
  Del_XY(4,a+b*4);
  {$IFDEF uTExtendedX87}
  //Ok:= Ok and (Abs(LinOffset-a)<tDelta) and (Abs(LinLinear-b)<tDelta) and (Abs(FitLin(4)-a-b*4)<tDelta);
  Ok:= Ok and (LinOffset=a) and (LinLinear=b) and (FitLin(4)=a+b*4);
  {$ELSE}
  Ok:= Ok and (LinOffset=a) and (LinLinear=b) and (FitLin(4)=a+b*4);
  {$ENDIF}
  Free;
  end;
Result:= Ok;
end;
{$ENDIF}

end.

