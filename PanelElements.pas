unit PanelElements; {© Theo van Soest Lazarus 2.0.8/FPC 3.0.4 31/03/2020-11/04/2021}
{$mode objfpc}{$h+}
{$I BistroMath_opt.inc}

(*
=================================================================================
 This library is original work of Theo van Soest.
 It is published under the GNU Lesser General Public License v3 (LGPL-3.0).
 https://tldrlegal.com/license/gnu-lesser-general-public-license-v3-%28lgpl-3%29

 Although this is a very specific implementation for BistroMath,
 the concept might have a wider application.
=================================================================================
*)

interface

uses SysUtils,
     TOtools,TObaseDef,TOconfigStrings,TOnumparser,Wellhofer;

     {
Parameters with the exclamation symbol have a left and right result. Therefore the left result is obtained as -"evaluation type" and the right results as +"evaluation type". When no sign is given the result will be the average of left and right value.

    a                Area ratio
                       wSource[Xsource].twSymAreaRatio
  ! b                Border position based on edges user choices
                       wSource[Xsource].twLevelPos[twUsedEdgeLevel].Penumbra[side].Calc;
    c                Center position as defined by user's choices
                      wSource[Xsource].twCenterPosCm as defined by choices
    C                Curvature of fit of top
                      wSource[Xsource].twTopModel.Qquad
    d                Relative flatness
                       wSource[Xsource].twFlatness*100
  ! D [20|50|80|90]  The result is the depth for a pdd or the edge of a profile
                       wSource[Xsource].twLevelPos[dxx].Penumbra[side].Calc;
  ! e                Derivative edge
                       wSource[Xsource].twLevelPos[dDerivative].Penumbra[side].Calc
    F                Relative Min | Max of IFA
                       ifthen(Xsign<0,wSource[Xsource].twRelMinInField,wSource[Xsource].twRelMaxInField);
    f                Absolute flatness
                       wSource[Xsource].twFlatness*100;
    G                Gamma analysis within IFA
                       GammaAnalysis(dsMeasured,dsReference,dsCalculated,True,NormAdjustNumEdit.Value/100);
    g                Gamma analysis over complete profile
                       GammaAnalysis(dsMeasured,dsReference,dsCalculated,False,NormAdjustNumEdit.Value/100);
  ! i                Sigmoid edge (inflection point)
                       wSource[Xsource].twLevelPos[dSigmoid].Penumbra[side].Calc;
  ! I                Sigmoid 50%,
                       wSource[Xsource].twLevelPos[dSigmoid50].Penumbra[side].Calc;
    L                Linac error
                       100*wSource[Xsource].twSymLinacError
    l                Elevation
                       100*wSource[Xsource].twLinSlope*
                           (GetPosition(Xsource,twFlatArr[twcRight])-GetPosition(Xsource,twFlatArr[twcLeft]))/twAbsNormVal
    m                Relative maximum
                       100*wSource[Xsource].twTopModel.Ytop/max(1,wSource[Xsource].twAbsNormVal)
    M                Top position
                       wSource[Xsource].twTopModel.Xtop
    N                Norm position
                       wSource[Xsource].twAbsNormPosCm
    n                Norm value
                       wSource[Xsource].twAbsNormVal
  ! p [|1|2]         Penumbra width
                       GetPenumbraWidth(XSource,side,ApplyDynamicWidth)
                         1: ApplyDynamicWidth:= False | according to standard definitions for conventional fields
                         2: ApplyDynamicWidth:= True  | based on relative dose of inflection point
                        (nothing) or other: value according to Field Types tab
  ! q                Slope of sigmoid model (always positive in this implementation)
                       Y[side]:= wSource[Xsource].twSigmoidFitData[side].twNMReport.BestVertex[sigmoid_Slope]/Ynorm
  ! Q                Sigmoid slope in the inflection point
                       Y[side]:= Abs(wSource[Xsource].twSigmoidFitData[side].twFitResult2)/Ynorm
    P [10|20|20/10]  Percentage depth dose
                       wSource[Xsource].twPDD10,twPDD20,twPDD20/twPDD10
  ! r [nn]           Profile Evaluation Point at fraction nn% of border relative to center of field
                       GetScaledQfValue((2*Ord(side)-1)*abs(n.mm)*edge,relative,scNormalised,Xsource)
  ! R [nn]           Profile Evaluation Point at fraction nn% of border, with border and center rounded to steps of 5 mm
    s                Symmetry
                       100*wSource[Xsource].twSymmetry
    S                Extended symmetry according to menu choices
  ! u                Border at user value
                          wSource[Xsource].twLevelPos[dUser].Penumbra[side].Calc
  ! U                Border at user value using Sigmoid
                          GetNormalisedRevLogistic(Side,Xsource,UserBorderDoseEdit_perc.Value)
    w                Width according to menu choices
                        wSource[Xsource].twLevelPos[twUsedEdgeLevel]
    X|Z[[-|+]value]  y value at X or Z*(1+depth/SSD)
       center based   GetScaledQfValue((2*Ord(side)-1)*abs(X),relative,scNormalised,Xsource)

    x|z[[-|+]value]  y value at x or z*(1+depth/SSD)
       absolute       GetScaledQfValue((2*Ord(side)-1)*abs(X),absolute,scNormalised,Xsource)
    Y  [[-|+]value]  x value at Y
                        if (ScanType in twcVertScans) and (ConvStg='100') and (Selection='Y') then
                          Y[side]:= twPosCm[twMaxArr]
                        else
       user scaling       Y[side]:= GetPenumbraValue(Xsource,X,side);
    y  [[-|+]value]  x value at y
                        if (ScanType in twcVertScans) and (ConvStg='100') and (Selection='Y') then
                          Y[side]:= twPosCm[twMaxArr]
                        else
       standard scaling   Y[side]:= GetPenumbraValue(Xsource,X/twAbsNormVal,side);
}

{05/03/2021 added 'Q'}
{31/03/2021 added 'Z','z'}
{01/04/2021 added pa_Xactual}
const
  EmptyXtype           =  #0;
  EvaluationXtypes     = ['a','b','c','C','d','D','e','F','f','G','i','I','L','l','M','m','N','n','p','P','q','Q','r','R','s','S','T','u','U','w','X','x','Y','y','Z','z',EmptyXtype];
  DefEnergyUncertainty = 0.01; {MeV}
  DefPanel             = 'PanelElements';
  DefCondTxt           = 'cond:';
  DefCondTypeString    = 'NFSMWEerdgus';     {linked to PCRconditionTypes}
  DefAnnotTxt          = 'annot:';
  DefAnnotTypeString   = '!sfFnzucCrSeT*RX'; {linked to AnnotationTypes}
  DefColorTxt          = 'color:';
  DefSizeTxt           = 'size:';

type
  //PCRconditionTypes is zero-based because of Lazarus limitations and linked to DefCondTypeString='NFSMWEfrdgus' which is a 1-based list
  PCRconditionTypes =(PCRstandard, PCRfffType , PCRsmall     , PCRMRlinac, PCRwedge      , PCRelectron,
                      PCRfffShape, PCRrefvalid, PCRisDivision, PCRisGamma, PCRisUnrelated, PCRSimpleViewHide);


  //linked to DefAnnotTypeString='!sfFnzucCrSeT*R', both lists indexed on 1
  AnnotationTypes=(pa_synthetic=1, pa_symmetric, pa_fitted , pa_fff , pa_normdif , pa_ssd   , pa_userlevel, pa_centered,
                   pa_centertype , pa_resampled, pa_shifted, pa_edge, pa_topmodel, pa_config, pa_RDD      , pa_Xactual );

  {The ResultsInfoRecord is the basic data element for retrieving analysis information in the results panel.
   See the supported EvaluationXtypes below.
   See function TAnalyseForm.EvaluateInfoRecord(var ARec:InfoRecord).}
  {01/02/2018 Ymultiplier, Y_mm}
  {05/06/2020 Ylevel}
  {17/06/2020 Xedge}
  {09/07/2020 Usource}
  {01/04/2021 added X_actual}
  ResultsInfoRecord=record
                      X          : twcFloatType;                                //input value for position or level
                      X_actual   : twcFloatType;                                //corrected value of X (when applicable)
                      Y          : array[twcSides] of twcFloatType;             //output for level or position
                      Sidedness  : Boolean;
                      ConvStg    : String;
                      Xsign      : Integer;                                     //0= unsided; -1/+1 = L/R
                      Xsource    : twcDataSource;                               //source
                      Usource    : twcDataSource;                               //confirmed source
                      Xchar      : Char;                                        //optional source selector
                      Xtype      : Char;                                        //evaluation type
                      Xedge      : twcPositionUseType;                          //confirmed edge
                      Xerrorval  : twcFloatType;
                      Ymultiplier: twcFloatType;
                      Y_mm       : UnitsType;                                   //see TOtools.pas
                      Iparse     : Integer;
                      Ylevel     : twcDoseLevel                                 //applied doselevel, when relevant
                    end;


 {===================TPanelConfig====================
 Object to transfer panel display rules from/to a ini file and manage the rules as a list of PanelConfigRec.
 TAnlyseForm binds this list to the array CxResults of CxBlocks(=TLabel for result + TLabel for values.
17/12/2019
 PCRSimpleViewHide, linked to SimpleModeItem
23/01/2020 FAddMode changed to integer; negative values clear all rules
09/02/2021 PCRconditionTypes:PCRMRlinac added}

  PCRfieldSizeArray  = array[0..1] of Single;
  PCRconditionsArray = array[PCRconditionTypes] of SmallInt; {-1=false,0=don't care,+1=true}

  {30/07/2020 added PCRlabelResult}
  PanelConfigRec=record
                PCRid           : Integer;
                PCRconfigstg    : String;
                PCRstg          : String;
                PCRdefaultsource: Boolean;
                PCRvalid        : Boolean;
                PCRsimple       : Boolean;                              {one single parameter is evaluated}
                PCRlabel        : String;                               {input for label}
                PCRlabelResult  : String;                               {postprocessing combined result with input label}
                PCRdecimals     : Integer;
                PCRunits        : String;
                PCRcol          : Integer;
                PCRrow          : Integer;
                PCRxrecord      : ResultsInfoRecord;
                PCRmodalities   : String;                               {'XEPO'; or-function}
                PCRenergySteps  : Integer;
                PCRenergyDif    : ShortInt;
                PCRscans        : set of twcScanTypes;                  {or-function}
                PCRconditions   : PCRconditionsArray;
                PCRannotations  : set of AnnotationTypes;               {-1=false,0=don't care,+1=true}
                PCRcolors       : set of AnnotationTypes;
                PCRfieldSize    : PCRfieldSizeArray;                    {fieldsize limits in cm}
               end;


TPanelConfig = Class(TObject)
 private
  FColLimit : Word;
  FRowLimit : Word;
  FDelimiter: Char;
  FaddMode  : Integer;
 public
  FElements: array of PanelConfigRec;
  constructor Create(MaxCols,MaxRows     :Word;
                     ADelimiter          :Char=','        );             reintroduce;
  function  AddElement(AElementStg       :String;
                       AStatusProc       :toExtMsgProc=nil): Boolean;
  function  GetCount                                       : Word;
  function  GetColMax                                      : Integer;
  function  GetRowMax                                      : Integer;
  function  GetMaxColumnCount                              : Integer;
  function  IDexists(aID                 :Integer         ): Boolean;
  procedure ReplaceDelimiter(NewDelimiter:Char            );
  procedure ConfigLoad(Sender            :TObject;
                       AFileName         :String          );             overload;
  procedure ConfigLoad(CF                :TConfigStrings  );             overload;
  procedure ConfigSave(Sender            :TObject;
                       AFileName         :String          );             overload;
  procedure ConfigSave(CF                :TConfigStrings  );             overload;
  procedure Clear;
  destructor Free;
  property AddMode       : Integer read FAddMode   write  FAddMode;
  property Count         : Word    read GetCount;
  property Delimiter     : Char    read FDelimiter write ReplaceDelimiter;
  property ColMax        : Integer read GetColmax;
  property RowMax        : Integer read GetRowmax;
  property MaxColumnLines: Integer read GetMaxColumnCount;
end;


implementation

uses Classes,Math;

{***************** TPanelConfig  ********************************************
 Holds and maintains all panel display rules. Can read and write to ini-file.
 The resulting list FEelements is handled by procedure RunPanelElements.}

constructor TPanelConfig.Create(MaxCols,MaxRows:Word;
                                ADelimiter     :Char=',');
begin
Inherited Create;
SetLength(FElements,0);
FColLimit := MaxCols;
FRowLimit := MaxRows;
FDelimiter:= ADelimiter;
FAddMode  := 1; {add standard rules}
end; {~create}

{02/01/2018
'v596   ,1 ,-1    ,n         ,0.01      ,"Normalisation value",1       ,%   ,0  ,0  ,XEPO      ,0     ,A    ,cond:-w,annot:csn'
 version,id,source,evaluation,multiplier,"label"              ,decimals,unit,col,row,modalities,energy,scans,cond:..,annot:..,..
         i  i     ,s         ,f          quoted for spaces    ,i       ,s   ,i  ,i  ,s         ,f     ,s    ,s      ,s,....

 version    :                is ignored, future use
 id         : integer > 0    the id number should be unique
                             a negative id removes element(id)
 label      : free text      when a space is within the text, the whole text should be "double quoted"
 unit       : free text      when unit ='%' the result will be multiplied by 100
 col,row    : >=0            there may be different elements with identical col and row which different or identical conditions; the last applicable will be visible
 evaluation : composed string of EvaluationXtype
              a simple type consists of of optional '-' or '+' followed by single EvaluationXtype
 modalities : 'XEPO' or subset
 energy     : [<|>]value
 scans      : A=any,
              H=twcHoriScans [snGT,snAB,snAngle,snGenericHorizontal]
              V=twcVertScans [snPDD,snFanLine]
              nnnnnnnnnnn    (n=0|1, maxlength 11,
                             snGT,snAB,snPDD,snAngle,snGenericHorizontal,snFreescan,snGenericProfile,snFanLine,snPlane,snUndefined)
 conditions : [-]W|F|S|s     [optional -]:=not, W=Wedge, F=FFF, S=Small, s=hide in simple mode
 annotations: '!sfFnzcrSe' or subset: pa_synthetic,pa_symmetric,pa_fitted,pa_fff,pa_normdif,pa_ssd,pa_centered,pa_resampled,pa_shifted,pa_edge)}
{05/01/2018 buildnumber added}
{17/01/2018 added energy selector,
            check on scantypes,
            clear when id=0,
            counting comma's and compare with #lines in parser}
{18/01/2018  energy: [<|>]value}
{01/02/2018 Ymultiplier, Y_mm}
{09/10/2018 keep invalid when PCRid<0}
{05/06/2020 Ylevel}
{09/07/2020 Usource}
{01/04/2021 X_actual}
function TPanelConfig.AddElement(AElementStg:String;
                                 AStatusProc:toExtMsgProc=nil): Boolean;
const CommaMinCnt=13;
var i,j,k,l,m: Integer;
    f        : twcFloatType;
    p        : toTNumParser;
    s        : String;
    ClearAll : Boolean;

    function LogResult(AResult:Boolean;
                       AMess  :String=''): Boolean;
    begin
    if (not AResult) and (AStatusProc<>nil) then
      begin
      if AMess='' then
        AMess:= Format('elem#%d="%s"',[p.CurrentLineNumber,p.CurrentLine]);
      AStatusProc(Format('>Panel?{%s}: %s',[AElementStg,AMess]));
      end;
    Result:= AResult;
    end;

    procedure pGetString(var AString:String;
                         EmptyOk    :Boolean=False);
    begin
    if Result then
      begin
      Result:= (not p.EndOfFile) or EmptyOk;
      if Result then
        begin
        p.NextLine;
        AString:= p.CurrentLine;
        Result := LogResult((Length(AString)>0) or EmptyOk);
        end;
      end;
    end;

    procedure pGetInteger(var AInteger:Integer);
    begin
    if Result then
      begin
      Result:= not p.EndOfFile;
      if Result then
        begin
        p.NextLine;
        AInteger:= p.NextInteger;
        Result  := LogResult(p.ConversionResult);
        end;
      end;
    end;

    procedure pGetFloat(var AFloat  :twcFloatType;
                        TakeNextLine:Boolean=True);
    begin
    if Result then
      begin
      Result:= not p.EndOfFile;
      if Result then
        begin
        if TakeNextLine then
          p.NextLine;
        AFloat:= p.NextFloat;
        Result:= LogResult(p.ConversionResult);
        end;
      end;
    end;

    function GetSign(AString:String): ShortInt;
    begin
    if Length(AString)=0   then Result:=  0
    else if AString[1]='-' then Result:= -1
    else if AString[1]='+' then Result:=  1
    else                        Result:=  0;
    end;

begin
if Length(AElementStg)>0 then
  begin
  i     := Length(FElements);
  f     := 0;
  k     := 0;
  p     := toTNumparser.Create(AElementStg,True,FDelimiter); {second parameter needed here};
  Result:= LogResult(p.DelimiterCountCheck,'Item split by space?');
  SetLength(FElements,Succ(i));
  with FElements[i] do
    begin
    pGetInteger(k);                                                             //buildnumber, no version checks at this moment
    pGetInteger(PCRid);
    ClearAll:= Result and (PCRid=0);
    Result  := (PCRid>0) and
                LogResult(p.DelimiterCount>=CommaMinCnt,'Incomplete line');
    pGetInteger(k);                                                             //curve selector
    PCRdefaultsource:= (k<0) or (not (k in [0..Ord(dsUnrelated)]));
    if not PCRdefaultsource then
      PCRxrecord.Xsource:= twcDataSource(k);
    PCRxrecord.Usource:= PCRxrecord.Xsource;
    PCRconfigstg      := AElementStg;
    FillChar(PCRconditions,SizeOf(PCRconditions),0);
    FillChar(PCRconditions,SizeOf(PCRcolors    ),0);
    pGetString(PCRstg);
    s:= PCRstg.Trim(['+','-']);
    l:= Length(s);
    m:= CountChars(s,csNumeric + ['/','-','+']);
    PCRsimple:= ((l-m=1)                     or
                 ((Length(PCRstg)=l+1) and (PCRstg[1] in ['-','+'])))
                and
                (s[1] in EvaluationXtypes-[EmptyXtype]);
    if PCRsimple then
      begin
      PCRxrecord.Xtype:= s[1];
      PCRxrecord.Xsign:= GetSign(PCRstg[1]);
      if (l-m=1) and (m>0) then
        begin
        PCRxrecord.ConvStg:= s.Substring(1,m).Trim(['+','-']);                  //zero-based
        pGetFloat(PCRxrecord.X,False);                                          //get X
        if PCRxrecord.Xsign=0 then
          PCRxrecord.Xsign:= GetSign(s[2])                                      //s[1] is Xtype, check for sign there
        else
          PCRxrecord.X:= PCRxrecord.X*PCRxrecord.Xsign;
        end;
      PCRxrecord.X_actual:= PCRxrecord.X;
      end;
    PCRxrecord.Xedge := dUseUndefined;
    PCRxrecord.Y_mm  := no_Units;
    PCRxrecord.Ylevel:= d50;
    pGetFloat(PCRxrecord.Ymultiplier);
    pGetFloat(PCRxrecord.Xerrorval);
    pGetString(PCRlabel);
    pGetInteger(PCRdecimals);
    pGetString(PCRunits,True);
    pGetInteger(PCRcol);
    pGetInteger(PCRrow);
    Result:= InRange(PCRcol,0,FColLimit) and ((FRowLimit=0) or InRange(PCRrow,0,FRowLimit));
    pGetString(PCRmodalities);
    pGetFloat(f);
    if Result then
      begin
      PCRlabelResult:= PCRlabel;
      PCRenergySteps:= Round(f/DefEnergyUncertainty);
      if p.ConversionFirstPos>1 then
        case p.CurrentLine[Pred(p.ConversionFirstPos)] of
          '<': PCRenergyDif:= -1;
          '>': PCRenergyDif:=  1;
         else  PCRenergyDif:=  0;
         end;
      Result:= (PCRcol>=0) and (PCRrow>=0);
      end;
    pGetString(s);
    if Length(s)>0 then
      begin
      for j:= 1 to Length(s) do
        Result:= Result and (S[j] in ['A','H','V','1','0']);
      if Result then {s contains at least 1 character}
        if s='H'      then PCRscans:= twcHoriScans
        else if s='V' then PCRscans:= twcVertScans
        else if s='A' then PCRscans:= [snGT..snUndefined]
        else for j:= 1 to Min(Length(s),SizeOf(twcScanTypes)) do
          if s[j]='1' then PCRscans:= PCRScans+[twcScanTypes(Pred(Ord(j)))];
      end;
    PCRfieldSize[0]:= -1;
    PCRfieldSize[1]:= -1;
    while Result and (not p.EndOfFile) do
      begin
      pGetString(s,True);
      if Pos(DefAnnotTxt,s)=1 then
        begin
        Delete(s,1,Length(DefAnnotTxt));
        for j:= 1 to Length(DefAnnotTypeString) do
         if Pos(DefAnnotTypeString[j],s)>0 then
           PCRannotations:= PCRannotations+[AnnotationTypes(j)];
        end
      else if Pos(DefColorTxt,s)=1 then
        begin
        Delete(s,1,Length(DefColorTxt));
        for j:= 1 to Length(DefAnnotTypeString) do
         if Pos(DefAnnotTypeString[j],s)>0 then
           PCRcolors:= PCRcolors+[AnnotationTypes(j)];
        end
      else if Pos(DefCondTxt,s)=1 then
        begin
        Delete(s,1,Length(DefCondTxt));
        for j:= 1 to Length(DefCondTypeString) do
          begin
          k:= Pos(DefCondTypeString[j],s);
          if k>0 then
            begin
            k:= ifthen((k>1) and (s[Pred(k)]='-'),-1,1);
            PCRconditions[PCRconditionTypes(j-1)]:=  k;                         //PCRconditionTypes is zero-based because of Lazarus limitations
            end;
          end; {for}
        end {else if}
      else if Pos(DefSizeTxt,s)=1 then
        begin
        j:= 0;
        repeat
          f:= p.NextFloat;
          if p.ConversionResult then
            begin
            Inc(j);                                                             //avoid endless loops
            case p.CurrentLine[Pred(p.ConversionFirstPos)] of
              '>': i:=  0;
              '<': i:=  1;
             else  f:= -1;
             end;
            if f>=0 then
              PCRfieldSize[i]:= f;
            end
          else
            f:= -1;
        until (f<0) or (j=2);
        end;
      end; {while}
    PCRvalid:= Result;
    k       := PCRid;
    end;
  if Result or (k<0) then
    begin
    k:= Abs(k);
    j:= i;
    while j>0 do
      begin
      Dec(j);
      if Abs(FElements[j].PCRid)=k then
        begin
        if FElements[j].PCRid>0 then                                            //keep invalid version
          FElements[j]:= FElements[i];
        SetLength(FElements,i);
        end;
      end; {while}
    end {result}
  else
    SetLength(FElements,i);
  p.Free;
  if ClearAll then
    Clear;
  end
else
  Result:= False;
end; {~addelement}


{02/01/2018}
function TPanelConfig.GetCount: Word;
begin
Result:= Length(FElements);
end; {~getcount}


{22/01/2018}
function TPanelConfig.GetRowMax: Integer;
var i: Integer;
begin
Result:= -1;
if Count>0 then
  for i:= 0 to Pred(Count) do
    Result:= Max(Result,FElements[i].PCRrow);
end; {~getrowmax}


{25/08/2020}
function TPanelConfig.GetColMax: Integer;
var i: Integer;
begin
Result:= -1;
if Count>0 then
  for i:= 0 to Pred(Count) do
    Result:= Max(Result,FElements[i].PCRcol);
end; {~getcolmax}


{25/08/2020 find maximum number of elements in a single column}
function TPanelConfig.GetMaxColumnCount: Integer;
var i,j,k,n: Integer;
begin
Result:= 0;
k     := ColMax;
if (k>=0) and (Count>0) then
  for i:= 0 to k do
    begin
    n:= 0;
    for j:= 0 to Pred(Count) do
      if FElements[j].PCRcol=i then
        Inc(n);
    Result:= Max(n,Result);
  end;
end; {getmaxcolumcount}


{09/10/2018}
function TPanelConfig.IDexists(aID:Integer): Boolean;
var i: Integer;
begin
Result:= False;
i     := Count;
while (i>0) and (not Result) do
  begin
  Dec(i);
  Result:= FElements[i].PCRid=aID;
  end; {while}
end; {~idexists}


{02/01/2018}
procedure TPanelConfig.ConfigLoad(Sender   :TObject;
                                  AFileName:String);
var CF: TConfigStrings;
begin
CF:= TConfigStrings.Create(AFileName);
ConfigLoad(CF);
CF.Free;
end; {~configload}


{02/01/2018}
procedure TPanelConfig.ConfigSave(Sender   :TObject;
                                  AFileName:String);
var CF: TConfigStrings;
begin
CF:= TConfigStrings.Create(AFileName);
ConfigSave(CF);
CF.Free;
end; {~configsave}


{02/01/2018}
{17/01/2018 delimiter support}
{06/10/2020 fundamentals alternative}
procedure TPanelConfig.ReplaceDelimiter(NewDelimiter:Char);
var i: Integer;
begin
if NewDelimiter<>FDelimiter then
  begin
  i:= Count;
  while i>0 do
    begin
    Dec(i);
    FElements[i].PCRconfigstg:= FElements[i].PCRconfigstg.Replace(FDelimiter,NewDelimiter);
    end;
  FDelimiter:= NewDelimiter;
  end;
end; {~replacedelimiter}


{02/01/2018}
{17/01/2018 delimiter support}
procedure TPanelConfig.ConfigSave(CF:TConfigStrings);
var i: Integer;
begin
if Length(FElements)>0 then
  begin
  CF.WriteString(DefPanel,'sep',Delimiter);
  CF.WriteInteger(DefPanel,'add',0);
  CF.WriteString(DefPanel,'doc',
                 'b(build),id,curve sel,eval.type,multiplier,errorval,"label",deci,unit,col,row,mod,energy,scantype[,cond:-][,annot:-][,color:-]');
  for i:= 0 to Pred(Length(FElements)) do
    CF.WriteString(DefPanel,Num2Stg(FElements[i].PCRid),FElements[i].PCRconfigstg);
  end;
end; {~configsave}


{02/01/2018}
{17/01/2018 delimiter support}
procedure TPanelConfig.ConfigLoad(CF:TConfigStrings);
var tList: TStringList;
    i,j  : Integer;
    id,s : String;
begin
tList:= TStringList.Create;
CF.ReadSection(DefPanel,tList);
j:= tList.Count;
if j>0 then
  for i:= 0 to Pred(j) do
    begin
    id:= tList.Strings[i];
    s := CF.ReadString(DefPanel,id,Delimiter);
    if id='sep' then
      begin
      if Length(s)=1 then
        Delimiter:= s[1];
      end
    else if id='add' then
      begin
      AddMode:= Sign(CF.ReadInteger(DefPanel,id,1));
      if AddMode=-1 then Clear;
      end
    else if id<>'doc' then
      AddElement(s);
    end;
tList.Free;
end; {~configload}


{02/01/2018}
procedure TPanelConfig.Clear;
begin
Finalize(FElements);
end; {~clear}


{02/01/2018}
destructor TPanelConfig.Free;
begin
Clear;
end; {~free}


end.
