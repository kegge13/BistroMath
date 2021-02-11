unit TONelderMead;   {© Theo van Soest, FPC 3.2.0: 10/02/2021}
{$MODE DELPHI}

(*
=== 28/04/2013 - 10/02/2021 ==== Nelder-Mead =================================================================
 ----------------------------------------------------
 Based on adaptive Nelder-Mead with dimension-dependent parameters.
 Fuchang Gao, Lixing Han: Implementing the Nelder-Mead simplex algorithm with adaptive parameters
 Comput Optim Appl, DOI 10.1007/s10589-010-9329-3 [2011]

 A vertex is a vector consisting of the parameters of the function to be optimised. The length is fDim.
 The Nelder-Mead optimisation creates a simplex of fDim+1 vertices using variations on the initial vertex.
 This simplex crawls through the fDim-dimensional space using the results of a given error function.
 For this reason the simplex can be seen as a amoebe (a jellyfish like bacteria).
 The error function is called with a (fDim) vertex and is expected to calculate the difference between
 data and the function to be optimised. The user is responsible for the error function.

 The Nelder-Mead optimisation does never end by itself. Therefore the user has to decide on when
 the result is good enough using:
 -score of the error function
 -rate of change of the error function
 -limits on cycles
 -limits on time
 -limits on restarts

 This implementation can run in single-thread mode with just one amoebe and in multi-threaded mode with
 a separate amoebe in each of the started threads. When all amoebes are done the best amoebe will be reported.
 Any restarts will be done with this best result as initial vertex.
 The error function must be thread-save when running in multi-thread mode: it must preserve read-only access
 to all underlying data since multiple threads will call the error function within their own thread.
 ---
 This unit is published under the GNU Lesser General Public License v3 (LGPL-3.0).
 https://tldrlegal.com/license/gnu-lesser-general-public-license-v3-%28lgpl-3%29
==============================================================================================================
*)



interface      {09/02/2021}

uses Classes, SysUtils;

type
  TaVertexDataType       = Double;
  TaFunctionVertex       = array of TaVertexDataType;
  TaVertexRecord         = record
                            Score : Double;
                            Vertex: TaFunctionVertex;
                            Valid : Boolean;
                            Tag   : Boolean;
                           end;
  TaVertexArray          = array of TaVertexRecord;
  TaMultParamFunction    = function(a:TaFunctionVertex): TaVertexDataType;
  TaMultParamObjFunction = function(a:TaFunctionVertex): TaVertexDataType of object;
  NelderMeadParam        = (NMreflection,NMexpansion,NMcontraction,NMshrinkage);
  TaIllegalFunModel      = (illLarge,illSmall,illZero);

  {All reports use this record.
   In multithreaded mode the reports are stored in a list and reported asynchronously by the main thread.}
  {27/07/2017 amoebeid, restarts->integer}
  {01/02/2021 LastCallBackTime,LastCallBackScore}
  NMReportRecord=record
                  BestScore        : TaVertexDataType;
                  BestVertex       : TaFunctionVertex;  //The parameters of the error function
                  Cycles           : LongWord;          //Used cycles for the optimisation
                  Seconds          : Single;            //Calender time used.
                  AmoebeID         : Word;              //Each thread has its own report, starting from 0
                  NMsteps          : array[NelderMeadParam] of LongWord; //Counts each type of step.
                  Restarts         : Integer;           //Restaring does help to jump into a better solution.
                  FitValid         : Boolean;           //The vertex produces a legal error function result
                  ENR              : Single;            //extended noise reduction tries to flatten 'waves'
                  ReportTime       : TDateTime;
                  Reported         : Boolean;
                 end;

  NMReportPointer       = ^NMReportRecord;
  TaAmoebeReportProc    = procedure(var AReport:NMReportRecord);
  TaAmoebeReportObjProc = procedure(var AReport:NMReportRecord) of object;

  //The TaNMSimplexShared is a container which is create by the main thread and shared with all t
  TaNMSimplexShared=class(TObject)
    private
      fAmoebes             : Word;
      procedure SetAmoebes(NumberOfAmoebes:Word);
      procedure ClearReportLists;
    protected
      fThreadID            : TThreadID;
      fActiveCnt           : Integer;
      fDim                 : Word;
      fLastDim             : Word;
      fNMpar               : array[NelderMeadParam] of TaVertexDataType;        //parameters of the Nelder-Mead algorithm
      fMaxCycles           : LongWord;
      fSmallLimits         : TaFunctionVertex;
      fLowerLimits         : TaFunctionVertex;
      fUpperLimits         : TaFunctionVertex;
      fErrorFunction       : TaMultParamFunction;
      fErrorObjFunction    : TaMultParamObjFunction;
      fIllegalTest         : TaIllegalFunModel;
      fCumulativeSeconds   : Single;
      fLoopReports         : array of array of NMReportRecord;                  //one array per amoebe
    public
     fAlive               : Boolean;
     fIllegalErrorFunValue: TaVertexDataType;
     fRandomChangeFraction: Single;
     fMaxRestarts         : Word;
     fMaxSeconds          : Single;
     fMinScoreChange      : TaVertexDataType;
     fIllegalErrorRetries : Word;
     fLoopCallBack        : TaAmoebeReportProc;
     fLoopCallBackObj     : TaAmoebeReportObjProc;
     fTimeCallBack        : TaAmoebeReportProc;
     fTimeCallBackObj     : TaAmoebeReportObjProc;
     fCallBackDifference  : Single;
     fCallBack_ms         : LongWord;
     fCrawlReportList     : array of NMreportRecord;                            //one per amoebe
     constructor Create(NumberofAmoebes:Word=1);
     procedure   InitializeCrawlList(InitialVertex  :TaFunctionVertex;
                                     InitialScore   :TaVertexDataType;
                                     AAmoebeID      :Integer=-1;
                                     InitialRestarts:Integer=0);
     function    Evaluate(var AVertex    :TaFunctionVertex): TaVertexDataType;
     function    IsLegalValue(var AValue:TaVertexDataType ): Boolean;
     procedure   RandomChange(var AVertex:TaFunctionVertex;
                              Index      :Integer=-1);
     procedure   LimitVertex(var AVertex:TaFunctionVertex );
     procedure   ProcessReports;
     destructor  Destroy;                                                       override;
     property Amoebes: Word    read fAmoebes write SetAmoebes;
    end;


  TaNMObjectRunProc  = procedure(var ACrawlReport:NMReportRecord;
                                 CallBackMode    :Boolean=False  ) of object;

  TaNMSimplexAmoebe=class(TObject)
  protected
   procedure Crawl;
  private
   fLocker           : TMultiReadExclusiveWriteSynchronizer;
   fCallBackMode     : Boolean;
   fCollectAllReports: Boolean;
   fAmoebeID         : Integer;
   fSharedData       : TaNMSimplexShared;
   fLastCallBackTime : TDateTime;
   fLastCallBackScore: TaVertexDataType;
   fNMP              : NMReportPointer;
   procedure AddLoopReport;
   procedure CallBack;
  public
   constructor Create(SharedData       : TaNMSimplexShared;
                      AmoebeID         : Integer;
                      CallBackAmoebes  : Integer=0;
                      CollectAllReports: Boolean=False);                        reintroduce;
   destructor  Destroy;                                                         override;
  end;


  TaNMSimplexAmoebeThread=class(TThread)
  protected
   procedure Execute;                                          override;
  private
   fAmoebe: TaNMSimplexAmoebe;
  public
   constructor Create(SharedObject      : TaNMSimplexShared;
                      AmoebeID          : Integer;
                      CallBackAmoebes   : Integer=0;
                      CollectAllReports : Boolean=False;
                      SetFreeOnTerminate: Boolean=True);        reintroduce;
   destructor  Destroy;                                         override;
  end;

  //The TaNMsimplex class is the main object
  TaNMsimplex=class(TObject)
   private
    fAdaptive       : Boolean;
    fBestSimplex    : TaVertexRecord;
    fLocker         : TMultiReadExclusiveWriteSynchronizer;
    function  GetAlive                                            : Boolean;
    procedure SetAlive(AState            :Boolean                );
    procedure SetAdaptive(AdaptiveMode   :Boolean                );
    procedure Initialize(NumberOfAmoebes :Word;
                         ErrObjFunction  :TaMultParamObjFunction;
                         ErrFunction     :TaMultParamFunction;
                         ErrFunParameters:Word                   );
    procedure InitReport(var AVertex     :TaFunctionVertex       );
    procedure InitLimits(DefaultLow      :TaVertexDataType=-1e25;
                         DefaultHigh     :TaVertexDataType= 1e25;
                         DefaultSmall    :TAVertexDataType= 1e-15);
    procedure SetNMSmall(Index           :Word;
                         AValue          :TaVertexDataType= 1e-15);
    procedure SetNMLower(Index           :Word;
                         AValue          :TaVertexDataType=-1e25 );
    procedure SetNMUpper(Index           :Word;
                         AValue          :TaVertexDataType= 1e25 );
    procedure SetMaxCycles(Cycles        :LongWord=0             );
    function  GetfNM(Index               :NelderMeadParam        ): TaVertexDataType;
    procedure SetfNM(Index               :NelderMeadParam;
                     AValue              :TaVertexDataType=0     );
    function  GetLoopReportPtr(Amoebe,Idx:Word                   ): NMReportPointer;
   public
    CrawlReport: NMReportRecord;
    ResultData : TaNMSimplexShared;
    constructor Create(AErrorObjFunction   :TaMultParamObjFunction;
                       ErrFunNumParameters :Word;                               //fDim
                       NumberOfAmoebes     :Word            =1;
                       IllegalErrorFunValue:TaVertexDataType=1e-10;
                       DefaultLowerLimit   :TaVertexDataType=-1e25;
                       DefaultUpperLimit   :TaVertexDataType= 1e25);  reintroduce;  overload;
    constructor Create(AErrorFunction      :TaMultParamFunction;
                       ErrFunNumParameters :Word;
                       NumberOfAmoebes     :Word            =1;
                       IllegalErrorFunValue:TaVertexDataType=1e-10;
                       DefaultLowerLimit   :TaVertexDataType=-1e25;
                       DefaultUpperLimit   :TaVertexDataType= 1e25);  reintroduce;  overload;
    procedure   StartAmoebe(AVertex             :TaFunctionVertex;              //multi-threaded
                            CumulativeSeconds   :Single     =0;
                            CallBackAmoebes     :Integer    =0;                 //number of in-procss reporting amoebes
                            ReportAllAmoebes    :Boolean    =False);            //on each restart and callback trigger a report is added
    procedure   SingleAmoebe(AVertex            :TaFunctionVertex;              //main thread only
                             CumulativeSeconds  :Single     =0;
                             CallBackAmoebes    :Integer    =0    );
    procedure   AddToReportList(var ACrawlReport:NMReportRecord   );
    procedure   StopAmoebe;
    destructor  Destroy;                                    override;
    property Adaptive                        : Boolean         read fAdaptive   write SetAdaptive;
    property Alive                           : Boolean         read GetAlive    write SetAlive;
    property BestSimplex                     : TaVertexRecord  read fBestSimplex;
    property LoopReportPtr[Amoebe,Index:Word]: NMReportPointer read GetLoopReportPtr;
    property MaxCycles                       : LongWord                         write SetMaxCycles;
    property VertexSmall[Index:Word]         : TaVertexDataType                 write SetNMSmall;
    property VertexLower[Index:Word]         : TaVertexDataType                 write SetNMLower;
    property VertexUpper[Index:Word]         : TaVertexDataType                 write SetNMUpper;
   end;


function  WithinRange(AValue,Limit1,Limit2:TaVertexDataType;
                     Limit1Inclusive      :Boolean=False;
                     Limit2Inclusive      :Boolean=False  ): Boolean;


implementation

uses {$IFDEF Windows}
      Windows,
     {$ENDIF}
      Math,  Forms,
      DateUtils;


{04/02/2021}
constructor TaNMSimplexShared.Create(NumberofAmoebes:Word=1);
begin
SetLength(fSmallLimits,0);
SetLength(fLowerLimits,0);
SetLength(fUpperLimits,0);
SetLength(fCrawlReportList,0);
fAlive               := False;
fActiveCnt           := 0;
fDim                 := 0;
fLastDim             := 0;
fMaxCycles           := 0;
fErrorFunction       := nil;
fErrorObjFunction    := nil;
fIllegalErrorFunValue:= 0;
fCumulativeSeconds   := 0;
fRandomChangeFraction:= 0;
fMaxRestarts         := 0;
fMaxSeconds          := 0;
fMinScoreChange      := 0;
fIllegalErrorRetries := 0;
fThreadID            := GetCurrentThreadID;
Amoebes              := NumberofAmoebes;
FillChar(fNMpar,SizeOf(fNMpar),0);
end; {~create}


{08/02/2021}
procedure TaNMSimplexShared.SetAmoebes(NumberOfAmoebes: Word);
begin
if not fAlive then
  begin
  fAmoebes:= NumberOfAmoebes;
  ClearReportLists;
  end;
end; {~setamoebes}


{22/07/2017}
{08/02/2021 added amoebeid}
procedure TaNMSimplexShared.InitializeCrawlList(InitialVertex  :TaFunctionVertex;
                                                InitialScore   :TaVertexDataType;
                                                AAmoebeID      :Integer=-1;
                                                InitialRestarts:Integer=0);
begin
if (AAmoebeID>=0) and (Length(fCrawlReportList)>AAmoebeID) then
  with fCrawlReportList[AAmoebeID] do
    begin
    FillChar(NMsteps,SizeOf(NMsteps),0);
    Seconds   := 0;
    Cycles    := 0;
    Restarts  := InitialRestarts;
    BestVertex:= Copy(InitialVertex);
    BestScore := InitialScore;
    AmoebeID  := AAmoebeID;
    end;
end; {~initializecrawllist}


procedure TaNMSimplexShared.LimitVertex(var AVertex:TaFunctionVertex);
var i: Word;
begin
for i:= 0 to fDim-1 do
  begin
  AVertex[i]:= Math.Max(fLowerLimits[i],Math.Min(AVertex[i],fUpperLimits[i]));
  if Abs(AVertex[i])<fSmallLimits[i] then
    AVertex[i]:= Sign(AVertex[i])*fSmallLimits[i];
  end;
end; {~limitvertex}


procedure TaNMSimplexShared.RandomChange(var AVertex:TaFunctionVertex;
                                         Index      :Integer=-1);
begin
if not InRange(Index,0,fDim-1) then
  Index:= Round(Random(fDim-1));
AVertex[Index]:= AVertex[Index]*(1+(Random-0.5)*fRandomChangeFraction);
end; {~randomchange}


function TaNMSimplexShared.IsLegalValue(var AValue:TaVertexDataType): Boolean;
begin
try
  case fIllegalTest of
    illLarge: Result:= abs(AValue/fIllegalErrorFunValue)< 0.9;
    illSmall: Result:= abs(AValue/fIllegalErrorFunValue)> 2;
   else       Result:= AValue                           <>0;
  end;
 except
              Result:= True;
 end;
end; {~islegalvalue}


function TaNMSimplexShared.Evaluate(var AVertex:TaFunctionVertex): TaVertexDataType;
begin
if assigned(fErrorObjFunction) then Result:= fErrorObjFunction(AVertex)
else                                Result:= fErrorFunction(AVertex);
end; {~evaluate}


{09/02/2021}
procedure TaNMSimplexShared.ClearReportLists;
var i,j: Integer;
begin
i:= Length(fCrawlReportList);
while i>0 do
  begin
  Dec(i);
  with fCrawlReportList[i] do
   if assigned(BestVertex) then Finalize(BestVertex);
  end;
i:= Length(fLoopReports);
while i>0 do
  begin
  Dec(i);
  j:= Length(fLoopReports[i]);
  while j>0 do
    begin
    Dec(j);
    with fLoopReports[i,j] do
     if assigned(BestVertex) then Finalize(BestVertex);
    end;
  Finalize(fLoopReports[i]);
  end;
SetLength(fCrawlReportList,fAmoebes);
SetLength(fLoopReports    ,fAmoebes)
end; {~clearreportlists}


{09/02/2021}
procedure TaNMSimplexShared.ProcessReports;
var i,j,k  : Integer;
    fLocker: TMultiReadExclusiveWriteSynchronizer;

begin
i      := Length(fLoopReports);
fLocker:= TMultiReadExclusiveWriteSynchronizer.Create;
while i>0 do
  begin
  Dec(i);
  fLocker.BeginRead;
  j:= Length(fLoopReports[i]);
  fLocker.EndRead;
  if j>0 then
    for k:= 0 to Pred(j) do
      begin
      fLocker.BeginWrite;
      fLoopReports[i,k].Reported:= True;
      if assigned(fLoopCallBackObj)      then fLoopCallBackObj(fLoopReports[i,k])
      else if assigned(fLoopCallBack)    then fLoopCallBack(fLoopReports[i,k])
      else if assigned(fTimeCallBackObj) then fTimeCallBackObj(fLoopReports[i,k])
      else if assigned(fLoopCallBack)    then fTimeCallBack(fLoopReports[i,k]);
      fLocker.EndWrite;
      end;
  end;
fLocker.Free;
end; {~processreports}


{09/02/2021}
destructor TaNMSimplexShared.Destroy;
begin
fAlive := False;
Amoebes:= 0;
Finalize(fSmallLimits);
Finalize(fLowerLimits);
Finalize(fUpperLimits);
Inherited;
end; {~destroy}


//------------TaNMSimplexAmoebe----------------------------


{09/02/2021}
constructor TaNMSimplexAmoebe.Create(SharedData       : TaNMSimplexShared;
                                     AmoebeID         : Integer;
                                     CallBackAmoebes  : Integer=0;
                                     CollectAllReports: Boolean=False);
begin
Inherited Create;
fLocker:= TMultiReadExclusiveWriteSynchronizer.Create;
if assigned(SharedData) then
  begin
  fAmoebeID         := AmoebeID;
  fSharedData       := SharedData;
  fNMP              := @fSharedData.fCrawlReportList[fAmoebeID];
  fCollectAllReports:= CollectAllReports;
  with fSharedData do
    fCallBackMode:= (AmoebeID<CallBackAmoebes) and
                   (assigned(fLoopCallBackObj) or assigned(fLoopCallBack) or assigned(fTimeCallBackObj) or assigned(fTimeCallBack));
  end
else
  fAmoebeID:= -1;
end; {~create}


{09/02/2021}
procedure TaNMsimplexAmoebe.AddLoopReport;
var b  : Boolean;
    i,j: Integer;
begin
i:= 0;
fLocker.BeginRead;
j:= Length(fSharedData.fLoopReports[fAmoebeID]);
b:= (j>0);
while (i<j) and b do
  begin
  b:= not fSharedData.fLoopReports[fAmoebeID,i].Reported;
  Inc(i);
  end;
fLocker.EndRead;
fLocker.BeginWrite;
if (not b) and (j>0) then j:= Pred(i)
else                      SetLength(fSharedData.fLoopReports[fAmoebeID],j+1);
fSharedData.fLoopReports[fAmoebeID,j]         := fNMP^;
fSharedData.fLoopReports[fAmoebeID,j].Reported:= False;
fLocker.EndWrite;
end; {~addloopreport}


{09/02/2021}
procedure TaNMsimplexAmoebe.CallBack;
var b  : Boolean;
begin
with fSharedData do
  begin
  b:= (MilliSecondsBetween(Now,fLastCallBackTime)>fCallBack_ms)     or
      ((fNMP^.BestScore<>0) and (abs(fLastCallBackScore/fNMP^.BestScore-1)>fCallBackDifference));
  if b then
    begin
    fLastCallBackTime := Now;
    fLastCallBackScore:= fNMP^.BestScore;
    fNMP^.ReportTime  := fLastCallBackTime;
    if GetThreadID=fThreadID then
      with fSharedData do
        begin
        if assigned(fLoopCallBackObj)      then fLoopCallBackObj(fNMP^)
        else if assigned(fLoopCallBack)    then fLoopCallBack(fNMP^)
        else if assigned(fTimeCallBackObj) then fTimeCallBackObj(fNMP^)
        else if assigned(fLoopCallBack)    then fTimeCallBack(fNMP^);
        end
    else
      AddLoopReport;
    end;
  end;
end; {~callback}


{09/02/2021 old object into one procedure}
procedure TaNMSimplexAmoebe.Crawl;
var i,j,Dim,LastDim               : Word;
    NMCycles,CumulativeMaxCycles  : LongWord;
    tmpVertex,BVertex,SavedVertex : TaFunctionVertex;
    GravityVertex,ReflectedVertex : TaFunctionVertex;
    ReflectedScore,x              : TaVertexDataType;
    tmpScore,BScore,ChangeFraction: TaVertexDataType;
    Start                         : TDateTime;
    LastAliveCheck                : Single;
    Simplex                       : TaVertexArray;

    procedure StoreSimplex(Index  :Word;
                           AScore :TaVertexDataType;
                           AVertex:TaFunctionVertex);
    begin
    with Simplex[Index] do
      begin
      Valid := True;
      Score := AScore;
      Vertex:= Copy(AVertex);
      end;
    end; {~storesimplex}

    procedure BubbleStore(NewScore     :TaVertexDataType;
                          var NewVertex:TaFunctionVertex;
                          Index        :Word=0);
    var j,k    : Word;
        StoreOk: Boolean;

      procedure MoveValid(a,b:Word);
      begin
      with Simplex[b] do
        begin
        Valid := Simplex[a].Valid;
        Tag   := Simplex[a].Tag;
        Score := Simplex[a].Score;
        Vertex:= Simplex[a].Vertex;
        end;
      with Simplex[a] do
        begin
        Valid:= False;
        Tag  := False;
        Score:= -1;
        Finalize(Vertex);
        end;
      end;

    begin
    while (Index<Dim) and (Simplex[Index].Valid) and (NewScore>Simplex[Index].Score) do Inc(Index); {simplex size = Dim+1}
    StoreOk:= False;
    j      := Succ(Index);
    if (Index<Dim) then
      begin
      if (Simplex[Index].Valid) and (Simplex[j].Valid) and (NewScore<=Simplex[Index].Score) then
        begin {storage position found but occupied}
        k:= Succ(j);
        while (Simplex[k].Valid) and (k<Dim) do Inc(k);
        while (k>j) and (not Simplex[k].Valid) do
          begin
          MoveValid(Pred(k),k);
          Dec(k);
          end;
        end;
      if (not Simplex[Index].Valid) and ((not Simplex[j].Valid) or (NewScore<=Simplex[j].Score)) then
        StoreOk:= True {usable empty space found}
      else if Simplex[Index].Valid and (not Simplex[j].Valid) and (NewScore<=Simplex[Index].Score) then
        begin
        MoveValid(Index,j); {create empty space by moving contents forward}
        StoreOk:= True;
        end
      else
        begin
        if (not Simplex[Index].Valid) and Simplex[j].Valid and (NewScore>Simplex[j].Score) then
          MoveValid(j,Index); {move empty space forward}
          BubbleStore(NewScore,NewVertex,j);
        end;
      end {index<nDim}
    else
      with Simplex[Index] do StoreOk:= (not Valid) or (NewScore<=Score);
    if StoreOk then
      StoreSimplex(Index,NewScore,NewVertex);
    end; {bubblestore}


    procedure BubbleReplace(NewScore     :TaVertexDataType;
                            var NewVertex:TaFunctionVertex;
                            Bubble       :Word);
    begin
    with Simplex[Bubble] do
      begin
      Valid:= False;
      Tag  := False;
      Score:= -1;
      Finalize(Vertex);
      end;
    BubbleStore(NewScore,NewVertex);
    end; {bubblereplace}

    {17/12/2015 make check on fIllegalErrorFunValue more sensitive}
    {20/12/2015 repaired loop}
    {24/12/2015 accomodate both small and large fIllegalErrorFunValue}
    {14/04/2018 limitvertex}
    procedure Calc_Vertex(NMparameter        :TaVertexDataType;
                          var Vertex1,Vertex2:TaFunctionVertex;
                          var NewScore       :TaVertexDataType;
                          var NewVertex      :TaFunctionVertex);
    var i,j: Word;
        b  : Boolean;
    begin
    for i:= 0 to LastDim do
      NewVertex[i]:= Vertex1[i]+NMparameter*(Vertex1[i]-Vertex2[i]);
    fSharedData.LimitVertex(NewVertex);
    j:= 0;
    with fSharedData do
      repeat
        Inc(j);
        NewScore:= Evaluate(NewVertex);
        b       := IsLegalValue(NewScore);
        if not b then
          begin
          RandomChange(NewVertex);
          LimitVertex(NewVertex);
          end;
      until b or (j>=fIllegalErrorRetries);
    if not b then
      begin
      NewScore := Simplex[0].Score*2;
      NewVertex:= Copy(Vertex1);
      end;
    end;

  {17/12/2015 make check on IllegalErrorFunValue more sensitive}
  {24/12/2015 accomodate both small and large IllegalErrorFunValue}
  {06/11/2017 safety precaution for empty vertex}
  function FillSimplex(var AVertex:TaFunctionVertex): Boolean;
  var s: TaVertexDataType;
      i: Word;
  begin
  i:= 0;
  with fSharedData do
    begin
    if Length(AVertex)<>fDim then
      AVertex:= Copy(SavedVertex);
    repeat
      Inc(i);
      s     := Evaluate(AVertex);
      Result:= IsLegalValue(s);
      if not Result then
        begin
        RandomChange(AVertex);
        LimitVertex(AVertex);
        end;
    until Result or (i>=fIllegalErrorRetries);
    if Result then
      BubbleStore(s,AVertex);
    end;
  end; {~fillsimplex}

begin
fLocker.BeginWrite;
Inc(fSharedData.fActiveCnt);
fLocker.EndWrite;
Start         := Now;
x             := 0;
ReflectedScore:= 0;
BScore        := 0;
tmpScore      := 0;
LastAliveCheck:= 0;
if fAmoebeID>=0 then with fSharedData do
  begin
  Dim    := fDim;
  LastDim:= Dim-1;
  SetLength(Simplex        ,Succ(Dim)); {simplex has one extra vertex}
  SetLength(GravityVertex  ,Dim);
  SetLength(ReflectedVertex,Dim);
  Setlength(tmpVertex      ,Dim);
  Setlength(SavedVertex    ,Dim);
  Setlength(BVertex        ,Dim);
  if Length(fNMP^.BestVertex)<>fDim then
    SetLength(fNMP^.BestVertex,Dim);
  fNMP^.FitValid:= False;
  for i:= 0 to LastDim do
    fNMP^.FitValid:= fNMP^.FitValid or (fNMP^.BestVertex[i]<>0);
  SavedVertex:= Copy(fNMP^.BestVertex);
  while (fNMP^.Restarts<=fMaxRestarts) and (fNMP^.Seconds<fMaxSeconds) and fNMP^.FitValid do
    begin
    for i:= 0 to Dim do with Simplex[i] do                                      //vertex size is fDim+1
      begin
      Score := -1;
      Valid := False;
      if assigned(Vertex) then
        Finalize(Vertex);
      end;
    j:= 0;
    repeat
      Inc(j);
      fNMP^.FitValid:= FillSimplex(fNMP^.BestVertex);
      if Length(fNMP^.BestVertex)<>Dim then
        fNMP^.BestVertex:= Copy(SavedVertex);
      for i:= 0 to LastDim do if fNMP^.FitValid then
        begin
        x                  := fNMP^.BestVertex[i];
        RandomChange(fNMP^.BestVertex,i);
        fNMP^.FitValid     := FillSimplex(fNMP^.BestVertex);
        fNMP^.BestVertex[i]:= x;
        end;
    until fNMP^.FitValid or (j>=fIllegalErrorRetries);
    fLastCallBackTime  := 0;
    fLastCallBackScore := Simplex[0].Score;
    NMCycles           := fNMP^.Cycles;
    CumulativeMaxCycles:= fMaxCycles+NMCycles;
    while (NMCycles<CumulativeMaxCycles) and (fNMP^.Seconds<fMaxSeconds) and fNMP^.FitValid do
      begin
      fNMP^.BestScore:= Simplex[0].Score;
      for i:= 0 to LastDim do
        begin  {calculation of center of gravity for nDim best vertices j in each dimension i}
        GravityVertex[i]:= 0;
        for j:= 0 to LastDim do GravityVertex[i]:= GravityVertex[i]+Simplex[j].Vertex[i];
        GravityVertex[i]:= GravityVertex[i]/fDim;
        end;
      LimitVertex(GravityVertex);
      Calc_Vertex(fNMpar[NMreflection],GravityVertex,Simplex[fDim].Vertex,ReflectedScore,ReflectedVertex);
      if WithinRange(ReflectedScore,Simplex[0].Score,Simplex[LastDim].Score,True) then  {reflection}
        begin {within range of best and second worst}
        Inc(fNMP^.NMsteps[NMreflection]);
        BubbleReplace(ReflectedScore,ReflectedVertex,fDim);             {replace worst}
        end
      else if (ReflectedScore<Simplex[0].Score) then
        begin                                                           {expansion}
        Calc_Vertex(fNMpar[NMexpansion],GravityVertex,Simplex[fDim].Vertex,BScore,BVertex);
        if (BScore<ReflectedScore) then
          begin
          Inc(fNMP^.NMsteps[NMexpansion]);
          BubbleReplace(BScore,BVertex,fDim); {replace worst}
          end
        else
          begin {reflection is still better}
          Inc(fNMP^.NMsteps[NMreflection]);
          BubbleReplace(ReflectedScore,ReflectedVertex,fDim); {replace worst}
          end;
        end
      else {reflectionscore>=Simplex[nDim]}
        begin                                                            {contraction}
        Calc_Vertex(fNMpar[NMcontraction],GravityVertex,Simplex[fDim].Vertex,x,tmpVertex);
        if x<Simplex[fDim].Score then
          begin
          Inc(fNMP^.NMsteps[NMcontraction]);
          BubbleReplace(x,tmpVertex,fDim); {replace worst}
          end
        else
          begin                                                          {reduction}
          Inc(fNMP^.NMsteps[NMshrinkage]);
          for i:= 1 to fDim do Simplex[i].Tag:= True;
          BVertex:= Copy(Simplex[0].Vertex);
          for i:= 1 to fDim do
            begin
            j:= 0;
            while not Simplex[j].Tag do Inc(j);
            Calc_Vertex(-fNMpar[NMshrinkage],BVertex,Simplex[j].Vertex,tmpScore,tmpVertex);
            BubbleReplace(tmpScore,tmpVertex,j); {replace j}
            end;
          end;
        end;
      Inc(NMCycles);
      ChangeFraction:= Abs(Simplex[0].Score/ifthen(fNMP^.BestScore=0,1,fNMP^.BestScore)-1);
      fNMP^.Cycles  := NMCycles;
      fNMP^.FitValid:= (ChangeFraction=0) or (ChangeFraction>fMinScoreChange);
      fNMP^.Seconds := fCumulativeSeconds+Math.Max(1e-8,SecondSpan(Now,Start));
      if fCallBackMode then
         begin
         fNMP^.BestVertex:= Copy(Simplex[0].Vertex);
         CallBack;
         end;
      if fCollectAllReports then
        AddLoopReport;
      if assigned(Simplex[0].Vertex) then
        SavedVertex:= Copy(Simplex[0].Vertex);
      if (fNMP^.Seconds-LastAliveCheck>0.5) then
        begin
        LastAliveCheck:= fNMP^.Seconds;
        fLocker.BeginRead;
        if not fAlive then
          fNMP^.FitValid:= False;
        fLocker.EndRead;
        end;
      end; {while l}
    Inc(fNMP^.Restarts);
    fNMP^.BestScore:= Simplex[0].Score;
    if assigned(Simplex[0].Vertex) then fNMP^.BestVertex:= Copy(Simplex[0].Vertex)
    else                                fNMP^.BestVertex:= Copy(SavedVertex);
    end; {while restarts}
  Dec(fNMP^.Restarts);
  for i:= 0 to Dim do Finalize(Simplex[i].Vertex);
  Finalize(Simplex);
  Finalize(tmpVertex);
  Finalize(BVertex);
  Finalize(GravityVertex);
  Finalize(SavedVertex);
  Finalize(ReflectedVertex);
  end;
fLocker.BeginWrite;
Dec(fSharedData.fActiveCnt);
fLocker.EndWrite;
end; {~crawl}


{03/02/2021}
destructor TaNMSimplexAmoebe.Destroy;
begin
fSharedData:= nil;
fLocker.Free;
Inherited;
end; {~destroy}


//--------TaNMSimplexAmoebeThread--------------------------------


{03/02/2021}
{09/02/2021 CollectAllReports}
constructor TaNMSimplexAmoebeThread.Create(SharedObject      : TaNMSimplexShared;
                                           AmoebeID          : Integer;
                                           CallBackAmoebes   : Integer=0;
                                           CollectAllReports : Boolean=False;
                                           SetFreeOnTerminate: Boolean=True);
begin
Inherited Create(False); {create unsuspended}
FreeOnTerminate:= SetFreeOnTerminate;
Priority       := tpHigher;
fAmoebe        := TaNMSimplexAmoebe.Create(SharedObject,AmoebeID,CallBackAmoebes,CollectAllReports);
end; {~create}


procedure TaNMSimplexAmoebeThread.Execute;
begin
fAmoebe.Crawl;
end; {~execute}


{03/02/2021}
destructor TaNMSimplexAmoebeThread.Destroy;
begin
fAmoebe.Free;
Inherited;
end; {~destroy}


//--------TaNMsimplex--------------------------------


{24/12/2015}
{04/02/2021 redesign}
constructor TaNMsimplex.Create(AErrorObjFunction   :TaMultParamObjFunction;
                               ErrFunNumParameters :Word;
                               NumberOfAmoebes     :Word            =1;
                               IllegalErrorFunValue:TaVertexDataType=1e-10;
                               DefaultLowerLimit   :TaVertexDataType=-1e25;
                               DefaultUpperLimit   :TaVertexDataType= 1e25);
begin
Initialize(NumberOfAmoebes,AErrorObjFunction,nil,ErrFunNumParameters);
ResultData.fIllegalErrorFunValue:= IllegalErrorFunValue;
InitLimits(DefaultLowerLimit,DefaultUpperLimit);
end; {~create}


{24/12/2015}
{04/02/2021 redesign}
constructor TaNMsimplex.Create(AErrorFunction      :TaMultParamFunction;
                               ErrFunNumParameters :Word;
                               NumberOfAmoebes     :Word            =1;
                               IllegalErrorFunValue:TaVertexDataType=1e-10;
                               DefaultLowerLimit   :TaVertexDataType=-1e25;
                               DefaultUpperLimit   :TaVertexDataType= 1e25);
begin
Initialize(NumberOfAmoebes,nil,AErrorFunction,ErrFunNumParameters);
ResultData.fIllegalErrorFunValue:= IllegalErrorFunValue;
InitLimits(DefaultLowerLimit,DefaultUpperLimit);
end; {~create}


{24/12/2015}
{04/02/2021 redesign}
procedure TaNMsimplex.Initialize(NumberOfAmoebes :Word;
                                 ErrObjFunction  :TaMultParamObjFunction;
                                 ErrFunction     :TaMultParamFunction;
                                 ErrFunParameters:Word);
begin
fLocker   := TMultiReadExclusiveWriteSynchronizer.Create;
ResultData:= TaNMSimplexShared.Create;
with ResultData do
  begin
  fAlive               := False;
  Amoebes              := Max(1,NumberOfAmoebes);
  fDim                 := ErrFunParameters;
  fLastDim             := Max(0,Pred(fDim));
  fMaxRestarts         := Ceil(fDim div 2);
  fMaxSeconds          := Power(2,fDim);
  fErrorFunction       :=  ErrFunction;
  fErrorObjFunction    := ErrObjFunction;
  fLoopCallBack        := nil;
  fLoopCallBackObj     := nil;
  fTimeCallBack        := nil;
  fTimeCallBackObj     := nil;
  fCumulativeSeconds   :=  0;
  fCallBackDifference  :=  0.01;
  fIllegalErrorRetries := 10;
  fMinScoreChange      :=  0;
  fRandomChangeFraction:=  0.3;
  fActiveCnt           :=  0;
  if fIllegalErrorFunValue=MinDouble then
    fIllegalErrorFunValue:= 1e-100
  else if fIllegalErrorFunValue=MaxDouble then
    fIllegalErrorFunValue:= 1e100;
  if           fIllegalErrorFunValue=0 then fIllegalTest:= illZero
  else if abs(fIllegalErrorFunValue)<1 then fIllegalTest:= illSmall
  else                                      fIllegalTest:= illLarge;
  end;
SetMaxCycles;
Adaptive              := True; {sets also defaults for fNM for dead amoebe}
//SetLength(fNMReportList,0         );
//SetLength(fLoopReports,fAmoebes);
//ClearLoopReports;
end; {~initialize}


{14/04/2018}
{04/02/2021 redesign}
procedure TaNMsimplex.InitLimits(DefaultLow  :TaVertexDataType= -1e25;
                                 DefaultHigh :TaVertexDataType=  1e25;
                                 DefaultSmall:TAVertexDataType=  1e-15);
var i: Integer;
begin
with ResultData do
  begin
  SetLength(fSmallLimits,fDim);
  SetLength(fLowerLimits,fDim);
  SetLength(fUpperLimits,fDim);
  for i:= 0 to fLastDim do
    begin
    fSmallLimits[i]:= DefaultSmall;
    fLowerLimits[i]:= DefaultLow;
    fUpperLimits[i]:= DefaultHigh;
    end;
  end;
end; {~initlimits}


procedure TaNMSimplex.SetMaxCycles(Cycles:LongWord=0);
begin
with ResultData do
  fMaxCycles:= ifthen(Cycles=0,100+Round(Power(3,fDim)),Cycles);
end; {~setmaxcycles}


{26/05/2020 better default}
procedure TaNMsimplex.SetNMSmall(Index :Word;
                                 AValue:TaVertexDataType=1e-15);
begin
with ResultData do
 if InRange(Index,0,fLastDim) then
   fSmallLimits[Index]:= Abs(AValue);
end; {~setnmsmall}


{26/05/2020 better default}
procedure TaNMsimplex.SetNMLower(Index :Word;
                                 AValue:TaVertexDataType=-1e25);
begin
with ResultData do
 if InRange(Index,0,fLastDim) then
  fLowerLimits[Index]:= AValue;
end; {~setnmlower}


{26/05/2020 better default}
procedure TaNMsimplex.SetNMUpper(Index :Word;
                                 AValue:TaVertexDataType=1e25);
begin
with ResultData do
 if InRange(Index,0,fLastDim) then
  fUpperLimits[Index]:= AValue;
end; {~setnmupper}


function TaNMSimplex.GetfNM(Index:NelderMeadParam): TaVertexDataType;
begin
Result:= ResultData.fNMpar[Index];
end; {~getfnm}


{08/02/2021}
function TaNMSimplex.GetLoopReportPtr(Amoebe,Idx:Word): NMReportPointer;
begin
Result:= nil;
with ResultData do
  if (Amoebe<Length(fLoopReports)) and (Idx<Length(fLoopReports[Amoebe])) then
    Result:= @fLoopReports[Amoebe,Idx];
end; {~getloopreportptr}


procedure TaNMSimplex.SetfNM(Index :NelderMeadParam;
                             AValue:TaVertexDataType=0);
var i: Word;
begin
with ResultData do
  begin
  i:= ifthen(Adaptive,fDim,2);
  case Index of
    NMreflection : fNMpar[NMreflection ]:= ifthen(AValue>0               ,AValue,1);
    NMcontraction: fNMpar[NMcontraction]:= ifthen(WithinRange(AValue,0,1),AValue,0.75-0.5/i);
    NMexpansion  : fNMpar[NMexpansion  ]:= ifthen(AValue>0               ,AValue,1+2/i);
    NMshrinkage  : fNMpar[NMshrinkage  ]:= ifthen(WithinRange(AValue,0,1),AValue,1-1/i);
   end;
 end;
end; {~setfnm}


{04/02/2021}
function TaNMsimplex.GetAlive: Boolean;
begin
fLocker.BeginRead;
Result:= ResultData.fAlive;
fLocker.EndRead;
end; {~getalive}


{04/02/2021}
procedure TaNMsimplex.SetAlive(AState:Boolean);
begin
fLocker.BeginRead;
with ResultData do
 if AState<>fAlive then
   begin
   fLocker.BeginWrite;
   fAlive:= AState;
   fLocker.EndWrite;
   end;
fLocker.EndRead;
end; {~setalive}


procedure TaNMsimplex.SetAdaptive(AdaptiveMode:Boolean);
var k: NelderMeadParam;
begin
with ResultData do
  if not fAlive then
    begin
    fAdaptive:= AdaptiveMode;
    for k:= NMreflection to NMshrinkage do SetfNM(k); {set defaults}
    end;
end; {~setadaptive}


{07/02/2021}
procedure TaNMsimplex.InitReport(var AVertex:TaFunctionVertex);
var i: Word;
begin
with ResultData,CrawlReport do
  begin
  FitValid  := Length(AVertex)=fDim;
  BestVertex:= Copy(AVertex);
  Restarts  := -1;
  Seconds   := 0;
  i         := 10;
  if FitValid then
    repeat
      Dec(i);
      BestScore := Evaluate(BestVertex);
      FitValid  := IsLegalValue(BestScore);
      if not FitValid then
        RandomChange(BestVertex);
    until FitValid or (i=0);
  fAlive:= FitValid;
  end;
end; {~initreport}


{22/07/2017}
{12/09/2017 no time reporting here}
{09/02/2021 added amoebeid}
procedure TaNMSimplex.AddToReportList(var ACrawlReport:NMReportRecord);
var k: NelderMeadParam;
begin
with ACrawlReport do
  begin
  if FitValid and assigned(BestVertex) and (BestScore<CrawlReport.BestScore) then
    begin
    CrawlReport.BestScore := BestScore;
    CrawlReport.BestVertex:= Copy(BestVertex);
    CrawlReport.AmoebeID  := AmoebeID;
    end;
  CrawlReport.Cycles:= CrawlReport.Cycles+Cycles;
  for k:= NMreflection to NMshrinkage do
    Inc(CrawlReport.NMsteps[k],NMsteps[k]);
  Finalize(BestVertex);
  end;
end; {~addtoreportlist}


(*
Direct access of an shared object through a thread should be avoided.
It will lead to memory leaks because of failing memory disposal of the thread-object.

Delphi syntax:
procedure Execute; virtual; abstract;

Description
Override Execute and insert the code that should be executed when the thread runs.
Execute is responsible for checking the value of the Terminated property to determine if the thread needs to exit.
A thread executes when Create is called if CreateSuspended set to false, or when Resume is first called after the thread is created if CreateSuspended set to true.
Note:	Do not use the properties and methods of other objects directly in the Execute method of a thread.
Instead, separate the use of other objects into a separate procedure call, and call that procedure by passing it as a parameter to the Synchronize method.

https://wiki.freepascal.org/User_Changes_2.4.4#TThread.Suspend_and_TThread.Resume_have_been_deprecated
*)
{12/01/2016 fSavedVertex guarantees a technical valid result}
{26/06/2017 fAlive must be true (all vectors are valid) to enter the main loop}
{13/07/2017 limits}
{20/07/2017}
{21/07/2017 NumCPU}
{22/07/2017 Terminated in loop}
{25/07/2017 stable version}
{27/07/2017 fNMCrawlReport.restarts instead of local variable in loop
            overall time reporting}
{04/08/2017 try to get valid bestvertex on start}
{12/09/2017 improved time reporting}
{08/05/2020 callbacks reduced}
{09/02/2021 callback mechanism reviewed}
{$push}{$warn 5091 off: AmoebeThreadList not initialised}
procedure TaNMsimplex.StartAmoebe(AVertex          : TaFunctionVertex;
                                  CumulativeSeconds: Single =0;
                                  CallBackAmoebes  : Integer=0;
                                  ReportAllAmoebes : Boolean=False);
var i,j              : Integer;
    Start            : TDateTime;
    AmoebeThreadList : array of TaNMSimplexAmoebeThread;
    RunTime,BreakTime: Double;
begin
with ResultData do
  if Amoebes<=1 then
    SingleAmoebe(AVertex,CumulativeSeconds,CallBackAmoebes)
  else if not Alive then
    begin
    InitReport(AVertex);
    SetLength(AmoebeThreadList,Amoebes);
    Start    := Now;
    BreakTime:= 2*Math.Max(2,fMaxSeconds);
    with CrawlReport do
      begin
      ClearReportLists;
      while (Restarts<fMaxRestarts) and FitValid do
        begin
        Inc(Restarts);
        fCumulativeSeconds:= Abs(CumulativeSeconds);
        for i:= 0 to Pred(Amoebes) do                                           //create zero-based list of amoebes
          begin                                                                 //restart is set to maxrestarts to avoid indiviual restarts
          ResultData.InitializeCrawlList(BestVertex,BestScore,i,fMaxRestarts);  //main thread creates report for every thread}
          AmoebeThreadList[i]:= TaNMSimplexAmoebeThread.Create(ResultData,i,CallBackAmoebes,ReportAllAmoebes);  //freeonterminate=true
          end;
        repeat
          Sleep(10);
          Application.ProcessMessages;
          RunTime:= SecondSpan(Now,Start);
          j      := fActiveCnt;
          if RunTime>BreakTime then
            StopAmoebe;
          ProcessReports;                                                       //foor all reports generated in threads
        until (j=0) or (RunTime>5*BreakTime);
        i:= Amoebes;
        while i>0 do
          begin
          Dec(i);
          AmoebeThreadList[i]:= nil;
          AddToReportList(ResultData.fCrawlReportList[i]);                      //copy bestvertex to CrawlReport when bestscore improves
          end;
        end; {while restarts}
      Seconds:= fCumulativeSeconds+SecondSpan(Now,Start);                       //report time of startamoebe
      end; {with}
    SetLength(AmoebeThreadList,0);
    end;
end; {~startamoebe}
{$pop}

procedure TaNMsimplex.SingleAmoebe(AVertex          :TaFunctionVertex;
                                   CumulativeSeconds:Single =0;
                                   CallBackAmoebes  :Integer=0);
var j                : Integer;
    Start            : TDateTime;
    Amoebe           : TaNMSimplexAmoebe;
    RunTime,BreakTime: Double;
    begin
with ResultData do if not Alive then
  begin
  InitReport(AVertex);
  Amoebes  := 1;                                                                //zero-based counting
  Start    := Now;
  BreakTime:= 2*Math.Max(2,fMaxSeconds);
  with CrawlReport do
    begin
    while (Restarts<fMaxRestarts) and FitValid do
      begin
      Inc(Restarts);
      fCumulativeSeconds:= Abs(CumulativeSeconds);
      ResultData.InitializeCrawlList(BestVertex,BestScore,0,fMaxRestarts);      //main thread creates report for every thread}
      Amoebe:= TaNMSimplexAmoebe.Create(ResultData,0,CallBackAmoebes);
      Amoebe.Crawl;
      repeat
        Sleep(10);
        RunTime:= SecondSpan(Now,Start);
        fLocker.Beginread;
        j      := fActiveCnt;
        fLocker.Endread;
        if RunTime>BreakTime then
           StopAmoebe;
      until (j=0) or (RunTime>5*BreakTime);
      AddToReportList(ResultData.fCrawlReportList[0]);                          //copy bestvertex to fNMCrawlReport when bestscore improves
      end; {while restarts}
    Seconds:= fCumulativeSeconds+SecondSpan(Now,Start);                         //report time of startamoebe
    end; {with}
  end;
end; {~singleamoebe}


procedure TaNMsimplex.StopAmoebe;
begin
Alive:= False;
end; {~stopamoebe}


destructor TaNMsimplex.Destroy;
begin
fLocker.Free;
ResultData.Free;
Finalize(fBestSimplex); {length should be fdim+1}
Inherited;
end; {~destroy}


//-----procedures-and functions------------------------------------------------

function WithinRange(AValue,Limit1,Limit2:TaVertexDataType;
                     Limit1Inclusive     :Boolean=False;
                     Limit2Inclusive     :Boolean=False ): Boolean;
begin
Result:= ((AValue>Limit1) or (Limit1Inclusive and (AValue=Limit1))) and
         ((AValue<Limit2) or (Limit2Inclusive and (AValue=Limit2)));
end; {withinrange}





end.

