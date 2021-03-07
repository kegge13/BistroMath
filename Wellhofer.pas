unit Wellhofer;  {© Theo van Soest Delphi: 01/08/2005-05/06/2020 | FPC 3.2.0: 07/03/2021}
{$mode objfpc}{$h+}
{$I BistroMath_opt.inc}

(*
=================================================================================
 This library is original work of Theo van Soest.
 It is published under the GNU Lesser General Public License v3 (LGPL-3.0).
 https://tldrlegal.com/license/gnu-lesser-general-public-license-v3-%28lgpl-3%29

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.

=================================================================================

This library can read at least 15 different data formats and represents
a wide range of profile data types in radiotherapy.
All functions are designed to accomodate for non-equidistant ("irregular") data sets.

=================================================================================
*)


(*-------------------------------------------------------------------------------
TWellhoferData=class(TRadthData) is the main object class in this unit. It is instantiated by the GUI.

 There are separate objects for most file formats; only the omnipro .txt and .rfb format as well as the sun nuclear data format
 are read directly by TWellhoferData.

 A further complication is that:
  a) different vendors or even different versions for one vendor use different directions for the GT axis,
  b) the presentation of the GT-axis to the user should be configurable to comply with the scanning software,
  c) effects of rotation of the water phantom should be supported,
  d) the internal data representation should be made consistent at all times.
  e) irregular data sets must be supported.

 TWellhoferData holds a series of identical data structures:
    wSource: array[twcDataSource] of twCurveDataRec;
	  twcDataSource     =twcFirstDataSource..twcLastDataSource;
	  twcFirstDataSource= dsMeasured;
	  twcLastDataSource = dsUnrelated;
          twAnyDataSource   =(dsMeasured,dsReference,dsMeasFiltered,dsRefFiltered,dsCalculated,dsBuffer,dsRefOrg,dsUnrelated,dsDefault);

  As filtered versions of the raw data are used intensively, it is efficient to keep filtered versions available at all times.
  The burden of this strategy is to keep the fileterd version updated at all times.

  The central concept of Bistromath is to compare a measurement with a reference.
  Loading a reference is complicated because the reference
   a) may be already available in memory,
   b) the file name may be different in older version of BistroMath,
   c) can be part of a structured series from a 2D-array,
   d) may be part of an unstructured multi-scan file.

 Core functions:
  function  LoadReference(AFileName             :String      ='';
                          SetCurrentAsRefSource :Boolean     =False       ): Boolean;
  function  PrepareProfile                                                 : Boolean;
  function  NearestPosition(Position            :twFloatType;
                            ASource             :twcDataSource=dsMeasured;
                            ForceAlwaysIn       :Boolean=True             ): Integer;
  function  GetInterpolatedValue(Position       :twFloatType;
                                 ASource        :twcDataSource=dsMeasured;
                                 DefaultValue   :twFloatType=0            ): twFloatType;
  function  GetQfittedValue(Position            :twFloatType;
                            ASource             :twcDataSource=dsMeasured;
                            DefaultValue        :twFloatType=0            ): twFloatType;
  function  GetScaledQfValue(Position           :twFloatType;
                             RelativeToCenter   :Boolean;
                             Scaling            :twScalingType;
                             ASource            :twcDataSource=dsMeasured ): twFloatType;
  function  FindLevelPos(ASource                :twcDataSource=dsMeasured;
                         ALevel                 :twDoseLevel=d50;
                         Symmetric              :Boolean=True             ): Boolean;
  function  FindEdge(ASource                    :twcDataSource=dsMeasured ): Boolean;
  procedure FastScan(ASource                    :twcDataSource=dsMeasured );
  function  Analyse(ASource                     :twcDataSource=dsMeasured;
                    AutoCenterProfile           :twAutoCenter=AC_default  ): Boolean;
---------------------------------------------------------------------------------------
READING DATA - READING DATA - READING DATA - READING DATA - READING DATA - READING DATA

Most data types have their objecttype, all derived from TRadthData.
Also TWellhoferData, the main analysis engine is derived from TRadthData.
In the TWellhoferData.DualReadData function an object to everry known type is opened as far as needed.
To be as fast as possible TWellhoferData itself can find out the file type:

  function TWellhoferData.EvaluateFileType(AIndentString:String=''): twcFileType;
  begin
  if Length(AIndentString)=0 then
    AIndentString:= IdentificationStg;
  if      Pos(rfaID        ,AIndentString)=1 then Result:= twcRFA_ascii
  else if Pos(eclipseID    ,AIndentString)=4 then Result:= twcEclipse
  else if Pos(mccID        ,AIndentString)=1 then Result:= twcMccProfile
  else if Pos(wICPAIdentStg,AIndentString)=1 then Result:= twcICprofilerAscii
  else if Pos(hdfID        ,AIndentString)=1 then Result:= twcHdfProfile
  else if Pos(xioID        ,AIndentString)=1 then Result:= twcCmsProfile
  else if Pos(w2ID         ,AIndentString)>0 then Result:= twcW2CAD
  else                                            Result:= twcUnknown;
  end;


For completeness each ascendent also must a GetFileType function.
It is very convenient that the only two known binary data type are handled by TWellhoferData itself.
Therefore the high level object can sort this out:


  function TWellhoferData.IsBinary(AFileName:String=''): Boolean;
  var t: twcFileType;
  begin
  t:= GetFileType(AFileName,True);
  Result:= (t in [twcWellhoferRfb,twcWDA]);
  if Result then
    BinStreamType:= t;
  end; {~isbinary}

In the user interface (TWellForm.DataFileOpen) this is exploited by first testing on binary types,
and when not, transferring the data to a TStringStream and go from there.

---

TRadthData.ReadData sets the scene for the reading strategy.
All in all there is a lot of object context jumping.

  function TRadthData.ReadData(AStream    :TStream;
                               AFileFormat:twcFileType=twcUnknown): Boolean;
  begin
  SetDefaults;
  FileFormat:= AFileFormat;

  //*************  if stream of base type TStream  ************
  if not (AStream is TStringStream) then


    begin
    if AStream.Size>0 then
      BinStream.CopyFrom(AStream,0);

  //*************  ascendants must reintroduce ReadBinData if applicable  ************
    Result:= ReadBindata;


  end
  else
    begin
    if (FLocalParser or (Parser.LineCount=0)) and (AStream is TStringStream) then
      begin
      AStream.Position:= 0;


  //*************  the stream is copied to a string stream  ************
      Parser.Assign(TStringStream(AStream));


      end;

  //*************  ascendants must reintroduce ParseData if applicable  ************
    Result:= ParseData;

    end;
  end; {~readdata}

--

  //*************  ascendants must reintroduce ParseData if applicable and call the inherited function first  ************
  function TRadthData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
  begin
  Result:= CheckData(nil) or CheckFileTypeOnly;
  if Length(Parser.FileName)>0 then
    FileName:= Parser.FileName;
  end; {~parsedata}

--

  //*************  ascendants must reintroduce CheckData if applicable and call the inherited function first  ************
  function TRadthData.CheckData(AStringList:TStrings): Boolean;
  var i: Integer;
  begin
  if assigned(AStringList) then
    begin
    i                := AStringList.Count;
    IdentificationStg:= AStringList.Strings[0];
    end
  else with FParser do
    begin
    i:= LineCount;
    GotoTop(True,False);

  //*************  for most text data types the first line is the needed IdentificationStg  ************
    IdentificationStg:= CurrentLine;

    end;

  //*************  assuming each point takes a line, we can set a minimum required number of lines  ************
  Result:= i>=twcDefMinProfilePoints;

  end; {~checkdata}

*)


interface

uses Types,Classes,Math,
     TOmath,TONelderMead,TOnumparser,TObaseDef,TOconfigStrings;

{imported version D7 dd. 20/12/2018 from here}

const
  twcWellhoferKey          ='WellhoferObject';
  twcRefDirKey             ='RefDir';
  twcRefDirDefaultName     ='references';
  twcMultiScanKey          ='multiscan_';
  twcSymCorrLimitKey       ='SymmetryCorrection';
  twcFilterKey             ='Filter';
  twcEnergyKey             ='Energy';
  twcSSDKey                ='SSD';
  twcGridKey               ='Grid';
  twcCalcKey               ='Norm';
  twcPenumbraKey           ='Penumbra';
  twcDrefKey               ='WellhoferRefDepths';
  twcDCKey                 ='WellhoferOD2dose';
  twcDRefBeams             ='WellhoferGenericBeams';
  twcLinacRadiusKey        ='BeamRadius';
  twcWMSdetKey             ='WMSdetInfo';
  twcMeasAxisStandard      ='ICD';
  twcTankAxisStandard      ='XYZ';
  twcMeasAxisPermutations  =twcMeasAxisStandard+',IDC,CID,CDI,DIC,DCI';
  twcDefUnknown            ='-';
 {$IFDEF DISCRETE_FIXED_DISTANCE}
  twcDefDiscretisationMult =1000000;
 {$ENDIF DISCRETE_FIXED_DISTANCE}
  twcDefMinDataLines       =  5;
  twcDefMinProfilePoints   = 10;
  twcDefMinScanLength      =  1.0;
  twcDefAccNormDif         =  0.05;
  twcMaxLogisticRange      =  0.15;
  twcDefAccDivFactor       =  2;
  twcDefaultValue          = -1;
  twcDefPipsPixelCm        =  0.025;
  twcDefSchusterPixelCm    =  0.5;
  twcDefSchusterPoints     = 88;
  twcDefENRblocks          = 25;
  twcDefSchusterDelimiters = ';#';
  twcOD2doseNumPar         =  6;
  twcModalityFormat        = '%s%0.2f';
  twcModalitySet           = ['C'..'X'];
  twcMultiRefMaxScan       = 4;
  twcMedianPointsPerThread = 50;
  twcSamePositionRadiusCm  = 0.02;
  twcDefSigmoidRadiusCm    = 1;
  twcDefMinFilterWidthCm   = 0.01;
  twcDefEdgeRangeCm        = 2;
  pddfit_I1                = 0;              {This ordering is important when skipping the build-up part}
  pddfit_mu1               = 1;
  pddfit_mu2               = 2;
  pddfit_mu3               = 3;
  pddfit_mu4               = 4;
  pddfit_Ib                = 5;
  pddfit_mub               = 6;
  pddfit_mubpower          = 7;
  pddfit_X_c               = 2;
  pddfitOdim               = pddfit_mu4+1;   { 5, low-energy photons}
  pddfitPdim               = pddfit_mub+1;   { 7, photons, may be expanded with pddfit_mubpower}
  pddfitXdim               = pddfit_X_c+1;   { 3, for extrapolation of photons}
  pddfit_I0                = pddfit_mub+1;   { 7}
  pddfit_d0                = pddfit_mub+2;   { 8}
  pddfit_Ix                = pddfit_mub+3;   { 9}
  pddfit_mx1               = pddfit_mub+4;   {10}
  pddfit_mx2               = pddfit_mub+5;   {11}
  pddfitEdim               = pddfit_mub+6;   {12, electrons}
  sigmoid_LowVal           = 0;
  sigmoid_InflectionMajor  = 1;
  sigmoid_Slope            = 2;
  sigmoid_HighVal          = 3;
  SigmoidDim               = sigmoid_HighVal+1;
  fitCalcErrorDef          = 1e10;

type
  fitNameArray            = array[0..pddfit_mx2] of String[7];
  fitNormArray            = array[0..pddfit_mx2] of Boolean;
const
  pddfitPnames: array[pddfit_I1..pddfit_mubpower] of String[7]=('I1' ,'mu1','mu2','mu3','mu4','Ib','mub','b_pwr');
  pddfitPnorm : array[pddfit_I1..pddfit_mubpower] of Boolean  =(True ,False,False,False,False,True,False,False);
  pddfitXnames: array[pddfit_I1..pddfit_X_c     ] of String[7]=('I1','mu1','c');
  pddfitXnorm : array[pddfit_I1..pddfit_X_c     ] of Boolean  =(True ,False,True);
  pddfitEnames: array[pddfit_I1..pddfit_mx2     ] of String[7]=('I1' ,'mu1','mu2','mu3','mu4','Ib' ,'mub','I0','d0' ,'Ix','mx1','mx2');
  pddfitEnorm : array[pddfit_I1..pddfit_mx2     ] of Boolean  =(False,False,False,False,False,False,False,True,False,True,False,False);

resourcestring
  twForParseError   ='Parsing error at line %d: %s%s';
  twForParseRead    ='%d points read%s';
  twForMinPoints    ='Only %d points; at least %d required.%s';
  twForBinRead1     ='Incorrect data in profile header';
  twForBinRead2     ='File read error in profile data';
  twForMinMax       ='Problem with maximum (%0.1f=%0.1f)';
  twForMinScanLen   ='Length of scan %0.1f cm; at least %0.1f cm required.';
  twForIllegalScan  ='Bi-directional or zero steps in scan at point %d.';
  twForPointError   =' -->point %d expected';
  twForUnsupported  ='Unsupported scan type: ';
  twForMatch        ='->Match cost function(%0.3f)=%0.5f [step %0.3f cm]';
  twForFileNotFound ='[ file %s not found ]';
  twForFileNotRead  ='[ file %s not loaded ]';
  twDiagonalFound   ='diagonal detected';
  twForNMreport     = 'ENR: %0.2f in %d cycles, %d restart%s, %0.1f s (amoebes: %d), max %0.2f at %0.2f cm';
  twMedianFilterStg = 'median-filtered';
  twQuadFilterStg   = 'quad-filtered';
  twGammaCalcStg    = 'gamma';
  twDerivativeStg   = 'derivative';
  twNMfitStg        = 'TvSpdd-fit';
  twLockedStg       = ' is locked';

{ ************OVERVIEW OF FILE RECOGNITION RULES*******************************

----IBA binary rfb format (as file from disk only)----
 Start with a P-type string of the version of the file.
 The string itself starts with "Version:". The first byte is the length of the string.
 In all investigated versions this was 14 ($0e). The next two bytes represent the integer type for the number of scangroups and, after a spacing of 6 bytes, the untyped string of 5 characters " CBEAM" should be detected for a valid header.

 0E 56 65 72 73 69 6F 6E 3A 36 2E 36 2E 32 36
     V  e  r  s  i  o  n  :  6  .  6  .  2  6

 01 00   FF FF   01 00 05 00  43 42 65 61 6D
 int(1) $FF$FF   (?  ?  ?  ?)  C  B  E  A  M

----IBA Wellhöfer v6----
 Start with 'Clinic:'.

----IBA Wellhöfer v7----
 Start with a date and time where both '-' and '/' are acceptable as separator in the date.
 When all digits are represented with n, the pattern 'n/nnnn nn:nn:nn' or 'n/nnnn n:nn:nn' should be found to start within the first five characters in the text.
 The AM/PM format is also supported here.

----Sun Nuclear disk file----
 Start with 'Tab-Delimited Scan Output'.

----Sun Nuclear clipboard format----
 Start with 'Delivery System'.

----WMS binary----
 Start with the binary representation of the file header size (588). The first integer in the header should be 6..

 4C 02     06 00
 int(588)  int(6)

----WMS ascii----
 Start with a double quoted file name with the extension '.wda' or '.wtx': '"xxxxxxx.wxx"' or a double quoted date string '"dd-mmm-yy"'.

----RFA ascii----
 Start with ':SYS'.

----MCC ascii----
 Start with 'BEGIN_SCAN_DATA'.

----Elekta CMS ascii----
 Start with '00001090'.

----generic ascii----
 The file should contain two colums of data values without any header, interpreted as position and dose.

 ----Varian W2CAD ascii----
  The string '$STOM' should be found somewhere in beginning of the file.
  Reads a maximum of 255 bytes to validate this.

 ----Varian Eclipse ascii----
  Start with xEFxBBxBF+'Patient Name: '

 ----HDF ascii----
 Start with '# Track:'.

----Pips ascii----
 Start with 'Type of Cross-Section:'.

----Schuster ascii----
 Start with 'Profile measured on '.

----SunNuclear ICprofiler ascii (export)----
 Start with 'Filename'

************ AXIS CONVENTION RULES ******************
For the TWellhoferData class the internal measurement axis in linac coordinates
is from G to T, from A to B and from Up to Down.

In most data types, read with TRadthData derived classes, represent the coordinates in
a linac coordinate system. Often the GT axis is inversed however. The mephysto data type
and OmniPro data types do also give the orientation of the XYZ tank coordinate system in
the header.
Most of the import data types represent the data as given in the original file format.
The appropriate TWellhoferData.ImportXXXProfile should set full 3D coordinates in the
TWellhoferData convention. The TWellhoferData.PrepareProfile procedure will extract 1D
positions for each data point from the 3D coordinates.}

{13/07/2015 Unrelated curve added
  Unrelated may keep any data and is not cleared through any routine at read time.
  It may be used to store raw reference data when not locked.}
{21/07/2015 RefOrg added.
  Separate storage space for reference original (not resampled) data}
{09/06/2016 dSigmoid added}
{22/07/2016 twDefCenterType added}
{15/11/2016 twCenterUseType added}
{03/12/2016 twIgnoreParams,twIgnoreSet added}
{27/12/2017 MeasFiltered added to datasources}
{26/01/2018 dsRefFiltered added to datasources}
{28/01/2018 twcCoupledSources}
{06/12/2018 twWellhoferAscii_v8}
{17/06/2020 dSigmoid50 added to twDoseLevel}
{21/06/2020 added twSigmoidType}
{20/07/2020 added twcFieldClass}
{21/07/2020 added fcWedge}
{18/08/2020 added fcMRlinac}
{13/10/2020 added fcElectron}
type
  twcChannels       =(FieldCh,RefCh);
  twcFieldClass     =(fcStandard,fcFFF,fcSmall,fcMRlinac,fcWedge,fcElectron);
  twcEdgeClass      =(fcPrimary,fcFallBack);
  twcFieldShape     =(Rectangular,Blocks,MLC,Circular);
  twcCenterType     =(CenterPenumbra,CenterOrigin,CenterMax);                                                                          {ordering critical for user interface}
  twcFFFPeakType    =(CenterFFFTopModel,CenterFFFSlopes);                                                                              {ordering critical for user interface}
  twcDoseLevel      =(dLow,dHigh,d20,d50,d80,d90,dUser,dDerivative,dInflection,dSigmoid50,dTemp);                                      {ordering critical within code and user interface, should be checked if changed}
  twcPositionUseType=(dUseBorder,dUseDerivative,dUseInflection,dUseSigmoid50,dUseOrigin,dUseMax,dUseFFFtop,dUseFFFslopes,dUseUndefined,dUseConfigured); {TAnalyseForm relies on this order, check all code!}
  twcNMpddFits      =(NM_Primary,NM_Extrapolation);
  twcAutoCenter     =(AC_default,AC_on,AC_off);
  twcNormalisation  =(NormOnCenter,NormOnOrigin,NormOnMax,NormOnInFieldArea);
  twcFieldSizeDesc  =(fInplane,fCrossplane);
  twcModalityChar   = 'C'..'X';
  twcShiftType      =(AbsShift,RelShift);
  twcSourceEnum     =(dsMeasured,dsReference,dsMeasFiltered,dsRefFiltered,dsCalculated,dsBuffer,dsRefOrg,dsUnrelated
                     {$IFDEF WELLHOFER_DUMPDATA},dsDefault{$ENDIF});                                                                   {order is used for coupling}
  twcTankAxis       =(X,Y,Z);
  twcMeasAxis       =(Inplane,Crossplane,Beam);
  twcScanTypes      =(snGT,snAB,snPDD,snAngle,snGenericHorizontal,snFreescan,
                      snGenericProfile,snFanLine,snPlane,snUndefined);              {order of first 3 is critical and assumed in code}
  twcScalingType    =(scNormalised,scAvgNorm,scPlotScaling,scMaximum);
  twcBeamType       =(Photons,Electrons,Protons,Other);
  twcFloatType      = Double;                                                      //was Extended in Delphi7
  twcStartStopType  =(Start,Stop);
  twcFloatArray     = array of twcFloatType;
  twcFloatArrayPtr  = ^twcFloatArray;
  twcAliasRec       = record
                       twKey,twValue: String;
                       twApplied    : Boolean;
                      end;
  twcAliasArray     = array of twcAliasRec;
  twcSides          = (twcLeft,twcRight);
  twcArrayLimit     = record
                       Calc   : twcFloatType;
                       Nearest: Integer;
                       Valid  : Boolean;
                      end;
  twcArrayLimitRec  = record
                       Level   : twcFloatType;
                       Penumbra: array[twcSides] of twcArrayLimit;
                      end;
  twcTankAxisChar   = 'X'..'Z';
  twcXYZset         = set of twcTankAxisChar;
  twcLimitsArray    = array[twcDoseLevel] of twcArrayLimitRec;

  twcOD2doseArray   = array[1..twcOD2doseNumPar] of twcFloatType;

  twcCoordinate     = record case integer of
                       0: (m:array[twcMeasAxis    ] of twcFloatType);
                       1: (t:array[twcTankAxis    ] of twcFloatType);
                       2: (c:array[twcTankAxisChar] of twcFloatType);
                       3: (i:array[0..2           ] of twcFloatType);
                      end;
  twcCoordArray     = array of twcCoordinate;

  twcFileType       = (twcWellhoferAscii_v6, twcWellhoferAscii_v7, twcWellhoferAscii_v8, twcWellhoferRfb   , twcWTX, twcWDA,
                       twcGenericProfile   , twcHdfProfile       , twcSchusterProfile  , twcCmsProfile     ,
                       twcPipsProfile      , twcMccProfile       , twcSNCfileAscii     , twcSNCclipboard   ,
                       twcRFA_ascii        , twcW2CAD            , twcEclipse          , twcICprofilerAscii, twcUnknown);

  twcGrafPoint      = record
                       X,Y: twcFloatType;
                      end;
  twcGrafProfile    = array of twcGrafPoint;

  wmsIntType        = SmallInt;                                                     {make no changes, needed for binary files, 2 bytes!!}
  wmsRealType       = Single;                                                       {make no changes, needed for binary files, 4 bytes}

  twcTankAxisID     = packed array[twcMeasAxis    ] of twcTankAxisChar;
  twcTankAxisSign   = record
                        case Boolean of
                         true : (c: packed array[twcTankAxisChar] of wmsIntType);
                         false: (t: packed array[twcTankAxis    ] of wmsIntType);
                        end;
  twcMeasAxisSign   = packed array[twcMeasAxis    ] of ShortInt;
  twcMeasAxisStg    = String[3];
  twcFieldDescrArr  = array[twcFieldSizeDesc]       of twcFloatType;
  twcGantrySetup    = (twCW_180_Down,twCCW_180_Down,twCW_180_Up,twCCW_180_Up);
  twcFitModels      = (pddPhoton,pddPhotonExtrapolation,pddElectron,penumbraSigmoid);
  twcMultiScanList  = array of String;
  twcIgnoreParams   = (twiLinac,twiModality,twiEnergy,twiSSD,twiFieldSize,twiWedge,twiDepth,twiDiagonal,twiScanDirection,twiScanClass,twiAngle,twiMeasDivice);
  twcIgnoreSet      = set of twcIgnoreParams;

const                                                                           //set definitions
  twcMultiFiles     : set of twcFileType =[twcRFA_ascii,twcMccProfile,twcWellhoferRfb,twcW2CAD,twcICprofilerAscii];
  twcHoriScans      : set of twcScanTypes=[snGT,snAB,snAngle,snGenericHorizontal];
  twcVertScans      : set of twcScanTypes=[snPDD,snFanLine];
  twcGenericFormats : set of twcFileType =[twcGenericProfile,twcHdfProfile];
  twcInherentFormats: set of twcFileType =[twcWellhoferAscii_v6,twcWellhoferAscii_v7,twcWellhoferRfb,twcSNCfileAscii,twcSNCclipboard];
  twcBinaryFormats  : set of twcFileType =[twcWDA,twcWellhoferRfb];

  twcFirstDataSource= dsMeasured;
  twcLastDataSource = dsUnrelated;
  twcLastRelated    = dsBuffer;
  twcFilterSources  = [dsMeasured    ,dsReference  ];
  twcFilteredCopies = [dsMeasFiltered,dsRefFiltered];
  twcCoupledSources :array[dsMeasured..dsReference      ] of twcSourceEnum=(dsMeasFiltered,dsRefFiltered);
  twcCoupledFiltered:array[dsMeasFiltered..dsRefFiltered] of twcSourceEnum=(dsMeasured    ,dsReference  );

{$IFDEF WELLHOFER_DUMPDATA}
var
  DumpDataFilter:set of twcSourceEnum=[dsMeasured..dsUnrelated];
{$ENDIF}

(*------modality---------------------------------------
 The TModalityList is used to store various types of information as TModalityObject
 as specialised decendandants. They can finally be presented in one unified user-interface.
*)
{06/11/2016 new implementation}
type
  TModalityObject=class(TObject)
    public
      Modality: String;
     constructor Create(AModality:String='');
     procedure   Copy(ASource:TModalityObject);                     virtual;
     destructor Destroy;                                            override;
    end;

  twModalityArr = array of TModalityObject;

  //modalities should be formatted with ModalityFormat -> twcModalityFormat  = '%s%0.2f';

  TModalityList=class(TObject)
    private
      FData      : twModalityArr;
      FStatusProc: toExtMsgProc;
     function  GetModData(Index      :Integer                 ): TModalityObject;
     function  GetModDataCount                                 : Integer;
     function  GetCommaText(Index    :Integer                 ): String;             virtual;
     function  GetDivisorText(Index  :Integer                 ): String;
     procedure ExceptMessage(AString :String                  );
    public
     constructor Create(AStatusProc    :toExtMsgProc      =nil);                     virtual;
     function  ModalityFormat(AModality:twcModalityChar;
                              AEnergy  :twcFloatType          ): String;
     function  AddModData(ACommaText :String;
                          ASeparator :Char=','                ): Integer;            virtual;
     function  DelModData(AModality  :String                  ): Boolean;  overload; virtual;
     function  DelModData(Index      :Integer                 ): Boolean;  overload; virtual;
     function  FindModData(AModality :String;
                           var RefObj:TModalityObject         ): Boolean;  overload; virtual;
     function  FindModData(AModality :String                  ): Integer;  overload;
     function  GetModalityList                                 : String;             virtual;
     procedure SetStatusProcedure(AStatusProc:toExtMsgProc=nil);
     procedure ClearModData;
     destructor Destroy;                                                   override;
     property Data[Index:Integer]       : TModalityObject read GetModData;
     property DataArray                 : twModalityArr   read FData;
     property DataCount                 : Integer         read GetModDataCount;
     property CommaText[Index:Integer]  : String          read GetCommaText;
     property DivisorText[Index:Integer]: String          read GetDivisorText;
     property StatusProcedure           :toExtMsgProc     read FStatusProc write SetStatusProcedure;
    end;

//------modalitynorm---------------------------------------
type
  twModNormRec = record
                  Depth: array[False..True] of twcFloatType;
                  Value: array[False..True] of twcFloatType;
                  end;

  TModalityNorm=class(TModalityObject)
    public
      NormRec: twModNormRec;
     constructor Create(AModality:String=''    );                               reintroduce;
     procedure   Copy(ASource    :TModalityNorm);                               reintroduce;
     destructor  Destroy;                                                       override;
    end;

  TModNormList=class(TModalityList)
    private
     function  GetCommaText(Index    :Integer          ): String;               override;
    public
     constructor Create(AStatusProc  :toExtMsgProc=nil );                       override;
     function  AddModData(ACommaText :String;
                          ASeparator :Char        =',' ): Integer;              override;
     function  FindModData(AModality :String;
                           var RefObj:TModalityNorm    ): Boolean;              reintroduce;
     function  GetModDepth(AModality :String;
                           AbsDepth  :Boolean     =True;
                           ZeroValue :twcFloatType=0   ): twcFloatType;
     function  GetModValue(AModality :String;
                           AbsValue  :Boolean     =True): twcFloatType;
     destructor Destroy;                                                        override;
    end;

//------modalityfilm---------------------------------------
type
  twModFilmRec = record
                  FilmType: String;
                  OD2dose : twcOD2doseArray;
                 end;

   TModalityFilm=class(TModalityObject)
    public
      FilmRec: twModFilmRec;
     constructor Create(AModality:String='');                                   reintroduce;
     procedure   Copy(ASource:TModalityFilm);                                   reintroduce;
     destructor Destroy;                                                        override;
    end;

  TModFilmList=class(TModalityList)
    private
     function  GetCommaText(Index    :Integer          ): String;               override;
    public
     constructor Create(AStatusProc  :toExtMsgProc=nil );                       override;
     function  AddModData(ACommaText :String;
                          ASeparator :Char        =',' ): Integer;              override;
     function  DelModData(AModality  :String;
                          AFilmType  :String           ): Boolean; reintroduce; overload;
     function  FindModData(AModality :String;
                           AFilmType :String;
                           var RefObj:TModalityFilm    ): Boolean; reintroduce; overload;
     function  FindModData(AModality :String;
                           AFilmType :String           ): Integer; reintroduce; overload;
     function  GetFilmTypeList                          : String;
     destructor Destroy;                                                        override;
    end;

//------modalitytext---------------------------------------
type
  TModalityText=class(TModalityObject)
    public
      Value: String;
     constructor Create(AModality:String='');
     procedure   Copy(ASource:TModalityText);                                   reintroduce;
     destructor Destroy;                                                        override;
    end;

  TModTextList=class(TModalityList)
    private
     function  GetCommaText(Index    :Integer         ): String;                override;
    public
     constructor Create(AStatusProc  :toExtMsgProc=nil);                        override;
     function  AddModData(ACommaText :String;
                          ASeparator :Char        =','): Integer;               override;
     function  FindModData(AModality :String;
                           var RefObj:TModalityText   ): Boolean;               reintroduce;
     function  GetModValue(AModality :String          ): String;
     destructor Destroy;                                                        override;
    end;

{supported datasources}
  twcDataSource=twcFirstDataSource..twcLastDataSource;


{======================== TRadthData profile base class =============================}
{24/08/2015 added ScanAngle}
{09/12/2015 added sharedparser}
{29/03/2016 added FRegisteredFiles}
{10/05/2016 added ErrorState}
{27/09/2016 added Binstream, binStreamType, binStreamFile, FBinaryAllowed}
{01/05/2020 added transfer of BinaryData in Create}
{16/05/2020 added FMultiScanCapable}
{13/10/2020 added AutoDecimalPoint,AutoDecimalList}
{19/10/2020 visibility of BinStream and BinsTreamFile extended to public}
{16/11/2020 added FindMoreData,ADataTopLine: support for multiple complete data sets in one single file intended for one single scan}
{14/01/2020 added PriorityMessage}
  TRadthData=class
    protected
     FExtraText       : TStringDynArray;
     FRegisteredFiles : String;
     FBinaryAllowed   : Boolean;
     FMultiScanCapable: Boolean;
    public
     AutoDecimalPoint : Boolean;
     AutoDecimalList  : String;
     BinStream        : TMemoryStream;
     BinStreamFile    : String;
     BinStreamType    : twcFileType;
     FileTime         : TDateTime;
     FileFormat       : twcFileType;
     UndefinedVal     : twcFloatType;
     UndefinedInt     : wmsIntType;
     PriorityMessage  : String;
     Linac            : String;
     IdentificationStg: String;
     ScanAngle        : twcFloatType;                                           //CW angle from AB axis
     ScanNr           : Integer;                                                //1-based
     ScanNrOk         : Integer;
     ScanMax          : Integer;
     ShowWarning      : Boolean;
     IsFile           : Boolean;
     DefaultExtension : String;
     ErrorState       : Boolean;
     ObjectCallSign   : String;
     {$IFDEF COMPILED_DEBUG}
     FailInfo         : String;
     {$ENDIF}
     constructor Create(SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
                        AStatusProc :toExtMsgProc =nil;
                        AIdentity   :String       ='base'    );
     procedure   StatusMessage(AMessage         :String;
                               UpdateLastMessage:Boolean =True;
                               MinLevel         :ShortInt=1  );
     function    GetNumPoints                                 : Integer;    virtual;
     function    CheckData(AStringList:TStrings              ): Boolean;    virtual;
     function    LoadBinStream(AFileName:String              ): Boolean;
     function    ReadData(AStringList :TStrings;
                          ADataTopLine:Integer    =0;
                          AFileFormat :twcFileType=twcUnknown): Boolean;    overload; virtual;
     function    ReadData(AStream     :TStream;
                          ADataTopLine:Integer    =0;
                          AFileFormat :twcFileType=twcUnknown): Boolean;    overload; virtual;
     function    ReadData(AFileName   :String;
                          ADataTopLine:Integer    =0;
                          AFileFormat :twcFileType=twcUnknown): Boolean;    overload; virtual;
     function    ReadBinData                                  : Boolean;              virtual;
     function    WriteData(AFileName  :String;
                           AStringList:TStrings;
                           ASource    :twcDataSource=dsMeasured;
                           ClearList  :Boolean      =True    ): Boolean;    overload; virtual;
     function    WriteData(AFileName  :String;
                           Binary     :Boolean      =True;
                           ASource    :twcDataSource=dsMeasured;
                           SetExt     :Boolean      =True    ): Boolean;    overload; virtual;
     function    WriteData(AFileName  :String;
                           OutPutType :twcFileType;
                           ASource    :twcDataSource=dsMeasured;
                           SetExt     :Boolean      =True    ): Boolean;    overload; virtual;
     destructor  Destroy;                                                   override;
    private
     FIdentity     : String;
     FFileName     : String;
     FStatusProc   : toExtMsgProc;
     FLastMessage  : String;
     FLogLevel     : Word;
     FScanType     : twcScanTypes;
     FWarning      : String;
     FFormatOk     : Boolean;
     FLocalParser  : Boolean;
     function    DualReadData(AStringList :TStrings;
                              AStream     :TStream;
                              AFileName   :String;
                              ADataTopLine:Integer    =0;
                              AFileFormat :twcFileType=twcUnknown    ): Boolean;     virtual;
     function    ParseData(CheckFileTypeOnly:Boolean=False           ): Boolean;     virtual;
     function    GetScanDirection(ASide:twcSides                     ): twcMeasAxisStg;
     function    GetLastMessage                                       : string;
     function    InsertIdentity(AMessage:String=''                   ): String;
     procedure   SetStatusProcedure(AStatusProc:toExtMsgProc=nil     );
     procedure   SetLogLevel(ALevel:Word=1);
     procedure   TransferLastMessage(var AMessage:String             );
     procedure   ExceptMessage(AString           :String             );
    protected
     FParser       : toTNumParser;
     FParserTopLine: Integer;                                                   //storage of readdata value, can be asked for by UI to do reread
     FParseOk      : Boolean;
     procedure   SetDefaults;                                                      virtual;
     procedure   AddWarning(AWarning:String);                                      virtual;
     function    GetFileType(AFileName :String='';
                             BinaryOnly:Boolean=False      ): twcFileType;         virtual;
     function    GetFieldLength                             : twcFloatType;        virtual;
     function    GetFieldDepth                              : twcFloatType;        virtual;
     function    GetBeamType                                : twcBeamType;         virtual;
     function    IsBinary(AFileName:String=''              ): Boolean;             virtual;
     function    ReadResults(PostText:String=''            ): Boolean;             virtual;
    public
     function    FindMoreData(FromCurrentLine:Boolean=False): Boolean;             virtual;
     function    GetDistance(c1,c2:twcCoordinate)           : twcFloatType;
     procedure   ShiftPoint(var p :twcCoordinate;
                            AShift:twcCoordinate           );
    published
     property BeamType        :twcBeamType                   read GetBeamType;
     property Energy          :twcFloatType                  read UndefinedVal;
     property FieldDepth      :twcFloatType                  read getFieldDepth;
     property FieldLength     :twcFloatType                  read GetFieldLength;
     property FieldGT_cm      :twcFloatType                  read UndefinedVal;
     property FieldAB_cm      :twcFloatType                  read UndefinedVal;
     property FileName        :String                        read FFileName   write FFileName;
     property Identity        :String                        read FIdentity;
     property LastMessage     :String                        read GetLastMessage;
     property LogLevel        :word                          read FLogLevel   write SetLogLevel;
     property MultiScanCapable:Boolean                       read FMultiScanCapable;
     property Parser          :toTNumParser                  read FParser;
     property ParserTopLine   :Integer                       read FParserTopLine;
     property ScanType        :twcScanTypes                  read FScanType;
     property ScanLeftSide    :twcMeasAxisStg index twcLeft  read GetScanDirection;
     property ScanRightSide   :twcMeasAxisStg index twcRight read GetScanDirection;
     property StatusProcedure :toExtMsgProc                  read FStatusProc write SetStatusProcedure;
     property Warning         :String                        read FWarning    write AddWarning;
     property WedgeAngle      :twcFloatType                  read UndefinedVal;
     property FormatOk        :Boolean                       read FFormatOk;
    end;

(*
-----RFA300 profile--------sample-------------------------------------
:MSR 	1	 # No. of measurement in file          :MSR [n] identifying line
:SYS BDS 0 # Beam Data Scanner System          :SYS BDS
#
# RFA300 ASCII Measurement Dump ( BDS format )
#
# Measurement number 	1                        #comment which shows meas number
#
%VNR 1.0          version type, should be 1.0
%MOD 	RAT         modality, only RAT supported
    [ FLM (Film) RAT (Ratio, Relative) ABS (Absolute) INT (Integrated) UDF (Undefined/Isodose) ]
%TYP 	SCN         Type = identifies type of curve, only SCN supported here
    [ SCN (Scan), ISO (Isodose), UDF (Undefined) ]
%SCN 	PRO         ScanType = identifies type of scan.
    [ DPT (DepthDose), PRO (Profile), MTX (Matrix), DIA (Diagonal), UDF (Undefined/Isodose) ]
%FLD 	ION         DetectorType
    [ ION (IonChamber), SEM (Semiconductor), UDF (Undefined) ]
%DAT 	12-09-2011  dd-mm-yyyy
%TIM 	13:26:36    hh:mm:ss
%FSZ 	400	400     FieldWidth and FieldHeight in [mm]
%BMT 	PHO	    6.0 RadType and energy [MV or MeV]
    [ COB (Cobalt), PHO (Photons), ELE (Electrons), UDF (Undefined) ]
%SSD 	1000        [mm]
%BUP 	0           buildup in [0.1 mm]
%BRD 	1000        BeamReferenceDist [mm]
%FSH 	-1          field shape
    [ -1 (Undefined), 0 (Circular), 1 (Rectangular), 2 (Irregular) ]
%ASC 	0           Accessory number
%WEG 	0           wedge angle [degrees]
%GPO 	0           GantryAngle [degrees]
%CPO 	0           CollimatorAngle [degrees]
%MEA 	2           MeasurementType
   [ -1 (Undefined), 0 (Absolute dose), 1 (Open depth), 2 (Open profile),
      4 (Wedge)    , 5 (Wedge depth)  , 6 (Wedge profile)                ]
%PRD 	500         ProfileDepth [0.1 mm]
%PTS 	347         number of points
%STS 	    0.0	  210.0	   50.0 # Start Scan values in mm ( X , Y , Z )
%EDS 	    0.0	 -210.0	   50.0 # End Scan values in mm ( X , Y , Z )
!·CommentsLine1   max 60 char
!·CommentsLine2   max 60 char
#
#	  X      Y      Z     Dose
#
= 	    0.0	  210.0	   50.0	   99.9
= 	    0.0	  209.1	   50.0	   99.5
= 	    0.0	  207.4	   50.0	   99.6

******data order and directions are GT [mm], AB [mm], UP [mm], Dose [%]*****
no swapping needed
*)

const
  rfaNumMeasID   = ':MSR';
  rfaSystemID    = ':SYS';
  rfaMeasNumberID= '# Measurement number';
  rfaModID       = '%MOD';
  rfaFileID      = '%TYP';
  rfaScanID      = '%SCN';
  rfaDetID       = '%FLD';
  rfaDateID      = '%DAT';
  rfaTimeID      = '%TIM';
  rfaFSizeID     = '%FSZ';
  rfaRadiationID = '%BMT';
  rfaSSDID       = '%SSD';
  rfaBUPID       = '%BUP';
  rfaBRDID       = '%BRD';
  rfaFShapeID    = '%FSH';
  rfaAccessoryID = '%ASC';
  rfaWedgeID     = '%WEG';
  rfaGantryID    = '%GPO';
  rfaCollimatorID= '%CPO';
  rfaMeasID      = '%MEA';
  rfaDepthID     = '%PRD';
  rfaPointsID    = '%PTS';
  rfaStartScanID = '%STS';
  rfaEndScanID   = '%EDS';
  rfaMeasInfoID  = '!';
  rfaDataID      = '=';
  rfaEndMeasID   = ':EOM';
  rfaEndFileID   = ':EOF';
  rfa_IDlen      =  3;
  rfaID          = rfaNumMeasID;
type
  rfa_ID    =String[rfa_IDlen];
  rfa_Header=record
               rfaMSR: Integer;
               rfaSYS: String;
             end;
  rfa_Data  =record
               rfaModType       : rfa_ID;                            {%MOD}
               rfaFileType      : rfa_ID;                            {%TYP}
               rfaScanType      : rfa_ID;                            {%SCN}
               rfaDetType       : rfa_ID;                            {%FLD}
               rfaDate          : TDateTime;                         {%DAT, %TIM}
               rfaField_mm      : array[twcFieldSizeDesc] of Integer;{%FSZ}
               rfaRadiation     : rfa_ID;                            {%BMT}
               rfaEnergy_MV     : twcFloatType;                      {%BMT}
               rfaSSD_mm        : Integer;                           {%SSD}
               rfaBUP_01mm      : Integer;                           {%BUP}
               rfaBRD_mm        : Integer;                           {%BRD}
               rfaFShape        : Integer;                           {%FSH}
               rfaAccessory     : Integer;                           {%ASC}
               rfaWedge_deg     : Integer;                           {%WEG}
               rfaGantry        : Integer;                           {%GPO}
               rfaCollimator    : Integer;                           {%CPO}
               rfaMeasType      : Integer;                           {%MEA}
               rfaDepth_01mm    : twcFloatType;                      {%PRD}
               rfaPoints        : Integer;                           {%PTS}
               rfaStart_Meas_cm : twcCoordinate;                     {%STS}
               rfaEnd_Meas_cm   : twcCoordinate;                     {%ETS}
               rfaComments      : array[0..1] of String;             {!}
               rfaCoordinates_cm: twcCoordArray;                     {=}
               rfaValues        : twcFloatArray;                     {=}
             end;

  {09/12/2015 added sharedparser}
  {01/05/2020 added transfer of BinaryData in Create}
  TRfaProfileData=class(TRadthData)
    private
     function  ParseData(CheckFileTypeOnly:Boolean=False): Boolean;      override;
    public
     RfaHeader  : rfa_Header;
     RfaData    : rfa_Data;
     constructor Create(SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
                        AStatusProc :toExtMsgProc =nil;
                        AIdentity   :String='RFA300'     );               reintroduce;
     procedure SetDefaults;                                               override;
     function  MakeTimeString(ADateTime:TDateTime        ): String;
     function  GetFileType(AFileName       :String ='';
                           BinaryOnly      :Boolean=False): twcFileType;  override;
     function  CheckData(AStringList       :TStrings     ): Boolean;      override;
     function  GetNumPoints                               : Integer;      override;
     function  GetProfile(Index            :Integer      ): twcGrafPoint;
     procedure PutProfile(Index            :Integer;
                          Point            :twcGrafPoint );
     function  ReadResults(PostText:String=''            ): Boolean;      override;
     function  WriteData(AFileName :String;
                         AStringList:TStrings;
                         ASource    :twcDataSource=dsMeasured;
                         ClearList  :Boolean      =True  ): Boolean;      overload; override;
     function  WriteData(AFileName :String;
                         OutPutType:twcFileType;
                         ASource   :twcDataSource=dsMeasured;
                         SetExt    :Boolean      =True   ): Boolean;      overload; override;
     destructor Destroy;                                                  override;
    published
     property ScanLeftSide;
     property ScanRightSide;
     property Energy;
     property FileName;
     property Identity;
     property LastMessage;
     property Warning;
     property WedgeAngle     :Integer        read RfaData.rfaWedge_deg;
     property FormatOk;
    end;

(*
-----Sun Nuclear ICprofiler ascii-------sample 2015--documentation--------------------------------------
Filename		c:\temp\test.prm
TimeStamp		8/14/2015 13:48:28
Description
Institution
Software Version		3.2.0.1
Machine Setup:
Room
Machine Type
Machine Model
Machine Serial Number
Beam Type		Undefined
Energy		-1
Wedge Angle	deg	0
Wedge Type	deg	None
Gantry Angle	deg	0
Collimator Angle	deg	0
Collimator Left	cm	0
Collimator Right	cm	0
Collimator Top	cm	0
Collimator Bottom	cm	0
Rate	MU/min	0
Dose	MU	0
Field Size	MU	0 x 0                                  GTxAB
Collector Setup:
Orientation		Y Axis                                 Orientation:{[Inverse ]{X|Y} Axis | Theta = -82 deg. 0/0 mm shift.}
Tray Mount		No
SSD	cm	100.000
Alignment	cm	Light Field
Buildup	cm	0.000
Buildup Type	cm	WaterEquiv
Calibration File	C:\SNC\Profiler\Factors\6264302\Slot-2 - 6 MV - 26-3-2012.cal
Collector Config:
Collector Model		IC PROFILER
Collector Serial		6264302
Collector Revision		F
Firmware Version	ms	2.5.1
Measurement Mode	ms	Pulsed
Nominal Gain	ms	4
Collection Interval	ms	125
General Analysis
CAX Dose		0
Photon Wedge Angle		0
Analysis Config		Custom
Flatness type		Variance
Symmetry Type		Area (sym)
Energy Calibration
Energy Analysis		Intercept
Penumbra Top		80
Penumbra Bottom		20
Flatness Field Percent		80
Symmetry Field Percent		80
Base Intensity Point		MAX
Intensity Field Percentage		50
X Axis Analysis
Field Size	cm	19.8407
Beam Center	cm	-0.59777
Light/Rad Coinc. Bottom	cm	0.518137
Light/Rad Coinc. Top	cm	-0.677403
Penumbra Bottom	cm	-2.4782
Penumbra Top	cm	18.1197
Flatness Result	perc	15.0413
Symmetry Result	perc	4.32286
Electron Energy	Ep0
Electron Energy	D80
Y Axis Analysis
Field Size	cm	19.8798
Beam Center	cm	-0.466749
Light/Rad Coinc. Bottom	cm	0.40664
Light/Rad Coinc. Top	cm	-0.526858
Penumbra Bottom	cm	-1.71754
Penumbra Top	cm	25.5189
Flatness Result	perc	15.0804
Symmetry Result	perc	5.34266
Electron Energy	Ep0
Electron Energy	D80
Positive Diagonal Analysis
Field Size	cm	27.9239
Beam Center	cm	-0.753221
Light/Rad Coinc. Bottom	cm	0.573028
Light/Rad Coinc. Top	cm	-0.933414
Penumbra Bottom	cm	-0.291774
Penumbra Top	cm	-0.579187
Flatness Result	perc	22.4746
Symmetry Result	perc	6.40666
Electron Energy	Ep0
Electron Energy	D80
Negative Diagonal Analysis
Field Size	cm	26.8252
Beam Center	cm	-0.109613
Light/Rad Coinc. Bottom	cm	-0.619935
Light/Rad Coinc. Top	cm	-0.839162
Penumbra Bottom	cm	-0.724678
Penumbra Top	cm	-0.901883
Flatness Result	perc	19.1264
Symmetry Result	perc	0.667473
Electron Energy	Ep0
Electron Energy	D80
Measured Data:	Total Dose-Normalised (%)
Pulses:		765
TimerTics	(1 uSec)	7661116

Detector ID	X Axis Position(cm)	Set 1
	-16.00	1.165611383
(...)
	16.00	0.932427059
Detector ID	Y Axis Position(cm)	Set 1
	-16.00	0.7533903644
(...)
	16.00	0.687218501
Detector ID	Positive Diagonal Position(cm)	Set 1
	-22.63	0.1917459202
(...)
	22.63	0.09906074965
Detector ID	Negative Diagonal Position(cm)	Set 1
	-22.63	0.2447433998
(...)
	22.63	0.1932210702

-----Sun Nuclear ICprofiler multiprofile------------sample 2015-----------------------------------
Version:	 25
Filename:	C:\SNC\Profiler\Data\14-8-2015 Loes\2R dependence - 20x20 xy-offset 5mm\U3 20x20 xy-offset 5mm @isoc 2R=0.4.prm
Date:	 8/14/2015	 Time:	13:48:28
Description:
Institution:
Calibration File:	C:\SNC\Profiler\Factors\6264302\Slot-2 - 6 MV - 26-3-2012.cal
Software Version:	3.2.0.1

	Profiler Setup
Buildup:	0	cm	WaterEquiv
Orientation:	Y Axis
SSD:	100	cm
Tray Mount:	No
Alignment:	Light Field


	Collector Config
Collector Model:	IC PROFILER
Collector Serial:	6264302	Revision:	F
Firmware Version:	2.5.1
Nominal Gain	4
Beam Mode:	Pulsed
Collection Interval:	125

	Machine Data
Room:
Machine Type:
Machine Model:
Machine Serial Number:
Beam Type:	Undefined	Energy:	U
Collimator:	Left: 0 Right: 0 Top: 0 Bottom: 0 cm
Wedge:	None	at	0
Rate:	0	mu/Min	Dose:	0
Gantry Angle:	0 deg	Collimator Angle:	0 deg

	Hardware Data
Cal-Serial Number:	6264302
Cal-Revision:	F
Temperature:	22.4005887	35733
Bp:	-2.634920597	Mp:	0.9815155864	Bt:	0.08539377898
	Dose Calibration
Dose Per Count:	0
Dose:	0
Absolute Calibration:	false
Energy:	U
TimeStamp:	12/30/1899 0:0:0
Comments:
	Reference Detector Dose Calibration
Dose Per Count:	0
Dose:	0
Absolute Calibration:	false
Serial Number:	0
Energy:	U
TimeStamp:	12/30/1899 0:0:0
Type:
Gain Ratios for Amp0:		1	0.5	0.25	0.125
Gain Ratios for Amp1:		1	0.5	0.25	0.125
Gain Ratios for Amp2:		1	0.5	0.25	0.125
Gain Ratios for Amp3:		1	0.5	0.25	0.125
Gain Ratios for Amp4:		1	0.5	0.25	0.125
Gain Ratios for Amp5:		1	0.5	0.25	0.125
Gain Ratios for Amp6:		1	0.5	0.25	0.125

	UltraSonic Calibration
Point 1(dist,mv):	0	0
Point 2(dist,mv):	0	0

Multi Frame:	true
Total Pulses:	765
Total Time:	7.661116123
PulseCountsDuringIdle:	2891649	PulseCountsDuringMmt:	24254237	BeamDurationDuringIdle:	19196323528	BeamDurationDuringMmt:	3474958556857
Detectors:	65	63	63	63	1	5
Detector Spacing:	0.5
Concatenation:	false
ConcatFile1:	none
ConcatFile2:	none
Imported Data:	false
-------------------------------
Definitions:
	Data:
		The measured data from each detector occurs in update periods defined by the collection data element
		Each update is saved in the next row
		Each update contains the total integrated raw count from each detector since the start of measurement
		The net raw counts in a given update is found by subtracting the prior update data

	Data Analysis:
		Integrated relative dose for Detector(n) = {RawCount(n) - TimeTic*LeakRate(n)}*cf(n)
		Net relative dose for Detector(n) during Update (u) = {[RawCount(n,u) - RawCount(n,u-1)] - [TimeTic(u) - TimeTic(u-1)] * LeakRate(n)}*cf(n)
		Absolute Dose for Detector(n) during update (u) is Net relative dose (calculated above) * Dose Per Count

	RawCount = the raw measurement value in DATA update rows (u) in the file under the Detector (n) columns X1, X2, X3, … and additional detectors and sensors
	TimeTic = the lapsed time since starting measurement (nominal units microsec)
	LeakRate = the leak rate in Raw Counts per TimeTic value, i.e., "Bias1" as illustrated in the table below.
	cf(n) = the array calibration for each detector n.

	· The Type Row contains the column data header (Update #, TimeTic, etc) which includes the detector ID, as in X1, X2, X3, … and additional detectors and sensors
	· The Bias1 row contains the Background leakage (dark current) of the detector, in units of count/Timetic
	· The Calibration row contains the calibration (normalization) factor for the detector. If none exists, all are set to 1.000.
	· The IgnoreDet row contains flags indicating if the detector is ignored or not (0=no, 1=yes)
	· The Data rows contains the
		o Update number from the device to the PC, starting with 0 or 1, then incrementing by 1, … (0, 1, 2, 3 or 1, 2, 3)
		o Timetic value is the lapse time since Start of pulse counting, nominally microsec
		o Pulses value is the number of LINAC pulses counted by data flags
		o Status is a flag value associated with the measurement condition during the update
		o 'X1' values are the raw counts from the detector X1 amplifier accumulated during the measurement update

    TYPE	UPDATE#	TIMETIC	PULSES	STATUS	X1	X2	X3	X4	X5	X6	X7 ....
BIAS1		21625772		0.00070772	0.00044789	0.00040511	0.00066356	0.00062388	0.0007122982	0.0005236807 ....
Calibration				  1.085543513	1.052441955	1.133811712	1.083792329	1.106805563	1.087155461	1.104373455 ...
IgnoreDet				  	0	0	0	0	0	0 0 ....
Data:	0	129297	12	65	305	304	299	396	439	504	536 ....
Data:	1	259329	25	65	625	648	622	818	914	1055	1124  ....
Data:	2	379358	37	65	949	950	929	1206	1350	1563	1654  ....
(..)
*)


const
  wICPAScanMax     = 4;
  wICPAAngle       =' Angle';
  wICPALeft        =' Left';
  wICPARight       =' Right';
  wICPATop         =' Top';
  wICPABottom      =' Bottom';
  wICPAIdentStg    ='Filename'+#09+#09;
  wICPATimeStamp   ='TimeStamp';
  wICPADescription ='Description';
  wICPAInstitution ='Institution';
  wICPARoom        ='Room';
  wICPABeamType    ='Beam Type';
  wICPAEnergy      ='Energy';
  wICPAWedgeAngle  ='Wedge'+wICPAAngle;
  wICPAGantry      ='Gantry';
  wICPACollimator  ='Collimator';
  wICPAFieldSize   ='Field Size';
  wICPAOrientation ='Orientation';
  wICPASetupInverse='Inverse';
  wICPASetupY      ='Y Axis';      {setup for theta=0}
  wICPASetupTheta  ='Theta';
  wICPASetupShift  ='shift';
  wICPASSD         ='SSD';
  wICPAModel       ='Collector Model';
  wICPAModelApprove='IC PROFILER';
  wICPADetectorLine='Detector ID';
  wICPAFrameLine   ='Frame';

type
  twICPAIndicators =(icLeft,icRight,icTop,icBottom);
  twICPAlines      =(icXline,icYline,icPosDiag,icNegDiag);

  {09/12/2015 added sharedparser}
  {01/05/2020 added transfer of BinaryData in Create}
  TICprofiler_ascii=class(TRadthData)
    private
     function  ParseData(CheckFileTypeOnly:Boolean=False): Boolean;      override;
    public
     icpClinic     : String;
     icpCollimator : Integer;
     icpCollimCm   : array[twICPAIndicators] of twcFloatType;
     icpComment    : String;
     icpData       : twcFloatArray;
     icpEnergy     : twcFloatType;
     icpFieldCm    : twcFieldDescrArr;
     icpFirstPoint : array[twICPAlines] of Integer;
     icpGantry     : Integer;
     icpMeasTime   : TDateTime;
     icpModality   : twcModalityChar;
     icpModel      : String;
     icpOrientation: String;
     icpSetupAngle : twcFloatType;                                               //device setup angle from GT axis CW
     icpPosGAOffsCm: twcCoordinate;
     icpPosCm      : twcFloatArray;
     icpScanLine   : twICPAlines;
     icpSSDcm      : twcFloatType;
     icpWedgeAngle : Integer;
     constructor Create(SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
                        AStatusProc :toExtMsgProc =nil;
                        AIdentity   :String       ='SNA' );               reintroduce;
     procedure SetDefaults;                                               override;
     function  MakeTimeString(ADateTime:TDateTime        ): String;
     function  GetFileType(AFileName       :String ='';
                           BinaryOnly      :Boolean=False): twcFileType;  override;
     function  CheckData(AStringList       :TStrings     ): Boolean;      override;
     function  GetNumPoints                               : Integer;      override;
     function  ReadResults(PostText:String=''            ): Boolean;      override;
     destructor Destroy;                                                  override;
    published
     property ScanLeftSide;
     property ScanRightSide;
     property Energy  read icpEnergy write icpEnergy;
     property FileName;
     property Identity;
     property LastMessage;
     property Warning;
     property FormatOk;
     end;


(*
-----Varian Eclipse-----
The start and end coordinate are AB,UD and GT respectively in cm.
The data points steps on the travelling direction in cm.
Line 11 gives fieldsize and energy.
-----Varian Eclipse---------sample----
Patient Name: Water, Phantom
Patient ID: WP001
Plan: QA1-6MV
Course: C1
Date: 06 October 2020 11:41:15 AM
Exported by: alanphys

Start: (-0.42, 4.78, 14.77)
End:   (-0.42, 4.78, -14.77)

	20x20-6MV
0.000000	6.259351
0.020008	6.288248
(...)
29.511242	6.251996
29.531250	6.223303
*)

const
  eclipseID= 'Patient Name: ';                                                  //the actual string is xEFxBBxBF+'Patient Name: '


type
  TEclipseData=class(TRadthData)
    private
     FStart_Cm       : twcCoordinate;
     FEnd_Cm         : twcCoordinate;
     FRange_Cm       : twcCoordinate;
     FValues         : twcGrafProfile;
     FDataMapping    : twcTankAxisID;                                           //twcTankAxisID=packed array[(Inplane,Crossplane,Beam)] of twcTankAxisChar;
     FAxisSign       : twcTankAxisSign;
     function ParseData(CheckFileTypeOnly:Boolean=False): Boolean;      override;
    public
     EcName          : String;
     EcID            : String;
     EcPlan          : String;
     EcCourse        : String;
     EcDate          : TDateTime;
     EcDateString    : String;
     EcOperator      : String;
     EcEnergy        : twcFloatType;
     EcModality      : String;
     EcField_cm      : array[twcFieldSizeDesc] of twcFloatType;                 //twcFieldSizeDesc=(fInplane,fCrossplane);
     constructor Create(SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
                        AStatusProc :toExtMsgProc =nil;
                        AIdentity   :String       ='Eclipse');             reintroduce;
     procedure SetDefaults;                                                override;
     function  MakeTimeString(ADateTime    :TDateTime     ): String;
     function  GetFileType(AFileName       :String ='';
                           BinaryOnly      :Boolean=False ): twcFileType;  override;
     function  CheckData(AStringList       :TStrings      ): Boolean;      override;
     function  GetNumPoints                                : Integer;      override;
     destructor Destroy;                                                   override;
    published
     property ScanLeftSide;
     property ScanRightSide;
     property Energy;
     property FileName;
     property Identity;
     property LastMessage;
     property Warning;
     property FormatOk;
     end;


(*
-----Varian w2CAD--------------sample----------------------------------
$NUMS 040             #not mandatory!
$STOM                 #start of one measurment
#
# Comment:
# Detector: CC 13 Field
# Operator:
#
%VERSION 02           #W2CAD version, not mandatory
%DATE 20-09-2011      #dd-mm-yyyy
%DETY CHA             #CHAmber|DIOde|DIAmond
%BMTY PHO             #PHOtons|(ELEctrons??)
%FLSZ 030*030         #x,y IEC1217 in mm, format "030*030" or "30 * 30"
%TYPE OPP             #OPD(open field pdd)|OPP(profile)|DPR (diagonal)|
                      #WDD(wedge pdd)|WDD_SSD80|WDD_SSD120|WDP(prof)|WLP(longitudinal)
%WDGL xx              #wedge name, 2 characters, (wedge angle?), wedge only
%WDGD L               #L (toe to neg X)|R (pos X) wedge direction, wedge only
%AXIS X               #X,Y,Z,D (IEC1217?)
%PNTS 755
%STEP 002             #in 0.1 mm
%SSD  1000            #SSD in mm
%DPTH 015             #scan depth in mm for profiles only
<-065.2 +000.0 +015.0 +000.5>  #x,y,z (mm), dose (%)
<-064.9 +000.0 +015.0 +000.5>
(...)
<+065.2 +000.0 +015.0 +000.5>
<+065.4 +000.0 +015.0 +000.5>
$ENOM                 #end of one measurement
$ENOF                 #end of file
*)

const
  w2ID             = '$STOM';
  w2NumMeas_ID     = '$NUMS';
  w2EndOfMeas_ID   = '$ENOM';
  w2EndOfFile_ID   = '$ENOF';
  w2Version_ID     = '%VERSION';
  w2Date_ID        = '%DATE';
  w2DetectorType_ID= '%DETY';
  w2Radiation_ID   = '%BMTY';
  w2FieldSize_mm_ID= '%FLSZ';
  w2WedgeName_ID   = '%WDGL';
  w2WedgeDir_ID    = '%WDGD';
  w2ScanType_ID    = '%TYPE';
  w2ScanAxis_ID    = '%AXIS';
  w2NumPoints_ID   = '%PNTS';
  w2Step_01mm_ID   = '%STEP';
  w2SSD_mm_ID      = '%SSD';
  w2Depth_mm_ID    = '%DPTH';
  w2Comments_ID    = '# Comment:';
  w2DetectorInfo_ID= '# Detector:';
  w2OperatorInfo_ID= '# Operator:';

{01/05/2020 added transfer of BinaryData in Create}
type
  Tw2CAD_data=class(TRadthData)
    private
     function  ParseData(CheckFileTypeOnly:Boolean=False): Boolean;      override;
    public
     w2Version       : Integer;
     w2Date          : TDateTime;
     w2DateString    : String;
     w2DetectorType  : String;
     w2Modality      : String;
     w2Field_mm      : array[twcFieldSizeDesc] of Integer;
     w2ScanType      : String;
     w2ScanAxis      : String;
     w2NumPoints     : Integer;
     w2Step_01_mm    : Integer;
     w2SSD_mm        : Integer;
     w2Depth_mm      : Integer;
     w2WedgeName     : String;
     w2WedgeDir      : String;
     w2WedgeAngle    : Integer;
     w2Comments      : String;
     w2DetectorInfo  : String;
     w2Operator      : String;
     w2Coordinates_mm: twcCoordArray;          {<}
     w2Values        : twcFloatArray;          {<}
     constructor Create(SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
                        AStatusProc :toExtMsgProc =nil;
                        AIdentity   :String       ='w2CAD');               reintroduce;
     procedure SetDefaults;                                                override;
     function  MakeTimeString(ADateTime    :TDateTime     ): String;
     function  GetFileType(AFileName       :String ='';
                           BinaryOnly      :Boolean=False ): twcFileType;  override;
     function  CheckData(AStringList       :TStrings      ): Boolean;      override;
     function  GetNumPoints                                : Integer;      override;
     function  GetProfile(Index            :Integer       ): twcGrafPoint;
     procedure PutProfile(Index            :Integer;
                          Point            :twcGrafPoint  );
     function  ReadResults(PostText        :String=''     ): Boolean;      override;
     destructor Destroy;                                                   override;
    published
     property ScanLeftSide;
     property ScanRightSide;
     property Energy;
     property FileName;
     property Identity;
     property LastMessage;
     property Warning;
     property FormatOk;
     end;

(*
-----PTW mcc profile-----------------sample----------------------------
BEGIN_SCAN_DATA
	FORMAT=CC-Export V1.9
	FILE_CREATION_DATE=29-Feb-2012 15:18:00
	LAST_MODIFIED=29-Feb-2012 15:18:00
	BEGIN_SCAN  1
  TASK_NAME=tba PDD Profiles        TASK_NAME=DOSE
		                                GROUP_NAME=TG_4.0
		PROGRAM=tbaScan                 PROGRAM=MultiCheck 3.04.0004
		COMMENT=FFF Acceptance - 7 MV
		MEAS_DATE=29-Feb-2012 15:15:32
		LINAC=A3 - L121
		MODALITY=X
		ISOCENTER=1000.00
		INPLANE_AXIS=X
		CROSSPLANE_AXIS=Y
		DEPTH_AXIS=Depth
		INPLANE_AXIS_DIR=GUN_TARGET
		CROSSPLANE_AXIS_DIR=RIGHT_LEFT
		DEPTH_AXIS_DIR=UP_DOWN
		ENERGY=7.00
		NOMINAL_DMAX=15.00
		SSD=900.00
		SCD=450.00
		BLOCK=0
		WEDGE_ANGLE=0.00
		FIELD_INPLANE=300.00
		FIELD_CROSSPLANE=300.00
		FIELD_TYPE=RECTANGULAR
		GANTRY=0.00
		GANTRY_UPRIGHT_POSITION=0
		GANTRY_ROTATION=CW
		COLL_ANGLE=90.00
		COLL_OFFSET_INPLANE=0.00
		COLL_OFFSET_CROSSPLANE=0.00
		SCAN_DEVICE=MP3
		SCAN_DEVICE_SETUP=BARA_GUN_TARGET
		ELECTROMETER=TANDEM
		RANGE_FIELD=MEDIUM
		RANGE_REFERENCE=AUTO
		DETECTOR=THIMBLE_CHAMBER
		DETECTOR_SUBCODE=SEMIFLEX
		DETECTOR_RADIUS=2.75
		DETECTOR_NAME=PTW 31010 Semiflex
		DETECTOR_SN=2107
		DETECTOR_CALIBRATION=304700000.00
		DETECTOR_IS_CALIBRATED=1
		DETECTOR_REFERENCE=THIMBLE_CHAMBER
		DETECTOR_REFERENCE_SUBCODE=SEMIFLEX
		DETECTOR_REFERENCE_RADIUS=2.75
		DETECTOR_REFERENCE_NAME=PTW 31010 Semiflex
		DETECTOR_REFERENCE_SN=1726
		DETECTOR_REFERENCE_IS_CALIBRATED=1
		DETECTOR_REFERENCE_CALIBRATION=304400000.00
		DETECTOR_HV=0.0
		DETECTOR_REFERENCE_HV=350.0
		REF_FIELD_DEPTH=100.00
		REF_FIELD_DEFINED=ISOCENTER
		REF_FIELD_INPLANE=100.00
		REF_FIELD_CROSSPLANE=100.00
		REF_SCAN_POSITIONS=-150.00;-148.00;-144.00;..;140.00;144.00;148.00;150.00;
		REF_OVERSCAN_FACTOR=1.00
		SCAN_CURVETYPE=INPLANE_PROFILE
		SCAN_DEPTH=100.00
		SCAN_OFFAXIS_INPLANE=0.00
		SCAN_OFFAXIS_CROSSPLANE=0.00
		SCAN_ANGLE=0.00
		SCAN_DIAGONAL=NOT_DIAGONAL
		SCAN_DIRECTION=POSITIVE
		MEAS_MEDIUM=WATER
		MEAS_PRESET=REFERENCE_DOSEMETER
		MEAS_TIME=0.300
		MEAS_UNIT=A.U.
		SCAN_SPEEDS=20.00; 50.00;40.00; 50.00;100.00; 50.00;400.00; 50.00;
		DELAY_TIMES=20.00; 0.000;150.00; 0.000;400.00; 0.000;
		PRESSURE=1013.25
		TEMPERATURE=20.00
		NORM_TEMPERATURE=20.00
		CORRECTION_FACTOR=1.0000
		EXPECTED_MAX_DOSE_RATE=16.00
		SCAN_COLOR=255                              added
		GUID={BAD91BB1-133D-4a50-8F3D-2DCE43AA789C} added
		DRAG_AND_DROP_STATUS=0                      added
		BEGIN_DATA
			-244.00		38.544E-03		12.712E+00        position/corrected meas value/reference
			-240.00		40.734E-03		12.746E+00
			-236.00		43.328E-03		12.743E+00
...
			236.00		43.834E-03		12.721E+00
			240.00		41.332E-03		12.562E+00
			244.00		39.074E-03		12.726E+00
		END_DATA
	END_SCAN  1
END_SCAN_DATA
*)
{
******data order: position [mm], corrected Dose, Reference [pC(?)] *****
swapping needed as described in axisdir
}

const
  mccBEGIN                  ='BEGIN';
  mcc_SCAN_DATA             ='_SCAN_DATA';
  mccFORMAT                 ='FORMAT';
  mccFILE_CREATION_DATE     ='FILE_CREATION_DATE';
  mccLAST_MODIFIED          ='LAST_MODIFIED';
  mcc_SCANnr                ='_SCAN ';
  mccTASK_NAME              ='TASK_NAME';
  mccGROUP_NAME             ='GROUP_NAME';
  mccPROGRAM                ='PROGRAM';
  mccCOMMENT                ='COMMENT';
  mccLINAC                  ='LINAC';
  mccMODALITY               ='MODALITY';
  mccISOCENTER              ='ISOCENTER';
  mccINPLANE                ='INPLANE';
  mccCROSSPLANE             ='CROSSPLANE';
  mccDEPTH                  ='DEPTH';
  mcc_AXIS                  ='_AXIS';
  mcc_DIR                   ='_DIR';
  mccENERGY                 ='ENERGY';
  mccNOMINAL_DMAX           ='NOMINAL_DMAX';
  mccSSD                    ='SSD';
  mccSCD                    ='SCD';
  mccBLOCK                  ='BLOCK';
  mccWEDGE_ANGLE            ='WEDGE_ANGLE';
  mccFIELD                  ='FIELD';
  mcc_INPLANE               ='_'+mccINPLANE;
  mcc_CROSSPLANE            ='_'+mccCROSSPLANE;
  mcc_TYPE                  ='_TYPE';
  mccGANTRY                 ='GANTRY';
  mcc_UPRIGHT_POSITION      ='_UPRIGHT_POSITION';
  mcc_ROTATION              ='_ROTATION';
  mccCOLL                   ='COLL';
  mcc_ANGLE                 ='_ANGLE';
  mcc_OFFSET                ='_OFFSET';
  mccSCAN                   ='SCAN';
  mcc_DEVICE                ='_DEVICE';
  mcc_SETUP                 ='_SETUP';
  mccELECTROMETER           ='ELECTROMETER';
  mccRANGE                  ='RANGE';
  mcc_REFERENCE             ='_REFERENCE';
  mccREF_                   ='REF_';
  mccDETECTOR               ='DETECTOR';
  mcc_SUBCODE               ='_SUBCODE';
  mcc_RADIUS                ='_RADIUS';
  mcc_NAME                  ='_NAME';
  mcc_SN                    ='_SN';
  mcc_CALIBRATION           ='_CALIBRATION';
  mcc_IS_CALIBRATED         ='_IS_CALIBRATED';
  mcc_HV                    ='_HV';
  mcc_DEPTH                 ='_DEPTH';
  mcc_DEFINED               ='_DEFINED';
  mcc_POSITIONS             ='_POSITIONS';
  mcc_COLOR                 ='_COLOR';
  mccOVERSCAN_FACTOR        ='OVERSCAN_FACTOR';
  mcc_CURVETYPE             ='_CURVETYPE';
  mcc_OFFAXIS               ='_OFFAXIS';
  mcc_DIAGONAL              ='_DIAGONAL';
  mcc_DIRECTION             ='_DIRECTION';
  mccMEAS                   ='MEAS';
  mcc_DATE                  ='_DATE';
  mcc_MEDIUM                ='_MEDIUM';
  mcc_PRESET                ='_PRESET';
  mcc_TIME                  ='_TIME';
  mcc_UNIT                  ='_UNIT';
  mcc_SPEEDS                ='_SPEEDS';
  mccDELAY_TIMES            ='DELAY_TIMES';
  mccPRESSURE               ='PRESSURE';
  mccTEMPERATURE            ='TEMPERATURE';
  mccNORM_TEMPERATURE       ='NORM_'+mccTEMPERATURE;
  mccCORRECTION_FACTOR      ='CORRECTION_FACTOR';
  mccEXPECTED_MAX_DOSE_RATE ='EXPECTED_MAX_DOSE_RATE';
  mccGUID                   ='GUID';
  mccDragDrop               ='DRAG_AND_DROP_STATUS';
  mcc_DATA                  ='_DATA';
  mccEND                    ='END';
  mccNotDiagonal            ='NOT'+mcc_DIAGONAL;
  mccFirstDiagonal          ='FIRST'+mcc_DIAGONAL;
  mccSecondDiagonal         ='SECOND'+mcc_DIAGONAL;
  mccID                     =mccBEGIN+mcc_SCAN_DATA;
  mcc_PROFILE               ='_PROFILE';
  mccCURVETYPE_INPLANE      =mccINPLANE+mcc_PROFILE;
  mccCURVETYPE_CROSSPLANE   =mccCROSSPLANE+mcc_PROFILE;
  mccCURVETYPE_PDD          ='PDD';
type
  tmElectroRec  =record
                   tmElType : String;                                   {TANDEM}
                   tmElRange: array[twcChannels] of String;        {MEDIUM,AUTO}
                 end;

  tmDetectorRec=record
                  tmDetType        : String;                   {THIMBLE_CHAMBER}
                  tmDetSubCode     : String;                          {SEMIFLEX}
                  tmDetRadius_mm   : twcFloatType;                        {2.75}
                  tmDetName        : String;                {PTW 31010 Semiflex}
                  tmDetSN          : String;                              {2107}
                  tmDetCalibration : twcFloatType;                {304700000.00}
                  tmDetIsCalibrated: Boolean;                                {1}
                  tmDetHV          : twcFloatType;                         {0.0}
                end;

  tmScanRec    =record
                  tmScanDepth_mm    : twcFloatType;                     {100.00}
                  tmScanOffAxis_mm  : twcFieldDescrArr;                   {0.00}
                  tmScanAngle       : twcFloatType;                       {0.00}
                  tmScanDiagonal    : String;      {NOT_|FIRST_|SECOND_DIAGONAL}
                  tmScanDirection   : String;                         {POSITIVE}
                  tmScanColor       : Integer;                             {255}
                end;

{All of them are “FIRST_DIAGONAL”.
The “X” diagonal (which has SCAN_CURVETYPE=CROSSPLANE_PROFILE) goes from GA to TB.
The “Y” diagonal (which has SCAN_CURVETYPE=INPLANE_PROFILE) goes from AT to GB.
Mephysto has created those, we are not responsible for the funny choice here J.}

  tmRefFieldRec=record
                  tmRefDepth     : twcFloatType;                        {100.00}
                  tmRefDef       : String;                           {ISOCENTER}
                  tmRefSize      : twcFieldDescrArr;                    {100.00}
                  tmRefScanPos_mm: twcFloatArray;            {-150.00; -148;...}
                  tmRefOverScan  : twcFloatType;                          {1.00}
                end;

  tmMccRecord=record
                tmTaskName      : String;                {tba PDD Profiles|DOSE}
                tmGroupName     : String;                             {?|TG_4.0}
                tmProgram       : String;                   {tbaScan|MultiCheck}
                tmComment       : String;                {FFF Acceptance - 7 MV}
                tmMeasDate      : TDateTime;              {29-Feb-2012 14:33:28}
                tmLinac         : String;                            {A3 - L121}
                tmModality      : twcModalityChar;                           {X}
                tmIsoc          : twcFloatType;                        {1000.00}
                tmTankAxis      : twcTankAxisID;              {permutatie X,Y,Z}
                tmAxisDir       : twcTankAxisSign;              {+/-1,+/-1,+/-1}
                tmMeasAxis      : twcMeasAxis;         {inplane/crossplane,beam}
                tmEnergy        : twcFloatType;                           {7.00}
                tmDmax          : twcFloatType;                          {15.00}
                tmSSD_mm        : twcFloatType;                         {900.00}
                tmSCD_mm        : twcFloatType;     {(source-collimator):450.00}
                tmBlock         : Integer;                                   {0}
                tmWedge         : twcFloatType;             {(wedge angle):0.00}
                tmField_mm      : twcFieldDescrArr;                     {300.00}
                tmFieldOffset_mm: twcFieldDescrArr;                       {0.00}
                tmFieldShape    : twcFieldShape;                   {RECTANGULAR}
                tmGantry        : twcFloatType;                       {(°):0.00}
                tmGantryUp      : twcFloatType;                              {0}
                tmGantryCW      : Boolean;                                  {CW}
                tmCollimator    : twcFloatType;                      {(°):90.00}
                tmScanDevice    : String;                    {MP3|STARCHECKMAXI}
                tmScan_Setup    : String;                      {BARA_GUN_TARGET}
                tmScanInfo      : tmScanRec;
                tmElectrometer  : tmElectroRec;
                tmDetectors     : array[twcChannels] of tmDetectorRec;
                tmRefField      : tmRefFieldRec;
                tmMedium        : String;                                {WATER}
                tmMeasPreset    : String;                  {REFERENCE_DOSEMETER}
                tmMeasTime_s    : twcFloatType;                          {0.300}
                tmMeasUnit      : String;                                 {A.U.}
                tmScanSpeeds    : twcFloatArray;            {20.00; 50.00; ... }
                tmDelays        : twcFloatArray;              {20.00; 0.000;...}
                tmPressure_hPa  : twcFloatType;                        {1013.25}
                tmTemperature_C : twcFloatType;                          {20.00}
                tmNormTemp_C    : twcFloatType;                          {20.00}
                tmPT            : twcFloatType;                         {1.0000}
                tmDoseRate      : twcFloatType;                          {16.00}
                tmGUID          : String; {BAD91BB1-133D-4a50-8F3D-2DCE43AA789C}
                tmDragDrop      : Boolean;                                   {0}
                tmPos_mm        : twcFloatArray;
                tmData          : twcFloatArray;
                tmCurveType     : String;
              end;

  {09/12/2015 added sharedparser}
  {10/02/2020 MccOriginValue added}
  {01/05/2020 added transfer of BinaryData in Create}
  TMccProfileData=class(TRadthData)
    private
     function  ParseData(CheckFileTypeOnly:Boolean=False): Boolean;      override;
    public
     MccCreated    : TDateTime;
     MccModified   : TDateTime;
     MccData       : tmMccRecord;
     MccOrgFormat  : twcFileType;
     MccOrgScanType: twcScanTypes;
     MccOriginValue: twcFloatType;    //the origin data point is only assigned to TG and not to AB in the original data
     constructor Create(SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
                        OriginValue :twcFloatType  =0;
                        AStatusProc :toExtMsgProc =nil;
                        AIdentity   :String       ='PTW');                 reintroduce;
     procedure SetDefaults;                                                override;
     procedure SetScanDefaults;
     procedure SetNumpoints(Npoints:wmsIntType);
     function  MakeTimeString(ADateTime    :TDateTime     ): String;
     function  GetNumPoints                                : Integer;      override;
     function  CheckData(AStringList       :TStrings      ): Boolean;      override;
     function  GetProfile(Index            :Integer       ): twcGrafPoint;
     function  GetFileType(AFileName       :String ='';
                           BinaryOnly      :Boolean=False ): twcFileType;  override;
     procedure PutProfile(Index            :Integer;
                          Point            :twcGrafPoint  );
     function  ReadResults(PostText:String=''             ): Boolean;      override;
     function  WriteData(AFileName :String;
                         AStringList:TStrings;
                         ASource    :twcDataSource=dsMeasured;
                         ClearList  :Boolean      =True   ): Boolean;      overload; override;
     function  WriteData(AFileName :String;
                         OutPutType:twcFileType;
                         ASource   :twcDataSource=dsMeasured;
                         SetExt    :Boolean      =True    ): Boolean;      overload; override;
     destructor Destroy;                                                   override;
    published
     property ScanType;
     property ScanLeftSide;
     property ScanRightSide;
     property Energy        :twcFloatType        read MccData.tmEnergy;
     property FileName;
     property Identity;
     property LastMessage;
     property Warning;
     property WedgeAngle     :twcFloatType        read MccData.tmWedge;
     property FormatOk;
    end;

{
-----HDF Profile------------------------------------------------
# Track: (0.013300000528494517, 0.14070000345440326, 5.000000237487257E-4) -> (0.24930000990629197, 0.14130000346913418, 5.000000237487257E-4)
# Length: 0.23600077208843656
0.0	                365.0	0.0                     relatieve pos, waarde, cm
0.0008474521494763608	368.0	1.999993615844263E-4
...
0.9991525478505235	343.0	0.23580077272685213
1.0	                344.0	0.23600077208843656
-----Generic Profile---------------------------------------------
0.0 365.0    positie, waarde
}
const
  hdfID='# track:';

{09/12/2015 added sharedparser}
{01/05/2020 added transfer of BinaryData in Create}
type
  THdfProfileData=class(TRadthData)
    private
      FHDFdata   : twcGrafProfile;
      FScanLength: twcFloatType;
      FStepSize  : twcFloatType;
      FDataMax   : twcFloatType;
     function  ParseData(CheckFileTypeOnly:Boolean=False       ): Boolean;      override;
    public
      ScanStart  : twcCoordinate;
      ScanStop   : twcCoordinate;
     constructor Create(SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
                        AStatusProc :toExtMsgProc =nil;
                        AIdentity   :String='hdf/generic'      );               reintroduce;
     procedure SetDefaults;                                                     override;
     function  GetNumPoints                                     : Integer;      override;
     function  CheckData(AStringList       :TStrings           ): Boolean;      override;
     function  GetProfile(Index            :Integer            ): twcGrafPoint;
     procedure PutProfile(Index            :Integer;
                          Point            :twcGrafPoint       );
     destructor Destroy;                                                        override;
    published
     property ScanType;
     property ScanLeftSide;
     property ScanRightSide;
     property ScanLength   : twcFloatType             read FScanLength;
     property StepSize     : twcFloatType             read FStepSize;
     property Energy;
     property FileName;
     property Identity;
     property LastMessage;
     property Warning;
     property FormatOk;
    end;

{
----Schuster--------------------------------------------------------------
x: 0, a: 0
Profile measured on 18/02/2008 10:45:35
 with Software BMS 2.202
Linac: U9
Photons at 10.0 MeV
crossplane
Buildup: 5.0 cm
x: 0, a: 0
31.1;31.9;...;3070.0  (88 stuks)
}

  {09/12/2015 added sharedparser}
  {01/05/2020 added transfer of BinaryData in Create}
  TSchusterProfileData=class(TRadthData)
    private
      FSchusterData: array of wmsRealType;
      FEnergy      : twcFloatType;
      FScanLength  : twcFloatType;
      FStepSize    : twcFloatType;
     function  ParseData(CheckFileTypeOnly:Boolean=False    ): Boolean;     override;
    public
      Depth        : twcFloatType;
      Modality     : twcModalityChar;
      ScanPos      : twcFloatType;
      ScanTime     : String;
     constructor Create(SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
                        AStatusProc :toExtMsgProc =nil;
                        AIdentity   :String='schuster'      );              reintroduce;
     function  CheckData(AStringList:TStrings               ): Boolean;     override;
     procedure SetDefaults;                                                 override;
     function  GetProfile(Index            :Integer         ): wmsRealType;
     procedure PutProfile(Index            :Integer;
                          Point            :wmsRealType     );
     function  GetNumPoints                                 : Integer;      override;
     destructor Destroy;                                                    override;
    published
     property ScanType;
     property ScanLeftSide;
     property ScanRightSide;
     property ScanLength   :twcFloatType             read FScanLength;
     property StepSize     :twcFloatType             read FStepSize;
     property Energy;
     property FileName;
     property Identity;
     property LastMessage;
     property Warning;
     property FormatOk;
    end;


{
----CMS--------------------sample-----------
00001090
Patient ID: cmsPHANTOM
Studyset ID: Phantom40
Plan ID: (null)
Doc: 19220100113.163719.101
T:  0.00 cm
X:  0.00 cm
Z:  0.00 cm
Angle:  0.00 deg.
Distance (cm)	Dose (cGy)
  -20.000	    2.800
  -19.900	    2.900
  ...
   20.000	    7.700
}

const
  xioID= '00001090';

{09/12/2015 added sharedparser}
{01/05/2020 added transfer of BinaryData in Create}
type
  TCmsProfileData=class(THdfProfileData)
    private
     function  ParseData(CheckFileTypeOnly:Boolean=False): Boolean;      override;
    public
     constructor Create(SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
                        AStatusProc :toExtMsgProc =nil;
                        AIdentity   :String       ='xio');               reintroduce;
     function  CheckData(AStringList:TStrings           ): Boolean;      override;
    published
     property ScanType;
     property ScanLeftSide;
     property ScanRightSide;
     property ScanLength;
     property StepSize;
     property Energy;
     property FileName;
     property Identity;
     property LastMessage;
     property Warning;
     property FormatOk;
    end;

{
----pips---------------sample-----------
Type of Cross-Section: row
Index: 515
Highest pixel: 254
At position: 0
Number of points: 1024
254
239
240
...  (peak has low values)
253
253
253
}

  {09/12/2015 added sharedparser}
  {01/05/2020 added transfer of BinaryData in Create}
  TPipsProfileData=class(TRadthData)
    private
      FPIPSdata  : array of wmsRealType;
      FScanLength: twcFloatType;
      FStepSize  : twcFloatType;
      FScanPos   : twcFloatType;
     function  ParseData(CheckFileTypeOnly:Boolean=False): Boolean;      override;
    public
     constructor Create(PixelCm     :twcFloatType  =twcDefPipsPixelCm;
                        SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
                        AStatusProc :toExtMsgProc=nil;
                        AIdentity   :String       ='pips');              reintroduce;
     procedure SetDefaults;                                              override;
     function  GetProfile(Index            :Integer      ): wmsRealType;
     procedure PutProfile(Index            :Integer;
                          Point            :wmsRealType  );
     function  GetNumPoints                               : Integer;     override;
     destructor Destroy;                                                 override;
    published
     property ScanType;
     property ScanLeftSide;
     property ScanRightSide;
     property Energy       :twcFloatType             read UndefinedVal;
     property ScanLength   :twcFloatType             read FScanLength;
     property ScanPos      :twcFloatType             read FScanPos;
     property StepSize     :twcFloatType             read FStepSize;
     property Identity;
     property LastMessage;
     property Warning;
     property FormatOk;
    end;

(*
**********WMS-binary*******************************
   +++++++ integer = smallint = 2 bytes ++
   -length of header      :   integer=588              2 bytes
   -header                :   record                 588
    .header identification:   integer=6    2+
    .header structure                    584+------ (588)
    .header identification:   integer=6    2+

   -length scan data      :   integer=n*10+4           2
   -data block=profile                            4+5n*2
    .data identification  :   integer=7    2+
    .data points          :   integer      2+ >> (4+5n*2)
    .n*(x,y,z,field,ref)  :   integer   5n*2+   positions in 0.1 mm
     (met x=gt, y=ab, z=bm)
   OR
   -length of plane data  :   integer=n*4+18           2
   -data block=plane                             18+2n*2
    .data-identification  :   integer=8    2+
    .rest of plane header : 8*integer    8*2+ >>(18+2n*2)
    .n*(field,ref)        :2n*integer   2n*2+
   +++++++++++++++++++++++++++++++++++++++

   According to the file 'common.f' on which this information is based, there follows
   at the end of a data block also a data identification. This appears not be true in practice;
   in the calculation of the data block size this is not included!

   Notes on variables in wmsFileHeader_Rec and wmsPlaneHeader_Rec:
  -Scan limits:
   *WmhBorders[1..2;AxGT..AxBm] gives de measurement positions in linac coördinates.
   *The limits for the scan axis are also given in wmhScan1,2.
    {GT/AB/Beam start/stop; for line scan ("L", see below) 0 en length van scan.
   *wmhKs indicates the data type: D=depth dose, G=gt, A=ab, L=line scan,
                                   U=undefined , P=plane.
   *wmhOrKs gives the originalle measured data type.
    In general wmhKs=wmhOrKs
   *The scan axis is set in wmhOrKs; wmhKs='P' (plane).
    In theory any combinations of two axes could be given for a plane.
    This proves to be fixed in all known software:
	  wmhOrKs   wmpPlane  scan/transportrichting   korte/lange zijde film
	    'G'        1      axgt/axab                    G/A   (rotatie 0°)
	    'A'        2      axab/axbm                    A/P
	    'G'        3      axgt/axbm                    G/P
   *Voor een vlak staan de grenzen voor de twee variabele assen ook in wmpI1row,
    wmpI2row, wmpI1col, wmpI2col.
   *De relatie tussen versnellercoördinaten en waterbakcoördinaten staat beschreven
    in wmhAxisID[AxGT..AxBm]='XYZ','XZY','YZX' etc. en de indicatie van de as-
    richting voor X-, Y- en Z-as van de waterbak in wmhAxisSign met 1 en -1 als geldige
    waarden.
  -wmhRaType: C=Co-60, X=fotonen, E=elektronen
  -wmhFldType: O=open, W=wedge, R=block rectangular, D=block circle, C=compensator
  -wmhASD: applicator-surface distance =0 voor fotonen, >0 voor elektronen
  -Indeling data-blok:
   *WmpISrow en wmpIScol geven de lengtes aan van resp. rijen en kolommen: de
    datapunten representeren dus wmpIScol rijen met elk wmpISrow datapunten.
  -Waarde van pixel:
   *Waarde van pixel in %: wmhNdose*wmpIfield of wmhNdosediv*wmpIfield/wmpIref.
   *De waarden van wmhDofs en wmhRofs (data- en referentie-offset) lijken hier geen
    invloed op te hebben en hebben dus alleen betrekking op de meetomstandigheden.
   *WmhDmax en wmhRmax geven de waarde van wmpIfield resp. wmpIref waarvan de
    absolute waarde het maximum in de meet-set is.
  -Vullen wms-bestand vanuit vff-formaat: zie beschrijving onder vff_obj.wms_output
  -In de profielen staan X,Y en Z in waterbak-coördinaten: waarden dus afhankelijk van
   opstelling van waterbak onder versneller!
   Xversneller:= wmpPos[wmhAxisID[AxGT]]*wmhAxisSign[wmhAxisID[AxGT]]
*)
const
  wmsUnknownType    =    0;
  wmsProfileType    =    7;
  wmsPlaneType      =    8;
  wmsMax_ProfilePnts= 5000;
  wmsMax_Char       =   30;

type
  wmsCharConvRec= record case boolean of
                    true : (c:array[1..wmsMax_Char] of Char);
                    false: (s:string[Pred(wmsMax_Char)]);
                   end;
  wmsComments   = (wmhG1,wmhG2,wmhP0,wmhP1,wmhP2,wmhP3,wmhP4,wmhU1,wmhU2,wmhU3,wmhU4);

  wmsFileHeadRec=packed record     {samenstelling/volgorde NIET veranderen; PLATO-RTS bijlage B}
    wmhIaaa06                                 : wmsIntType;                       {iaaa06=6 002}
    wmhDate                                   : array[1..14] of CHAR;                      {016}
    wmhDevice,wmhTime                         : array[1..12] of CHAR;                      {040}
    wmhAxisID                                 : twcTankAxisID;                  {perm.'XYZ' 043}
    wmhNOP1                                   : CHAR;                                   {#0 044}
    wmhAxisSign                               : twcTankAxisSign;                        {±1 050}
    wmhFmax     ,wmhRmax,                                                    {Field/Ref max 054}
    wmhFofs     ,wmhRofs,wmhFsign,wmhRsign    : wmsIntType;          {Field/Ref offset,sign 062}
    wmhKs       ,wmhOrKs                      : CHAR;      {scantype,(orgineel) U/P/D/G/A/L 064}
    wmhBorders                                : array[1..2,twcMeasAxis] of wmsIntType;     {076}
    wmhScan                                   : array[1..2] of wmsIntType;        {*0.01 cm 080}
    wmhMCm      ,wmhCmSec    ,wmhIsodose,                 {stap (cm),snelh.(cm/s),100%      092}
    wmhMtime    ,wmhNdose    ,wmhNdosediv     : wmsRealType; {meettijd, norm. dose,dose/ref 104}
    wmhDRdiv    ,wmhNsamp                     : wmsIntType;      {boolean dose/ref, samples 108}
    wmhRaType   ,wmhFdType                    : CHAR;                 {'C/X/E', 'O/W/R/D/C' 110}
    wmhASD      ,wmhEnergy,                                         {app-surface-dist,(MeV) 108}
    wmhFdAB_cm  ,wmhFdGT_cm,                                                     {veld (cm) 126}
    wmhBRect1_cm,wmhBRect2_cm,wmhBDiam_cm     : wmsRealType;     {0 of blok-afmetingen (cm) 138}
    wmhWedgeAngle                             : wmsIntType;                                {140}
    wmhTrayTr   ,wmhCollim,                                           {tray transmissie,(°) 148}
    wmhGantry_cm,wmhSSD_cm                    : wmsRealType;                          {(cm) 156}
    wmhComs                                   : array[wmsComments,1..30] of CHAR;          {486}
    wmhDosCon                                 : wmsRealType;            {dosisconversie=1   490}
    wmhBShape   ,wmhNOP2                      : CHAR;                                      {492}
    wmhAIdent   ,wmhWIdent                    : wmsIntType;          {accesory/wedge ident. 496}
    wmhINOP3                                  : array[1..45] of wmsIntType;                {586}
    wmhIzzz06                                 : wmsIntType;                       {izzz06=6 588}
   end;

 wmsFileHeaderBlock   =packed record
    wmsRecSize06                            : word;                            {wmssize06=588}
    wmsRec06                                : wmsFileHeadRec;
   end;

 wmsProfileHeaderRec  =packed record
    wmpIaaa07,wmpNpoints                    : wmsIntType;       {wmsProfileType,aantal punten}
   end;

 wmsProfileHeaderBlock=packed record  {wmsrecsize07=wmpnpoints*wmspropointsize+wmsproheadsize}
    wmsRecSize07                            : word;                           {= #punten*10+4}
    wmsRec07                                : wmsProfileHeaderRec;
   end;

 wmsProfilePoint      =packed record
   case integer of
    1: (wmpPos                              : array[twcTankAxisChar] of wmsIntType;
        wmpIfield,wmpIref                   : wmsIntType);
    2: (wmpPointArray                       : array[1..5] of wmsIntType);
    3: (wmpAccPos                           : array[twcMeasAxis] of wmsIntType; {only when reorganized}
        wmpField,wmpRef                     : wmsIntType);
   end;

 wmsPlaneHeaderRec    =packed record
    wmpIaaa08,wmpPlane ,wmpLrow  ,wmpLcol,  {iaaa08=8,1..3,rijlengte,aantal rijen}
    wmpI1row ,wmpI2row ,wmpI1col ,wmpI2col,   {row=a=X(=GT), col=ß=Y(=AB) *0.1 mm}
    wmpIcon                                 : wmsIntType;
   end;

 wmsPlaneHeaderBlock  =packed record
    wmsRecSize08                            : WORD;            {wmsblocksize08=18}
    wmsRec08                                : wmsPlaneHeaderRec;
   end;

 wmsPlanePoint        =packed record
    wmpIfield,wmpIref                       : wmsIntType;
   end;

 wmsHeaderPtr = ^wmsFileHeadRec;

const
 wmsRecSize           = 2;
 wmsGenHeadSize       = SizeOf(wmsFileHeadRec);
 wmsExtraSize         = SizeOf(WORD);              {wmssize06,wmssize07,wmssize08}
 wmsGenHeadBlockSize  = SizeOf(wmsFileHeaderBlock);
 wmsProHeadSize       = SizeOf(wmsProfileHeaderRec);
 wmsProHeadBlockSize  = SizeOf(wmsProfileHeaderBlock);
 wmsProPointSize      = SizeOf(wmsProfilePoint);
 wmsProfileMax        = $FFF0 div wmsProPointSize;
 wmsProDataOffsRecords= (wmsGenHeadBlockSize+wmsProHeadBlockSize) div wmsRecSize;
 wmsProDataRecords    = wmsProPointSize div wmsRecSize;
 wmsPlaHeadSize       = SizeOf(wmsPlaneHeaderRec);
 wmsPlaHeadBlockSize  = SizeOf(wmsPlaneHeaderBlock);
 wmsPlaPointSize      = SizeOf(wmsPlanePoint);
 wmsPlaDataOffsRecords= (wmsGenHeadBlockSize+wmsPlaHeadBlockSize) div wmsRecSize;
 wmsPlaPointRecords   = wmsPlaPointSize div wmsRecSize;
 wmsPlaLineMax        = $FFF0 div wmsPlaPointSize;
 wmsPlaneLim:LongInt  = 10000;
 wmsPlaLineLim:WORD   =   256;


type
 wmsProfileArr        = array[1..wmsProfileMax] of wmsProfilePoint;
 wmsProfilePtr        = ^wmsProfileArr;
 wmsPlaLineArr        = array[1..wmsPlaLineMax] of wmsPlanePoint;
 wmsPlaLinePtr        = ^wmsPlaLineArr;
 wmsPlaneLine         =
   record
    wmpLineNr,wmpNElem                      : WORD;      {1..wmpilcol,1..wmpilrow}
    wmpLine                                 : wmsPlaLinePtr; {allocatie gebruiker}
   end;      {aantal benodigde bytes voor wmpline^: wmplineelem*wmsplainpointsize}


//----wms--------------------------------------------------------------
  {09/12/2015 added sharedparser}
  {01/05/2020 added transfer of BinaryData in Create}
  {11/06/2020 added SetLinacFromHeader}
  {16/11/2020 ADataTopLine}
  TWmsData      =class(TRadthData)
    private
     wmsProfile      : array of wmsProfilePoint;
     wmsProfileHeader: wmsProfileHeaderBlock;
     wmsFileHeader   : wmsFileHeaderBlock;
     function  ParseData(CheckFileTypeOnly:Boolean=False ): Boolean;      override;
     function  GetScanTypeString                          : string;
     function  GetScanType                                : twcScanTypes;
     procedure SetNumpoints(Npoints:wmsIntType);
     procedure SetLinacFromHeader;
    protected
     function  GetBeamType                                : twcBeamType;   override;
    public
     constructor Create(SharedParser:toTNumParser =nil;
                        ParsedFile  :String       ='';
                        BinaryData  :TMemoryStream=nil;
                        BinaryFile  :String       ='';
     			AStatusProc :toExtMsgProc =nil;
                        AIdentity   :String       ='wms'   );              reintroduce;
     procedure SetDefaults;                                                override;
     procedure Stg2Char(Stg               :string;
                        var WMS_Char_Array:array of Char   );
     function  Char2Stg(var WMS_Char_Array:array of Char;
                        ArrayLength       :Byte            ): string;
     function  IsBinary(AFileName    :String =''           ): Boolean;      override;
     function  GetFileType(AFileName :String ='';
                           BinaryOnly:Boolean=False        ): twcFileType;  override;
     function  GetFieldLength                               : twcFloatType; override;
     function  GetFieldDepth                                : twcFloatType; override;
     function  GetNumPoints                                 : Integer;      override;
     function  ReadBinData                                  : Boolean;      override;
     function  ReadData(AFileName   :String;
                        ADataTopLine:Integer    =0;
                        AFileFormat :twcFileType=twcUnknown): Boolean;     override;
     function  WriteData(AFileName  :String;
                         Binary     :Boolean      =True;
                         ASource    :twcDataSource=dsMeasured;
                         SetExt     :Boolean      =True    ): Boolean;     overload; override;
     function  WriteData(AFileName  :String;
                         AStringList:TStrings;
                         ASource    :twcDataSource=dsMeasured;
                         ClearList  :Boolean      =True    ): Boolean;     overload; override;
     function  WriteData(AFileName  :String;
                         OutPutType :twcFileType;
                         ASource    :twcDataSource=dsMeasured;
                         SetExt     :Boolean      =True    ): Boolean;     overload; override;
     function  GetProfile(Index            :wmsIntType;
                          ConvertToAccPos  :Boolean=False  ): wmsProfilePoint;
     procedure PutProfile(Index            :wmsIntType;
                          WmsPoint         :wmsProfilePoint;
                          ConvertFromAccPos:Boolean=False  );
     destructor Destroy;                                               override;
    published
     property Energy       :wmsRealType    read  wmsFileHeader.wmsRec06.wmhEnergy;
     property FieldGT_cm   :wmsRealType    read  wmsFileHeader.wmsRec06.wmhFdGT_cm;
     property FieldAB_cm   :wmsRealType    read  wmsFileHeader.wmsRec06.wmhFdAB_cm;
     property ScanType                     read  GetScanType;
     property ScanLeftSide;
     property ScanRightSide;
     property Identity;
     property LastMessage;
     property Warning;
     property FormatOk;
   end;

(*
**********omnipro asci-formaat*********************
  1 Clinic:	clinic
  2 Address:	address
  3 Telephone:	030-xxxxxxx
  4 Email:	xx@xx.nl
  5
  6 Beam Description:
  7
  8 Radiation device	U04
  9 Energy	6 MV Photon
 10 Wedge:	 0 °
 11 Gantry	0 °(0°up, CW)
 12 Collimator	0 °
 13 SSD, SAD [cm]:	100	100
 14 Applicator:	No Applicator
 15 Field size [cm x cm]:	40 x 40
 16 Field position inline (min, max) [cm]:	-20	20
 17 Field position crossline (min, max) [cm]:	-20	20
 18 Medium:	Water
 19
 20 CurveType: 	Inline Profile
 21
 22 Curve Description:
 23
 24 Measurement Time:	2005-07-27 16:09:04
 25 Modification Time:	2005-07-27 17:38:08
 26 Operator:
 27 Measurement comment:
 28 Setup comment:
 29 Renormalise factor:	-
 30 Curve offset [cm]:	-0,1
 31
 32 Detector
 33 Quantity:	Relative Dose
 34 Detector Type:	IC 15	Ion Chamber (Cylindrical)	Offset to P eff [cm]: -0,18	Radius: 0.68cm
 35 Can be used for CA24 calibration:	No
 36
 37 Electrometer
 38 Electrometer type:	CU500E
 39 Measurement mode:	Continuous
 40 Reference division:	-
 41 Reference (avg, min, max):	1002	985	1015
 42 Measurements per point:	-
 43 Sampling time [ms]:	0
 44 HV type:	-
 45 Normalisation value, field and reference:	997	981
 46 Dark current, field and reference:	-7	2
 47 High voltage, field and reference [V]:	300	300
 48 Gain, field and reference:	18	18
 49 Range, field and reference:	HIGH	HIGH
 50
 51 Servo
 52 Servo type:	Blue 48 48 41
 53 Scan Speed [mm/s]:	15
 54 Water surface correction [cm]:	9,4
 55 Water offset correction [cm]:	0
 56 Origin [X, Y, Z] [cm]:	0,94	0,33	5,41
 57 Servo axis [I, C, D]:	-Y	X	-Z
 58 Isocentre [I, C, D] [cm]:	0	0	0
 59 Normalisation [I, C, D] [cm]:	0	0	4,99
 60
 61 Position A [I, C, D] [cm]:	0,33	-0,94	5,23
 62 Position B [I, C, D] [cm]:	0,33	-0,94	5,23
 63 Position C [I, C, D] [cm]:	0,33	-0,94	5,23
 64 Position D [I, C, D] [cm]:	0,33	-0,94	5,23
 65 Number Of Points:	606
 66 Start Point [I,C,D] [cm]:	-22,65	-0,02	5
 67 End Point [I,C,D] [cm]:	22,51	-0,02	5
 68 Points [cm]:	Inline	Crossline	Depth	Relative Dose
 69	-22,67	-0,02	5	11
 70	-22,60	-0,02	5	11,2
 71	-22,50	-0,02	5	11,4
672	22,37	-0,02	5	11,8
673	22,44	-0,02	5	11,5
674	22,46	-0,02	5	11,3
675   Scan Angle:	90
*)

  twClinicRec=record
      twClinic   : String;
      twAddress  : String;
      twTelephone: String;
      twEmail    : String;
    end;

  {23/08/2015
    twBSSD_cm removed
    twBSacnAngle moved to twCurveDesRec}
  twBeamDesRec=record
      twBEnergy     : twcFloatType;      {MV/MeV}
      twBModality   : twcModalityChar;   {'X','E'}
      twBWedge      : SmallInt;          {hoek}
      twBWedgeType  : String;            {hard/dynamic/motorized}
      twBGantry     : SmallInt;          {hoek}
      twBGantryScale: twcGantrySetup;    {twCW_180_Down,twCCW_180_Down,twCW_180_Up,twCCW_180_Up}
      twBCollimator : SmallInt;          {hoek}
      twBSAD_cm     : twcFloatType;      {cm}
      twBApplicator : String;
      twBASD        : twcFloatType;      {cm, appl-surface distance =0 (X), >0 voor (E)}
      twBFieldLo    : twcFieldDescrArr;  {cm}
      twBFieldHi    : twcFieldDescrArr;  {cm}
      twBMedium     : String;            {'water'}
      twBTrayTransm : twcFloatType;      {0<=waarde<=1}
    end;

  twCurveDesRec=record
      twDesTypeString  : String;         {'Inplane/Crossplane/Depth/Freescan/Angle/Point To Point Scan}
      twDesScanType    : twcScanTypes;
      twDesVaryingAxis : array[twcMeasAxis] of Boolean;
      twDesModTime     : String;
      twDesModDateTime : TDateTime;
      twDesOperator    : String;
      twDesMeasComment : String;
      twDesSetupComment: String;
      twDesNormalise   : twcFloatType;  {?}
      twDesShift       : twcFloatType;  {?}
    end;

  twDetectorDesRec=record
      twDetQuantity     : String;
      twDetName         : String;
      twDetType         : String;
      twDetPeffOffset_cm: twcFloatType;
      twDetRadius_cm    : twcFloatType;
      twCalFactor       : twcFloatType;
      twCalDate         : TDateTime;
      twPressure_hPa    : twcFloatType;
      twTemperature_C   : twcFloatType;
    end;

  twChannelRec=record
     twNorm         : twcFloatType;
     twDarkCurrent  : twcFloatType;
     twHV           : twcFloatType;
     twGain         : Integer;
     twRange        : String;
    end;

  twElectrometerDesRec=record
      twElMeterType  : String;
      twElMeasMode   : String;
      twElSamples    : Integer;
      twElSampleMs   : Integer;
      twElHVType     : String;
      twElRefAvg     : Integer;
      twElRefMin     : Integer;
      twElRefMax     : Integer;
      twElChannels   : array[twcChannels] of twChannelRec;
    end;

  twMeasDeviceRec=record
      twDeviceName           : string;
      twDeviceSpeed_mm_s     : twcFloatType;
      twDeviceOriginXYZ_cm   : twcCoordinate;
      twDeviceMappingICD     : twcTankAxisID;
      twDeviceDirXYZ         : twcTankAxisSign;
      twDeviceIsocICD_cm     : twcCoordinate;
      twDeviceNormICD_cm     : twcCoordinate;
      twDeviceWaterOffset_cm : twcFloatType;
      twDeviceWaterSurface_cm: twcFloatType;
      twDeviceRefPosition_cm : array['A'..'D'] of twcCoordinate;
    end;

  {26/06/2016: twFitMaxScaling}
  {13/07/2020: twFitOffsetCm added as replacement for shared twSigmoidOffsetCm}
  {05/03/2021: added twFitResult1,2 for optional model results}
  twFitRecord=record
      twFitLowCm            : twcFloatType;
      twFitHighCm           : twcFloatType;
      twFitNormalisation    : twcFloatType;         {normalisation on source data set}
      twFitMaxScaling       : twcFloatType;         {scaling for dmax=100}
      twFitScalingPointCm   : twcFloatType;
      twFitOffsetCm         : twcFloatType;         {former twSigmoidOffset is now separate for each side}
      twFitModel            : twcFitModels;         {models are: pddPhoton,pddPhotonExtrapolation,pddElectron,fffSlope,fffTop,penumbraSigmoid}
      twFitResult1          : twcFloatType;         {optional calculation results; sigmoid true inflection point}
      twFitResult2          : twcFloatType;         {optional calculation results; sigmoid slope in inflection point}
      twFitValid            : Boolean;
      twNMReport            : NMReportRecord;
    end;

   {10/12/2015}
   twFFFslopeRecord=record
      twFFFvalid            : Boolean;
      twFFFfirst            : Integer;
      twFFFlast             : Integer;
      twFFFoffset           : twcFloatType;
      twFFFgain             : twcFloatType;
    end;

  {13/07/2015 twLocked added}
  {20/07/2015 twComposite added}
  {06/08/2015 twIsFiltered added}
  {01/09/2015 twOriginPosValid added}
  {10/12/2015 added twFFFslope, replaced twLeft/RightFlatArr with twInFieldArr[side]}
  {12/12/2015 twFFFdetected}
  {20/12/2015 twTopModel added, twMinVal removed}
  {15/11/2016 added twCenterPosDefUse}
  {11/01/2017 added twUsedEdgeLevel}
  {14/01/2017 added twSigmoidOffsetCm}
  {23/11/2017 added twFlatPosCm,twSymAreaRatio}
  {12/01/2018 added twAbsNormConfig to note used info from modlist}
  {27/01/2018 twAbsNormDefUse}
  {15/05/2020 added twFirstScanPosCm,twLastScanPosCm for convenience}
  {22/05/2020 twSigmoidDone}
  {21/07/2020 removed twIsWedgedProfile}
  {27/08/2020 reintroduced twMaxPosCm,twMaxValue; twTopModel now only used for fitresults of top}
  {23/02/2021 reintroduced twFFFdetected because MRLinac can also be fff}
  twCurveDataRec=record
    twAbsNormConfig  : Boolean;       {a configured value/position is used to normalise}
    twAbsNormDefUse  : twcPositionUseType;
    twAbsNormPosCm   : twcFloatType;
    twAbsNormValue   : twcFloatType;
    twAlignedTo      : twcDataSource; {profile is shifted to align with other source}
    twAnalysed       : Boolean;       {analys is succesfully completed}
    twAppliedNormVal : twcFloatType;  {use this to override twAbsNormval}
    twAvgNormValue   : twcFloatType;
    twBackGround     : twcFloatType;  {background correction value}
    twBeamInfo       : twBeamDesRec;
    twCenterPosDefUse: twcPositionUseType;
    twCenterPosCm    : twcFloatType;
    twCenterPosValid : Boolean;
    twCenterArr      : Integer;
    twComposite      : Boolean;      {true when twData does not represent the original data}
    twConfidenceLimit: twcFloatType; {gamma analysis}
    twCoordinates    : twcCoordArray;{stored in measurement coordinates (ICD), conforms with vendors}
    twCurveIDString  : String;       {standard description of data to check equivalence}
    twData           : twcFloatArray;
    twDataFirst      : Integer;      {see also twScanFirst/Last}
    twDataLast       : Integer;
    twDataHistoryStg : String;       {description of history}
    twDevice         : String;       {holds copy of radiation device}
    twDerivativeValid: Boolean;
    twExtraText      : TStringDynArray;
    twFastScan       : Boolean;
    twFFFdetected    : Boolean;
    twFFFslope       : array[twcSides] of twFFFslopeRecord;
    twFFFslopesTop   : twcFloatType;
    twFileIDString   : String;
    twFileName       : String;
    twFilmData       : Boolean;
    twFilterPoints   : Integer;
    twFilterString   : String;
    twFirstDataPosCm : twcFloatType;  {twPosCm[twDataFirst]}
    twFirstScanPosCm : twcFloatType;  {twPosCm[twScanFirst]}
    twFittedData     : Boolean;
    twInFieldAreaOk  : Boolean;
    twInFieldArr     : array[twcSides] of Integer;
    twInFieldPosCm   : array[twcSides] of twcFloatType;
    twFlatness       : twcFloatType;
    twIsDerivative   : Boolean;
    twIsDiagonal     : Boolean;
    twIsFiltered     : Boolean;
    twIsGamma        : Boolean;
    twIsRelative     : Boolean;
    twLastDataPosCm  : twcFloatType;  {twPosCm[twDataLast]}
    twLastScanPosCm  : twcFloatType;  {twPosCm[twScanLast]}
    twLevelPos       : twcLimitsArray;{bordervalues}
    twLinSlope       : twcFloatType;  {slope of data within InField area}
    twLocalPeak      : Boolean;       {search for local peak}
    twLocked         : Boolean;       {when locked not used for standard routines}
    twMayneordApplied: Boolean;
    twMaxArr         : Integer;       {always maxpos in twData}
    twMaxPosCm       : twcFloatType;  {might deviate from twMaxArr based value}
    twMaxValue       : twcFloatType;  {might deviate from twMaxArr based value}
    twMeasTime       : String;
    twMeasDateTime   : TDateTime;
    twMinArr         : Integer;
    twMirrored       : Boolean;
    tw2DoseConv      : Boolean;
    twOD2doseFilm    : String;
    twOriginPosValid : Boolean;
    twOriginalFormat : twcFileType;
    twPDD10          : twcFloatType;
    twPDD20          : twcFloatType;
    twPddFitData     : array[twcNMpddFits] of twFitRecord;
    twPlotScaling    : twcFloatType;
    twPoints         : Integer;
    twPosCm          : twcFloatArray;
    twPosCmExportSign: wmsIntType;   {on export the original sign, which may be changed by axis flipping, when the position is restored}
   {$IFDEF POSINTEGRAL}
    twPosIntegral    : twcFloatType;
   {$ENDIF}
    twRelAvgInField  : twcFloatType;                                            //average value within in-field area
    twRelMinInField  : twcFloatType;
    twRelMaxInField  : twcFloatType;
    twRefNormFactor  : twcFloatType;
    twRelNormPosCm   : twcFloatType;
    twRelNormValue   : twcFloatType;
    twResampled      : Boolean;
    twPosScaling     : twcFloatType;                                            //scaling => twPosScaling:=twSDD2SSDratio/twSSD2NormRatio
    twRelatedSource  : twcDataSource;
    twScanAngle      : twcFloatType;                                            //CW angle from AB-axis
    twScanDevice     : String;
    twScanFirst      : Integer;
    twScanLast       : Integer;
    twScanNr         : Integer;
    twScanLength     : twcFloatType;
    twScanTypeString : String;
    twSelf           : twcDataSource;
    twSetFieldType   : twcFieldClass;
    twShiftCm        : twcFloatType;
    twSigmoidDone    : Boolean;
    twSigmoidFitData : array[twcSides] of twFitRecord;                          //fit results are based on raw, unscaled data, but with twSigmoidOffsetCm included
    twSSD_cm         : twcFloatType;
    twSDD2SSDratio   : twcFloatType;
    twSSD2NormRatio  : twcFloatType;                                            //ratio of used SSD to standard SSD}
    twSNR            : twcFloatType;
    twVector_ICD_cm  : array[twcStartStopType] of twcCoordinate;
    twStepSizeCm     : twcFloatType;
    twStepSign       : SmallInt;
    twSymCorrected   : Boolean;
    twSymmetry       : twcFloatType;
    twSymLinacError  : twcFloatType;
    twSymAreaRatio   : twcFloatType;
    twTag            : Integer;
    twTopModel       : TQuadFitReport;
    twUsedEdgeLevel  : twcDoseLevel;
    twValid          : Boolean;
    twWidthCm        : twcFloatType;                                            //width at edge level
  end;

  twCurveDataPtr=^twCurveDataRec;

(*------TWellhoferData-------------------------------------------
Note that all scan data on import or direct read are stored in the scan direction conventions
of the OmniPro v6 format: G-T, A-B, U-D
*)
const
     rfbString1='Version:';
     rfbString2='CBeam';

{19/07/2015 function NextPos added}
{20/07/2015 function IsCompositeData addded}
{01/08/2015
  FRefTempFile removed: in-memory implementation
  wTakeCurrentRefSource added
  SetReferenceOrg and UnSetReferenceOrg}
{15/08/2015
  function Getvalue renamed to GetQfittedValue
  function GetInterpolatedValue added
  wSamePositionRadiusCm added}
{09/12/2015 added sharedparser}
{11/12/2015 added wFFFMinDoseDifPerc, wFFFMinEdgeDifCm}
{20/12/2015 added PassSettings}
{04/01/2016 split wLinacSymSign}
{14/02/2016 replaced tmemorystream with tstringstream}
{18/03/2016 added wCheckRefIgnoreLinac}
{29/03/2016 added FRegisteredFiles, RegisteredFileTypes}
{12/05/2016 added wAxisPreserveOnExport}
{09/06/2016 implementation of dInflection in FitPenumbra}
{26/06/2016 added FNMPddFirst, FNMPddLast to limit range for error calculation}
{22/07/2016 replaced wCenterAtMax with wCenterDefinition}
{03/08/2016 wFMultiRefFreeList}
{29/09/2016 added FRefOrgFileName}
{13/10/2016 added StripExtension to function GetCurveIDstring}
{09/11/2016 wReferenceFromGeneric, FModBeamList added}
{18/11/2016 ReportDifferences}
{04/12/2016 added SyntheticProfile and GetEquivalentField}
{11/01/2017 wEdgeFallBackCm, FUserLevel}
{29/03/2017 wCenterDefinition: array[twSetFieldType] of twCenterType}
{30/03/2017 wNormalisation: array[twSetFieldType] of twNormalisation}
{23/11/2017 integrate function}
{27/11/2017 added function fieldcenter}
{05/12/2017 added wEdgeForce}
{15/12/2017 added wMultiScanLooping and ResetMultiScanCounters}
{05/01/2018 function name changed from GetPenumbra to GetPenumbraWidth}
{02/02/2018 MakeCurveName: ignorezerovalue}
{03/06/2018 FIndexingMode}
{11/06/2018 wRenormaliseData}
{10/02/2020 FRefOrg2D_OriVal added}
{27/04/2020 InitCurve added}
{01/05/2020 added transfer of BinaryData in Create}
{11/05/2020 added FModNormLocal,FModFilmLocal,FModBeamLocal}
{18/06/2020 added RevLogisticFunction, changed implementation of LogisticFunction to work with shift_cm}
{19/06/2020 added GetNormalisedRevLogistic}
{08/07/2020 added GetNormalisedSigmoidLevel}
{11/07/2020 new function FindEdge which was part of FindLevelPos}
{19/07/2020 added wSmallFielddetection,wSmallFieldLimitCm}
{21/07/2020 added wSmallFieldFilterDivider,GetAdjustedFilterWidthCm}
{24/07/2020 added wWedge90ShiftFactor}
{28/07/2020 added wFieldTypeDetection, removed wFFFdetection,wSmallFielddetection}
{18/08/2020 added wMRlinacTUlist}
{27/08/2020 added wTopModelRadiusCm}
{01/09/2020 added AliasListKeyApplied, ResetAliasList}
{03/09/2020 extended GetPenumbraWidth to produce also dynamic width}
{17/09/2020 introduction of FFrozen}
{29/09/2020 added PassRefOrg option to PassSettings}
{30/09/2020 added PassRefOrgData}
{09/10/2020 added EclipseData}
{20/10/2020 call to Analyse changed}
{16/11/2020 ADataTopLine}
{03/03/2021 added wDiagonalDetection, removed DetectDiagonalScans}
{07/03/2021 FDefaultSSDcm}
type
  TWellhoferData=class(TRadthData)
    private
     FActiveCnt      : Integer;         {business counter, should be 0 before admitting new data or analysis}
     FAliasList      : twcAliasArray;
     FAlignRef       : Boolean;
     FArrayScanRefOk : Boolean;
     FArrayScanRefUse: Boolean;
     FDefaultSSDcm   : twcFloatType;
     FAutoLoadRef    : Boolean;
     FCalcWidth_cm   : twcFloatType;
     FCentered       : Boolean;
     FFilterWidth    : twcFloatType;
     FLastFileType   : twcFileType;
     FLastMultiFile  : String;
     FFrozen         : Boolean;
     FMatchOverride  : Boolean;
     FModNormList    : TModNormList;
     FModNormLocal   : Boolean;
     FModFilmList    : TModFilmList;
     FModFilmLocal   : Boolean;
     FModBeamList    : TModTextList;
     FModBeamLocal   : Boolean;
     Fmcc            : TMccProfileData;
     FMultiScanList  : twcMultiScanList;                                        //list of reference file names in multiscan file, element 0 is file itself
     FPDDfit_simplex : TaNMsimplex;
     FNoPenumbraOk   : Boolean;
     FOrgExtension   : String;
     FPenumbraH      : twcFloatType;
     FPenumbraL      : twcFloatType;
     FReferenceDir   : String;
     FRefOrgSrc      : TStream;
     FRefOrgFileName : String;
     FRefOrgSrcType  : twcFileType;
     FRefOrg2D_OriVal: twcFloatType;                                             //preservere originvalue mcc-profile (when taken from other direction)
     FResampleGrid   : twcFloatType;
     FTempLevel      : twcFloatType;
     FUserLevel      : twcFloatType;
     FZeroStepsOk    : Boolean;
     FNMEdgeSource   : twcDataSource;
     FTimeRepSource  : twcDataSource;
     FNMEdgeFirst    : Integer;
     FNMEdgeLast     : Integer;
     FNMPddSource    : twcDataSource;
     FNMPddFirst     : Integer;
     FNMPddLast      : Integer;
     FNMPddFit       : twcNMpddFits;
     FNMpddscaling   : TaVertexDataType;
     FNMreset        : Boolean;
     FMultiRefIndex  : Boolean;
     FIndexingMode   : Boolean;
     function  ParseData(CheckFileTypeOnly            :Boolean      =False     ): Boolean; override;
     function  DualReadData(AStringList               :TStrings;
                           AStream                    :TStream;
                           AFileName                  :String;
                           ADataTopLine               :Integer      =0;
                           AFileFormat                :twcFileType  =twcUnknown): Boolean; override;
     function  Parse_Wellhofer_SNC_ascii                                        : Boolean;
     function  GetReady                                                         : Boolean;
     function  PrepareProfile                                                   : Boolean;
     function  CalcValue(Lpos,Rpos                    :Integer;                                      //BistroMath core function
                        X                             :twcFloatType;
                        ASource                       :twcDataSource=dsMeasured;
                        InverseCalc                   :Boolean      =False     ): twcFloatType;
     function  FindCalcRange(CalcPosCm                :twcFloatType;                                 //find range around posiiton
                             var Lpos,Rpos            :Integer;
                             ASource                  :twcDataSource=dsMeasured): Boolean; overload;
     function  FindCalcRange(ADataLevel               :twcFloatType;                                 //find range around level
                             NearestPos               :Integer;
                             ASide                    :twcSides;
                             var Lpos,Rpos            :Integer;
                             ASource                  :twcDataSource=dsMeasured): Boolean; overload;
     function  SigmoidFitErrorResult(var a            :TaFunctionVertex        ): TaVertexDataType;  //costfunction for edge fit
     function  TvSpddFunction(a                       :TaFunctionVertex;
                              cm                      :TaVertexDataType        ): TaVertexDataType;
     function  TvSpddFitErrorResult(var a             :TaFunctionVertex        ): TaVertexDataType;  //costfunction for pdd fit; passing by address is 1.8 µs per call faster
     function  PDDfitMaxErrorResult(var cm            :TaFunctionVertex        ): TaVertexDataType;  //costfunction for search of maximum
     function  GetRegisteredFileTypes                                           : String;
     procedure CheckDataOrdering(ASource              :twcDataSource=dsMeasured);
     function  SetScanType(AScanType                   :twcScanTypes;
                          ASource                     :twcDataSource=dsMeasured): Boolean;           //if adjusted, result is false
     procedure SetUserLevel(ADoseFraction             :twcFloatType            );
     procedure CopyParameters(var ASource,ADestination:twCurveDataRec          );
     procedure SetAxisID(AxisIDstg                    :String;
                         var AxisMapping              :twcTankAxisID;
                         var AxisSigns                :twcTankAxisSign         );
     procedure SetFieldGT(ASize                       :twcFloatType            );
     procedure SetFieldAB(ASize                       :twcFloatType            );
     procedure LoopReport(var AReport                 :NMReportRecord          );
     procedure TimeReport(var AReport                 :NMReportRecord          );
     procedure InitBorders(ASource                    :twcDataSource=dsMeasured;
                           InitFitData                :Boolean      =True      );
    public {when changing this list, also update procedure PassSettings}
      wApplyUserLevel           : Boolean;
      wApplySigmoidToBuffer     : Boolean;
      wAutoShiftCm              : array[twcMeasAxis] of Extended;
      wAxisPreserveOnExport     : Boolean;
      wCenterProfiles           : Boolean;
      wCenterDefinition         : array[twcFieldClass] of twcCenterType;
      wCheckRefCurveString      : Boolean;
      wCheckRefIgnoreLinac      : Boolean;
      wCurveInfo                : twCurveDesRec;
      wDefaultIgnoreSet         : twcIgnoreSet;
      wDetectorInfo             : twDetectorDesRec;
      wDiagonalDetection        : array[twcFieldClass] of Boolean;
      wEdgeDetect               : Boolean;                                      {if set or wedge found, a derivative is calculated for edge detection}
      wEdgeFallBackCm           : twcFloatType;                                 {when 50% level differs more, use edge}
      wEdgeMethod               : array[twcEdgeClass,twcFieldClass] of twcDoseLevel;
      wTakeCurrentRefSource     : Boolean;
      wEPenumbraH               : twcFloatType;
      wEPenumbraL               : twcFloatType;
      wFieldTypeDetection       : array[twcFieldClass] of Boolean;
      wFFFPeakDef               : twcFFFPeakType;
      wFFFMinDoseDifPerc        : twcFloatType;
      wFFFMinEdgeDifCm          : twcFloatType;
      wFullAnalysis             : Boolean;                                      {do limited analysis when false}
      wGeneralInfo              : twClinicRec;
      wGenericToPDD             : Boolean;
      wInflectionSigmoidRadiusCm: twcFloatType;
      wLinacSymSign             : array[twcFieldSizeDesc] of ShortInt;
      wLinacSymInnerRadiusCm    : twcFloatType;
      wLinacSymOuterRadiusCm    : twcFloatType;
      wMeas2TankMapping         : twcMeasAxisStg;
      wMeterInfo                : twElectrometerDesRec;
      wMultiScanNr              : Integer;                                      {1-based, support for multiple scans per file, see ptw}
      wMultiScanStep            : Integer;                                      {-1 or +1}
      wMultiScanMax             : Integer;
      wMultiScanLooping         : Boolean;
      wNormalisation            : array[twcFieldClass] of twcNormalisation;
      wNominalIFA               : Boolean;                                      //try to derive IFA from nominal field size
      w2D_ArrayRefList          : TStringList;                                  //list of acceptable 2D structured data devices
      wPipsPixelCm              : twcFloatType;
      wOutlierFilter            : Boolean;
      wRefPoint                 : twcCoordinate;
      wRefAlignPeak             : Boolean;
      wRefAtDefaultSSD          : Boolean;
      wReferenceFromGeneric     : Boolean;
      wRenormaliseData          : Boolean;                                      {renormalise original data to [source]twAbsNormVal}
      wResampleData             : Boolean;
      wSamePositionRadiusCm     : twcFloatType;
      wScaleSDD2SSD             : Boolean;
      wScale2DefaultSSD         : Boolean;
      wSmallFieldLimitCm        : twcFloatType;
      wSmallFieldFilterDivider  : twcFloatType;
      wTopModelRadiusCm         : array[twcFieldClass] of twcFloatType;
      wWedge90ShiftFactor       : twcFloatType;                                 //see FastScan
      wMRlinacTUlist            : String;                                       //see FastScan
      wMeasDeviceInfo           : twMeasDeviceRec;
      wSource                   : array[twcDataSource] of twCurveDataRec;
      wTryBinaryOnly            : Boolean;
      wUserAxisSign             : twcMeasAxisSign;   {wUserAxisSign is used in PrepareProfile to swap any axis direction to match the users display with other applications}
      wwmsHeader                : wmsFileHeadRec;
      wXPenumbraH               : twcFloatType;
      wXPenumbraL               : twcFloatType;
     constructor Create(AModalityNormList:TModNormList =nil;
                        AModalityFilmList:TModFilmList =nil;
                        AModalityBeamList:TModTextList =nil;
                        SharedParser     :toTNumParser =nil;
                        ParsedFile       :String       ='';
                        BinaryData       :TMemoryStream=nil;
                        BinaryFile       :String       ='';
                        AStatusProc      :toExtMsgProc =nil;
                        AIdentity        :String       ='bistromath'          );             reintroduce;
     procedure ConfigLoad(Sender                   :TObject                   );
     procedure ConfigSave(Sender                   :TObject                   );
     procedure PassSettings(var ADestination       :TWellhoferData;
                            AObjectCallSign        :String        ='';
                            PassRefOrg             :Boolean       =False      );
     procedure ReadConfig (CF                      :TConfigStrings=nil        );
     procedure WriteConfig(CF                      :TConfigStrings=nil        );
     procedure SetDefaults;                                                                  override;
     function  CheckAlternativeSource(ASource      :twcDataSource;
                                      AssureValid  :Boolean=False             ): twcDataSource;
     procedure CheckSize(var ASource               :twCurveDataRec;
                         NumPoints                 :Integer=-1);                              overload;
     procedure CheckSize(ASource                   :twcDataSource=dsMeasured;
                         NumPoints                 :Integer=-1);                              overload;
     procedure InitCurve(var ACurveRec             :twCurveDataRec);
     procedure ClearCurve(var ACurveRec            :twCurveDataRec;
                          CleanUp                  :Boolean=False             );               overload;
     procedure ClearCurve(ASource                  :twcDataSource;
                          CleanUp                  :Boolean=False             );               overload;
     procedure ResetAnalysis(ASource               :twcDataSource=dsMeasured  );
     procedure Purge;                                                           //will clear all, except dsMeasured,dsReference and overhead
     procedure ResetMultiScanCounters;
     procedure SetNumPoints(ASource                :twcDataSource;
                            MeasuredPoints         :wmsIntType=0              );
     function  IndexMultiScan(AFileName            :String='';
                              ACurveIDString       :String=''                 ): Boolean;
     function  EvaluateFileType(AIndentString      :String=''                 ): twcFileType;
     function  GetNumPoints                                                    : Integer;      override;
     function  GetSourceNumPoints(ASource          :twcDataSource=dsMeasured  ): wmsIntType;
     function  DosePoint2Value(DosePoint           :twcDoseLevel              ): twcFloatType;
     function  DosePoint2String(DosePoint          :twcDoseLevel              ): String;
     function  String2DosePoint(ADose              :String;
                                ADefault           :twcDoseLevel=dUser        ): twcDoseLevel;
     function  IsBinary(AFileName                  :String =''                ): Boolean;      override;
     function  GetFileType(AFileName               :String ='';
                           BinaryOnly              :Boolean=False             ): twcFileType;  override;
     function  GetFieldLength                                                  : twcFloatType; override;
     function  GetFieldDepth                                                   : twcFloatType; override;
     function  GetBeamType                                                     : twcBeamType;  override;
     function  GetFieldGT                                                      : twcFloatType;
     function  GetFieldAB                                                      : twcFloatType;
     function  GetFieldSize(ASource                :twcDataSource;
                            ADirection             :twcFieldSizeDesc          ): twcFloatType;
     function  GetEquivalentField(ASource          :twcDataSource=dsMeasured  ): twcFloatType;
     function  GetPenumbraValue(ASource            :twcDataSource;
                                ADoseLevel         :twcFloatType;
                                ASide              :twcSides                  ): twcFloatType; overload;
     function  GetPenumbraValue(ASource            :twcDataSource;
                                ADoseLevel         :twcDoseLevel;
                                ASide              :twcSides                  ): twcFloatType; overload;
     function  GetPenumbraValue(ASource            :twcDataSource;
                                ADoseLevel         :String;
                                ASide              :twcSides                  ): twcFloatType; overload;
     function  GetPenumbraWidth(ASource            :twcDataSource;
                                ASide              :twcSides;
                                DynamicWidth       :Boolean=False             ): twcFloatType;
     function  ReadRfb(AFileName                   :String                    ): Boolean;     overload;
     function  ReadRfb(AStream                     :TStream                   ): Boolean;     overload;
     function  ReadData(AStringList                :TStrings;
                        ADataTopLine               :Integer    =0;
                        AFileFormat                :twcFileType=twcUnknown    ): Boolean;     overload; override;
     function  ReadData(AStream                    :TStream;
                        ADataTopLine               :Integer    =0;
                        AFileFormat                :twcFileType=twcUnknown    ): Boolean;     overload; override;
     function  ReadData(AFileName                  :String;
                        ADataTopLine               :Integer    =0;
                        AFileFormat                :twcFileType=twcUnknown    ): Boolean;     overload; override;
     function  AdvReadData(AStringList             :TStrings;
                           ADataTopLine            :Integer       =0;
                           UnFreeze                :Boolean       =True;
                           ResampleData            :Boolean       =False;
                           CoordinateOrder         :twcMeasAxisStg=twcMeasAxisStandard;
                           AFileFormat             :twcFileType   =twcUnknown;
                           ASourceReference        :String        =''         ): Boolean;    overload;
     function  AdvReadData(AFileName               :String;
                           ADataTopLine            :Integer       =0;
                           UnFreeze                :Boolean       =True;
                           IsBinaryFile            :Boolean       =False;
                           ResampleData            :Boolean       =False;
                           CoordinateOrder         :twcMeasAxisStg=twcMeasAxisStandard;
                           AFileFormat             :twcFileType   =twcUnknown ): Boolean;   overload;
     function  AdvStreamData(AStream               :TStream;
                             ADataTopLine          :Integer       =0;
                             UnFreeze              :Boolean       =True;
                             ResampleData          :Boolean       =False;
                             CoordinateOrder       :twcMeasAxisStg=twcMeasAxisStandard;
                             AFileFormat           :twcFileType   =twcUnknown;
                             ASourceReference      :String=''                 ): Boolean;
     function  WriteData  (AFileName               :String;
                           AStringList             :TStrings;
                           ASource                 :twcDataSource =dsMeasured;
                           ClearList               :Boolean       =True       ): Boolean;   overload; override;
     function  WriteData  (AFileName               :String;
                           OutPutType              :twcFileType;
                           ASource                 :twcDataSource =dsMeasured;
                           SetExt                  :Boolean       =True       ): Boolean;   overload; override;
     function  GetCurveIDstring(ASource            :twcDataSource =dsMeasured;
                                StripExtension     :Boolean       =False      ): String;
     procedure ReportDifferences(ASource           :twcDataSource =dsMeasured;
                                 AReference        :twcDataSource =dsRefOrg   );
     function  LoadReference(AFileName             :String        ='';
                             SetCurrentAsRefSource :Boolean       =False      ): Boolean;
     function  ReferenceValid(AReference           :twcDataSource =dsReference): Boolean;
     function  ImportPipsProfile(Pips              :TPipsProfileData          ): Boolean;
     function  ImportEclipse(Eclipse               :TEclipseData              ): Boolean;
     function  ImportW2CADProfile(Aw2CAD           :Tw2CAD_data               ): Boolean;
     function  ImportSchusterProfile(Schuster      :TSchusterProfileData      ): Boolean;
     function  ImportHdfProfile(Hdf                :THdfProfileData           ): Boolean;
     function  ImportRfaProfile(Rfa                :TRfaProfileData           ): Boolean;
     function  ImportMccProfile(Mcc                :TMccProfileData           ): Boolean;
     function  ImportSNAProfile(SNA                :TICprofiler_ascii         ): Boolean;
     function  ImportWmsProfile(Wms                :TWmsData                  ): Boolean;
     function  ExportRfaProfile(Rfa                :TRfaProfileData;
                                ASource            :twcDataSource =dsMeasured;
                                ScalingFactor      :twcFloatType  =1.0        ): Boolean;
     function  ExportMccProfile(Mcc                :TMccProfileData;
                                ASource            :twcDataSource =dsMeasured;
                                ScalingFactor      :twcFloatType  =1.0        ): Boolean;
     function  ExportWmsProfile(Wms                :TWmsData;
                                ASource            :twcDataSource =dsMeasured;
                                ScalingFactor      :twcFloatType  =1.0        ): Boolean;
     function  IsRegisteredFileType(AFileName      :String                    ): Boolean;
     function  IsCompositeData(ASource             :twcDataSource =dsMeasured ): Boolean;
     function  GetPosition(ASource                 :twcDataSource;
                           I                       :Integer)                   : twcFloatType;
     function  DistanceToRefPoint(APoint           :twcCoordinate             ): twcFloatType;
     function  MakeCurveName(CreateMultiName       :Boolean       =False;
                             ApplyDeviceAlias      :Boolean       =False;
                             IgnoredParams         :twcIgnoreSet  =[];
                             IgnoreZeroValue       :Boolean       =True;
                             ASource               :twcDataSource =dsMeasured ): String;
     function  GetFieldWidthCm(ASource             :twcDataSource =dsMeasured;
                               ALevel              :twcDoseLevel  =d50        ): twcFloatType;
     function  GetFieldCenterCm(ASource            :twcDataSource =dsMeasured;
                                ALevel             :twcDoseLevel  =d50        ): twcFloatType;
     function  BordersValid(ASource                :twcDataSource =dsMeasured;
                            ALevel                 :twcDoseLevel  =d50        ): Boolean;
     function  GetLevelDistance(Level1,Level2      :twcDoseLevel;
                                ASide              :twcSides;
                                ASource            :twcDataSource =dsMeasured ): twcFloatType;
     function  FindLevelPos(ASource                :twcDataSource =dsMeasured;
                            ALevel                 :twcDoseLevel  =d50;
                            Symmetric              :Boolean       =True       ): Boolean;     //BistroMath core function
     function  NearestPosition(Position            :twcFloatType;                             //BistroMath core function
                               ASource             :twcDataSource =dsMeasured;
                               ForceAlwaysIn       :Boolean       =True       ): Integer;
     function  NextPos(APos                        :twcFloatType;                             //BistroMath core function
                       ASource                     :twcDataSource =dsMeasured ): twcFloatType;
     function  ResetValues(ASource                 :twcDataSource =dsMeasured ): Boolean;
     function  GetInterpolatedValue(Position       :twcFloatType;                             //BistroMath core function
                                    ASource        :twcDataSource =dsMeasured;
                                    DefaultValue   :twcFloatType  =0          ): twcFloatType;
     function  GetQfittedValue(Position            :twcFloatType;                             //BistroMath core function
                               ASource             :twcDataSource =dsMeasured;
                               DefaultValue        :twcFloatType  =0          ): twcFloatType;
     function  GetScaledQfValue(Position           :twcFloatType;                             //BistroMath core function
                                RelativeToCenter   :Boolean;
                                Scaling            :twcScalingType;
                                ASource            :twcDataSource =dsMeasured ): twcFloatType;
     procedure CopyCurve(var ASource,ADestination  :twCurveDataRec;
                         InitializeDestination     :Boolean       =False      );          overload;
     procedure CopyCurve(ASource,ADestination      :twcDataSource;
                         InitializeDestination     :Boolean       =False      );          overload;
     function  InsertPoint(X,Y                     :twcFloatType;
                           ASource                 :twcDataSource =dsMeasured ): Integer; overload;
     function  InsertPoint(X,Y                     :twcFloatType;
                           ACoordinate             :twcCoordinate;
                           ASource                 :twcDataSource =dsMeasured ): Integer; overload;
     function  Ionisation2Dose(ASource             :twcDataSource =dsMeasured;
                               ADestination        :twcDataSource =dsMeasured ): Boolean;
     function  Divide(ASource                      :twcDataSource =dsMeasured;
                      ADivisor                     :twcDataSource =dsReference;
                      ADestination                 :twcDataSource =dsCalculated;
                      AutoScaling                  :Boolean       =True;
                      NormFactor                   :twcFloatType  =1;
                      PreFilter                    :Boolean       =True;
                      PostFilter                   :Boolean       =True;
                      IsRelative                   :Boolean       =True       ): Boolean;
     function  SyntheticProfile(ASource            :twcDataSource =dsMeasured;
                                Divisor            :twcDataSource =dsReference;
                                AutoScaling        :Boolean       =True;
                                PreFilter          :Boolean       =True;
                                PostFilter         :Boolean       =True       ): Boolean;
     function  Match(ASource                       :twcDataSource =dsReference;
                     AReference                    :twcDataSource =dsMeasured;
                     ResultType                    :twcShiftType  =AbsShift;
                     AutoCorrect                   :Boolean       =True;
                     MatchLimitL                   :twcFloatType  =0;
                     MatchLimitR                   :twcFloatType  =0           ): twcFloatType;
     function  Integrate(FirstPosCm,LastPosCm      :twcFloatType;
                         ASource                   :twcDataSource =dsMeasured;
                         UseResampling             :Boolean       =False
                        {$IFDEF POSINTEGRAL};
                         PositionWeighted          :Boolean       =False
                        {$ENDIF}                                              ): twcFloatType;
     function  Derive(cm                           :twcFloatType  =twcDefaultValue;           //BistroMath core function
                      ASource                      :twcDataSource =dsMeasured;
                      ADestination                 :twcDataSource =dsCalculated;
                      PreFilter                    :Boolean       =False      ): twcFloatType;
     function  Mirror(ASource                      :twcDataSource =dsMeasured;
                      ADestination                 :twcDataSource =dsMeasured;
                      ARotationPoint               :twcFloatType  =0          ): Boolean;
     function  Merge(ASource                       :twcDataSource =dsUnrelated;
                     ADestination                  :twcDataSource =dsMeasured;
                     ShiftSourceCm                 :twcFloatType  =0;
                     DoMatch                       :Boolean       =True;
                     ScaleOverlap                  :Boolean       =True       ): Boolean;
     function  SigmoidPenumbraFit(ASource          :twcDataSource =dsMeasured;
                                  ApplyModel       :Boolean       =False;
                                  ADestination     :twcDataSource =dsMeasured ): Boolean;           //shiftCm is applied shift for model
     function  RawLogisticFunction(const a         :TaFunctionVertex;
                                   const cm        :TaVertexDataType;
                                   const shiftCm   :twcFloatType  =0          ): TaVertexDataType;  //needs multiplication with twFitNormalisation to represent twData
     function  RawLogisticDerivative(const a       :TaFunctionVertex;
                                     const cm      :TaVertexDataType;
                                     const shiftCm :twcFloatType=0): TaVertexDataType;
     function  RevRawLogisticFunction(const a      :TaFunctionVertex;
                                      const FxRaw  :TaVertexDataType;
                                      const shiftCm:twcFloatType=0            ): TaVertexDataType; //expects FxRaw scaled down with twFitNormalisation to match RawLogisticFunction
     function  GetNormalisedRevLogistic(ASide      :twcSides;
                                        ASource    :twcDataSource =dsMeasured;
                                        Apercentage:twcFloatType  =50         ):twcFloatType;
     function  GetNormalisedSigmoidLevel(cm        :twcFloatType;
                                         ASource   :twcDataSource =dsMeasured ): twcFloatType; //autoselect side
     function  SigmoidFitAvailable(ASource         :twcDataSource =dsMeasured ): Boolean;
     function  ApplySigmoidPenumbraFit(ASource     :twcDataSource =dsMeasured;
                                       ADestination:twcDataSource =dsMeasured ): Boolean;
     function  GammaAnalysis(ASource               :twcDataSource =dsMeasured;
                             AReference            :twcDataSource =dsReference;
                             ADestination          :twcDataSource =dsCalculated;
                             InFieldAreaOnly       :Boolean       =True;
                             AutoScaling           :Boolean       =True;
                             SourceScaling         :twcFloatType  =1;
                             PreFilter             :Boolean       =True;
                             PostFilter            :Boolean       =True       ): twcFloatType;
     function  NMpddmodelResult(ASource            :twcDataSource;
                                AfitVertex         :twcNMpddFits;
                                cm                 :TaVertexDataType          ): TaVertexDataType;
     function  OD2doseConversion(PreferedModality  :String        ='';
                                 PreferedFilmType  :String        ='';
                                 ASource           :twcDataSource =dsMeasured ): Boolean;
     procedure LoadAliasList(AliasConfigList       :TStrings                  );
     function  ApplyAliasList(AKey                 :string                    ): string;
     function  AliasListKeyApplied(AKey            :string                    ): Boolean;
     procedure ResetAliasList;
     function  ApplyModBeamList(ADefault           :string        ='';
                                AModality          :String        =''         ): string;
     function  FitReport(GiveFormula               :Boolean       =False;
                         ASource                   :twcDataSource =dsMeasured;
                         AReport                   :twcNMpddFits  =NM_Primary ): String;
     procedure Resample(StepCm                     :twcFloatType;
                        ASource                    :twcDataSource =dsMeasured;
                        ADestination               :twcDataSource =dsCalculated);
     procedure Add(ASource1                        :twcDataSource =dsMeasured;
                   ASource2                        :twcDataSource =dsReference;
                   ADestination                    :twcDataSource =dsCalculated;
                   Source2Scaling                  :twcFloatType  =1);
     procedure Multiply(Factor                     :twcFloatType;
                        ASource                    :twcDataSource =dsCalculated;
                        ADestination               :twcDataSource =dsCalculated);
     procedure PDDmaxNMFit(ASource                 :twcDataSource =dsMeasured;
                           AFit                    :twcNMpddFits  =NM_Primary );
     procedure QfitMaxPos(ASource                  :twcDataSource =dsMeasured;
                          ForceFitCenter           :Boolean       =False);
     function  Mayneord(SSD_org_cm,SSD_new_cm      :twcFloatType;                 {zero or negative values are replaced by known data}
                        Dmax_org_cm                :twcFloatType  =0;
                        ASource                    :twcDataSource =dsMeasured ): Boolean;
     procedure QuadFilter(cm                       :twcFloatType  =twcDefaultValue;
                          ASource                  :twcDataSource =dsMeasured;
                          ADestination             :twcDataSource =dsCalculated;
                          PostAnalysis             :Boolean       =False;
                          AllowRepeatedFiltering   :Boolean       =False;
                          ResetBorderValues        :Boolean       =True       );
     procedure MedianFilter(cm                     :twcFloatType  =twcDefaultValue;
                            ASource                :twcDataSource =dsMeasured;
                            ADestination           :twcDataSource =dsCalculated;
                            PostAnalysis           :Boolean       =False;
                            AllowRepeatedFiltering :Boolean       =False;
                            ResetBorderValues      :Boolean       =True       );
     procedure Shift(cm                            :twcFloatType  =0;
                     ShiftType                     :twcShiftType  =RelShift;
                     ASource                       :twcDataSource =dsMeasured );
     procedure AlignCurves(ASource                 :twcDataSource =dsReference;
                           AReference              :twcDataSource =dsMeasured );
     procedure SubtractBackground(AbsoluteValue    :twcFloatType  =0;
                                  ASource          :twcDataSource =dsMeasured );
     procedure CorrectSymmetry(ASource             :twcDataSource =dsMeasured;
                               PostAnalysis        :Boolean       =True       );
     procedure AddPoints(ASource                   :twcDataSource;
                         AddedPoints               :Integer       =1;
                         AtFront                   :Boolean       =False      );
     procedure PddFit(ASource                      :twcDataSource =dsMeasured;
                      ADestination                 :twcDataSource =dsCalculated);
     function  FindEdge(ASource                    :twcDataSource =dsMeasured ): Boolean;     //BistroMath core function
     procedure FastScan(ASource                    :twcDataSource =dsMeasured );              //BistroMath core function
     function  Analyse(ASource                     :twcDataSource =dsMeasured;                //BistroMath core function
                       AutoCenterProfile           :twcAutoCenter =AC_default ): Boolean;
     function  GetAdjustedFilterWidthCm(ASource    :twcDataSource =dsMeasured ): twcFloatType;
     procedure SetFilterWidth(cm                   :twcFloatType              );
     procedure SetResampleGrid(cm                  :twcFloatType              );
     procedure SetCalcWidth(cm                     :twcFloatType              );
     procedure SetNoPenumbraOk(ANoPenumbraOk       :Boolean                   );
     procedure SetZeroStepsOk(AZeroStepsOk         :Boolean                   );
     procedure AddDefaultAliasList(AliasConfigList :TStrings                  );
     procedure SetArrayScanRefOk(ASource           :twcDataSource =dsMeasured );
     procedure SetArrayScanRefUse(AState           :Boolean                   );
     procedure SetAutoLoadRef(AState               :Boolean                   );
     procedure SetReferenceDir(Directory           :String                    );
     procedure SetMultiRefIndex(AMultiRefIndex     :Boolean                   );
     function  SetReferenceOrg(ASource             :twcDataSource =dsMeasured;            //copy data to wsource[reforg]
                               KeepAsReference     :Boolean       =False;
                               AWellhofer          :TWellhoferData=nil        ): Boolean;
     function  TakeReferenceOrg(ACurveIDStg        :String        =''         ): Boolean;
     procedure UnSetReferenceOrg;                                                         //invalidate wsource[reforg]
     function  StopProcessing                                                  : Boolean;
     {$IFDEF WELLHOFER_DUMPDATA}
     procedure DumpData(const Info                 :String        ='';
                        ASource                    :twcDataSource =dsMeasured;
                        OriginSource               :twcSourceEnum =dsDefault  );
     {$ENDIF}
     destructor Destroy;                                                                         override;
    published
     property AcceptMissingPenumbra:Boolean         read FNoPenumbraOk    write SetNoPenumbraOk  default False;
     property AcceptZeroSteps      :Boolean         read FZeroStepsOk     write SetZeroStepsOk   default False;
     property AutoLoadReference    :Boolean         read FAutoLoadRef     write SetAutoLoadRef   default False;
     property AlignReference       :Boolean         read FALignRef        write FAlignRef        default True;
     property ArrayScanRefOk       :Boolean         read FArrayScanRefOk;
     property ArrayScanRefUse      :Boolean         read FArrayScanRefUse write SetArrayScanRefUse;
     property BeamType;
     property CalcWidth_cm         :twcFloatType    read FCalcWidth_cm    write SetCalcWidth;
     property Energy               :twcFloatType    read wSource[dsMeasured].twBeamInfo.twBEnergy;
     property FieldGT_cm           :twcFloatType    read GetFieldGT       write SetFieldGT;
     property FieldAB_cm           :twcFloatType    read GetFieldAB       write SetFieldAB;
     property FieldLength;
     property FieldDepth;
     property FileName;
     property FilterWidth          :twcFloatType    read FFilterWidth     write SetFilterWidth;
     property Freeze               :Boolean         read FFrozen          write FFrozen;
     property Identity;
     property IsValid              :Boolean         read wSource[dsMeasured].twValid;
     property LastDetectedFileType :twcFiletype     read FLastFiletype;
     property LastMessage;
     property ModalityNormList     :TModNormList    read FModNormList;
     property ModalityFilmList     :TModFilmList    read FModFilmList;
     property ModalityBeamList     :TModTextList    read FModBeamList;
     property MultiRefIndex        :Boolean         read FMultiRefIndex         write SetMultiRefIndex;
     property MatchOverride        :Boolean         read FMatchOverride         write FMatchOverride   default False;
     property PenumbraHi           :twcFloatType    read FPenumbraH;
     property PenumbraLo           :twcFloatType    read FPenumbraL;
     property EngineReady          :Boolean         read GetReady;
     property ReferenceDirectory   :String          read FReferenceDir          write SetReferenceDir;
     property RegisteredFileTypes  :String          read GetRegisteredFileTypes;
     property ResampleGridSize     :twcFloatType    read FResampleGrid          write SetResampleGrid;
     property ResetFit             :Boolean                                     write FNMreset;
     property ScanType;
     property UserBorderDoseLevel  :twcFloatType    read FUserLevel             write SetUserLevel;
     property Warning;
     property WedgeAngle           :SmallInt        read wSource[dsMeasured].twBeamInfo.twBWedge;
     property FormatOk;
    end;


{translation of level to more generic type}
function GetRelatedPositionType(ADoseLevel:twcDoseLevel): twcPositionUseType;


{global typed constants can be changed}
{06/01/2016 twcPddFitMubPower added}
{08/01/2016 twcPddFitMubPowerFixed added}
{12/01/2016 twcPddFitCostENRWeighted added}
{24/01/2016 twcSymCorrectionLevel added}
{07/07/2016 twcFFFRadiusCm, twcFFFInFieldExtCm added}
{18/01/2017 twcMatchInclusionLimit}
{13/06/2017 twcPDDminTopCm}
{31/05/2018 twcMccInsertOrigin}
{07/03/2021 twcDefaultSSD_MRcm}
const
  twcPddFitMubPower       :twcFloatType=   1.15; {amendment to pddfit model, 06/01/2015}
  twcPddFitMubPowerFixed  :Boolean     = False;  {fixed value}
  twcPddFitCostENRWeighted:Boolean     = True;
  twcGenericToElectron    :Boolean     = False;
  twcPddFitZWeightPower   :twcFloatType=   0;
  twcD20                  :twcFloatType=   0.2;
  twcD50                  :twcFloatType=   0.5;
  twcD80                  :twcFloatType=   0.8;
  twcD90                  :twcFloatType=   0.9;
  twcSamePositionRangeCm  :twcFloatType=   0.2;  {cm}
  twcYtopQfitRelDif       :twcFloatType=   0.01;
  twcNCSInFieldAxis       :twcFloatType=   0.8;
  twcNCSInFieldDiagonal   :twcFloatType=   0.7;
  twcSearchNoiseFactor    :twcFloatType=   1.1;
  twcMinNormVal           :twcFloatType=   0.00000001;
  twcSymCorrectionLimit   :twcFloatType=   2.0;  {maximum}
  twcSymCorrectionLevel   :twcFloatType=   0.1;  {minimal fraction of normval to apply symmetry correction}
  twcDefaultEnergy_MeV    :twcFloatType=   6.0;
  twcOmniPro7MinRatioPerc :twcFloatType=  10;    {ratio in omnipro_v7 data should exceed this level, otherwise normalisedfield is taken}
  twcDefaultSSDcm         :twcFloatType= 100.0;
  twcDefaultSSD_MRcm      :twcFloatType= 143.5;
  twcDeriveStatsBinDiv    :Integer     =  10;    {divide number of bands with this number; implications when in upper or lower region}
  twcDeriveStatsBinWDiv   :Integer     =  30;    {same but alleviated for wedged profiles}
  twcENRlimit             :twcFloatType=  2.0;
  twcNMseconds            :twcFloatType=  2.0;
  twcNMcycles             :Integer     =  0;
  twcNMrestarts           :Integer     =  6;
  twcNMdigits             :Integer     =  9;
  twcPDDpar               :array[pddfit_I1..pddfit_mx2] of Boolean= (True,True,True,True,True,True,True,True,True,True,True,True);
  twcDefaultICDstring     :String      ='-YX-Z';
  twcWMSdetInfo           :Integer     =  10;    {-1 or 0..10=ord wmsComments= (wmhG1,wmhG2,wmhP0,wmhP1,wmhP2,wmhP3,wmhP4,wmhU1,wmhU2,wmhU3,wmhU4)}
  twcDeriveMinMax         :twcFloatType=   0.90; {toegestane relatieve waarde van afgeleide in eerste en laatste punt}
  twcDeriveBinFraction    :twcFloatType=   0.25; {maximum voor grootse bin}
  twcDeriveLookAhead      :Integer     =   3;    {aantal punten dat vooruit gekeken wordt bij overschreiden bandlow en bandhigh}
  twcGammaCutoffDepth     :twcFloatType=   0.5;  {cm}
  twcGammaCutoffPercent   :twcFloatType=   5;    {%}
  twcGammaLocalDosePerc   :Boolean     = True;   {relative to local dose}
  twcGammaDosePercBase    :twcFloatType=   1.0;  {%}
  twcGammaDistCmBase      :twcFloatType=   0.1;  {cm}
  twcGammaDistCmStep      :twcFloatType=   0.02; {cm}
  twcGammaSearchMultiplier:twcFloatType=   5;    {limit search to Gamma-value at distance 0 multiplied with this factor}
  twcMaxRelMatchDif       :twcFloatType=  10;
  twcMatchRangeDivider    :Word        =   2;    {verkleiningsfactor range bij iteratie}
  twcMatchStepsNumber     :Word        =   6;    {aantal schuifstappen dat binnen -range .. +range gemaakt wordt}
  twcMatchNormDeltaPercent:twcFloatType=   2;    {grootte van delta-stap op normwaarde in %}
  twcMatchInclusionLimit  :twcFloatType=   0.8;
  twcOriginMinNormFraction:twcFloatType=   0.95;
  twcOutlierPointLimit    :Integer     =   7;
  twcPDDminTopCm          :twcFloatType=   0.3;
  twcExtBlackList         :String      ='.exe.ini.cnt.hlp.nld.eng.lnk';
  twcFieldShapeStg        :array[twcFieldShape     ] of String=('RECTANGULAR','BLOCKS','MLC','CIRCULAR');
  twcDataSourceNames      :array[twcSourceEnum     ] of String=('Measured','Reference','Filtered','RefFiltered','Calculated','Buffer','RefOrg','Unrelated'
                                                                {$IFDEF WELLHOFER_DUMPDATA},'Default'{$ENDIF});
  twcDoseLevelNames       :array[twcDoseLevel      ] of String=('dLow','dHigh','d20','d50','d80','d90','dUser','Derivative','Inflection','Sigmoid50','dTemp');
  twcCenterTypeNames      :array[twcCenterType     ] of String=('Border/Edge','Origin','Maximum');                                                                          {ordering critical for user interface}
  twcPositionUseNames     :array[twcPositionUseType] of String=('Border','Derivative','Inflection','Sigmoid50','Origin','Maximu','FFF Top','FFF slopes','Undefined','Configured');
  twcNormalisationNames   :array[twcNormalisation  ] of String=('Center','Origin','Maximum','In-Field area');
  twcMeasAxisNames        :array[twcMeasAxis       ] of String=('Inplane','Crossplane','Beam');
  twcFieldClassNames      :array[twcFieldClass     ] of String=('Standard','FFF','Small','MRlinac','Wedge','Electrons');
  twcFFFInFieldExtCm      :twcFloatType= 0.5;                                    //extention of slope from conventional "InField area"
  twcMccInsertOrigin      :Boolean     = False;

var
  twNumCPU                : Word;

const
  AppVersionString        :string='';


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

implementation

uses SysUtils, StrUtils,   DateUtils, lazFileUtils,
     TOtools,  TOfile;

{$IFDEF THREADED}
{28/07/2015
  Added CommonInit
  Added FStarts}
type
  twCalculationsType =(twCalcGamma,twCalcMatch,twCalcQuadFit);
  TMathThread=class(TThread)
  protected
    procedure Execute;                                                            override;
  public
    FActive               : Boolean;
    FWellhofer            : TWellhoferData;
    FSource1              : twcDataSource;
    FSource2              : twcDataSource;
    FDataPtr              : twcFloatArrayPtr;
    FDistance             : twcFloatType;
    FPoint                : Integer;
    FShiftCostFunctionStep: twcFloatType;
    FLeft                 : twcFloatType;
    FRight                : twcFloatType;
    FResult               : twcFloatType;
    FVertNorm             : twcFloatType;
    FSigned               : Boolean;
    FStatsSampler         : TStatssampler;
    FFit                  : TQuadFit;
    FCalculation          : twCalculationsType;
    FOk                   : Boolean;
    {$IFDEF THREAD_USESTATS}
    FStarts               : Integer;
    {$ENDIF}
    {$IFDEF THREAD_GAMMA}
    constructor Create(AWellhofer                         :TWellhoferData;
                       ASource,AReference                 :twcDataSource;
                       ADestinationPtr                    :twFloatArrayPtr;
                       AStatsSampler                      :TStatssampler);   reintroduce;  overload;  {gammawork}
    {$ENDIF}
    constructor Create(AWellhofer                         :TWellhoferData);  reintroduce;  overload;  {quadfitwork}
    procedure   CommonInit(AWellhofer                     :TWellhoferData);
    procedure   QuadFitWork(FilterWidth                   :twcFloatType;
                            ASource_and_Destination       :twcDataSource);
    {$IFDEF THREAD_GAMMA}
    procedure   GammaWork(Point                           :Integer);
    {$ENDIF}
    destructor  Destroy;                                                     override;
  end;

  twMathThreadList=array of TMathThread;
{$ENDIF}


function Fill_FieldDescrArr(AValue:twcFloatType): twcFieldDescrArr;
var i: twcFieldSizeDesc;
begin
for i:= fInplane to fCrossplane do
  Result[i]:= AValue;
end;


function CheckBlackList(const AFilename:String): Boolean;
var i: Integer;
begin
i:= AFilename.LastDelimiter('.')+1;
if i>0 then Result:= Pos(Copy(AFileName.ToLower,i,5),twcExtBlackList)=0
else        Result:= True;
end; {checkblacklist}


function GetRelatedPositionType(ADoseLevel:twcDoseLevel): twcPositionUseType;
begin
case ADoseLevel of
  dLow,dHigh,d20,d50,
  d80,d90,dUser,dTemp: Result:= dUseBorder;
  dDerivative        : Result:= dUseDerivative;
  dInflection        : Result:= dUseInflection;
  dSigmoid50         : Result:= dUseSigmoid50;
 else
                       Result:= dUseUndefined;
 end;
end;

//----------TModalityObject------------------------------------------------------


constructor TModalityObject.Create(AModality:String='');
begin
inherited Create;
Modality:= AModality;
end; {~create}


{19/01/2017}
procedure TModalityObject.Copy(ASource:TModalityObject);
begin
Modality:= ASource.Modality;
end; {~copy}


destructor TModalityObject.Destroy;
begin
Inherited;
end; {~destroy}


//----------TModalityList------------------------------------------------------


constructor TModalityList.Create(AStatusProc:toExtMsgProc=nil);
begin
inherited Create;
SetStatusProcedure(AStatusProc);
end; {~create}


function TModalityList.ModalityFormat(AModality:twcModalityChar;
                                      AEnergy  :twcFloatType): String;
begin
if not (AModality in twcModalitySet) then
  AModality:= 'X';
Result:= Format(twcModalityFormat,[AModality,AEnergy]);
end; {~modalityformat}


function TModalityList.GetModData(Index:Integer): TModalityObject;
begin
if InRange(Index,0,High(FData)) then Result:= FData[Index]
else                                 Result:= nil;
end; {~getmoddata}


function TModalityList.GetCommaText(Index:Integer): String;
begin
Result:= Data[Index].Modality;
end; {~getcommatext}


function TModalityList.GetDivisorText(Index:Integer): String;
begin
Result:= StringReplace(GetCommaText(Index),',','|',[rfReplaceAll]);
end; {~getdivisortext}


function TModalityList.GetModDataCount: Integer;
begin
Result:= Length(FData);
end; {~getmoddatacount}

{$push}{$warn 5024 off: Parameter "ASeparator" not used}
function TModalityList.AddModData(ACommaText:String;
                                  ASeparator:Char=','): Integer;
var b: Boolean;
begin
Result:= Length(FData);
b     := False;
while (b=False) and (Result>0) do {check for record with same modality}
  begin
  Dec(Result);
  b:= FData[Result].Modality=ACommaText;
  end;
if b then
  try
    FreeAndNil(FData[Result]);
  except
    ExceptMessage('TModalityList.AddModData!');
  end
else
  begin
  Result:= Length(FData);
  SetLength(FData,Succ(Result));
  end;
end; {~addmoddata}
{$pop}

{07/11/2016}
{19/01/2017 free last element (j) of FData, not FData[Index]}
function TModalityList.DelModData(Index:Integer): Boolean;
var j: Integer;
begin
Result:= (Index>=0) and (Index<DataCount);
if Result then
  begin
  j:= Pred(DataCount);
  while Index<j do
    begin
    FData[Index].Copy(FData[Succ(Index)]);
    Inc(Index);
    end;
  try
    FreeAndNil(FData[j]);
   except
    ExceptMessage('TModalityList.DelModData!');
   end;
  SetLength(FData,j)
  end;
end; {~delmoddata}


function TModalityList.DelModData(AModality:String): Boolean;
begin
Result:= DelModData(FindModData(AModality));
end; {~delmoddata}


function TModalityList.FindModData(AModality :String;
                                   var RefObj:TModalityObject): Boolean;
var i: Integer;
begin
i:= FindModData(AModality);
Result:= i>=0;
if Result then RefObj:= Data[i]
else           RefObj:= nil;
end; {~findmoddata}


function TModalityList.FindModData(AModality:String): Integer;
var i: Integer;
    b: Boolean;
begin
i:= Length(FData);
b:= False;
if i>0 then
  repeat
    Dec(i);
    b:= (FData[i].Modality=AModality);
  until b or (i=0);
if b then Result:= i
else      Result:= -1;
end; {~findmoddata}


function TModalityList.GetModalityList: String;
var i: Integer;
    s: String;
begin
i:= DataCount;
s:= '';
while i>0 do
  begin
  Dec(i);
  with Data[i] do
    if length(s)=0            then s:= Modality
    else if Pos(Modality,s)=0 then s:= s+','+Modality;
  end;
Result:= s;
end; {~getmodalitylist}


procedure TModalityList.ClearModData;
var i: Integer;
begin
i:= Length(FData);
while i>0 do
  begin
  Dec(i);
  try
    FreeAndNil(FData[i]);
   except
    ExceptMessage('TModalityList.ClearModData!');
   end;
  end;
Finalize(FData);
end; {~clearmoddata}


procedure TModalityList.SetStatusProcedure(AStatusProc:toExtMsgProc=nil);
begin
FStatusProc:= AStatusProc;
end; {~setstatusprocedure}


procedure TModalityList.ExceptMessage(AString:String);
begin
if Assigned(StatusProcedure) then
  StatusProcedure(AString);
end; {~exceptmessage}


destructor TModalityList.Destroy;
begin
ClearModData;
Inherited;
end; {~destroy}


//----------TModalityNorm------------------------------------------------------


constructor TModalityNorm.Create(AModality:String='');
begin
inherited Create(AModality);
end; {~create}


{19/01/2017}
procedure TModalityNorm.Copy(ASource:TModalityNorm);
begin
inherited Copy(ASource);
NormRec:= ASource.NormRec;
end; {~copy}


destructor TModalityNorm.Destroy;
begin
Inherited;
end; {~destroy}


//----------TModalityNormList------------------------------------------------------


constructor TModNormList.Create(AStatusProc:toExtMsgProc=nil);
begin
inherited Create(AStatusProc);
end; {~create}


function TModNormList.GetModValue(AModality:String;
                                  AbsValue :Boolean=True): twcFloatType;
var i: integer;
begin
if assigned(FData) then
  i:= inherited FindModData(AModality)
else
  i:= -1;
if i<0 then Result:= -999
else        Result:= TModalityNorm(Data[i]).NormRec.Value[AbsValue];
end; {~getmodvalue}


function TModNormList.GetModDepth(AModality:String;
                                  AbsDepth :Boolean    =True;
                                  ZeroValue:twcFloatType=0    ): twcFloatType;
var i: integer;
begin
if assigned(FData) then
  i:= inherited FindModData(AModality)
else
  i:= -1;
if i<0 then
  Result:= Zerovalue
else
  begin
  Result:= TModalityNorm(Data[i]).NormRec.Depth[AbsDepth];
  if Result=0 then
    Result:= ZeroValue;
  end;
end; {~getmoddepth}


{24/05/2017}
function TModNormList.AddModData(ACommaText:String;
                                 ASeparator:Char=','): Integer;
var p: toTNumParser;
    s: string;
    c: Char;
    b: Boolean;
    r: TModalityNorm;
begin
p:= toTNumParser.Create;
p.SetDecPointChars([DefaultFormatSettings.DecimalSeparator]);
p.CurrentLine:= ACommaText;
c:= UpCase(p.CurrentLine[1]);
s:= ModalityFormat(c,p.NextFloat);
if p.ConversionResult then
  begin
  r            := TModalityNorm.Create(s);
  Result       := inherited AddModData(s,ASeparator);
  FData[Result]:= r;
  with r.NormRec do
     for b:= False to True do
       begin
       Depth[b]:= p.NextFloat(1000,0   ,False);
       Value[b]:= p.NextFloat(1000,0.01,False);
       end;
  end
else Result:= -1;
try
  FreeAndNil(p);
 except
  ExceptMessage('TModNormList.AddModData!');
 end;
end; {~addmoddata}


function TModNormList.FindModData(AModality :String;
                                  var RefObj:TModalityNorm): Boolean;
var r: TModalityObject;
begin
r     := nil;
Result:= Inherited FindModData(AModality,r);
RefObj:= TModalityNorm(r);
end; {~findmoddata}


function TModNormList.GetCommaText(Index:Integer):String;
var a: Boolean;
    s: String;
begin
s:= '';
if Index<DataCount then with Data[Index] as TModalityNorm do
  with NormRec do
    begin
    for a:= False to True do
       s:= Format('%s,%0.3f,%0.3f',[s,Depth[a],Value[a]]);
    s:= Format('%s%s',[Modality,s]);
    end;
Result:= s;
end; {~getcommatext}


destructor TModNormList.Destroy;
begin
Inherited;
end; {~destroy}


//----------TModalityFilm------------------------------------------------------


constructor TModalityFilm.Create(AModality:String='');
begin
inherited Create(AModality);
end; {~create}


{19/01/2017}
procedure TModalityFilm.Copy(ASource:TModalityFilm);
begin
inherited Copy(ASource);
FilmRec:= ASource.FilmRec;
end; {~copy}


destructor TModalityFilm.Destroy;
begin
Inherited;
end; {~destroy}


//----------TModalityFilmList------------------------------------------------------


constructor TModFilmList.Create(AStatusProc:toExtMsgProc=nil);
begin
inherited Create(AStatusProc);
end; {~create}


function TModFilmList.GetFilmTypeList: String;
var i: Integer;
    s: String;
begin
i:= DataCount;
s:= '';
while i>0 do
  begin
  Dec(i);
  with Data[i] as TModalityFilm do
    if length(s)=0                    then s:= FilmRec.FilmType
    else if Pos(FilmRec.FilmType,s)=0 then s:= s+','+FilmRec.FilmType;
  end;
Result:= s;
end; {~getfilmtypelist}


function TModFilmList.AddModData(ACommaText:String;
                                 ASeparator:Char=','): Integer;
var p: toTNumParser;
    s: string;
    c: Char;
    i: Integer;
    r: TModalityFilm;
begin
if ACommaText.CountChar(ASeparator)=7 then Insert(',100'   ,ACommaText,Pos(ASeparator,ACommaText));  {patch <1.96}
if ACommaText.CountChar(ASeparator)=8 then Insert(',100,10',ACommaText,Pos(ASeparator,ACommaText));  {patch <1.97}
p:= toTNumParser.Create;
p.SetDecPointChars([DefaultFormatSettings.DecimalSeparator]);
p.CurrentLine:= ACommaText;
c:= UpCase(p.CurrentLine[1]);
s:= ModalityFormat(c,p.NextFloat);
if p.ConversionResult then
  begin
  r            := TModalityFilm.Create(s);
  Result       := inherited AddModData(s,ASeparator);
  FData[Result]:= r;
  with r.FilmRec do
    begin
    if ACommaText.CountChar(ASeparator)=10 then
      for i:= 1 to 4 do
        p.NextFloat; {patch <3.01, stepping over depth dose normalisation data}
    p.NextChar; {step over separator, otherwise string will be empty}
    FilmType:= p.NextString([',','|']-[DefaultFormatSettings.DecimalSeparator]);
    for i:= 1 to twcOD2doseNumPar do
      if p.ConversionResult then
        OD2dose[i]:= p.NextFloat(1e7); {read OD to dose parameters}
    if OD2dose[twcOD2doseNumPar]<=0 then
      OD2dose[twcOD2doseNumPar]:= 10;
    end;
  end
else Result:= -1;
try
  FreeAndNil(p);
 except
  ExceptMessage('TModFilmList.AddModData!');
 end;
end; {~addmoddata}


function TModFilmList.DelModData(AModality:String;
                                 AFilmType:String): Boolean;
begin
Result:= DelModData(FindModData(AModality,AFilmType));
end; {~delmoddata}


function TModFilmList.FindModData(AModality :String;
                                  AFilmType :String;
                                  var RefObj:TModalityFilm): Boolean;
var i: Integer;
begin
i:= FindModData(AModality,AFilmType);
Result:= i>=0;
if Result then RefObj:= TModalityFilm(GetModData(i))
else           RefObj:= nil;
end; {~findmoddata}


function TModFilmList.FindModData(AModality:String;
                                  AFilmType:String): Integer;
var i: Integer;
    b: Boolean;
begin
i:= DataCount;
b:= False;
if i>0 then
  repeat
    Dec(i);
    with Data[i] as TModalityFilm  do
      b:= (Modality=AModality) and ((AFilmType='') or (FilmRec.FilmType=AFilmType));
  until b or (i=0);
if b then Result:=  i
else      Result:= -1;
end; {~findmoddata}


function TModFilmList.GetCommaText(Index:Integer):String;
var i: Integer;
    s: String;
begin
s:= '';
if Index<DataCount then with Data[Index] as TModalityFilm do
  begin
  for i:= 1 to twcOD2doseNumPar do
    s:= Format('%s,%0.3f',[s,FilmRec.OD2dose[i]]);
  s:= Format('%s,%s%s',[Modality,FilmRec.FilmType,s]);
  end;
Result:= s;
end; {~getcommatext}


destructor TModFilmList.Destroy;
begin
ClearModData;
Inherited;
end; {~destroy}


//----------TModalityText------------------------------------------------------


constructor TModalityText.Create(AModality:String='');
begin
inherited Create(AModality);
end; {~create}


{19/01/2017}
procedure TModalityText.Copy(ASource:TModalityText);
begin
inherited Copy(ASource);
Value:= ASource.Value;
end; {~copy}


destructor TModalityText.Destroy;
begin
Inherited;
end; {~destroy}


//----------TModalityTextList------------------------------------------------------


constructor TModTextList.Create(AStatusProc:toExtMsgProc=nil);
begin
inherited Create(AStatusProc);
end; {~create}


function TModTextList.GetModValue(AModality:String): String;
var i: integer;
begin
i:= Inherited FindModData(AModality);
if i<0 then Result:= ''
else        Result:= TModalityText(Data[i]).Value;
end; {~getmodvalue}


{24/05/2017}
function TModTextList.AddModData(ACommaText:String;
                                 ASeparator:Char=','): Integer;
var p: toTNumParser;
    s: String;
    c: Char;
    r: TModalityText;
begin
p:= toTNumParser.Create;
p.SetDecPointChars([DefaultFormatSettings.DecimalSeparator]);
p.CurrentLine:= ACommaText;
c:= UpCase(p.CurrentLine[1]);
s:= ModalityFormat(c,p.NextFloat);
if p.ConversionResult then
  begin
  p.NextChar;
  r            := TModalityText.Create(s);
  Result       := inherited AddModData(s,ASeparator);
  FData[Result]:= r;
  r.Value      := p.RemainderOfLine;
  end
else
  Result:= -1;
try
  FreeAndNil(p);
 except
  ExceptMessage('TModTextList.AddModData!');
 end;
end; {~addmoddata}


function TModTextList.FindModData(AModality :String;
                                  var RefObj:TModalityText): Boolean;
var r: TModalityObject;
begin
r:= nil;
Result:= Inherited FindModData(AModality,r);
RefObj:= TModalityText(r);
end; {~findmoddata}


function TModTextList.GetCommaText(Index:Integer):String;
begin
with Data[Index] as TModalityText do
  Result:= Format('%s,%s',[Modality,Value]);
end; {~getcommatext}


destructor TModTextList.Destroy;
begin
Inherited;
end; {~destroy}


//----------TRadthData----------------------------------------------------------

{09/12/2015 added sharedparser}
{29/03/2016 added FRegisteredFiles}
{27/09/2016 FBinaryAllowed}
{01/05/2020 added transfer of BinaryData}
{16/05/2020 added FMultiScanCapable}
constructor TRadthData.Create(SharedParser:toTNumParser =nil;
                              ParsedFile  :String       ='';
                              BinaryData  :TMemoryStream=nil;
                              BinaryFile  :String       ='';
                              AStatusProc :toExtMsgProc =nil;
                              AIdentity   :String       ='base');
begin
SetDefaults;
FRegisteredFiles := '';
LogLevel         := 1;

BinStream        := TMemoryStream.Create;
if BinaryData<>nil then
  begin
  BinStream.CopyFrom(BinaryData,0);
  BinStream.Seek(0,soFromBeginning);
  end;
FMultiScanCapable:= False;
BinStreamType    := twcUnknown;
BinStreamFile    := BinaryFile;
FBinaryAllowed   := False;
FIdentity        := AIdentity;
ShowWarning      := False;
ScanNr           := 1;
ScanNrOk         := 0;
ScanMax          := 0;
AutoDecimalPoint := False;
AutoDecimalList  := '.,';
ObjectCallSign   := 'primary';
StatusProcedure  := AStatusProc;
FLocalParser     := (SharedParser=nil);
FFilename        := ParsedFile;
FParserTopLine   := 0;
if FLocalParser then
  begin
  FParser:= toTNumParser.Create;
  FParser.SetDecPointChars(['.']);
  end
else
  FParser:= SharedParser;
end; {~create}


{10/05/2016 ErrorState added}
{14/01/2020 prioritymessage}
procedure TRadthData.SetDefaults;
begin
FileFormat       := twcUnknown;
UndefinedVal     := 0;
FLastMessage     := '';
FScanType        := snUndefined;
Linac            := twcDefUnknown;
ScanAngle        := 0;
IdentificationStg:= '';
Warning          := '';
DefaultExtension := '.txt';
FileTime         := Now;
IsFile           := False;
ErrorState       := False;
PriorityMessage  := '';
end; {~setdefaults}


{14/08/2016}
{22/08/2016 LoadFromFile does not guarantee position of file pointer; use seek}
function TRadthData.LoadBinStream(AFileName:String): Boolean;
begin
Result:= BinStreamFile=AFileName;
if Result then
  BinStream.Seek(0,soFromBeginning)
else
  begin
  Result:= FileExists(AFileName);
  if Result then
    begin
    BinStream.LoadFromFile(AFileName);
    BinStream.Seek(0,soFromBeginning);
    BinStreamType := twcUnknown;
    BinStreamFile:= AFilename;
    end;
  end;
end; {~loadbinstream}


function TRadthData.GetDistance(c1,c2:twcCoordinate): twcFloatType;
begin
Result:= VectorLength(c1.t[X],c1.t[Y],c1.t[Z],c2.t[X],c2.t[Y],c2.t[Z]);
end; {getdistance}


procedure TRadthData.ShiftPoint(var p :twcCoordinate;
                                AShift:twcCoordinate);
var m: twcMeasAxis;
begin
for m:= Inplane to Beam do
  p.m[m]:= p.m[m]+AShift.m[m];
end; {shiftpoint}


(*   GetScanDirection
This is based on the OmniPro v6 definition of the scanangle and axis directions
Note that the user interface might swap the letters as needed.
*)
{11/09/2018 swapped relation between scanangle 45/135 and GA/TA}
function TRadthData.GetScanDirection(ASide:twcSides): twcMeasAxisStg;
var Stg: String[4];
    i  : Byte;
begin
case FScanType of
  snGT           : Stg:= 'GT';
  snAB           : Stg:= 'AB';
  snPDD,snFanLine: Stg:= 'UD';
  snAngle        : Stg:= ifthen(ScanAngle<90,'TAGB','GATB');
 else              Stg:= 'LR';
 end;
i     := Length(Stg) div 2;
Result:= Copy(Stg,Succ(Ord(ASide)*i),i);
end; {~getscandirection}


{08/09/2015}
{13/08/2016 InsertIdentity}
{05/03/2021 do not combine warning with lastmessage}
function TRadthData.GetLastMessage: string;
begin
if Length(Warning)>0 then
  begin
  Result := 'Warning: '+Warning;
  Warning:= '';
  end
else
  Result:= FLastMessage;
if Length(Result)>0 then
  Result:= InsertIdentity(Result);
end; {~getlastmessage}


{15/08/2016}
procedure TRadthData.TransferLastMessage(var AMessage:String);
begin
if Length(FLastMessage)>0 then
  begin
  AMessage:= FLastMessage;
  StatusMessage(AMessage);
  end;
end; {~transferlastmessage}


{13/08/2016}
function TRadthData.InsertIdentity(AMessage:String=''): String;
begin
if (Length(AMessage)=0) or (AMessage[1]<>'[') then
  begin
  if ScanMax>1 then Result:= Format(' %d/%d',[ScanNr,ScanMax])
  else              Result:= '';
  Result:= Format('[%s%s] %s',[FIdentity,Result,AMessage]);
  end
else
  Result:= AMessage;
end; {~insertidentity}


function TRadthData.GetNumPoints: Integer;
begin
Result:= UndefinedInt;
end; {~getnumpoints}


function TRadthData.GetBeamType: twcBeamType;
begin
Result:= Other;
end; {~getbeamtype}


//does only sort out file type without setting IdentificationStg or FileFormat
{01/05/2020 assign only if filename differs}
{19/08/2020 speed improvement with BinaryOnly}
function TRadthData.GetFileType(AFileName :String ='';
                                BinaryOnly:Boolean=False): twcFileType;
var sFt: twcFileType;
begin
sFt   := FileFormat;
Result:= sFt;
if (Result=twcUnknown) and (not BinaryOnly) then with FParser do
  begin
  if AFileName<>FileName then
    Assign(AFileName);
  ParseData(True);
  Result    := FileFormat;
  FileFormat:= sFt;
  end;
end; {~getfiletype}


function TRadthData.GetFieldLength: twcFloatType;
begin
Result:= UndefinedVal;
end; {~getfieldlength}


function TRadthData.GetFieldDepth: twcFloatType;
begin
Result:= UndefinedVal;
end; {~getfieldlength}


{$push}{$warn 5024 off: Parameter not used}
function TRadthData.IsBinary(AFileName:String=''): Boolean;
begin
Result:= False;
end; {~isbinary}
{$pop}


procedure TRadthData.SetLogLevel(ALevel:Word=1);
begin
FLogLevel:= Min(ALevel,4);
if FLogLevel=0 then
  StatusMessage('',True,0);
end; {~setloglevel}


procedure TRadthData.SetStatusProcedure(AStatusProc:toExtMsgProc=nil);
begin
FStatusProc:= AStatusProc;
end; {~setstatusprocedure}


{$push}{$warn 5092 off: Variable does not seem to be initialized}
{17/06/2020 add loglevel to FStatusProc}
{20/08/2020 pass only new messages}
{14/01/2020 prioritymessage}
procedure TRadthData.StatusMessage(AMessage         :String;
                                   UpdateLastMessage:Boolean=True;
                                   MinLevel         :ShortInt=1);
begin
if MinLevel<0 then
  begin
  PriorityMessage:= AMessage;
  MinLevel       := LogLevel;
  end;
if (LogLevel>=MinLevel) and ((AMessage<>FLastMessage) or (LogLevel>3)) then
  begin
  if UpdateLastMessage then
    FLastMessage:= AMessage;
  if LogLevel>3 then
    AMessage:= AMessage+FormatDateTime(' hh:mm:ss.zzz',Now);
  if FStatusProc<>nil then
    FStatusProc(AMessage,MinLevel);
  end;
end; {~statusmessage}
{$pop}


procedure TRadthData.AddWarning(AWarning:String);
begin
if AWarning=''                                     then FWarning:= ''
else if ShowWarning and (Pos(AWarning,FWarning)=0) then FWarning:= FWarning+#32+AWarning;
end; {~addwarning}


procedure TRadthData.ExceptMessage(AString:String);
begin
if Assigned(StatusProcedure) then
  StatusProcedure(AString);
end; {~exceptmessage}


{10/08/2020 take only filename of parser when not empty}
function TRadthData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
begin
Result:= CheckData(nil) or CheckFileTypeOnly;
if Length(Parser.FileName)>0 then
  FileName:= Parser.FileName;
end; {~parsedata}


{16/11/2020 support for multiple complete data sets in one single file intended for one single scan}
function TRadthData.FindMoreData(FromCurrentLine:Boolean=False): Boolean;
begin
if assigned(FParser) then
  with FParser do
    begin
    if not FromCurrentLine then
      NextLine(True);
    while (Length(Trim(CurrentLine))=0) and (not EndOfFile) do
      NextLine(True);
    Result:= (Length(Trim(CurrentLine))>0) and (not EndOfFile);
    end
else
  Result:= False;
end; {~findmoredata}


function TRadthData.CheckData(AStringList:TStrings): Boolean;
var i: Integer;
begin
if assigned(AStringList) then
  begin
  i                := AStringList.Count;
  IdentificationStg:= AStringList.Strings[0];
  end
else with FParser do
  begin
  i:= LineCount;
  GotoTop(True,False);
  IdentificationStg:= CurrentLine;
  end;
Result:= i>=twcDefMinProfilePoints;
end; {~checkdata}


{15/12/2015 AStream}
{14/02/2016 replaced tmemorystream with tstringstream}
{27/09/2016 FBinaryAllowed}
{16/11/2020 ADataTopLine}
function TRadthData.DualReadData(AStringList :TStrings;
                                 AStream     :TStream;
                                 AFileName   :String;
                                 ADataTopLine:Integer    =0;
                                 AFileFormat :twcFileType=twcUnknown): Boolean;
begin
IsFile:= (AStringList=nil) and (AStream=nil);
Result:= (not IsFile) or FileExists(AFileName);
if Result and CheckBlackList(AFileName) then
  begin
  if IsFile then
    Result:= ReadData(AFileName  ,ADataTopLine,AFileFormat)
  else if (FBinaryAllowed or (not assigned(AStringList))) and assigned(AStream) then
    Result:= ReadData(AStream    ,ADataTopLine,AFileFormat)
  else
   Result:= ReadData(AStringList,ADataTopLine,AFileFormat);
  end;
end; {~dualreaddata}


{27/09/2016}
function TRadthData.ReadBinData: Boolean;
begin
Result:= False;
end; {~readbindata}


{15/12/2015}
{14/02/2016 replaced tmemorystream with tstringstream}
{21/07/2016 more general support for streams}
{27/09/2016 base level support for bin streams}
{18/04/2020 In FPC 3.2.0 TStringSream is descendant of TMemoryStream; the latter was completely separate <= FPC 3.0.4}
{16/11/2020 ADataTopLine}
function TRadthData.ReadData(AStream     :TStream;
                             ADataTopLine:Integer    =0;
                             AFileFormat :twcFileType=twcUnknown): Boolean;
begin
SetDefaults;
FileFormat:= AFileFormat;
if not (AStream is TStringStream) then
  begin
  if AStream.Size>0 then
    BinStream.CopyFrom(AStream,0);
  Result:= ReadBindata;
  end
else
  begin
  if (FLocalParser or (Parser.LineCount=0)) and (AStream is TStringStream) then
    begin
    AStream.Position:= 0;
    Parser.Assign(TStringStream(AStream));
    end;
  FParser.SetTop(ADataTopLine);
  FParserTopLine:= ADataTopLine;
  Result        := ParseData;
  end;
end; {~readdata}


{because TStrings is input, the data are in some ascii-format}
{16/11/2020 ADataTopLine}
function TRadthData.ReadData(AStringList :TStrings;
                             ADataTopLine:Integer    =0;
                             AFileFormat :twcFileType=twcUnknown): Boolean;
begin
SetDefaults;
FileFormat    := AFileFormat;
FParserTopLine:= ADataTopLine;
if LogLevel>2 then
  StatusMessage(Format('Reading(%s) {%s} ',[FIdentity,ifthen(assigned(AStringList),AStringList.Strings[0],'')]),True,3);
with FParser do
  begin
  if FLocalParser or (FParser.LineCount=0) then
    Assign(AStringList,True);
  SetTop(ADataTopLine);
  Result:= ParseData;
  end;
end; {~readdata}


{$push}{$I-}
{16/11/2020 ADataTopLine}
function TRadthData.ReadData(AFileName   :String;
                             ADataTopLine:Integer    =0;
                             AFileFormat :twcFileType=twcUnknown): Boolean;
begin
SetDefaults;
FileFormat:= AFileFormat;
Result    := CheckBlackList(AFileName) and FileExists(AFileName);
if Result then
  begin
  if LogLevel>2 then
    StatusMessage(Format('Reading(%s) %s...',[FIdentity,AFileName]),True,3);
  FileTime      := FileDateToDateTime(FileAge(AFileName));
  FFileName     := AFileName;
  FParserTopLine:= ADataTopLine;
  with FParser do
    begin
    Assign(AFileName);
    SetTop(ADataTopLine);
    Result:= ParseData;
    end;
  end;
end; {~readdata}
{$pop}


{$push}{$warn 5024 off: Parameter "ASource" not used}
{26/09/2016 changed order}
function TRadthData.WriteData(AFileName  :String;
                              AStringList:TStrings;
                              ASource    :twcDataSource=dsMeasured;
                              ClearList  :Boolean     =True    ): Boolean;
begin
FFileName:= AFileName;
Result   := False;
Parser.Clear;
if assigned(AStringList) and ClearList then
  AStringList.Clear;
end; {~writedata}
{$pop}


{$push}{$warn 5024 off: Parameter "ASource" not used}
function TRadthData.WriteData(AFileName:String;
                              Binary   :Boolean        =True;
                              ASource  :twcDataSource   =dsMeasured;
                              SetExt   :Boolean        =True): Boolean;
var T: TStringList;
    S: TFileStream;
begin
FFileName:= AFileName;
if SetExt then
  FFileName:= ChangeFileExt(FileName,DefaultExtension);
if Binary then
  Result:= False
else
  begin
  T     := TStringList.Create;
  Result:= WriteData(FileName,T,ASource);
  if Result then
    begin
    S:= TFileStream.Create(Filename,fmCreate,fmShareDenyNone);
    try
      T.SaveToStream(S);
     finally
      try
        FreeAndNil(S);
       except
        ExceptMessage('TRadthData.WriteData(S)!');
       end;
     end; {try}
    end;
  try
    FreeAndNil(T);
   except
    ExceptMessage('TRadthData.WriteData(T)!');
   end;
  end;
end; {~writedata}
{$pop}


{$push}{$warn 5024 off: Parameter not used}
function TRadthData.WriteData(AFileName :String;
                              OutPutType:twcFileType;
                              ASource   :twcDataSource=dsMeasured;
                              SetExt    :Boolean=True          ): Boolean;
begin
FFileName:= AFileName;
Result   := False;
end; {~writedata}
{$pop}


{01/07/2015
The introduction of FFormatOk makes the distinction between correct
data but with too few data points possible.
This helps to retain the correct conclusions on the file type.}
{13/08/2016 InsertIdentity}
{$push}{$warn 5092 off}
function TRadthData.ReadResults(PostText:String=''): Boolean;
begin
if Length(PostText)>0 then
  PostText:= ' '+PostText;
FFormatOk:= FParseOk;
if not FParseOk then with FParser do
  StatusMessage(Format(twForParseError,[LastLineOkNumber,LastLineOk+' ('+SearchText+ErrorString+')',PostText]))
else
  begin
  FParseOk:= (GetNumPoints>twcDefMinProfilePoints);
  if FParseOk then StatusMessage(InsertIdentity(Format(twForParseRead,[GetNumPoints,PostText])))
  else             StatusMessage(InsertIdentity(Format(twForMinPoints,[GetNumPoints,twcDefMinProfilePoints,PostText])));
end;
Result:= FParseOk;
end; {~readresults}
{$pop}

{14/08/2016 binstream}
destructor TRadthData.Destroy;
begin
try
  FreeAndNil(BinStream);
 except
  ExceptMessage('TRadthData.Destroy:BinStream!');
 end;
Finalize(FExtraText);
if FLocalParser then
  try
    FreeAndNil(FParser);
   except
      ExceptMessage('TRadthData.Destroy:FParser!');
   end;
FStatusProc:= nil;
end; {~destroy}

//----------TRfaProfileData-----------------------------------------------------

{09/12/2015 added sharedparser}
{29/03/2016 added FRegisteredFiles}
{01/05/2020 transfer of binarydata and filenames}
{16/05/2020 added FMultiScanCapable}
constructor TRfaProfileData.Create(SharedParser:toTNumParser =nil;
                                   ParsedFile  :String       ='';
                                   BinaryData  :TMemoryStream=nil;
                                   BinaryFile  :String       ='';
		                   AStatusProc :toExtMsgProc =nil;
                                   AIdentity   :String       ='RFA300');
begin
Inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
Initialize(RfaData.rfaCoordinates_cm);
Initialize(RfaData.rfaValues);
FMultiScanCapable:= True;
FRegisteredFiles := '.asc';
end; {~create}


procedure TRfaProfileData.SetDefaults;
begin
Inherited;
DefaultExtension:= '.asc';
with RfaHeader do
  begin
  rfaMSR:= 1;
  rfaSYS:= 'BDS';
  end;
with RfaData do
  begin
  rfaStart_Meas_cm        := Default(twcCoordinate);
  rfaModType              := 'RAT';
  rfaFileType             := 'SCN';
  rfaScanType             := 'PRO';
  rfaDetType              := 'ION';
  rfaDate                 :=  Now;
  rfaField_mm[fInplane   ]:=  100;
  rfaField_mm[fCrossplane]:=  100;
  rfaRadiation            := ifthen(twcGenericToElectron,'ELE','PHO');
  rfaEnergy_MV            :=    6;
  rfaSSD_mm               := Round(twcDefaultSSDcm*10);
  rfaBUP_01mm             :=    0;
  rfaBRD_mm               := 1000;
  rfaFShape               :=    1;
  rfaAccessory            :=    0;
  rfaWedge_deg            :=    0;
  rfaGantry               :=    0;
  rfaCollimator           :=    0;
  rfaMeasType             :=    2;
  rfaDepth_01mm           :=  100;
  rfaPoints               :=    0;
  rfaEnd_Meas_cm          := rfaStart_Meas_cm;
  rfaComments[0]          := '';
  rfaComments[1]          := '';
  end;
end; {~setdefaults}


{03/06/2020 check for illegal values}
function TRfaProfileData.MakeTimeString(ADateTime:TDateTime): String;
begin
Result:= FormatDateTime('dd-mmm-yyyy',Math.Min(ADateTime,200000));
end; {~maketimestring}


{$push}{$warn 5057 off:Local variable "s" does not seem to be initialized}
{15/08/2016 BinStream implementation}
{19/08/2020 speed improvement with BinaryOnly}
function TRfaProfileData.GetFileType(AFileName :String ='';
                                     BinaryOnly:Boolean=False): twcFileType;
type CStg=record case Boolean of
            True :(Stg  : String[Length(rfaID)]);
            False:(Bytes: array[0..Length(rfaID)] of Byte);
          end;
var s: Cstg;
    n: Byte;
begin
Result:= Inherited GetFileType(AFileName,BinaryOnly);
if (Result=twcUnknown)  and (not BinaryOnly) and LoadBinStream(AFileName) then
  begin
  n:= BinStream.Read(s.Bytes[1],Length(rfaID));
  s.Bytes[0]:= n;
  if s.Stg=rfaID then
    Result:= twcRFA_ascii;
  BinStreamType:= Result;
  end;
end; {~getfiletype}
{$pop}


function TRfaProfileData.CheckData(AStringList:TStrings): Boolean;
begin
Result:= (inherited CheckData(AStringList)) and (Pos(rfaNumMeasID,IdentificationStg)=1);
if Result then with FParser do
  begin
  GotoTop(True);
  FileFormat:= twcRFA_ascii;
  ScanMax   := NextInteger;
  end;
end; {~checkdata}


{$push}{$warn 5057 off}
{16/12/2105 added scannr to readresults}
{20/03/2017 range checking applied}
{06/10/2020 fundamentals alternative}
function TRfaProfileData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
var i,CommentCnt: Integer;
    Handled     : Boolean;

  function SearchLine(ASearchText:String): String;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText,False,True);
    Result := RemainderOfLine;
    end
  else
    Result:= '';
  end;

  function RfaTest(RfaText:String): Boolean;
  begin
  Result:= ifthen(FParseOk,Pos(RfaText,FParser.CurrentLine),0)>0;
 {$IFDEF COMPILED_DEBUG}
  if Result then
    StatusMessage(Format('->RFA[%d] %s',[Parser.CurrentLineNumber,FParser.CurrentLine]));
 {$ENDIF}
  end;

  procedure RfaFill(RfaText       :String;
                    var Coordinate:twcCoordinate);         overload;
  var mAxis: twcMeasAxis;
  begin
  if RfaTest(RfaText) then with FParser do
    begin
    for mAxis:= Inplane to Beam do
      if FParseOk then
        begin
        Coordinate.m[mAxis]:= NextFloat(1e4)/10;
        FParseOk           := ConversionResult;
        end;
    if FParseOk then
      Handled:= NextLine;
    end;
  end;

  procedure RfaFill(RfaText     :String;
                    var AInteger:Integer;
                    MinLimit    :Integer=-32000;
                    MaxLimit    :Integer= 32000);         overload;
  begin
  try
    if RfaTest(RfaText) then
      begin
      AInteger:= FParser.NextInteger(MinLimit,MaxLimit);
      if not FParser.ConversionResult then
        Handled:= False;
      Handled := FParser.NextLine;
      end;
   except
    AInteger:= 0;
   end;
  end;

  procedure RfaFill(RfaText   :String;
                    var AFloat:twcFloatType;
                    MaxLimit  :twcFloatType=1e6);          overload;
  begin
  try
    if RfaTest(RfaText) then
      begin
      AFloat:= FParser.NextFloat(MaxLimit);
      if not FParser.ConversionResult then
        Handled:= False;
      Handled:= FParser.NextLine;
      end;
   except
    AFloat:= 0;
   end;
  end;

  procedure RfaFill(RfaText:String;
                    var A  :array of Integer);            overload;
  var i: Word;
  begin
  i:= 0;
  try
    if RfaTest(RfaText) then
      repeat
        A[i]:= FParser.NextInteger;
        Inc(i);
      until (i=Length(A)) or (not FParser.GetConversionResult);
   except
    FillChar(A,Length(A),0);
   end;
  if i>0 then
    Handled:= FParser.NextLine;
  end;

  procedure RfaFill(RfaText  :String;
                    var ADate:TDateTime;
                    asTime   :Boolean=False);       overload;
  begin
  if RfaTest(RfaText) then with FParser do
    begin
    FParseOk:= Search(RfaText,False,True);
    if asTime then ADate:= NextDate('HMS')+ADate
    else           ADate:= NextDate('mdy');
    Handled:= FParser.NextLine;
    end;
  end;

  procedure RfaFill(RfaText     :String;
                    var Arfa_ID :rfa_ID;
                    LineComplete:Boolean=True);       overload;
  var Stg: String;
  begin
  if RfaTest(RfaText) then
    begin
    FParser.Search(RfaText,True,True,False);
    Stg:= Parser.RemainderOfLine.TrimLeft(['%',chTab,chSpace]);
    Stg:= Copy(Stg,1,Min(Length(Stg),rfa_IDlen));
    if Length(Stg)>0 then
      ARfa_ID:= Stg;
    if LineComplete then
      Handled:= FParser.NextLine;
    end;
  end;

  procedure RfaFill(RfaText    :String;
                    var Arfa_ID:rfa_ID;
                    var AFloat :twcFloatType);         overload;
  begin
  if RfaTest(RfaText) then
    begin
    RfaFill(RfaText,ARfa_ID,False);
    AFloat := FParser.NextFloat(1e5);
    Handled:= FParser.NextLine;
    end;
  end;

  procedure RfaFill(RfaText    :String;
                    var AString:String);              overload;
  begin
  if RfaTest(RfaText) then
    begin
    FParseOk:= FParser.Search(RfaText,True,True);
    AString := TrimLeft(FParser.RemainderOfLine);
    Handled := FParser.NextLine;
    end;
  end;

  {******data order and directions are GT [mm], AB [mm], UP [mm], Dose [%]*****
   no swapping needed}

  procedure RfaFill(RfaText        :String;
                    var Coordinates:twcCoordArray;
                    var Values     :twcFloatArray);    overload;
  var ACoordinate: twcCoordinate;
      AValue     : twcFloatType;
      mAxis      : twcMeasAxis;
      i          : Integer;
  begin
  with FParser do
    while RfaTest(RfaText) do
      begin
      ACoordinate:= Default(twcCoordinate);
      for mAxis:= Inplane to Beam do
        if FParseOk then
          begin
          ACoordinate.m[mAxis]:= NextFloat(1e4)/10;
          FParseOk            := ConversionResult;
          end;
      if FParseOk then
        begin
        AValue := FParser.NextFloat(1e6);
        FParseOk:= ConversionResult;
        if FParseOk then
          begin
          i:= Length(Coordinates);
          SetLength(Coordinates,Succ(i));
          SetLength(Values     ,Succ(i));
          Coordinates[i]:= ACoordinate;
          Values[i]     := AValue;
          end;
        end;
      Handled:= FParser.NextLine;
      end;
  end;

  function SetScan(AScanNr:Integer): Integer;
  begin
  Result:= 0;
  repeat
    FParseOk:= FParser.Search(rfaMeasNumberID);           {# Measurement number 	1}
    if FParseOk then
       Result:= FParser.NextInteger(0,10000);
  until (not FParseOk) or (Result=AScanNr);
  FParseOk                 := (i>=0);
  FParser.CurrentLineNumber:= FParser.LastLineOkNumber;
  end;

begin
FParseOk:= inherited ParseData(CheckFileTypeOnly) and (CheckFileTypeOnly xor (FileFormat=twcRfa_ascii));
if FParseOk and (not CheckFileTypeOnly) and (FileFormat=twcRFA_ascii) then with FParser,RfaHeader do
  begin
  CommentCnt:= 0;
  rfaSys    := TrimLeft(SearchLine(rfaSystemID));  {:SYS BDS 0 # Beam Data Scanner System}
  FParseOk  := Length(rfaSys)>0;
  if FParseOk then
    begin
    if ScanMax=0 then
      begin
      ScanMax:= SetScan(0);
      GotoTop;
      end;
    i:= SetScan(EnsureRange(ScanNr,1,ScanMax));
    with RfaData do
      begin
      ScanNr := i;
      Handled:= False;
      repeat
      Handled:= Handled and (Length(FParser.CurrentLine)>1);
      if not Handled then
        repeat
          NextLine;
        until Length(FParser.CurrentLine)>1;
        Handled:= False;
        if FParseOk and not (RfaTest(rfaEndMeasID) or RfaTest(rfaEndFileID)) then
          begin
          RfaFill(rfaModID                         ,rfaModType);                  {%MOD 	RAT}
          RfaFill(rfaFileID                        ,rfaFileType);                 {%TYP 	SCN}
          RfaFill(rfaScanID                        ,rfaScanType);                 {%SCN 	DPT}
          RfaFill(rfaDetID                         ,rfaDetType);                  {%FLD 	ION}
          RfaFill(rfaDateID                        ,rfaDate);                     {%DAT 	12-09-2011}
          RfaFill(rfaTimeID                        ,rfaDate,True);                {%TIM 	13:53:07}
          RfaFill(rfaFSizeID                       ,rfaField_mm);                 {%FSZ 	100	100}
          RfaFill(rfaRadiationID                   ,rfaRadiation,rfaEnergy_MV);   {%BMT 	PHO	    6.0}
          RfaFill(rfaSSDID                         ,rfaSSD_mm,0);                 {%SSD 	1000}
          RfaFill(rfaBUPID                         ,rfaBUP_01mm);                 {%BUP 	0}
          RfaFill(rfaBRDID                         ,rfaBRD_mm,0);                 {%BRD 	1000}
          RfaFill(rfaFShapeID                      ,rfaFShape);                   {%FSH 	-1}
          RfaFill(rfaAccessoryID                   ,rfaAccessory);                {%ASC 	0}
          RfaFill(rfaWedgeID                       ,rfaWedge_deg,0);              {%WEG 	0}
          RfaFill(rfaGantryID                      ,rfaGantry,360);               {%GPO 	0}
          RfaFill(rfaCollimatorID                  ,rfaCollimator,360);           {%CPO 	0}
          RfaFill(rfaMeasID                        ,rfaMeasType);                 {%MEA 	1}
          RfaFill(rfaDepthID                       ,rfaDepth_01mm,1e4);           {%PRD 	0}
          RfaFill(rfaPointsID                      ,rfaPoints,2);                 {%PTS 	141}
          RfaFill(rfaStartScanID                   ,rfaStart_Meas_cm);            {%STS 	    0.0	    0.0  160.0}
          RfaFill(rfaEndScanID                     ,rfaEnd_Meas_cm);              {%ETS 	    0.0	    0.0  -10}
          if CommentCnt<2 then
            begin
            RfaFill(rfaMeasInfoID                  ,rfaComments[CommentCnt]);     {!}
            if Length(rfaComments[CommentCnt])>0 then
              Inc(CommentCnt);
            end;
          while RfaTest('#') and FParseOk do
            NextLine;
          RfaFill(rfaDataID                        ,rfaCoordinates_cm,rfaValues); {=}
          end;
        if LogLevel>3 then
          StatusMessage(Format('%d:%s',[FParser.CurrentLineNumber,FParser.CurrentLine]),False,4);
      until (not FParseOk) or RfaTest(rfaEndMeasID) or RfaTest(rfaEndFileID);
      end; {with}
    end; {if FParseOk}
  end; {if FParseOk and fileformat}
Result:= FParseOk and (CheckFileTypeOnly or ReadResults);
end; {~parsedata}
{$pop}


function TRfaProfileData.ReadResults(PostText:String=''): Boolean;
begin
Result:= Inherited ReadResults(PostText);
if Result then
  ScanNrOk:= ScanNr;
end; {~readresults}


function TRfaProfileData.GetNumPoints: Integer;
begin
Result:= Length(RfaData.rfaValues);
end; {~getnumpoints}


{$push}{$warn 5057 off}
function TRfaProfileData.GetProfile(Index:Integer): twcGrafPoint;

var p: twcGrafPoint;
begin
if InRange(Index,0,Pred(GetNumPoints)) then with RfaData do
   begin
   p.X:= GetDistance(rfaStart_Meas_cm,rfaCoordinates_cm[Index]);
   p.Y:= rfaValues[Index];
   end
else
   p:= Default(twcGrafPoint);
Result:= p;
end; {~get_profile}
{$pop}


procedure TRfaProfileData.PutProfile(Index:Integer;
                                     Point:twcGrafPoint);
begin
if InRange(Index,0,Pred(GetNumPoints)) then with RfaData do
  rfaValues[Index]:= Point.Y;
end; {~putprofile}


{29/10/2016 chTab before dose value in data line}
function TRfaProfileData.WriteData(AFileName  :String;
                                   AStringList:TStrings;
                                   ASource    :twcDataSource=dsMeasured;
                                   ClearList  :Boolean     =True     ): Boolean;
var i: Integer;

  procedure WriteRfaString(ALabel,AStg:String);
  begin
  AStringList.Append(ALabel+ifthen(Length(AStg)>0,chSpace+chTab,'')+Astg);
  end;

  {29/10/2016 extra space in format string removed}
  procedure WriteRfaInteger(ALabel:String;
                            AValue:Integer);
  begin
  AStringList.Append(Format('%s%s%d',[ALabel,chTab,AValue]));
  end;

  procedure WriteRfaFloat(ALabel:String;
                          AValue:twcFloatType);
  begin
  AStringList.Append(Format('%s%s%0.2f',[ALabel,chTab,AValue]));
  end;

  procedure WriteRfaComment(AStg  :String='';
                            IDchar:Char='#');
  begin
  AStringList.Append(IDchar+ifthen(Length(AStg)>0,chSpace,'')+Astg);
  end;

  function RfaCoordinateStg(ACoordinate:twcCoordinate): String;
  begin
  with ACoordinate do Result:= Format('%7.2f%s%7.2f%s%7.2f',[m[Inplane]*10,chTab,m[Crossplane]*10,chTab,m[Beam]*10]);
  end;

begin
inherited WriteData(AFileName,AStringList,ASource,ClearList);
with AStringList,RfaData do
  begin
  Clear;
  Result:= True;
  try
    WriteRfaString(rfaNumMeasID   ,'1'           );   {:MSR 	10	 # No. of measurements in file}
    WriteRfaString(rfaSystemID+' BDS 0',''       );   {:SYS BDS 0 # Beam Data Scanner System}
    WriteRfaComment;
    WriteRfaComment('RFA300 ASCII Measurement Dump ( BDS format )');
    WriteRfaComment;
    WriteRfaComment(rfaMeasNumberID+' 1');
    WriteRfaComment;
    WriteRfaString('%VNR'          ,'1.0'        );   {%VNR 1.0}
    WriteRfaString(rfaModID        ,rfaModType   );   {%MOD RAT}
    WriteRfaString(rfaFileID       ,rfaFileType  );   {%TYP SCN}
    WriteRfaString(rfaScanID       ,rfaScanType  );   {%SCN PRO}
    WriteRfaString(rfaDetID        ,rfaDetType   );   {%FLD ION}
    WriteRfaString(rfaDateID       ,Format('%0.2d-%0.2d-%0.4d',[MonthOf(rfaDate),DayOf(rfaDate)   ,YearOf(rfaDate)         ])); {%DAT 	03-27-2008}
    WriteRfaString(rfaTimeID       ,Format('%0.2d:%0.2d:%0.2d',[HourOf(rfaDate) ,MinuteOf(rfaDate),SecondOf(rfaDate)       ])); {%TIM 	15:57:33}
    WriteRfaString(rfaFSizeID      ,Format('%d%s%d'      ,[rfaField_mm[fInplane],chTab            ,rfaField_mm[fCrossplane]])); {%FSZ 	400	400}
    WriteRfaString(rfaRadiationID  ,Format('%s%s%0.1f'   ,[rfaRadiation         ,chTab            ,rfaEnergy_MV            ])); {%BMT 	PHO 	    6.0}
    WriteRfaInteger(rfaSSDID       ,rfaSSD_mm    );   {%SSD 	1000}
    WriteRfaInteger(rfaBUPID       ,rfaBUP_01mm  );   {%BUP 	0}
    WriteRfaInteger(rfaBRDID       ,rfaBRD_mm    );   {%BRD 	1000}
    WriteRfaInteger(rfaFShapeID    ,rfaFShape    );   {%FSH 	-1}
    WriteRfaInteger(rfaAccessoryID ,rfaAccessory );   {%ASC 	0}
    WriteRfaInteger(rfaWedgeID     ,rfaWedge_deg );   {%WEG 	0}
    WriteRfaInteger(rfaGantryID    ,rfaGantry    );   {%GPO 	0}
    WriteRfaInteger(rfaCollimatorID,rfaCollimator);   {%CPO 	90}
    WriteRfaInteger(rfaMeasID      ,rfaMeasType  );   {%MEA 	2}
    WriteRfaFloat(rfaDepthID       ,rfaDepth_01mm);   {%PRD 	160}
    WriteRfaInteger(rfaPointsID    ,rfaPoints    );   {%PTS 	637}
    WriteRfaString(rfaStartScanID  ,RfaCoordinateStg(rfaStart_Meas_cm));   {%STS 	    0.0	 -236.6	   16.0}
    WriteRfaString(rfaEndScanID    ,RfaCoordinateStg(rfaEnd_Meas_cm  ));   {%EDS 	    0.0	  242.9	   16.0}
    for i:= 0 to 1 do
      WriteRfaComment(rfaComments[i],rfaMeasInfoID);
    WriteRfaComment('X      Y      Z     Dose');
    for i:= 0 to Pred(GetNumPoints) do
      WriteRfaString(rfaDataID,Format('%s%s%8.1f',[RfaCoordinateStg(rfaCoordinates_cm[i]),chTab,rfaValues[i]]));
    WriteRfaString(rfaEndMeasID    ,''           );   {:EOM  # End of Measurement}
    WriteRfaString(rfaEndFileID    ,''           );   {:EOF # End of File}
    if Length(FExtraText)>0 then
      for i:= 0 to Pred(Length(FExtraText)) do
        WriteRfaComment(FExtraText[i]);
   except
     Result:= False;
   end;
  end;
end; {~writedata}


function TRfaProfileData.WriteData(AFileName :String;
                                   OutPutType:twcFileType;
                                   ASource   :twcDataSource=dsMeasured;
                                   SetExt    :Boolean=True          ): Boolean;
begin
if OutPutType=FileFormat then Result:= WriteData(AFileName,False,ASource,SetExt)
else                          Result:= False;
end; {~writedata}


destructor TRfaProfileData.Destroy;
begin
with RfaData do
  begin
  Finalize(rfaCoordinates_cm);
  Finalize(rfaValues);
  end;
Inherited;
end; {~destroy}

//----------TICprofiler_ascii-----------------------------------------------------

{29/03/2016 added FRegisteredFiles}
{01/05/2020 transfer of binarydata and filenames}
{16/05/2020 added FMultiScanCapable}
constructor TICprofiler_ascii.Create(SharedParser:toTNumParser =nil;
                                     ParsedFile  :String       ='';
                                     BinaryData  :TMemoryStream=nil;
                                     BinaryFile  :String       ='';
                                     AStatusProc :toExtMsgProc =nil;
                                     AIdentity   :String       ='SNA');
begin
Inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
FMultiScanCapable:= True;
FRegisteredFiles := '.txt';
end; {~create}


{21/08/2015}
procedure TICprofiler_ascii.SetDefaults;
begin
Inherited;
DefaultExtension:= '.txt';
FScanType       := snPlane;
icpClinic       := '';
icpCollimator   :=   0;
icpComment      := '';
icpEnergy       :=   0;
icpGantry       :=   0;
icpMeasTime     := Now;
icpModel        :=  '';
if twcGenericToElectron then icpModality:= 'E'
else                         icpModality:= 'X';
icpOrientation  := 'Y';
icpSetupAngle   :=   0;
icpScanLine     := icXline;
icpSSDcm        := 100;
icpWedgeAngle   :=   0;
icpPosGAOffsCm  := Default(twcCoordinate);
icpFieldCm      := Default(twcFieldDescrArr);
SetLength(icpPosCm,0);
SetLength(icpData ,0);
FillChar(icpCollimCm   ,SizeOf(icpCollimCm   ),0);
FillChar(icpFirstPoint ,SizeOf(icpFirstPoint ),0);
end; {~setdefaults}


{20/08/2015}
function TICprofiler_ascii.MakeTimeString(ADateTime:TDateTime): String;
begin
Result:= FormatDateTime('dd-mm-yyyy hh:mm:ss',ADateTime);
end; {~maketimestring}


{20/08/2015}
{15/08/2016 BinStream implementation}
{30/03/2017 'Version:'+#9 added as valid identification}
{19/08/2020 speed improvement with BinaryOnly}
{$push}{$warn 5057 off:Local variable "s" does not seem to be initialized}
function TICprofiler_ascii.GetFileType(AFileName :String ='';
                                       BinaryOnly:Boolean=False): twcFileType;
const IdentLen=Length(wICPAIdentStg);
type CStg=record case Boolean of
            True :(Stg  : String[IdentLen]);
            False:(Bytes: array[0..IdentLen] of Byte);
          end;
var s: Cstg;
    n: Byte;
begin
Result:= Inherited GetFileType(AFileName,BinaryOnly);
if (Result=twcUnknown) and (not BinaryOnly) and LoadBinStream(AFileName) then
  begin
  n:= BinStream.Read(s.Bytes[1],IdentLen);
  s.Bytes[0]:= n;
  if Pos(wICPAIdentStg,s.Stg)=1 then
    Result:= twcICprofilerAscii;
  BinStreamType:= Result;
  end;
end; {~getfiletype}
{$pop}

{21/08/2015}
function TICprofiler_ascii.CheckData(AStringList:TStrings): Boolean;
begin
Result:= (inherited CheckData(AStringList)) and (Pos(wICPAIdentStg,IdentificationStg)=1);
if Result then
  FileFormat:= twcICprofilerAscii;
end; {~checkdata}


{21/08/2015}
{16/12/2105 added scannr to readresults}
{20/03/2017 range checking applied}
{31/03/2017 ignore frame data after clipboard array data}
{06/10/2020 fundamentals alternative}
function TICprofiler_ascii.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
var Handled: Boolean;

  function w2Test(w2Text       :String;
                  AtStartOfLine:Boolean=True): Boolean;
  var i: Integer;
  begin
  Result:= FParseOk;
  if Result then
    begin
    i:= Pos(w2Text,FParser.CurrentLine);
    if AtStartOfLine then Result:= i=1
    else                  Result:= i>=1;
    end;
    //if Result then StatusMessage(Format('->SNA[%d] %s',[Parser.CurrentLineNumber,FParser.CurrentLine]));
  end;

  procedure w2Fill(w2Text      :String;
                   var AInteger:Integer;
                   MinLimit    :Integer=0;
                   MaxLimit    :Integer=10000);          overload;
  begin
  try
    if w2Test(w2Text) then
      begin
      AInteger:= FParser.NextInteger(MinLimit,MaxLimit);
      Handled := FParser.NextLine;
      end;
   except
    AInteger:= 0;
   end;
  end;

  procedure w2Fill(w2Text   :String;
                   var AChar:twcModalityChar);                overload;
  begin
  try
    if w2Test(w2Text) then
      begin
      if      Pos('tron' ,FParser.CurrentLine)>0 then AChar:= 'E'
      else if Pos('roton',FParser.CurrentLine)>0 then AChar:= 'P'
      else                                            AChar:= 'X';
      Handled:= FParser.NextLine;
      end;
   except
    AChar:= 'X';
   end;
  end;

  procedure w2Fill(w2Text:String;
                   var A :twcFieldDescrArr);            overload;
  var i: Word;
  begin
  try
    i:= 0;
    if w2Test(w2Text) then
      begin
      repeat
        A[twcFieldSizeDesc(i)]:= FParser.NextFloat;
        Inc(i);
      until (i=Length(A)) or (not FParser.GetConversionResult);
      Handled:= FParser.GetConversionResult;
      FParser.NextLine;
      end;
   except
    A:= Default(twcFieldDescrArr);
   end;
  end;

  procedure w2Fill(w2Text   :String;
                   var ADate:TDateTime);                overload;
  begin
  if w2Test(w2Text) then with FParser do
    begin
    FParseOk:= Search(w2Text,False,True);
    ADate  := NextDate('mdyHMS');
    Handled:= GetConversionResult;
    NextLine;
    end;
  end;

  procedure w2Fill(w2Text      :String;
                   var AString :String;
                   LineComplete:Boolean=True);          overload;
  var Stg: String;
  begin
  if w2Test(w2Text) then
    begin
    FParser.Search(w2Text,True,True,False);
    Stg:= FParser.RemainderOfLine.TrimLeft([chTab,chSpace]);                    //use ansi version of StrTrim
    if Length(Stg)>0 then
      begin
      AString:= Stg;
      end;
    if LineComplete then
      FParser.NextLine;
    Handled:= True;
    end;
  end;

  procedure w2Fill(w2Orientation,w2Theta,w2Shift:String;
                   var AOrientation             :String;
                   var ATheta                   :twcFloatType;
                   var AOffset                  :twcCoordinate);     overload;
  begin
  if w2Test(w2Orientation) then
    begin
    w2Fill(w2Orientation,AOrientation,False);
    if Pos(w2Theta,AOrientation)>0 then
      begin
      ATheta := FParser.NextFloat;
      FParseOk:= FParser.ConversionResult;
      end
    else
      ATheta:= 0;
    if FParseOk and (Pos(w2Shift,AOrientation)>0) then
      begin
      AOffset.m[InPlane]:= FParser.NextFloat/10;
      FParseOk          := FParser.ConversionResult;
      if FParseOk then
        begin
        AOffset.m[CrossPlane]:= FParser.NextFloat/10;
        FParseOk             := FParser.ConversionResult;
        end;
      end;
    Handled:= FParseOk and FParser.NextLine;
    if Handled and (ATheta=0) then
      ATheta:= ifthen(Pos(wICPASetupInverse,AOrientation)>0,180,0)+ifthen(Pos(wICPASetupY,AOrientation)>0,0,90);
    end;
  end;

  procedure w2Fill(w2Text    :String;
                   var AFloat:twcFloatType;
                   ARange    :twcFloatType=1e6;
                   Alow      :twcFloatType=0;
                   NegativeOk:Boolean    =True );  overload;
  begin
  if w2Test(w2Text) then
    begin
    try
      AFloat:= FParser.NextFloat(ARange,Alow,NegativeOk);
     except
      AFloat:= 0;
     end;
    Handled:= FParser.NextLine;
    end;
  end;

  procedure w2Fill(w2Text:String;
                   ALine :twICPAlines);    overload;
  var i: Integer;
  begin
  if w2Test(w2Text,False) then
    with FParser do
      begin
      icpFirstPoint[Aline]:= Length(icpPosCm);
      Handled:= FParser.NextLine;
      while Handled and (not (w2Test(wICPADetectorLine) or w2Test(wICPAFrameLine))) do
        begin
        i:= Length(icpPosCm);
        SetLength(icpPosCm,Succ(i));
        SetLength(icpData ,Succ(i));
        icpPosCm[i]:= NextFloat;
        Handled    := GetConversionResult;
        if Handled then
          begin
          icpData[i]:= NextFloat;
          FParseOk   := GetConversionResult;
          end;
        Handled:= FParseOk and FParser.NextLine;
        end; {while}
      end; {with}
  end;

begin
FParseOk:= inherited ParseData(CheckFileTypeOnly) and (CheckFileTypeOnly xor (FileFormat=twcICprofilerAscii));
if FParseOk and (not CheckFileTypeOnly) and (FileFormat=twcICprofilerAscii) then with FParser do
  begin
  Handled:= False;
  repeat
    Handled:= Handled and (Length(FParser.CurrentLine)>1);
    if not Handled then
      repeat
        NextLine;
      until Length(FParser.CurrentLine)>1;
    Handled:= False;
    if FParseOk and not (w2Test(w2ID) or w2Test(w2EndOfMeas_ID) or w2Test(w2EndOfFile_ID)) then
      begin
      w2Fill(wICPATimeStamp             ,icpMeasTime             );    {TimeStamp		8/14/2015 13:48:28}
      w2Fill(wICPADescription           ,icpComment              );    {Description}
      w2Fill(wICPAInstitution           ,icpClinic               );    {Institution}
      w2Fill(wICPARoom                  ,Linac                   );    {Room}
      w2Fill(wICPABeamType              ,icpModality             );    {Beam Type		Undefined}
      w2Fill(wICPAEnergy                ,icpEnergy,100           );    {Energy		-1}
      w2Fill(wICPAWedgeAngle            ,icpWedgeAngle,0,90      );    {Wedge Angle	deg	0}
      w2Fill(wICPACollimator+wICPALeft  ,icpCollimCm[icLeft  ],50);    {Collimator Left	cm	0}
      w2Fill(wICPACollimator+wICPARight ,icpCollimCm[icRight ],50);    {Collimator Right	cm	0}
      w2Fill(wICPACollimator+wICPATop   ,icpCollimCm[icTop   ],50);    {Collimator Top	cm	0}
      w2Fill(wICPACollimator+wICPABottom,icpCollimCm[icBottom],50);    {Collimator Bottom	cm	0}
      w2Fill(wICPAFieldSize             ,icpFieldCm              );    {Field Size	MU	0 x 0}
      w2Fill(wICPAOrientation,wICPASetupTheta,wICPASetupShift,
             icpOrientation,icpSetupAngle,icpPosGAOffsCm         );    {Orientation	([Inverse]	Y Axis | Theta) [ 0/0 shift]}
      w2Fill(wICPASSD                   ,icpSSDcm,1000,0,False   );    {SSD	cm	100.000}
      w2Fill(wICPAModel                 ,icpModel                );    {Collector Model		IC PROFILER}
      end;
     FParseOk:= FParseOk and (not EndOfFile);
  until (not FParseOk) or w2Test(wICPADetectorLine);
  if icpModel<>wICPAModelApprove then
    begin
    FParseOk:= False;
    StatusMessage('Unsupported type: '+icpModel);
    end;
   if FParseOk then
     repeat
       w2Fill('X Axis'  ,icXline  );
       w2Fill('Y Axis'  ,icYline  );
       w2Fill('Positive',icPosDiag);
       w2Fill('Negative',icNegDiag);
     until (not FParseOk) or w2Test(wICPAFrameLine) or EndOfFile or
           (icpFirstPoint[icNegDiag]-icpFirstPoint[icPosDiag] = Length(icpData)-icpFirstPoint[icNegDiag]);
  end; {if FParseOk and fileformat}
Result:= FParseOk and (CheckFileTypeOnly or ReadResults);  {ReadResults->GetNumPoints: sets scantype and angle}
end; {~parsedata}


{23/08/2015 call getnumpoints}
function TICprofiler_ascii.ReadResults(PostText:String=''): Boolean;
begin
Result:= (GetNumPoints>0) and   {set scannr and scantype}
         Inherited ReadResults(PostText);
end; {~readresults}


{21/08/2015}
function TICprofiler_ascii.GetNumPoints: Integer;
begin
ScanNr:= EnsureRange(ScanNr,1,wICPAScanMax);
if ScanNr<>ScanNrOk then
  begin
  if Length(icpOrientation)=0 then
    icpOrientation:= 'Y';
  ScanMax    := wICPAScanMax;
  ScanNrOk   := ScanNr;
  icpScanLine:=twICPAlines(Pred(ScanNr));
  case ScanNr of
    1: ScanAngle:=   0;
    2: ScanAngle:=  90;
    3: ScanAngle:=  45; {because of inverted y-axis angles 45 and 135 are exchanged}
    4: ScanAngle:= 135;
   end;
  ScanAngle:= ScanAngle+icpSetupAngle;
  case Round(ScanAngle) mod 180 of
     0: FScanType:= snAB;
    90: FScanType:= snGT;
   else
        FScanType:= snAngle;
   end;
  end;
if ScanNr=wICPAScanMax then Result:= Length(icpData)-icpFirstPoint[icNegDiag]
else                        Result:= icpFirstPoint[twICPAlines(ScanNr)]-icpFirstPoint[twICPAlines(Pred(ScanNr))];
end; {~getnumpoints}


{21/08/2015}
destructor TICprofiler_ascii.Destroy;
begin
Finalize(icpPosCm);
Finalize(icpData);
Inherited;
end; {~destroy}


//----------Tw2CAD_data-----------------------------------------------------

{09/12/2015 added sharedparser}
{29/03/2016 added FRegisteredFiles}
{01/05/2020 transfer of binarydata and filenames}
{16/05/2020 added FMultiScanCapable}
constructor Tw2CAD_data.Create(SharedParser:toTNumParser =nil;
                               ParsedFile  :String       ='';
                               BinaryData  :TMemoryStream=nil;
                               BinaryFile  :String       ='';
                               AStatusProc :toExtMsgProc =nil;
                               AIdentity   :String       ='w2CAD');
begin
Inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
Initialize(w2Coordinates_mm);
Initialize(w2Values);
FMultiScanCapable:= True;
FRegisteredFiles := '.asc';
end; {~create}


{13/12/2015 wDateString added}
procedure Tw2CAD_data.SetDefaults;
begin
Inherited;
DefaultExtension:= '.asc';
w2Version       :=    0;
w2Date          := Now;
w2DateString    := FormatDateTime('dd-mm-yyyy hh:mm',w2Date);
w2DetectorType  := 'CHA';
if twcGenericToElectron then w2Modality:= 'ELE'
else                         w2Modality:= 'PHO';
FillChar(w2Field_mm,SizeOf(w2Field_mm),0);
w2ScanType      := 'OPP';
w2ScanAxis      := 'X';
w2NumPoints     :=    0;
w2Step_01_mm    :=    0;
w2SSD_mm        := Round(twcDefaultSSDcm*10);
w2Depth_mm      :=    0;
w2WedgeAngle    :=    0;
w2WedgeName     := '00';
w2WedgeDir      := 'L';
w2Comments      := '';
w2DetectorInfo  := '';
w2Operator      := '';
end; {~setdefaults}


function Tw2CAD_data.MakeTimeString(ADateTime:TDateTime): String;
begin
Result:= FormatDateTime('dd-mm-yyyy',ADateTime);
end; {~maketimestring}


{15/08/2016 BinStream implementation}
{19/08/2020 speed improvement with BinaryOnly}
{$push}{$warn 5057 off:Local variable "s" does not seem to be initialized}
function Tw2CAD_data.GetFileType(AFileName :String ='';
                                 BinaryOnly:Boolean=False): twcFileType;
type CStg=record case Boolean of
            True :(Stg  : String[255]);
            False:(Bytes: array[0..255] of Byte);
          end;
var s: Cstg;
    n: Byte;
begin
Result:= Inherited GetFileType(AFileName,BinaryOnly);
if (Result=twcUnknown) and (not BinaryOnly) and LoadBinStream(AFileName) then
  begin
  n:= BinStream.Read(s.Bytes[1],255);
  s.Bytes[0]:= n;
  if Pos(w2ID,s.Stg)>0 then
    Result:= twcW2CAD;
  BinStreamType:= Result;
  end;
end; {~getfiletype}
{$pop}


{19/11/2020 w2ID is specified to be in first 255 bytes, not first line}
function Tw2CAD_data.CheckData(AStringList:TStrings): Boolean;
var i: Integer;
begin
Result:= (inherited CheckData(AStringList));
if Result then
  with FParser do
    begin
    Result:= (IdentificationStg=w2ID);
    if not Result then
      begin
      i:= IdentificationStg.Length;
      repeat
        NextLine;
        IdentificationStg:= CurrentLine;
        Result           := (IdentificationStg=w2ID);
        i                := i+IdentificationStg.Length;
      until Result or (i>255);
      end;
    if Result then
      begin
      FileFormat:= twcW2CAD;
      while CurrentLine[1]='#' do
        NextLine;
      if Pos(w2NumMeas_ID,CurrentLine)=1 then
        ScanMax:= NextInteger;
      end;
    end;
end; {~checkdata}


{$push}{$warn 5057 off}
{16/12/2105 added scannr to readresults}
{06/10/2020 fundamentals alternative}
function Tw2CAD_data.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
var i      : Integer;
    Handled: Boolean;

  function SearchLine(ASearchText:String): String;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText,False,True);
    Result := RemainderOfLine;
    end
  else
    Result:= '';
  end;

  function w2Test(w2Text:String): Boolean;
  begin
  Result:= ifthen(FParseOk,Pos(w2Text,FParser.CurrentLine),0)>0;
  //if Result then StatusMessage(Format('->w2CAD[%d] %s',[Parser.CurrentLineNumber,FParser.CurrentLine]));
  end;

  procedure w2Fill(w2Text      :String;
                   var AInteger:Integer;
                   MinLimit    :Integer=0;
                   MaxLimit    :Integer=10000);          overload;
  begin
  try
    if w2Test(w2Text) then
      begin
      AInteger:= FParser.NextInteger(MinLimit,MaxLimit);
      Handled := FParser.NextLine;
      end;
   except
    AInteger:= 0;
   end;
  end;

  procedure w2Fill(w2Text:String;
                   var A :array of Integer);            overload;
  var i: Word;
  begin
  try
    i:= 0;
    if w2Test(w2Text) then
      begin
      repeat
        A[i]:= FParser.NextInteger;
        Inc(i);
      until (i=Length(A)) or (not FParser.GetConversionResult);
      Handled:= FParser.GetConversionResult;
      FParser.NextLine;
      end;
   except
    FillChar(A,SizeOf(A),0);
   end;
  end;

  procedure w2Fill(w2Text   :String;
                   var ADate:TDateTime);                overload;
  begin
  if w2Test(w2Text) then with FParser do
    begin
    FParseOk    := Search(w2Text,False,True);
    w2DateString:= RemainderOfLine;
    ADate       := NextDate('dmy');
    Handled     := GetConversionResult;
    NextLine;
    end;
  end;

  procedure w2Fill(w2Text      :String;
                   var AString :String;
                   LineComplete:Boolean=True);          overload;
  var Stg: String;
  begin
  if w2Test(w2Text) then
    begin
    FParser.Search(w2Text,True,True,False);
    Stg:= FParser.RemainderOfLine.TrimLeft([chTab,chSpace]);                    //use ansi version of StrTrim
    if Length(Stg)>0 then
      begin
      AString:= Stg;
      end;
    if LineComplete then
      FParser.NextLine;
    Handled:= True;
    end;
  end;

  procedure w2Fill(w2Text      :String;
                   var AString :String;
                   var AInteger:Integer);             overload;
  begin
  if w2Test(w2Text) then
    begin
    w2Fill(w2Text,AString,False);
    try
      AInteger:= FParser.NextInteger;
     except
      AInteger:= 0;
     end;
    Handled:= FParser.NextLine;
    end;
  end;

  {******data order and directions are GT [mm], AB [mm], UP [mm], Dose [%]*****}
  procedure w2Fill(w2Text         :String;
                   var Coordinates:twcCoordArray;
                   var Values     :twcFloatArray);    overload;
  var ACoordinate: twcCoordinate;
      AValue     : twcFloatType;
      mAxis      : twcMeasAxis;
      i          : Integer;
  begin
  with FParser do
    while w2Test(w2Text) do
      begin
      ACoordinate:= Default(twcCoordinate);
      for mAxis:= Inplane to Beam do
        if FParseOk then
          begin
          ACoordinate.m[mAxis]:= NextFloat;
          FParseOk             := ConversionResult;
          end;
      if FParseOk then
        begin
        AValue := FParser.NextFloat;
        FParseOk:= ConversionResult;
        if FParseOk then
          begin
          i:= Length(Coordinates);
          SetLength(Coordinates,Succ(i));
          SetLength(Values     ,Succ(i));
          Coordinates[i]:= ACoordinate;
          Values[i]     := AValue;
          end;
        end;
        Handled:= FParser.NextLine;
      end;
  end;

  function SetScan(AScanNr:Integer): Integer;
  begin
  Result:= 0;
  FParser.GotoTop;
  repeat
    FParseOk:= FParser.Search(w2ID);
    if FParseOk then
      Inc(Result);
  until (not FParseOk) or (Result=AScanNr);
  FParseOk                 := (i>=0);
  FParser.CurrentLineNumber:= FParser.LastLineOkNumber;
  end;

begin
FParseOk:= inherited ParseData(CheckFileTypeOnly) and (CheckFileTypeOnly xor (FileFormat=twcW2CAD));
if FParseOk and (not CheckFileTypeOnly) and (FileFormat=twcW2CAD) then with FParser do
  begin
  if ScanMax=0 then
    begin
    ScanMax:= SetScan(0);
    GotoTop;
    end;
  i:= SetScan(EnsureRange(ScanNr,1,ScanMax));
  ScanNr:= i;
  Handled:= False;
  repeat
    Handled:= Handled and (Length(FParser.CurrentLine)>1);
    if not Handled then
      repeat
        NextLine;
      until Length(FParser.CurrentLine)>1;
    Handled:= False;
    if LogLevel>2 then
      StatusMessage(Format('Parsing(%s) {%s} ',[FIdentity,FParser.CurrentLine]),True,3);
    if FParseOk and not (w2Test(w2ID) or w2Test(w2EndOfMeas_ID) or w2Test(w2EndOfFile_ID)) then
      begin
      w2Fill('<'              ,w2Coordinates_mm,w2Values);{<}
      w2Fill(w2Comments_ID    ,w2Comments);               {# Comment:}
      w2Fill(w2DetectorInfo_ID,w2DetectorInfo);           {# Detector:}
      w2Fill(w2OperatorInfo_ID,w2Operator);               {# Operator:}
      w2Fill(w2Version_ID     ,w2Version,0,1000);         {%VERSION}
      w2Fill(w2Date_ID        ,w2Date);                   {%DATE}
      w2Fill(w2DetectorType_ID,w2DetectorType);           {%DETY}
      w2Fill(w2Radiation_ID   ,w2Modality);               {%BMTY}
      w2Fill(w2FieldSize_mm_ID,w2Field_mm);               {%FLSZ}
      w2Fill(w2ScanType_ID    ,w2ScanType);               {%TYPE}
      w2Fill(w2WedgeName_ID   ,w2WedgeName,w2WedgeAngle); {%WDGL}
      w2Fill(w2WedgeDir_ID    ,w2WedgeDir);               {%WDGD}
      w2Fill(w2ScanAxis_ID    ,w2ScanAxis);               {%AXIS}
      w2Fill(w2NumPoints_ID   ,w2NumPoints);              {%PNTS}
      w2Fill(w2Step_01mm_ID   ,w2Step_01_mm,1);           {%STEP}
      w2Fill(w2SSD_mm_ID      ,w2SSD_mm);                 {%SSD}
      w2Fill(w2Depth_mm_ID    ,w2Depth_mm,-10000);        {%DPTH}
      end;
    until (not FParseOk) or w2Test(w2EndOfMeas_ID) or w2Test(w2EndOfFile_ID);
  end; {if FParseOk and fileformat}
Result:= FParseOk and (CheckFileTypeOnly or ReadResults);
end; {~parsedata}
{$pop}


function Tw2CAD_data.ReadResults(PostText:String=''): Boolean;
begin
Result:= Inherited ReadResults(PostText);
if Result then
  ScanNrOk:= ScanNr;
end; {~readresults}


function Tw2CAD_data.GetNumPoints: Integer;
begin
Result:= Length(w2Values);
end; {~getnumpoints}


{$push}{$warn 5057 off}
function Tw2CAD_data.GetProfile(Index:Integer): twcGrafPoint;

var p: twcGrafPoint;
begin
if InRange(Index,0,Pred(GetNumPoints)) then
   begin
   p.X:= GetDistance(w2Coordinates_mm[0],w2Coordinates_mm[Index])/10;
   p.Y:= w2Values[Index];
   end
else
   p:= Default(twcGrafPoint);
Result:= p;
end; {~get_profile}
{$pop}


procedure Tw2CAD_data.PutProfile(Index:Integer;
                                 Point:twcGrafPoint);
begin
if InRange(Index,0,Pred(GetNumPoints)) then
  w2Values[Index]:= Point.Y;
end; {~putprofile}

(*
function Tw2CAD_data.WriteData(AFileName  :String;
                               AStringList:TStrings;
                               ASource    :twcDataSource=dsMeasured): Boolean;
var i: Integer;

  procedure WriteRfaString(ALabel,AStg:String);
  begin
  AStringList.Append(ALabel+ifthen(Length(AStg)>0,chSpace+chTab,'')+Astg);
  end;

  procedure WriteRfaInteger(ALabel:String;
                            AValue:Integer);
  begin
  AStringList.Append(Format('%s %s%d',[ALabel,chTab,AValue]));
  end;

  procedure WriteRfaComment(AStg  :String='';
                            IDchar:Char='#');
  begin
  AStringList.Append(IDchar+ifthen(Length(AStg)>0,chSpace,'')+Astg);
  end;

  function RfaCoordinateStg(ACoordinate:twcCoordinate): String;
  begin
  with ACoordinate do Result:= Format('%7.2f%s%7.2f%s%7.2f',[m[Inplane]*10,chTab,m[Crossplane]*10,chTab,m[Beam]*10]);
  end;

begin
inherited WriteData(AFileName,AStringList,ASource);
with AStringList,RfaData do
  begin
  Clear;
  Result:= True;
  try
    WriteRfaString(rfaNumMeasID   ,'1'           );   {:MSR 	10	 # No. of measurements in file}
    WriteRfaString(rfaSystemID+' BDS 0',''       );   {:SYS BDS 0 # Beam Data Scanner System}
    WriteRfaComment;
    WriteRfaComment('RFA300 ASCII Measurement Dump ( BDS format )');
    WriteRfaComment;
    WriteRfaComment(rfaMeasNumberID+' 1');
    WriteRfaComment;
    WriteRfaString('%VNR'          ,'1.0'        );   {%VNR 1.0}
    WriteRfaString(rfaModID        ,rfaModType   );   {%MOD RAT}
    WriteRfaString(rfaFileID       ,rfaFileType  );   {%TYP SCN}
    WriteRfaString(rfaScanID       ,rfaScanType  );   {%SCN PRO}
    WriteRfaString(rfaDetID        ,rfaDetType   );   {%FLD ION}
    WriteRfaString(rfaDateID       ,Format('%0.2d-%0.2d-%0.4d',[MonthOf(rfaDate),DayOf(rfaDate)   ,YearOf(rfaDate)         ])); {%DAT 	03-27-2008}
    WriteRfaString(rfaTimeID       ,Format('%0.2d:%0.2d:%0.2d',[HourOf(rfaDate) ,MinuteOf(rfaDate),SecondOf(rfaDate)       ])); {%TIM 	15:57:33}
    WriteRfaString(rfaFSizeID      ,Format('%d%s%d'      ,[rfaField_mm[fInplane],chTab            ,rfaField_mm[fCrossplane]])); {%FSZ 	400	400}
    WriteRfaString(rfaRadiationID  ,Format('%s%s%0.1f'   ,[rfaRadiation         ,chTab            ,rfaEnergy_MV            ])); {%BMT 	PHO 	    6.0}
    WriteRfaInteger(rfaSSDID       ,rfaSSD_mm    );   {%SSD 	1000}
    WriteRfaInteger(rfaBUPID       ,rfaBUP_01mm  );   {%BUP 	0}
    WriteRfaInteger(rfaBRDID       ,rfaBRD_mm    );   {%BRD 	1000}
    WriteRfaInteger(rfaFShapeID    ,rfaFShape    );   {%FSH 	-1}
    WriteRfaInteger(rfaAccessoryID ,rfaAccessory );   {%ASC 	0}
    WriteRfaInteger(rfaWedgeID     ,rfaWedge_deg );   {%WEG 	0}
    WriteRfaInteger(rfaGantryID    ,rfaGantry    );   {%GPO 	0}
    WriteRfaInteger(rfaCollimatorID,rfaCollimator);   {%CPO 	90}
    WriteRfaInteger(rfaMeasID      ,rfaMeasType  );   {%MEA 	2}
    WriteRfaInteger(rfaDepthID     ,rfaDepth_01mm);   {%PRD 	160}
    WriteRfaInteger(rfaPointsID    ,rfaPoints    );   {%PTS 	637}
    WriteRfaString(rfaStartScanID  ,RfaCoordinateStg(rfaStart_Meas_cm));   {%STS 	    0.0	 -236.6	   16.0}
    WriteRfaString(rfaEndScanID    ,RfaCoordinateStg(rfaEnd_Meas_cm  ));   {%EDS 	    0.0	  242.9	   16.0}
    for i:= 0 to 1 do WriteRfaComment(rfaComments[i],rfaMeasInfoID);
    WriteRfaComment('X      Y      Z     Dose');
    for i:= 0 to Pred(GetNumPoints) do WriteRfaString(rfaDataID,Format('%s%8.1f',[RfaCoordinateStg(rfaCoordinates_cm[i]),rfaValues[i]]));
    WriteRfaString(rfaEndMeasID    ,''           );   {:EOM  # End of Measurement}
    WriteRfaString(rfaEndFileID    ,''           );   {:EOF # End of File}
    if Length(ExtraText)>0 then for i:= 0 to Pred(Length(ExtraText)) do WriteRfaComment(ExtraText[i]);
   except
     Result:= False;
   end;
  end;
end; {~writedata}


function Tw2CAD_data.WriteData(AFileName :String;
                                   OutPutType:twcFileType;
                                   ASource   :twcDataSource=dsMeasured;
                                   SetExt    :Boolean=True          ): Boolean;
begin
if OutPutType=FileFormat then Result:= WriteData(AFileName,False,ASource,SetExt)
else                          Result:= False;
end; {~writedata}
*)

destructor Tw2CAD_data.Destroy;
begin
Finalize(w2Coordinates_mm);
Finalize(w2Values);
Inherited;
end; {~destroy}


//----------TEclipseData-----------------------------------------------------

{09/10/2020 new}
constructor TEclipseData.Create(SharedParser:toTNumParser =nil;
                                ParsedFile  :String       ='';
                                BinaryData  :TMemoryStream=nil;
                                BinaryFile  :String       ='';
                                AStatusProc :toExtMsgProc =nil;
                                AIdentity   :String       ='Eclipse');
begin
Inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
Initialize(FValues);
FRegisteredFiles:= '.txt';
end; {~create}


{09/10/2020 new}
procedure TEclipseData.SetDefaults;
var t: twcTankAxis;
begin
Inherited;
DefaultExtension:= '.txt';
FStart_Cm       := Default(twcCoordinate);
FEnd_Cm         := Default(twcCoordinate);
FDataMapping    := 'ZXY';                                                       //Inplane,Crossplane,Beam maps to ZXY
for t:= X to Z do
  FAxisSign.t[t]:= ifthen(t=Z,-1,1);
EcName          := '';
EcID            := '';
EcPlan          := '';
EcCourse        := '';
EcDate          := Now;
EcDateString    := MakeTimeString(EcDate);
EcOperator      := '';
EcEnergy        := 0;
EcModality      := '';
end; {~setdefaults}


{09/10/2020 new}
function TEclipseData.MakeTimeString(ADateTime:TDateTime): String;
begin
Result:= FormatDateTime('dd mmm yyyy hh:mm:ss am/pm',ADateTime);
end; {~maketimestring}


{09/10/2020 new}
{11/10/2020 forgot to build in test}
{$push}{$warn 5057 off:Local variable "s" does not seem to be initialized}
function TEclipseData.GetFileType(AFileName :String ='';
                                 BinaryOnly:Boolean=False): twcFileType;
const IdentLen=Length(eclipseID)+10;
type CStg=record case Boolean of
            True :(Stg  : String[IdentLen]);
            False:(Bytes: array[0..IdentLen] of Byte);
          end;
var s: CStg;
    n: Byte;
begin
Result:= Inherited GetFileType(AFileName,BinaryOnly);
if (Result=twcUnknown) and (not BinaryOnly) and LoadBinStream(AFileName) then
  begin
  n:= BinStream.Read(s.Bytes[1],IdentLen);
  s.Bytes[0]:= n;
  if Pos(eclipseID,s.Stg)=4 then
    Result:= twcEclipse;
  end;
end; {~getfiletype}
{$pop}


{09/10/2020 new}
{11/10/2020 forgot to build in test}
function TEclipseData.CheckData(AStringList:TStrings): Boolean;
begin
Result:= (inherited CheckData(AStringList)) and (InRange(Pos(eclipseID,IdentificationStg),1,4));
if Result then
  FileFormat:= twcEclipse;
end; {~checkdata}


//{$push}{$warn 5057 off}
{09/10/2020 new}
{12/10/2020 'x' is not mandatory for Eclipse format but a habit of AC}
function TEclipseData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
var i: Integer;
    m: twcFloatType;
    b: Boolean;

  procedure EcFill(SearchText   :String;
                   var Remainder:String);
  begin
  FparseOk:= Parser.Search(SearchText+ifthen(SearchText.EndsWith(': '),'',': '));
  if FparseOk then
    Remainder:= Parser.RemainderOfLine;
  end;

  procedure EcFill(SearchText     :String;
                   var ACoordinate:twcCoordinate);
  var t: twcTankAxis;
  begin
  FparseOk:= Parser.Search(SearchText+ifthen(SearchText.EndsWith(': '),'',': '));
  if FParseOk then
    for t:= X to Z do
      ACoordinate.t[t]:= Parser.NextFloat*FAxisSign.t[t];
  end;

  procedure EcFill(var APoint:twcGrafPoint);
  begin
  FparseOk:= Parser.NextLine(True) and (Parser.CountNumbers=2);
  if FParseOk then
    begin
    APoint.X:= Parser.NextFloat;
    APoint.Y:= Parser.NextFloat;
    end;
  end;

begin
FParseOk:= inherited ParseData(CheckFileTypeOnly) and (CheckFileTypeOnly xor (FileFormat=twcEclipse));
if FParseOk then
  with Parser do
    begin
    GotoTop;
    EcFill(eclipseID,EcName      );
    EcFill('ID'     ,EcID        );
    EcFill('Plan'   ,EcPlan      );
    EcFill('Course' ,EcCourse    );
    EcFill('Date'   ,EcDateString);
    Parser.GotoTop;
    Search('Date');
    EcDate:= NextDate;
    EcFill('by'     ,EcOperator  );
    if FParseOk then
      begin
       EcFill('Start',FStart_Cm);
       EcFill('End'  ,FEnd_Cm  );
       if Search('x') then
         begin
         GotoLeft;
         EcField_cm[fCrossplane]:= NextFloat;
         EcField_cm[fInplane   ]:= NextFloat;
         if Pos('V',RemainderOfLine)>0 then
           begin
           EcEnergy  := Abs(NextFloat);
           EcModality:= RemainderOfLine;
           end
         else
           NextLine;
         end;
      i:= 0;
      b:= False;
      while FParseOk do
        begin
        SetLength(FValues,Succ(i));
        EcFill(FValues[i]);
        if FParseOk and (not b) then                                            //if the first x value not equals zero then it is a rogue point; try again
          b:= (FValues[0].X=0);
        if b then
          Inc(i);
        end;
      if i>0 then
        begin
        SetLength(FValues,i-1);
        FParseOk:= (i>1);
        if FParseOk then
          begin
          m:= FValues[i-2].x;
          for i:= 0 to 2 do
          FRange_Cm.i[i]:= (FEnd_Cm.i[i]-FStart_Cm.i[i])/m;
          end;
        end;
      end;
    end;
Result:= FParseOk and (CheckFileTypeOnly or ReadResults);
end; {~parsedata}
//{$pop}


{09/10/2020 new}
function TEclipseData.GetNumPoints: Integer;
begin
Result:= Length(FValues);
end; {~getnumpoints}


{09/10/2020 new}
destructor TEclipseData.Destroy;
begin
Finalize(FValues);
Inherited;
end; {~destroy}


//----------TMccProfileData-----------------------------------------------------

{25/08/2015 MccOrgScanType,MccSetScanType}
{09/12/2015 added sharedparser}
{29/03/2016 added FRegisteredFiles}
{10/02/2020 initialise MccOriginValue}
{01/05/2020 transfer of binarydata and filenames}
{16/05/2020 added FMultiScanCapable}
constructor TMccProfileData.Create(SharedParser:toTNumParser =nil;
                                   ParsedFile  :String       ='';
                                   BinaryData  :TMemoryStream=nil;
                                   BinaryFile  :String       ='';
                                   OriginValue :twcFloatType  =0;
                                   AStatusProc :toExtMsgProc =nil;
                                   AIdentity   :String       ='PTW');
begin
Inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
with MccData do
  begin
  Initialize(tmRefField.tmRefScanPos_mm);
  Initialize(tmScanSpeeds);
  Initialize(tmDelays);
  Initialize(tmPos_mm);
  Initialize(tmData);
  end;
MccOrgFormat     := twcUnknown;
MccOrgScanType   := snUndefined;
MccOriginValue   := OriginValue;
FMultiScanCapable:= True;
FRegisteredFiles := '.mcc';
end; {~create}


{25/08/2015 tmCurveType}
procedure TMccProfileData.SetDefaults;
var i: twcChannels;
    t: twcTankAxis;
begin
Inherited;
DefaultExtension:= '.mcc';
MccCreated      := Now;
MccModified     := MccCreated;
with MccData,tmElectrometer,tmRefField,tmScanInfo do
  begin
  tmScanOffAxis_mm   := Default(twcFieldDescrArr);
  tmTankAxis         :='XYZ';
  for t:= X to Z do
    tmAxisDir.t[t]   :=    1;
  tmTaskName         :=   '';
  tmGroupName        :=   '';
  tmProgram          := ExtractFileName(ParamStr(0));
  tmComment          :=   '';
  tmLinac            :=   '';
  if twcGenericToElectron then tmModality:= 'E'
  else                         tmModality:= 'X';
  tmIsoc             := 1000;
  tmEnergy           :=    0;
  tmDmax             :=   15;
  tmSSD_mm           := twcDefaultSSDcm*10;
  tmSCD_mm           :=  380;
  tmBlock            :=    0;
  tmField_mm         := Fill_FieldDescrArr(400);
  tmFieldShape       := RECTANGULAR;
  tmGantry           :=    0;
  tmGantryUp         :=    0;
  tmGantryCW         :=  True;
  tmCollimator       :=    0;
  tmScanDevice       :=   '';
  tmScanColor        :=    0;
  tmScan_Setup       :=   '';
  tmElType           :=   '';
  for i:= FieldCh to RefCh do with tmDetectors[i] do
    begin
    tmElRange[i]     :='AUTO';
    tmDetType        :='THIMBLE_CHAMBER';
    tmDetSubCode     :='FLEX';
    tmDetRadius_mm   :=    3;
    tmDetName        := twcDefUnknown;
    tmDetSN          :=  '0';
    tmDetCalibration :=    1;
    tmDetIsCalibrated:= False;
    tmDetHV          :=  300;
    end;
  tmRefDepth         :=  100;
  tmRefDef           :='ISOCENTER';
  tmRefSize          := Fill_FieldDescrArr(100);
  tmRefOverScan      :=    1;
  tmMedium           :='WATER';
  tmMeasPreset       :='REFERENCE_DOSEMETER';
  tmMeasTime_s       :=    0.3;
  tmMeasUnit	       :='A.U.';
  tmPressure_hPa     := 1013.25;
  tmTemperature_C    :=   22;
  tmNormTemp_C       := tmTemperature_C;
  tmPT               :=    1;
  tmDoseRate         :=    6;
  tmGUID             := '{BAD91BB1-133D-4a50-8F3D-2DCE43AA789C}';
  tmDragDrop         := False;
  tmCurveType        := mccCURVETYPE_CROSSPLANE;
  end;
SetScanDefaults;
end; {~setdefaults}


procedure TMccProfileData.SetScanDefaults;
begin
SetNumPoints(0);
with MccData,tmElectrometer,tmRefField,tmScanInfo do
  begin
  tmMeasAxis         := Crossplane;
  tmMeasDate         := MccCreated;
  tmFieldOffset_mm   := Fill_FieldDescrArr(0);
  tmScanDepth_mm     :=    0;
  tmScanAngle        :=    0;
  tmScanDiagonal     := mccNotDiagonal;
  tmScanDirection    :='POSITIVE';
  tmWedge            :=    0;
  end;
end; {~setscandefaults}


function TMccProfileData.MakeTimeString(ADateTime:TDateTime): String;
begin
Result:= FormatDateTime('dd-mmm-yyyy hh:nn:ss',ADateTime);
end; {~maketimestring}


{15/08/2016 BinStream implementation}
{19/08/2020 speed improvement with BinaryOnly}
{$push}{$warn 5057 off:Local variable "s" does not seem to be initialized}
function TMccProfileData.GetFileType(AFileName :String ='';
                                     BinaryOnly:Boolean=False): twcFileType;
type CStg=record case Boolean of
            True :(Stg  : String[Length(mccID)]);
            False:(Bytes: array[0..Length(mccID)] of Byte);
          end;
var s: Cstg;
    n: Byte;
begin
Result:= Inherited GetFileType(AFileName,BinaryOnly);
if (Result=twcUnknown) and (not BinaryOnly) and LoadBinStream(AFileName) then
  begin
  n:= BinStream.Read(s.Bytes[1],Length(mccID));
  s.Bytes[0]:= n;
  if s.Stg=mccID then
    Result:= twcMccProfile;
  BinStreamType:= Result;
  end;
end; {~getfiletype}
{$pop}


function TMccProfileData.CheckData(AStringList:TStrings): Boolean;
begin
Result:= inherited CheckData(AStringList) and (Pos(mccBEGIN+mcc_SCAN_DATA,IdentificationStg)=1);
if Result then with FParser do
  begin
  ScanMax   := 0;
  FileFormat:= twcMccProfile;
  while Search(mccBEGIN+mcc_SCANnr,True,False,False) do
    ScanMax:= Max(ScanMax,NextInteger);
  end;
end; {~checkdata}


procedure TMccProfileData.SetNumpoints(Npoints:wmsIntType);
begin
with MccData do
  begin
  SetLength(tmPos_mm,Npoints);
  SetLength(tmData  ,Npoints);
  end
end; {~setnumpoints}


{01/07/2015
  Hard-coding of ScanMax to 4 for starcheckmaxi removed.}
{10/08/2015
  New variant found in the wild:
  INPLANE_AXIS=GT
  CROSSPLANE_AXIS=AB
  DEPTH_AXIS=Depth
  SCAN_ANGLE=90 for INPLANE scan}
{16/12/2105 added scannr to readresults}
{23/05/2016 set length of any array to zero before adding data from a unstructured list}
{20/03/2017 range checking applied}
{10/02/2018 added trim to searchline}
{01/06/2018 twcMccInsertOrigin}
function TMccProfileData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
var i,s,LastLine: Integer;

  function SearchLine(ASearchText:String): String;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText+'=',False,True);
    Result := Trim(RemainderOfLine);
    end
  else
    Result:= '';
  end;

  function SearchNextDate(ASearchText:String): TDateTime;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText,False,True);
    Result := NextDate;
    end
  else
    Result:= Now;
  end;

  function MccTest(MccText     :String;
                   AddEqualSign:Boolean=True): Boolean;
  var i: Integer;
      s: String;
  begin
  if AddEqualSign then s:= FParser.CurrentLine
  else                 s:= FParser.RemainderOfLine;
  i:= ifthen(FParseOk,Pos(MccText+ifthen(AddEqualSign,'=',''),s),0);
  Result:= (i>0) and (Pos(mccComment,Copy(FParser.CurrentLine,1,i))=0);
  //if Result and AddEqualSign then StatusMessage(Format('->PTW[%d] %s',[Parser.CurrentLineNumber,FParser.CurrentLine]));
  end;

  procedure MccFill(MccText    :String;
                    var AString:String);                  overload;
  begin
  if MccTest(MccText) then
    begin
    Astring:= SearchLine(MccText);
    FParser.NextLine;
    end;
  end;

  procedure MccFill(MccText  :String;
                    var AChar:twcModalityChar);             overload;
  begin
  if MccTest(MccText) then
    begin
    AChar:= SearchLine(MccText)[1];
    FParser.NextLine;
    end;
  end;

  procedure MccFill(MccText  :String;
                    var AChar:Char);                      overload;
  begin
  if MccTest(MccText) then
    begin
    AChar:= SearchLine(MccText)[1];
    FParser.NextLine;
    end;
  end;

  procedure MccFill(MccText  :String;
                    var AAI  :twcTankAxisID;
                    AMeasAxis:twcMeasAxis);                overload;
  var c: Char;
      s: String;
  begin
  if MccTest(MccText) then
    begin
    c:= 'X';
    s:= UpperCase(SearchLine(MccText));
    if      s='GT'      then c:= 'X'
    else if s='AB'      then c:= 'Y'
    else if s='DEPTH'   then c:= 'Z'
    else if Length(s)>0 then c:= s[1];
    if not (c in ['X'..'Z']) then
      c:= Char(Ord(AMeasAxis)+Ord('X'));
    AAI[AMeasAxis]:= c;
    FParser.NextLine;
    end;
  end;

  procedure MccFill(MccText   :String;
                    var AFloat:twcFloatType;
                    ARange    :twcFloatType=1e5;
                    ALow      :twcFloatType=0;
                    NegativeOk:Boolean    =True);          overload;
  begin
  if MccTest(MccText) then
    begin
    AFloat:= FParser.NextFloat(ARange,Alow,NegativeOk);
    FParser.NextLine;
    end;
  end;

  procedure MccFill(MccText  :String;
                    var ADate:TDateTime);                 overload;
  begin
  if MccTest(MccText) then
    begin
    ADate:= SearchNextDate(MccText);
    FParser.NextLine;
    end;
  end;

  procedure MccFill(MccText     :String;
                    var AInteger:Integer);                overload;
  begin
  try
    if MccTest(MccText) then
      begin
      AInteger:= FParser.NextInteger;
      FParser.NextLine;
      end;
   except
    AInteger:= 0;
   end;
  end;

  procedure MccFill(MccText        :String;
                    var AFieldShape:twcFieldShape);       overload;
  var c: Char;
  begin
  if MccTest(MccText) then
    begin
    c:= SearchLine(MccText)[1];
    case c of
      'B': AFieldShape:= Blocks;
      'M': AFieldShape:= MLC;
      'C': AFieldShape:= Circular;
     else  AFieldShape:= Rectangular;
     end;
    FParser.NextLine;
    end;
  end;

  procedure MccFill(MccText :String;
                    var ABool:Boolean;
                    AString  :String;
                    TrueTest :Boolean=True);              overload;
  begin
  if MccTest(MccText) then
    begin
    ABool:= (SearchLine(MccText)=AString)=TrueTest;
    FParser.NextLine;
    end;
  end;

  procedure MccFill(MccText :String;
                    var ADet:tmDetectorRec);             overload;
  begin
  if (MccText='') or MccTest(MccText,False) then with ADet do
    begin
    MccFill(mccDETECTOR+MccText                  ,tmDetType);               {DETECTOR=THIMBLE_CHAMBER}
    MccFill(mccDETECTOR+MccText+mcc_SUBCODE      ,tmDetSubCode);            {DETECTOR_SUBCODE=SEMIFLEX}
    MccFill(mccDETECTOR+MccText+mcc_RADIUS       ,tmDetRadius_mm);          {DETECTOR_RADIUS=2.75}
    MccFill(mccDETECTOR+MccText+mcc_NAME         ,tmDetName);               {DETECTOR_NAME=PTW 31010 Semiflex}
    MccFill(mccDETECTOR+MccText+mcc_SN           ,tmDetSN);                 {DETECTOR_SN=2107}
    MccFill(mccDETECTOR+MccText+mcc_CALIBRATION  ,tmDetCalibration);        {DETECTOR_CALIBRATION=304700000.00}
    MccFill(mccDETECTOR+MccText+mcc_IS_CALIBRATED,tmDetIsCalibrated,'1');   {DETECTOR_IS_CALIBRATED=1}
    MccFill(mccDETECTOR+MccText+mcc_HV           ,tmDetHV);                 {DETECTOR_HV=0.0}
    end;
  end;

  procedure MccFill(MccText :String;
                    var AArr:twcFloatArray);             overload;
  var i: Integer;
      r: Boolean;
      f: twcFloatType;
  begin
  if MccTest(MccText) then with FParser do
    begin
    r:= Search(MccText,False,True);
    if r then
      Setlength(AArr,0);
    while r and (Length(RemainderOfLine)>1) do
      begin
      f:= NextFloat;
      i:= Length(AArr);
      r:= ConversionResult;
      if r then
        begin
        SetLength(AArr,Succ(i));
        AArr[i]:= f;
        end;
      end; {while}
    NextLine;
    end; {if}
  end;

  procedure MccFill(MccText :String;
                    var ARef:tmRefFieldRec);             overload;
  begin
  if MccTest(MccText,False) then with ARef do
    begin
    MccFill(mccText+mccFIELD+mcc_DEPTH     ,tmRefDepth);                    {REF_FIELD_DEPTH=100.00}
    MccFill(mccText+mccFIELD+mcc_DEFINED   ,tmRefDef);                      {REF_FIELD_DEFINED=ISOCENTER}
    MccFill(MccText+mccFIELD+mcc_INPLANE   ,tmRefSize[fInplane]);           {REF_FIELD_INPLANE=100.00}
    MccFill(MccText+mccFIELD+mcc_CROSSPLANE,tmRefSize[fCrossplane]);        {REF_FIELD_CROSSPLANE=100.00}
    MccFill(MccText+mccOVERSCAN_FACTOR     ,tmRefOverScan);                 {REF_OVERSCAN_FACTOR=1.00}
    MccFill(MccText+mccSCAN+mcc_POSITIONS  ,tmRefScanPos_mm);               {REF_SCAN_POSITIONS=-130.00;-125.00;....}
    end;
  end;

  procedure MccFill(MccText :String;
                    var AScan:tmScanRec);             overload;
  begin
  if MccTest(MccText,False) then with AScan,MccData do
    begin
    if MccTest(mccText+mcc_CURVETYPE) then  {SCAN_CURVETYPE=INPLANE_PROFILE}
      begin
      tmCurveType:= SearchLine(mccText+mcc_CURVETYPE);
      case tmCurveType[1] of
        'I': begin FScanType:= snGT;   tmMeasAxis:= Inplane;    end;
        'C': begin FScanType:= snAB;   tmMeasAxis:= Crossplane; end;
        'P': begin FScanType:= snPDD;  tmMeasAxis:= Beam;       end;
        else       FScanType:= snUndefined;
       end;
      MccOrgScanType:= FScanType;
      FParser.NextLine;
      end
    else
      begin
      MccFill(mccText+mcc_DEPTH,tmScanDepth_mm);                                 {SCAN_DEPTH=100.00}
      MccFill(mccText+mcc_OFFAXIS+mcc_INPLANE,tmScanOffAxis_mm[fInplane]);       {SCAN_OFFAXIS_INPLANE=0.00}
      MccFill(mccText+mcc_DIAGONAL,tmScanDiagonal);                              {SCAN_DIAGONAL=NOT_DIAGONAL}
      MccFill(mccText+mcc_ANGLE,tmScanAngle);                                    {SCAN_ANGLE}
      MccFill(mccText+mcc_OFFAXIS+mcc_CROSSPLANE,tmScanOffAxis_mm[fCrossplane]); {SCAN_OFFAXIS_CROSSPLANE=0.00}
      MccFill(mccText+mcc_DIRECTION,tmScanDirection);                            {SCAN_DIRECTION=POSITIVE}
      MccFill(mccText+mcc_COLOR    ,tmScanColor);                                {255}
      end;
    end;
  end;

  procedure MccFill(MccText  :String;
                    var ASign:twcTankAxisSign;
                    AAxis    :twcTankAxisChar);              overload;
  begin
  if MccTest(MccText) then
    begin
    ASign.c[AAxis]:= ifthen(SearchLine(MccText)[1] in ['G','L','U'],1,-1); {must be compatible with GetScanDirection}
    FParser.NextLine;
    end;
  end;

  {01/06/2018}
  {10/02/2020 if twcMccInsertOrigin AND (MccOriginValue > 0) then}
  {30/10/2020 ... AND (tmTaskName='DOSE'), also do nothing at all if not twcMccInsertOrigin}
  procedure InsertOrigin;
  var i,j,n: Integer;
  begin
  with MccData do
    if (tmScanDevice='STARCHECKMAXI') and twcMccInsertOrigin and (tmTaskName='DOSE') then //current implementation only for StarCheckMaxi, unreliable in DOSERATE mode
      begin
      n:= GetNumPoints;
      if  n>20 then
        begin
        i:= 0;
        j:= n-2;
        while (i<j) and (Sign(tmPos_mm[i])=Sign(tmPos_mm[Succ(i)])) do          //step until sign of position changes
         Inc(i);
        if tmPos_mm[i]=0 then                                                   //try to find point with position=0
          MccOriginValue:= tmData[i]                                            //and store that value
        else if tmPos_mm[Succ(i)]=0 then
          MccOriginValue:= tmData[Succ(i)]
        else if MccOriginValue>0 then                                           //there is no origin, create it if there is a originvalue available
          begin
          SetNumPoints(Succ(n));
          Inc(i);
          for j:= n downto Succ(i) do                                           //push forward all data points with higher position
            begin
            tmPos_mm[j]:= tmPos_mm[Pred(j)];
            tmData[j]  := tmData[Pred(j)];
            end;
          tmPos_mm[i]:= 0;
          tmData[i]  := MccOriginValue;
          end;
        end;
      end; {STARCHECKMAXI}
  end; {insertorigin}

begin
FParseOk:= inherited ParseData(CheckFileTypeOnly) and (CheckFileTypeOnly xor (FileFormat=twcMccProfile));    {BEGIN_SCAN_DATA}
if (FParseOk and (not CheckFileTypeOnly) and (FileFormat=twcMccProfile)) then with FParser do
  begin
  GotoTop;
  i          := -1;
  s          := EnsureRange(ScanNr,1,ScanMax);
  FParseOk   := Pos('CC-Export V1.',SearchLine(mccFORMAT))>0;                        {FORMAT=CC-Export V1.9}
  MccCreated := SearchNextDate(mccFILE_CREATION_DATE);                               {FILE_CREATION_DATE=29-Feb-2012 15:18:00}
  MccModified:= SearchNextDate(mccLAST_MODIFIED);                                    {LAST_MODIFIED=29-Feb-2012 15:18:00}
  if FParseOk then
    begin
    repeat
       FParseOk:= Search(mccBEGIN+mcc_SCANnr);                                       {BEGIN_SCAN  1}
       if FParseOk then
         i:= NextInteger;
    until (not FParseOk) or (i=s) or (s<1);
    if not FParseOk then
      begin
      FParseOk         := (i>=0);
      CurrentLineNumber:= LastLineOkNumber;
      end;
    with MccData do
      begin
      LastLine:= -1;
      ScanNr  :=  i;
      SetScanDefaults;
      repeat
        if FParser.CurrentLineNumber=LastLine then
          NextLine;                                                             //tmRefField must be handled first because of conflict with FIELD_INPLANE
        LastLine:= FParser.CurrentLineNumber;
        MccFill(mccREF_                          ,tmRefField);                          {REF_FIELD_DEPTH=100.00}
        MccFill(mccTASK_NAME                     ,tmTaskName);                          {TASK_NAME=tba PDD Profiles; for StarCheck: DOSE | DOSERATE}
        MccFill(mccGROUP_NAME                    ,tmGroupName);                         {GROUP_NAME=TG_4.0}
        MccFill(mccPROGRAM                       ,tmProgram);                           {PROGRAM=tbaScan}
        MccFill(mccCOMMENT                       ,tmComment);                           {COMMENT=FFF Acceptance - 7 MV}
        MccFill(mccMEAS+mcc_DATE                 ,tmMeasDate);                          {MEAS_DATE=29-Feb-2012 15:15:32}
        MccFill(mccLINAC                         ,tmLinac);                             {LINAC=A3 - L121}
        MccFill(mccMODALITY                      ,tmModality);                          {MODALITY=X}
        MccFill(mccISOCENTER                     ,tmIsoc,1e4,0,False);                  {ISOCENTER=1000.00}
        MccFill(mccINPLANE+mcc_AXIS              ,tmTankAxis,Inplane);                  {INPLANE_AXIS=X | GT}
        MccFill(mccCROSSPLANE+mcc_AXIS           ,tmTankAxis,Crossplane);               {CROSSPLANE_AXIS=Y | AB}
        MccFill(mccDEPTH+mcc_AXIS                ,tmTankAxis,Beam);                     {DEPTH_AXIS=Depth}
        MccFill(mccINPLANE+mcc_AXIS+mcc_DIR      ,tmAxisDir,tmTankAxis[Inplane]);       {INPLANE_AXIS_DIR=GUN_TARGET}
        MccFill(mccCROSSPLANE+mcc_AXIS+mcc_DIR   ,tmAxisDir,tmTankAxis[Crossplane]);    {CROSSPLANE_AXIS_DIR=RIGHT_LEFT}
        MccFill(mccDEPTH+mcc_AXIS+mcc_DIR        ,tmAxisDir,tmTankAxis[Beam]);          {DEPTH_AXIS_DIR=UP_DOWN}
        MccFill(mccENERGY                        ,tmEnergy,100,0,False);                {ENERGY=7.00}
        MccFill(mccNOMINAL_DMAX                  ,tmDmax,1000,0,False);                 {NOMINAL_DMAX=15.00}
        MccFill(mccSSD                           ,tmSSD_mm,1e5,0,False);                {SSD=900.00}
        MccFill(mccScD                           ,tmSCD_mm,1e5,0,False);                {SCD=450.00}
        MccFill(mccBLOCK                         ,tmBlock);                             {BLOCK=0}
        MccFill(mccWEDGE_ANGLE                   ,tmWedge,90,0,False);                  {WEDGE_ANGLE=0.00}
        MccFill(mccFIELD+mcc_INPLANE             ,tmField_mm[fInplane],1e4,0,False);    {FIELD_INPLANE=300.00}
        MccFill(mccFIELD+mcc_CROSSPLANE          ,tmField_mm[fCrossplane],1e4,0,False); {FIELD_CROSSPLANE=300.00}
        MccFill(mccFIELD+mcc_TYPE                ,tmFieldShape);                        {FIELD_TYPE=RECTANGULAR}
        MccFill(mccGANTRY                        ,tmGantry,360);                        {GANTRY=0.00}
        MccFill(mccGANTRY+mcc_UPRIGHT_POSITION   ,tmGantryUp);                          {GANTRY_UPRIGHT_POSITION=0}
        MccFill(mccGANTRY+mcc_ROTATION           ,tmGantryCW,'CW');                     {GANTRY_ROTATION=CW}
        MccFill(mccCOLL+mcc_ANGLE                ,tmCollimator,360);                    {COLL_ANGLE=90.00}
        MccFill(mccCOLL+mcc_OFFSET+mcc_INPLANE   ,tmFieldOffset_mm[fInplane]   ,1000);  {COLL_OFFSET_INPLANE=0.00}
        MccFill(mccCOLL+mcc_OFFSET+mcc_CROSSPLANE,tmFieldOffset_mm[fCrossplane],1000);  {COLL_OFFSET_CROSSPLANE=0.00}
        MccFill(mccSCAN+mcc_DEVICE               ,tmScanDevice);                        {SCAN_DEVICE=MP3 | STARCHECKMAXI}
        MccFill(mccSCAN+mcc_DEVICE+mcc_SETUP     ,tmScan_Setup);                        {SCAN_DEVICE_SETUP=BARA_GUN_TARGET}
        MccFill(mccELECTROMETER                  ,tmElectrometer.tmElType);             {ELECTROMETER=TANDEM}
        MccFill(mccRANGE+'_'+mccFIELD            ,tmElectrometer.tmElRange[FieldCh]);   {RANGE_FIELD=MEDIUM}
        MccFill(mccRANGE+mcc_REFERENCE           ,tmElectrometer.tmElRange[RefCh]);     {RANGE_REFERENCE=AUTO}
        MccFill(''                               ,tmDetectors[FieldCh]);
        MccFill(mcc_REFERENCE                    ,tmDetectors[RefCh]);
        MccFill(mccSCAN                          ,tmScanInfo);
        MccFill(mccMEAS+mcc_MEDIUM               ,tmMedium);                            {MEAS_MEDIUM=WATER}
        MccFill(mccMEAS+mcc_PRESET               ,tmMeasPreset);                        {MEAS_PRESET=REFERENCE_DOSEMETER}
        MccFill(mccMEAS+mcc_TIME                 ,tmMeasTime_s,1e4,0,False);            {MEAS_TIME=0.300}
        MccFill(mccMEAS+mcc_UNIT                 ,tmMeasUnit);                          {MEAS_UNIT=A.U.}
        MccFill(mccScan+mcc_SPEEDS               ,tmScanSpeeds);                        {SCAN_SPEEDS=20.00; 50.00;40.00; 50.00;100.00; 50.00;400.00; 50.00;}
        MccFill(mccDELAY_TIMES                   ,tmDelays);                            {DELAY_TIMES=20.00; 0.000;150.00; 0.000;400.00; 0.000;}
        MccFill(mccPRESSURE                      ,tmPressure_hPa);                      {PRESSURE=1013.25}
        MccFill(mccNORM_TEMPERATURE              ,tmNormTemp_C,200);                    {NORM_TEMPERATURE=20.00}
        MccFill(mccTEMPERATURE                   ,tmTemperature_C,200);                 {TEMPERATURE=20.00}
        MccFill(mccCORRECTION_FACTOR             ,tmPT,9,0,False);                      {CORRECTION_FACTOR=1.0000}
        MccFill(mccEXPECTED_MAX_DOSE_RATE        ,tmDoseRate,1000);                     {EXPECTED_MAX_DOSE_RATE=16.00}
        MccFill(mccGUID                          ,tmGUID);                              {BAD91BB1-133D-4a50-8F3D-2DCE43AA789C}
        MccFill(mccDragDrop                      ,tmDragDrop,'0',False);                {0}
        FParseOk:= FParseOk and (not FParser.EndOfFile);
      until (not FParseOk) or MccTest(mccBEGIN+mcc_DATA,False);
      while FParseOk and NextLine and (not MccTest(mccEND+mcc_DATA,False)) do
        begin
        i:= GetNumPoints;
        SetNumPoints(Succ(i));
        tmPos_mm[i]:= NextFloat;
        tmData[i]  := NextFloat;   {reference values are discarded}
        FParseOk   := GetConversionResult;
        if not FParseOk then
          SetNumPoints(i);
        end; {while}
      InsertOrigin; {does something for starcheckmaxi only when twcMccInsertOrigin=true}
      with tmScanInfo do if (ScanAngle>=90) and (ScanType=snGT) then
        ScanAngle:= ScanAngle-90;  {there seem to be two variants of angle definition}
      end; {if,with}
    end; {if FParseOk}
  end; {if FParseOk and fileformat}
Result:= FParseOk and (CheckFileTypeOnly or ReadResults);
end; {~parsedata}


function TMccProfileData.ReadResults(PostText:String=''): Boolean;
begin
Result:= Inherited ReadResults(PostText);
if Result then
  ScanNrOk:= ScanNr;
end; {~readresults}


function TMccProfileData.GetNumPoints: Integer;
begin
Result:= Length(MccData.tmData);
end; {~getnumpoints}


{$push}{$warn 5057 off}
function TMccProfileData.GetProfile(Index:Integer): twcGrafPoint;

var p: twcGrafPoint;
begin
if InRange(Index,0,Pred(GetNumPoints)) then with MccData do
   begin
   p.X:= tmPos_mm[Index];
   p.Y:= tmData[Index];
   end
else
   p:= Default(twcGrafPoint);
Result:= p;
end; {~get_profile}
{$pop}


procedure TMccProfileData.PutProfile(Index:Integer;
                                     Point:twcGrafPoint);
begin
if InRange(Index,0,Pred(GetNumPoints)) then with MccData do
  begin
  tmData[Index]  := Point.X;
  tmPos_mm[Index]:= Point.Y;
  end;
end; {~putprofile}


{25/08/2015 tmCurveType,MccOrgScanType}
{$push}{$warn 5092 off}
function TMccProfileData.WriteData(AFileName  :String;
                                   AStringList:TStrings;
                                   ASource    :twcDataSource=dsMeasured;
                                   ClearList  :Boolean     =True     ): Boolean;
var i  : Integer;
    Stg: String;

  procedure WriteMccString(AIndentLevel:Integer;
                           ALabel      :String;
                           AStg        :String='';
                           WriteEmpty  :Boolean=False);                         overload;
  begin
  if (WriteEmpty or (Length(AStg)>0)) then
    begin
    while AIndentLevel>0 do
      begin
      Dec(AIndentLevel);
      ALabel:= chTab+ALabel;
      end;
    AStringList.Append(ALabel+ifthen(Length(AStg)>0,'='+Astg,''));
    end;
  end;

  procedure WriteMccString(AIndentLevel:Integer;
                           ALabel      :String;
                           AValue      :Integer);                               overload;
  begin
  WriteMccString(AIndentLevel,ALabel,Num2Stg(AValue));
  end;

  procedure WriteMccString(AIndentLevel:Integer;
                           ALabel      :String;
                           AValue      :twcFloatType;
                           Decimals    :Byte=2);                                overload;
  begin
  WriteMccString(AIndentLevel,ALabel,Num2Stg(AValue,0,Decimals));
  end;

  {21/02/2020 initialize s}
  procedure WriteMccString(AIndentLevel:Integer;
                           ALabel      :String;
                           AValueList  :twcFloatArray;
                           Decimals    :Byte=2);                                overload;
  var i: Integer;
      s: String;
  begin
  s:= '';
  for i:= 0 to Pred(Length(AValueList)) do
    s:= s+Num2Stg(AValueList[i],0,Decimals)+';';
  WriteMccString(AIndentLevel,ALabel,s);
  end;

  procedure WriteMccString(AIndentLevel:Integer;
                           ALabel      :String;
                           AAxis       :twcMeasAxis;
                           Def1,Def2   :String);                                overload;
  var b: Boolean;
  begin
  with MccData do b:= tmAxisDir.c[tmTankAxis[AAxis]]>0;
  WriteMccString(AIndentLevel,ALabel+mcc_AXIS+mcc_DIR,ifthen(b,Def2,Def1)+'_'+ifthen(b,Def1,Def2));
  end;

  procedure WriteMccString(AIndentLevel:Integer;
                           ALabel      :String;
                           ABool       :Boolean);                               overload;
  begin
  WriteMccString(AIndentLevel,ALabel,ifthen(ABool,1,0));
  end;

  {$push}{$warn 5092 off}
  procedure WriteMccString(AIndentLevel:Integer;
                           ALabel      :String;
                           ADetector   :tmDetectorRec);                         overload;
  begin
  ALabel:= mccDETECTOR+ALabel;
  with ADetector do
    begin
    WriteMccString(AIndentLevel,ALabel                  ,tmDetType);                     {DETECTOR=THIMBLE_CHAMBER}
    WriteMccString(AIndentLevel,ALabel+mcc_SUBCODE      ,tmDetSubCode);                  {DETECTOR_SUBCODE=SEMIFLEX}
    WriteMccString(AIndentLevel,ALabel+mcc_RADIUS       ,tmDetRadius_mm);                {DETECTOR_RADIUS=2.75}
    WriteMccString(AIndentLevel,ALabel+mcc_NAME         ,tmDetName);                     {DETECTOR_NAME=PTW 31010 Semiflex}
    WriteMccString(AIndentLevel,ALabel+mcc_SN           ,tmDetSN);                       {DETECTOR_SN=2107}
    WriteMccString(AIndentLevel,ALabel+mcc_CALIBRATION  ,tmDetCalibration);              {DETECTOR_CALIBRATION=304700000.00}
    WriteMccString(AIndentLevel,ALabel+mcc_IS_CALIBRATED,tmDetIsCalibrated);             {DETECTOR_IS_CALIBRATED=1}
    WriteMccString(AIndentLevel,ALabel+mcc_HV           ,tmDetHV);                       {DETECTOR_HV=0.0}
    end;
  end;
  {$pop}

begin
inherited WriteData(AFileName,AStringList,ASource,ClearList);
with AStringList,MccData,tmRefField,tmScanInfo do
  begin
  Clear;
  Result:= True;
  try
    WriteMccString(0,mccBEGIN+mcc_SCAN_DATA           ,'',True);                           {BEGIN_SCAN_DATA}
    WriteMccString(1,mccFORMAT                        ,'CC-Export V1.9');                  {FORMAT=CC-Export V1.9}
    WriteMccString(1,mccFILE_CREATION_DATE            ,MakeTimeString(MccCreated));        {FILE_CREATION_DATE=29-Feb-2012 15:18:00}
    WriteMccString(1,mccLAST_MODIFIED                 ,MakeTimeString(MccModified));       {LAST_MODIFIED=29-Feb-2012 15:18:00}
    WriteMccString(1,mccBEGIN+mcc_SCANnr+'  1'        ,'',True);                           {BEGIN_SCAN  1}
    WriteMccString(2,mccTASK_NAME                     ,tmTaskName);                        {TASK_NAME=tba PDD Profiles}
    WriteMccString(2,mccPROGRAM                       ,tmProgram);                         {PROGRAM=tbaScan}
    WriteMccString(2,mccCOMMENT                       ,tmComment);                         {COMMENT=FFF Acceptance - 7 MV}
    WriteMccString(2,mccMEAS+mcc_DATE                 ,MakeTimeString(tmMeasDate));        {MEAS_DATE=29-Feb-2012 15:15:32}
    WriteMccString(2,mccLINAC                         ,tmLinac);                           {LINAC=selftest}
    WriteMccString(2,mccMODALITY                      ,tmModality);                        {MODALITY=X}
    WriteMccString(2,mccISOCENTER                     ,tmIsoc,1);                          {ISOCENTER=1000.00}
    WriteMccString(2,mccINPLANE+mcc_AXIS              ,tmTankAxis[Inplane]);               {INPLANE_AXIS=X}
    WriteMccString(2,mccCROSSPLANE+mcc_AXIS           ,tmTankAxis[Crossplane]);            {CROSSPLANE_AXIS=Y}
    WriteMccString(2,mccDEPTH+mcc_AXIS                ,tmTankAxis[Beam]);                  {DEPTH_AXIS=Depth}
    WriteMccString(2,mccINPLANE                       ,Inplane,'GUN','TARGET');            {INPLANE_AXIS_DIR=GUN_TARGET}
    WriteMccString(2,mccCROSSPLANE                    ,Crossplane,'LEFT','RIGHT');         {CROSSPLANE_AXIS_DIR=RIGHT_LEFT}
    WriteMccString(2,mccDEPTH                         ,Beam,'UP','DOWN');                  {DEPTH_AXIS_DIR=UP_DOWN}
    WriteMccString(2,mccENERGY                        ,tmEnergy);                          {ENERGY=7.00}
    WriteMccString(2,mccNOMINAL_DMAX                  ,tmDmax);                            {NOMINAL_DMAX=15.00}
    WriteMccString(2,mccSSD                           ,tmSSD_mm);                          {SSD=900.00}
    WriteMccString(2,mccScD                           ,tmSCD_mm);                          {SCD=450.00}
    WriteMccString(2,mccBLOCK                         ,tmBlock);                           {BLOCK=0}
    WriteMccString(2,mccWEDGE_ANGLE                   ,tmWedge);                           {WEDGE_ANGLE=0.00}
    WriteMccString(2,mccFIELD+mcc_INPLANE             ,tmField_mm[fInplane]);              {FIELD_INPLANE=300.00}
    WriteMccString(2,mccFIELD+mcc_CROSSPLANE          ,tmField_mm[fCrossplane]);           {FIELD_CROSSPLANE=300.00}
    WriteMccString(2,mccFIELD+mcc_TYPE                ,twcFieldShapeStg[tmFieldShape]);    {FIELD_TYPE=RECTANGULAR}
    WriteMccString(2,mccGANTRY                        ,tmGantry);                          {GANTRY=0.00}
    WriteMccString(2,mccGANTRY+mcc_UPRIGHT_POSITION   ,tmGantryUp);                        {GANTRY_UPRIGHT_POSITION=0}
    WriteMccString(2,mccGANTRY+mcc_ROTATION           ,ifthen(tmGantryCW,'','C')+'CW');    {GANTRY_ROTATION=CW}
    WriteMccString(2,mccCOLL+mcc_ANGLE                ,tmCollimator);                      {COLL_ANGLE=90.00}
    WriteMccString(2,mccCOLL+mcc_OFFSET+mcc_INPLANE   ,tmFieldOffset_mm[fInplane]);        {COLL_OFFSET_INPLANE=0.00}
    WriteMccString(2,mccCOLL+mcc_OFFSET+mcc_CROSSPLANE,tmFieldOffset_mm[fCrossplane]);     {COLL_OFFSET_CROSSPLANE=0.00}
    if MccOrgFormat=twcMccProfile then
      begin
      WriteMccString(2,mccSCAN+mcc_DEVICE             ,tmScanDevice);                      {SCAN_DEVICE=MP3}
      WriteMccString(2,mccSCAN+mcc_DEVICE+mcc_SETUP   ,tmScan_Setup);                      {SCAN_DEVICE_SETUP=BARA_GUN_TARGET}
      WriteMccString(2,mccELECTROMETER                ,tmElectrometer.tmElType);           {ELECTROMETER=TANDEM}
      WriteMccString(2,mccRANGE+'_'+mccFIELD          ,tmElectrometer.tmElRange[FieldCh]); {RANGE_FIELD=MEDIUM}
      WriteMccString(2,mccRANGE+mcc_REFERENCE         ,tmElectrometer.tmElRange[RefCh]);   {RANGE_REFERENCE=AUTO}
      WriteMccString(2,''                             ,tmDetectors[FieldCh]);
      WriteMccString(2,mcc_REFERENCE                  ,tmDetectors[RefCh]);
      Stg:= mccREF_+mccFIELD;
      WriteMccString(2,Stg+mcc_DEPTH     ,tmRefDepth);                                     {REF_FIELD_DEPTH=100.00}
      WriteMccString(2,Stg+mcc_DEFINED   ,tmRefDef);                                       {REF_FIELD_DEFINED=ISOCENTER}
      WriteMccString(2,Stg+mcc_INPLANE   ,tmRefSize[fInplane]);                            {REF_FIELD_INPLANE=100.00}
      WriteMccString(2,Stg+mcc_CROSSPLANE,tmRefSize[fCrossplane]);                         {REF_FIELD_CROSSPLANE=100.00}
      WriteMccString(2,mccREF_+mccOVERSCAN_FACTOR     ,tmRefOverScan);                     {REF_OVERSCAN_FACTOR=1.00}
      WriteMccString(2,mccREF_+mccSCAN+mcc_POSITIONS  ,tmRefScanPos_mm);                   {REF_SCAN_POSITIONS=-130.00;-125.00;....}
      WriteMccString(2,mccScan+mcc_SPEEDS             ,tmScanSpeeds);                      {SCAN_SPEEDS=20.00; 50.00;40.00; 50.00;100.00; 50.00;400.00; 50.00;}
      WriteMccString(2,mccMEAS+mcc_PRESET             ,tmMeasPreset);                      {MEAS_PRESET=REFERENCE_DOSEMETER}
      WriteMccString(2,mccPRESSURE                    ,tmPressure_hPa);                    {PRESSURE=1013.25}
      WriteMccString(2,mccTEMPERATURE                 ,tmTemperature_C);                   {TEMPERATURE=20.00}
      WriteMccString(2,mccNORM_TEMPERATURE            ,tmNormTemp_C);                      {NORM_TEMPERATURE=20.00}
      WriteMccString(2,mccCORRECTION_FACTOR           ,tmPT);                              {CORRECTION_FACTOR=1.0000}
      WriteMccString(2,mccEXPECTED_MAX_DOSE_RATE      ,tmDoseRate);                        {EXPECTED_MAX_DOSE_RATE=16.00}
      WriteMccString(2,mccGUID                        ,tmGUID);                            {BAD91BB1-133D-4a50-8F3D-2DCE43AA789C}
      WriteMccString(2,mccDragDrop                    ,tmDragDrop);                        {0}
      end;
    WriteMccString(2,mccSCAN+mcc_CURVETYPE             ,tmCurveType);                      {SCAN_CURVETYPE=INPLANE_PROFILE}
    WriteMccString(2,mccSCAN+mcc_DEPTH                 ,tmScanDepth_mm);                   {SCAN_DEPTH=100.00}
    WriteMccString(2,mccSCAN+mcc_OFFAXIS+mcc_INPLANE   ,tmScanOffAxis_mm[fInplane]);       {SCAN_OFFAXIS_INPLANE=0.00}
    WriteMccString(2,mccSCAN+mcc_OFFAXIS+mcc_CROSSPLANE,tmScanOffAxis_mm[fCrossplane]);    {SCAN_OFFAXIS_CROSSPLANE=0.00}
    WriteMccString(2,mccSCAN+mcc_ANGLE                 ,tmScanAngle);                      {SCAN_ANGLE}
    WriteMccString(2,mccSCAN+mcc_DIAGONAL              ,tmScanDiagonal);                   {SCAN_DIAGONAL=NOT_DIAGONAL}
    WriteMccString(2,mccSCAN+mcc_DIRECTION             ,tmScanDirection);                  {SCAN_DIRECTION=POSITIVE}
    WriteMccString(2,mccSCAN+mcc_COLOR                 ,tmScanColor);                      {255}
    WriteMccString(2,mccMEAS+mcc_MEDIUM                ,tmMedium);                         {MEAS_MEDIUM=WATER}
    WriteMccString(2,mccMEAS+mcc_TIME                  ,tmMeasTime_s);                     {MEAS_TIME=0.300}
    WriteMccString(2,mccMEAS+mcc_UNIT                  ,tmMeasUnit);                       {MEAS_UNIT=A.U.}
    WriteMccString(2,mccDELAY_TIMES                    ,tmDelays);                         {DELAY_TIMES=20.00; 0.000;150.00; 0.000;400.00; 0.000;}
    WriteMccString(2,mccBEGIN+mcc_DATA                 ,'',True);                          {BEGIN_DATA}
    for i:= 0 to High(tmData) do
      WriteMccString(3,Format('%0.2f'+chTab+chTab+'%9.5e'+chTab+'10.000E+00',[tmPos_mm[i],tmData[i]]),'',True);
    WriteMccString(2,mccEND+mcc_DATA                   ,'',True);                          {END_DATA}
    WriteMccString(1,mccEND+mcc_SCANnr+'  1'           ,'',True);                          {END_SCAN  1}
    WriteMccString(0,mccEND+mcc_SCAN_DATA              ,'',True);                          {END_SCAN_DATA}
    if Length(FExtraText)>0 then
      for i:= 0 to Pred(Length(FExtraText)) do
        WriteMccString(0,'# '+FExtraText[i],'',True);
   except
     Result:= False;
   end;
  end;
end; {~writedata}
{$pop}


function TMccProfileData.WriteData(AFileName :String;
                                   OutPutType:twcFileType;
                                   ASource   :twcDataSource=dsMeasured;
                                   SetExt    :Boolean=True          ): Boolean;
begin
if OutPutType=FileFormat then Result:= WriteData(AFileName,False,ASource,SetExt)
else                          Result:= False;
end; {~writedata}


destructor TMccProfileData.Destroy;
begin
with MccData,tmRefField do
  begin
  Finalize(tmRefScanPos_mm);
  Finalize(tmScanSpeeds);
  Finalize(tmDelays);
  Finalize(tmPos_mm);
  Finalize(tmData);
  end;
Inherited;
end; {~destroy}


//----------THdfProfileData-----------------------------------------------------

{09/12/2015 added sharedparser}
{29/03/2016 added FRegisteredFiles}
{01/05/2020 transfer of binarydata and filenames}
constructor THdfProfileData.Create(SharedParser:toTNumParser =nil;
                                   ParsedFile  :String       ='';
                                   BinaryData  :TMemoryStream=nil;
                                   BinaryFile  :String       ='';
                                   AStatusProc :toExtMsgProc =nil;
                                   AIdentity   :String       ='hdf/generic');
begin
Inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
FRegisteredFiles:= '.hdf.txt';
end; {~create}


procedure THdfProfileData.SetDefaults;
begin
Inherited;
DefaultExtension:= '.hdf';
FScanType       := snGenericProfile;
end; {~setdefaults}


{04/08/2015 check on zero-length IdentificationStg}
{29/07/2020 removed all character checking for generic type; just want to find two numbers}
function THdfProfileData.CheckData(AStringList:TStrings): Boolean;
var i,j: Integer;
begin
Result:= inherited CheckData(AStringList) and (Length(IdentificationStg)>0);
if Result then
  begin
  if Pos(hdfID,LowerCase(IdentificationStg))=1 then
    FileFormat:= twcHdfProfile
  else
    with FParser do
      begin
      j          := 0;
      CurrentLine:= IdentificationStg;
      for i:= 1 to 2 do
        begin
        NextFloat;
        if ConversionResult then
          j:= i;
        end;
      if j=2 then
        FileFormat:= twcGenericProfile;
      end;
  end; {if result}
end; {~checkdata}


{08/06/2020 skip too short lines in GetNextLine; do not test on ConversionResult at EndOfFile}
function THdfProfileData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
var v,r  : twcFloatType;
    c    : twcCoordinate;
    tAxis: twcTankAxis;

  function GetNextLine(var Remainder:String): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk := NextLine(True);
    Remainder:= CurrentLine;
    end;
  Result:= FParseOk;
  end;

  function GetNextLine(var a         :array of twcFloatType;
                       Arraylen      :Byte;
                       UseCurrentLine:Boolean=False;
                       Multiplication:wmsRealType=1): Boolean;  overload;
  var i: Byte;
  begin
  if FParseOk then with FParser do
    begin
    if not UseCurrentLine then
      repeat
        FParseOk:= NextLine;
      until (Length(CurrentLine)>2*Arraylen-1) or EndOfFile;
    for i:= 0 to Pred(ArrayLen) do
      if FParseOk then
        try
          a[i]:= NextFloat*Multiplication;
          FParseOk:= ConversionResult;
         except
          a[i]:= 0;
          FParseOk:= False;
         end;
    end;
  Result:= FParseOk;
  end;

  {$push}{$warn 5057 off:Local variable "a" does not seem to be initialized}
  function GetNextLine(UseCurrentLine:Boolean=False): Boolean;  overload;
  const n=3;
  var a: array[1..n] of twcFloatType;
      i: Integer;
  begin
  Result:= GetNextLine(a,ifthen(FileFormat=twcHdfProfile,n,2),UseCurrentLine);
  if Result then
    begin
    i:= Length(FHDFdata);
    SetLength(FHDFdata,Succ(i));
    with FHDFdata[i] do
      begin
      if FileFormat=twcHdfProfile then X:= a[1]*v
      else                             X:= a[1];
      Y       := a[2];
      FDataMax:= Max(Y,FDataMax);
      end;
    end;
  end;
  {$pop}

{$push}{$warn 5057 off:Local variable does not seem to be initialized}
begin
Result  := inherited ParseData(CheckFileTypeOnly);
FDataMax:= -1000;
FParseOk:= Result or (CheckFileTypeOnly xor (FileFormat in [twcHdfProfile,twcGenericProfile]));
if (Result and (not CheckFileTypeOnly) and (FileFormat in [twcHdfProfile,twcGenericProfile]) ) then with FParser do
  begin
  if AutoDecimalPoint then
    FParser.AutoSetDecPoint(AutoDecimalList)
  else
     FParser.SetDecPointChars(['.']);
  FParseOk:= GotoTop(True);                                     {# Track:  Crossplane,Depth,Inplane}
  if FileFormat=twcHdfProfile then
    begin
    GetNextLine(ScanStart.t,3,True,100);
    v       := 0;
    ScanStop:= ScanStart;
    while GetNextLine(c.t,3,True,100) do
      begin
      v       := v+GetDistance(ScanStop,c);
      ScanStop:= c;
      end;
    FParseOk:= v>0;
    if FParseOk then
      begin
      try
        r:= GetDistance(ScanStart,ScanStop)/v;
       except
        r:= 1;
       end;
      if r<>1 then
        for tAxis:= X to Z do
          if abs(ScanStart.t[tAxis]-ScanStop.t[tAxis])>0.5 then
            ScanStop.t[tAxis]:= r*(ScanStop.t[tAxis]-ScanStart.t[tAxis])+ScanStart.t[tAxis]
          else
            ScanStop.t[tAxis]:= ScanStart.t[tAxis];
      v:= GetDistance(ScanStart,ScanStop);
      while FParseOk and (Pos('#',CurrentLine)>0) do
        NextLine;       {# Length..}
      FParseOk:= FParseOk and GetNextLine(True);                   {0.0 365.0 0.0}
      end;
    end; {hdfprofile}
  while FParseOk do
    GetNextLine;                               {overige xy-paren}
  FParseOk:= EndOfFile and (GetNumPoints>1);
  if FParseOk then
    begin
    if FileFormat<>twcHdfProfile then
      begin
      FileFormat    := twcGenericProfile;
      FScanType     := snGenericProfile;
      ScanStart.t[X]:= FHDFdata[0             ].X;
      ScanStart.t[Y]:= 0;
      ScanStart.t[Z]:= 0;
      ScanStop.t[X] := FHDFdata[GetNumPoints-1].X;
      ScanStop.t[Y] := 0;
      ScanStop.t[Z] := 0;
      end;
    FScanLength:= GetDistance(ScanStart,ScanStop);
    FStepSize  := FScanLength/Max(1,GetNumPoints-1);
    end;
  end;
Result:= CheckFileTypeOnly or (Result and ReadResults);
end; {~parsedata}
{$pop}


function THdfProfileData.GetNumPoints: Integer;
begin
Result:= Length(FHDFdata);
end; {~getnumpoints}


{$push}{$warn 5057 off:Local variable does not seem to be initialized}
function THdfProfileData.GetProfile(Index:Integer): twcGrafPoint;

var p: twcGrafPoint;
begin
if InRange(Index,0,Pred(GetNumPoints)) then p:= FHDFdata[Index]
else                                        p:= Default(twcGrafPoint);
Result:= p;
end; {~get_profile}
{$pop}


procedure THdfProfileData.PutProfile(Index:Integer;
                                     Point:twcGrafPoint);
begin
if InRange(Index,0,Pred(GetNumPoints)) then
  FHDFdata[Index]:= Point;
end; {~putprofile}


destructor THdfProfileData.Destroy;
begin
Finalize(FHDFdata);
Inherited;
end; {~destroy}

//----------TCmsProfileData--------------------------------------------------

{09/12/2015 added sharedparser}
{29/03/2016 added FRegisteredFiles}
{01/05/2020 transfer of binarydata and filenames}
constructor TCmsProfileData.Create(SharedParser:toTNumParser =nil;
                                   ParsedFile  :String       ='';
                                   BinaryData  :TMemoryStream=nil;
                                   BinaryFile  :String       ='';
                                   AStatusProc :toExtMsgProc =nil;
                                   AIdentity   :String       ='xio');
begin
Inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
FRegisteredFiles:= '.xio';
end; {~create}


function TCmsProfileData.CheckData(AStringList:TStrings): Boolean;
begin
Result:= (inherited CheckData(AStringList)) and (IdentificationStg=xioID);
if Result then
  FileFormat:= twcCmsProfile
else
  FileFormat:= twcUnknown;
end; {~checkdata}


function TCmsProfileData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
var pm,n : word;
    tAxis: twcTankAxis;
    c    : Char;
    ym,v : twcFloatType;

  function GetNextLine(ASearchText:String;
                       var V      :twcFloatType): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText);
    V       := NextFloat;
    end;
  Result:= FParseOk;
  end; {searchline}

  function GetNextLine: Boolean;  overload;
  var a: array[1..2] of twcFloatType;
      i: word;
  begin
  with FParser do
    begin
    FParseOk:= FParseOk and NextLine;
    if FParseOk then
      begin
      for i:= 1 to 2 do
        a[i]:= NextFloat;
      i:= Length(FHDFdata);
      SetLength(FHDFdata,Succ(i));
      with FHDFdata[i] do begin  X := a[1];  Y := a[2];  end;
      if a[1]>ym then     begin  pm:= i;     ym:= a[2];  end;
      end;
    end;
  Result:= FParseOk;
  end;

begin
Result:= inherited ParseData(CheckFileTypeOnly) or (CheckFileTypeOnly xor (FileFormat=twcCmsProfile));
if (Result and (not CheckFileTypeOnly) and (FileFormat=twcCmsProfile)) then with FParser do
  begin
  if AutoDecimalPoint then
    FParser.AutoSetDecPoint(AutoDecimalList)
  else
     FParser.SetDecPointChars(['.']);
  Linac   :='CMS';
  v       := 0;
  FParseOk:= GotoTop(True);
  if FParseOk and (not CheckFileTypeOnly) then
    begin
    FParseOk      := Search('Doc') and NextLine;
    c             := NextChar;
    ScanStart.t[X]:= NextFloat;
    GetNextLine(':',ScanStart.t[Y]);
    GetNextLine(':',ScanStart.t[Z]);
    GetNextLine('Angle',v);
    if c='S'           then begin  FScanType:= snGT;   tAxis:= X;  end
    else if Round(v)=0 then begin  FScanType:= snAB;   tAxis:= Y;  end
    else                    begin  FScanType:= snPDD;  tAxis:= Z;  end;
    NextLine;
    pm:= 0;
    ym:= -9e9;
    while FParseOk do
      GetNextLine;
    FParseOk:= EndOfFile and (GetNumPoints>1);
    n      := Pred(GetNumPoints);
    if FParseOk then
      begin
      if (FScanType=snGT) or ((FScanType=snPDD) and (n-pm>pm) and (FHDFdata[0].X<-2) and (FHDFdata[n].X<2)) then
        for pm:= 0 to n do
          FHDFdata[pm].X:= -FHDFdata[pm].X;
      ScanStop          := ScanStart;
      ScanStart.t[tAxis]:= ScanStart.t[tAxis]+FHDFdata[0].X;
      ScanStop .t[tAxis]:= ScanStop .t[tAxis]+FHDFdata[n].X;
      FScanLength       := GetDistance(ScanStart,ScanStop);
      FStepSize         := FScanLength/Max(1,n);
      end;
    end;
  end;
Result:= CheckFileTypeOnly or (Result and ReadResults);
end; {~parsedata}


//----------TSchusterProfileData--------------------------------------------------

{09/12/2015 added sharedparser}
{29/03/2016 added FRegisteredFiles}
{01/05/2020 transfer of binarydata and filenames}
constructor TSchusterProfileData.Create(SharedParser:toTNumParser =nil;
                                        ParsedFile  :String       ='';
                                        BinaryData  :TMemoryStream=nil;
                                        BinaryFile  :String       ='';
                                        AStatusProc :toExtMsgProc =nil;
                                        AIdentity   :String       ='schuster');
begin
Inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
FRegisteredFiles:= '.txt';
end; {~create}


procedure TSchusterProfileData.SetDefaults;
begin
FScanType  := snGenericHorizontal;
FStepSize  := twcDefSchusterPixelCm;
FScanLength:= FStepSize*twcDefSchusterPoints;
if twcGenericToElectron then Modality:= 'E'
else                         Modality:= 'X';
Depth      := 0;
Inherited;
end; {~setdefaults}


{29/09/2016 specific implementation for identification of schuster file}
function TSchusterProfileData.CheckData(AStringList:TStrings): Boolean;
var SPtr: ^TStrings;
    i   : Integer;
begin
if assigned(AStringList) then SPtr:= Addr(AStringList)
else                          SPtr:= Addr(Parser.Strings);
i:= Max(0,Pred(SPtr^.Count));
if i>0 then IdentificationStg:= SPtr^.Strings[i]                                //do not try to read non-existent data
else        IdentificationStg:= '';
Result:= CountChars(IdentificationStg,twcDefSchusterDelimiters)>=Pred(twcDefSchusterPoints);
if Result then
  FileFormat:= twcSchusterProfile;
end; {~checkdata}


function TSchusterProfileData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
var i: Integer;
    s: String;
  function GetNextLine(ASearchText  :String;
                       var Remainder:String): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk  := Search(ASearchText);
    Remainder:= RemainderOfLine;
    end;
  Result:= FParseOk;
  end;

  function GetNextLine(ASearchText:String;
                       var AValue :twcFloatType): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText);
    AValue := NextFloat;
    end;
  Result:= FParseOk and FParser.ConversionResult;
  end; {searchline}

begin
Result:= inherited ParseData(CheckFileTypeOnly);
if Result and (not CheckFileTypeOnly) then with FParser do
  begin
  FParseOk:= GotoTop;
  FParseOk:= GetNextLine('Profile measured on ',ScanTime);
  if FParseOk and (not CheckFileTypeOnly) then
    begin
    FParseOk:= GetNextLine('Linac: ',Linac) and GetNextLine('at',FEnergy);
    if FParseOk then
      begin
      s:= LowerCase(CurrentLine);
      if Pos('photons',s)=1 then Modality:= 'X'
      else if s[1]='e'      then Modality:= 'E'
      else                       Modality:= 'P';
      FParseOk:= NextLine;
      if LowerCase(CurrentLine)='crossplane' then FScanType:= snAB
      else                                        FScanType:= snGT;
      FParseOk:= FParseOk and GetNextLine('Buildup:',Depth);
      while FParseOk and (CountChars(CurrentLine,twcDefSchusterDelimiters)<Pred(twcDefSchusterPoints)) do
        FParseOk:= NextLine;
      i:= 0;
      SetLength(FSchusterdata,twcDefSchusterPoints);
      while FParseOk and (i<twcDefSchusterPoints) do
        begin
        FSchusterData[i]:= NextFloat;
        FParseOk         := ConversionResult;
        Inc(i);
        end;
      if FParseOk then
        FileFormat:= twcSchusterProfile;
      end;
    end;
  end;
Result:= CheckFileTypeOnly or (Result and ReadResults);
end; {~parsedata}


function TSchusterProfileData.GetNumPoints: Integer;
begin
Result:= Length(FSchusterData);
end; {~getnumpoints}


function TSchusterProfileData.GetProfile(Index:Integer): wmsRealType;
begin
if InRange(Index,0,Pred(GetNumPoints)) then Result:= FSchusterData[Index]
else                                        Result:= 0;
end; {~get_profile}


procedure TSchusterProfileData.PutProfile(Index:Integer;
                                          Point:wmsRealType);
begin
if Length(FSchusterData)=0 then
  SetLength(FSchusterdata,twcDefSchusterPoints);
if InRange(Index,0,Pred(GetNumPoints)) then
  FSchusterData[Index]:= Point;
end; {~putprofile}


destructor TSchusterProfileData.Destroy;
begin
Finalize(FSchusterData);
Inherited;
end; {~destroy}


//----------TPipsProfileData-----------------------------------------------------

{09/12/2015 added sharedparser}
{29/03/2016 added FRegisteredFiles}
{01/05/2020 transfer of binarydata and filenames}
constructor TPipsProfileData.Create(PixelCm     :twcFloatType  =twcDefPipsPixelCm;
                                    SharedParser:toTNumParser =nil;
                                    ParsedFile  :String       ='';
                                    BinaryData  :TMemoryStream=nil;
                                    BinaryFile  :String       ='';
		                    AStatusProc :toExtMsgProc =nil;
                                    AIdentity   :String      ='pips');
begin
Inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
if PixelCm<=0 then FStepSize:= twcDefPipsPixelCm
else               FStepSize:= PixelCm;
FRegisteredFiles:= '.dat';
end; {~create}


procedure TPipsProfileData.SetDefaults;
begin
FScanType:= snGenericHorizontal;
Inherited;
end; {~setdefaults}


function TPipsProfileData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
var i,Points,Ymax: Integer;
  function GetNextLine(var Remainder:String): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk  := NextLine;
    Remainder:= CurrentLine;
    end;
  Result:= FParseOk;
  end;

  function GetNextLine(ASearchText:String;
                       var AValue :Integer): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText);
    AValue := NextInteger;
    end;
  Result:= FParseOk and FParser.ConversionResult;
  end;

  function GetNextLine(ProfilePoint  :Integer;
                       UseCurrentLine:Boolean=False): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    if not UseCurrentLine then
      FParseOk:= NextLine;
    if FParseOk then
      begin
      if Succ(ProfilePoint)>Length(FPIPSdata) then
        SetLength(FPIPSdata,Succ(ProfilePoint));
      FPIPSdata[ProfilePoint]:= Ymax-NextFloat;
      end;
    end;
  Result:= FParseOk and FParser.ConversionResult;;
  end;

begin
Result:= inherited ParseData(CheckFileTypeOnly);
if Result and (not CheckFileTypeOnly) then with FParser do
  begin
  FParseOk:= GotoTop(True) and (Pos('Type of Cross-Section:',CurrentLine)=1);
  i       := 0;
  Ymax    := 0;
  Points  := 0;
  if FParseOk and (not CheckFileTypeOnly) then
    begin
    if Pos('row',LowerCase(CurrentLine))>0 then FScanType:= snAB
    else                                        FScanType:= snGT;
    FParseOk   := GetNextLine('Index:',i)                and
                  GetNextLine('Highest pixel:',Ymax)     and
                  GetNextLine('Number of points:',Points);
    SetLength(FPIPSdata,Points);
    FScanPos   := StepSize*(i-Pred(Points)/2);
    FScanLength:= StepSize*Pred(Points);
    while Pos(':',CurrentLine)>0 do
      FParseOk:= NextLine;
    FParseOk:= FParseOk and GetNextLine(0,True);                                {365.0}
    i:= 1;
    while FParseOk and (i<Points) do
      begin
      GetNextLine(i);
      Inc(i);
      end;
    if not FParseOk then
      SetLength(FPIPSdata,i);
    FParseOk:= (i>twcDefMinProfilePoints) and (i=GetNumPoints) and EndOfFile;
    end;
  if FParseOk then
    FileFormat:= twcPipsProfile;
  end;
Result:= CheckFileTypeOnly or (Result and ReadResults);
end; {~parsedata}


function TPipsProfileData.GetNumPoints: Integer;
begin
Result:= Length(FPIPSdata);
end; {~getnumpoints}


function TPipsProfileData.GetProfile(Index:Integer): wmsRealType;
begin
if InRange(Index,0,Pred(GetNumPoints)) then Result:= FPIPSdata[Index]
else                                        Result:= 0;
end; {~get_profile}


procedure TPipsProfileData.PutProfile(Index:Integer;
                                     Point:wmsRealType);
begin
if InRange(Index,0,Pred(GetNumPoints)) then FPIPSdata[Index]:= Point;
end; {~putprofile}


destructor TPipsProfileData.Destroy;
begin
Finalize(FPIPSdata);
Inherited;
end; {~destroy}

//----------TWmsData------------------------------------------------------------

{09/12/2015 added sharedparser}
{29/03/2016 added FRegisteredFiles}
{27/09/2016 added FBinaryAllowed}
{01/05/2020 transfer of binarydata and filenames}
constructor TWmsData.Create(SharedParser:toTNumParser =nil;
                            ParsedFile  :String       ='';
                            BinaryData  :TMemoryStream=nil;
                            BinaryFile  :String       ='';
                            AStatusProc :toExtMsgProc =nil;
                            AIdentity   :String       ='wms');
begin
Inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
FRegisteredFiles:= '.wtx.wda';
FBinaryAllowed  := True;
end; {~create}


procedure TWmsData.SetDefaults;
var i: wmsComments;
    t: twcTankAxis;
const
    a:twcTankAxisID  =('X','Y','Z');
begin
Inherited;
DefaultExtension:= '.wtx';
with wmsFileHeader,wmsRec06 do                                                  //layout of code follows layout of text file
  begin
  Stg2Char('',wmhDate);
  Stg2Char('',wmhDevice);
  Stg2Char('',wmhTime);
  wmsRecSize06:= wmsGenHeadSize;
  wmhIaaa06   :=   6;
  wmhAxisID   :=   a;
  for t:= X to Z do
    wmhAxisSign.t[t]:= 1;
  wmhIsoDose  := 100;
  wmhFmax     :=  1   ;         wmhRmax     :=   1   ;
  wmhFofs     :=  0   ;         wmhRofs     :=   0   ;
  wmhFsign    :=  1   ;         wmhRsign    :=   1   ;
  wmhKs       :='G'   ;         wmhOrKs     :='G'    ;
  FillChar(wmhBorders,SizeOf(wmhBorders),0);
  wmhMcm      :=  0   ;         wmhCmSec    :=   0.5 ;    wmhIsoDose  := 100;
  wmhMTime    :=  0.01;         wmhNdose    :=   0.01;    wmhNdosediv := 100;
  if twcGenericToElectron then  wmhRaType   := 'E'
  else                          wmhRaType   := 'X';
  wmhFdType   := 'O'  ;
  wmhASD      :=  0   ;         wmhEnergy   :=   0   ;
  wmhFdAB_cm  :=  0   ;         wmhFdGT_cm  :=   0   ;
  wmhDRdiv    :=  1   ;         wmhNsamp    :=   5   ;
  wmhBRect1_cm:=  0   ;         wmhBRect2_cm:=   0   ;    wmhBDiam_cm :=   0;
  wmhTrayTr   :=  1   ;         wmhCollim   :=  90   ;    wmhGantry_cm:=   0;
  wmhNOP1     :=  #0  ;         wmhNOP2     :=  #0   ;
  wmhSSD_cm   := twcDefaultSSDcm;
  for I:= wmhG1 to wmhU4 do
    Stg2Char('',wmhComs[I,1]);
  wmhDosCon   :=  0   ;         wmhBShape   := 'S';
  wmhAIdent   :=  0   ;         wmhWIdent   :=   0;
  FillChar(wmhINOP3,SizeOf(wmhINOP3),0);
  wmhIzzz06   :=  6   ;
  end;
wmsProfileHeader.wmsRec07.wmpIaaa07:= wmsProfileType;
end; {~setdefaults}


{$push}{$warn 5092 off}
{06/10/2020 fundamentals alternative}
procedure TWmsData.Stg2char(Stg                           :string;
		                        var WMS_Char_Array:array of Char);
var A: wmsCharConvRec absolute WMS_Char_Array;       {wms_char_array als string}
begin
Stg   := Stg.Trim('"');
Stg   := Copy(Stg,1,Min(Length(Stg),Sizeof(WMS_Char_Array)-3))+'$+'; {maximum allowed string length is arraylength-1}
A.s   := Stg.PadRight(Pred(Sizeof(WMS_Char_Array)),chNull);
A.c[1]:= '+';
end; {~stg2char}
{$pop}


//fill wms type character array
function TWmsData.Char2Stg(var WMS_Char_Array:array of Char;
                           ArrayLength       :Byte           ): string;
var A: string[Pred(wmsMax_Char)];
    i: Byte;
begin
A[0]:= Chr(Pred(ArrayLength));                     {maak string van a}
i:= 1;
repeat
   A[i]:= WMS_Char_Array[i];
   Inc(i);
until i=ArrayLength;
A[0]:= Chr(Pred(Pos('$+',A)));
Result:= A;
end; {~char2stg}


//wtx conventions
function TWmsData.GetScanTypeString: string;
begin
case wmsFileHeader.wmsRec06.wmhKs of {U/P/D/G/A/L}
  'G': Result:= 'GT';
  'A': Result:= 'AB';
  'D': Result:= 'PDD';
  'L': Result:= 'line';
  'P': Result:= 'plane';
  'U': Result:= 'undefined';
 else  Result:= 'illegal descriptor';
 end;
end; {~getscantypestring}


//output is user defined field size in scan direction
function TWmsData.GetFieldLength: twcFloatType;
begin
with wmsFileHeader.wmsRec06 do case ScanType of
  snGT: Result:= wmhFdGT_cm;
  snAB: Result:= wmhFdAB_cm;
 else Result:= UndefinedVal;
 end;
end; {~getfieldlength}


function TWmsData.GetBeamType: twcBeamType;
begin
if wmsFileHeader.wmsRec06.wmhRaType='X' then Result:= Photons
else                                         Result:= Electrons;
end; {~getbeamtype}


//only defined for true horizontal scans
function TWmsData.GetFieldDepth: twcFloatType;
begin
with wmsFileHeader.wmsRec06 do case ScanType of
  snGT,snAB: Result:= wmhBorders[1,Beam];
 else        Result:= UndefinedVal;
 end;
end; {~getfielddepth}


//conversion of wtx character to twcScanTypes
function TWmsData.GetScanType: twcScanTypes;
begin
case wmsFileHeader.wmsRec06.wmhKs of {U/P/D/G/A/L}
  'G': Result:= snGT;
  'A': Result:= snAB;
  'D': Result:= snPDD;
  'L': Result:= snFreeScan;
  'P': Result:= snPlane;
 else  Result:= snUndefined;
 end;
end; {~getscantype}


//check on binary filetype, needs additional fileopen action
function TWmsData.IsBinary(AFileName:String): Boolean;
begin
Result:= GetFileType(AFileName)=twcWDA;
end; {~isbinary}


{$push}{$warn 5057 off: Local variable "p" does not seem to be initialized}{$I-}
{15/08/2016 BinStream implementation}
{01/05/2020 take advantage of preloaded binstream}
{19/08/2020 speed improvement with BinaryOnly}
function TWmsData.GetFileType(AFileName :String;
                              BinaryOnly:Boolean=False): twcFileType;
var n,m: Integer;
    h  : wmsFileHeaderBlock;
begin
Result:= Inherited GetFileType(AFileName,BinaryOnly);
if ((Result=twcUnknown) and LoadBinStream(AFileName))then
  begin
  m:= wmsGenHeadBlockSize div wmsRecSize;
  BinStream.Seek(0,soFromBeginning);
  n:= BinStream.Read(h,m);
  with h,wmsRec06 do
    if (n=m) and (wmsRecSize06=wmsGenHeadSize) and (wmhIaaa06=6) {and (wmhIzzz06=6)} then
      Result:= twcWDA;
  BinStreamType:= Result;
  end;
end; {~getfiletype}
{$pop}


{$push}{$warn 5057 off: Local variable "p" does not seem to be initialized}{$I-}
function  TWmsData.GetProfile(Index          :wmsIntType;
                              ConvertToAccPos:Boolean=False): wmsProfilePoint;

var p: wmsProfilePoint;
    m: twcMeasAxis;
begin
if InRange(Index,0,Pred(GetNumPoints)) then p:= wmsProfile[Index]
else                                        p:= Default(wmsProfilePoint);
Result:= p;
if ConvertToAccPos then with wmsFileHeader.wmsRec06 do
  for m:= Inplane to Beam do
    Result.wmpAccPos[m]:= p.wmpPos[wmhAxisID[m]]*wmhAxisSign.c[wmhAxisID[m]];
end; {~get_profile}
{$pop}


procedure TWmsData.PutProfile(Index            :wmsIntType;
                              WmsPoint         :wmsProfilePoint;
                              ConvertFromAccPos:Boolean=False);
var m: twcMeasAxis;
begin
if InRange(Index,0,Pred(GetNumPoints)) then
  begin
  wmsProfile[Index]:= WmsPoint;
  if ConvertFromAccPos then with wmsProfile[Index],wmsFileHeader.wmsRec06 do
    for m:= Inplane to Beam do
      wmpPos[wmhAxisID[m]]:= WmsPoint.wmpAccPos[m]*wmhAxisSign.c[wmhAxisID[m]];
  end;
end; {~putprofile}


procedure TWmsData.SetNumpoints(Npoints:wmsIntType);
begin
with wmsProfileHeader do
  begin
  try
    Npoints:= Min(Max(0,Npoints),(High(wmsRecSize07)-wmsProHeadSize) div wmsProPointSize);
   except
    Npoints:= 0;
   end;
  wmsRecSize07:= Npoints*wmsProPointSize+wmsProHeadSize;
  wmsRec07.wmpNpoints:= Npoints;
  end;
SetLength(wmsProfile,Npoints);
end; {~setnumpoints}


function TWmsData.GetNumPoints: Integer;
begin
Result:= wmsProfileHeader.wmsRec07.wmpNpoints;
end; {~getnumpoints}


{11/06/2020}
procedure TWmsData.SetLinacFromHeader;
var Stg: String;
    i  : Integer;
begin
with wmsFileHeader.wmsRec06 do
  begin
  Stg:= Char2Stg(wmhDevice,SizeOf(wmhDevice));
  i  := Succ(Stg.IndexOfAny(CharsetToString(csNotAlphaNumeric)));               {U02: Elekta SL25-15 (i:= 4), indexofany is zero-based}
  if i>0 then
    begin
    Delete(Stg,i,Length(Stg)-i+1);
    Stg2Char(Stg,wmhDevice);
    end;
  end;
Linac:= Stg;
end; {~setlinacfromheader}


{$push}{$warn 5057 off: Local variable "p" does not seem to be initialized}
{11/06/2020 added SetLinacFromHeader}
function TWmsData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
var Stg: String;
    m  : twcMeasAxis;
    p  : wmsProfilePoint;
    xyz: twcXYZset;
    c  : Char;
    n  : wmsComments;
    i,j: wmsIntType;

  function GetNextLine(var Remainder:String): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk  := NextLine;
    Remainder:= CurrentLine;
    end;
  Result:= FParseOk;
  end;

  function GetNextLine(var wmsCharStg:array of Char;
                       UseCurrentLine:Boolean=False;
                       DetectCR      :Boolean=False): Boolean;  overload;
  var Stg: string;
  begin
  if FParseOk then with FParser do
    begin
    if not UseCurrentLine then
      FParseOk:= NextLine;
    if DetectCR then
      begin
      Stg    := CurrentLine;
      FParseOk:= Stg[1]='"';
      while FParseOk and ((Length(CurrentLine)=0) or (CurrentLine[Length(CurrentLine)]<>'"')) do
        begin
        FParseOk:= NextLine;
        if Length(CurrentLine)>0 then
          Stg:= Stg+', '+CurrentLine;
        end;
      end
    else
      begin
      FParseOk:= (Length(CurrentLine)>=2) and
                (CurrentLine[1]='"'    ) and (CurrentLine[Length(CurrentLine)]='"');
      Stg    := CurrentLine;
      end;
    if FParseOk then
      Stg2Char(Copy(Stg,1,Min(Length(Stg),wmsMax_Char)),wmsCharStg);
    end;
  Result:= FParseOk;
  end;

  function GetNextLine(var a         :array of wmsIntType;
                       Arraylen      :Byte;
                       UseCurrentLine:Boolean=False): Boolean;  overload;
  var i: Byte;
  begin
  if FParseOk then with FParser do
    begin
    if not UseCurrentLine then
      FParseOk:= NextLine;
    for i:= 0 to Pred(ArrayLen) do
      if FParseOk then
        begin
        a[i]:= NextInteger;
        FParseOk:= ConversionResult;
        end;
    end;
  Result:= FParseOk;
  end;

  function GetNextLine(var t         :twcTankAxisSign;
                       UseCurrentLine:Boolean=False  ): Boolean;  overload;
  var i: twcTankAxisChar;
  begin
  if FParseOk then with FParser do
    begin
    if not UseCurrentLine then
      FParseOk:= NextLine;
    for i:= 'X' to 'Z' do
      if FParseOk then
        begin
        t.c[i]  := NextInteger;
        FParseOk:= ConversionResult;
        end;
    end;
  Result:= FParseOk;
  end;

  function GetNextLine(var a         :array of wmsRealType;
                       Arraylen      :Byte;
                       UseCurrentLine:Boolean=False): Boolean;  overload;
  var i: Byte;
  begin
  if FParseOk then with FParser do
    begin
    if not UseCurrentLine then
      FParseOk:= NextLine;
    for i:= 0 to Pred(ArrayLen) do
      if FParseOk then
        begin
        a[i]    := NextFloat;
        FParseOk:= ConversionResult;
        end;
    end;
  Result:= FParseOk;
  end;

  function GetNextLine(var Int1      :wmsIntType;
                       UseCurrentLine:Boolean=False): Boolean;  overload;
  const n=1;
  var a: array[1..n] of wmsIntType absolute Int1;
  begin
  Result:= GetNextLine(a,n,UseCurrentLine);
  end;

  {$push}{$warn 5024 off}
  function GetNextLine(var Int1,Int2 :wmsIntType;
                       UseCurrentLine:Boolean=False): Boolean;  overload;
  const n=2;
  var a: array[1..n] of wmsIntType absolute Int1;
  begin
  Result:= GetNextLine(a,n,UseCurrentLine);
  end;
  {$pop}

  {$push}{$warn 5024 off}
  function GetNextLine(var Int1,Int2,Int3:wmsIntType;
                           UseCurrentLine:Boolean=False): Boolean;  overload;
  const n=3;
  var a: array[1..n] of wmsIntType absolute Int1;
  begin
  Result:= GetNextLine(a,n,UseCurrentLine);
  end;
  {$pop}

  {$push}{$warn 5024 off}
  function GetNextLine(var Int1,Int2,Int3,Int4:wmsIntType;
                           UseCurrentLine     :Boolean=False): Boolean;  overload;
  const n=4;
  var a: array[1..n] of wmsIntType absolute Int1;
  begin
  Result:= GetNextLine(a,n,UseCurrentLine);
  end;
  {$pop}

  function GetNextLine(var Real1     :wmsRealType;
                       UseCurrentLine:Boolean=False): Boolean;  overload;
  const n=1;
  var a: array[1..n] of wmsRealType absolute Real1;
  begin
  Result:= GetNextLine(a,n,UseCurrentLine);
  end;

  {$push}{$warn 5024 off}
  function GetNextLine(var Real1,Real2:wmsRealType;
                       UseCurrentLine :Boolean=False): Boolean;  overload;
  const n=2;
  var a: array[1..n] of wmsRealType absolute Real1;
  begin
  Result:= GetNextLine(a,n,UseCurrentLine);
  end;
  {$pop}

  {$push}{$warn 5024 off}
  function GetNextLine(var Real1,Real2,Real3:wmsRealType;
                       UseCurrentLine       :Boolean=False): Boolean;  overload;
  const n=3;
  var a: array[1..n] of wmsRealType absolute Real1;
  begin
  Result:= GetNextLine(a,n,UseCurrentLine);
  end;
  {$pop}

begin
Result:= inherited ParseData(CheckFileTypeOnly);
if Result then with wmsFileHeader.wmsRec06 do
  if FileFormat in [twcUnknown,twcWTX] then
    begin
    FParseOk:= FParser.GotoTop(True);
    j       := 0;
    if Copy(LowerCase(FParser.CurrentLine),Length(FParser.CurrentLine)-3,2)='.w' then    {xxx.wda}
       FParseOk:= FParser.NextLine;
    GetNextLine(wmhDate  ,True);                                                //"16-SEP-06"
    GetNextLine(wmhDevice);                                                     //"U3"
    Stg:= Char2Stg(wmhDevice,SizeOf(wmhDevice));
    i  := Succ(Stg.IndexOfAny(CharsetToString(csNotAlphaNumeric)));             //U02: Elekta SL25-15 (i:= 4), indexofany is zero-based
    if i>0 then
      begin
      Delete(Stg,i,Length(Stg)-i+1);
      Stg2Char(Stg,wmhDevice);
      end;
    Linac:= Stg;
    GetNextLine(wmhTime       );                                      {"12:00:00"}
    GetNextLine(Stg           );                   {bakoriëntatie (standaard) XYZ}
    xyz:= ['X','Y','Z'];
    if FParseOk then
      for m:= Inplane to Beam do
        begin
        c           := Stg[Succ(Ord(m))];
        wmhAxisID[m]:= c;
        FParseOk     := FParseOk and (c in xyz);
        xyz         := xyz-[c];
        end;
    if FParseOk then
      FileFormat:= twcWTX;
    if FParseOk and not CheckFileTypeOnly then
      begin
      SetLinacFromHeader;
      GetNextLine(wmhAxisSign);                                                 {1 1 1}
      GetNextLine(wmhFmax ,wmhRmax ,wmhFofs,wmhRofs);                 {10049 10000 0 0}
      GetNextLine(wmhFsign,wmhRsign);                                             {1 1}
      GetNextLine(Stg);                                                            {DD}
      FParseOk:= Length(Stg)=2;
      if FParseOk then begin  wmhKs:= Stg[1];  wmhOrKs:= Stg[2];   end;
      if Pos(wmhKs,'GADLU')=0 then
        begin
        FParseOk:= False;
        StatusMessage('Unsupported scan type: '+GetScanTypeString);
        end;
      GetNextLine(wmhBorders[1],3);                                         {0 0 0 ...}
      GetNextLine(wmhBorders[2],3,True);                                 {... 0 0 3000}
      GetNextLine(wmhScan,2);                                                  {0 3000}
      GetNextLine(wmhMCm       ,wmhCmSec);                          {0.100000 1.000000}
      GetNextLine(wmhIsodose   ,wmhMtime);                        {100.000000 0.010000}
      GetNextLine(wmhNdose     ,wmhNdosediv);                     {0.010000 100.000000}
      GetNextLine(wmhDRdiv     ,wmhNsamp);                                        {1 1}
      GetNextLine(Stg);                                                            {XO}
      FParseOk:= Length(Stg)=2;
      if FParseOk then begin  wmhRaType:= Stg[1];  wmhFdType:= Stg[2];  end;
      GetNextLine(wmhASD       ,wmhEnergy);                        {0.000000 10.000000}
      GetNextLine(wmhFdAB_cm   ,wmhFdGT_cm);                      {30.000000 30.000000}
      GetNextLine(wmhBRect1_cm ,wmhBRect2_cm,wmhBDiam_cm); {0.000000 0.000000 0.000000}
      GetNextLine(wmhWedgeAngle);                                               {0 ...}
      GetNextLine(wmhTrayTr    ,wmhCollim, True);              {... 1.000000 90.000000}
      GetNextLine(wmhGantry_cm    ,wmhSSD_cm);                    {0.000000 100.000000}
      for n:= wmhG1 to wmhU4 do GetNextLine(wmhComs[n],False,True);        {"comments"}
      GetNextLine(wmhDosCon);                                                {0.000000}
      GetNextLine(Stg);                                                            {RD}
      FParseOk:= Length(Stg)=2;
      if FParseOk then wmhBShape:=Stg[1];
      GetNextLine(wmhAIdent);                                                       {0}
      GetNextLine(wmhWIdent);                                                       {0}
      GetNextLine(Stg);                                                             {`}
      FParseOk:= Stg=#145;
      GetNextLine(j);                                                             {401}
      SetNumPoints(j);
      i:= 0;
      while FParseOk and (i<j) do
        begin
        GetNextLine(p.wmpPointArray,5);
        PutProfile(i,p);
        Inc(i);
        end;
      if not FParseOk then
        SetNumPoints(i);
      FParseOk:= (i>twcDefMinProfilePoints);
      end;
    end; {twtwx}
Result:= CheckFileTypeOnly or (Result and ReadResults);
end; {~parsedata}
{$pop}


{26/09/2016 true binstream implementation}
{11/06/2020 added SetLinacFromHeader}
{02/10/2020 set BinStreamType}
function TWmsData.ReadBinData: Boolean;
var m,n: Integer;
begin
Result:= BinStream.Size>0;
if Result then
  begin
  BinStream.Position:= 0;
  n:= BinStream.Read(wmsFileHeader,wmsGenHeadBlockSize);
  with wmsFileHeader do
    Result:= (n=wmsGenHeadBlockSize) and (wmsRecSize06=wmsGenHeadSize) and (wmsRec06.wmhIaaa06=6);
  if Result then with wmsFileHeader,wmsRec06 do
    begin
    BinStreamType:= twcWDA;
    if Pos(wmhKs,'GADL')=0 then
      begin
      Result:= False;
      StatusMessage(twForUnsupported+GetScanTypeString);
      end;
    if Result then
      begin
      n:= BinStream.Read(wmsProfileHeader,wmsProHeadBlockSize);
      with wmsProfileHeader do
        begin
        Result:= (n=wmsProHeadBlockSize) and (wmsRec07.wmpIaaa07=wmsProfileType) and
                 ((wmsRecSize07-wmsProHeadSize) div wmsProPointSize = wmsRec07.wmpNpoints);
        if Result then
          begin
          SetLinacFromHeader;
          SetNumPoints(wmsRec07.wmpNpoints);
          m:= (wmsRecSize07-wmsProHeadSize);
          n:= BinStream.Read(wmsProfile[0],m);
          Result:= (n=m);
          if not Result then
            StatusMessage(twForBinRead2);
          end
        else  StatusMessage(twForBinRead1,True,2);
        end; {with}
      end; {result}
    end; {with wmsfileheader}
  end; {result}
end; {~readbindata}


{30/03/2016 fix:transfer AFileName to FFileName for wda}
{14/08/2016 new implementation based on BinStream}
{16/11/2020 ADataTopLine}
function TWmsData.ReadData(AFileName   :String;
                           ADataTopLine:Integer    =0;
                           AFileFormat :twcFileType=twcUnknown): Boolean;
begin
if AFileFormat=twcUnknown then
  AFileFormat:= GetFileType(AFileName);
Result:= (AFileFormat=twcWTX) and
         Inherited ReadData(AFileName,ADataTopLine,AFileFormat);
if (not Result) and (AFileFormat=twcWDA) then
  begin
  if BinStream.Size=0 then
    LoadBinStream(AFilename);
  Result:= ReadBinData;
  end;
end; {~readdata}


function  TWmsData.WriteData(AFileName  :String;
                             AStringList:TStrings;
                             ASource    :twcDataSource=dsMeasured;
                             ClearList  :Boolean     =True     ): Boolean;
var Stg : string;
    i   : Integer;
    Axis: twcMeasAxis;
    Ncom: wmsComments;

  procedure Write_WMS_charStg(var WMS_Char_Array: array of Char;
                              ArrayLength       :Byte);
  begin
  AStringList.Append('"'+Char2Stg(WMS_Char_Array,ArrayLength)+'"');
  end; {write_wms_charstg}

  procedure WriteIntegers(a:array of wmsIntType);    overload;
  var i: integer;
  begin
  Stg:= '';
  for i:= 0 to Pred(Length(a)) do
    Stg:= Format('%s%9d',[Stg,a[i]]);
  AStringList.Append(Stg);
  end;

  procedure WriteIntegers(t:twcTankAxisSign);    overload;
  var i: twcTankAxisChar;
  begin
  Stg:= '';
  for i:= 'X' to 'Z' do
    Stg:= Format('%s%9d',[Stg,t.c[i]]);
  AStringList.Append(Stg);
  end;

  procedure WriteReals(a:array of wmsRealType);    overload;
  var i: integer;
  begin
  Stg:= '';
  for i:= 0 to Pred(Length(a)) do
    Stg:= Format('%s%9.3f',[Stg,a[i]]);
  AStringList.Append(Stg);
  end;

begin
inherited WriteData(AFileName,AStringList,ASource,ClearList);
with AStringList,wmsFileHeader.wmsRec06 do
  begin
  Clear;
  Append(Copy(ChangeFileExt(FileName,'.wda'),
         FileName.LastIndexof(PathSeparator)+2,                                 //zero-based
         Length(FileName)));
  Write_WMS_charStg(wmhDate  ,SizeOf(wmhDate  ));
  Write_WMS_charStg(wmhDevice,SizeOf(wmhDevice));
  Write_WMS_charStg(wmhTime  ,SizeOf(wmhTime  ));
  Append(wmhAxisID);
  WriteIntegers(wmhAxisSign);
  WriteIntegers([wmhFmax,wmhRmax,wmhFofs,wmhRofs]);
  WriteIntegers([wmhFsign,wmhRsign              ]);
  Append(wmhKs+wmhOrKs                           );
  Stg:= '';
  for i:= 1 to 2 do
    for Axis:= Inplane to Beam do
      Stg:= Format('%s%9d',[Stg,wmhBorders[i,Axis]]);
  Append(Stg);
  WriteIntegers( wmhScan                               );
  WriteReals(   [wmhMCm,wmhCmSec                      ]);
  WriteReals(   [wmhIsodose,wmhMTime                  ]);
  WriteReals(   [wmhNdose,wmhNdosediv                 ]);
  WriteIntegers([wmhDRdiv,wmhNsamp                    ]);
  Append(wmhRaType+wmhFdType                           );
  WriteReals(   [wmhASD,wmhEnergy                     ]);
  WriteReals(   [wmhFdAB_cm,wmhFdGT_cm                ]);
  WriteReals(   [wmhBrect1_cm,wmhBRect2_cm,wmhBDiam_cm]);
  Append(Format('%9d%9.3f%9.3f',[wmhWedgeAngle,wmhTrayTr,wmhCollim]));
  WriteReals(   [wmhGantry_cm,wmhSSD_cm               ]);
  for Ncom:= wmhG1 to wmhU4 do
    Write_WMS_charStg(wmhComs[Ncom],SizeOf(wmhComs[Ncom]));
  WriteReals(   [wmhDosCon                            ]);
  Append(wmhBShape+wmhBShape);
  WriteIntegers([wmhAIdent                            ]);
  WriteIntegers([wmhWIdent                            ]);
  Append(#145                                          );                       //end of header
  WriteIntegers([GetNumPoints                         ]);                       //start profile
  for i:= 0 to Pred(GetNumPoints) do
    WriteIntegers(wmsProfile[i].wmpPointArray);
  Append(#32);                                                                  //extra empty line mandatory!
  Append('***  End of file  ***');
  if Length(FExtraText)>0 then
    for i:= 0 to Pred(Length(FExtraText)) do
      Append('# '+FExtraText[i]);
  end;
Result:= True;
end; {~writedata}


{$push}{$I-}
{15/08/2016 BinStream implementation}
function TWmsData.WriteData(AFileName:String;
                            Binary   :Boolean     =True;
                            ASource  :twcDataSource=dsMeasured;
                            SetExt   :Boolean=True          ): Boolean;
var m,n: Integer;
begin
if Binary then
  begin
  inherited WriteData(ChangeFileExt(AFileName,'.wda'),Binary,ASource,False);
  BinStream.Clear;
  with wmsFileHeader,wmsRec06 do
    begin
    wmsRecSize06:= wmsGenHeadSize;
    wmhIaaa06   :=   6;
    wmhIzzz06   :=   6;
    end;
  n     := BinStream.Write(wmsFileHeader,wmsGenHeadBlockSize);
  Result:= (n=wmsGenHeadBlockSize);
  if Result then
    begin
    wmsProfileHeader.wmsRec07.wmpIaaa07:= wmsProfileType;
    n     := BinStream.Write(wmsProfileHeader,wmsProHeadBlockSize);
    Result:= (n=wmsProHeadBlockSize);
    if Result then with wmsProfileHeader do
      begin
      m:= wmsRecSize07-wmsProHeadSize;
      n     := BinStream.Write(wmsProfile[0],m);
      Result:= (n=m);
      try
        BinStream.SaveToFile(AFileName);
       except
        Result:= False;
       end;
      end;
    end;
  end {binary}
else Result:=
  Inherited WriteData(AFileName,False,ASource,SetExt);
end; {~writedata}
{$pop}


function TWmsData.WriteData(AFileName :String;
                            OutPutType:twcFileType;
                            ASource   :twcDataSource=dsMeasured;
                            SetExt    :Boolean=True          ): Boolean;
begin
if OutPutType in [twcWTX,twcWDA] then Result:= WriteData(AFileName,OutPutType=twcWDA,ASource,SetExt)
else                                  Result:= False;
end; {~writedata}


destructor TWmsData.Destroy;
begin
Finalize(wmsProfile);
Inherited;
end; {~destroy}


//----------TWellhoferData------------------------------------------------------

{01/08/2015 wTakeCurrentRefSource introduced and TempRefFile removed for in-memory implementation}
{21/09/2015 FUserLevel and FTempLevel set to twcD50;}
{03/12/2015 wInflectionSigmoid}
{09/12/2015 added sharedparser}
{11/12/2015 added wFFFMinDoseDifPerc, wFFFMinEdgeDifCm}
{15/12/2015 added FMRefOrgSrc}
{04/01/2016 split wLinacSymSign}
{06/07/2016 added wFFFdetection and wFFFcentering}
{22/07/2016 wCenterDefinition}
{03/08/2016 wFMultiRefFreeList}
{27/09/2016 added FBinaryAllowed}
{09/11/2016 wReferenceFromGeneric, FModBeamList added}
{11/01/2017 wEdgeFallBackCm}
{29/03/2017 wAutoShiftCm}
{21/06/2017 wRefAlignPeak}
{23/07/2017 FActiveCnt}
{04/12/2017 wInflectionSigmoid[fieldclass]}
{05/12/2017 wEdgeForce}
{03/06/2018 FIndexingMode}
{11/06/2018 wRenormaliseData}
{10/02/2020 FRefOrg2D_OriVal}
{28/04/2020 initcurve}
{01/05/2020 transfer of binarydata and filenames}
{11/05/2020 use AModalityNormList and AModalityFilmList}
{11/06/2020 wEdgePrimaryMethod,wEdgeFallBackMethod}
{19/07/2020 wSmallFielddetection,wSmallFieldLimitCm}
{20/07/2020 twcFieldClass}
{21/07/2020 wSmallFieldFilterDivider, fcWedge}
{24/07/2020 wWedge90ShiftFactor}
{27/08/2020 wTopModelRadiusCm}
{13/10/2020 defaults for all fieldtypes set}
{22/10/2020 initialise wUserAxisSign to 1, not 0}
{15/02/2021 initialise FAliasList}
constructor TWellhoferData.Create(AModalityNormList:TModNormList =nil;
                                  AModalityFilmList:TModFilmList =nil;
                                  AModalityBeamList:TModTextList =nil;
                                  SharedParser     :toTNumParser =nil;
                                  ParsedFile       :String       ='';
                                  BinaryData       :TMemoryStream=nil;
                                  BinaryFile       :String       ='';
                                  AStatusProc      :toExtMsgProc=nil;
                                  AIdentity        :String       ='bistromath');
var i: twcDataSource;
    f: twcFieldClass;
begin
inherited Create(SharedParser,ParsedFile,BinaryData,BinaryFile,AStatusProc,AIdentity);
FModNormLocal                     := (AModalityNormList=nil);
FModFilmLocal                     := (AModalityFilmList=nil);
FModBeamLocal                     := (AModalityBeamList=nil);
if FModNormLocal then FModNormList:= TModNormList.Create
else                  FModNormList:= AModalityNormList;
if FModFilmLocal then FModFilmList:= TModFilmList.Create
else                  FModFilmList:= AModalityFilmList;
if FModBeamLocal then FModBeamList:= TModTextList.Create
else                  FModBeamList:= AModalityBeamList;
FBinaryAllowed                    := True;
FAlignRef                         := True;
FAutoLoadRef                      := False;
FNoPenumbraOk                     := False;
FZeroStepsOk                      := False;
FNMPddSource                      := dsMeasured;
FTimeRepSource                    := dsMeasured;
FNMPddScaling                     :=  1;
FActiveCnt                        :=  0;
FNMreset                          := True;
FReferenceDir                     := AppendPathDelim(AppendPathDelim(CommonAppData)+twcRefDirDefaultName);
w2D_ArrayRefList                  := TStringList.Create;
FArrayScanRefUse                  := False;
FArrayScanRefOk                   := False;
FRefOrgSrc                        := TStringStream.Create('');
FRefOrgFileName                   := '';
FRefOrg2D_OriVal                  := 0;
wScale2DefaultSSD                 := False;
wEdgeFallBackCm                   := 0.2;
wGenericToPDD                     := False;
wMeas2TankMapping                 := twcMeasAxisStandard;
wMultiScanNr                      :=  1;
wMultiScanStep                    :=  1;
wMultiScanMax                     :=  0;
wMultiScanLooping                 := True;
FMultiRefIndex                    := True;
wNominalIFA                       := False;
FIndexingMode                     := False;
wLinacSymSign[fInplane   ]        :=  1;
wLinacSymSign[fCrossplane]        :=  1;
wLinacSymInnerRadiusCm            :=  7.3;
wLinacSymOuterRadiusCm            := 15.3;
FPenumbraH                        := 80;
FPenumbraL                        := 20;
FUserLevel                        := twcD50;
FTempLevel                        := 0;
FLastMultiFile                    := '';
wEdgeDetect                       := True;
wApplyUserLevel                   := False;
wApplySigmoidToBuffer             := False;
wInflectionSigmoidRadiusCm        := twcDefSigmoidRadiusCm;
wPipsPixelCm                      := twcDefPipsPixelCm;
wSamePositionRadiusCm             := twcSamePositionRadiusCm;
wCheckRefCurveString              := True;
wTakeCurrentRefSource             := False;
wScaleSDD2SSD                     := False;
wCenterProfiles                   := False;
wRenormaliseData                  := False;
wResampleData                     := False;
wReferenceFromGeneric             := False;
wRefAtDefaultSSD                  := False;
wTryBinaryOnly                    := False;
wFullAnalysis                     := True;
wOutlierFilter                    := True;
wRefAlignPeak                     := False;
wFFFMinDoseDifPerc                := 10;
wFFFMinEdgeDifCm                  :=  0.05;
wSmallFieldLimitCm                :=  2;
wSmallFieldFilterDivider          :=  4;
wWedge90ShiftFactor               :=  1;
wMRlinacTUlist                    := 'comma separated';
Fmcc                              := nil;
FMultiScanList                    := nil;
FAliasList                        := nil;
FPDDfit_simplex                   := nil;
FillChar(wUserAxisSign,SizeOf(wUserAxisSign),1);
for f:= Low(twcFieldClass) to High(twcFieldClass) do
  begin
  wCenterDefinition[f]            := CenterPenumbra;
  wTopModelRadiusCm[f]            := 2.5;
  wEdgeMethod[fcPrimary ,f]       := d50;
  wEdgeMethod[fcFallBack,f]       := dInflection;
  end;
wCenterDefinition[fcFFF         ] := CenterOrigin;
wEdgeMethod[fcPrimary ,fcFFF    ] := dInflection;
wEdgeMethod[fcPrimary ,fcMRlinac] := dInflection;
wEdgeMethod[fcPrimary ,fcWedge  ] := dInflection;
FillChar(wFieldTypeDetection,SizeOf(wFieldTypeDetection),1);
FillChar(wDiagonalDetection ,SizeOf(wDiagonalDetection ),1);
FillChar(wAutoShiftCm       ,SizeOf(wAutoShiftCm       ),0);
LoadAliasList(nil);
ResetMultiScanCounters;
ResetAliasList;
for i:= twcFirstDataSource to twcLastDataSource do
  begin
  InitCurve(wSource[i]);
  ClearCurve(i,False);                                                          //unset Freeze
  end;
end; {~create}


{$push}{$warn 5092 off}
{29/03/2016}
{30/03/2016 AddTypes to avoid double entries}
{27/11/2020 dot should be included in string}
function TWellhoferData.GetRegisteredFileTypes: String;
var T: TRadthData;

  procedure AddTypes(AString:String);
  var i: Integer;
      s: array of String;
  begin
  s:= AString.Split(['.'],TStringSplitOptions.ExcludeEmpty);
  i:= Length(s);
  while i>0 do
    begin
    Dec(i);
    if Pos(s[i],FRegisteredFiles)=0 then
      FRegisteredFiles:= FRegisteredFiles+'.'+s[i];
    end;
  end;

  procedure FreeT(AMess:String);
  begin
  try
   FreeAndNil(T);
   except
    ExceptMessage('GetRegisteredFileTypes:'+AMess+'!');
   end;
  end;

begin
if Length(FRegisteredFiles)>0 then
  Result:= FRegisteredFiles
else
  begin
  FRegisteredFiles:= '.txt.rfb.snctxt';
  T:= TRfaProfileData.Create;
  AddTypes(T.FRegisteredFiles);
  FreeT('TRfaProfileData');

  T:= TICprofiler_ascii.Create;
  AddTypes(T.FRegisteredFiles);
  FreeT('TICprofiler_ascii');

  T:= Tw2CAD_data.Create;
  AddTypes(T.FRegisteredFiles);
  FreeT('Tw2CAD_data');

  T:= TMccProfileData.Create;
  AddTypes(T.FRegisteredFiles);
  FreeT('TMccProfileData');

  T:= THdfProfileData.Create;
  AddTypes(T.FRegisteredFiles);
  FreeT('THdfProfileData');

  T:= TCmsProfileData.Create;
  AddTypes(T.FRegisteredFiles);
  FreeT('TCmsProfileData');

  T:= TSchusterProfileData.Create;
  AddTypes(T.FRegisteredFiles);
  FreeT('TSchusterProfileData');

  T:= TPipsProfileData.Create(0);
  AddTypes(T.FRegisteredFiles);
  FreeT('TPipsProfileData');

  T:= TWmsData.Create;
  AddTypes(T.FRegisteredFiles);
  FreeT('TWmsData');
  Result:= FRegisteredFiles;
  end;
end; {~getregisteredfiletypes}
{$pop}


{$push}{$warn 5092 off}
{29/03/2016}
{06/10/2020 fundamentals alternative}
{27/11/2020 avoid taking last part by spliiting on pathseparator; just take file extension}
function TWellhoferData.IsRegisteredFileType(AFileName:String): Boolean;
begin
Result:= (Pos(ExtractFileExt(AFilename),RegisteredFileTypes)>0);                //modified 27/11/2020 acc
end; {~isregisteredfiletype}
{$pop}


procedure TWellhoferData.SetDefaults;
var k: twcDataSource;

  procedure SetChannel(var Channel:twChannelRec);
  begin
  with Channel do
    begin
    twNorm       :=   1;
    twDarkCurrent:=   0;
    twHV         := 300;
    twGain       :=  25;
    twRange      := 'HIGH';
    end;
  end;

begin
Inherited;
with wGeneralInfo,wCurveInfo,
     wMeterInfo,wDetectorInfo,wMeterInfo,wMeasDeviceInfo do
  begin
  twClinic               :=   '';
  twAddress              :=   '';
  twTelephone            :=   '';
  twEmail                :=   '';
  twDesTypeString        := 'Undefined';
  twDesScanType          := snUndefined;
  twDesModTime           :=   '';
  twDesOperator          :=   '';
  twDesMeasComment       :=   '';
  twDesSetupComment      :=   '';
  twDesNormalise         :=    1;
  twDesShift             :=    0;
  twDetQuantity          := 'Relative Dose';
  twDetType              := 'Ion chamber (cylindrical)';
  twDetName              := twcDefUnknown;
  twDetPeffOffset_cm     :=   -0.18;
  twDetRadius_cm         :=    0.68;
  twElMeasMode           := 'continuous';
  twElMeterType          := 'CU500E';
  twElSamples            :=    1;
  twElSampleMs           :=    0;
  twElRefMin             := 1000;
  twElRefMax             := 1000;
  twElRefAvg             := 1000;
  twElHVType             := twcDefUnknown;
  twDeviceName           := twcDefUnknown;
  twDeviceSpeed_mm_s     :=    0;
  twDeviceWaterOffset_cm :=    0;
  twDeviceWaterSurface_cm:=    0;
  twDeviceOriginXYZ_cm   := Default(twcCoordinate);
  twDeviceIsocICD_cm     := Default(twcCoordinate);
  twDeviceNormICD_cm     := Default(twcCoordinate);
  FillChar(twDeviceRefPosition_cm,SizeOf(twDeviceRefPosition_cm),0);
  FillChar(twDesVaryingAxis      ,SizeOf(twDesVaryingAxis      ),0);
  SetChannel(twElChannels[FieldCh]);
  SetChannel(twElChannels[RefCh]);
  SetAxisID(twcDefaultICDstring,twDeviceMappingICD,twDeviceDirXYZ);
  for k:= dsMeasured to twcLastRelated do
    wSource[k].twValid:= False;
  end;
FDefaultSSDcm:= twcDefaultSSDcm;
FCentered    := False;
FParseOk     := False;
Freeze       := False;
end; {~setdefaults}


{21/12/2015 update this list when addeing new variables}
{18/03/2016 addded wCheckRefIgnoreLinac}
{12/05/2016 wAxisPreserveOnExport}
{06/07/2016 added wFFFdetection and wFFFcentering}
{22/07/2016 wCenterDefinition}
{25/07/2016 removed wTryBinaryOnly}
{03/08/2016 wFMultiRefFreeList}
{27/09/2016 exclude wMultiScanNr and wMultiScanMax, included FStatusProc}
{29/09/2016 include wMultiScanNr again}
{28/03/2017 added wFFFPeakType}
{21/06/2017 wRefAlignPeak}
{11/06/2018 wRenormaliseData}
{10/07/2020 wEdgePrimaryMethod,wEdgeFallBackMethod}
{19/07/2020 wSmallFielddetection,wSmallFieldLimitCm}
{21/07/2020 wSmallFieldFilterDivider}
{24/07/2020 wWedge90ShiftFactor}
{18/08/2020 wMRlinacTUlist}
{20/08/2020 AObjectCallSign}
{27/08/2020 wTopModelRadiusCm}
{29/09/2020 option PassRefOrg added}
{30/09/2020 use PassRefOrgData}
{13/10/2020 AutoDecimalPoint,AutoDecimalList}
{03/03/2021 wDiagonalDetection added, DetectDiagonalScans removed}
{07/03/2021 wNominalIFA}
(* wAutoShiftCm is not passed to load unshifted references *)
procedure TWellhoferData.PassSettings(var ADestination:TWellhoferData;
                                      AObjectCallSign :String        ='';
                                      PassRefOrg      :Boolean       =False);
begin
if assigned(ADestination) then
  begin
  Inc(FActiveCnt); {increase business}
  ADestination.wApplyUserLevel           := wApplyUserLevel;
  ADestination.wApplySigmoidToBuffer     := wApplySigmoidToBuffer;
  ADestination.AutoDecimalPoint          := AutoDecimalPoint;
  ADestination.AutoDecimalList           := AutoDecimalList;
  ADestination.wUserAxisSign             := wUserAxisSign;
  ADestination.wAxisPreserveOnExport     := wAxisPreserveOnExport;
  ADestination.wCenterProfiles           := wCenterProfiles;
  ADestination.wDiagonalDetection        := wDiagonalDetection;
  ADestination.wEdgeFallBackCm           := wEdgeFallBackCm;
  ADestination.wGenericToPDD             := wGenericToPDD;
  ADestination.wCenterDefinition         := wCenterDefinition;
  ADestination.wCheckRefCurveString      := wCheckRefCurveString;
  ADestination.wCheckRefIgnoreLinac      := wCheckRefIgnoreLinac;
  ADestination.wTakeCurrentRefSource     := wTakeCurrentRefSource and PassRefOrg;
  ADestination.wEdgeDetect               := wEdgeDetect;
  ADestination.wEdgeMethod               := wEdgeMethod;
  ADestination.wInflectionSigmoidRadiusCm:= wInflectionSigmoidRadiusCm;
  ADestination.wEPenumbraH               := wEPenumbraH;
  ADestination.wEPenumbraL               := wEPenumbraL;
  ADestination.wFullAnalysis             := wFullAnalysis;
  ADestination.wFFFPeakDef               := wFFFPeakDef;
  ADestination.wMultiScanLooping         := wMultiScanLooping;
  ADestination.wFieldTypeDetection       := wFieldTypeDetection;
  ADestination.wSmallFieldLimitCm        := wSmallFieldLimitCm;
  ADestination.wSmallFieldFilterDivider  := wSmallFieldFilterDivider;
  ADestination.wWedge90ShiftFactor       := wWedge90ShiftFactor;
  ADestination.wFFFMinDoseDifPerc        := wFFFMinDoseDifPerc;
  ADestination.wFFFMinEdgeDifCm          := wFFFMinEdgeDifCm;
  ADestination.wLinacSymSign             := wLinacSymSign;
  ADestination.wLinacSymInnerRadiusCm    := wLinacSymInnerRadiusCm;
  ADestination.wLinacSymOuterRadiusCm    := wLinacSymOuterRadiusCm;
  ADestination.wMeas2TankMapping         := wMeas2TankMapping;
  ADestination.FMultiRefIndex            := FMultiRefIndex;
  ADestination.wNominalIFA               := wNominalIFA;
  ADestination.wMRlinacTUlist            := wMRlinacTUlist;
  ADestination.w2D_ArrayRefList.Text     := w2D_ArrayRefList.Text;
  ADestination.wPipsPixelCm              := wPipsPixelCm;
  ADestination.wOutlierFilter            := wOutlierFilter;
  ADestination.wRefPoint                 := wRefPoint;
  ADestination.wRefAlignPeak             := wRefAlignPeak;
  ADestination.wRefAtDefaultSSD          := wRefAtDefaultSSD;
  ADestination.wResampleData             := wResampleData;
  ADestination.wRenormaliseData          := wRenormaliseData;
  ADestination.wSamePositionRadiusCm     := wSamePositionRadiusCm;
  ADestination.wScaleSDD2SSD             := wScaleSDD2SSD;
  ADestination.wScale2DefaultSSD         := wScale2DefaultSSD;
  ADestination.wTopModelRadiusCm         := wTopModelRadiusCm;
  ADestination.wXPenumbraH               := wXPenumbraH;
  ADestination.wXPenumbraL               := wXPenumbraL;
  ADestination.wNormalisation            := wNormalisation;
  ADestination.AcceptMissingPenumbra     := AcceptMissingPenumbra;
  ADestination.AcceptZeroSteps           := AcceptZeroSteps;
  ADestination.AutoLoadReference         := AutoLoadReference;
  ADestination.AlignReference            := AlignReference;
  ADestination.CalcWidth_cm              := CalcWidth_cm;
  ADestination.FFilterWidth              := FFilterWidth;
  ADestination.FArrayScanRefUse          := FArrayScanRefUse;
  ADestination.MatchOverride             := MatchOverride;
  ADestination.ReferenceDirectory        := ReferenceDirectory;
  ADestination.ResampleGridSize          := ResampleGridSize;
  ADestination.UserBorderDoseLevel       := UserBorderDoseLevel;
  ADestination.FStatusProc               := FStatusProc;
  ADestination.FRegisteredFiles          := FRegisteredFiles;
  if Length(AObjectCallSign)=0 then
    ADestination.ObjectCallSign          := ObjectCallSign
  else
    ADestination.ObjectCallSign          := AObjectCallSign;
  if PassRefOrg then
    ADestination.SetReferenceOrg(dsMeasured,True,Self);
  Dec(FActiveCnt);
  end;
end; {~passsettings}


{03/06/2018}
{17/06/2020 dSigmoid50 as new last element}
procedure TWellhoferData.InitBorders(ASource    :twcDataSource=dsMeasured;
                                     InitFitData:Boolean      =True     );
var d,l: twcDoseLevel;
    s  : twcSides;
begin
with wSource[ASource] do
  begin
  if InitFitData then l:= dSigmoid50
  else                l:= dDerivative;
  for d:= dLow to l do with twlevelPos[d] do
    begin
    Level:= 0;
    for s:= twcleft to twcRight do
      Penumbra[s].Valid:= False;
    end;
  if InitFitData then
    for s:= twcleft to twcRight do
      twSigmoidFitData[s].twFitvalid:= False;
  end;
end; {~initborders}


{28/04/2020 Default}
procedure TWellhoferData.InitCurve(var ACurveRec:twCurveDataRec);
begin
ACurveRec:= Default(twCurveDataRec);
CheckSize(ACurveRec)
end; {~initcurve}


{20/07/2015 twComposite added}
{27/11/2015 twEdgeFitData added}
{20/12/2015 twTopModel}
{01/08/2016 twFileIDString}
{23/11/2017 added twFlatPosCm,twSymAreaRatio}
{12/01/2018 added twAbsNormConfig to note used info from modlist}
{27/01/2018 twAbsNormDefUse}
{27/04/2020 initialise option removed}
{22/05/2020 twSigmoidDone}
procedure TWellhoferData.ClearCurve(var ACurveRec:twCurveDataRec;
                                    CleanUp      :Boolean=False);
var r: twcNMpddFits;
    s: twcSides;
begin
with ACurveRec,twBeamInfo do
  begin
  Freeze           := False;
  twBEnergy        := twcDefaultEnergy_MeV;
  if twcGenericToElectron then twBModality:= 'E'
  else                         twBModality:= 'X';
  twBWedge         := 0;
  twBGantry        := 0;
  twBGantryScale   := twCW_180_Down;
  twBCollimator    := 0;
  twBSAD_cm        := FDefaultSSDcm;
  twBApplicator    := '';
  twBASD           := 0;
  twBFieldLo       := Fill_FieldDescrArr(UndefinedVal/2);
  twBFieldHi       := twBFieldLo;
  twBMedium        := 'water';
  twBTrayTransm    := 0;
  twAbsNormPosCm   := 0;
  twAbsNormConfig  := False;
  twAbsNormValue   := 0;
  twAppliedNormVal := 0;
  twAnalysed       := False;
  twAvgNormValue   := 0;
  twBackGround     := 0;
  twAbsNormDefUse  := dUseUndefined;
  twCenterPosDefUse:= dUseUndefined;
  twCenterPosCm    := 0;
  twCenterPosValid := False;
  twCenterArr      := 0;
  twComposite      := False;
  twCurveIDString  := '';
  twDataFirst      := 0;
  twDataLast       := 0;
  twDataHistoryStg := '';
  twDevice         := '';
  twFastScan       := False;
  twFFFdetected    := False;
  twFileIDString   := '';
  twFileName       := '';
  twFilmData       := False;
  twFilterPoints   := 0;
  twFilterString   := '';
  twFittedData     := False;
  twInFieldAreaOk  := False;
  twFlatness       := 0;
  twIsDerivative   := False;
  twIsDiagonal     := False;
  twIsFiltered     := False;
  twIsGamma        := False;
  twSSD_cm         := twcDefaultSSDcm;
  FillChar(twFFFslope    ,SizeOf(twFFFslope    ),0);
  FillChar(twInFieldArr  ,SizeOf(twInFieldArr  ),0);
  FillChar(twInFieldPosCm,SizeOf(twInFieldPosCm),0);
  twTopModel       :=  Default(TQuadFitReport);
  Finalize(twExtraText);
  twLocalPeak      := False;
  twAlignedTo      := twSelf;
  twMayneordApplied:= False;
  twSigmoidDone    := False;
  twMeasTime       := '';
  twLinSlope       :=   0;
  twMaxArr         :=   0;
  twMaxPosCm       :=   0;
  twMaxValue       :=   0;
  twRelMaxInField  :=   0;
  twMinArr         :=   0;
  twRelMinInField  :=   0;
  twFirstDataPosCm :=   0;
  twLastDataPosCm  :=   0;
  twOriginPosValid := False;
  twPDD10          :=   0;
  twPDD20          :=   0;
  twPlotScaling    :=   0;
  twPoints         :=   0;
 {$IFDEF POSINTEGRAL}
  twPosIntegral    :=   0;
 {$ENDIF}
  twRefNormFactor  :=   0;
  twRelNormPosCm   :=   0;
  twRelNormValue   :=   0;
  twScanDevice     := '';
  twScanFirst      :=   0;
  twScanLast       :=   0;
  twScanNr         :=   0;
  twScanLength     :=   0;
  twShiftCm        :=   0;
  twSNR            :=   0;
  twStepSizeCm     :=   0;
  twStepSign       :=   0;
  twSymmetry       :=   0;
  twSymAreaRatio   :=   0;
  twSymLinacError  :=   0;
  twTag            :=   0;
  twWidthCm        :=   0;
  twLocked         := False;
  twMirrored       := False;
  tw2DoseConv      := False;
  twOD2doseFilm    := '';
  twOriginalFormat := twcWellhoferAscii_v6;
  twIsRelative     := False;
  twDerivativeValid:= False;
  twResampled      := False;
  twSelf           := dsMeasured;
  twRelatedSource  := twSelf;
  twSymCorrected   := False;
  twUsedEdgeLevel  := d50;
  twValid          := False;
  twOriginalFormat := FileFormat;
  for r:= NM_Primary to NM_Extrapolation do with twPddFitData[r] do
    begin
    twFitValid:= False;
    if CleanUp then
      Finalize(twNMReport.BestVertex);
    end;
  for s:= twcLeft to twcRight do with twSigmoidFitData[s] do
    begin
    twFitValid:= False;
    if CleanUp then
      Finalize(twNMReport.BestVertex);
    end;
  Finalize(twCoordinates);
  Finalize(twPosCm);
  Finalize(twData);
  end;
end; {~clearcurve}


{03/06/2018 initborders}
{27/04/2020 initialise option removed}
procedure TWellhoferData.ClearCurve(ASource   :twcDataSource;
                                    CleanUp   :Boolean=False);
begin
if ASource=dsMeasured then
  ResetAliasList;
ClearCurve(wSource[ASource],CleanUp);
wSource[ASource].twSelf          := ASource;
wSource[ASource].twDataHistoryStg:= twcDataSourceNames[ASource];
InitBorders(ASource);
end; {~clearcurve}


procedure TWellhoferData.ConfigLoad(Sender: TObject);
var CF: TConfigStrings;
begin
CF:= nil;
ReadConfig(CF);
end; {~configload}


procedure TWellhoferData.ConfigSave(Sender:TObject);
var CF: TConfigStrings;
begin
CF:= nil;
WriteConfig(CF);
end; {~configsave}


{11/12/2015 added wFFFMinDoseDifPerc, wFFFMinEdgeDifCm}
{04/01/2016 split wLinacSymSign}
{06/01/2016 twcPddFitMubPower and LogLevel added}
{08/01/2016 twcPddFitMubPowerFixed added}
{12/01/2016 twcPddFitCostENRWeighted added}
{24/06/2016 twcSymCorrectionLevel}
{07/11/2016 twcDCKey, splitting modalitydata in FModFilmList and FModNormList}
{09/11/2016 FModBeammList added}
{18/01/2017 twcMatchInclusionLimit}
{03/02/2017 edgesigmoid removed as preset}
{12/07/2017 twcPddFitZWeightPower}
{10/01/2020 do not read DUser}
{16/05/2020 storage of wMultiScanNr removed}
{07/03/2021 twcDefaultSSD_MRcm}
procedure TWellhoferData.ReadConfig(CF:TConfigStrings=nil);
var IsLocal: Boolean;
    S      : TStringList;
    i      : Integer;
    Section: String;

  function ReadFraction(ASection,AValue:String;
                        ADefault       :twcFloatType): twcFloatType;
  var v: twcFloatType;
  begin
  v:= CF.ReadFloat(ASection,AValue,ADefault);
  if v>1 then Result:= v/100
  else        Result:= v;
  end;

  procedure ReadModSection(ASection:String;
                           AModList:TModalityList);
  var i: Integer;
  begin
  CF.ReadSectionValues(ASection,S);
  if S.Count>0 then
    begin
    AModList.ClearModData;
    i:= 0;
    while i<S.Count do
      begin
      AModList.AddModData(S.Strings[i],'|');
      Inc(i);
      end;
    end;
  end;

begin
Inc(FActiveCnt);
IsLocal:= CF=nil;
Section:= twcWellhoferKey;
S      := TStringList.Create;
if IsLocal then
  CF:= TConfigStrings.Create('');
with CF do
  begin
  ReferenceDirectory         := ReadString(Section  ,twcRefDirKey              ,ReferenceDirectory);
  FilterWidth                := ReadFloat(Section   ,twcFilterKey              ,0.6);
  wOutlierFilter             := ReadBool(Section    ,twcFilterKey+'Outlier'    ,True);
  twcDefaultEnergy_MeV       := ReadFloat(Section   ,twcEnergyKey              ,6);
  twcDefaultSSDcm            := ReadFloat(Section   ,twcSSDKey                 ,100);
  twcDefaultSSD_MRcm         := ReadFloat(Section   ,twcSSDKey+'_MR'           ,100);
  w2D_ArrayRefList.CommaText := ReadString(Section  ,twcMultiScanKey+'list'    ,'' );
  twcOutlierPointLimit       := ReadInteger(Section ,twcFilterKey+'Limit'      ,7  );
  ResampleGridSize           := ReadFloat(Section   ,twcGridKey                ,0.0);
  CalcWidth_cm               := ReadFloat(Section   ,twcCalcKey                ,0.2);
  wFFFMinDoseDifPerc         := ReadFloat(Section   ,'FFFMinDoseDif'           ,wFFFMinDoseDifPerc);
  wFFFMinEdgeDifCm           := ReadFloat(Section   ,'FFFMinEdgeDifCm'         ,wFFFMinEdgeDifCm);
  wLinacSymSign[fInplane]    := ifthen(ReadInteger(Section,twcLinacRadiusKey+'SignGT',1)>0,1,-1);
  wLinacSymSign[fCrossplane] := ifthen(ReadInteger(Section,twcLinacRadiusKey+'SignAB',1)>0,1,-1);
  wLinacSymInnerRadiusCm     := ReadFloat(Section   ,twcLinacRadiusKey+'In'    ,wLinacSymInnerRadiusCm);
  wLinacSymOuterRadiusCm     := ReadFloat(Section   ,twcLinacRadiusKey+'Out'   ,wLinacSymOuterRadiusCm);
  twcWMSdetInfo              := ReadInteger(Section ,twcWMSdetKey              ,10);
  wXPenumbraH                := ReadFraction(Section,twcPenumbraKey+'XH'       ,twcD80);
  wXPenumbraL                := ReadFraction(Section,twcPenumbraKey+'XL'       ,twcD20);
  wEPenumbraH                := ReadFraction(Section,twcPenumbraKey+'EH'       ,twcD90);
  wEPenumbraL                := ReadFraction(Section,twcPenumbraKey+'EL'       ,twcD50);
  twcDefaultICDstring        := ReadString(Section  ,'ICD'                     ,twcDefaultICDstring);
  twcOriginMinNormFraction   := ReadFraction(Section,'OriginMinNorm'           ,twcOriginMinNormFraction);
  twcPddFitZWeightPower      := EnsureRange(ReadFloat(Section,'Z_weighting'    ,twcPddFitZWeightPower),0,5);
  twcPddFitMubPower          := EnsureRange(ReadFloat(Section,'Mu_b_Power'     ,twcPddFitMubPower),0,5);
  twcPddFitMubPowerFixed     := ReadBool(Section,'Mu_b_Fixed'                  ,twcPddFitMubPowerFixed);
  twcPddFitCostENRWeighted   := ReadBool(Section,'CostENRweight'               ,twcPddFitCostENRWeighted);
  twcD20                     := ReadFraction(Section,'D20'                     ,twcD20);
  twcD50                     := ReadFraction(Section,'D50'                     ,twcD50);
  twcD80                     := ReadFraction(Section,'D80'                     ,twcD80);
  twcD90                     := ReadFraction(Section,'D90'                     ,twcD90);
  twcNCSInFieldAxis          := ReadFloat(Section  ,'NCSInFieldArea'           ,twcNCSInFieldAxis);
  twcNCSInFieldDiagonal      := ReadFloat(Section  ,'NCSInFieldDiagonal'       ,twcNCSInFieldDiagonal);
  twcDeriveMinMax            := ReadFloat(Section  ,'DeriveMinMax'             ,twcDeriveMinMax);
  twcDeriveBinFraction       := ReadFloat(Section  ,'DeriveBinFraction'        ,twcDeriveBinFraction);
  twcDeriveLookAhead         := ReadInteger(Section,'DeriveLookAhead'          ,twcDeriveLookAhead);
  twcGammaCutoffDepth        := ReadFloat(Section  ,'GammaCutoffDepth'         ,twcGammaCutoffDepth);
  twcGammaCutoffPercent      := ReadFloat(Section  ,'GammaCutoffPercentage'    ,twcGammaCutoffPercent);
  twcGammaLocalDosePerc      := ReadBool( Section  ,'GammaLocalDosePerc'       ,twcGammaLocalDosePerc);
  twcGammaDosePercBase       := ReadFloat(Section  ,'GammaDosePercBase'        ,twcGammaDosePercBase);
  twcGammaDistCmBase         := ReadFloat(Section  ,'GammaDistCmBase'          ,twcGammaDistCmBase);
  twcGammaDistCmStep         := ReadFloat(Section  ,'GammaDistCmStep'          ,twcGammaDistCmStep);
  twcGammaSearchMultiplier   := ReadFloat(Section  ,'GammaSearchFactor'        ,twcGammaSearchMultiplier);
  twcMatchRangeDivider       := ReadInteger(Section,'MatchRangeFactor'         ,twcMatchRangeDivider);
  twcMatchStepsNumber        := ReadInteger(Section,'MatchStepFactor'          ,twcMatchStepsNumber);
  twcMatchNormDeltaPercent   := ReadFloat(Section  ,'MatchNormPercRange'       ,twcMatchNormDeltaPercent);
  twcMatchInclusionLimit     := ReadFloat(Section  ,'MatchMatchInclusionLimit' ,twcMatchInclusionLimit);
  twcSymCorrectionLimit      := ReadFloat(Section  ,twcSymCorrLimitKey+'Limit' ,twcSymCorrectionLimit);
  twcSymCorrectionLevel      := ReadFloat(Section  ,twcSymCorrLimitKey+'Level' ,twcSymCorrectionLevel);
  twcENRlimit                := ReadFloat(Section  ,'ENR'                      ,twcENRlimit);
  twcNMseconds               := ReadFloat(Section  ,'NMsec'                    ,twcNMseconds);
  twcNMrestarts              := Abs(ReadInteger(Section,'NMrestarts'           ,twcNMrestarts));
  twcNMdigits                := Abs(ReadInteger(Section,'NMdigits'             ,twcNMdigits));
  twcNMcycles                := Abs(ReadInteger(Section,'NMcycles'             ,twcNMcycles));
  LogLevel                   := EnsureRange(ReadInteger(Section,'LogLevel'     ,LogLevel),1,4);
  for i:= pddfit_I1 to pddfit_mx2 do
    twcPDDpar[i]             := ReadBool(Section   ,pddfitEnames[i]            ,twcPDDpar[i]);
  ReadModSection(twcDRefBeams,FModBeamList);
  ReadModSection(twcDrefKey  ,FModNormList);
  ReadModSection(twcDrefKey  ,FModFilmList); {patch <3.01}
  ReadModSection(twcDCKey    ,FModFilmList); {starting with v3.01}
  if IsLocal then
    try
      Free;
     except
      ExceptMessage('WH.ReadConfig:CF!');
     end;
  end;
try
  FreeAndNil(S);
 except
  ExceptMessage('WH.ReadConfig:S!');
 end;
try
  if not DirectoryExists(ReferenceDirectory) then
    ReferenceDirectory:= AppendPathDelim(ExtractFilePath(ParamStr(0))+twcRefDirDefaultName);
 except
 end;
Dec(FActiveCnt);
end; {~readconfig}


{04/06/2015 added w2D_ArrayRefList.CommaText}
{11/12/2015 added wFFFMinDoseDifPerc, wFFFMinEdgeDifCm}
{04/01/2016 split wLinacSymSign}
{06/01/2016 twcPddFitMubPower added}
{08/01/2016 twcPddFitMubPowerFixed added}
{12/01/2016 twcPddFitCostENRWeighted added}
{24/06/2016 twcSymCorrectionLevel}
{07/11/2016 twcDCKey, splitting modalitydata in FModFilmList and FModNormList}
{09/11/2016 FModBeammList added}
{10/11/2016 schrijfvolgorde WriteModList aangepast}
{18/01/2017 twcMatchInclusionLimit}
{03/02/2017 edgesigmoid removed as preset}
{12/07/2017 twcPddFitZWeightPower}
{10/01/2020 do not write DUser}
{16/05/2020 storage of wMultiScanNr removed}
{07/03/2021 twcDefaultSSD_MRcm}
procedure TWellhoferData.WriteConfig(CF:TConfigStrings=nil);
var IsLocal: Boolean;
    i      : Integer;
    Section: AnsiString;                                                        //ansi version

  procedure WriteModList(AModList:TModalityList;
                         AKey    :String        );
  var i,j: Integer;
      s  : String;
  begin
  CF.EraseSection(AKey);
  i:= 0;
  while i<AModList.DataCount do
    begin
    s:= AModList.DivisorText[i];
    j:= s.IndexOf('|');                                                         //zero-based
    CF.WriteString(AKey,Copy(s,1,j),Copy(s,j+2));                               //copy is 1-based
    Inc(i);
    end;
  end;

begin
Section:= twcWellhoferKey;
IsLocal:= CF=nil;
if IsLocal then
  CF:= TConfigStrings.Create('');
with CF do
  begin
  WriteModList(FModNormList,twcDrefKey  );
  WriteModList(FModFilmList,twcDCKey    );
  WriteModList(FModBeamList,twcDRefBeams);
  WriteString(Section,twcRefDirKey               ,ReferenceDirectory);
  WriteString(Section,twcPenumbraKey+'XH'        ,Num2Stg(wXPenumbraH               ,0,3));
  WriteString(Section,twcPenumbraKey+'XL'        ,Num2Stg(wXPenumbraL               ,0,3));
  WriteString(Section,twcPenumbraKey+'EH'        ,Num2Stg(wEPenumbraH               ,0,3));
  WriteString(Section,twcPenumbraKey+'EL'        ,Num2Stg(wEPenumbraL               ,0,3));
  WriteString(Section,twcEnergyKey               ,Num2Stg(twcDefaultEnergy_MeV      ,0,2));
  WriteString(Section,twcSSDKey                  ,Num2Stg(twcDefaultSSDcm           ,0,1));
  WriteString(Section,twcSSDKey+'_MR'            ,Num2Stg(twcDefaultSSD_MRcm        ,0,1));
  WriteString(Section,twcFilterKey               ,Num2Stg(FFilterWidth              ,0,3));
  WriteString(Section,'Z_weighting'              ,Num2Stg(twcPddFitZWeightPower     ,0,2));
  WriteString(Section,'Mu_b_Power'               ,Num2Stg(twcPddFitMubPower         ,0,2));
  WriteBool(Section  ,'Mu_b_Fixed'               ,twcPddFitMubPowerFixed                 );
  WriteBool(Section  ,'CostENRweight'            ,twcPddFitCostENRWeighted               );
  WriteBool(Section  ,twcFilterKey+'Outlier'     ,wOutlierFilter                         );
  WriteString(Section,twcMultiScanKey+'list'     ,w2D_ArrayRefList.CommaText             );
  WriteInteger(Section,twcFilterKey+'Limit'      ,twcOutlierPointLimit                   );
  WriteString(Section,twcGridKey                 ,Num2Stg(ResampleGridSize          ,0,3));
  WriteString(Section,'FFFMinDoseDif'            ,Num2Stg(wFFFMinDoseDifPerc        ,0,1));
  WriteString(Section,'FFFMinEdgeDifCm'          ,Num2Stg(wFFFMinEdgeDifCm          ,0,1));
  WriteInteger(Section,twcLinacRadiusKey+'SignGT',wLinacSymSign[fInplane]                 );
  WriteInteger(Section,twcLinacRadiusKey+'SignAB',wLinacSymSign[fCrossplane]              );
  WriteString(Section,twcLinacRadiusKey+'In'     ,Num2Stg(wLinacSymInnerRadiusCm    ,0,2));
  WriteString(Section,twcLinacRadiusKey+'Out'    ,Num2Stg(wLinacSymOuterRadiusCm    ,0,2));
  WriteString(Section,twcCalcKey                 ,Num2Stg(CalcWidth_cm              ,0,3));
  WriteString(Section,'ICD'                      ,twcDefaultICDstring                    );
  WriteString(Section,'D20'                      ,Num2Stg(twcD20                    ,0,3));
  WriteString(Section,'D50'                      ,Num2Stg(twcD50                    ,0,3));
  WriteString(Section,'D80'                      ,Num2Stg(twcD80                    ,0,3));
  WriteString(Section,'D90'                      ,Num2Stg(twcD90                    ,0,3));
  WriteInteger(Section,twcWMSdetKey              ,twcWMSdetInfo                          );
  WriteString(Section,twcSymCorrLimitKey+'Limit' ,Num2Stg(twcSymCorrectionLimit     ,0,2));
  WriteString(Section,twcSymCorrLimitKey+'Level' ,Num2Stg(twcSymCorrectionLevel     ,0,2));
  WriteString(Section,'NCSInFieldArea'           ,Num2Stg(twcNCSInFieldAxis         ,0,2));
  WriteString(Section,'NCSInFieldDiagonal'       ,Num2Stg(twcNCSInFieldDiagonal     ,0,2));
  WriteString(Section,'DeriveMinMax'             ,Num2Stg(twcDeriveMinMax           ,0,2));
  WriteString(Section,'DeriveBinFraction'        ,Num2Stg(twcDeriveBinFraction      ,0,3));
  WriteString(Section,'DeriveLookAhead'          ,Num2Stg(twcDeriveLookAhead        ,0,0));
  WriteString(Section,'GammaCutoffDepth'         ,Num2Stg(twcGammaCutoffDepth       ,0,2));
  WriteString(Section,'GammaCutoffPercentage'    ,Num2Stg(twcGammaCutoffPercent     ,0,2));
  WriteString(Section,'GammaDosePercBase'        ,Num2Stg(twcGammaDosePercBase      ,0,2));
  WriteString(Section,'GammaDistCmBase'          ,Num2Stg(twcGammaDistCmBase        ,0,2));
  WriteString(Section,'GammaDistCmStep'          ,Num2Stg(twcGammaDistCmStep        ,0,4));
  WriteString(Section,'GammaSearchFactor'        ,Num2Stg(twcGammaSearchMultiplier  ,0,1));
  WriteBool(  Section,'GammaLocalDosePerc'       ,        twcGammaLocalDosePerc          );
  WriteString(Section,'MatchRangeFactor'         ,Num2Stg(twcMatchRangeDivider      ,0,1));
  WriteString(Section,'MatchStepFactor'          ,Num2Stg(twcMatchStepsNumber       ,0,1));
  WriteString(Section,'MatchNormPercRange'       ,Num2Stg(twcMatchNormDeltaPercent  ,0,1));
  WriteString(Section,'MatchMatchInclusionLimit' ,Num2Stg(twcMatchInclusionLimit    ,0,2));
  WriteString(Section,'ENR'                      ,Num2Stg(twcENRlimit               ,0,1));
  WriteString(Section,'NMsec'                    ,Num2Stg(twcNMseconds              ,0,1));
  WriteInteger(Section,'NMrestarts'              ,twcNMrestarts                          );
  WriteInteger(Section,'NMdigits'                ,twcNMdigits                            );
  WriteInteger(Section,'NMcycles'                ,twcNMcycles                            );
  for i:= pddfit_I1 to pddfit_mx2 do
    WriteBool(Section ,pddfitEnames[i]           ,twcPDDpar[i]                           );
  if IsLocal then
    begin
    UpdateFile;
    try
      Free;
     except
      ExceptMessage('WH.WriteConfig:CF!');
     end;
    end;
  end; {with CF}
end; {~writeconfig}


function TWellhoferData.DosePoint2Value(DosePoint:twcDoseLevel): twcFloatType;
begin
case Dosepoint of
  dLow : Result:= PenumbraLo;
  dHigh: Result:= PenumbraHi;
  d20  : Result:= twcD20;      {0.2}
  d80  : Result:= twcD80;      {0.8}
  d90  : Result:= twcD90;      {0.9}
  dTemp: Result:= FTempLevel;
  dUser: Result:= FUserLevel;
 else    Result:= twcD50;      {0.5}
 end;
end; {~dosepoint2value}


function TWellhoferData.DosePoint2String(DosePoint:twcDoseLevel): String;
begin
Result:= Num2Stg(Round(100*DosePoint2Value(DosePoint)));
end; {~dosepoint2string}


{$push}{$warn 5092 off}
function TWellhoferData.String2DosePoint(ADose   :String;
                                         ADefault:twcDoseLevel=dUser): twcDoseLevel;
begin
ADose:= LowerCase(ADose);
if              Adose='20'   then Result:= d20
else if         Adose='50'   then Result:= d50
  else if       Adose='80'   then Result:= d80
    else if     Adose='90'   then Result:= d90
      else if   Adose='low'  then Result:= dLow
        else if Adose='high' then Result:= dHigh
          else                    Result:= ADefault;
end; {~string2dosepoint}
{$pop}


{14/10/2020 store result also BinstreamType}
function TWellhoferData.IsBinary(AFileName:String=''): Boolean;
var t: twcFileType;
begin
t:= GetFileType(AFileName,True);
Result:= (t in [twcWellhoferRfb,twcWDA]);
if Result then
  BinStreamType:= t;
end; {~isbinary}


function TWellhoferData.GetNumPoints: Integer;
begin
Result:= GetSourceNumPoints;
end; {~getnumpoints}


function TWellhoferData.GetSourceNumPoints(ASource:twcDataSource=dsMeasured): wmsIntType;
begin
Result:= wSource[ASource].twPoints;
end; {~getsourcenumpoints}


function TWellhoferData.GetBeamType: twcBeamType;
begin
case wSource[dsMeasured].twBeamInfo.twBModality of
  'X': Result:= Photons;
  'P': Result:= Protons;
  'E': Result:= Electrons;
 else  Result:= Other;
 end;
end; {~getbeamtype}


{01/09/2015}
{09/10/2020 added eclipse}
function TWellhoferData.EvaluateFileType(AIndentString:String=''): twcFileType;
begin
if Length(AIndentString)=0 then
  AIndentString:= IdentificationStg;
if      Pos(rfaID        ,AIndentString)=1 then Result:= twcRFA_ascii
else if Pos(eclipseID    ,AIndentString)=4 then Result:= twcEclipse
else if Pos(mccID        ,AIndentString)=1 then Result:= twcMccProfile
else if Pos(wICPAIdentStg,AIndentString)=1 then Result:= twcICprofilerAscii
else if Pos(hdfID        ,AIndentString)=1 then Result:= twcHdfProfile
else if Pos(xioID        ,AIndentString)=1 then Result:= twcCmsProfile
else if Pos(w2ID         ,AIndentString)>0 then Result:= twcW2CAD
else                                            Result:= twcUnknown;
end;


{01/09/2015 EvaluateFileType}
{01/05/2020 transfer BinStream}
{19/08/2020 speed improvement with BinaryOnly}
{22/10/2020 speed improvement by not ignoring result Inherited GetFileType}
function TWellhoferData.GetFileType(AFileName :String ='';
                                    BinaryOnly:Boolean=False): twcFileType;
var Wms     : TWmsData;
    Pips    : TPipsProfiledata;
    Schuster: TSchusterProfiledata;
    s       : ShortString;
    n       : Byte;
begin
s            := '';
FLastFileType:= twcUnknown;
FileFormat   := twcUnknown;
if LoadBinStream(AFileName) then
  begin
  BinStream.Seek(0,soFromBeginning);
  n:= BinStream.Read(s[1],Pred(SizeOf(ShortString)));
  s[0]:= Char(n);
  if (Pos(rfbString1,s)=2) and
     (Pos(rfbString2,s)>0) then
     FLastFileType:= twcWellhoferRfb
  else
    FLastFileType:= EvaluateFileType(s);
  end;
if FLastFileType=twcUnknown then
  begin
  FLastFileType:= Inherited GetFileType(AFileName,BinaryOnly);
  if FLastFileType=twcUnknown then
    begin
    Wms          := TWmsData.Create(FParser,FFileName,BinStream,BinStreamFile);
    FLastFileType:= Wms.GetFileType(AFileName);
    try
      FreeAndNil(Wms);
     except
      ExceptMessage('WH.GetFileType:Wms!');
     end;
    end;
  if (FLastFileType=twcUnknown) and (not BinaryOnly) then
    begin
    Pips         := TPipsProfileData.Create(wPipsPixelCm,FParser,FFileName,BinStream,BinStreamFile);
    FLastFileType:= Pips.GetFileType(AFileName);
    FreeAndNil(Pips);
    if FLastFileType=twcUnknown then
      begin
      Schuster     := TSchusterProfileData.Create(FParser,FFileName,BinStream,BinStreamFile);
      FLastFileType:= Schuster.GetFileType(AFileName);
      try
        FreeAndNil(Schuster);
       except
        ExceptMessage('WH.GetFileType:Schuster!');
       end;
      end; {schuster}
    end; {pips}
  end; {inherited}
Result:= FLastFileType;
end; {~getfiletype}


{23/08/2015 check on sizes>0}
function TWellhoferData.GetFieldLength: twcFloatType;
var Alfa: twcFloatType;
begin
if (FieldGT_cm<=0) or (FieldAB_cm<=0) then
  Result:= UndefinedVal
else
  with wSource[dsMeasured].twBeamInfo do
   case ScanType of
    snGT,snAB: Result:= ifthen((ScanType=snAB) xor ((twBCollimator+180) mod 180=90),FieldAB_cm,FieldGT_cm);
    snAngle  : begin
               try
                 Alfa:= ArcTan(FieldGT_cm/FieldAB_cm)*180/Pi;
                except
                 Alfa:= 0;
                end;
               if (ScanAngle<=Alfa) or (ScanAngle>=180-Alfa) then Result:= FieldGT_cm
               else                                               Result:= FieldAB_cm;
               end;
    snPDD  : Result:= GetEquivalentField;
   else      Result:= UndefinedVal;
   end;
end; {~getfieldlength}


{04/12/2016}
function TWellhoferData.GetEquivalentField(ASource:twcDataSource=dsMeasured): twcFloatType;
begin
with wSource[ASource].twBeamInfo do
  Result:= 2*GetFieldSize(ASource,fInPlane)*GetFieldSize(ASource,fCrossPlane)/(GetFieldSize(ASource,fInPlane)+GetFieldSize(ASource,fCrossPlane));
end; {~getequivalentfield}


function TWellhoferData.GetFieldDepth: twcFloatType;
begin
if ScanType in twcHoriScans then Result:= wSource[dsMeasured].twVector_ICD_cm[Start].m[Beam]
else                             Result:= UndefinedVal;
end; {~getfielddepth}


function TWellhoferData.GetFieldGT: twcFloatType;
begin
Result:= GetFieldSize(dsMeasured,fInplane);
end; {~getfieldgt}


function TWellhoferData.GetFieldAB: twcFloatType;
begin
Result:= GetFieldSize(dsMeasured,fCrossplane);
end; {~getfieldab}


function TWellhoferData.GetFieldSize(ASource   :twcDataSource;
                                     ADirection:twcFieldSizeDesc): twcFloatType;
begin
with wSource[ASource].twBeamInfo do
  Result:= twBFieldHi[ADirection]-twBFieldLo[ADirection];
end; {~getfieldsize}


procedure TWellhoferData.SetFieldGT(ASize:twcFloatType);
begin
with wSource[dsMeasured].twBeamInfo do
  begin
  twBFieldHi[fInplane]:= abs(ASize)/2;
  twBFieldLo[fInplane]:= -twBFieldHi[fInplane];
  end;
end; {~setfieldgt}


procedure TWellhoferData.SetFieldAB(ASize:twcFloatType);
begin
with wSource[dsMeasured].twBeamInfo do
  begin
  twBFieldHi[fCrossplane]:= abs(ASize/2);
  twBFieldLo[fCrossplane]:= -twBFieldHi[fCrossplane];
  end;
end; {~setfieldab}


{20/07/2015}
function TWellhoferData.IsCompositeData(ASource:twcDataSource=dsMeasured): Boolean;
begin
with wSource[ASource] do
  Result:= twValid and twComposite;
end; {~iscompositedata}


function TWellhoferData.GetPenumbraValue(ASource   :twcDataSource;
                                         ADoseLevel:twcDoseLevel;
                                         ASide     :twcSides): twcFloatType;
begin
with wSource[ASource].twLevelPos[ADoseLevel].Penumbra[ASide] do
 if Valid then Result:= Calc
 else          Result:= 0;
end; {~getpenumbravalue}


{20/01/2018}
function TWellhoferData.GetPenumbraValue(ASource   :twcDataSource;
                                         ADoseLevel:String;
                                         ASide     :twcSides): twcFloatType;
var DP: twcDoseLevel;
begin
DP:= String2DosePoint(ADoseLevel,dTemp);
if DP=dTemp then
  try
    Val(ADoseLevel,FTempLevel);
    if FTempLevel>1 then
      FTempLevel:= FTempLevel/100;
    FindLevelPos(ASource,DP);
  except
    DP:= d50;
  end;
Result:= GetPenumbraValue(ASource,DP,ASide);
end; {~getpenumbravalue}


{20/01/2018}
{12/04/2018 better test for executing FindLevelPos}
function TWellhoferData.GetPenumbraValue(ASource   :twcDataSource;
                                         ADoseLevel:twcFloatType;
                                         ASide     :twcSides): twcFloatType;
begin
ADoseLevel:= Abs(ADoseLevel);
if ADoseLevel>1 then
  ADoseLevel:= ADoseLevel/100;
if (ADoseLevel<>FTempLevel) or (not BordersValid(ASource,dTemp)) then
  begin
  FTempLevel:= ADoseLevel;
  FindLevelPos(ASource,dTemp);
  end;
Result:= GetPenumbraValue(ASource,dTemp,ASide);
end; {~getpenumbravalue}


{05/02/2018 name changed from GetPenumbra to GetPenumbraWidth}
{03/09/2020 implementation of wDynamicPenumbraWidth}
{$push}{$warn 5059 off: function result not initialised; there is always a result here}
function TWellhoferData.GetPenumbraWidth(ASource     :twcDataSource;
                                         ASide       :twcSides;
                                         DynamicWidth:Boolean=False): twcFloatType;
var yRef,xLow,xHigh: twcFloatType;
begin
with wSource[ASource] do
  if DynamicWidth then
    begin
    if not twSigmoidFitData[ASide].twFitValid then
      DynamicWidth:= SigmoidPenumbraFit(ASource);
    if DynamicWidth then
      begin
      yRef        := GetNormalisedSigmoidLevel(GetPenumbraValue(ASource,dInflection,ASide),ASource);
      FTempLevel  := PenumbraHi*yRef/50;
      DynamicWidth:= FindLevelPos(ASource,dTemp,False);
      if DynamicWidth then
        begin
        xHigh       := GetPenumbraValue(ASource,dTemp,ASide);
        FTempLevel  := PenumbraLo*yRef/50;
        DynamicWidth:= FindLevelPos(ASource,dTemp,False);
        if DynamicWidth then
          begin
          xLow  := GetPenumbraValue(ASource,dTemp,ASide);
          Result:= Abs(xHigh-xLow);
          end;
        end;
      end;
    end;
if not DynamicWidth then
  Result:= GetLevelDistance(dHigh,dLow,ASide,Asource);
end; {~getpenumbrawidth}
{$pop}


{29/09/2016}
procedure TWellhoferData.SetMultiRefIndex(AMultiRefIndex:Boolean);
begin
FMultiRefIndex:= AMultiRefIndex;
if AMultiRefIndex then
  FRefOrgFileName:= '';
end; {~setmultirefindex}


{10/09/2016 try..except}
procedure TWellhoferData.SetReferenceDir(Directory:String);
var Drive,OldDir: String;
begin
if LowerCase(Directory)<>Lowercase(FReferenceDir) then
  begin
  try
    OldDir:= FReferenceDir;
    Drive := ExtractFileDrive(Directory);
    if (Length(Drive)>0) and (not DirectoryExists(Drive)) then
      Directory:= twcRefDirDefaultName;
    Directory:= ExpandFileName(Directory);
    if not DirectoryExists(Directory) and not ForceDirectories(Directory) then
      Directory:= GetCurrentDir;
    FReferenceDir:= AppendPathDelim(Directory);
   except
    FReferenceDir:= OldDir;
   end;
  end;
end; {~setreferencedir}


procedure TWellhoferData.SetResampleGrid(cm:twcFloatType);
begin
FResampleGrid:= Max(0.0,cm);
end; {~setresamplegrid}


procedure TWellhoferData.SetFilterWidth(cm:twcFloatType);
begin
FFilterWidth:= Max(twcDefMinFilterWidthCm,cm);
end; {~setfilterwidth}


{21/07/2020 GetAdjustedFilterWidthCm}
function TWellhoferData.GetAdjustedFilterWidthCm(ASource:twcDataSource=dsMeasured): twcFloatType;
begin
Result:= FFilterWidth;
with wSource[ASource] do if twSetFieldType=fcSmall then
  begin
  if BordersValid(ASource) then
    Result:= Min(GetFieldWidthCm(ASource)/wSmallFieldFilterDivider,Result)
  else
    Result:= Result/wSmallFieldFilterDivider;
  end;
end; {~getadjustedfilterwidth}


procedure TWellhoferData.SetCalcWidth(cm:twcFloatType);
begin
FCalcWidth_cm:= Max(twcDefMinFilterWidthCm,cm);
end; {~setcalcwidth}


procedure TWellhoferData.ResetAnalysis(ASource:twcDataSource=dsMeasured);
begin
with wSource[ASource] do
  begin
  twFastScan:= False;
  twAnalysed:= False;
  end;
end; {~resetanalysis}


//will clear all, except dsMeasured,dsReference and overhead
{14/09/2020}
{17/09/2020 introduction of FFrozen}
{29/09/2020 keep also dsUnrelated}
procedure TWellhoferData.Purge;
var s: twcDataSource;
begin
if not FFrozen then
  for s:= twcFirstDataSource to twcLastDataSource do
    if s in twcFilterSources then                                               //dsmeasured and dsreference
      ResetAnalysis(s)
    else if not (s in [dsUnrelated,dsRefOrg]) then                              //keep dsUnrelated,dsRefOrg in current state
      ClearCurve(s);
end; {~purge}


procedure TWellhoferData.ResetMultiScanCounters;
begin
wMultiScanNr     := 0;
wMultiScanMax    := 0;
wMultiScanStep   := 1;
wMultiScanLooping:= True;
if assigned(FMcc) then with FMcc do
  begin
  ScanNr := 0;
  ScanMax:= 0;
  end;
end; {~resetmultiscancounters}


procedure TWellhoferData.SetZeroStepsOk(AZeroStepsOk:Boolean);
var k: twcDataSource;
begin
if (FZeroStepsOk<>AZeroStepsOk) then
  begin
  for k:= dsMeasured to twcLastRelated do
    wSource[k].twAnalysed:= False;
  FZeroStepsOk:= AZeroStepsOk;
  end;
end; {~setzerostepsok}


procedure TWellhoferData.SetNoPenumbraOk(ANoPenumbraOk:Boolean);
var k: twcDataSource;
begin
if (FNoPenumbraOk<>ANoPenumbraOk) then
  begin
  for k:= dsMeasured to twcLastRelated do
    wSource[k].twAnalysed:= False;
  FNoPenumbraOk:= ANoPenumbraOk;
  end;
end; {~setnopenumbraok}


procedure TWellhoferData.SetUserLevel(ADoseFraction:twcFloatType);
begin
if (ADoseFraction>0) and (ADoseFraction<=1) then FUserLevel:= ADoseFraction
else                                             FUserLevel:= -1;
end; {~setuserlevel}


{05/06/2015}
procedure TWellhoferData.SetArrayScanRefUse(AState:Boolean);
begin
if AState<>FArrayScanRefUse then
  begin
  FArrayScanRefUse:= AState;
  if AState then
    SetArrayScanRefOk;
  end;
end; {~setarrayscanrefuse}


{13/07/2015 prevent usage of preloaded temporary reference}
{21/07/2015 reforg used now}
{30/12/2016 always invalidate reforg on change}
procedure TWellhoferData.SetAutoLoadRef(AState:Boolean);
begin
if AState<>FAutoLoadRef then
  with wSource[dsRefOrg] do
    begin
    if twValid and (not twLocked) then
      twValid:= False;
    FAutoLoadRef:= AState;
    end;
end; {~setautoloadref}


{01/08/2015}
{15/12/2015 FMRefOrgSrc}
{22/07/2016 dual binary/text mode}
{22/09/2016 bug found; avoid appending to existing data: FRefOrgSrc.Size:= 0}
{10/02/2020 pass MccOriginValue to FRefOrg2D_OriVal}
{01/10/2020 pass FrefOrgSrcType}
{02/10/2020 accidently always dsMeasured was copied}
{05/10/2020 copy also to dsReference}
{14/12/2020 include if MultiRefIndex then...  for MultiscanList}
function TWellhoferData.SetReferenceOrg(ASource        :twcDataSource =dsMeasured;
                                        KeepAsReference:Boolean       =False;
                                        AWellhofer     :TWellhoferData=nil): Boolean;
begin
if AWellhofer=nil then
  AWellhofer:= Self;
Result:= AWellhofer.wSource[ASource].twValid and (not wSource[dsRefOrg].twLocked);
if Result then
  begin
  CopyCurve(AWellhofer.wSource[ASource],wSource[dsRefOrg]);
  CopyCurve(AWellhofer.wSource[ASource],wSource[dsReference]);
  FRefOrgFileName             := AWellhofer.wSource[dsMeasured].twFilename;
  wSource[dsRefOrg].twLocked  := False;
  wSource[dsRefOrg].twFastScan:= False;
  wTakeCurrentRefSource       := KeepAsReference;                               //can also be changed in user interface
  Finalize(FMultiScanList);
  if (wSource[dsRefOrg].twOriginalFormat in twcBinaryFormats) and (addr(FRefOrgSrc)<>addr(AWellhofer.BinStream)) then
    begin {binary}
    if not (FRefOrgSrc is TMemoryStream) then
      begin
      try
        FreeAndNil(FRefOrgSrc);
       except
        ExceptMessage('WH.SetReferenceOrg:FRefOrgSrc1!');
       end;
      FRefOrgSrc:= TMemoryStream.Create;
      end
    else
      FRefOrgSrc.Size:= 0;
    FRefOrgSrc.CopyFrom(AWellhofer.BinStream,0);                                //zero length copies complete stream!
    FrefOrgSrcType:= AWellhofer.BinStreamType;
    end {binary}
  else
    begin {text}
    if not (FRefOrgSrc is TStringStream) then
      begin
      try
        FreeAndNil(FRefOrgSrc);
       except
        ExceptMessage('WH.SetReferenceOrg:FRefOrgSrc2!');
       end;
      FRefOrgSrc:= TStringStream.Create('');
      end
    else
      FRefOrgSrc.Size:= 0;
    AWellhofer.Parser.Strings.SaveToStream(FRefOrgSrc);                         //copy awellhofer to self.FRefOrgSrc
    FRefOrgSrcType:= AWellhofer.FileFormat;
    end; {text}
  {$IFDEF MULTIREF_INDEX}
  if MultiRefIndex then
    begin
    if AWellhofer=Self then
      IndexMultiScan(wSource[dsMeasured].twFileName)
    else if assigned(AWellhofer.FMultiScanList) then
      FMultiScanList:= Copy(AWellhofer.FMultiScanList);
    end;
  {$ENDIF}
  end;
if FRefOrgSrcType=twcMccProfile then                                            //take also this extra information from AWellhofer
  FRefOrg2D_OriVal:= AWellhofer.Fmcc.MccOriginValue;
end; {~setreferenceorg}


{01/08/2015}
{15/12/2015 FMRefOrgSrc}
{21/07/2017 completely clear wSource[RefOrg}
{10/02/2020 clear FRefOrg2D_OriVal}
{30/09/2020 clear also FRefOrgSrc}
procedure TWellhoferData.UnSetReferenceOrg;
begin
FRefOrgSrc.Size := 0;
FRefOrg2D_OriVal:= 0;
FRefOrgSrc.Size := 0;                                                           //clear underlying data
ClearCurve(dsRefOrg);
end; {~unsetreferenceorg}


{05/08/2015}
{13/08/2016 applying ACurveIDStg and wCheckRefCurveString}
{17/11/2016 added failure reporting}
function TWellhoferData.TakeReferenceOrg(ACurveIDStg:String=''): Boolean;
begin
Result:= wSource[dsRefOrg].twValid;
if Result then
  begin
  Result:= (not wSource[dsReference].twLocked);
  if Result then
    begin
    Result:= (not wCheckRefCurveString) or (Length(ACurveIDStg)=0) or (wSource[dsRefOrg].twCurveIDString=ACurveIDStg);
    if Result then
      begin
      CopyCurve(dsRefOrg,dsReference);
      with wSource[dsReference] do
        begin
        twLocked        := False;
        twDataHistoryStg:= twcDataSourceNames[dsReference];
        twAlignedTo     := dsReference;
        end;
      end {idstrings ok}
    else
      ReportDifferences;
    end {not locked}
  else
    StatusMessage('reference locked',False);
  end; {valid}
end; {~takereferenceorg}


{05/06/2015 taken from prepareprofile}
{18/03/2016 do not change state of FArrayScanRefOk when not FArrayScanRefUse or invalid}
{02/08/2016 twFileIDString}
{02/02/2018 MakeCurveName: ignorezerovalue=false}
procedure TWellhoferData.SetArrayScanRefOk(ASource:twcDataSource=dsMeasured);
var i: Word;
begin
with wSource[ASource] do
  begin
  if twValid and FArrayScanRefUse then
    begin
    FArrayScanRefOk:= False;
    if twOriginalFormat=twcGenericProfile then twCurveIDString:= 'generic'
    else                                       twCurveIDString:= MakeCurveName(False,True); {name made with wMultiScanRefOk=false}
    i:= w2D_ArrayRefList.Count;
    with wMeasDeviceInfo do
      if Length(twDeviceName)>0 then
        while (not FArrayScanRefOk) and (i>0) do
          begin
          Dec(i);
          FArrayScanRefOk:= (AnsiCompareText(w2D_ArrayRefList.Strings[i],twDeviceName)=0);
          end;
    end;
  if FArrayScanRefOk then
    begin
    twScanDevice  := wMeasDeviceInfo.twDeviceName;
    twFileIDString:= MakeCurveName(True,True,[],False);
    end
  else
    twFileIDString:= twCurveIDString;
  end;
end; {~setarrayscanrefok}


{18/07/2016 wMultiScanReferences}
{02/08/2016 preserve wMultiScanNr and data, force FArrayScanRefUse off}
{03/08/2016 restore original scan always when curveidstring is correct}
{22/08/2016 use extra object for non-measurement data}
{03/06/2018 FIndexingMode}
{29/09/2020 call to PassSettings changed}
{01/10/2020 progress monitor}
function TWellhoferData.IndexMultiScan(AFileName     :String='';
                                       ACurveIDString:String=''): Boolean;
var i,o         : Integer;
    a,b,r,WExt,l: Boolean;
    s           : String;
    t           : twcFileType;
    W           : TWellhoferData;
    tStart      : TDateTime;
begin
Inc(FActiveCnt);
FIndexingMode:= True;
WExt         := (Length(AFileName)>0) and (AFileName<>FileName);
if WExt then
  begin
  W:= TWellhoferData.Create;
  PassSettings(W,'index',False);
  W.AutoLoadReference:= False;
  W.ReadData(AFileName,0,twcUnknown);
  end
else
  W:= Self;
with W do
  begin
  t     := wSource[dsMeasured].twOriginalFormat;
  Result:= (t in twcMultiFiles);
  if Result then
    begin
    SetLength(FMultiScanList,2);
    FMultiScanList[0]:= FileName;
    a                := FAutoLoadRef;
    FAutoLoadRef     := False;
    b                := FArrayScanRefUse;
    o                := wMultiScanNr*ifthen(GetCurveIDString=ACurveIDString,1,-1); {preserve scannr}
    FArrayScanRefUse := False;
    FParser.Preloaded:= True;
    i                := 0;
    l                := False;
    tStart           := Now;
    ResetMultiScanCounters;
    repeat
      Inc(wMultiScanNr);
      i:= Max(i,wMultiScanNr);
      if t in twcBinaryFormats then r:= ReadData(BinStream     ,0,t)
      else                          r:= ReadData(Parser.Strings,0,t);
      if r then
        begin
        s:= wSource[dsMeasured].twCurveIDString;
        StatusMessage(InsertIdentity(s),False);
        end
      else
        s:= '';
      if (o<0) and (Length(s)>0) and (s=ACurveIDString) then
        o:= wMultiScanNr;
      if Length(FMultiScanList)<Succ(wMultiScanMax) then
       SetLength(FMultiScanList,Succ(wMultiScanMax));
      FMultiScanList[wMultiScanNr]:= s;
      l:= l or (MilliSecondsBetween(Now,tStart)>1000);
      if l then
       begin
       StatusMessage(Format('Indexing %d...',[wMultiScanNr]),False,0);
       tStart:= Now;
       l     := False;
       end;
    until (wMultiScanNr=wMultiScanMax) or (wMultiScanNr<i);
    if not WExt then
      begin
      FAutoLoadRef    := a;
      FArrayScanRefUse:= b;
      wMultiScanNr    := Abs(o);
      if t in twcBinaryFormats then ReadData(BinStream     ,0,t)
      else                          ReadData(Parser.Strings,0,t);
      end;
    end
  else
    Finalize(FMultiScanList);
  end; {with W}
if WExt then
  begin
  FMultiScanList:= W.FMultiScanList;
  try
     FreeAndNil(W);
   except
    ExceptMessage('WH.IndexMultiScan!');
   end;
  end;
FIndexingMode:= False;
Dec(FActiveCnt);
end; {indexmultiscan}


{02/08/2016 replacement of makefilename}
{03/12/2016 IgnoredParams}
{24/01/2107 added offset from non-variable scandirections}
{28/09/2017 for scanangle first result of stg was accidently discarded}
{23/07/2018 aliaslist also applied on wMeasDeviceInfo.twDeviceName}
{21/07/2020 fcWedge}
{07/03/2021 FDefaultSSDcm}
function TWellhoferData.MakeCurveName(CreateMultiName :Boolean     =False;
                                      ApplyDeviceAlias:Boolean     =False;
                                      IgnoredParams   :twcIgnoreSet=[];
                                      IgnoreZeroValue :Boolean=True;
                                      ASource         :twcDataSource=dsMeasured): String;
const ic:array[Inplane..Crossplane] of Char=('i','c');
var Stg    : String;
    tEnergy: Real;
    i      : Word;
    c      : Char;
    t      : twcMeasAxis;
begin
if IgnoredParams=[] then
  IgnoredParams:= wDefaultIgnoreSet;
with wCurveInfo,wSource[ASource],twBeamInfo do
  if FParseOk then
    begin
    if twBEnergy>1 then tEnergy:= twBEnergy
    else                tEnergy:= twBEnergy*1000;
    Stg:= '';
    if not (twiLinac in IgnoredParams) then
      Stg:= Stg+ifthen(ApplyDeviceAlias,ApplyAliasList(ApplyModBeamList(Linac)),Linac);
    if not (twiModality in IgnoredParams) then
      Stg:= Stg+twBModality;
    i  := Length(Stg);
    while i>0 do
      if Stg[i]=#32 then
        Delete(Stg,i,1)
      else
       Dec(i);
    if not (twiEnergy in IgnoredParams) then
      Stg:= Stg+Num2Stg(Round(tEnergy));
    if CreateMultiName then
      Stg:= Stg+Format('_%s_',[ApplyAliasList(wMeasDeviceInfo.twDeviceName)])
    else if not (twiScanClass in IgnoredParams) then
      begin
      if (twiScanDirection in IgnoredParams)then
        begin
        if ScanType in twcHoriScans then c:= 'h'
        else                             c:= 'v';
        end
      else
        c:= twScanTypeString[1];
      Stg:= Stg+c;
      if (ScanType=snAngle) and (not (twiAngle in IgnoredParams)) then
        Stg:= Stg+Num2Stg(ScanAngle,3,0,'0')+'_';
      end;
    if not (twiFieldSize in IgnoredParams) then
      Stg:= Stg+ifthen(FieldGT_cm<>FieldAB_cm,Num2Stg(FieldGT_cm,2,0,'0')+Num2Stg(FieldAB_cm,2,0,'0'),Num2Stg(FieldLength,2,0,'0'));
    if (not (twiDiagonal in IgnoredParams)) and (not CreateMultiName) and twIsDiagonal then
      Stg:= Stg+'d';
    if not (twiSSD in IgnoredParams) then
      Stg:= Stg+Format('_ssd%s',[Num2Stg(Round(ifthen(wRefAtDefaultSSD,FDefaultSSDcm,wSource[ASource].twSSD_cm)),3,'0')]);
    if  not (((FieldDepth=0) and IgnoreZeroValue) or (twiDepth in IgnoredParams) ) then
      Stg:= Stg+Format('_d%sm',[Num2Stg(Round(10*FieldDepth),3,'0')]);
    if  ((WedgeAngle>0) or (twSetFieldType=fcWedge)) and (not (twiWedge in IgnoredParams)) then
      Stg:= Stg+'w';
    for t:= Inplane to CrossPlane do
      if (not twDesVaryingAxis[t]) and (Abs(twVector_ICD_cm[Start].m[t])>1) then
        Stg:= Stg+'_'+ic[t]+Num2Stg(wUserAxisSign[t]*twVector_ICD_cm[Start].m[t],2,0);
    end
  else
    Stg:= 'default';
Result:= AnsiLowerCase(Trim(Stg))+ifthen(CreateMultiName,FOrgExtension,DefaultExtension);
end; {~makecurvename}


function TWellhoferData.DistanceToRefPoint(APoint:twcCoordinate): twcFloatType;
begin
Result:= VectorLength(APoint.m[Inplane   ]-wRefPoint.m[Inplane   ],
                      APoint.m[Crossplane]-wRefPoint.m[Crossplane],
                      APoint.m[Beam      ]-wRefPoint.m[Beam      ]);
end; {~distancetorefpoint}


{17/07/2015 new}
function TWellhoferData.InsertPoint(X,Y    :twcFloatType;
                                    ASource:twcDataSource=dsMeasured): Integer;
var i,j: Integer;
begin
i:= NearestPosition(X,ASource);
with wSource[ASource] do
  begin
  if X>twPosCm[i] then
    Inc(i);
  CheckSize(ASource,Succ(twPoints));                                            //set new size to all related data, including twDataLast
  if (i<twDataLast) then
    for j:= twDataLast downto Succ(i) do                                        //push forward next data points}
      begin
      twCoordinates[j]:= twCoordinates[Pred(j)];
      twPosCm[j]      := twPosCm[Pred(j)];
      twData[j]       := twData[Pred(j)];
      end;
  twCoordinates[i]:= Default(twcCoordinate);
  twPosCm[i]      := X;
  twData[i]       := Y;
  end;
Result:= i;
end; {~insertpoint}


{17/07/2015 new}
function TWellhoferData.InsertPoint(X,Y        :twcFloatType;
                                    ACoordinate:twcCoordinate;
                                    ASource    :twcDataSource=dsMeasured): Integer;
begin
Result:= InsertPoint(X,Y,ASource);
wSource[ASource].twCoordinates[Result]:= ACoordinate;
end; {~insertpoint}


function TWellhoferData.GetReady: Boolean;
begin
Result:=(FActiveCnt=0);
end; {~getready}


{27/12/2017 accept meaningful predefined values for limits}
{14/01/2018 always set twdatalast}
procedure TWellhoferData.CheckSize(var ASource:twCurveDataRec;
                                   NumPoints  :Integer=-1);
begin
with ASource do
  begin
  if NumPoints>=0 then
    twPoints:= NumPoints;
  if Length(twData)<>twPoints then
    begin
    twDataLast:= Pred(twPoints);
    twScanLast:= twDataLast;
    if twDataFirst>0 then
      twDataFirst:= Min(twDataFirst,twDataLast);
    twScanFirst:= Max(twDataFirst,twScanFirst);
    if (twScanLast=0) or (twScanLast>=twDataLast) then
      twScanLast:= twDataLast;
    twFastScan:= False;
    SetLength(twCoordinates,twPoints);
    SetLength(twData       ,twPoints);
    SetLength(twPosCm      ,twPoints);
    end;
  end;
end; {~checksize}


procedure TWellhoferData.CheckSize(ASource  :twcDataSource=dsMeasured;
                                   NumPoints:Integer=-1);
begin
CheckSize(wSource[ASource],NumPoints);
end; {~checksize}


{17/09/2020 introduction of FFrozen}
procedure TWellhoferData.AddPoints(ASource    :twcDataSource;
                                   AddedPoints:Integer=1;
                                   AtFront    :Boolean=False);
var i: Integer;

  procedure DataExpand(var AnArray:twcFloatArray);
  var i: Integer;
  begin
  with wSource[ASource] do
    begin
    SetLength(AnArray,twPoints);
    if AtFront then
      for i:= twPoints-AddedPoints-1 downto 0 do
        AnArray[i+AddedPoints]:= AnArray[i];
    end;
  end;

begin
if not FFrozen then
  with wSource[ASource] do
    begin
    Inc(twPoints  ,AddedPoints);
    DataExpand(twPosCm);
    DataExpand(twData);
    SetLength(twCoordinates,twPoints);
    if AtFront then
      for i:= twPoints-AddedPoints-1 downto 0 do
        twCoordinates[i+AddedPoints]:= twCoordinates[i];
    end;
end; {~addpoints}


{17/09/2020 introduction of FFrozen}
procedure TWellhoferData.SetNumPoints(ASource       :twcDataSource;
                                      MeasuredPoints:wmsIntType=0);
begin
if not FFrozen then
  with wSource[ASource] do
    if twPoints<>MeasuredPoints then
      CheckSize(ASource,MeasuredPoints);
end; {~setnumpoints}


{21/07/2015}
{18/03/2016 always lowercase}
{13/10/2016 StripExtension added}
{05/12/2016 wDefaultIgnoreSet}
{07/12/2016 only result if curve is valid}
function TWellhoferData.GetCurveIDstring(ASource       :twcDataSource=dsMeasured;
                                         StripExtension:Boolean      =False): String;
begin
if wSource[ASource].twValid then
  begin
  if wSource[ASource].twCurveIDString='' then
    SetArrayScanRefOk(ASource);
  if wDefaultIgnoreSet=[] then
    Result:= LowerCase(wSource[ASource].twCurveIDString)
  else
    Result:= MakeCurveName(False,True,wDefaultIgnoreSet,True,ASource);
  if StripExtension then
    Result:= Copy(Result,0,Pred(Pos('.',Result)));
  end
else
  Result:= '';
end; {~getcurveidstring}


function TWellhoferData.GetPosition(ASource:twcDataSource;
                                    i      :Integer): twcFloatType;
begin
with wSource[ASource] do if twValid then
  begin
  if InRange(i,twDataFirst,twDataLast) then Result:= twPosCm[i]
  else                                      Result:= 0;
  end
else                                        Result:= 0;
end; {~getposition}


{06/10/2020 fundamentals alternative}
procedure TWellhoferData.SetAxisID(AxisIDstg      :String;                      //'-YX-Z'
                                   var AxisMapping:twcTankAxisID;
                                   var AxisSigns  :twcTankAxisSign);
var i    : Integer;
    mAxis: twcMeasAxis;
    c    : Char;
begin
i:= 0;
for mAxis:= Inplane to Beam do
  begin
  i:= AxisIDstg.IndexOfAny(['X','Y','Z'],i);                                    //zero-based
  FParseOk:= i>=0;
  if FParseOk then
    begin
    c                 := AxisIDstg.Chars[i];
    AxisMapping[mAxis]:= c;
    AxisSigns.c[c]    := ifthen(AxisIDstg.Chars[Max(0,Pred(i))]='-',-1,1);      //zero-based
    Inc(i);
    end;
  end;
end; {~setaxisid}


{29/07/2015 Make also 'nn/MMM/nnnn' and MMM/nn/nnnn' acceptable for twWellhoferAscii_v7}
{01/09/2015 EvaluateFileType}
{11/12/2018 twWellhoferAscii_v8}
function TWellhoferData.ParseData(CheckFileTypeOnly:Boolean=False): Boolean;
const WellhoferAscii_v8_ident='Measurement time:';
var Stg  : String;
    i,j,k: Integer;
begin
Inc(FActiveCnt);
FParseOk:= inherited ParseData(CheckFileTypeOnly);
Stg     := CharSetReplaceAll(csNumeric,'n',IdentificationStg.ToLower.Replace('-','/').Replace('a','p'));
for i:= 1 to 12 do
  Stg.Replace(FParser.Months[i]+'/','nn/');                                                    //make also 'nn/MMM/nnnn' and MMM/nn/nnnn' acceptable
i:= Max(Pos('n/nnnn ',Stg),Pos('n/nn ',Stg));                                                  //identification parameter
j:= Pos('n:nn:nn',Stg);                                                                        //identification parameter
k:=ifthen(Pos(WellhoferAscii_v8_ident,IdentificationStg)=1,Length(WellhoferAscii_v8_ident),0); //identification parameter
if Pos('Clinic:',IdentificationStg)=1 then
  begin
  FileFormat:= twcWellhoferAscii_v6;
  Result    := CheckFileTypeOnly or Parse_Wellhofer_SNC_Ascii;
  end
else if InRange(i,1+k,7+k) and InRange(j-i,4,15) then                                          //i,j,k used here for identification
  begin
  if k=0 then FileFormat:= twcWellhoferAscii_v7
  else        FileFormat:= twcWellhoferAscii_v8;
  Result    := CheckFileTypeOnly or Parse_Wellhofer_SNC_Ascii;
  end
else if Pos('Tab-Delimited Scan Output',IdentificationStg)=1 then
  begin
  FileFormat:= twcSNCfileAscii;
  Result    := CheckFileTypeOnly or Parse_Wellhofer_SNC_Ascii;
  end
else if Pos('Delivery System',IdentificationStg)=1 then
  begin
  FileFormat:= twcSNCclipboard;
  Result    := CheckFileTypeOnly or Parse_Wellhofer_SNC_Ascii;
  end
else
  begin
  FileFormat:= EvaluateFileType;
  Result    := CheckFileTypeOnly;
  end;
Dec(FActiveCnt);
end; {~parsedata}


{02/10/2015 twcOmniPro7MinRatioPerc}
{20/03/2016 read all ratiodata in v7 and decide afterwards whether or not to use these}
{16/05/2020 applied FMultiScanCapable}
{06/10/2020 fundamentals alternative}
{21/10/2020 chop off " Accelerator" in v8 Linac name}
function TWellhoferData.Parse_Wellhofer_SNC_ascii: Boolean;
var Stg         : String;
    DataAxisID  : twcTankAxisID;
    DataAxisSwap: twcTankAxisSign;

  function SearchLine(ASearchText    :String;
                      var DateTimeStg:String;
                      var DateTime   :TDateTime;
                      dmy_order      :String='ymdHMS';
                      AM_order       :String='mdyHMS'): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk   := Search(ASearchText);
    DateTimeStg:= RemainderOfLine;
    DateTime   := NextDate(dmy_order,AM_order);
    end;
  Result:= FParseOk;
  end; {searchline}

  function SearchLine(ASearchText    :String;
                      var Remainder  :String;
                      OptionalText   :String='';
                      FromCurrentLine:Boolean=False;
                      ApplyAlias     :Boolean=True): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText,OptionalText,True,FromCurrentLine);
    if ApplyAlias then Remainder:= ApplyAliasList(RemainderOfLine)
    else               Remainder:= RemainderOfLine;
    end;
  Result:= FParseOk;
  end;

  function SearchLine(ASearchText:String;
                      var Value  :Integer;
                      AcceptNAN  :Boolean=False): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText,True);
    Value   := NextInteger;
    FParseOk:= ConversionResult or AcceptNAN;
    end;
  Result:= FParseOk;
  end;

  function SearchLine(ASearchText:String;
                      var Value  :SmallInt;
                      AcceptNAN  :Boolean=False): Boolean;  overload;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText,True);
    Value  := NextInteger;
    FParseOk:= ConversionResult or AcceptNAN;
    end;
  Result:= FParseOk;
  end;

  function SearchLine(ASearchText:String;
                      var Value  :twcFloatType;
                      DetectScale:Boolean=False;
                      AcceptNAN  :Boolean=False;
                      AMultiplier:twcFloatType=1): Boolean;  overLoad;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk    := Search(ASearchText,True);
    AMultiplier:= ifthen(DetectScale and (Pos('mm',FParser.CurrentLine)>0),0.1,1)*AMultiplier;
    try
      Value:= NextFloat*AMultiplier;
     except
      Value:= 0;
     end;
    FParseOk:= ConversionResult or AcceptNAN;
    end;
  Result:= FParseOk;
  end;

  function SearchLine(ASearchText      :String;
                      var Value1,Value2:twcFloatType;
                      DetectScale      :Boolean=False;
                      AcceptNAN        :Boolean=False): Boolean;  overLoad;
  var Scale: twcFloatType;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText,True);
    Scale   := ifthen(DetectScale and (Pos('mm]',FParser.CurrentLine)>0),0.1,1);
    Value1  := NextFloat*Scale;
    FParseOk:= FParseOk and (ConversionResult or AcceptNAN);
    Value2  := NextFloat*Scale;
    FParseOk:= FParseOk and (ConversionResult or AcceptNAN);
    end;
  Result:= FParseOk;
  end;

  function SearchLine(ASearchText      :String;
                      var Value1,Value2:Integer;
                      AcceptNAN        :Boolean=False): Boolean;  overLoad;
  begin
  if FParseOk then with FParser do
    begin
    FParseOk:= Search(ASearchText);
    Value1  := NextInteger;
    FParseOk:= FParseOk and (ConversionResult or AcceptNAN);
    Value2  := NextInteger;
    FParseOk:= FParseOk and (ConversionResult or AcceptNAN);
    end;
  Result:= FParseOk;
  end;

  function SearchLine(ASearchText   :String;
                      var Coordinate:twcCoordinate;
                      DetectScale   :Boolean=False;
                      ZeroForNAN    :Boolean=False;
                      AMultiplier   :twcFloatType=1): Boolean;  overLoad;
  var mAxis: twcMeasAxis;
      tChar: twcTankAxisChar;
      tAxis: twcTankAxis;
  begin
  if FParseOk then with FParser do
    begin
    if Length(ASearchText)>0 then FParseOk:= Search(ASearchText)
    else                          FParseOk:= NextLine;
    AMultiplier:= ifthen(DetectScale and (Pos('mm',FParser.CurrentLine)>0),0.1,1)*AMultiplier;
    for mAxis:= Inplane to Beam do
      if FParseOk then
        begin
        tChar              := DataAxisID[mAxis];
        tAxis              := twcTankAxis(Ord(tChar)-Ord('X'));
        Coordinate.t[tAxis]:= NextFloat*DataAxisSwap.c[tChar]*AMultiplier;   {@@@@@test - toegevoegd @@@@}
        FParseOk           := ConversionResult or ZeroForNAN;
        end; {for}
    end;
  Result:= FParseOk;
  end; {searchline}

  procedure SearchLine(ASearchText     : String;
                       var ADesTypeStg : String;
                       var ADesScanType: twcScanTypes;
                       var AMeasAxis   : twcMeasAxis;
                       var AScanAngle  : twcFloatType);         overload;
  var Stg: String;
  begin
  if SearchLine(ASearchText,ADesTypeStg) then
    begin
    Stg       := LowerCase(ADesTypeStg);
    AScanAngle:= 0;
    if      Pos('inplane'  ,Stg)+
            Pos('inline'   ,Stg) >0       then begin
                                               ADesScanType:= snGT;
                                               AMeasAxis   := InPlane;
                                               end
    else if Pos('cross'    ,Stg)>0        then begin
                                               ADesScanType:= snAB;
                                               AMeasAxis   := Crossplane;
                                               end
    else if Pos('diagonal' ,Stg)>0        then begin
                                               ADesScanType:= snAngle;
                                               AMeasAxis   := Crossplane;
                                               AScanAngle  := ifthen(Pos('diagonalmm',Stg)>0,-45,45);
                                               end
    else if Pos('depth'    ,Stg)+
            Pos('beam'     ,Stg)>0        then begin
                                               ADesScanType:= snPDD;
                                               AMeasAxis   := Beam;
                                               end
    else if Pos('angle'    ,Stg)>0        then ADesScanType:= snAngle
    else if (Pos('point to point',Stg)>0) or
            (Pos('freescan',Stg)>0)       then ADesScanType:= snFreescan
    else if Pos('fanline'  ,Stg)>0        then ADesScanType:= snFanLine
    else FParseOk:= False;
    end;
  end;

  procedure ParseModality;
  begin
  with wSource[dsMeasured].twBeamInfo do
    if Pos('Photon',FParser.CurrentLine)+Pos('MV',FParser.CurrentLine)>0    then twBModality:= 'X'
    else if Pos('Ele',FParser.CurrentLine)+Pos('MeV',FParser.CurrentLine)>0 then twBModality:= 'E'
    else                                                                         twBModality:= 'P';
  end;

  function ReadPoint(Nr            :Integer;
                     ZeroForNAN    :Boolean=False;
                     CoorMultiplier:twcFloatType=1;
                     AStopStg      :String=''): Boolean;
  begin
  if FParseOk then with wSource[dsMeasured],FParser do
    begin
    SearchLine('',twCoordinates[Nr],False,ZeroForNAN,CoorMultiplier);   {lees xyz}
    if FParseOk then
      begin
      twData[nr]:= NextFloat;                          {lees waarde}
      FParseOk  := ConversionResult or ZeroForNAN;
      end;
    if not FParseOk then
      ErrorString:= Format(twForPointError,[Succ(Nr)]);
    end;
  if Length(AStopStg)=0 then Result:= FParseOk
  else                       Result:= Pos(AStopStg,FParser.CurrentLine)=0;
  end; {readpoint}

  procedure Wellhofer_ascii_v6;
  var c: Char;
      i: Integer;
      m: twcMeasAxis;
      s: twcFloatType;
  begin
  FMultiScanCapable:= False;
  FIdentity        := 'omnipro_v6';
  m                := Inplane;
  with wGeneralInfo,wCurveInfo,wSource[dsMeasured],twBeamInfo,
       wMeterInfo,wDetectorInfo,wMeterInfo,wMeasDeviceInfo do
    begin
    SetAxisID('XYZ',DataAxisID,DataAxisSwap);                                   //data are GT , AB , UD, Dose
    wSource[dsMeasured].twOriginalFormat:= FileFormat;
    SearchLine('Clinic:'   ,twClinic               );
    SearchLine('Address:'  ,twAddress              );
    SearchLine('Telephone:',twTelephone            );
    SearchLine('Email:'    ,twEmail                );
    SearchLine('Device'    ,Linac,':'  ,False,False);
    if FParseOk then
      begin
      i:= Linac.IndexOfAny(CharsetToString(csNotAlphaNumeric));                 //U02: Elekta SL25-15 (i:= 3)
      if i>=0 then
        Linac:= Linac.Remove(i);
      wSource[dsMeasured].twDevice:= Linac;
      end;
    if SearchLine('Energy'            ,twBEnergy) then ParseModality;
    SearchLine('Wedge:'               ,twBWedge);
    SearchLine('Gantry'               ,twBGantry);
    SearchLine('Collimator'           ,twBCollimator);
    SearchLine('SSD'                  ,wSource[dsMeasured].twSSD_cm,twBSAD_cm,True);
    SearchLine('Applicator:'          ,twBApplicator);
    SearchLine('Field position inline',twBFieldLo[fInplane   ],twBFieldHi[fInplane   ],True);
    SearchLine('crossline'            ,twBFieldLo[fCrossplane],twBFieldHi[fCrossplane],True);
    SearchLine('Medium:'              ,twBMedium);
    SearchLine('CurveType:'           ,twDesTypeString,twDesScanType,m,ScanAngle);
    SetScanType(twDesScanType);
    SearchLine('Time:'                ,wSource[dsMeasured].twMeasTime,
                                       wSource[dsMeasured].twMeasDateTime);
    SearchLine('Time:'                ,twDesModTime,twDesModDateTime);
    SearchLine('Operator:'            ,twDesOperator);
    SearchLine('comment:'             ,twDesMeasComment);
    SearchLine('comment:'             ,twDesSetupComment);
    SearchLine('Renormalise'          ,twDesNormalise,False,True);
    SearchLine('offset'               ,twDesShift,True,True);
    if FParseOk and SearchLine('Quantity:',twDetQuantity) then with FParser do
      begin
      NextLine;
      twFilmData  := Pos('Type',CurrentLine)=0;
      if twFilmData then
        begin
        twDetType         := 'Film';
        twDetName         := twcDefUnknown;
        twDetPeffOffset_cm:= 0;
        twElMeterType     := twcDefUnknown;
        twElMeasMode      := twcDefUnknown;
        end
      else
        begin
        SearchLine('Type:',Stg,'',True);
        twDetType          := Trim(Copy(Stg,1,Pred(Pos('Offset',Stg))));  {Detector Type: IC 15	Ion Chamber (Cylindrical)  Offset to P eff [cm]: -0.18	Radius:68cm}
        twDetName          := twDetType;
        FParseOk           := Search('[cm]:',False,True);                       //both [cm] and [mm] are found into the wild
        if FParseOk then
          s                := 1
        else
          begin
          CurrentLineNumber:= LastLineOkNumber;
          FParseOk          := Search('[mm]:',False,True);
          s                := 0.1;
          end;
        if FParseOk then
          begin
          twDetPeffOffset_cm:= NextFloat*s;
          twDetRadius_cm    := NextFloat*s;
          end;
        while twDetRadius_cm>1 do
          twDetRadius_cm:= twDetRadius_cm/10;
        SearchLine('Electrometer type:' ,twElMeterType);
        SearchLine('mode:'              ,twElMeasMode);
        SearchLine('(avg, min, max)'    ,twElRefAvg,twElRefMin);
        twElRefMax:= NextInteger;
        SearchLine('per point'          ,twElSamples,True);
        SearchLine('Sampling'           ,twElSampleMs,True);
        SearchLine('HV type:'           ,twElHVType);
        SearchLine('Normalisation value',twElChannels[FieldCh].twNorm       ,twElChannels[RefCh].twNorm       ,False,True);
        SearchLine('Dark'               ,twElChannels[FieldCh].twDarkCurrent,twElChannels[RefCh].twDarkCurrent,False,True);
        SearchLine('voltage'            ,twElChannels[FieldCh].twHV         ,twElChannels[RefCh].twHV         ,False,True);
        SearchLine('Gain'               ,twElChannels[FieldCh].twGain       ,twElChannels[RefCh].twGain       ,True);
        if SearchLine('reference:'      ,Stg) then
          begin
          i:= Pos(#32,Stg);
          twElChannels[FieldCh].twRange:= copy(Stg,1,i-1);
          twElChannels[RefCh  ].twRange:= copy(Stg,i,Length(Stg)-1);
          end;
        end;
      end;
    SearchLine('Servo type',twDeviceName);
    SearchLine('Speed'     ,twDeviceSpeed_mm_s,True,True,10);
    SearchLine('surface'   ,twDeviceWaterSurface_cm,True,True);
    SearchLine('offset'    ,twDeviceWaterOffset_cm,True,True);
    SearchLine('Origin'    ,twDeviceOriginXYZ_cm,True,True);
    i:= 0;
    if SearchLine('axis',Stg) then
      begin
      if Pos('X',Stg)=0 then
        Stg:= twcDefaultICDstring;
      SetAxisID(Stg,twDeviceMappingICD,twDeviceDirXYZ);
      end;
    SearchLine('Isocentre'            ,twDeviceIsocICD_cm    ,True,True);
    SearchLine('Normalisation'        ,twDeviceNormICD_cm    ,True,True);
    for c:= 'A' to 'D' do
      SearchLine(c,twDeviceRefPosition_cm[c],True,True);
    if SearchLine('Number Of Points:' ,i) then
      begin
      SetNumPoints(dsMeasured,i);                                               //resize arrays, set twDataFirst, twDataLast
      SearchLine('Start'              ,twVector_ICD_cm[Start],True,True);
      SearchLine('End'                ,twVector_ICD_cm[Stop ],True,True);
      FParser.Search('Points');
      s:= ifthen(Pos('[mm]',FParser.CurrentLine)>0,0.1,1);
      i:= twDataFirst;
      while FParseOk and (i<=twDataLast) do
        begin
        ReadPoint(i,False,s);
        Inc(i);
        end;
      if FParseOk then
        begin
        SearchLine('Angle:',ScanAngle);
        FParseOk:= True;
        end
      else
        begin
        SetNumPoints(dsMeasured,Pred(i));
        FParseOk:= i>twcDefMinProfilePoints;
        if Pos('Angle',FParser.CurrentLine)>0 then
          begin
          FParser.GotoLeft;
          ScanAngle:= FParser.NextFloat;
          end;
        end;
      twValid:= FParseOk;
      end;
    end;
  end; {wellhofer_asccii_v6}

 {The "Normalised field" is the value that uses the normalisation the user has performed (without reference division).
  The "Ratio" is the normalised field value incl. the reference division.
  The .rfb file format does not preserve the current or the reference signal values, only the relative one.
  If you open an .rfb file in OmniPro Accept 7.x, and generate the ASCII output, the relative value is stored under the "Normalised field" column,
  because there is no data to perform the reference division and fill in the "Ratio" column.}

  {$push}{$warn 5091 off: NormalisedData not initialised}
  procedure Wellhofer_ascii_v7; {works for variants 7.1-4 and 8}
  const LinacNameAddition=' Accelerator';
  var i,j                        : Integer;
      m                          : twcMeasAxis;
      NormalisedData             : twcFloatArray;
      Normalised,NormalisedMax,
      Ratio,RatioMax             : twcFloatType;
      Stg                        : String;
  begin
  FMultiScanCapable:= False;
  FIdentity        := 'omnipro_v7';
  m                := Inplane;
  Stg              := '';
  with wGeneralInfo,wSource[dsMeasured],twBeamInfo,wCurveInfo,
       wMeterInfo,wDetectorInfo,wMeterInfo,wMeasDeviceInfo do
    begin
    SetAxisID(twcDefaultICDstring,twDeviceMappingICD,twDeviceDirXYZ);
    SetAxisID('-XYZ',DataAxisID,DataAxisSwap); {data are TG , AB , UD, Dose}
    SearchLine(''                ,wSource[dsMeasured].twMeasTime,
                                  wSource[dsMeasured].twMeasDateTime,'dmyHMS');
    twDesModDateTime:= wSource[dsMeasured].twMeasDateTime;
    wSource[dsMeasured].twOriginalFormat:= FileFormat;
    if SearchLine('device:'           ,Linac,'',False,False) then
      wSource[dsMeasured].twDevice:= Linac;
    if Linac.EndsWith(LinacNameAddition,True) then                              //v8 shit
      Linac:= Linac.Remove(Linac.Length-Length(LinacNameAddition),Length(LinacNameAddition));
    if SearchLine('Energy:'           ,twBEnergy) then
      ParseModality;
    SearchLine('Controller:'          ,twElMeterType);
    SearchLine('detector:'            ,twDetType);
    SearchLine('SAD:'                 ,twBSAD_cm,False,False,0.1);
    SearchLine('SSD:'                 ,wSource[dsMeasured].twSSD_cm,False,False,0.1);
    FParseOk   := FParser.Search('Field size:',True);
    FieldAB_cm := FParser.NextFloat/10;
    FParseOk   := FParseOk and FParser.ConversionResult;
    FieldGT_cm := FParser.NextFloat/10;
    FParseOk   := FParseOk and FParser.ConversionResult;
    twDetName  := twDetType;
    SearchLine('Gantry'               ,twBGantry);
    SearchLine('medium:'              ,twBMedium);
    SearchLine('Scan type:'           ,twDesTypeString,twDesScanType,m,ScanAngle);
    SetScanType(twDesScanType);
    SearchLine('mode:'                ,twElMeasMode);
    if FileFormat=twcWellhoferAscii_v8 then
      begin
      SearchLine('Wedge:',Stg);
      if Trim(Stg)='none' then wSource[dsMeasured].twBeamInfo.twBWedge:= 0
      else                     wSource[dsMeasured].twBeamInfo.twBWedge:= FParser.NextInteger;
      end;
    FParseOk     := FParser.Search('Points') and FParser.Search('Crossline',True,True);
    RatioMax     := 0;
    NormalisedMax:= 0;
    if FParseOk then
      begin
      repeat
        i:= GetNumPoints;
        j:= Succ(i);
        SetNumPoints(dsMeasured,j);
        SetLength(NormalisedData,j);
        SearchLine('',wSource[dsMeasured].twCoordinates[i],False,False,0.1);    {lees xyz}
        if FParseOk then
          begin
          Normalised       := FParser.NextFloat;
          NormalisedData[i]:= Normalised;
          FParser.NextFloat; {"Current field"}
          FParseOk         := FParser.ConversionResult;
          if FParseOk then
            begin
            Ratio        := FParser.NextFloat;
            twData[i]    := Ratio;
            NormalisedMax:= Max(Normalised,NormalisedMax);
            RatioMax     := Max(Ratio     ,RatioMax);
            FParseOk     := FParser.ConversionResult;
            end;
          end; {FParseOk}
       until not FParseOk;
      if (RatioMax<twcOmniPro7MinRatioPerc) and (NormalisedMax>10) then
        wSource[dsMeasured].twData:= NormalisedData;
      Finalize(NormalisedData);
      SetNumPoints(dsMeasured,i);
      FParseOk:= (i>0);
      end;
    end;
  end; {wellhofer_asccii_v7}
  {$pop}

    { SNC_ascii
    Tab-Delimited Scan Output
    FILE HEADER
    File Name	test 22_03_2012.snctxt
    File Date	03/26/2012 18:01
    File Export Version	1.4.0.3216
    File Version	2.0
    File Scan Count	1
    BEGIN SCAN
    Summary Comments
    Summary Beam Type	Photon
    Summary Energy (MV/MeV)	6.00
    Summary FieldSize X (cm)	40.00
    Summary FieldSize Y (cm)	40.00
    Summary Wedge Type	Open Field
    Summary Wedge Angle (degrees)	0.00
    Summary Scan Type	Crossline


    BEGIN DOSE TABLE
    Action	Initial Scan
	    X (cm)	Y (cm)	Z (cm)	Relative Dose (%)
	    -25.971	-0.014	5	4.71918616758957
	    -25.861	-0.014	5.001	4.80255899979677
	    -25.751	-0.013	5.001	4.91166572174545
    ...
	    25.724	-0.004	5.001	5.03370666050066
	    25.834	-0.005	5.001	4.97704821355911
	    25.944	-0.005	5.001	4.88959523205777
	    26	-0.005	5.001	4.79638982365823
    END DOSE TABLE
    SCAN HEADER
    FACILITY INFORMATION
    Institution	UMC
    Delivery System	U3
    Delivery System Manufacturer	Elekta
    Delivery System Model #
    Delivery System Serial #
    Field Detector Model #	IC15 F
    Field Detector Serial #
    Reference Detector Model #	IC15 R
    Reference Detector Serial #
    SNC EQUIPMENT
    Application Programming Interface	1.4.0.3216
    Hardware Device Interface	1.4.0.34714
    3D Scanner Model #	Not Available
    3D Scanner Serial #
    3D Scanner Firmware
    Drive Factor Diameter (pulse/mm)	1350
    Drive Factor Vertical (pulse/mm)	630
    Drive Factor Ring (pulse/mm)	706.67
    Electrometer Model #	Not Available
    Electrometer Serial #
    Electrometer Firmware
    Leveling Platform Model #	Not Available
    Leveling Platform Serial #	Not Available
    Lift Table Model #	Not Available
    Lift Table Serial #	Not Available
    Reservoir Model #	Not Available
    Reservoir Serial #	Not Available
    DELIVERY SYSTEM
    Beam Type	Photon
    Energy (MV / MeV)	6.00
    Gantry Angle (degrees)	0
    Collimator Angle (degrees)	0.00
    Collimation Type	Jaws
    Wedge Type	Open Field
    Wedge Angle (degrees)	0.00
    Wedge Direction
    Field Size X (cm)	40.00
    Field Size Y (cm)	40.00
    Collimator Position Jaws X1 (cm)
    Collimator Position Jaws X2 (cm)
    Collimator Position Jaws Y1 (cm)
    Collimator Position Jaws Y2 (cm)
    Collimator Position MLC X1 (cm)
    Collimator Position MLC X2 (cm)
    Collimator Position MLC Y1 (cm)
    Collimator Position MLC Y2 (cm)
    SETUP PARAMETERS
    Ring Center (cm)	25.2190724278959
    Angle Offset (degrees)	4.1
    Hysteresis Minus (cm)	0.093802416407125
    Hysteresis Plus (cm)	-0.094144434729505
    MEASUREMENT DETAILS
    Comments
    Scan Id	30
    Scan Date	03/22/2012 16:28
    Scan Type	Crossline
    Scan Medium	Water
    Source to Surface Distance (cm)	100.00
    Measurement Mode	Continuous
    Scan Speed (cm/s)	0.5 cm / second
    Diameter Drive Scan Direction	False
    Half Beam	False
    Additional Scan Range (cm)	5.00
    Integrated Measurement	False
    Effective Point of Measurement (cm)	0.177
    Detector Bias Voltage (V)	299.77
    Field Background Rate (counts/s)	0.00489790218161142
    Reference Background Rate (counts/s)	0.00625953893882111
    Normalisation Value (Field/Reference)	0.984774818258455
    Electrometer Temperature (degrees C)	7.0010967095781666666666666672
    Probe Temperature (degrees C)	31.999694819562055555555555558
    Pressure (mbar)	1043.23940557632000
    +5VA Sensor	5.07988929748535
    }
  {16/05/2020 implemented multiscan capability, never tested however}
  procedure SNC_ascii;
  var FieldSize: array['X'..'Y'] of twcFloatType;
      c        : Char;
      i        : Integer;
      b        : Boolean;
      mAxis    : twcMeasAxis;
  begin
  with wGeneralInfo,wSource[dsMeasured],twBeamInfo,wCurveInfo,
       wMeterInfo,wDetectorInfo,wMeterInfo,wMeasDeviceInfo do
    begin
    FMultiScanCapable:= True;
    FIdentity        := 'SNC .snctxt';
    mAxis            := InPlane;
    SetAxisID('Y-XZ',twDeviceMappingICD,twDeviceDirXYZ);  {AB, TG, UD}
    DataAxisID                        := twDeviceMappingICD;
    DataAxisSwap                      := twDeviceDirXYZ;  {TG -> GT}
    twOriginalFormat                  := FileFormat;
    SearchLine('File Name'      ,FFileName);
    SearchLine('File Date'      ,Stg,FileTime,'mdy');
    SearchLine('File Scan Count',ScanMax);
    ScanNr  := Max(1,ScanNr);
    ScanNrOk:= 0;
    while FparseOk and (ScanNrOk<ScanNr) do
       begin
       SearchLine('Summary Comments',twDesMeasComment);
       Inc(ScanNrOk);
       end;
    if FParseOk then
      begin
      if SearchLine('Beam Type',Stg) then
        ParseModality;
      SearchLine('Energy',twBEnergy);
      for c:= 'X' to 'Y' do
        begin
        FieldSize[c]:= 0;
        SearchLine('FieldSize',FieldSize[c]);
        end;
      SearchLine('Summary Wedge Type',Stg);
      if Trim(Stg)='Open Field' then
        twbWedge:= 0
      else
        SearchLine('Angle'  ,twBWedge);
      SearchLine('Scan Type',twDesTypeString,twDesScanType,mAxis,ScanAngle);
      SetScanType(twDesScanType);
      if SearchLine('Action',Stg) then
        FParser.NextLine;
      if FParseOk then
        begin
        repeat
          i:= GetNumPoints;
          SetNumPoints(dsMeasured,Succ(i));
          b:= ReadPoint(i,True,1,'END DOSE');
        until not b;
        SetNumPoints(dsMeasured,i);
        FParseOk:= not b;
        end;
      SearchLine('Institution'    ,twClinic                    );
      SearchLine('Delivery System',Linac        ,'',False,False);
      SearchLine('Field'          ,twDetType                   );
      SearchLine('Collimator'     ,twBCollimator               );
      SearchLine('Scan Date'      ,wSource[dsMeasured].twMeasTime,wSource[dsMeasured].twMeasDateTime,'mdy');
      FieldGT_cm                  := FieldSize['Y'];
      FieldAB_cm                  := FieldSize['X'];
      wSource[dsMeasured].twDevice:= Linac;
      end; {fparseok}
    end;
  end; {snc_ascii}

    { SNC_clipboard
    Delivery System	U09               Delivery System	U09
    Energy	10 MV                     Energy	10 MV
    Scan Type	Crossline               Scan Type	PDD
    Depth	5.00                        Depth
    Field	21cm x 16cm (Jaws)          Field	9.6cm x 10.4cm (Jaws)
    Wedge	Open Field                  Wedge	Open Field
    Comments	26x26 cr                Comments	10.4x9.6
	    5.00 cm : ScanId=406             	9.6cm x 10.4cm : ScanId=397
    -18.075	2.44                      0	80.71
    -17.825	2.56                      0.25	113.77
    ...
    }

  procedure SNC_clipboard;
  var FieldSize: array['X'..'Y'] of twcFloatType;
      i        : Integer;
      b        : Boolean;
      mAxis    : twcMeasAxis;
      Depth    : twcFloatType;
  begin
  with wGeneralInfo,wSource[dsMeasured],twBeamInfo,wCurveInfo,
       wMeterInfo,wDetectorInfo,wMeterInfo,wMeasDeviceInfo,wSource[dsMeasured] do
    begin
    FMultiScanCapable:= False;
    FIdentity        := 'SNC clipboard';
    twMeasDateTime   := FileTime;
    Depth            := 0;
    mAxis            := Inplane;
    i                := 0;
    FieldSize['X']   := 0;
    FieldSize['Y']   := 0;
    DateTimeToString(twMeasTime,'yyyy-mm-dd hh:nn:ss',FileTime);
    SetAxisID('Y-XZ',twDeviceMappingICD,twDeviceDirXYZ);  {AB, TG, UD}
    DataAxisID                         := twDeviceMappingICD;
    DataAxisSwap                       := twDeviceDirXYZ;  {TG -> GT}
    twOriginalFormat:= FileFormat;
    SearchLine('Delivery System',Linac,'',True,False);  {Delivery System	U09\t}
    wSource[dsMeasured].twDevice:= Linac;
    SearchLine('Energy',twBEnergy);
    SearchLine('Scan Type',twDesTypeString,twDesScanType,mAxis,ScanAngle);
    SetScanType(twDesScanType);
    SearchLine('Depth',Depth,False,True);
    SearchLine('Field',FieldSize['X'],FieldSize['Y']);
    SearchLine('Wedge',Stg);
    twbWedge:= ifthen(Stg.Trim.ToLower='open field',0,60);
    SearchLine('ScanId',i);
    b:= True;
    if FParseOk then with FParser do
      while b do
         begin
         b:= NextLine;
         if b then
           begin
           i:= GetNumPoints;
           SetNumPoints(dsMeasured,Succ(i));
           twCoordinates[i]         := Default(twcCoordinate);
           twCoordinates[i].m[mAxis]:= NextFloat;
           FParseOk:= ConversionResult;
           if FParseOk then
             begin
             if mAxis<>Beam then
               twCoordinates[i].m[Beam]:= Depth;
             twData[i]:= NextFloat;
             FParseOk:= ConversionResult;
             end;
           if not FParseOk then
             SetNumPoints(dsMeasured,i);
           end;
         end; {while}
    if FParseOk and (not b) then
      begin
      SetNumPoints(dsMeasured,i);
      FieldGT_cm:= FieldSize['Y'];
      FieldAB_cm:= FieldSize['X'];
      end;
    end;
  end; {snc_clipboard}

begin
Inc(FActiveCnt);
FParseOk:= True;
FParser.GotoTop;
if AutoDecimalPoint then
  FParser.AutoSetDecPoint(AutoDecimalList)
else
  FParser.SetDecPointChars(['.']);
case FileFormat of
  twcWellhoferAscii_v6                     : Wellhofer_ascii_v6;
  twcWellhoferAscii_v7,twcWellhoferAscii_v8: Wellhofer_ascii_v7;
  twcSNCfileAscii                          : SNC_ascii;
  twcSNCclipboard                          : SNC_Clipboard;
 else
  FParseOk:= False;
 end;
Result:= ReadResults;
Dec(FActiveCnt);
end; {~parse_wellhofer_snc_ascii}

(*
****BistroMath core function****
coordinates are recalculated as needed on basis wUserAxisSign[mAxis]
twStepSize is calculated from coordinates
twVector_ICD_cm is setup
reference is loaded
*)
{04/06/2015  make curvestring again when wMultiScanRefOk=true}
{23/08/2015
  dependencies on correct data in Angle moved from SetScanType to here
  twBSSD_cm removed}
{27/08/2015 strip spaces from wMeasDeviceInfo.twDeviceName}
{29/09/2015 twCurveIDstring:= MakeFilename(True,True,True); added}
{15/12/2015 for multiscanref files the servoname is copied to the detectorname}
{19/03/2016 mathematical approach to shortest distance to origin}
{14/04/2016 local var TankMapping not used consequently}
{02/08/2016 makecurvename}
{16/08/2016 set twCurveIDstring *after* FastScan is done!}
{07/12/2016 if not autoloadref, then try to take reforg}
{14/01/2017 rescaling of extreme data values}
{20/03/2017 range checking and div0 protection}
{29/03/2017 wAutoShiftCm}
{23/10/2017 ScanAngle reviewed, explicite test for 90 degrees}
{03/11/2017 for angle scans: consistent scanangle calculation in OmniPro v6 GTABUD coordinate system}
{12/01/2018 added twAbsNormConfig to note used info from modlist}
{22/02/2018 apply StrTrim to Linac instead of removing anything surrounded by illegal characters}
{26/02/2018 ... but still clip at first space}
{21/08/2018 tmpCoord.m[mAxis]:= twVector_ICD_cm[Stop].m[mAxis]-twVector_ICD_cm[Start].m[mAxis] was calculated before twVector_ICD_cm[nn] was set}
{11/09/2018 swapped relation between scanangle 45/135 and GA/TA}
{02/11/2018 wGenericToPDD handling improved}
{16/11/2018 rescaling for profiles improved}
{16/05/2020 applied FMultiScanCapable}
{27/08/2020 twMaxPosCm, twMaxValue}
{06/10/2020 fundamentals alternative}
{12/10/2020 chop off zero-value points at the end}
{13/10/2020 detection of electrons}
{23/10/2020 chop off only for vertical scans}
{15/12/2020 removed strange disfunctional statement dating from 2017
            ...else if TakeReferenceOrg then TakeReferenceOrg... out of ...if FAutoLoadRef then LoadReference...
            TakeReferenceOrg should be called by LoadReference only}
{07/03/2021 detect fcMRlinac field type, twcDefaultSSD_MRcm}
function TWellhoferData.PrepareProfile: Boolean;
var i,j        : Integer;
    mAxis      : twcMeasAxis;
    e,vmax     : twcFloatType;
    varAxisHex : Byte;
    MeasAxisID : twcTankAxisID;
    tmpCoord   : twcCoordinate;
    TankMapping: twcMeasAxisStg;
    v          : twcStartStopType;

  function NotOnCAX(ACoordinate:twcCoordinate): Boolean;
  begin
  Result:= (Abs(ACoordinate.m[Inplane]-0)>0.5) or (Abs(ACoordinate.m[Crossplane]-0)>0.5);
  end;

  function FanTangent(ACoordinate:twcCoordinate): twcFloatType;
  begin
  try
    Result:= (wSource[dsMeasured].twSSD_cm+ACoordinate.m[Beam])/Max(0.01,VectorLength(ACoordinate.m[Inplane],ACoordinate.m[Crossplane]));
   except
    Result:= 0;
   end;
  end;

  function CalcRefPos(aAxis:twcMeasAxis): twcFloatType;
  var deltaZ: twcFloatType;
  begin
  if aAxis=Beam then
    Result:= 0
  else with wSource[dsMeasured] do
    try
      deltaZ:= twVector_ICD_cm[Stop ].m[Beam ]-twVector_ICD_cm[Start].m[Beam];
      Result:= twVector_ICD_cm[Start].m[aAxis];
      if Abs(deltaZ)>0.5 then
         Result:= Result - twVector_ICD_cm[Start].m[Beam]*(twVector_ICD_cm[Stop].m[aAxis]-twVector_ICD_cm[Start].m[aAxis])/deltaZ;
     except
      Result:= 0;
     end;
  end;

  function EqualPos(ACoordinate:twcCoordinate;
                    m1,m2      :twcMeasAxis): Boolean;
  begin
  Result:= abs(abs(ACoordinate.m[m1])-abs(ACoordinate.m[m2]))<0.1;
  end;

begin
Inc(FActiveCnt);
with wCurveInfo do
  begin
  if not FMultiScanCapable then
    begin
    wMultiScanNr := 1;
    wMultiScanMax:= 1;
    end;
  TankMapping:= wMeas2TankMapping;
  Linac      := CharSetTrimAll(csComplete-['a'..'z','A'..'Z','0'..'9','-','_']-[chSpace],Linac).TrimLeft;
  if Length(Linac)=0 then
    Linac    := 'linac';
  if (Length(TankMapping)<3) or (Pos(TankMapping,twcMeasAxisPermutations)=0) then
    TankMapping:= twcMeasAxisStandard;
  for mAxis:= Inplane to Beam do
    MeasAxisID[mAxis]:= twcTankAxisStandard[Pos(TankMapping[Succ(Ord(mAxis))],twcMeasAxisStandard)];
  if ScanAngle<0 then
    ScanAngle:= ScanAngle+180;
  wSource[dsCalculated].twValid:= False;
  with wSource[dsMeasured] do
    begin
    twDevice        := Linac;
    twScanTypeString:= twDesTypeString;
    twFileName      := FileName;
    twValid         := FParseOk;
    twIsDiagonal    := False;
    twScanAngle     := ScanAngle;
    twIsGamma       := False;
    twIsRelative    := False;
    twInFieldAreaOk := False;
    twFastScan      := False;
    twFilterPoints  := 0;
    twLinSlope      := 0;
    twAbsNormPosCm  := 0;
    twAbsNormConfig := False;
    twLocalPeak     := False;
    twRefNormFactor := 1;
    twScanDevice    := '';
    vmax            := twData[0];
    if (twBeamInfo.twBModality='E') and wFieldTypeDetection[fcElectron] then
      begin
      twSetFieldType:= fcElectron;
      FPenumbraH    := wEPenumbraH;
      FPenumbraL    := wEPenumbraL;
      end
    else
      begin
      twSetFieldType:= fcStandard;
      FPenumbraH    := wXPenumbraH;
      FPenumbraL    := wXPenumbraL;
      end;
    if TankMapping<>twcMeasAxisStandard then
      begin
      if (ScanType<>snPDD) and (ScanType<>snGenericHorizontal) then
        twDesScanType:= snUndefined;
      for i:= twDataFirst to twDataLast do
        begin
        tmpCoord:= twCoordinates[i];
        for mAxis:= Inplane to Beam do
          twCoordinates[i].c[MeasAxisID[mAxis]]:= tmpCoord.m[mAxis];
        end;
      end;
    for mAxis:= Inplane to Beam do                                              //wUserAxisSign is used to swap any axis to match the users display with other applications
      begin
      if wUserAxisSign[mAxis]<0 then
        for i:= twDataFirst to twDataLast do
          twCoordinates[i].m[mAxis]:= -twCoordinates[i].m[mAxis];
      if wAutoShiftCm[mAxis]<>0 then
        for i:= twDataFirst to twDataLast do
          twCoordinates[i].m[mAxis]:= twCoordinates[i].m[mAxis]+wAutoShiftCm[mAxis];
      end;
    twVector_ICD_cm[Start]:= twCoordinates[twDataFirst];
    twVector_ICD_cm[Stop ]:= twCoordinates[twDataLast ];
    for mAxis:= Inplane to Beam do                                              //will be used to derive scanangle in OmniPro v6 GTABUD coordinate system
      tmpCoord.m[mAxis]:= twVector_ICD_cm[Stop].m[mAxis]-twVector_ICD_cm[Start].m[mAxis];
    twSSD_cm         := Max(1,twSSD_cm);
    if wFieldTypeDetection[fcMRlinac] and (Pos(Linac,wMRlinacTUlist)>0) then
      twSetFieldType:= fcMRlinac;
    FDefaultSSDcm    := ifthen(twSetFieldType=fcMRlinac,twcDefaultSSD_MRcm,twcDefaultSSDcm);
    twSDD2SSDratio   := ifthen(abs(twVector_ICD_cm[Start].m[Beam]-twVector_ICD_cm[Stop].m[Beam])<0.1,1+twVector_ICD_cm[Start].m[Beam]/Max(10,Abs(twSSD_cm)),1);
    twPosScaling     := Max(0.1,ifthen(wScaleSDD2SSD,Max(1,twSDD2SSDratio),1)*ifthen(wScale2DefaultSSD,twSSD_cm/Max(10,FDefaultSSDcm),1));
    twResampled      := False;
    twSelf           := dsMeasured;
    twAlignedTo      := dsMeasured;
    twConfidenceLimit:= 0;
    varAxisHex       := 0;
    twPosCmExportSign:= 1;
    for mAxis:= Inplane to Beam do
      begin                                                                     //set refpoint to first datapoint
      twDesVaryingAxis[mAxis]:= Abs(twVector_ICD_cm[Stop ].m[mAxis]-twVector_ICD_cm[Start].m[mAxis])>0.5;
      twPosCmExportSign      := twPosCmExportSign*ifthen(twDesVaryingAxis[mAxis],wUserAxisSign[mAxis],1);
      wRefpoint.m[mAxis]     := ifthen(twDesVaryingAxis[mAxis],0,twVector_ICD_cm[Start].m[mAxis]);
      Inc(varAxisHex,ifthen(twDesVaryingAxis[mAxis],1 shl ord(mAxis),0));
      end;
    if twDesScanType in [snFreeScan,snAngle,snUndefined,snGenericHorizontal,snGenericProfile] then
      begin
      case varAxisHex of
        1: SetScanType(snGT);
        2: SetScanType(snAB);
        3: if LineDistance2Origin(twVector_ICD_cm[Start].m[Inplane],twVector_ICD_cm[Start].m[Crossplane],
                                  twVector_ICD_cm[Stop ].m[Inplane],twVector_ICD_cm[Stop ].m[Crossplane])<0.5 then
             SetScanType(snAngle)
           else
             SetScanType(snFreeScan);
        4: SetScanType(snPDD);
        5..7: if NotOnCAX(twVector_ICD_cm[Start]) and NotOnCAX(twVector_ICD_cm[Stop ]) and
                (FanTangent(twVector_ICD_cm[Start])=FanTangent(twVector_ICD_cm[Stop ])) then
                SetScanType(snFanLine)
              else
                SetScanType(snFreeScan);
        else SetScanType(snFreeScan);
       end;
      twDesScanType:= ScanType;
      end;
     if twDesScanType=snAngle then
       begin
       if max(abs(twVector_ICD_cm[Start].m[Inplane]),abs(twVector_ICD_cm[Start].m[Crossplane]))>
          max(abs(twVector_ICD_cm[Stop ].m[Inplane]),abs(twVector_ICD_cm[Stop ].m[Crossplane])) then v:= Start
       else                                                                                          v:= Stop;
       if twVector_ICD_cm[v].m[InPlane]=0 then
         begin
         SetScanType(snAB);
         ScanAngle:= 0;
         end
       else
         try
           if tmpCoord.m[InPlane]=0 then
             ScanAngle:= 90
           else
             ScanAngle:= Round(ArcTan(tmpCoord.m[CrossPlane]/tmpCoord.m[InPlane])*1800/Pi)/10; {scanangle in OmniPro v6 GTABUD coordinate system}
           if ScanAngle<>0 then
             twDesScanType:= snAngle;
          except
           ScanAngle:= 0;
          end;
       if ScanAngle<0 then
         ScanAngle:= ScanAngle+180;
       i:= Round(ScanAngle);
       if i=45       then twDesTypeString:= 'Diagonal TA'
       else if i=135 then twDesTypeString:= 'Diagonal GA';
       end;
    if (twDesScanType in [snFreescan]) and (not twDesVaryingAxis[Beam]) then
      twDesScanType:= snGenericHorizontal;
    if (((twDesScanType in [snFreescan,snGenericProfile,snUndefined,snPDD]) and wGenericToPDD and (GetDistance(twVector_ICD_cm[Stop],twVector_ICD_cm[Start])>3)) or
            (twDesScanType=snFanLine)) then
      begin
      for mAxis:= Inplane to Beam do
        wRefPoint.m[mAxis]:= CalcRefPos(mAxis);
      if wGenericToPDD then
        begin
        twDesScanType  := snPDD;
        twDesTypeString:= ApplyAliasList('PDD');
        end;
      if twDesScanType in [snFreescan,snPDD] then
        begin
        for i:= twDataFirst to twDataLast do
          begin
          twCoordinates[i].m[Beam      ]:= DistanceToRefPoint(twCoordinates[i])*Sign(twCoordinates[i].m[Beam]);
          twCoordinates[i].m[Inplane   ]:= wRefPoint.m[Inplane   ];
          twCoordinates[i].m[Crossplane]:= wRefPoint.m[Crossplane];
          end;
        twVector_ICD_cm[Start]:= twCoordinates[twDataFirst];
        twVector_ICD_cm[Stop ]:= twCoordinates[twDataLast];
        end;
      end;
    case twDesScanType of
      snGT,snAB,snPDD: begin
                       mAxis:= twcMeasAxis(Ord(twDesScanType));
                       for i:= twDataFirst to twDataLast do
                         twPosCm[i]:= twCoordinates[i].m[mAxis]/twPosScaling;
                       end;
      snFanLine      : begin
                       for mAxis:= Inplane to CrossPlane do
                         wRefPoint.m[mAxis]:= twVector_ICD_cm[Start].m[mAxis];
                       for i:= twDataFirst to twDataLast do
                         twPosCm[i]:= DistanceToRefPoint(twCoordinates[i])*Sign(twCoordinates[i].m[Beam]);
                       end;
      snGenericHorizontal,
      snFreeScan     : begin
                       wRefPoint:= twVector_ICD_cm[Start];
                       for i:= twDataFirst to twDataLast do
                         twPosCm[i]:= DistanceToRefPoint(twCoordinates[i]);
                       end;
      snAngle        : for i:= twDataFirst to twDataLast do
                         twPosCm[i]:= wUserAxisSign[CrossPlane]*Sign(twCoordinates[i].m[Crossplane])*DistanceToRefPoint(twCoordinates[i]);
     end; {case}
    twStepSizeCm          := Max(0.0001,GetDistance(twCoordinates[twDataLast],twCoordinates[twDataFirst])/Max(1,Pred(twPoints)));
    FScanType             := twDesScanType;
    CheckDataOrdering;                                            {sets also twPosFirst and twPosLast}
    wMeasDeviceInfo.twDeviceName:= AnsiReplaceStr(wMeasDeviceInfo.twDeviceName,chSpace,'');
    if Pos('film',LowerCase(wDetectorInfo.twDetType))>0 then
      twFilmData:= True;
    if wResampleData then
      Resample(ResampleGridSize,dsMeasured,dsMeasured);
    if ScanType in twcVertScans then
      begin
      i:= twDataLast;
      j:= 0;
      while (i>twDataFirst) and (twData[i]=0) and twValid do
        begin
        SetNumPoints(dsMeasured,twPoints-1);                                    //chop off zero-value points at the end
        twValid:= (twPoints>=twcDefMinProfilePoints);                           //introduced for Varian Eclipse pdd's
        Dec(i);
        Inc(j);
        end;
      if j>0 then
       StatusMessage(Format('chopped off last zero values (%d point%s)',[j,ifthen(j=1,'','s')]));
      end;
    for i:= twDataFirst to twDataLast do
      if twData[i]>vmax then
        begin
        vmax    := twData[i];
        twMaxArr:= i;
        end;
    e:= 1;
    if (vmax>0) and (ScanType in twcHoriScans) and ((vmax<10) or (vmax>32000)) then
      begin
      e   := NiceStep(100/vmax);
      vmax:= e*vmax;
      end;
    if e<>1 then
      begin
      for i:= twDataFirst to twDataLast do
        twData[i]:= e*twData[i];
      StatusMessage(Format('%s data scaled with factor %0.3f',[twDataHistoryStg,e]));
      twDataHistoryStg:= Format('%0.3f*%s',[e,twDataHistoryStg]);
      end;
    twMaxPosCm:= twPosCm[twMaxArr];
    twMaxValue:= Max(twcMinNormVal,vmax);
    {$IFDEF WELLHOFER_DUMPDATA}
    DumpData('PrepareProfile');
    {$ENDIF}
    FastScan;
    twCurveIDstring:= MakeCurveName(False,True);                                //needs preliminary analysis of FastScan
    if twFileName='' then
      twFilename:= twCurveIDstring;
    SetArrayScanRefOk;                                                          //check whether this measurement accepts a array device data set as reference
    if FArrayScanRefOk and (wDetectorInfo.twDetName=twcDefUnknown) then
      wDetectorInfo.twDetName:= wMeasDeviceInfo.twDeviceName;
    end;
  ClearCurve(dsCalculated,True);
  ClearCurve(dsBuffer    ,True);
  if FAutoLoadRef then
    LoadReference                      //===================LoadReference==============
  else
    ClearCurve(dsReference,True);
  if Warning<>'' then
    StatusMessage(LastMessage);
  Result:= FParseOk and IsValid;
  end;
Dec(FActiveCnt);
end; {~prepareprofile}


{BistroMath assumes increasing position values}
procedure TWellhoferData.CheckDataOrdering(ASource:twcDataSource=dsMeasured);
var i,j: Integer;
    v  : twcFloatType;
    c  : twcCoordinate;
begin
with wSource[ASource] do
  begin
  if twPosCm[twDataLast]<twPosCm[twDataFirst] then
    begin
    i:= twDataFirst;
    j:= twDataLast;
    repeat
      v:= twPosCm[j];       twPosCm[j]      := twPosCm[i];       twPosCm[i]      := v;
      v:= twData[j];        twData[j]       := twData[i];        twData[i]       := v;
      c:= twCoordinates[j]; twCoordinates[j]:= twCoordinates[i]; twCoordinates[i]:= c;
      Inc(i);
      Dec(j);
    until i>=j;
    end;
  twFirstDataPosCm:= twPosCm[twDataFirst];
  twLastDataPosCm := twPosCm[twDataLast ];
  twScanLength    := twLastDataPosCm-twFirstDataPosCm;
  twStepSign      := Sign(twScanLength);
  end;
end; {checkdataordering}


//because TStrings is input, the data are in some ascii-format
{17/09/2020 introduction of FFrozen}
{16/11/2020 ADataTopLine}
function TWellhoferData.ReadData(AStringList :TStrings;
                                 ADataTopLine:Integer    =0;
                                 AFileFormat :twcFileType=twcUnknown): Boolean;
begin
Result:= (not FFrozen) and DualReadData(AStringList,nil,FileName,ADataTopLine,AFileFormat);
end; {~readdata}


{14/02/2016 replaced tmemorystream with tstringstream}
{17/09/2020 introduction of FFrozen}
{16/11/2020 ADataTopLine}
function TWellhoferData.ReadData(AStream     :TStream;
                                 ADataTopLine:Integer    =0;
                                 AFileFormat :twcFileType=twcUnknown): Boolean;
begin
Result:= (not FFrozen) and DualReadData(nil,AStream,FileName,ADataTopLine,AFileFormat);
end; {~readdata}


{17/09/2020 introduction of FFrozen}
{16/11/2020 ADataTopLine}
function TWellhoferData.ReadData(AFileName   :String;
                                 ADataTopLine:Integer    =0;
                                 AFileFormat :twcFileType=twcUnknown): Boolean;
begin
Result:= (not FFrozen) and DualReadData(nil,nil,AFileName,ADataTopLine,AFileFormat);
end; {~readdata}


{20/07/2016 moved to stream based versiom}
{17/09/2020 introduction of FFrozen}
function TWellhoferData.ReadRfb(AFileName:String): Boolean;
begin
Result:= (not FFrozen);
if Result then
  begin
  if (AFileName<>FLastMultiFile) and (GetFileType(AFileName)=twcWellhoferRfb) then
    begin
    wMultiScanMax := 1;
    FLastMultiFile:= AFileName;
    FFileName     := AFileName;
    end;
  Result:= ReadRfb(BinStream);
  end;
end;  {~readrfb}


(*
The binary rfb format is reconstructed from several sources (RFx2PTW/Bert van der Leije/DDHK Rotterdam and Radpy/Stephen Flounder) but has still some undocumented fields.
It is complicated by design and changed multiple times with different versions of OmniPro.
Thanks to Bert van der Leije for the major parts of this reconstruction. Additional reverse engineering done by TvS.
*)
{22/07/2015.
  Internal counter (ScanNr) starts at 0 for rfb.
  Global counter (wMultiScanNr) has base 1.
  This blocked stepping back to first profile. Now Scan[Pred(wMultiScanNr)] is taken.
  wMultiScanMax is reported as maximum number instead of ScanMax that here has also base 0.}
{15/12/2015 wMultiScanNr copied to twScanNr}
{22/07/2016 stream based version}
{29/07/2016 buggy data files found
  A file was found, exported from OmniPro v7.4.24, that contained an incorrect string length for a shortstring type.
  For these strings the first byte sets the string length. In the found case, containing the detector description, this value was much larger than the actual string length.
  As consequence too many bytes are assigned to the string, loosing data alignment completely.
  Now a marginal check is done to scan the string for ascii values lower than $0A. The string length will then be corrected to the last valid character.
  This proved to work in the found case.}
{16/03/2018 Finally word position for number of scans in subgroup found.
  After carefully summing all there might still be more scans to follow.
  Therefore at the very end after scanning the last group (if needed) ScanMax is incremented if there still is significant file space.}
{30/05/2020 added FMultiScanCapable}
{02/10/2020 set BinStreamType}
{06/10/2020 fundamentals alternative}
function TWellhoferData.ReadRfb(AStream:TStream): Boolean; {works also for tmemorystream}
{$IFDEF READRFB_TEST}
var StreamPos: Integer;
{$ENDIF}

  procedure ScanData(AScanNr:Integer);
  const m_ordering: array[1..3] of twcMeasAxis=(Crossplane,Inplane,Beam);
  var i,j,Version,
      n_ScanGroups,ScanGroup       : Integer;
      c                            : Char;
      t                            : twcTankAxis;
      ma                           : twcMeasAxis;
      mc                           : twcChannels;
      Stg,meas_type                : String;
      Read_SubHeader,End_SubHeader,
      AbsoluteDose                 : Boolean;
      ICD_offset,tmpRangeCoord     : twcCoordinate;
      ScanHex,b                    : Byte;
      n                            : Word;

    function byte_at_pointer: byte;
    begin
    try
      Result:= 0;
      AStream.Read(Result,1);
      AStream.Seek(-1,soFromCurrent);
     except
      Result  := 0;
      FParseOk:= False;
     end;
   {$IFDEF READRFB_TEST}
    StreamPos:= AStream.Position;
   {$ENDIF}
    end;

    function to_String(pre_incr:integer=0): string;
    var s  : ShortString;
        m,n: Byte;
    begin
    if pre_incr<>0 then
      AStream.Seek(pre_incr,soFromCurrent); {<length>[data]}
    try
      s:= '';
      m:= 0;
      AStream.Read(m,1);
      AStream.Seek(-1,soFromCurrent);
      AStream.Read(s,m+1);
      if m>0 then
        begin
        n:= 0;
        while n<m do
          begin
          Inc(n);
          if Ord(s[n])<Ord(chLF) then
            begin
            AStream.Seek(n-m-1,soFromCurrent);
            m:= n;
            s[0]:= Char(m);
            end;
          end;
        Result:= Trim(s);
        end
      else
        Result:= '';
     except
      Result  := '';
      FParseOk:= False;
     end;
   {$IFDEF READRFB_TEST}
    StreamPos:= AStream.Position;
   {$ENDIF}
    end;

    function to_String2(pre_incr:integer=0): string;
    var s: ShortString;
        m: Byte;
    begin
    s:= '';
    m:= 0;
    if pre_incr<>0 then
      AStream.Seek(pre_incr,soFromCurrent); {<length>[data]}
    AStream.Read(m,1);
    try
      AStream.Read(s,m+1);
      SetLength(s,m);         {repair length}
      Result:= Trim(s);
     except
      Result  := '';
      FParseOk:= False;
     end;
   {$IFDEF READRFB_TEST}
    StreamPos:= AStream.Position;
   {$ENDIF}
    end;

    function to_int(pre_incr:integer=0): smallint;
    begin
    Result:= 0;
    if pre_incr<>0 then
      AStream.Seek(pre_incr,soFromCurrent); {<length>[data]}
    try
      AStream.Read(Result,Sizeof(Result));
     except
      FParseOk:= False;
     end;
    end;

    function to_word(pre_incr:integer=0): word;
    begin
    Result:= 0;
    if pre_incr<>0 then
      AStream.Seek(pre_incr,soFromCurrent); {<length>[data]}
    try
      AStream.Read(Result,Sizeof(Result));
     except
      FParseOk:= False;
     end;
   {$IFDEF READRFB_TEST}
    StreamPos:= AStream.Position;
   {$ENDIF}
    end;

    function to_longword(pre_incr:integer=0): LongWord;
    begin
    if pre_incr<>0 then
      AStream.Seek(pre_incr,soFromCurrent); {<length>[data]}
    try
      Result:= 0;
      AStream.Read(Result,Sizeof(Result));
     except
      Result := 0;
      FParseOk:= False;
     end;
   {$IFDEF READRFB_TEST}
    StreamPos:= AStream.Position;
   {$ENDIF}
    end;

    function to_double(pre_incr:integer=0;
                       scaling :Double =1;
                       default :Double =0): Double;
    begin
    if pre_incr<>0 then
      AStream.Seek(pre_incr,soFromCurrent); {<length>[data]}
    try
      Result:= 0;
      AStream.Read(Result,Sizeof(Result));
      Result:= Result*Scaling;
      if abs(Result)<1e-10     then Result:= 0
      else if abs(Result)>1e10 then Result:= Default;
     except
      Result  := 0;
      FParseOk:= False;
     end;
   {$IFDEF READRFB_TEST}
    StreamPos:= AStream.Position;
   {$ENDIF}
    end;

    //is remapped to ICB through m_ordering
    function get_CIB_double(Scaling:Double=0.1): twcCoordinate;
    var i: SmallInt;
    begin
    for i:= 1 to 3 do
      Result.m[m_ordering[i]]:= to_double(0,Scaling);
   {$IFDEF READRFB_TEST}
    StreamPos:= AStream.Position;
   {$ENDIF}
    end;

    procedure to_Meas_point(MeasIndex:Integer);
    var ma: twcMeasAxis;
    begin
    with wSource[dsMeasured] do
      begin
      twPosCm[MeasIndex]:= to_double(0,0.1);
      twData[MeasIndex] := to_double;
      for ma:= InPlane to Beam do
         twCoordinates[MeasIndex].m[ma]:= ICD_offset.m[ma]+twPosCm[MeasIndex]*tmpRangeCoord.m[ma];
      end;
   {$IFDEF READRFB_TEST}
    StreamPos:= AStream.Position;
   {$ENDIF}
    end;

  begin
  AStream.Seek(0,soFromBeginning);
  ScanNr      := AScanNr;
  ScanNrOk    := 0;
  Version     := 0;
  n_ScanGroups:= 0;
  ScanGroup   := 0;
  b           := 0;
  if FParseOk then
    begin
    Stg     := to_string;                                               {Version:6.3.14}
    FParseOk:= (Pos(rfbString1,Stg)=1);                                 {test again on identity for safety reasons}
    end;
  if FParseOk then
    begin
    Version     := StrToInt(Stg[9]+Stg[11]);
    n_ScanGroups:= to_int;                                              {70}
    ScanMax     := n_ScanGroups;                                        {there are at least n_Scangroups scans}
    repeat                                                              {while ([b]<>$FF) or ([b+1]<>$FF) move pointer}
      AStream.Read(b,1);
      if b<>$FF then
        begin
        AStream.Read(b,1);
        AStream.Seek(-1,soFromCurrent);
        end
      else
        AStream.Seek(-1,soFromCurrent);
    until b=$FF;
    AStream.Seek(4,soFromCurrent);
    Stg    := to_string2;                                               {CBeam}
    FParseOk:= Stg='CBeam';
    if not FParseOk then
      FParser.ErrorString:= 'no rfb-format';
    end;
  if FParseOk then with wSource[dsMeasured].twBeamInfo do
    begin
    BinStreamType := twcWellhoferRfb;
    Read_SubHeader:= True;
    repeat // until ScanGroup=n_ScanGroups
      Inc(ScanGroup);
      Linac                       := to_string;                         {U02: Elekta SL25-15}
      wSource[dsMeasured].twDevice:= Linac;
      twBEnergy                   := to_double(2);                      {10 [M(e)V]}
      case to_int of                                                    {Photon=0,Electron=1,Proton=2,Neutron=3,Cobalt=4,Isotope=5}
        0: twBModality:= 'X';
        1: twBModality:= 'E';
        2: twBModality:= 'P';
        3: twBModality:= 'N';
        4: twBModality:= 'C';
        5: twBModality:= 'I';
       end; {case}
      if Version<60 then
        begin
        twBFieldHi[fInplane   ]:= to_double(2,0.1);                     {200 [*0.1 mm]}
        twBFieldHi[fCrossplane]:= to_double(2,0.1);                     {200 [*0.1 mm]}
        end;
      case to_int of                                                    {0}
        0:  twBWedgeType:= 'Hard';
        1:  twBWedgeType:= 'Dynamic';
        2:  twBWedgeType:= 'Enhanced';
        3:  twBWedgeType:= 'Virtual';
        4:  twBWedgeType:= 'Soft';
       else twBWedgeType:= '';
       end;
      twBWedge                    := to_int(2);                         {0 [degrees]}
      twBGantry                   := to_int(2);                         {0 [degrees]}
      twBCollimator               := to_int(2);                         {0 [degrees]}
      wSource[dsMeasured].twSSD_cm:= to_double(2,0.1,FDefaultSSDcm);    {1000 [mm]}
      twBSAD_cm                   := to_double(2,0.1,100);              {1000 [mm]}
      twBApplicator               := to_string;                         {Undefined}
      if FParseOk and ((Length(twBApplicator)=0) or (twBApplicator[1]<>#0)) then
        begin//if Version > 50 then begin
        if (twBApplicator='No Applicator') or (twBApplicator='Undefined') then
          twBApplicator:= '';
        case to_int of                                                  {1}
          0:  twBMedium:= 'air';
          1:  twBMedium:= 'water';
          2:  twBMedium:= 'film'
         else twBMedium:= 'unknown';
         end; {case}
        end {> v50}
      else {<= v50}
        twBApplicator:= '';
      if FParseOk and (Version>53) then with wGeneralInfo do
        begin
        twClinic               := to_string;                            {Radiotherapie}
        twAddress              := to_string;                            {UMC}
        twTelephone            := to_string;                            {}
        twEmail                := to_string;                            {}
        twBFieldLo[fInplane   ]:= to_double(2,0.1);                     {-200 [*0.1 mm]}
        twBFieldHi[fInplane   ]:= to_double(2,0.1);                     {200 [*0.1 mm]}
        twBFieldLo[fCrossplane]:= to_double(2,0.1);                     {-200 [*0.1 mm]}
        twBFieldHi[fCrossplane]:= to_double(2,0.1);                     {200 [*0.1 mm]}
        twBGantryScale         := twcGantrySetup(to_int);               {0}
        end; {> v53}
      //--- begin binnenloop (scangroup)
      repeat
        if Read_SubHeader and FParseOk then
          begin
          repeat                                                        {while ([b]<>$80) and (([b]<>$FF) or (b+1]<>$FF)) move pointer}
            n            := to_word;
            End_SubHeader:= (n>=$8000);
            if (n>0) and (not End_SubHeader) then
              Inc(ScanMax,Pred(n));                                     {n-1 because initial value of ScanMax is n_ScanGroups}
          until End_SubHeader;
          FParseOk:= AStream.Position<AStream.Size;
          if FParseOk and (n=$ffff) then
            begin
            meas_type:= to_string2(2);
            FParseOk := meas_type<>'CSinglePointCurve';
            if not FParseOk then
              FParser.ErrorString:= 'single point curves are not supported';
            end; {FParseOk}
          end; {sub_header}
        // -------- start of scan ---------
        if FParseOk then with wCurveInfo,wDetectorInfo,wSource[dsMeasured] do
          begin
          Inc(ScanNrOk);
          twMeasDateTime  := unixtodatetime(max(8*3600,to_longword)-8*3600);       // date, why 7 or 8 hours subtracted??
          twMeasTime      := FormatDateTime('dd-mmm-yyyy hh:nn',twMeasDateTime);
          twDesModDateTime:= unixtodatetime(max(8*3600,to_longword)-8*3600);       // date, why 7 or 8 hours subtracted??
          twDesModTime    := FormatDateTime('dd-mmm-yyyy hh:nn',twDesModDateTime);
          b:= byte_at_pointer;                                                  {do not advance pointer, value generally should be 2}
          case b of
            1:  twDetQuantity:= 'Relative optical density';
            2:  twDetQuantity:= 'Relative dose';
            3:  twDetQuantity:= 'Relative ionisation';
            4:  twDetQuantity:= 'Absolute dose';
            5:  twDetQuantity:= 'Charge'
           else twDetQuantity:= 'Unknown';
           end; {case}
          AbsoluteDose:= (b=4) or (meas_type='CSinglePointCurve');
          AStream.Seek(1,soFromCurrent);
          if Version>50 then
            begin
            twDetRadius_cm:= to_double;
            try
              while twDetRadius_cm>1 do
                twDetRadius_cm:= twDetRadius_cm/10;
             except
              twDetRadius_cm:= 0.5;
             end;
            twCalFactor:= to_double;
            if Version>51 then
              begin
              twTemperature_C:= to_double;
              twPressure_hPa := to_double;
              Stg            := to_string;                         {cal date}
              to_double;                                           {unknown}
              end; {versie>51}
            end
          else
            begin   {versie<=50}
            AStream.Seek(16,soFromCurrent);                        {move pointer 16 bytes}
            end;
          twDetName       := to_string;
          case to_int of
            1:  twDetType := 'Single diode';
            2:  twDetType := 'LDA-11';
            3:  twDetType := 'LDA-25';
            4:  twDetType := 'Ion chamber (cylindrical)';
            5:  twDetType := 'Ion chamber (plane parallel)';
            6:  twDetType := 'Stereotaxie';
            7:  twDetType := 'Film';
            8:  twDetType := 'CA24';
            9:  twDetType := 'BIS-2G';
           else twDetType :=  '';
           end; {case}
          twDesOperator   := to_string;
          twDesMeasComment:= trim(to_String);
          if AbsoluteDose then
            begin
            AStream.Seek(6,soFromCurrent);                         {move pointer 6 bytes}
            i:= to_int;
            CheckSize(dsMeasured,i);
            AStream.Seek(2,soFromCurrent);                         {move pointer 2 bytes}
            if Version>60 then
              AStream.Seek(291,soFromCurrent);    {move pointer 291 bytes}
            // voor absdose y,x,z,refy,refx,refz
            twVector_ICD_cm[Start]:= get_CIB_double;
            twVector_ICD_cm[Stop ].m[CrossPlane  ]                 := to_double(0,0.1);
            twVector_ICD_cm[Stop ].m[InPlane     ]                 := to_double(0,0.1);
            if Version>60 then twVector_ICD_cm[Stop ].m[InPlane]   := to_double(0,0.1); // -y
            twVector_ICD_cm[Stop ].m[Beam        ]                 := to_double(0,0.1);
            if Version>60 then twVector_ICD_cm[Stop ].m[CrossPlane]:= to_double(0,0.1); // -x
            twTemperature_C                                        := to_double;
            twPressure_hPa                                         := to_double;
            with wSource[dsMeasured] do
              while i>0 do
                begin
                Dec(i);
                twPosCm[i]:= 0;
                twData[i] := to_double;
                end;
            if byte_at_pointer>0 then
              AStream.Seek(2,soFromCurrent)                                     {move pointer 2 bytes}
            else
              FParseOk:= False;
            end;
          end; {FParseOk}
          if (Version>53) then with wMeasDeviceInfo,wMeterInfo,wCurveInfo do
            begin
            for i:= 1 to 3 do
              if FParseOk then {axismapping: cross|inline|depth: z_neg=-3, y_neg=-2, x_neg=-1, x_pos=1, y_pos=2, z_pos=3}
                begin
                j:= to_int;
                if not InRange(Abs(j),1,3) then
                  FParseOk:= False
                else
                  try
                    c                                := Char(Ord('X')+Pred(Abs(j)));
                    twDeviceMappingICD[m_ordering[i]]:= c;
                    twDeviceDirXYZ.c[c]              := Sign(j);
                   except
                    FParseOk:= False;
                   end;
                end; {for}
            if FParseOk then
              begin
              twElSamples       := to_int;
              twDeviceSpeed_mm_s:= to_double(2);
              AStream.Seek(4,soFromCurrent);                                    {13; 6 ??, skip 4 bytes}
              for t:= X to Z do
                twDeviceOriginXYZ_cm.t[t]:= to_int/100-24;                      {x=2438; y=2425; z=3620 from bottom tank corner *0.1 [mm]}
              twDeviceIsocICD_cm:= get_CIB_double;
              twDeviceNormICD_cm:= get_CIB_double;
              for mc:= FieldCh to RefCh do twElChannels[mc].twNorm       := to_double;
              for mc:= FieldCh to RefCh do twElChannels[mc].twDarkCurrent:= to_double;
              for mc:= FieldCh to RefCh do twElChannels[mc].twHV         := to_double;
              for mc:= FieldCh to RefCh do twElChannels[mc].twGain       := to_int;
              for mc:= FieldCh to RefCh do twElChannels[mc].twRange      := to_string;
              twDeviceWaterSurface_cm                                    := to_double;
              AStream.Seek(4,soFromCurrent);                                    {skip 4 bytes}
              twElRefMin:= Round(to_double);
              twElRefMax:= Round(to_double);
              twElRefAvg:= Round(to_double);
              AStream.Seek(10,soFromCurrent);                                   {double(-1), int(4), skip 10 bytes}
              twDesNormalise:= to_double;
              AStream.Seek(8,soFromCurrent);                                    {curve offset, equal to detector offset, skip 8 bytes}
              twDesMeasComment := to_string;
              AStream.Seek(2,soFromCurrent);                                    {int(0), skip 2 bytes}
              for c:= 'A' to 'D' do
                twDeviceRefPosition_cm[c]:= get_CIB_double(0.1);
              AStream.Seek(10,soFromCurrent);                                   {skip 10 bytes}
              end;
            end
          else {versie<=53}
            AStream.Seek(ifthen(Version<52,8,10),soFromCurrent);                {move pointer 8 or 10 bytes}
          if FParseOk then with wSource[dsMeasured] do
            begin
            twVector_ICD_cm[Start]:= get_CIB_double;
            twVector_ICD_cm[Stop ]:= get_CIB_double;
            twScanLength := Max(0.1,GetDistance(twVector_ICD_cm[Start],twVector_ICD_cm[Stop]));
            ICD_offset   := twVector_ICD_cm[Start];
            ScanHex      := 0;
            for ma:= Inplane to Beam do
              if Abs(twVector_ICD_cm[Start].m[ma]-twVector_ICD_cm[Stop].m[ma])>0.5 then
                begin
                Inc(ScanHex,1 shl Ord(ma));
                ICD_offset.m[ma]:= 0;
                end;
            case ScanHex of
              1:  Stg:= 'inline';
              2:  Stg:= 'crossline';
              3:  Stg:= 'diagonal';
              4:  Stg:= 'pdd';
             else Stg:= 'freescan';
             end;
            if ScanHex<4 then
              Stg:= Stg+Format(' d=%0.1f cm',[ICD_offset.m[Beam]]);
            SetNumPoints(dsMeasured,to_int);
            FParseOk:= (twPoints>0);
            if FParseOk then
              begin
              for ma:= Inplane to Beam do
                tmpRangeCoord.m[ma]:= Abs(twVector_ICD_cm[Stop].m[ma]-twVector_ICD_cm[Start].m[ma])/twScanLength;
              for i:= 0 to Pred(twPoints) do
                to_Meas_point(i);                         {-----------------------read datapoints----------------------------}
              if (ScanNr=0) or (ScanNr=ScanNrOk) then
                StatusMessage(Format('s%2.2d/%3.3d, %s, %s%0.*f, %0.*fx%0.*f, %s',
                                     [ScanGroup,ScanNrOk,
                                      twMeasTime,
                                      twBModality,ifthen(twBEnergy<1,3,0),ifthen(twBEnergy<1,1000,1)*twbEnergy,
                                      ifthen(FieldGT_cm-Trunc(FieldGT_cm)>0.05,1,0),FieldGT_cm,ifthen(FieldAB_cm-Trunc(FieldAB_cm)>0.05,1,0),FieldAB_cm,
                                      Stg]) );
               end;
          end; {FParseOk}
        //------ end of scan ------
        ScanHex:= byte_at_pointer;                      {------ scan repeat signal: more scans follow when ScanHex>0 ------}
        if ScanHex=0 then  {prepare for next subgroup}
          begin
          Read_SubHeader:= True;
          if (Version>50) and (AStream.Position<AStream.Size) then
            begin
            AStream.Seek(4,soFromCurrent);                                      {move pointer 4 bytes}
            ScanHex:= byte_at_pointer;
            if (ScanHex>0) and (Version>63) and (meas_type='CTMRCurve') then
              Read_SubHeader:= False;
            end;
          end
        else {scanhex>0, more scans in same scangroup}
          begin
          if FParseOk then
            repeat                                                              {while ([b]<>$80) and (([b]<>$FF) or ([b+1]<>$FF)) move pointer}
              AStream.Read(b,1);
              if b=$80 then
                b:= $FF;
              if b<>$FF then
                begin
                AStream.Read(b,1);
                AStream.Seek(-1,soFromCurrent);
                end
              else
                AStream.Seek(-1,soFromCurrent);
            until (b=$FF) or (AStream.Size-AStream.Position<2);
          Read_SubHeader:= (byte_at_pointer=$FF);
          if not Read_SubHeader then
            AStream.Seek(1,soFromCurrent);                                      {move pointer 1 byte}
          end; {scanhex>0}
          if (ScanGroup=n_ScanGroups) and (ScanNrOk=ScanMax) and (AStream.Size-AStream.Position>200) then
            Inc(ScanMax);
      until (not FParseOk) or (ScanHex=0) or (ScanNrOk=ScanNr) or (AStream.Size-AStream.Position<2);  {when ScanNr is reached we have found the wanted curve}
      //---eind binnenloop (scangroup)
      if ScanNrOk=ScanNr then
        ScanGroup:= n_ScanGroups;
      if ScanGroup<n_ScanGroups then
        begin
        AStream.Read(b,1);
        while b<>$80 do
          AStream.Read(b,1);
        end;
    until (ScanGroup=n_ScanGroups) or (AStream.Size-AStream.Position<2) or (not FParseOk);
    FParseOk:= FParseOk and (ScanNr=0) or (ScanNrOk=ScanNr);
    end; {FParseOk}
  end;

begin
Result:= FParseOk and (not FFrozen);
if Result then
  begin
  Inc(FActiveCnt);
  SetDefaults;   {sets fparseok to false}
  FMultiScanCapable:= True;
  FParseOk         := True;
  FIdentity        := 'omnipro_rfb';
  FileFormat       := twcWellhoferRfb;
  BinStreamType    := FileFormat;
  if (wMultiScanNr>wMultiScanMax) or (wMultiScanMax<=1) then
    begin
    ScanData(0);
    wMultiScanMax:= ScanMax;
    end;
  wMultiScanNr:= EnsureRange(wMultiScanNr,1,wMultiScanMax);
  if FParseOk then
    begin
    ScanData(wMultiScanNr);
    wMultiScanMax:= Max(ScanMax,wMultiScanMax);
    ScanMax      := wMultiScanMax;
    end;
  wSource[dsMeasured].twOriginalFormat:= twcWellhoferRfb;
  wSource[dsMeasured].twScanNr        := ScanNr;
  wCurveInfo         .twDesScanType   := snUndefined;
  Result                              := ReadResults;
  Dec(FActiveCnt);
  end;
end;  {~readrfb}


(*
****BistroMath core function****
This function opens all supported data formats as either a stream or a filename reference.
When a format has a specialised object, this is initialised and used. This object is then combined with a specific
import procedure within the twellhoferdata object.
*)
{01/07/2015
  Introduction of FormatOk en wMultiScanStep to avoid needless attempts
  with alternative file formats and to overcome illegal scans in a multi scan file.}
{11/08/2015 Cycle through multiple scan file}
{21/08/2015 SNA added}
{01/09/2015
  Use of TWellhoferData.ParseData -> TWellhoferData.EvaluateFiletype -> FileFormat
  reduces number of useless trials significantly.}
{09/12/2015 SharedParser}
{15/12/2015 AStream}
{14/02/2016 replaced tmemorystream with tstringstream}
{13/08/2016 ScanMax:= 0 for omnipro-types}
{15/12/2017 wMultiScanLooping}
{10/02/2020 pass FRefOrg2D_OriVal}
{17/09/2020 introduction of FFrozen}
{09/10/2020 added Eclipse}
{16/11/2020 ADataTopLine}
//User either StringSteam/BinStream or AFileName.
//wMultiScanNr is used
function TWellhoferData.DualReadData(AStringList :TStrings;
                                     AStream     :TStream;
                                     AFileName   :String;
                                     ADataTopLine:Integer    =0;
                                     AFileFormat :twcFileType=twcUnknown): Boolean;
var Wms     : TWmsData;
    Pips    : TPipsProfiledata;
    Schuster: TSchusterProfiledata;
    Rfa     : TRfaProfileData;
    Hdf     : THdfProfileData;
    Cms     : TCmsProfileData;
    w2CAD   : Tw2CAD_data;
    Eclipse : TEclipseData;
    SNA     : TICprofiler_ascii;
    i       : Integer;
begin
Inc(FActiveCnt);
FParseOk:= CheckBlackList(AFileName) and (not FFrozen);
if FParseOk then
  begin
  ClearCurve(dsMeasured ,True);
  ClearCurve(dsReference,True);
  ResetAliasList;
  ScanNr:= wMultiScanNr;
  IsFile:= (AStringList=nil) and (AStream=nil);
  if not FParser.Preloaded then
    FParser.Clear;
  if IsFile and (AFileFormat=twcUnknown) then
    AFileFormat:= GetFileType(AFileName);
  FParseOk:= (AFileFormat in twcInherentFormats+[twcUnknown]);
  if FParseOk then
    begin
    if AFileFormat in [twcWellhoferRfb] then
        begin
        if IsFile                        then FParseOk:= ReadRfb(AFileName)
        else if AStream is TMemoryStream then FParseOk:= ReadRfb(AStream)
          else                                FParseOk:= False;
        end
    else
      begin
      if IsFile then
        begin  {using dualreaddata here will cause loop with stack overflow}
        if wTryBinaryOnly      then SetDefaults {=> parse_ok:= false, no succes...}
        else                        FParseOk:= inherited ReadData(AFileName  ,ADataTopLine,AFileFormat);
        end
      else
        if assigned(AStream)   then FParseOk:= inherited ReadData(AStream    ,ADataTopLine,AFileformat)
        else                        FParseOk:= inherited ReadData(AStringList,ADataTopLine,AFileformat);
      end;
   if not FParseOk then
     AFileFormat:= FileFormat
   else
     begin
     wMultiScanNr := ScanNr;
     wMultiScanMax:= ScanMax;
     end;
   end;
  if (not FParseOk) and (AFileFormat in [twcWTX,twcWDA,twcUnknown]) then
    begin
    Wms         := TWmsData.Create(FParser);
    Wms.LogLevel:= LogLevel;
    FParseOk    := Wms.DualReadData(AStringList,AStream,AFileName,ADataTopLine,AFileFormat) and ImportWmsProfile(Wms);
    if Wms.FormatOk then
      begin
      AFileFormat:= Wms.FileFormat;
      if not FParseOk then
        FileFormat:= twcUnknown;
      end;
    Wms.TransferLastMessage(FLastMessage);
    try
      FreeAndNil(Wms);
     except
      ExceptMessage('WH.DualReadData:Wms!');
     end;
    end; {wms}
  if (not FParseOk) and (AFileFormat in [twcRFA_ascii,twcUnknown]) then
    begin
    Rfa       := TRfaProfileData.Create(FParser);
    Rfa.ScanNr:= wMultiScanNr;
    if AFileName<>FLastMultiFile then
        begin
        FLastMultiFile:= AFileName;
        Rfa.ScanMax   := 0;
        end
      else Rfa.ScanMax:= wMultiScanMax;
    Rfa.FileTime:= FileTime;
    Rfa.LogLevel:= LogLevel;
    i           := -1;
    repeat
      FParseOk:= Rfa.DualReadData(AStringList,AStream,AFileName,ADataTopLine,AFileFormat) and ImportRfaProfile(Rfa);
      if (not FParseOk) and wMultiScanLooping then
        begin
        Rfa.ScanNr:= Rfa.ScanNr+wMultiScanStep;
        if Rfa.ScanMax>1 then
          Rfa.ScanNr:= Succ((Rfa.ScanNr+Rfa.ScanMax-1) mod Rfa.ScanMax);
        if i=-1 then i:= Rfa.ScanMax
        else         Dec(i);
        end;
    until FParseOk or (not wMultiScanLooping) or (not Rfa.FormatOk) or (Rfa.ScanNr<1) or (Rfa.ScanNr>Rfa.ScanMax) or (i<=0);
    if Rfa.FormatOk then
      begin
      AFileFormat  := Rfa.FileFormat;
      wMultiScanMax:= Rfa.ScanMax;
      wMultiScanNr := EnsureRange(Rfa.ScanNr,1,wMultiScanMax);
      if not FParseOk then
        FileFormat:= twcUnknown;
      end;
    Rfa.TransferLastMessage(FLastMessage);
    try
      FreeAndNil(Rfa);
     except
      ExceptMessage('WH.DualReadData:Rfa!');
     end;
    end; {rfa}
  if (not FParseOk) and (AFileFormat in [twcMccProfile,twcUnknown]) then
    begin
    if not assigned(FMcc) then
      FMcc:= TMccProfileData.Create(FParser,AFileName,BinStream,AFileName,FRefOrg2D_OriVal);
    FMcc.ScanNr  := wMultiScanNr;
    FMcc.FileTime:= FileTime;
    FMcc.LogLevel:= LogLevel;
    i            := -1;
    repeat
      FParseOk:= FMcc.DualReadData(AStringList,AStream,AFileName,ADataTopLine,AFileFormat) and ImportMccProfile(FMcc);
      if (not FParseOk) and wMultiScanLooping then
        begin
        FMcc.ScanNr:= FMcc.ScanNr+wMultiScanStep;
        if FMcc.ScanMax>1 then
          FMcc.ScanNr:= Succ((FMcc.ScanNr+FMcc.ScanMax-1) mod FMcc.ScanMax);
        if i=-1 then i:= FMcc.ScanMax
        else         Dec(i);
        end;
    until FParseOk or (not wMultiScanLooping) or (not FMcc.FormatOk) or (FMcc.ScanNr<1) or (FMcc.ScanNr>FMcc.ScanMax) or (i<=0);
    if FMcc.FormatOk then
      begin
      AFileFormat  := FMcc.FileFormat;
      wMultiScanMax:= FMcc.ScanMax;
      wMultiScanNr := EnsureRange(FMcc.ScanNr,1,wMultiScanMax);
      if not FParseOk then
        FileFormat:= twcUnknown;
      end;
    FMcc.TransferLastMessage(FLastMessage);
    end; {mcc}
  if (not FParseOk) and (AFileFormat in [twcICprofilerAscii,twcUnknown]) then
    begin
    SNA          := TICprofiler_ascii.Create(FParser);
    SNA.ScanNr   := wMultiScanNr;
    SNA.FileTime := FileTime;
    SNA.LogLevel := LogLevel;
    i            := -1;
    repeat
      FParseOk:= SNA.DualReadData(AStringList,AStream,AFileName,ADataTopLine,AFileFormat) and ImportSNAProfile(SNA);
      if (not FParseOk) and wMultiScanLooping then
        begin
        SNA.ScanNr:= SNA.ScanNr+wMultiScanStep;
        if SNA.ScanMax>1 then
          SNA.ScanNr:= Succ((SNA.ScanNr+SNA.ScanMax-1) mod SNA.ScanMax);
        if i=-1 then i:= SNA.ScanMax
        else         Dec(i);
        end;
    until FParseOk or (not wMultiScanLooping) or (not SNA.FormatOk) or (SNA.ScanNr<1) or (SNA.ScanNr>SNA.ScanMax) or (i<=0);
    if SNA.FormatOk then
      begin
      AFileFormat  := SNA.FileFormat;
      wMultiScanMax:= SNA.ScanMax;
      wMultiScanNr := EnsureRange(SNA.ScanNr,1,wMultiScanMax);
      if not FParseOk then
        FileFormat:= twcUnknown;
      end;
    SNA.TransferLastMessage(FLastMessage);
    try
      FreeAndNil(SNA);
     except
      ExceptMessage('WH.DualReadData:SNA!');
     end;
    end; {sna}
  if (not FParseOk) and (AFileFormat in [twcCmsProfile,twcUnknown]) then
    begin
    Cms         := TCmsProfileData.Create(FParser);
    Cms.LogLevel:= LogLevel;
    FParseOk    := Cms.DualReadData(AStringList,AStream,AFileName,ADataTopLine,AFileFormat) and ImportHdfProfile(Cms);
    if Cms.FormatOk then
      AFileFormat:= Cms.FileFormat;
    Cms.TransferLastMessage(FLastMessage);
    try
      FreeAndNil(Cms);
     except
      ExceptMessage('WH.DualReadData:Cms!');
     end;
    end; {cms}
  if (not FParseOk) and (AFileFormat in [twcW2CAD,twcUnknown]) then
    begin
    w2CAD:= Tw2CAD_data.Create(FParser);
    if AFileName<>FLastMultiFile then
      begin
      FLastMultiFile:= AFileName;
      w2CAD.ScanMax := 0;
      end
    else
      w2CAD.ScanMax:= wMultiScanMax;
    w2CAD.ScanNr  := wMultiScanNr;
    w2CAD.LogLevel:= LogLevel;
    w2CAD.FileTime:= FileTime;
    repeat
      FParseOk:= w2CAD.DualReadData(AStringList,AStream,AFileName,ADataTopLine,AFileFormat) and ImportW2CADProfile(w2CAD);
      if not FParseOk then
        w2CAD.ScanNr:= w2CAD.ScanNr+wMultiScanStep;
    until FParseOk or (not w2CAD.FormatOk) or (w2CAD.ScanNr<1) or (w2CAD.ScanNr>w2CAD.ScanMax);
    if w2CAD.FormatOk then
      begin
      AFileFormat  := w2CAD.FileFormat;
      wMultiScanMax:= w2CAD.ScanMax;
      wMultiScanNr := EnsureRange(w2CAD.ScanNr,1,wMultiScanMax);
      if not FParseOk then
        FileFormat:= twcUnknown;
      end;
    w2CAD.TransferLastMessage(FLastMessage);
    try
      FreeAndNil(w2CAD);
     except
      ExceptMessage('WH.DualReadData:Varian!');
     end;
    end; {w2CAD}
  if (not FParseOk) and (AFileFormat in [twcHdfProfile,twcUnknown]) then
    begin
    Hdf         := THdfProfileData.Create(FParser);
    Hdf.LogLevel:= LogLevel;
    FParseOk     := Hdf.DualReadData(AStringList,AStream,AFileName,ADataTopLine,AFileFormat) and ImportHdfProfile(Hdf);
    if Hdf.FormatOk then
      AFileFormat:= Hdf.FileFormat;
    try
      FreeAndNil(Hdf);
     except
      ExceptMessage('WH.DualReadData:Hdf!');
     end;
    end; {hdf}
  if (not FParseOk) and (AFileFormat in [twcEclipse,twcUnknown]) then
    begin
    Eclipse         := TEclipseData.Create(FParser);
    Eclipse.LogLevel:= LogLevel;
    FParseOk        := Eclipse.DualReadData(AStringList,AStream,AFileName,ADataTopLine,AFileFormat) and ImportEclipse(Eclipse);
    if Eclipse.FormatOk then
      AFileFormat:= Eclipse.FileFormat;
    try
      FreeAndNil(Eclipse);
     except
      ExceptMessage('WH.DualReadData:Eclipse!');
     end;
    end; {hdf}
  if (not FParseOk) and (AFileFormat in [twcPipsProfile,twcUnknown]) then
    begin
    Pips         := TPipsProfileData.Create(wPipsPixelCm,FParser);
    Pips.LogLevel:= LogLevel;
    FParseOk      := Pips.DualReadData(AStringList,AStream,AFileName,ADataTopLine,AFileFormat) and ImportPipsProfile(Pips);
    if Pips.FormatOk then
      AFileFormat:= Pips.FileFormat;
    Pips.TransferLastMessage(FLastMessage);
    try
      FreeAndNil(Pips);
     except
      ExceptMessage('WH.DualReadData:Pips!');
     end;
    end; {pips}
  if (not FParseOk) and (AFileFormat in [twcSchusterProfile,twcUnknown]) then
    begin
    Schuster         := TSchusterProfileData.Create(FParser);
    Schuster.LogLevel:= LogLevel;
    Schuster.FileTime:= FileTime;
    FParseOk         := Schuster.DualReadData(AStringList,AStream,AFileName,ADataTopLine,AFileFormat) and ImportSchusterProfile(Schuster);
    if Schuster.FormatOk then
      AFileFormat:= Schuster.FileFormat;
    Schuster.TransferLastMessage(FLastMessage);
    try
      FreeAndNil(Schuster);
     except
      ExceptMessage('WH.DualReadData:Schuster!');
     end;
    end; {schuster}
  end; {blacklist}
wSource[dsMeasured].twOriginalFormat:= AFileFormat;
if FParseOk and IsFile then
  FFilename:= AFileName;
Result:= FParseOk and PrepareProfile;
Dec(FActiveCnt);
end; {~dualreaddata}


(* The general path followed is designed for both files and streams.
TWellhoferData.AdvReadData
  TWellhoferData.ReadData
     TWellhoferData.DualReadData
       TWellhoferData.ParseData
         or
       contributing_object.DualReadData
        contributing_object.ReadData
         contributing_object.ParseData
          contributing_object.CheckDataData
*)
//because TStrings is input, the data are in some ascii-format
{17/09/2020 introduction of FFrozen}
{18/09/2020 UnFreeze}
{16/11/2020 ADataTopLine}
function TWellhoferData.AdvReadData(AStringList     :TStrings;
                                    ADataTopLine    :Integer       =0;
                                    UnFreeze        :Boolean       =True;
                                    ResampleData    :Boolean       =False;
                                    CoordinateOrder :twcMeasAxisStg=twcMeasAxisStandard;
                                    AFileFormat     :twcFileType   =twcUnknown;
                                    ASourceReference:String        =''           ): Boolean;
begin
Result:= Unfreeze or (not FFrozen);
if Result then
  begin
  Freeze           := False;
  wResampleData    := ResampleData;
  wMeas2TankMapping:= CoordinateOrder;
  FFileName        := ASourceReference;
  StatusMessage(Format('Reading {%s} ',[ifthen(assigned(AStringList),AStringList.Strings[0],'')]),True,3);
  Result           := ReadData(AStringList,ADataTopLine,AFileFormat);
  end;
end; {~advreaddata}


{17/09/2020 introduction of FFrozen}
{18/09/2020 UnFreeze}
{16/11/2020 ADataTopLine}
function TWellhoferData.AdvReadData(AFileName       :String;
                                    ADataTopLine    :Integer       =0;
                                    UnFreeze        :Boolean       =True;
                                    IsBinaryFile    :Boolean       =False;
                                    ResampleData    :Boolean       =False;
                                    CoordinateOrder :twcMeasAxisStg=twcMeasAxisStandard;
                                    AFileFormat     :twcFileType   =twcUnknown            ): Boolean;
begin
Result:= Unfreeze or (not FFrozen);
if Result then
  begin
  Freeze           := False;
  FParser.PreLoaded:= False;
  wTryBinaryOnly   := IsBinaryFile;
  wResampleData    := ResampleData;
  wMeas2TankMapping:= CoordinateOrder;
  StatusMessage(Format('Reading %s... ',[AFileName]),True,3);
  Result           := ReadData(AFileName,ADataTopLine,AFileFormat);
  end;
end; {~advreaddata}


{15/12/2015}
{14/02/2016 replaced tmemorystream with tstringstream}
{17/09/2020 introduction of FFrozen}
{18/09/2020 UnFreeze}
{16/11/2020 ADataTopLine}
function TWellhoferData.AdvStreamData(AStream         :TStream;
                                      ADataTopLine    :Integer       =0;
                                      UnFreeze        :Boolean       =True;
                                      ResampleData    :Boolean       =False;
                                      CoordinateOrder :twcMeasAxisStg=twcMeasAxisStandard;
                                      AFileFormat     :twcFileType   =twcUnknown;
                                      ASourceReference:String        =''): Boolean;
begin
Result:= Unfreeze or (not FFrozen);
if Result then
  begin
  Freeze           := False;
  if (AStream=nil) and (AFileFormat in twcBinaryFormats) then
    AStream        := BinStream;
  wResampleData    := ResampleData;
  wMeas2TankMapping:= CoordinateOrder;
  if Length(ASourceReference)>0 then
    FFileName      := ASourceReference;
  Result           := ReadData(AStream,ADataTopLine,AFileFormat);
  end;
end; {~advreaddata}


{23/08/2015 dependencies on correct data in Angle moved to PrepareProfile}
{13/09/2018 mapping of snGenericProfile}
{08/06/2020 a profile cannot freely be set to another file type before import, make notice as function result}
function TWellhoferData.SetScanType(AScanType:twcScanTypes;
                                     ASource :twcDataSource=dsMeasured): Boolean;
begin
if (AScanType=snGenericProfile) then
  begin
  Result:= False;
  if wGenericToPDD then AScanType:= snPDD
  else                  AScanType:= snGenericHorizontal;
  StatusMessage('Data interpreted as '+ifthen(AScanType=snPDD,'PDD','profile'));
  end
else
  Result:= True;
with wCurveInfo do
  case AScanType of
    snGT     : begin
               twDesTypeString:= 'Inplane';
               ScanAngle      :=  90;
               wSource[ASource].twScanAngle:= ScanAngle;
               end;
    snAB     : begin
               twDesTypeString:= 'Crossplane';
               ScanAngle      :=   0;
               wSource[ASource].twScanAngle:= ScanAngle;
               end;
    snPDD    : twDesTypeString:= 'Depth Dose';
    snAngle  : twDesTypeString:= 'Angle';
    snFanline: twDesTypeString:= 'Fanline';
   else        twDesTypeString:= 'Freescan';
   end;
wCurveInfo.twDesScanType:= AScanType;
FScanType               := AScanType;
end; {~setscantype}


{09/12/2015 transfer of flastmessage}
{16/05/2020 applied FMultiScanCapable}
function TWellhoferData.ImportPipsProfile(Pips:TPipsProfileData): Boolean;
var i       : Integer;
    m,c     : twcMeasAxis;
    ofs     : twcFloatType;
begin
SetDefaults;
Result:= Pips<>nil;
if Result then if Pips.GetNumPoints>0 then
  with wSource[dsMeasured],twBeamInfo,wCurveInfo do
    begin
    FMultiScanCapable:= Pips.FMultiScanCapable;
    FIdentity        := Pips.Identity;
    FLastMessage     := Pips.FLastMessage;
    FOrgExtension    := Pips.DefaultExtension;
    twBEnergy        := Pips.Energy;
    twBModality      := 'X';
    ofs              := Pips.Scanlength/2;
    FileFormat       := Pips.FileFormat;
    FileTime         := Pips.FileTime;
    FFileName        := Pips.FileName;
    SetScanType(Pips.FScanType);
    if ScanType=snGT then begin  m:= InPlane;     c:= CrossPlane;  end
    else                  begin  m:= CrossPlane;  c:= InPlane;     end;
    twFilmdata       := False;
    twOriginalFormat := FileFormat;
    twMeasDateTime   := FileTime;
    twDesModDateTime := FileTime;
    twDevice         := Pips.Linac;
    DateTimeToString(twMeasTime,'yyyy-mm-dd hh:nn:ss',FileTime);
    SetNumPoints(dsMeasured,Pips.GetNumPoints);
    for i:= 0 to Pred(Pips.GetNumPoints) do
      begin
      twPosCm[i]              := i*Pips.StepSize-ofs;
      twCoordinates[i].m[m]   := twPosCm[i];
      twCoordinates[i].m[c]   := Pips.ScanPos;
      twCoordinates[i].m[Beam]:= 0;
      twData[i]               := Pips.GetProfile(i);
      end;
    twVector_ICD_cm[Start]:= twCoordinates[0];
    twVector_ICD_cm[Stop ]:= twCoordinates[Pred(Pips.GetNumPoints)];
    StatusMessage(Pips.LastMessage,True,2);
    end;
end; {~importpipsprofile}


{09/10/2020 new}
function TWellhoferData.ImportEclipse(Eclipse:TEclipseData): Boolean;
var i    : Integer;
    mAxis: twcMeasAxis;
begin
SetDefaults;
Result:= Eclipse<>nil;
if Result then if Eclipse.GetNumPoints>0 then
  with wSource[dsMeasured],twBeamInfo,wCurveInfo,wDetectorInfo do
    begin
    FMultiScanCapable:= Eclipse.FMultiScanCapable;
    FIdentity        := Eclipse.Identity;
    FLastMessage     := Eclipse.FLastMessage;
    FileTime         := Eclipse.FileTime;
    FOrgExtension    := Eclipse.DefaultExtension;
    twDesOperator    := Eclipse.EcOperator;
    twDesScanType    := snUndefined;
    FieldGT_cm       := Eclipse.EcField_cm[fInplane];
    FieldAB_cm       := Eclipse.EcField_cm[fCrossplane];
    twDesMeasComment := Eclipse.EcID+'/'+Eclipse.EcPlan+'/'+Eclipse.EcCourse;
    twMeasDateTime   := Eclipse.EcDate;
    twMeasTime       := Eclipse.EcDateString;
    twScanNr         := Eclipse.ScanNr;
    i                := Eclipse.GetNumPoints;
    if EClipse.EcModality.ToLower='mev' then
      twBModality:= 'E'
    else
      twBModality:= 'X';
    SetNumPoints(dsMeasured,i);
    with Eclipse do
      begin
      for i:= 0 to GetNumPoints-1 do
        begin
        twPosCm[i]:= FValues[i].X;
        for mAxis:= Inplane to Beam do
          twCoordinates[i].m[mAxis]:= FValues[i].X*FRange_Cm.c[FDataMapping[mAxis]];
        twData[i]                  := Eclipse.FValues[i].Y;
        end;
      end;
    end;
end; {~importeclipse}


{13/12/2015 Linac:= Aw2CAD.w2Comments;}
{15/12/2015 ScanNr copied to twScanNr}
{16/05/2020 applied FMultiScanCapable}
function TWellhoferData.ImportW2CADProfile(Aw2CAD:Tw2CAD_data): Boolean;
var mAxis: twcMeasAxis;
    i    : Integer;
begin
SetDefaults;
Result:= Aw2CAD<>nil;
if Result then if Aw2CAD.GetNumPoints>0 then
  with wSource[dsMeasured],twBeamInfo,wCurveInfo,wDetectorInfo do
    begin
    FMultiScanCapable:= Aw2CAD.FMultiScanCapable;
    FIdentity        := Aw2CAD.Identity;
    FLastMessage     := Aw2CAD.FLastMessage;
    FileTime         := Aw2CAD.FileTime;
    FOrgExtension    := Aw2CAD.DefaultExtension;
    twDetName        := Aw2CAD.w2DetectorInfo;
    twDetType        := Aw2CAD.w2DetectorType;
    twDesOperator    := Aw2CAD.w2Operator;
    twDesScanType    := snUndefined;
    FieldGT_cm       := Aw2CAD.w2Field_mm[fInplane]/10;
    FieldAB_cm       := Aw2CAD.w2Field_mm[fCrossplane]/10;
    twBWedge         := Aw2CAD.w2WedgeAngle;
    twBWedgeType     := Format('%s (%s)',[Aw2CAD.w2WedgeName,Aw2CAD.w2WedgeDir]);
    twDesMeasComment := Aw2CAD.w2Comments;
    Linac            := Aw2CAD.w2Comments;
    if Aw2CAD.w2Modality='PHO' then twBModality:= 'X'
    else                            twBModality:= Aw2CAD.w2Modality[1];
    SetNumPoints(dsMeasured,Aw2CAD.w2NumPoints);
    twDevice         := Linac;
    twMeasDateTime   := Aw2CAD.w2Date;
    twMeasTime       := Aw2CAD.w2DateString;
    twOriginalFormat := Aw2CAD.FileFormat;
    twSSD_cm         := Aw2CAD.w2SSD_mm/10;
    twScanNr         := Aw2CAD.ScanNr;
    with Aw2CAD do for i:= 0 to Pred(w2NumPoints) do
      begin
      for mAxis:= Inplane to Beam do
        twCoordinates[i].m[mAxis]:= w2Coordinates_mm[i].m[mAxis]/10;
      twData[i]                  := Aw2CAD.w2Values[i];
      end;
    end;
end; {~importw2cadprofile}


{09/12/2015 transfer of flastmessage}
{16/05/2020 applied FMultiScanCapable}
function TWellhoferData.ImportSchusterProfile(Schuster:TSchusterProfileData): Boolean;
var i             : Integer;
    m,c           : twcMeasAxis;
    ofs           : twcFloatType;
begin
SetDefaults;
Result:= Schuster<>nil;
if Result then if Schuster.GetNumPoints>0 then
  with wSource[dsMeasured],twBeamInfo,wCurveInfo do
    begin
    with DefaultFormatSettings do
      begin
      DateSeparator  := '/';
      ShortDateFormat:= 'd/M/yyyy';
      LongTimeFormat := 'hh:mm:ss';
      end;
    FMultiScanCapable:= Schuster.FMultiScanCapable;
    FIdentity        := Schuster.Identity;
    FLastMessage     := Schuster.FLastMessage;
    FOrgExtension    := Schuster.DefaultExtension;
    twBEnergy        := Schuster.Energy;
    twBModality      := Schuster.Modality;
    FileFormat       := Schuster.FileFormat;
    FileTime         := Schuster.FileTime;
    ofs              := Schuster.Scanlength/2;
    Linac            := Schuster.Linac;
    FFileName        := Schuster.FileName;
    SetScanType(Schuster.FScanType);
    if ScanType=snGT then begin  m:= InPlane;     c:= CrossPlane;  end
    else                  begin  m:= CrossPlane;  c:= InPlane;     end;
    twDevice         := Linac;
    twFilmdata       := False;
    twOriginalFormat := FileFormat;
    twMeasTime       := Schuster.ScanTime;
    twStepSizeCm     := Schuster.StepSize;
    try
      twMeasDateTime := StrToDateTime(twMeasTime,FormatSettings);
     except
      twMeasDateTime := FileTime;
     end;
    twDesModDateTime := twMeasDateTime;
    SetNumPoints(dsMeasured,Schuster.GetNumPoints);
    for i:= 0 to Pred(Schuster.GetNumPoints) do
      begin
      twPosCm[i]              := i*Schuster.StepSize-ofs;
      twCoordinates[i].m[m]   := twPosCm[i];
      twCoordinates[i].m[c]   := Schuster.ScanPos;
      twCoordinates[i].m[Beam]:= Schuster.Depth;
      twData[i]               := Schuster.GetProfile(i);
      end;
    twVector_ICD_cm[Start]    := twCoordinates[0];
    twVector_ICD_cm[Stop ]    := twCoordinates[Pred(Schuster.GetNumPoints)];
    StatusMessage(Schuster.LastMessage,True,2);
    end;
end; {~importschusterprofile}


{09/12/2015 transfer of flastmessage}
{15/12/2015 ScanNr copied to twScanNr}
{16/05/2020 applied FMultiScanCapable}
function TWellhoferData.ImportRfaProfile(Rfa:TRfaProfileData): Boolean;
var i,n  : Integer;
    mAxis: twcMeasAxis;
begin
SetDefaults;
with wGeneralInfo,wSource[dsMeasured],twBeamInfo,wCurveInfo,
     wMeterInfo,wDetectorInfo,wMeterInfo,wMeasDeviceInfo,wSource[dsMeasured],Rfa.RfaData do
  begin
  twFilmData:= (rfaModType='FLM');
  Result    := ((rfaModType='RAT') or (rfaModType='ABS') or twFilmData) and (rfaFileType='SCN');
  if Result then
    begin
    if      rfaRadiation='ELE'   then twBModality:= 'E'
    else if rfaRadiation='PHO'   then twBModality:= 'X'
    else if twcGenericToElectron then twBModality:= 'E'
    else                              twBModality:= 'X';
    if rfaScanType ='DPT' then twDesScanType:= snPDD
    else                       twDesScanType:= snUndefined;
    twBASD           := 100;
    FMultiScanCapable:= Rfa.FMultiScanCapable;
    FIdentity        := Rfa.Identity;
    FLastMessage     := Rfa.FLastMessage;
    FOrgExtension    := Rfa.DefaultExtension;
    FileFormat       := Rfa.FileFormat;
    FFileName        := Rfa.FileName;
    Linac            := ifthen(Length(rfaComments[0])>0,rfaComments[0],'RFA300');
    twBEnergy        := rfaEnergy_MV;
    twBWedge         := Round(rfaWedge_deg);
    twBGantry        := Round(rfaGantry);
    twBCollimator    := Round(rfaCollimator);
    twSSD_cm         := rfaSSD_mm/10;
    twBApplicator    := Num2Stg(rfaAccessory);
    FieldGT_cm       := rfaField_mm[fInplane]/10;
    FieldAB_cm       := rfaField_mm[fCrossplane]/10;
    twBMedium        := 'water';
    twBTrayTransm    := 1;
    twDesModTime     := Rfa.MakeTimeString(rfaDate);
    twDesOperator    := '';
    twDesMeasComment := rfaComments[1];
    twDesNormalise   := 1;
    twDesShift       := 0;
    n                := Pred(Rfa.GetNumPoints);
    Self.SetNumPoints(dsMeasured,Rfa.GetNumPoints);
    SetAxisID('-XY-Z',twDeviceMappingICD,twDeviceDirXYZ);
    for mAxis:= Inplane to Beam do   {fixed mapping assumed by using coordinates.m}
      begin
      twVector_ICD_cm[Start].m[mAxis]:= rfaCoordinates_cm[0].m[mAxis];
      twVector_ICD_cm[Stop ].m[mAxis]:= rfaCoordinates_cm[n].m[mAxis];
      wRefPoint    .m[mAxis]:= twVector_ICD_cm[Start].m[mAxis];
      end;
    twElSamples     := 1;
    twDesModDateTime:= rfaDate;
    twDevice        := Linac;
    twOriginalFormat:= FileFormat;
    twMeasTime      := twDesModTime;
    twMeasDateTime  := twDesModDateTime;
    twScanNr        := Rfa.ScanNr;
    twStepSizeCm    := GetDistance(twVector_ICD_cm[Start],twVector_ICD_cm[Stop ])/n;
    if n>0 then
      for i:= 0 to n do
        begin
        for mAxis:= Inplane to Beam do
          twCoordinates[i].m[mAxis]:= rfaCoordinates_cm[i].m[mAxis];
        twData[i]                  := rfaValues[i];
        end;
    StatusMessage(Rfa.LastMessage,True,2);
    end
  else
    StatusMessage('Unsupported RFA300-data',True,2);
  end;
end; {~importrfaprofile}


{17/07/2014 data scaled to 100}
{10/08/2015 files found with unreliable scanangle}
{14/08/2015 Get_tmPoscm must not apply tmScanOffAxis_mm}
{25/08/2015 diagonal detection reviewed}
{17/09/2015 tmScanDepth_mm was not used in Get_tmPoscm}
{09/12/2015 transfer of flastmessage}
{15/12/2015 ScanNr copied to twScanNr}
{03/06/2016 corrected mixup of AxisOffset crossplane/inplane in Get_tmPosCm}
{26/07/2016 repaired potential floating point error with scanning data (try)}
{07/11/2017 swap sign for both diagonals}
{10/02/2018 reversed AxisOffset crossplane/inplane in Get_tmPosCm again}
{16/05/2020 applied FMultiScanCapable}
function TWellhoferData.ImportMccProfile(Mcc:TMccProfileData): Boolean;
var i,n          : Integer;
    mAxis        : twcMeasAxis;
    Fprojection  : array[twcMeasAxis] of twcFloatType;
    AngleHorAB,Dm: twcFloatType;

  function Get_tmPoscm(AAxis :twcMeasAxis;
                       APoint:Integer    ): twcFloatType;
  var AxisOffset: twcFloatType;
  begin
  with Mcc.MccData,tmScanInfo do
    begin
    case AAxis of
      Inplane   : AxisOffSet:= tmAxisDir.c[tmTankAxis[Inplane   ]]*tmScanOffAxis_mm[fInplane   ];
      Crossplane: AxisOffSet:= tmAxisDir.c[tmTankAxis[Crossplane]]*tmScanOffAxis_mm[fCrossplane];
     else         AxisOffSet:= tmScanDepth_mm;
     end;
    Result:= Round(100*(AxisOffSet+tmPos_mm[APoint]*Fprojection[AAxis]))/1000;
    end;
  end;

begin
SetDefaults;
with wGeneralInfo,wSource[dsMeasured],twBeamInfo,wCurveInfo,
     wMeterInfo,wDetectorInfo,wMeterInfo,wMeasDeviceInfo,
     Mcc.MccData,tmScanInfo do
  begin
  FMultiScanCapable:= Mcc.FMultiScanCapable;
  FIdentity        := Mcc.Identity;
  FLastMessage     := Mcc.FLastMessage;
  FOrgExtension    := Mcc.DefaultExtension;
  FileFormat       := Mcc.FileFormat;
  FFileName        := Mcc.FileName;
  n                := Length(tmScanSpeeds);
  i                := n;
  twDeviceSpeed_mm_s:= 0;
  while i>0 do
    begin
    Dec(i);
    twDeviceSpeed_mm_s:= twDeviceSpeed_mm_s+tmScanSpeeds[i]/n;
    end;
  Self.SetNumPoints(dsMeasured,Mcc.GetNumPoints);
  n                := Pred(Mcc.GetNumPoints);
  Linac            := tmLinac;
  twBEnergy        := tmEnergy;
  twBModality      := tmModality;
  twBWedge         := Round(tmWedge);
  twBGantry        := Round(tmGantry);
  twBCollimator    := Round(tmCollimator);
  twBApplicator    := 'Undefined';
  twBMedium        := tmMedium;
  twBTrayTransm    := 1;
  twDesModTime     := Mcc.MakeTimeString(Mcc.MccModified);
  twDesOperator    := '';
  twDesMeasComment := tmComment;
  twDeviceName     := tmScanDevice;
  twDesSetupComment:= tmScan_Setup;
  twDesNormalise   := 1;
  twDesShift       := 0;
  twBASD           := 100;
  twBFieldHi[fInplane   ]:= (tmFieldOffset_mm[fInplane]+tmField_mm[fInplane   ]/2)/10;      //registered field size
  twBFieldHi[fCrossplane]:= (tmFieldOffset_mm[fInplane]+tmField_mm[fCrossplane]/2)/10;
  twBFieldLo[fInplane   ]:= (tmFieldOffset_mm[fInplane]-tmField_mm[fInplane   ]/2)/10;
  twBFieldLo[fCrossplane]:= (tmFieldOffset_mm[fInplane]-tmField_mm[fCrossplane]/2)/10;
  SetScanType(Mcc.ScanType);
  case Mcc.ScanType of
    snGT,snAB: begin
               if tmScanDiagonal=mccNotDiagonal then
                 ScanAngle:= tmScanAngle
               else
                 begin
                 SetScanType(snAngle); {note: first / second: INPLANE_AXIS_DIR=TARGET_GUN / GUN_TARGET -> tmAxisDir[tmTankAxis[Inplane]]}
                 ScanAngle:= ifthen(tmScanDiagonal=mccFirstDiagonal,45,-135); {TG/GT swapping introduces extra 90° turn}
                 end;
               if ScanAngle=90 then
                 ScanAngle:= 0; {ignore 90: versions with unreliable information ("SCAN_ANGLE=90.00") were found in the wild}
               AngleHorAB:= (ScanAngle+ifthen((tmScanDiagonal=mccNotDiagonal) and (Mcc.ScanType=snGT),90,0))*Pi/180;
               Fprojection[Inplane   ]:= Sin(AngleHorAB)*tmAxisDir.c[tmTankAxis[Inplane]];
               Fprojection[Crossplane]:= Cos(AngleHorAB)*tmAxisDir.c[tmTankAxis[Crossplane]];
               Fprojection[Beam      ]:= 0;
               end;
   else       begin
              Fprojection[Inplane   ]:= 0;
              Fprojection[Crossplane]:= 0;
              Fprojection[Beam      ]:= tmAxisDir.c[tmTankAxis[Beam]];
              end;
   end;
  Mcc.FScanType     := FSCanType;
  twDeviceMappingICD:= tmTankAxis;
  twDeviceDirXYZ    := tmAxisDir;
  for mAxis:= Inplane to Beam do
    begin
    twVector_ICD_cm[Start].m[mAxis]:= Get_tmPoscm(mAxis,0);
    twVector_ICD_cm[Stop ].m[mAxis]:= Get_tmPoscm(mAxis,n);
    wRefPoint             .m[mAxis]:= twVector_ICD_cm[Start].m[mAxis];
    end;
  twSSD_cm        := tmSSD_mm/10;
  twElSamples     := 1;
  twDesModDateTime:= tmMeasDate;
  twDevice        := Linac;
  twOriginalFormat:= FileFormat;
  twMeasTime      := Mcc.MakeTimeString(tmMeasDate);
  twMeasDateTime  := twDesModDateTime;
  twStepSizeCm    := (tmPos_mm[n]-tmPos_mm[0])/(n*10);
  twScanNr        := Mcc.ScanNr;
  Result          := (n>0);
  if Result then
    begin
    try
      Dm:= tmData[0];
      for i:= 0 to n do
        if tmData[i]>Dm then
          Dm:= tmData[i];
      if Dm>0 then
       Dm:= ifthen(Dm<50,100/Dm,1);
      for i:= 0 to n do
        begin
        for mAxis:= Inplane to Beam do
          twCoordinates[i].m[mAxis]:= Get_tmPoscm(mAxis,i);  {GTABUD}
        twData[i]:= Dm*tmData[i];
        end;
     except
      Result:= False;
     end; {try}
    end; {result}
  StatusMessage(Mcc.LastMessage,True,2);
  end;
end; {~importmccprofile}


{21/08/2015}
{28/08/2015 Check_icpField}
{09/12/2015 transfer of flastmessage}
{15/12/2015 ScanNr copied to twScanNr}
{16/05/2020 applied FMultiScanCapable}
function TWellhoferData.ImportSNAProfile(SNA:TICprofiler_ascii): Boolean;
var t: twcTankAxis;
const tmy:twcTankAxisID  ='YXZ';
      tmx:twcTankAxisID  ='XYZ';
var c            : array[1..4] of twICPAIndicators;
    mAxis        : twcMeasAxis;
    Fprojection  : array[twcMeasAxis] of twcFloatType;
    i,j,k        : Integer;
    a            : twcFloatType;

  procedure Check_icpField(ASide:twcFieldSizeDesc);
  var s: twcFloatType;
  begin
  s:= ifthen(ASide=fInplane,FieldGT_cm,FieldAB_cm);
  if SNA.icpFieldCm[ASide]>s then
    begin
    wSource[dsMeasured].twBeamInfo.twBFieldLo[ASide]:= -s/2;
    wSource[dsMeasured].twBeamInfo.twBFieldHi[ASide]:=  s/2;
    end;
  end;

begin
SetDefaults;
with wGeneralInfo,wSource[dsMeasured],twBeamInfo,wCurveInfo,
     wMeterInfo,wDetectorInfo,wMeterInfo,wMeasDeviceInfo do
  begin
  FMultiScanCapable:= SNA.FMultiScanCapable;
  FIdentity        := SNA.Identity;
  FLastMessage     := SNA.FLastMessage;
  FOrgExtension    := SNA.DefaultExtension;
  FileFormat       := SNA.FileFormat;
  FFileName        := SNA.FileName;
  Linac            := SNA.Linac;
  twBEnergy        := SNA.Energy;
  twBModality      := SNA.icpModality;
  twBWedge         := SNA.icpWedgeAngle;
  twBGantry        := SNA.icpGantry;
  twBCollimator    := SNA.icpCollimator;
  twBApplicator    := 'Undefined';
  twBTrayTransm    := 1;
  twDesModTime     := SNA.MakeTimeString(SNA.icpMeasTime);
  twDesModDateTime := SNA.icpMeasTime;
  twDesMeasComment := SNA.icpComment;
  twDeviceName     := SNA.icpModel;
  ScanNr           := SNA.ScanNr;
  ScanMax          := SNA.ScanMax;
  ScanAngle        := SNA.ScanAngle;
  twDesNormalise   := 1;
  twDesShift       := 0;
  twBASD           := 100;
  j                := SNA.GetNumPoints;
  k                := SNA.icpFirstPoint[SNA.icpScanLine];
  SetNumPoints(dsMeasured,j);
  SetScanType(SNA.ScanType);
  if SNA.icpOrientation[1]='Y' then
    begin
    twDeviceMappingICD:= tmy; {set mapping from yxz to icd}
    for t:= X to Z do
      twDeviceDirXYZ.t[t]:= ifthen(t=X,-1,1);
    c[1]:= icBottom;
    c[2]:= icTop;
    c[3]:= icLeft;
    c[4]:= icRight;
    end
  else
    begin
    twDeviceMappingICD:= tmx;
    for t:= X to Z do
      twDeviceDirXYZ.t[t]:= 1;;
    c[1]:= icLeft;
    c[2]:= icRight;
    c[3]:= icBottom;
    c[4]:= icTop;
    end;
  a                      := ScanAngle*Pi/180;
  twBFieldLo[fInplane   ]:= -SNA.icpCollimCm[c[1]];
  twBFieldHi[fInplane   ]:=  SNA.icpCollimCm[c[2]];
  twBFieldLo[fCrossplane]:= -SNA.icpCollimCm[c[3]];
  twBFieldHi[fCrossplane]:=  SNA.icpCollimCm[c[4]];
  Check_icpField(fInPlane);
  Check_icpField(fCrossPlane);
  Fprojection[Inplane   ]:= -Sin(a);
  Fprojection[Crossplane]:= Cos(a);
  Fprojection[Beam      ]:= 0;
  twDevice    := Linac;
  twMeasTime  := twDesModTime;
  twIsDiagonal:= (ScanNr>2);
  twSSD_cm    := SNA.icpSSDcm;
  twDataFirst := 0;
  twDataLast  := Pred(j);
  twScanNr    := SNA.ScanNr;
  for i:= twDataFirst to twDataLast do
    begin
    twPosCm[i]:= SNA.icpPosCm[i+k];
    twData[i] := SNA.icpData[i+k];
    for mAxis:= Inplane to Beam do
      twCoordinates[i].m[mAxis]:= twPosCm[i]*Fprojection[mAxis]+SNA.icpPosGAOffsCm.m[mAxis];
    end;
  twStepSizeCm:= Max(0.001,abs(twPosCm[twDataLast]-twPosCm[twDataFirst])/Max(1,twDataLast-twDataFirst));
  StatusMessage(SNA.LastMessage,True,2);
  Result:= True;
  end;
end; {~importsnaprofile}


{09/12/2015 transfer of flastmessage}
{05/11/2018 better protection for empty scans}
{07/12/2018 bug repair in q result}
{16/05/2020 applied FMultiScanCapable}
{08/06/2020 accomodate conversion from XY-tuples to tankmapping also for twcGenericToPDD: TankMapping:= ifthen(ScanType=snPDD,'DCI','CDI')}
function TWellhoferData.ImportHdfProfile(Hdf:THdfProfileData): Boolean;
var i          : Integer;
    m,v        : twcMeasAxis;
    TankMapping: twcMeasAxisStg;
    MappedAxis : array[twcMeasAxis] of twcTankAxis;
    ScanLine   : array[twcMeasAxis] of twcFloatType;
    hdfpoint   : twcGrafPoint;
    q          : twcFloatType;
    h          : twcCoordinate;
    c          : twcTankAxis;
begin
Result:= (Hdf<>nil);
if Result then if Hdf.GetNumPoints>0 then
  with wSource[dsMeasured],twBeamInfo,wCurveInfo do
    begin
    SetDefaults;
    q:= 0;
    for c:= X to Z do
      begin
      h.t[c]:= abs(Hdf.ScanStart.t[c]-Hdf.ScanStop.t[c]);
      if (q=0) and (h.t[c]>0) then q:= h.t[c];     {only scans larger than 0.5 cm are accepted}
      end;
    Result:= (q>0);
    if Result then
      begin
      SetScanType(Hdf.FScanType);
      twDesScantype:= ScanType;
      twBEnergy    := 0;
      if twcGenericToElectron then twBModality:= 'E'
      else                         twBModality:= 'X';
      FMultiScanCapable:= Hdf.FMultiScanCapable;
      FIdentity        := Hdf.Identity;
      FLastMessage     := Hdf.FLastMessage;
      FOrgExtension    := Hdf.DefaultExtension;
      FileFormat       := Hdf.FileFormat;
      FileTime         := Hdf.FileTime;
      FFileName        := Hdf.FileName;
      Linac            := Hdf.Linac;
      SetNumPoints(dsMeasured,Hdf.GetNumPoints);
      DateTimeToString(twMeasTime,'yyyy-mm-dd hh:nn:ss',FileTime);
      twMeasDateTime   := FileTime;
      twDesModDateTime := FileTime;
      twOriginalFormat := FileFormat;
      twDevice         := Linac;
      if (FileFormat=twcCmsProfile) then
        begin
        case twDesScanType of
          snAB: v:= CrossPlane;
          snGT: v:= Inplane;
         else   v:= Beam;
         end;
        for i:= 0 to Pred(Hdf.GetNumPoints) do
          begin
          hdfpoint  := Hdf.GetProfile(i);
          twPosCm[i]:= hdfpoint.X;
          for m:= Inplane to Beam do
            twCoordinates[i].m[m]:= ifthen(m=v,hdfpoint.X,Hdf.ScanStart.t[twcTankAxis(Ord(m))]);
          twData[i]:= hdfpoint.Y;
          end;
        end
      else if Hdf.ScanLength>0 then
        begin
        TankMapping:= ifthen(ScanType=snPDD,'DCI','CDI');
        for m:= Inplane to Beam do
          MappedAxis[m]:= twcTankAxis(Pred(Pos(twcMeasAxisStandard[Succ(Ord(m))],TankMapping)));
        q:= ifthen(Hdf.FDataMax<10,100,1);
          for m:= Inplane to Beam do
            ScanLine[m]:= (Hdf.ScanStop.t[MappedAxis[m]]-Hdf.ScanStart.t[MappedAxis[m]])/Hdf.ScanLength;
        for i:= 0 to Pred(Hdf.GetNumPoints) do
          begin
          hdfpoint  := Hdf.GetProfile(i);
          twPosCm[i]:= hdfpoint.X;
          for m:= Inplane to Beam do
            twCoordinates[i].m[m]:= hdfpoint.X*ScanLine[m];
          twData[i]:= q*hdfpoint.Y;
          end;
        end;
      twVector_ICD_cm[Start]:= twCoordinates[0];
      twVector_ICD_cm[Stop ]:= twCoordinates[Pred(Hdf.GetNumPoints)];
      StatusMessage(Hdf.LastMessage,True,2);
      Result:= True;
      end; {q>0}
    end; {hdf<>nil}
end; {~importhdfprofile}


{16/05/2020 applied FMultiScanCapable}
function TWellhoferData.ImportWmsProfile(Wms:TWmsData): Boolean;
var com     : wmsComments;
    mAxis   : twcMeasAxis;
    i       : wmsIntType;
    yy,mm,dd: Integer;
    wmspoint: wmsProfilePoint;
begin
Result:= False;
SetDefaults;
if Wms<>nil then with Wms.wmsFileHeader.wmsRec06 do
  if Wms.GetNumPoints>0 then
    with wGeneralInfo,wSource[dsMeasured],twBeamInfo,wCurveInfo,
         wMeterInfo,wDetectorInfo,wMeterInfo,wMeasDeviceInfo do
      begin
      FIdentity        := Wms.Identity;
      FMultiScanCapable:= Wms.FMultiScanCapable;
      FLastMessage     := Wms.FLastMessage;
      FOrgExtension    := Wms.DefaultExtension;
      wwmsHeader       := WMS.wmsFileHeader.wmsRec06;                       {wms-formatted storage}
      twBEnergy        := wmhEnergy;
      twBModality      := wmhRaType;
      twBWedge         := wmhWedgeAngle;
      twBGantry        := Round(wmhGantry_cm);
      twBCollimator    := Round(wmhCollim);
      twBApplicator    := 'Undefined';
      if twBModality='E' then
        twBASD         := 95
      else
        twBASD         :=  0;
      Self.FieldGT_cm  := wmhFdGT_cm;
      Self.FieldAB_cm  := wmhFdAB_cm;
      twBMedium        :=  'water';
      twBTrayTransm    := wmhTrayTr;
      case wmhKs of
        'A': begin  twDesTypeString:= 'Crossplane';  twDesScanType:= snAB;        end;
        'D': begin  twDesTypeString:= 'Depth Dose';  twDesScanType:= snPDD;       end;
        'G': begin  twDesTypeString:= 'Inplane';     twDesScanType:= snGT;        end;
        else begin  twDesTypeString:= 'FreeScan';    twDesScanType:= snFreescan;  end;
       end;
      twDeviceMappingICD   := wmhAxisID;
      twDeviceDirXYZ       := wmhAxisSign;
      twDeviceDirXYZ.c['Z']:= -wmhAxisSign.c['Z'];
      twDesModTime         := Wms.Char2Stg(wmhDate,SizeOf(wmhDate))+#32+#32+Wms.Char2Stg(wmhTime,SizeOf(wmhTime));
      twDesOperator        := '';
      twDesMeasComment     := '';
      for com:= wmhG1 to wmhU4 do if Length(Wms.Char2Stg(wmhComs[com],SizeOf(wmhComs[com])))>0 then
        twDesMeasComment   := twDesMeasComment+ApplyAliasList(Wms.Char2Stg(wmhComs[com],SizeOf(wmhComs[com])))+#32#32;
      if twcWMSdetInfo in [0..Ord(wmhU4)] then
        twDetName          := Wms.Char2Stg(wmhComs[wmsComments(twcWMSdetInfo)],SizeOf(wmhComs[wmsComments(twcWMSdetInfo)]));
      twDesSetupComment    := '';
      twDesNormalise       := 1;
      twDesShift           := 0;
      twDeviceSpeed_mm_s   := wmhCmSec*10;
      twElSamples          := wmhNsamp;
      with FParser do
        begin
        CurrentLine:= twDesModTime;
        dd:= abs(NextInteger);
        mm:= abs(NextMonth);
        yy:= abs(NextInteger);
        if yy<100         then Inc(yy,2000);
        if yy>CurrentYear then Dec(yy,100);
        if TryEncodeDate(yy,mm,dd,twDesModDateTime) then
          twDesModDateTime:= twDesModDateTime+(NextInteger+NextInteger/60)/24;
        end;
      Self.SetNumPoints(dsMeasured,Wms.GetNumPoints);
      twSSD_cm:= wmhSSD_cm;
      for mAxis:= Inplane to Beam do   {fixed mapping assumed by using coordinates.m}
        begin
        twVector_ICD_cm[Start].m[mAxis]:= wmhBorders[1,mAxis]/100;
        twVector_ICD_cm[Stop ].m[mAxis]:= wmhBorders[2,mAxis]/100;
        wRefPoint.m[mAxis]     := wmhBorders[1,mAxis]/100;
        twDesVaryingAxis[mAxis]:= abs(wmhBorders[1,mAxis]-wmhBorders[2,mAxis])/100>0.5;
        end;
      twDevice               := Linac;
      twOriginalFormat       := FileFormat;
      twMeasTime             := twDesModTime;
      twMeasDateTime         := twDesModDateTime;
      twStepSizeCm           := wmhMCm;
      if GetNumPoints>0 then
        for i:= 0 to Pred(GetNumPoints) do
          begin
          wmspoint:= Wms.GetProfile(i,True);
          for mAxis:= Inplane to Beam do twCoordinates[i].m[mAxis]:= 0.01*wmspoint.wmpAccPos[mAxis];
          try
            twData[i]:= 100.0*(wmspoint.wmpIfield/wmspoint.wmpIref);
           except
            twData[i]:= 100;
           end;
          end;
      Result:= True;
      end; {with, wmsrec6}
if Result then
  begin
  FileFormat:= Wms.FileFormat;
  FFileName := Wms.FileName;
  Linac     := Wms.Linac;
  StatusMessage(wms.LastMessage,True,2);
  end;
end; {~importwmsprofile}


{$push}{$warn 5057 off}{$warn 5058 off: Variable does not seem to be initialized}
{24/07/2015 Shift already completely done on data.}
{12/05/2016 wAxisPreserveOnExport}
{27/04/2020 Scalingfactor reviewed}
function TWellhoferData.ExportRfaProfile(Rfa          :TRfaProfileData;
                                         ASource      :twcDataSource=dsMeasured;
                                         ScalingFactor:twcFloatType =1.0         ): Boolean;
var mAxis  : twcMeasAxis;
    i      : wmsIntType;
    r      : Real;
    ExpSign: twcMeasAxisSign;
begin
Result:= Rfa<>nil;
if Result then
  with Rfa.RfaData,wSource[ASource],twBeamInfo,wCurveInfo do
    begin
    Rfa.FExtraText          := Copy(twExtraText);
    Rfa.FileFormat          := twcRFA_ascii;
    rfaModType              := ifthen(twFilmData,'FLM','RAT');
    rfaScanType             := ifthen(ScanType=snPDD,'DPT',ifthen(twIsDiagonal,'DIA','PRO'));
    rfaDetType              := ifthen(twFilmData,'UDF',ifthen(twBModality='X','ION','SEM'));
    rfaDate                 := twDesModDateTime;
    rfaField_mm[fInplane]   := Round(FieldGT_cm*10);
    rfaField_mm[fCrossplane]:= Round(FieldAB_cm*10);
    rfaRadiation            := ifthen(twBModality='X','PHO','ELE');
    rfaEnergy_MV            := Energy;
    rfaSSD_mm               := Round(twSSD_cm*10);
    rfaWedge_deg            := WedgeAngle;
    rfaGantry               := twBGantry;
    rfaCollimator           := twBCollimator;
    rfaMeasType             := ifthen(ScanType=snPDD,1,2)+ifthen(WedgeAngle=0,0,4);
    rfaDepth_01mm           := Round(ifthen(ScanType=snPDD,0,twVector_ICD_cm[Start].m[Beam])*10);
    rfaPoints               := twPoints;
    rfaStart_Meas_cm        := twVector_ICD_cm[Start];
    rfaEnd_Meas_cm          := twVector_ICD_cm[Stop ];
    rfaComments[0]          := Linac;
    If Result then
      begin
      SetLength(rfaCoordinates_cm,twPoints);
      SetLength(rfaValues        ,twPoints);
      r:= 500/Max(twcMinNormVal,twData[twMaxArr]);
      if ScalingFactor<>1 then
        r:= Min(100*Abs(ScalingFactor),r);
      if wAxisPreserveOnExport then ExpSign:= wUserAxisSign
      else                          FillChar(ExpSign,SizeOf(ExpSign),1);
      for i:= 0 to Pred(twPoints) do
        begin
        for mAxis:= Inplane to Beam do
          rfaCoordinates_cm[i].m[mAxis]:= ExpSign[mAxis]*twCoordinates[i].m[mAxis];
        rfaValues[i]:= r*twData[i];
        end;
      end;
    end;
end; {~exportrfaprofile}
{$pop}


{24/07/2015 Shift already completely done on data.}
{26/08/2015 Do not copy ExtraText from TWellhoferData tmCurveType,MccOrgScanType}
{12/05/2016 wAxisPreserveOnExport}
{24/01/2017 also preserving preferred ptw axis directions}
{07/11/2017 debug export for diagonals}
{27/04/2020 Scalingfactor reviewed}
function TWellhoferData.ExportMccProfile(Mcc          :TMccProfileData;
                                         ASource      :twcDataSource=dsMeasured;
                                         ScalingFactor:twcFloatType =1.0         ): Boolean;
var i: wmsIntType;
    f: twcFieldSizeDesc;
    c: twcChannels;
    r: twcFloatType;
    t: twcTankAxisChar;
begin
Result       := Mcc<>nil;
ScalingFactor:= Max(0.0001,Abs(ScalingFactor));
with Mcc.MccData,tmScanInfo,wSource[ASource],twBeamInfo,wCurveInfo do
  begin
  SetAxisID('XYZ',tmTankAxis,tmAxisDir);
  Mcc.FileFormat                  := twcMccProfile;
  Mcc.MccOrgFormat                := FileFormat;
  Mcc.FScanType                   := ScanType;
  Mcc.MccOrgScanType              := ScanType;
  tmScanDiagonal                  := mccNotDiagonal;
  tmScanAngle                     := -ScanAngle;
  tmScanDirection                 := 'POSITIVE';
  tmAxisDir.c[tmTankAxis[Inplane]]:= -1;
  case FScanType of
    snAngle        : begin
                     if Inrange(Round(tmScanAngle+180) mod 180,0,90) then
                       begin
                       tmCurveType    := mccCURVETYPE_CROSSPLANE;
                       tmScanDiagonal := mccFirstDiagonal;
                       Mcc.FScanType  := snAB;
                       end
                     else
                       begin
                       tmCurveType    := mccCURVETYPE_INPLANE;
                       tmScanDiagonal := mccSecondDiagonal;
                       tmAxisDir.c[tmTankAxis[Inplane]]:= 1;
                       tmScanDirection:= 'NEGATIVE';
                       Mcc.FScanType  := snGT;
                       end;
                     end;
    snAB           : tmCurveType:= mccCURVETYPE_CROSSPLANE;
    snPDD,snFanline: tmCurveType:= mccCURVETYPE_PDD;
   else  tmCurveType:= mccCURVETYPE_INPLANE;
   end;
  if Mcc.ScanType in twcVertScans   then tmMeasAxis:= Beam
  else if Mcc.ScanType=snGT         then tmMeasAxis:= Inplane
  else                                   tmMeasAxis:= Crossplane;
  Mcc.ScanNr        := 1;
  tmProgram         := ExtractFileName(ParamStr(0))+' v'+AppVersionString;
  if FileFormat<>twcMccProfile then
    begin
    tmComment:= twDesMeasComment;
    with tmDetectors[FieldCh] do
      begin
       tmDetType        := wDetectorInfo.twDetType;
       tmDetRadius_mm   := wDetectorInfo.twDetRadius_cm;
       tmDetSN          := '';
       tmDetCalibration := 0;
       tmDetIsCalibrated:= False;
       tmDetHV          := wMeterInfo.twElChannels[FieldCh].twHV;
       end;
    tmDetectors[RefCh]           := tmDetectors[FieldCh];
    tmScanDepth_mm               := wUserAxisSign[Beam      ]*ifthen(twDesVaryingAxis[Beam      ],0,twVector_ICD_cm[Start].m[Beam      ])*10;
    tmScanOffAxis_mm[fInplane]   := wUserAxisSign[Crossplane]*ifthen(twDesVaryingAxis[Crossplane],0,twVector_ICD_cm[Start].m[Crossplane])*10;
    tmScanOffAxis_mm[fCrossplane]:= wUserAxisSign[Inplane   ]*ifthen(twDesVaryingAxis[Inplane   ],0,twVector_ICD_cm[Start].m[Inplane   ])*10;
    tmScanAngle                  := ScanAngle;
    tmLinac                      := Linac;
    tmModality                   := twBModality;
    tmMeasDate                   := twMeasDateTime;
    tmEnergy                     := twBEnergy;
    tmSSD_mm                     := twSSD_cm*10;
    tmWedge                      := WedgeAngle;
    tmGantry                     := twBGantry;
    tmCollimator                 := twBCollimator;
    tmScanDevice                 := wMeasDeviceInfo.twDeviceName;
    tmMedium                     := twBMedium;
    tmMeasTime_s                 := wMeterInfo.twElSampleMs;
    tmMeasUnit                   := wDetectorInfo.twDetQuantity;
    for f:= fInplane to fCrossplane do
      begin
      tmField_mm[f]      := (twBFieldHi[f]-twBFieldLo[f])*10;
      tmFieldOffset_mm[f]:= (twBFieldHi[f]+twBFieldLo[f])*5;
      end;
    with tmElectrometer do
      begin
      tmElType:=tmScanDevice;
      for c:= FieldCh to RefCh do
        tmElRange[c]:= wMeterInfo.twElChannels[c].twRange;
      end;
    end;
  tmTankAxis     := wMeasDeviceInfo.twDeviceMappingICD;
  Mcc.SetNumpoints(GetNumPoints);
  r:= ifthen(wAxisPreserveOnExport {and (twOriginalFormat<>twMccProfile)},twPosCmExportSign,1)*10;
  for t:= 'X' to 'Z' do
    if twDesVaryingAxis[twcMeasAxis(Ord(t)-Ord('X'))] then
      r:= r*tmAxisDir.c[t]; {mapping x..z to inplane..beam}
  for i:= 0 to Pred(GetNumPoints) do
    begin
    tmPos_mm[i]:= r*twPosCm[i];
    tmData[i]  := ScalingFactor*twData[i];
    end;
  end;
end; {~exportmccprofile}


{24/07/2015 Shift already completely done on data.}
{12/05/2016 wAxisPreserveOnExport}
{27/04/2020 Scalingfactor reviewed}
{22/05/2020 r factor of 100 to high}
function TWellhoferData.ExportWmsProfile(Wms          :TWmsData;
                                         ASource      :twcDataSource=dsMeasured;
                                         ScalingFactor:twcFloatType =1.0         ): Boolean;
var mAxis                    : twcMeasAxis;
    i                        : wmsIntType;
    yy,mm,dd,hh,minute,sec,ms: Word;
    wmspoint                 : wmsProfilePoint;
    r                        : Real;
    ExpSign                  : twcMeasAxisSign;
begin
Result:= (Wms<>nil) and wSource[ASource].twValid;
if Result then
  begin
  if FileFormat in [twcWDA,twcWTX] then
    begin
    Wms.wmsFileHeader.wmsRec06:= wwmsHeader;
    try
      r:= 100*Max(Max(Abs(ScalingFactor),1),Pred(High(wmsIntType))/wwmsHeader.wmhFmax);
     except
      r:= 100;
     end;
    end
  else with Wms.wmsFileHeader.wmsRec06,wGeneralInfo,wSource[ASource],twBeamInfo,wCurveInfo,
            wMeterInfo,wDetectorInfo,wMeterInfo,wMeasDeviceInfo do
    begin
    Wms.Linac     := Linac;
    Wms.FExtraText:= Copy(twExtraText);
    DecodeDateTime(twDesModDateTime,yy,mm,dd,hh,minute,sec,ms);
    Wms.Stg2Char(Linac                  ,wmhDevice);
    Wms.Stg2Char(Linac                  ,wmhComs[wmhG1]);
    Wms.Stg2Char(twElMeterType          ,wmhComs[wmhU2]);
    Wms.Stg2Char(twDetName+' '+twDetType,wmhComs[wmhU3]);
    Wms.Stg2Char(twDesOperator          ,wmhComs[wmhU4]);
    Wms.Stg2Char(Format('%.2d-%s-%.2d' ,[dd,UpperCase(Wms.FParser.Months[mm]),yy]),wmhDate);
    Wms.Stg2Char(Format('%.2d:%.2d:%.2d',[hh,minute,sec]),wmhTime);
    try
      r:= Max(Min(Abs(ScalingFactor),1),Pred(High(wmsIntType))/Max(twcMinNormVal,twData[twMaxArr]));
     except
      r:= 100;
     end;
    wmhAxisID       := twDeviceMappingICD;
    wmhAxisSign     := twDeviceDirXYZ;
    wmhAxisSign.t[Z]:= -twDeviceDirXYZ.t[Z];
    wmhEnergy       := twBEnergy;
    wmhWedgeAngle   := twBWedge;
    wmhGantry_cm    := twBGantry;
    wmhCollim       := twBCollimator;
    wmhSSD_cm       := twSSD_cm;
    wmhFdGT_cm      := Self.FieldGT_cm;
    wmhFdAB_cm      := Self.FieldAB_cm;
    wmhTrayTr       := twBTrayTransm;
    wmhMCm          := twStepSizeCm;
    wmhCmSec        := twDeviceSpeed_mm_s/10;
    wmhNsamp        := twElSamples;
    wmhFmax         := Round(r*twData[twMaxArr]);
    wmhRmax         := 10000;
    wmhNDose        := 100;
    wmhDRdiv        := 1;
    wmhRaType       := twBModality;
    wmhASD          := twBASD;
    twMeasDateTime  := twDesModDateTime;
    case twDesScanType of
      snGT      : wmhKs     := 'G';
      snAB      : wmhKs     := 'A';
      snPDD     : wmhKs     := 'D';
      snFreescan: begin
                  wmhKs     := 'L';
                  wmhScan[1]:= 0;
                  wmhScan[2]:= Round(100*DistanceToRefPoint(twVector_ICD_cm[Stop ]));
                  end;
       else       wmhKs     := 'U';
     end;
    wmhOrKs:= wmhKs;
    if twBWedge=0 then wmhFdType:= 'O'
    else               wmhFdType:= 'W';
    end;
  with wSource[ASource] do
    begin
    if wCurveInfo.twDesScanType in [snGT,snAB,snPDD] then with wms.wmsFileHeader.wmsRec06 do
      begin
      case wCurveInfo.twDesScanType of
        snAB : mAxis:= Crossplane;
        snPDD: mAxis:= Beam;
       else    mAxis:= Inplane;
       end;
      wmhScan[1]:= Round(100*(twVector_ICD_cm[Start].m[mAxis]+twShiftCm));
      wmhScan[2]:= Round(100*(twVector_ICD_cm[Stop ].m[mAxis]+twShiftCm));
      for mAxis:= Inplane to Beam do
        begin
        wmhBorders[1,mAxis]:= Round(100*wUserAxisSign[mAxis]*twVector_ICD_cm[Start].m[mAxis]);
        wmhBorders[2,mAxis]:= Round(100*wUserAxisSign[mAxis]*twVector_ICD_cm[Stop ].m[mAxis]);
        end;
      end;
    Wms.SetNumPoints(GetSourceNumPoints(ASource));
    if wAxisPreserveOnExport then ExpSign:= wUserAxisSign
    else                          FillChar(ExpSign,SizeOf(ExpSign),1);
    for i:= 0 to Pred(GetSourceNumPoints(ASource)) do with wmspoint do
      begin
      for mAxis:= Inplane to Beam do
        wmpAccPos[mAxis]:= Round(100*ExpSign[mAxis]*twCoordinates[i].m[mAxis]);
      wmpIfield:= Round(r*twData[i]);
      wmpIref  := 10000;
      Wms.PutProfile(i,wmspoint,True);
      end;
    end; {with}
  end; {result}
end; {~exportwmsprofile}


{20/07/2015 twComposite added}
{06/08/2015 twIsFiltered added}
{12/12/2015 twFFFslope}
{20/12/2015 twTopModel}
{01/08/2016 twFileIDString}
{15/11/2016 twCenterDef}
{14/01/2017 twSigmoidOffset}
{14/02/2017 twScanDevice to identify 2Darray}
{23/11/2017 added twFlatPosCm,twSymAreaRatio}
{12/01/2018 added twAbsNormConfig to note used info from modlist}
{27/01/2018 twAbsNormDefUse}
{15/05/2020 twFirstScanPosCm,twLastScanPosCm}
{22/05/2020 twSigmoidDone}
{27/08/2020 twMaxPosCm, twMaxValue}
{23/02/2021 reintroduced twFFFdetected because MRLinac can also be fff}
procedure TWellhoferData.CopyParameters(var ASource,ADestination:twCurveDataRec);
begin
with ADestination do
  begin
  twAbsNormConfig   := ASource.twAbsNormConfig;
  twAbsNormDefUse   := ASource.twAbsNormDefUse;
  twAbsNormPosCm    := ASource.twAbsNormPosCm;
  twAbsNormValue    := ASource.twAbsNormValue;
  twAppliedNormVal  := ASource.twAppliedNormVal;
  twAnalysed        := ASource.twAnalysed;
  twAlignedTo       := ASource.twAlignedTo;
  twAlignedTo       := ADestination.twSelf;
  twAvgNormValue    := ASource.twAvgNormValue;
  twBackGround      := ASource.twBackGround;
  twBeamInfo        := ASource.twBeamInfo;
  twCenterPosDefUse := ASource.twCenterPosDefUse;
  twCenterPosCm     := ASource.twCenterPosCm;
  twCenterPosValid  := ASource.twCenterPosValid;
  twCenterArr       := ASource.twCenterArr;
  twComposite       := ASource.twComposite;
  twConfidenceLimit := ASource.twConfidenceLimit;
  twCurveIDString   := ASource.twCurveIDString;
  twDataFirst       := ASource.twDataFirst;
  twDataLast        := ASource.twDataLast;
  twDataHistoryStg  := ASource.twDataHistoryStg;
  twDevice          := ASource.twDevice;
  twDerivativeValid := ASource.twDerivativeValid;
  twSigmoidDone     := ASource.twSigmoidDone;
  twSigmoidFitData  := ASource.twSigmoidFitData;
  twExtraText       := ASource.twExtraText;
  twFastScan        := ASource.twFastScan;
  twSetFieldType    := ASource.twSetFieldType;
  twFFFdetected     := ASource.twFFFdetected;
  twFFFslopesTop    := ASource.twFFFslopesTop;
  twFFFslope        := ASource.twFFFslope;
  twFileIDString    := ASource.twFileIDString;
  twFileName        := ASource.twFileName;
  twFilmData        := ASource.twFilmData;
  twFilterPoints    := ASource.twFilterPoints;
  twFilterString    := ASource.twFilterString;
  twFirstDataPosCm  := ASource.twFirstDataPosCm;
  twFirstScanPosCm  := ASource.twFirstScanPosCm;
  twFittedData      := ASource.twFittedData;
  twInFieldAreaOk   := ASource.twInFieldAreaOk;
  twInFieldArr      := ASource.twInFieldArr;
  twInFieldPosCm    := ASource.twInFieldPosCm;
  twFlatness        := ASource.twFlatness;
  twIsDerivative    := ASource.twIsDerivative;
  twIsFiltered      := ASource.twIsFiltered;
  twIsDiagonal      := ASource.twIsDiagonal;
  twIsGamma         := ASource.twIsGamma;
  twIsRelative      := ASource.twIsRelative;
  twLastDataPosCm   := ASource.twLastDataPosCm;
  twLastScanPosCm   := ASource.twLastScanPosCm;
  twLevelPos        := ASource.twLevelPos;
  twLinSlope        := ASource.twLinSlope;
  twLocalPeak       := ASource.twLocalPeak;
  twLocked          := ASource.twLocked;
  twMayneordApplied := ASource.twMayneordApplied;
  twMaxArr          := ASource.twMaxArr;
  twMaxPosCm        := ASource.twMaxPosCm;
  twMaxValue        := ASource.twMaxValue;
  twMeasTime        := ASource.twMeasTime;
  twMeasDateTime    := ASource.twMeasDateTime;
  twMinArr          := ASource.twMinArr;
  twMirrored        := ASource.twMirrored;
  tw2DoseConv       := ASource.tw2DoseConv;
  twOD2doseFilm     := ASource.twOD2doseFilm;
  twOriginalFormat  := ASource.twOriginalFormat;
  twOriginPosValid  := ASource.twOriginPosValid;
  twPDD10           := ASource.twPDD10;
  twPDD20           := ASource.twPDD20;
  twPddFitData      := ASource.twPddFitData;
  twPlotScaling     := ASource.twPlotScaling;
  twPoints          := ASource.twPoints;
  twPosCmExportSign := ASource.twPosCmExportSign;
 {$IFDEF POSINTEGRAL}
  twPosIntegral     := ASource.twPosIntegral;
 {$ENDIF}
  twRelatedSource   := ASource.twRelatedSource;
  twRelAvgInField   := ASource.twRelAvgInField;
  twRelMaxInField   := ASource.twRelMaxInField;
  twRelMinInField   := ASource.twRelMinInField;
  twRefNormFactor   := ASource.twRefNormFactor;
  twRelNormPosCm    := ASource.twRelNormPosCm;
  twRelNormValue    := ASource.twRelNormValue;
  twResampled       := ASource.twResampled;
  twPosScaling      := ASource.twPosScaling;
  twScanAngle       := ASource.twScanAngle;
  twScanDevice      := ASource.twScanDevice;
  twScanTypeString  := ASource.twScanTypeString;
  twScanFirst       := ASource.twScanFirst;
  twScanLast        := ASource.twScanLast;
  twScanNr          := ASource.twScanNr;
  twScanLength      := ASource.twScanLength;
  twSSD_cm          := ASource.twSSD_cm;
  twSDD2SSDratio    := ASource.twSDD2SSDratio;
  twSSD2NormRatio   := ASource.twSSD2NormRatio;
  twShiftCm         := ASource.twShiftCm;
  twSNR             := ASource.twSNR;
  twVector_ICD_cm   := ASource.twVector_ICD_cm;
  twStepSizeCm      := ASource.twStepSizeCm;
  twStepSign        := ASource.twStepSign;
  twSymCorrected    := ASource.twSymCorrected;
  twSymmetry        := ASource.twSymmetry;
  twSymAreaRatio    := ASource.twSymAreaRatio;
  twSymLinacError   := ASource.twSymLinacError;
  twTag             := ASource.twTag;
  twTopModel        := ASource.twTopModel;
  twUsedEdgeLevel   := ASource.twUsedEdgeLevel;
  twValid           := ASource.twValid;
  twWidthCm         := ASource.twWidthCm;
  end;
end; {~copyparameters}


{06/08/2015 InitialiseDestination}
{07/08/2015 LOCKING_MSSG}
{27/04/2020 InitCurve}
procedure TWellhoferData.CopyCurve(var ASource,ADestination:twCurveDataRec;
                                   InitializeDestination   :Boolean=False);
{$IFDEF THREADED}
var i: Integer;
begin
i:= 100;
{$ELSE}
begin
{$ENDIF THREADED}
if InitializeDestination then
  begin
  InitCurve(ADestination);
  ClearCurve(ADestination,False);
  end
{$IFDEF THREADED}
else if ADestination.twLocked then
  begin
  {$IFDEF LOCKING_MSSG}
  StatusMessage(ADestination.twDataHistoryStg+twLockedStg);
  {$ENDIF LOCKING_MSSG}
  while ADestination.twLocked and (i>0)do
    begin
    Sleep(10);
    Dec(i);
    end;
  end
{$ENDIF THREADED};
CopyParameters(ASource,ADestination);
with ADestination do
  begin
  CheckSize(ADestination);
  twCoordinates:= Copy(ASource.twCoordinates {,0,ASource.twPoints});
  twData       := Copy(ASource.twData        {,0,ASource.twPoints});
  twPosCm      := Copy(ASource.twPosCm       {,0,ASource.twPoints});
  {$IFDEF THREADED}
  if i=0 then
    twLocked:= False
  {$IFDEF LOCKING_MSSG}
  else         StatusMessage(ADestination.twDataHistoryStg+' ok')
  {$ENDIF LOCKING_MSSG};
  {$ENDIF THREADED}
  end;
end; {~copycurve}



{14/07/2015}
{06/08/2015 InitialiseDestination}
procedure TWellhoferData.CopyCurve(ASource,ADestination :twcDataSource;
                                   InitializeDestination:Boolean=False);
begin
CopyCurve(wSource[ASource],wSource[ADestination],InitializeDestination);
end; {~copycurve}


{13/08/2015 wCheckRefCurveString added to test}
{19/03/2016 added wCheckRefIgnoreLinac}
function TWellhoferData.ReferenceValid(AReference:twcDataSource=dsReference): Boolean;
var MeasStg,RefStg: String;
begin
Result:= IsValid and wSource[AReference].twValid;
if Result then
  begin
  MeasStg:= GetCurveIDString;
  RefStg := GetCurveIDString(AReference);
  if wCheckRefIgnoreLinac then
    RefStg:= StringReplace(RefStg,wSource[AReference].twDevice,LowerCase(wSource[dsMeasured].twDevice),[rfIgnoreCase]);
  Result:= (not wCheckRefCurveString) or (RefStg=MeasStg);
  end;
end; {~referencevalid}


{20/11/2016}
{22/11/2016 added boolean compare twIsDiagonal}
{02/01/2018 added zero unexpected}
{29/10/2020 removed zero unexpected}
procedure TWellhoferData.ReportDifferences(ASource   :twcDataSource=dsMeasured;
                                           AReference:twcDataSource=dsRefOrg);

  function CompareValues(Description,String1,String2:String;
                         DifString                  :String='<>'): Boolean;  overload;
  begin
  Result:= (String1=String2) and (DifString='<>');
  if not Result then
    StatusMessage(Format('%s: %s %s %s',[Description,String1,DifString,String2]));
  end;

  function CompareValues(Description  :String;
                         Value1,Value2:twcFloatType): Boolean;  overload;
  begin
  Result:= CompareValues(Description,Num2Stg(Value1,0,2),Num2Stg(Value2,0,2));
  end;

  function CompareValues(Description  :String;
                         Value1,Value2:Boolean): Boolean;     overload;
  begin
  Result:= CompareValues(Description,BoolStrings[Value1],BoolStrings[Value2]);
  end;

begin
if wSource[ASource].twValid then
  begin
  CompareValues('sources'        ,twcDataSourceNames[ASource]            ,twcDataSourceNames[AReference],'versus');
  if not CompareValues('Linac'   ,wSource[ASource].twDevice              ,wSource[AReference].twDevice) then
    CompareValues('Linac (aliased)',ApplyAliasList(ApplyModBeamList(wSource[ASource].twDevice)),
                                    wSource[AReference].twDevice);
  CompareValues('Modality'        ,wSource[ASource].twBeamInfo.twBModality,wSource[AReference].twBeamInfo.twBModality);
  CompareValues('Energy'          ,wSource[ASource].twBeamInfo.twBEnergy  ,wSource[AReference].twBeamInfo.twBEnergy  );
  CompareValues('Linac'           ,wSource[ASource].twScanTypeString      ,wSource[AReference].twScanTypeString);
  CompareValues('SSD'             ,wSource[ASource].twSSD_cm              ,wSource[AReference].twSSD_cm      );
  CompareValues('field GT'        ,GetFieldSize(ASource,fInplane)         ,GetFieldSize(AReference,fInplane   ));
  CompareValues('field AB'        ,GetFieldSize(ASource,fCrossplane)      ,GetFieldSize(AReference,fCrossplane));
  CompareValues('Scan angle'      ,wSource[ASource].twScanAngle           ,wSource[AReference].twScanAngle);
  CompareValues('Wedge angle'     ,wSource[ASource].twBeamInfo.twBWedge   ,wSource[AReference].twBeamInfo.twBWedge);
  if ScanType in twcHoriScans then
    CompareValues('depth'         ,wSource[ASource   ].twVector_ICD_cm[Start].m[Beam],
                                   wSource[AReference].twVector_ICD_cm[Start].m[Beam]);
  CompareValues('Diagonal status' ,wSource[ASource].twIsDiagonal          ,wSource[AReference].twIsDiagonal);
  end;
end; {~reportdifferences}


{$push}{$warn 5092 off: Variable RefStg does not seem to be initialized}
(*
****BistroMath core function****
There are multiple sources for references:
-a forced reference in memory
-the last used reference in memory
-a (possible structured) multiple reference in memory
-the reference directory
This procedure tries all in-memory options first and then searches on disk. Disk search is based on filename only.
*)
{13/07/2015 wSource[Unrelated] used to store unmatched reference when valid and not locked.}
{21/07/2015
  wSource[RefOrg] used now. Set twDataHistoryStg to twReferenceStg.
  usage of GetCurveIDString}
{01/08/2015 TakeCurrentRefSource and wTakeCurrentRefSource introduced}
{04/08/2015 Readability of code improved.}
{05/08/2015 usage of TakeReferenceOrg which will reset Source[Reference].twAlignedTo}
{18/09/2015 logging}
{16/12/2015
  for multiscanref files needs equal measured.twScanNr and reforg.twScanNr
  FRefOrgSrc}
{25/12/2015 for analysis for useorg}
{19/03/2016 added wCheckRefIgnoreLinac, use of referencevalid}
{10/05/2016 logmessage added to CompareIDs, ErrorState set}
{30/07/2016 wMultiScanReferences}
{02/08/2016 makecurvename}
{03/08/2016 r.makecurvename(false,..) -> r.makecurvename(r.MultiScanRefOk,..)}
{22/09/2016 if wMultiRefIndex then... changed to if wMultiRefIndex and (Length(FMultiScanList)>0) then...}
{29/09/2016 limited application of renamefile due to lost files...}
{07/10/2016 safety catch: when renaming file new filename should not be empty, use try..except}
{22/10/2016 extra check wSource[twValid] before trusting it}
{28/09/2017 added statusmessage of target}
{01/10/2018 CheckRefOrg: ( (not MultiScanRefUse) or (wSource[dsRefOrg].twCurveIDstring=wSource[dsMeasured].twCurveIDstring))}
{11/10/2018 CheckRefOrg: ( (not MultiScanRefOk) or (wSource[dsRefOrg].twCurveIDstring=wSource[dsMeasured].twCurveIDstring))}
{09/07/2020 if (not UseOrg) AND (LogLevel>1) then ReportDifferences}
{11/08/2020 checkreforg made more sensitive for changeover from single scan to array scan}
{24/08/2020 error in parameters of Format for r.ReadData}
{29/09/2020 call to PassSettings changed}
{01/10/2020 use CompareCurveIDStrings in CheckRefOrg}
{05/10/2020 changed to ((not wCheckRefCurveString) and wTakeCurrentRefSource) or CompareCurveIDStrings)}
{14/12/2020 review of checkreforg}
{14/01/2020 PriorityMessage: LogLevel=-1}
function TWellhoferData.LoadReference(AFileName            :String ='';
                                      SetCurrentAsRefSource:Boolean=False): Boolean;
var r                                : TWellhoferData;
    s,MeasCurveIDstg,RefOrgCurveIDstg: RawByteString;
    FromDisk,SameID,GenName,UseOrg   : Boolean;
    ReportedSrc                      : twcDataSource;
    {$IFDEF MULTIREF_INDEX}
    RefScanNr                        : Integer;
    {$ENDIF}

  procedure LogMessage(ContextStg,ReferenceStg,ResultStg:String;
                       ResultValue                      :Boolean;
                       ALogLevel                        :Integer=1);
  begin
  if (ALogLevel<=LogLevel) or ((ALogLevel=1) and (not ResultValue)) then
    begin
    ErrorState:= ErrorState or (not ResultValue);
    StatusMessage(Format('%s: src=%s | ref=%s | %s',[ContextStg,GetCurveIDString,ReferenceStg,ifthen(ResultValue,'','not ')+ResultStg]),True,ALogLevel);
    end;
  end;

  function CheckMultiRef(AcceptStream:Boolean=False): Boolean;
  begin
  AcceptStream:= AcceptStream and (FRefOrgSrc.Size>0)
                 {$IFDEF MULTIREF_INDEX} and MultiRefIndex and (Length(FMultiScanList)>0){$ENDIF};
  Result      := AcceptStream or
                 ( FArrayScanRefOk and wSource[dsRefOrg].twValid and
                    ({$IFDEF MULTIREF_INDEX}(RefScanNr>0) or {$ENDIF}
                     (wSource[dsRefOrg].twScanNr=wSource[dsMeasured].twScanNr)) );
  end;

  function CompareIDs(MeasStg,RefStg,MeasLinac,RefLinac:String): Boolean;
  begin
  if wCheckRefIgnoreLinac then
    begin
    if Pos(LowerCase(MeasLinac),MeasStg)=1 then
      MeasStg:= Copy(MeasStg,Succ(Length(MeasLinac)));
    if Pos(LowerCase(RefLinac) ,RefStg )=1 then
      RefStg := Copy(RefStg ,Succ(Length(RefLinac )));
    end;
  Result:= (RefStg=MeasStg);
  if LogLevel>1 then
    LogMessage('ID check',RefStg,'ok',Result,2);
  end;

  (*
  The wanted output should be:
  general tests:
  -reforg should be valid
  -reforg should be unlocked
  result:
  -for a single scan (not ArrayScanRefOk): comparison of MeasCurveIDstg,RefOrgCurveIDstg (wCheckRefIgnoreLinac: exclusion of linac name)
  -for a structured multiref(ArrayScanRefOk): comparison of twFileIDstring               (wCheckRefIgnoreLinac: exclusion of linac name)
  -true for override of comparison
  *)
  function CheckRefOrg: Boolean;
  var NoTest: Boolean;
  begin
  NoTest:= (not wCheckRefCurveString) and wTakeCurrentRefSource;
  Result:= wSource[dsRefOrg].twValid and (not wSource[dsRefOrg].twLocked)
            and
          (( NoTest or ReferenceValid(dsRefOrg)                                             ) or
           ((not ArrayScanRefOk) and
             CompareIDs(MeasCurveIDstg              ,RefOrgCurveIDstg,
                         wSource[dsMeasured].twDevice,wSource[dsRefOrg].twDevice)           ) or
           ( ArrayScanRefOk      and
             CompareIDs(wSource[dsMeasured].twFileIDstring,wSource[dsRefOrg].twFileIDstring,
                         wSource[dsMeasured].twDevice      ,wSource[dsRefOrg].twDevice     )) );
  if LogLevel>1 then
    LogMessage('Use Current Ref'+ifthen(ArrayScanRefOk,Format(' scan %d',[wSource[dsMeasured].twScanNr]),''),
               GetCurveIDString(dsRefOrg),'ok',Result);
  end;

  {03/08/2016 wCheckRefIgnoreLinac}
  {20/12/2018 i=2: not multiscanlist}
  function CheckMultiRefOrgList(ACurveIDString:String): Integer;
  var i,j: Integer;
  begin
  if not Assigned(FMultiScanList) then
    Result:= 0
  else
    begin
    i:= Length(FMultiScanList);
    j:= Succ(ifthen(wCheckRefIgnoreLinac,Length(ApplyAliasList(wSource[dsRefOrg].twDevice)),0));
    Result:= 1;                                                                 //FMultiScanList[0] is filename
    while (Result<i) and (Copy(FMultiScanList[Result],j)<>ACurveIDString) do
      Inc(Result);
    if (Result=i) or (i=2) then
      Result:= 0;
    end;
  end;

  function SetDiskFile(AString    :String;
                       PriorityMsg:Boolean=False): Boolean;
  begin
  s       := AString;
  Result  := FileExists(AString);
  LogMessage('Find Ref',CompressedFilename(AString),'found',Result,ifthen(PriorityMsg and (not Result),-1,1)*2);
  end;

  function LocalCurveID(ASource:twcDataSource): String;
  begin
  if wCheckRefIgnoreLinac then
    Result:= MakeCurveName(False,True,wDefaultIgnoreSet+[twiLinac],True,ASource)
  else
    Result:= ApplyAliasList(wSource[ASource].twCurveIDString);
  end;

begin
//wSource[RefOrg].twValid:= False; {test: force file-load}
Inc(FActiveCnt);
Result          := wSource[dsMeasured].twValid and (not FFrozen);
FromDisk        := False;
MeasCurveIDstg  := LocalCurveID(dsMeasured);
RefOrgCurveIDstg:= wSource[dsRefOrg  ].twCurveIDString;
UseOrg          := CheckRefOrg;
{$IFDEF MULTIREF_INDEX}
RefScanNr:= 0;
{$ENDIF}
if wCheckRefCurveString then ReportedSrc:= dsMeasured
else                         ReportedSrc:= dsRefOrg;
if Result then
  begin
  GenName:= AFileName='';
 {$IFDEF MULTIREF_INDEX}
  if MultiRefIndex and (Length(FMultiScanList)>0) then
    s:= FMultiScanList[0]
  else
 {$ENDIF}
    s:= ReferenceDirectory+ifthen(FArrayScanRefOk,wSource[dsMeasured].twFileIDString,wSource[dsMeasured].twCurveIDString);
  StatusMessage('LoadReference->'+s,True,1);
  if not UseOrg then
    begin
    if GenName then with wSource[dsMeasured] do
      begin
      {$IFDEF MULTIREF_INDEX}
      if MultiRefIndex and (Length(FMultiScanList)>0) and
        (wTakeCurrentRefSource or (wSource[dsRefOrg].twScanDevice=wSource[dsMeasured].twScanDevice)) then
        begin
        RefScanNr:= CheckMultiRefOrgList(MeasCurveIDstg);
        UseOrg   := (RefScanNr>0);
        end
      else
      {$ENDIF}
        UseOrg:= CheckRefOrg or
                ( CheckMultiRef(True) and (SetCurrentAsRefSource or wTakeCurrentRefSource or (FRefOrgFileName=s)) );
      if (not UseOrg) or (AFileName='') then
        AFileName:= ReferenceDirectory+ifthen(FArrayScanRefOk,twFileIDString,twCurveIDString);
      end;
    if not (UseOrg or SetCurrentAsRefSource or wTakeCurrentRefSource) then
      begin
      FromDisk:= SetDiskFile(AFileName,True) or
                 SetDiskFile(ReferenceDirectory+MakeCurveName(FArrayScanRefOk,True,[twiSSD]         )) or //exclude ssd
                 SetDiskFile(ReferenceDirectory+MakeCurveName(FArrayScanRefOk,True,[twiDepth]       )) or //exclude depth
                 SetDiskFile(ReferenceDirectory+MakeCurveName(FArrayScanRefOk,True,[twiDepth,twiSSD]));   //exclude both
      UseOrg:= (FRefOrgFileName=s) and CheckRefOrg;                                                       //the current file might be good enough now
      end; {forced read from file}
    end; {useorg}
  if (not UseOrg) and (LogLevel>1) then
    ReportDifferences;
  if UseOrg and (CheckMultiRef(False) or SetCurrentAsRefSource or wTakeCurrentRefSource) and TakeReferenceOrg(MeasCurveIDstg) then
    begin
    wSource[dsReference  ].twAnalysed:= False;                                     //the settings might have been changed, so analyse completely
    wSource[dsRefFiltered].twAnalysed:= False;
    StatusMessage(twcDataSourceNames[dsReference]+' -> '+GetCurveIDString(ReportedSrc));
    if AlignReference then
      AlignCurves
    else
      Analyse(dsReference);
    end
  else if FromDisk or UseOrg or
          (({$IFDEF MULTIREF_INDEX}(RefScanNr>0) or {$ENDIF}(not (SetCurrentAsRefSource or wTakeCurrentRefSource))) and CheckMultiRef(True)) then
    begin
    r:= TWellhoferData.Create;
    PassSettings(r,'reference');
    r.AutoLoadReference:= False;                                                   //***** prevent loop! *****
    if FromDisk and ((FRefOrgFileName<>s) or (not wSource[dsRefOrg].twValid)) then //if multiscanfile already in memory, then don't open it from file again
      begin
      StatusMessage('->LoadReference '+CompressedFilename(s),True,1);
      try
        FromDisk:= r.ReadData(s);
       except
        FromDisk:= False;
       end;
      if FromDisk then
        begin
        r.Parser.PreLoaded:= True;
        SetReferenceOrg(dsMeasured,True,r);                                        //transfer also the raw data from a possibly multi-dataset
        {$IFDEF MULTIREF_INDEX}
        if MultiRefIndex and r.IndexMultiScan(s,GetCurveIDString) then             //IndexMultiScan will try to take the apropriate curve
          FMultiScanList:= r.FMultiScanList;
        {$ENDIF}
        end;
      end
    else
      begin
      {$IFDEF MULTIREF_INDEX}
      r.Filename      := s;
      r.ObjectCallSign:= 'multi-ref';                                              //just for debugging purposes
      r.wMultiScanNr  := ifthen(MultiRefIndex,RefScanNr,wMultiScanNr);             //set which scan to read from multi-dataset
      r.wMultiScanMax := RefScanNr;                                                //may speed up reading considerably
      StatusMessage(Format('->LoadReference %s [%d/%d]',[CompressedFilename(s),r.wMultiScanNr,r.wMultiScanMax]));
      {$ENDIF}
      try
        FromDisk:= r.ReadData(FRefOrgSrc,0,FRefOrgSrcType);                        //read scan from possibly multi-dataset out of saved stream
       except
        FromDisk:= False;
       end;
      end;
    SameID:= r.Analyse and CompareIDs(MeasCurveIDstg,r.GetCurveIDString,Linac,r.Linac); //=> prepareprofile => fastscan(measured) => twCenterPosValid
    Result:= FromDisk and ((not wCheckRefCurveString) or SameID);                  //true when file read&analysed and (has the correct id (or can be ignored))
    if Result then
      begin
      CopyCurve(r.wSource[dsMeasured],wSource[dsReference]);
      ClearCurve(dsRefFiltered);
      wSource[dsReference].twDataHistoryStg:= twcDataSourceNames[dsReference];
      wSource[dsReference].twFastScan      := False;
      if not wSource[dsRefOrg].twLocked then
        CopyCurve(r.wSource[dsMeasured],wSource[dsRefOrg]);
      if AlignReference then
        AlignCurves;
      end
    else
      Warning:= Format(twForFileNotRead,[CompressedFilename(s)]);
    try
      if FromDisk     and GenName       and (Length(AFileName)>4)                           and (s<>AFileName) and (r.wMultiScanMax=0) and
        (s<>FileName) and FileExists(s) and (ExtractFilePath(s)=ExtractFilePath(AFileName)) and (not FileExists(AFileName)) then
        RenameFile(s,AFilename);
     except
       {do nothing}
     end;
    try
      FreeAndNil(r);
     except
      ExceptMessage('WH.LoadReference!');
     end;
    end
  else
    begin
    Result := False;
    Warning:= Format(twForFileNotFound,[CompressedFilename(s)]);
    ClearCurve(dsReference);
    end;
  end;
Dec(FActiveCnt);
end; {~loadreference}
{$pop}


{$push}{$warn 5092 off: Variable does not seem to be initialized}{$warn 5057 off: not initialised}
{24/07/2015 Points are already shifted}
{12/05/2016 wAxisPreserveOnExport}
{03/06/2016 corrected wAxisPreserveOnExport}
function  TWellhoferData.WriteData(AFileName  :String;
                                   AStringList:TStrings;
                                   ASource    :twcDataSource=dsMeasured;
                                   ClearList  :Boolean     =True     ): Boolean;
var Stg    : string;
    tAxis  : twcTankAxisChar;
    mAxis  : twcMeasAxis;
    i      : Integer;
    c      : Char;
    tOrigin: twcCoordinate;
    ExpSign: twcMeasAxisSign;

  procedure WriteIntegers(a  :array of Integer;
                          Stg:String='');    overload;
  var i: integer;
  begin
  Stg:= Stg+chSpace;
  for i:= 0 to Pred(Length(a)) do
    Stg:= Format('%s %d',[Stg,a[i]]);
  AStringList.Append(Stg);
  end;

  procedure WriteReals1(a             :array of twcFloatType;
                        Stg           :String='';
                        AxisFlipExport:Boolean=False);
  var i: integer;
  begin
  Stg:= Stg+':'+chSpace;
  AxisFlipExport:= AxisFlipExport and (Length(a)=3);
  for i:= 0 to Pred(Length(a)) do
    Stg:= Format('%s %f',[Stg,ifthen(AxisFlipExport,wUserAxisSign[twcMeasAxis(i)],1)*a[i]]);
  AStringList.Append(Stg);
  end;

  procedure WriteReals2(a             :array of twcFloatType;
                        Stg           :String='';
                        AxisFlipExport:Boolean=False);
  begin
  WriteReals1(a,Stg+chSpace+'[cm]',AxisFlipExport);
  end;

  procedure WriteStrings1(Stg1,Stg2:String);
  begin
  AStringList.Append(Stg1+chTab+Stg2);
  end;

  procedure WriteStrings2(Stg1,Stg2:String);
  begin
  WriteStrings1(Stg1+': ',Stg2);
  end;

  procedure WriteStrings3(Stg1,Stg2:String);
  begin
  WriteStrings1(Stg1+' Time: ',Stg2);
  end;

  procedure WriteDate(Stg     :string;
                      DateTime:TDateTime);
  var s: string;
  begin
  DateTimeToString(s,'yyyy-mm-dd hh:nn:ss',DateTime);
  AStringList.Append(Stg+' Time: '+s);
  end;

  procedure WritePoint(a:twcCoordinate;
                       f:twcFloatType);
  var mAx: twcMeasAxis;
      s  : string;
  begin
  s:= '';
  for mAx:= Inplane to Beam do
    s:= Format('%s %8.3f',[s,ExpSign[mAx]*a.m[mAx]]);
  AStringList.Append(Format(' %s %8.3f',[s,f]));
  end;

begin
inherited WriteData(AFileName,AStringList,ASource,ClearList);
with wGeneralInfo,wSource[ASource],twBeamInfo,wCurveInfo,wMeterInfo,
     wDetectorInfo,wMeterInfo,wMeasDeviceInfo,AStringList do
  begin
  if wAxisPreserveOnExport then ExpSign:= wUserAxisSign
  else                          FillChar(ExpSign,SizeOf(ExpSign),1);
  Clear;
  WriteStrings2('Clinic'                ,twClinic);
  WriteStrings2('Address'               ,twAddress);
  WriteStrings2('Telephone'             ,twTelephone);
  WriteStrings2('Email'                 ,twEmail);
  Append('');
  Append('Beam Description:');
  Append('');
  WriteStrings1('Radiation device'      ,Linac);
  case twBModality of
    'X': Stg:= 'MV Photon';
    'E': Stg:= 'MeV Electron';
    'P': Stg:= 'MeV Proton';
   else  Stg:= 'Undefined';
   end;
  if Round(twBEnergy)=twBEnergy then i:= 0
  else                               i:= 1;
  Append(Format('Energy%s%0.*f %s'      ,[chTab,i,twBEnergy,Stg]));
  Append(Format('Wedge:%s%d °'          ,[chTab,twBWedge]));
  Append(Format('Gantry%s%d °(0°up, CW)',[chTab,twBGantry]));
  Append(Format('Collimator%s%d °'      ,[chTab,twBCollimator]));
  WriteReals2([wSource[ASource].twSSD_cm,twBSAD_cm],'SSD, SAD');
  WriteStrings2('Applicator',twBApplicator);
  Append(Format('Field size [cm x cm]: %f x %f',[FieldGT_cm,FieldAB_cm]));
  WriteReals2([twBFieldLo[fInplane   ],twBFieldHi[fInplane   ]] ,'Field position inline (min, max)');
  WriteReals2([twBFieldLo[fCrossplane],twBFieldHi[fCrossplane]] ,'Field position crossline (min, max)');
  WriteStrings2('Medium',twBMedium);
  Append('');
  case twDesScanType of
    snGT   : Stg:= 'Inline Profile';
    snAB   : Stg:= 'Crossline Profile';
    snPDD  : Stg:= 'Depth Dose';
    snAngle: Stg:= 'Angle';
   else      Stg:= 'Point To Point';
   end;
  WriteStrings2('CurveType'             ,Stg);
  Append('');
  Append('Curve Description:');
  Append('');
  if twOriginalFormat in [twcWellhoferAscii_v6,twcWellhoferAscii_v7,twcWTX,twcWDA] then
    begin
    WriteStrings3('Measurement'         ,wSource[ASource].twMeasTime);
    WriteStrings3('Modification'        ,twDesModTime);
    end
  else
    begin
    WriteDate('Measurement'             ,wSource[ASource].twMeasDateTime);
    WriteDate('Modification'            ,twDesModDateTime);
    end;
  WriteStrings2('Operator'              ,twDesOperator);
  WriteStrings2('Measurement comment'   ,twDesMeasComment);
  WriteStrings2('Setup comment'         ,twDesSetupComment);
  if twDesNormalise=0 then Stg:= twcDefUnknown
  else                     Stg:= Num2Stg(twDesNormalise,0,2);
  WriteStrings2('Renormalise factor'    ,Stg);
  WriteReals2([twDesShift],'Curve offset');
  Append('');
  Append('Detector');
  WriteStrings2('Quantity',twDetQuantity);
  Stg:= Format('Detector Type: %s %s  Offset to P eff [cm]: %0.2f Radius: %0.2f cm',
                [twDetName,twDetType,twDetPeffOffset_cm,twDetRadius_cm]);
  Append(Stg);
  Append('Can be used for CA24 calibration:	No');
  Append('');
  Append('Electrometer');
  WriteStrings2('Electrometer type'     ,twElMeterType);
  WriteStrings2('Measurement mode'      ,twElMeasMode);
  Append('Reference division:	-');
  WriteIntegers([twElRefAvg,twElRefMin,twElRefMax],'Reference (avg, min, max):');
  if twElSamples=0 then Stg:= twcDefUnknown
  else                  Stg:= Num2Stg(twElSamples);
  WriteStrings2('Measurements per point',Stg);
  WriteIntegers([twElSampleMs],'Sampling time [ms]:');
  WriteStrings2('HV type'               ,twElHVType);
  WriteIntegers([Round(twElChannels[FieldCh].twNorm)         ,Round(twElChannels[RefCh].twNorm       )],
                'Normalisation value, field and reference:');
  WriteIntegers([Round(twElChannels[FieldCh].twDarkCurrent  ),Round(twElChannels[RefCh].twDarkCurrent)],
                'Dark current, field and reference:');
  WriteIntegers([Round(twElChannels[FieldCh].twHV           ),Round(twElChannels[RefCh].twHV         )],
                'High voltage, field and reference [V]:');
  WriteIntegers([twElChannels[FieldCh].twGain                ,twElChannels[RefCh].twGain              ],
                'Gain, field and reference:');
  Append(Format('Range, field and reference: %s  %s',
                [twElChannels[FieldCh].twRange               ,twElChannels[RefCh].twRange             ]));
  Append('');
  Append('Servo');
  WriteStrings2('Servo type',twDeviceName);
  WriteReals1([twDeviceSpeed_mm_s     ] ,'Scan Speed [mm/s]');
  WriteReals2([twDeviceWaterSurface_cm] ,'Water surface correction');
  WriteReals2([twDeviceWaterOffset_cm ] ,'Water offset correction');
  Stg:= 'Servo axis [I, C, D]:';
  for mAxis:= Inplane to Beam do
    begin   {calculate effects of shift here}
    tAxis           := twDeviceMappingICD[mAxis];
    Stg             := Format('%s %s%s',[Stg,ifthen(twDeviceDirXYZ.c[tAxis]=1,chSpace,'-'),tAxis]);
    tOrigin.c[tAxis]:= twDeviceOriginXYZ_cm.c[tAxis];
    end;
  WriteReals1(tOrigin.t          ,'Origin [X, Y, Z]');
  Append(Stg);
  WriteReals2(twDeviceIsocICD_cm.m,'Isocentre [I, C, D]'    ,True);
  WriteReals2(twDeviceNormICD_cm.m,'Normalisation [I, C, D]',True);
  Append('');
  for c:= 'A' to 'D' do
    WriteReals2(twDeviceRefPosition_cm[c].m,'Position '+c+' [I, C, D]');
  WriteIntegers([GetSourceNumPoints(ASource)]     ,'Number Of Points:');
  WriteReals1(wSource[ASource].twVector_ICD_cm[Start].m,'Start Point [I,C,D]',True);
  WriteReals1(wSource[ASource].twVector_ICD_cm[Stop ].m ,'End Point [I,C,D]' ,True);
  Append('Points [cm]:	Inline	Crossline	Depth	Relative Dose');
  for i:= 0 to Pred(twPoints) do
    WritePoint(twCoordinates[i],twData[i]);
  WriteIntegers([Round(ScanAngle)],'Scan Angle:');
  if Length(twExtraText)>0 then
    for i:= 0 to Pred(Length(twExtraText)) do
      Append('# '+twExtraText[i]);
  end;
Result:= True;
end; {~writedata}
{$pop}


{13/07/2018 statusmessage}
function TWellhoferData.WriteData(AFileName :String;
                                  OutPutType:twcFileType;
                                  ASource   :twcDataSource=dsMeasured;
                                  SetExt    :Boolean=True          ): Boolean;
var w: TWmsData;
    r: TRfaProfileData;
begin
case OutPutType of
  twcWellhoferAscii_v6:
    Result:= WriteData(AFileName,False,ASource,SetExt);
  twcMccProfile:
    begin
    if not assigned(FMcc) then
      FMcc:= TMccProfileData.Create;
    ExportMccProfile(FMcc,ASource);
    Result   := FMcc.WriteData(AFileName,OutPutType,ASource,SetExt);
    FFileName:= FMcc.FileName;
    end;
  twcRFA_ascii:
    begin
    r:= TRfaProfileData.Create;
    ExportRfaProfile(r,ASource);
    Result   := r.WriteData(AFileName,OutPutType,ASource,SetExt);
    FFileName:= r.FileName;
    try
      FreeAndNil(r);
     except
      ExceptMessage('WH.WriteData:r!');
     end;
    end;
  twcWTX,twcWDA:
    begin
    w:= TWmsData.Create;
    ExportWmsProfile(w,ASource);
    Result   := w.WriteData(AFileName,OutPutType,ASource,SetExt);
    FFileName:= w.FileName;
    try
      FreeAndNil(w);
     except
      ExceptMessage('WH.WriteData:w!');
     end;
    end;
 else
    Result:= False;
 end;
if Result then
  StatusMessage(Format('stored: %s (%s)',[FFilename,twcDataSourceNames[ASource]]),False)
end; {~writedata}


{20/07/2015 twcCoordinate also shifted}
{20/12/2015 twTopModel}
{25/07/2016 twFFFslope}
{05/12/2017 sigmoiddata}
{27/01/2018 sync reffiltered, twAbsNormDefUse
            when twAbsNormPosCm=0 do not shift this value, but recalculate twAbsNormval instead}
{28/01/2018 twcCoupledSources}
{15/05/2020 twFirstScanPosCm,twLastScanPosCm}
{17/06/2020 dSigmoid50 as new last element}
{13/07/2020 twFitOffsetCm}
{27/08/2020 twMaxPosCm}
{17/09/2020 introduction of FFrozen}
{01/10/2020 shift all position elements in sigmoiddata}
procedure TWellhoferData.Shift(cm       :twcFloatType=0;
                               ShiftType:twcShiftType=RelShift;
                               ASource  :twcDataSource=dsMeasured);
var i     : integer;
    p     : twcDoseLevel;
    s     : twcSides;
    mShift: twcCoordinate;
    mAxis : twcMeasAxis;
    mBool : array[twcMeasAxis] of Boolean;
begin
with wSource[ASource] do if twValid and (not FFrozen) then
  begin
  if ShiftType=AbsShift then
    cm:= cm-twShiftCm;
  if (cm<>0) and (twScanLength>0) then
    begin
    for mAxis:= Inplane to Beam do
      begin
      mShift.m[mAxis]:= cm*(twCoordinates[twDataLast].m[mAxis]-twCoordinates[twDataFirst].m[mAxis])/twScanlength;
      mBool[mAxis]   := mShift.m[mAxis]<>0; {for efficiency purposes only}
      end;
    for s:= twcLeft to twcRight do with twFFFslope[s] do
      twFFFoffset:= twFFFoffset+cm*twFFFgain;
    twFFFslopesTop:= twFFFslopesTop+cm;
    with twTopModel do
      begin
      Xtop:= Xtop+cm;
      Xmin:= Xmin+cm;
      Xmax:= Xmax+cm;
      Xavg:= Xavg+cm;
      Qofs:= Qofs+(Qquad*cm-Qlin)*cm;
      Qlin:= Qlin-2*Qquad*cm;
      end;
    twCenterPosCm   := twCenterPosCm   +cm;
    twMaxPosCm      := twMaxPosCm      +cm;
    twFirstDataPosCm:= twFirstDataPosCm+cm;
    twFirstScanPosCm:= twFirstScanPosCm+cm;
    twLastDataPosCm := twLastDataPosCm +cm;
    twLastScanPosCm := twLastScanPosCm +cm;
    for i:= 0 to Pred(twPoints) do
      begin
      twPosCm[i]:= twPosCm[i]+cm;
      for mAxis:= Inplane to Beam do
        if mBool[mAxis] then
          twCoordinates[i].m[mAxis]:= twCoordinates[i].m[mAxis]+mShift.m[mAxis];
      end;
    twVector_ICD_cm[Start]:= twCoordinates[twDataFirst];
    twVector_ICD_cm[Stop ]:= twCoordinates[twDataLast ];
    twPDD10               := twPDD10  +cm;
    twPDD20               := twPDD20  +cm;
    twShiftCm             := twShiftCm+cm;
    if not (ScanType in twcVertScans) then
      begin
      if twAbsNormDefUse=dUseOrigin then
        begin
        twAbsNormValue  := GetQfittedValue(0,ASource);
        twRelNormValue  := twAbsNormValue;
        twAppliedNormVal:= twAbsNormValue;
        end
      else
        begin
        twRelNormPosCm:= twRelNormPosCm+cm;
        twAbsNormPosCm:= twAbsNormPosCm+cm;
        end;
      end;
    for s:= twcLeft to twcRight do
      begin
      twInFieldPosCm[s]:= twInFieldPosCm[s]+cm;
      for p:= dLow to dTemp do
        with twLevelPos[p].Penumbra[s] do
        if Valid then
          Calc:= Calc+cm;
      with twSigmoidFitData[s] do
        if twFitValid then
          begin
          twFitLowCm                                    := twFitLowCm         +cm;
          twFitHighCm                                   := twFitHighCm        +cm;
          twFitScalingPointCm                           := twFitScalingPointCm+Cm;
          twFitOffsetCm                                 := twFitOffsetCm      +cm;
          twFitResult1                                  := twFitResult1       +cm;
          twNMReport.BestVertex[sigmoid_InflectionMajor]:= twNMReport.BestVertex[sigmoid_InflectionMajor]+cm;
          end;
      end; {for sides}
    if ASource in twcFilterSources then
      Shift(cm,ShiftType,twcCoupledSources[ASource]);                           //keep curves in sync
    twFastScan:= False;
    end; {cm<>0}
  end; {valid}
end; {~shift}


{31/07/2015 alignment on basis of valid edges}
{05/08/2015 twAlignedTo intoduced}
{06/08/2015 when already aligned a match is not done again}
{21/06/2017 wRefAlignPeak}
{14/01/2018 both curves should succesfully analysed}
{27/08/2020 twMaxPosCm, twMaxValue}
{17/09/2020 introduction of FFrozen}
procedure TWellhoferData.AlignCurves(ASource   :twcDataSource=dsReference;
                                     AReference:twcDataSource=dsMeasured);
var Cm                 : twcFloatType;
    M                  : Boolean;
    MEdgeType,REdgeType: twcDoseLevel;

    function TestEdge(ASide :twcSides     ): Boolean;
    begin
    Result:= (wSource[ASource   ].twLevelPos[MEdgeType].Penumbra[ASide].Valid and
              wSource[AReference].twLevelPos[REdgeType].Penumbra[ASide].Valid    );
    if Result then
      Cm:= wSource[AReference].twLevelPos[REdgeType].Penumbra[ASide].Calc-
           wSource[ASource   ].twLevelPos[MEdgeType].Penumbra[ASide].Calc;
    end;

begin
if (not FFrozen)                                    and
   (not (wCurveInfo.twDesScanType in twcVertScans)) and
    Analyse(ASource) and Analyse(AReference) then
  begin
  M := FMatchOverride;
  Cm:= 0;
  if not M then
    begin
    MEdgeType:= wSource[ASource   ].twUsedEdgeLevel;
    REdgeType:= wSource[AReference].twUsedEdgeLevel;
    if wRefAlignPeak and (wSource[ASource].twSetFieldType=fcFFF) and (wSource[AReference].twSetFieldType=fcFFF) then
      Cm:= wSource[AReference].twMaxPosCm-wSource[ASource].twMaxPosCm
    else if wSource[AReference].twCenterPosValid and wSource[ASource].twCenterPosValid then
      Cm:= wSource[AReference].twCenterPosCm-wSource[ASource].twCenterPosCm
    else
      M:= not (TestEdge(twcLeft) or TestEdge(twcRight));
    end;
  if M and (not (wSource[ASource].twAlignedTo=AReference))then
    Cm:= Match(ASource,AReference,RelShift,False);  {for matching no analysis is needed}
  Shift(Cm,RelShift,ASource);               {cm has to be defined already at this stage}
  wSource[ASource].twAlignedTo:= AReference;                    {set alignment relation}
  end;
end; {~aligncurves}


{02/05/2020 twcCoupledSources applied}
{17/09/2020 introduction of FFrozen}
procedure TWellhoferData.SubtractBackground(AbsoluteValue:twcFloatType =0;
                                            ASource      :twcDataSource=dsMeasured);
var i: Integer;
    b: twcFloatType;
begin
with wSource[ASource] do if twValid and (not FFrozen) then
  begin
  if ASource in twcFilterSources then
    wSource[twcCoupledSources[ASource]].twValid:= False;
  b           := AbsoluteValue-twBackGround;
  twBackGround:= AbsoluteValue;
  if b<>0 then
    begin
    twFastScan:= False;
    for i:= twDataFirst to twDataLast do
      twData[i]:= twData[i]-b;
    end;
  end;
end; {~subtractbackground}


{$push}{$warn 5091 off: n not initialised}
{02/05/2020 twcCoupledSources applied}
{17/09/2020 introduction of FFrozen}
function TWellhoferData.OD2doseConversion(PreferedModality:String      ='';
                                          PreferedFilmType:String      ='';
                                          ASource         :twcDataSource=dsMeasured): Boolean;
var i: Integer;
    r: TModalityFilm;
    t: twcFloatType;
    n: twcFloatArray;
begin
Result:= not FFrozen;
if Result then
  with wSource[ASource],twBeamInfo do
    begin
    r:= nil;
    if PreferedModality='' then
      PreferedModality:= FModFilmList.ModalityFormat(twBModality,twBEnergy);
    Result:= twValid and (not tw2DoseConv) and
       FModFilmList.FindModData(PreferedModality,PreferedFilmType,r);
    if Result then
      begin
      if ASource in twcFilterSources then
        wSource[twcCoupledSources[ASource]].twValid:= False;
      with r.FilmRec do if OD2dose[6]>0 then
        begin
        twFastScan   := False;
        tw2DoseConv  := True;
        twOD2doseFilm:= FilmType;
        SetLength(n,twPoints);
        for i:= twDataFirst to twDataLast do
          try
          t   := twData[i]/OD2dose[6];
          n[i]:= (((OD2dose[5]*t+OD2dose[4])*t+OD2dose[3])*t+OD2dose[2])*t+OD2dose[1];
          except
            tw2DoseConv:= False;
          end;
        if tw2DoseConv then
          twData:= copy(n,0,twPoints);
        Result:= tw2DoseConv;
        end;
      end; {result}
    end;
end; {~od2doseconversion}
{$pop}


{17/09/2015 tPtr^.twComposite:= True}
{01/12/2015 nicestep 3 toegevoegd}
{28/01/2018 twcCoupledSources}
{27/04/2020 InitCurve}
{17/09/2020 introduction of FFrozen}
procedure TWellhoferData.Resample(StepCm      :twcFloatType;
                                  ASource     :twcDataSource=dsMeasured;
                                  ADestination:twcDataSource=dsCalculated);
var i,j,n : Integer;
    x,l,p0: twcFloatType;
    dx    : twcCoordinate;
    tPtr  : twCurveDataPtr;
    t     : twcMeasAxis;
    DeqA  : boolean;
begin
DeqA:= (ADestination=ASource);
if (not FFrozen) and wSource[ASource].twValid then
  begin
  if wSource[ASource].twResampled then
    begin
    if not DeqA then
      CopyCurve(ADestination,ASource);
    end
  else
    begin
    if DeqA then
      begin
      New(tPtr);
      InitCurve(tPtr^);
      ClearCurve(tPtr^,False);
      end
    else
      tPtr:= @wSource[ADestination];
    with wSource[ASource] do
      begin
      twVector_ICD_cm[Start]:= twCoordinates[twDataFirst];
      twVector_ICD_cm[Stop ]:= twCoordinates[twDataLast];
      if StepCm<=0 then
        StepCm:= Max(0.001,twStepSizeCm);
      x            := NiceStep(StepCm,[1,2,3,4,5]);
      if Abs(x/StepCm-1)<0.01 then
        StepCm:= x;
      twStepSizeCm := StepCm;
      j            := Ceil(twFirstDataPosCm/StepCm);
      n            := Floor(twLastDataPosCm/StepCm)-j;
      l            := twLastDataPosCm-twFirstDataPosCm;
      p0           := twFirstDataPosCm;
      for t:= Inplane to Beam do
        dx.m[t]:= twVector_ICD_cm[Stop ].m[t]-twVector_ICD_cm[Start].m[t];      //obtain travel length on each axis
      end;
    CopyParameters(wSource[ASource],tPtr^);
    CheckSize(tPtr^,Succ(n));
    tPtr^.twDataFirst:= 0;
    tPtr^.twDataLast := n;
    for i:= 0 to n do
      begin
      x               := (i+j)*StepCm;
      for t:= Inplane to Beam do
        tPtr^.twCoordinates[i].m[t]:= wSource[ASource].twVector_ICD_cm[Start].m[t]+((x-p0)/l)*dx.m[t];
      tPtr^.twPosCm[i]:= x;
      tPtr^.twData[i] := GetQfittedValue(x,ASource);
      end;
    tPtr^.twResampled           := True;
    tPtr^.twComposite           := True;
    tPtr^.twScanLength          := tPtr^.twPosCm[n]-tPtr^.twPosCm[0];
    tPtr^.twVector_ICD_cm[Start]:= tPtr^.twCoordinates[0];
    tPtr^.twVector_ICD_cm[Stop ]:= tPtr^.twCoordinates[n];
    tPtr^.twRelatedSource       := ASource;
    if DeqA then
      begin
      CopyCurve(tPtr^,wSource[ASource]);
      ClearCurve(tPtr^,True);
      Dispose(tPtr);
      end;
    end; {resampling}
  if ADestination in twcFilterSources then
    wSource[twcCoupledSources[ADestination]].twValid:= False;
  end;
end; {~resample}


{20/07/2015 twComposite added}
{05/10/2015 QuadFilter added again}
{28/01/2018 twcCoupledSources}
{03/06/2018 initborders}
{17/09/2020 introduction of FFrozen}
procedure TWellhoferData.Add(ASource1      :twcDataSource=dsMeasured;
                             ASource2      :twcDataSource=dsReference;
                             ADestination  :twcDataSource=dsCalculated;
                             Source2Scaling:twcFloatType =1);
var t  : twcMeasAxis;
    i,n: Integer;
    dx : twcCoordinate;
begin
if (not FFrozen)                                           and
   wSource[ASource1].twValid and wSource[ASource2].twValid and
   (not (ADestination in [ASource1,ASource2])) then
  begin
  CopyCurve(ASource1,ADestination);
  InitBorders(ADestination);
  with wSource[ADestination] do
    begin
    n:= Max(1,Pred(twPoints));
    for t:= Inplane to Beam do
      dx.m[t]:= (twVector_ICD_cm[Stop ].m[t]-twVector_ICD_cm[Start].m[t])/n;
    while twFirstDataPosCm>wSource[ASource2].twFirstDataPosCm do
      begin
      AddPoints(ADestination,1,True);
      for i:= Max(0,twDataFirst)+1 downto 1 do
         begin
         twPosCm[i-1]:= twPosCm[i]-twStepSizeCm;
         for t:= Inplane to Beam do
           twCoordinates[i-1].m[t]:= twCoordinates[i].m[t]-dx.m[t];
         end;
      Inc(twDataLast);
      twFirstDataPosCm:= twPosCm[twDataFirst];
      twLastDataPosCm := twPosCm[twDataLast];
      end;
    while twLastDataPosCm<wSource[ASource2].twLastDataPosCm do
      begin
      AddPoints(ADestination);
      n:= Pred(twPoints);
      for i:= Min(twDataLast+1,Pred(twPoints)) to Pred(twPoints) do
        begin
        twPosCm[i]:= twPosCm[Pred(i)]+twStepSizeCm;
        for t:= Inplane to Beam do
          twCoordinates[i].m[t]:= twCoordinates[Pred(i)].m[t]+dx.m[t];
        end;
      Inc(twDataLast);
      twLastDataPosCm:= twPosCm[twDataLast];
      end;
    twDataLast      := n;
    twDataHistoryStg:= wSource[ASource1].twDataHistoryStg+'+'+wSource[ASource2].twDataHistoryStg;
    for n:= 0 to twDataLast do
      twData[n]:= GetQfittedValue(twPosCm[n],ASource1)+GetQfittedValue(twPosCm[n],ASource2)*Source2Scaling;
    QuadFilter(-1,ADestination,ADestination,False,True);
    twComposite    := True;
    twRelatedSource:= ASource1;
    if ADestination in twcFilterSources then
      wSource[twcCoupledSources[ADestination]].twValid:= False;
    end; {with}
  end;
end; {~add}


(* NormalisedCostFunction theory
   |             s
105| s  sssss  ss ss
   |  ss     ss
   |  rrr   rrr   rrr
100| r   rrr   rrr
   |
   _________________________

  The height of sss to rrr is determined by NormFactor.
  For a too large value of NormFactor the sum of differences (over all points)
  sss-rrr will be positive and for a too low value negative.
  By changing NormFactor from a (almost) neutral level in equal steps up and down, Nhigh and Nlow are found.

  costfunction
   |              * Nhigh (y=Yh, x=1+delta)
   |            .
   |          .
   _________.______________x-axis (normfactor)
   |      .
   |    * Nlow (y=Y1, x=1-delta)

   The crossing with the x-axis (x0) can now be found through linear interpolation.
   x0 = 2*delta/ (1 - (Yh/Yl)) + 1 - delta.
   For delta a value of 0.02 is chosen.
*)
{14/07/2015
  Improved initial values.
  Limited match area.
  Error normalised to number of matched points.
  Introduced match range override.}
{15/08/2015
  GetInterpolatedValue used}
{31/08/2015
  Removed matchmassage call before loop because of not initialised shiftstep}
{18/01/2017
  reduced initial ShiftRange
  twcMatchInclusionLimit}
{09/02/2017
  reviewed ShiftRange, minimal value>2
  reviewed initial result}
{11/02/2017 with missinge penumbra ignore maximum and use mid of scans to initialise}
{21/06/2017 replaced twDataFirst/Last with twScanFirst/Last to avoid non-useful data}
{20/11/2018 until (Abs(s)<=twcMaxRelMatchDif) or (twScanLast-j-twScanFirst-i<30)}
{17/09/2020 introduction of FFrozen}
function TWellhoferData.Match(ASource    :twcDataSource=dsReference;
                              AReference :twcDataSource=dsMeasured;
                              ResultType :twcShiftType=AbsShift;
                              AutoCorrect:Boolean     =True;
                              MatchLimitL:twcFloatType=0;
                              MatchLimitR:twcFloatType=0): twcFloatType;
var s,ShiftCostFunctionStep,
    ShiftError,ShiftRange,ShiftStep,
    minError,NormValue,OriginalShift: twcFloatType;
    MatchOk                         : Boolean;
    i,j                             : Integer;

  procedure MatchMessage(AShift,CostFunctionResult:twcFloatType);
  begin
  StatusMessage(Format(twForMatch,[AShift,Min(CostFunctionResult,10),ShiftStep]));
  end;

  function GetPos(ACurve :twcDataSource;
                  LeftPos:Boolean     ): twcFloatType;
  begin
  with wSource[ACurve] do
    Result:= twPosCm[ifthen(LeftPos,twScanFirst,twScanLast)];
  end;

  {27/04/2020 NormValue was used instead of vertnorm}
  function ShiftCostFunction(VertNorm:twcFloatType;
                             Signed  :Boolean): twcFloatType;
  var n                  : Integer;
      Dif,p,v1,v2,l,m1,m2: twcFloatType;
  begin
  if MatchOk then
    begin
    Dif:= 0;
    n  := 0;
    p  := MatchLimitL;
    l  := 0;
    m1 := twcMatchInclusionLimit*wSource[ASource   ].twMaxValue;
    m2 := twcMatchInclusionLimit*wSource[AReference].twMaxValue;
    repeat
      v1:= GetQfittedValue(p,ASource,0);
      if v1>m1 then
        begin
        Inc(n);
        try
          v2:= GetInterpolatedValue(p,AReference);
          if v2>m2 then
            begin
            Inc(n);
            v1:= v1*VertNorm/v2-1;
            end
          else
            v1:= 0;
         except
          v1:= 0;
         end;
        if Signed then Dif:= Dif+v1
        else           Dif:= Dif+abs(v1);
        l:= l+ShiftCostFunctionStep;
        end;
      p:= p+ShiftCostFunctionStep;
    until p>=MatchLimitR;
    if (n<2) or (MatchLimitR=MatchLimitL) or (l/(MatchLimitR-MatchLimitL)<0.5) then
      begin
      Result:= 1000;
      end
    else
      try
        Result:= 1000*Dif/n;
       except
        MatchOk:= False;
        Result := 0;
       end;
    end
  else
    Result:= 0;
  end; {shiftcostfunction}

  function NormalisedShiftCostFunction(AShift:twcFloatType): twcFloatType;
  var NormDelta,NormFactor: twcFloatType;
      C                   : array[0..1] of twcFloatType;
  begin
  NormDelta:= twcMatchNormDeltaPercent/100;
  Shift(OriginalShift+AShift,AbsShift,ASource);
  try
    C[1]:= ShiftCostFunction(NormValue*(1+NormDelta),True);
    C[0]:= ShiftCostFunction(NormValue*(1-NormDelta),True);
    if (C[1]=C[0]) or (C[0]=0) then NormFactor:= 1
    else                            NormFactor:= 2*NormDelta/(1-(C[1]/C[0])) + 1 - NormDelta;
    Result:= ShiftCostFunction(NormValue*NormFactor,False);
   except
    Result:= 1;
   end;
  if LogLevel>2 then
    MatchMessage(AShift+ifthen(ResultType=AbsShift,OriginalShift,0),0{Result});
  end; {normalisedshiftcostfunction}

begin {all positions here are relative, the costfunction shifts are absolute, including OriginalShift}
Result:= 0;
if not FFrozen then
  begin
  Inc(FActiveCnt);
  OriginalShift:= wSource[ASource].twShiftCm;
  ShiftStep    := 0;
  if (MatchLimitL=0) and (MatchLimitR=0) then
    with wSource[AReference] do
      begin
      i            := 0;
      j            := 0;
      repeat
        MatchLimitL:= twData[twScanFirst+i];  {first inspect and limit vertical range}
        MatchLimitR:= twData[twScanLast -j];
        s          := MatchLimitL-MatchLimitR;
        if Abs(s)>twcMaxRelMatchDif then
          begin
          if s>0 then Inc(j)
          else        Inc(i);
          end;
      until (Abs(s)<=twcMaxRelMatchDif) or (twScanLast-j-twScanFirst-i<30);
      MatchLimitL:= twPosCm[twScanFirst+i];  {now adjust horizontal range}
      MatchLimitR:= twPosCm[twScanLast -j];
      end;
  ShiftCostFunctionStep:= Max(0.000001,Abs(Min(wSource[ASource].twStepSizeCm,wSource[AReference].twStepSizeCm)/2));
  ShiftRange           := Min(Abs(wSource[ASource].twLastDataPosCm -wSource[AReference].twLastDataPosCm),
                              Abs(wSource[ASource].twFirstDataPosCm-wSource[AReference].twFirstDataPosCm));
  try
    NormValue          := wSource[AReference].twAppliedNormVal/wSource[ASource].twAppliedNormVal;
   except
    NormValue          := 1;
   end;
  MatchOk              := (ShiftRange>0) and ((MatchLimitR-MatchLimitL)/ShiftCostFunctionStep>5);
  if wSource[ASource   ].twLevelPos[d50].Penumbra[twcLeft].Valid and
     wSource[AReference].twLevelPos[d50].Penumbra[twcLeft].Valid     then
    Result:= wSource[AReference].twLevelPos[d50].Penumbra[twcLeft].Calc-
             wSource[ASource   ].twLevelPos[d50].Penumbra[twcLeft].Calc
  else if wSource[ASource   ].twLevelPos[d50].Penumbra[twcRight].Valid and
          wSource[AReference].twLevelPos[d50].Penumbra[twcRight].Valid     then
    Result:= wSource[AReference].twLevelPos[d50].Penumbra[twcRight].Calc-
             wSource[ASource   ].twLevelPos[d50].Penumbra[twcRight].Calc
  else
    begin
    Result    := (MatchLimitL+MatchLimitR-GetPos(ASource,True)-GetPos(ASource,False))/2;
    minError  := NormalisedShiftCostFunction(Result);
    ShiftError:= NormalisedShiftCostFunction(0); {check if no shift is better result}
    if ShiftError<minError then
      Result:= 0;
    end;
  minError:= NormalisedShiftCostFunction(Result);
  repeat
    ShiftRange:= ShiftRange/twcMatchRangeDivider;
    ShiftStep := ShiftRange/twcMatchStepsNumber;
    s         := Result-ShiftRange;
    repeat
      ShiftError:= NormalisedShiftCostFunction(s);
      if ShiftError<minError then
        begin
        minError := ShiftError;
        Result   := s;
        end;
      s:= s+ShiftStep;
    until (s>Result+ShiftRange) or (not MatchOk);
    MatchMessage(Result,MinError);
  until (ShiftStep<ShiftCostFunctionStep) or (not MatchOk);
  if not MatchOk then
    Result:= 0;
  Shift(OriginalShift+ifthen(AutoCorrect,Result,0),AbsShift,ASource);
  Result:= Result+ifthen(ResultType=AbsShift,OriginalShift,0);
  Dec(FActiveCnt);
  end;
end; {~match}


{12/06/2018 twLinSlope, twPlotScaling, twFFFslope, twTopModel}
{11/06/2020 issues with Mult on extended type}
procedure TWellhoferData.Multiply(Factor      :twcFloatType;
                                  ASource     :twcDataSource=dsCalculated;
                                  ADestination:twcDataSource=dsCalculated);
var i: Integer;
    s: twcSides;

  procedure Mult(var X:twcFloatType);
  begin
  X:= X*Factor;
  end;

begin
if not FFrozen then
  begin
  if ASource<>ADestination then
    CopyCurve(ASource,ADestination);
  with wSource[ADestination] do
    begin
    for i:= twDataFirst to twDataLast do
      Mult(twData[i]);
    Mult(twAvgNormValue);
    Mult(twAbsNormValue);
    Mult(twRelNormValue);
    Mult(twPlotScaling);
    Mult(twAppliedNormVal);
    Mult(twLinSlope);
    Mult(twMaxValue);
    for s:= twcLeft to twcRight do
      with twFFFslope[s] do
        if twFFFvalid then
          begin
          Mult(twFFFoffset);
          Mult(twFFFgain);
          end;
    twTopModel.Qofs := twTopModel.Qofs *Factor;
    twTopModel.Qlin := twTopModel.Qlin *Factor;
    twTopModel.Qquad:= twTopModel.Qquad*Factor;
    twRelatedSource := ASource;
    end;
  end;
end; {~multiply}


{26/01/2018 assures initialised source}
{28/01/2018 twcCoupledSources}
function TWellhoferData.CheckAlternativeSource(ASource    :twcDataSource;
                                               AssureValid:Boolean=False): twcDataSource;

  function SubCheck(Source,Proposition,Replacement:twcDataSource): twcDataSource;
  begin
  if Source=Proposition then
    begin
    Result:= Replacement;
    if AssureValid and (not wSource[Replacement].twValid) then
      CopyCurve(Proposition,Replacement);
    end
  else
    Result:= Source;
  end;

begin
Result:= SubCheck(SubCheck(ASource,dsMeasured,twcCoupledSources[dsMeasured]),dsReference,twcCoupledSources[dsReference]);
end; {~checkalternativesource}


{04/12/2016
 Output is written to Calculated}
{18/12/2016 apply quadfilter instead of median filter}
{25/11/2018 autoscaling}
{17/09/2020 introduction of FFrozen}
function TWellhoferData.SyntheticProfile(ASource    :twcDataSource=dsMeasured;
                                         Divisor    :twcDataSource=dsReference;
                                         AutoScaling:Boolean     =True;
                                         PreFilter  :Boolean     =True;
                                         PostFilter :Boolean     =True): Boolean;
const OfsCm=2;
var IgnoreSet: twcIgnoreSet;

  function GetBorderPos(tSource:twcDataSource;
                        tSide  :twcSides;
                        Offset :twcFloatType): twcFloatType;
  begin
  with wSource[tSource] do
    Result:= twLevelPos[twUsedEdgeLevel].Penumbra[tSide].Calc+OffSet;
  end;

begin
Result:= (not FFrozen) and wSource[ASource].twValid and wSource[Divisor].twValid;
if Result then
  begin
  Inc(FActiveCnt);
  IgnoreSet:= [twiFieldSize,twiDiagonal];
  if wCheckRefIgnoreLinac then IgnoreSet:= IgnoreSet+[twiLinac];
  if (MakeCurveName(False,True,IgnoreSet,True,ASource)=MakeCurveName(False,True,IgnoreSet,True,Divisor)) and
     (GetEquivalentField(ASource)<GetEquivalentField(Divisor)) then
    begin
    wSource[Divisor].twFirstDataPosCm:= Max(GetBorderPos(ASource,twcLeft ,-OfsCm),GetBorderPos(Divisor,twcLeft , OfsCm));
    wSource[Divisor].twLastDataPosCm := Min(GetBorderPos(ASource,twcRight, OfsCm),GetBorderPos(Divisor,twcRight,-OfsCm));
    Result:= Divide(ASource,Divisor,dsCalculated,AutoScaling,1,PreFilter,PostFilter,False); {output in calculated}
    end
  else
    Result:= False;
  Dec(FActiveCnt);
  end;
end; {~syntheticprofile}


{$push}{$warn 5091 off: Local variable "tmpDCurve" of a managed type does not seem to be initialized}
(*
*********  BistroMath core function **********
 output is written to Calculated
 twFirstPosCm and twlastPosCm of divisor can be used to limit range
*)
{20/07/2015 twComposite added}
{28/07/2015:
  Add curly brackets to contributing history strings when
  it is already composite.}
{15/11/2016 repaired div0 problem}
{05/12/2016 use 0 as lower limit for division}
{20/06/2017 twScanFirst/Last adapted to twFirstPosCm/twlastPosCm}
{06/12/2017 clipping corrected (ul)}
{27/12/2017 make use of measfiltered when possible}
{27/01/2018 use dsRefFiltered}
{29/03/2018 decrease twScanLast for clipped data on end}
{03/06/2018 initborders}
{25/11/2018 autoscaling}
{28/04/2020 initcurve}
{29/04/2020 asource used instead of tsource in needbackup}
{21/07/2020 GetAdjustedFilterWidthCm}
{29/07/2020 MedianFilter called with wrong source}
function TWellhoferData.Divide(ASource     :twcDataSource=dsMeasured;
                               ADivisor    :twcDataSource=dsReference;
                               ADestination:twcDataSource=dsCalculated;
                               AutoScaling :Boolean     =True;
                               NormFactor  :twcFloatType=1;
                               PreFilter   :Boolean     =True;
                               PostFilter  :Boolean     =True;
                               IsRelative  :Boolean     =True): Boolean;
var i,j                 : Integer;
    f,p,pmin,pmax,q,r,ul: twcFloatType;
    Sbackup,Dbackup     : Boolean;
    dPtr                : twCurveDataPtr;
    fSource,fDivisor    : twcDataSource;
    tmpSCurve,tmpDCurve : twCurveDataRec;
    {$IFDEF THREADED}
    MathThreadList      : twMathThreadList;
    {$ENDIF}

  function NeedBackup(tSource:twcDataSource): Boolean;
  begin
  Result:= PreFilter and (not (tSource in twcFilterSources+twcFilteredCopies));
  end;

  function GetHistoryString(tSource:twcDataSource): String;
  begin
  with wSource[tSource] do
    Result:= ifthen(twComposite,'{','')+twDataHistoryStg+ifthen(twComposite,'}','')
  end;

  procedure PreEmptiveFilter(tSource,tDest:twcDataSource);
  begin
  if not (tSource in twcFilteredCopies)  then
    QuadFilter(GetAdjustedFilterWidthCm(tSource),tSource,tDest);     {repeatedfiltering is assumed false}
  end;

begin
Result:= FFrozen;
if not Result then
  begin
  fSource := CheckAlternativeSource(ASource ,True);  {assures initialised source}
  fDivisor:= CheckAlternativeSource(ADivisor,True);
  InitCurve(tmpDCurve);
  InitCurve(tmpSCurve);
  {$IFDEF COMPILED_DEBUG}
  if LogLevel>1 then
    begin
    if not IsValid                              then FailInfo:= 'invalid source'
    else if not wSource[ADivisor].twValid       then FailInfo:= 'invalid divisor'
    else if ASource=fDivisor                    then FailInfo:= 'source=divisor'
    else if fSource=fDivisor                    then FailInfo:= 'fsource=divisor'
    else if wSource[ASource ].twAvgNormValue<=0 then FailInfo:= 'source normvalue'
    else if wSource[ADivisor].twAvgNormValue<=0 then FailInfo:= 'divisor normvalue'
    else                                             FailInfo:= '';
    end;
  {$ENDIF}
  Result:= IsValid and wSource[ADivisor].twValid and (ASource<>ADivisor) and
           (ASource<>fDivisor) and (fSource<>ADivisor)                   and
           (wSource[ASource].twAvgNormValue>0) and (wSource[ADivisor].twAvgNormValue>0);
  {$IFDEF COMPILED_DEBUG}
  FailInfo:= ifthen(Result,'start','entrance');
  {$ENDIF}
  if Result then
    begin
    Inc(FActiveCnt);
    Sbackup  := NeedBackup(ASource);
    Dbackup  := False;
    if ADivisor=ADestination then
      begin
      New(dPtr);
      InitCurve(dPtr^);
      ClearCurve(dPtr^,False);
      end
    else
      begin
      dPtr   := @wSource[ADivisor];
      Dbackup:= NeedBackup(ADivisor);
      if Dbackup then                                {for dsReference take dsRefFiltered}
        CopyCurve(wSource[ADivisor],tmpDCurve,True); {make backup}
      end;
    if NormFactor<10 then
      NormFactor:= 100*Abs(Max(1,NormFactor));
    if AlignReference and (not (wSource[ADivisor].twAlignedTo in twcFilterSources+twcFilteredCopies-[ADivisor])) then
      AlignCurves(ADivisor,ASource);
    if PreFilter then
      begin
      {$IFDEF COMPILED_DEBUG}
      FailInfo:= 'prefilter';
      {$ENDIF}
      if Sbackup then
        CopyCurve(wSource[ASource],tmpSCurve,True);  {make backup of source}
      {$IFDEF THREADED}
      if (not ((fSource in twcFilteredCopies) and (fDivisor in twcFilteredCopies))) and
         (twNumCpu>1) then {both need filtering, threading meaningful}
        begin
        {$IFDEF COMPILED_DEBUG}
        FailInfo:= 'threaded filter';
        {$ENDIF}
        SetLength(MathThreadList,Min(2,twNumCPU));
        for i:= 0 to High(MathThreadList) do
          MathThreadList[i]:= TMathThread.Create(Self);
        MathThreadList[0].QuadFitWork(GetAdjustedFilterWidthCm(fSource ),fSource);
        MathThreadList[1].QuadFitWork(GetAdjustedFilterWidthCm(fDivisor),fDivisor);
        for i:= 0 to 1 do
          with MathThreadList[i] do
            if FActive then
              WaitFor;
        for i:= 0 to High(MathThreadList) do
          try
            FreeAndNil(MathThreadList[i]);
           except
            ExceptMessage('WH.Divide!');
           end;
        Finalize(MathThreadList);
        end {both needed filtering}
      else                                             {probably both are already done}
        begin
        PreEmptiveFilter(ASource ,fSource );
        PreEmptiveFilter(ADivisor,fDivisor);
        end;
      {$ELSE}
      PreEmptiveFilter(ASource ,fSource );
      PreEmptiveFilter(ADivisor,fDivisor);
      {$ENDIF}
      end;
    if aDivisor=ADestination then CopyCurve(wSource[fSource],dPtr^) {destination starts as aligned copy of fSrc, order critical!}
    else                          CopyCurve(fSource,ADestination);
    {$IFDEF COMPILED_DEBUG}
    FailInfo:= wSource[ADestination].twFileName;
    {$ENDIF}
    try
      if AutoScaling and (wSource[ASource].twAvgNormValue>0) then
        f:= wSource[fDivisor].twAvgNormValue/wSource[ASource].twAvgNormValue
      else
        f:= 1;
     except
      f  := 1;
      {$IFDEF COMPILED_DEBUG}
      FailInfo:= 'norm except';
      {$ENDIF}
     end;
    with wSource[fDivisor] do
      begin
      pmin:= twFirstDataPosCm;
      pmax:= twLastDataPosCm;
      end;
    ul:= twcDefAccDivFactor*NormFactor;
    f := f*NormFactor;
    with wSource[ADestination] do
      begin
      for i:= twDataFirst to twDataLast do {-------------------------main calculation------------------}
        try
          p:= twPosCm[i];
          r:= GetQfittedValue(p,fDivisor);
          if InRange(p,pmin,pmax) and (r>0) then
            begin
            q:= f*twData[i]/r;
            if q<ul then twData[i]:= Max(0,q)
            else         twData[i]:= 0;
            end
          else           twData[i]:= 0;
        except
          twData[i]:= 0;
        end;
      i:= twDataFirst;
      while (twData[i]=0) and (i<twDataLast) do
        Inc(i);
      j:= twDataLast;
      while (twData[j]=0) and (j>i)          do
        Dec(j);
      twScanFirst    := i;
      twScanLast     := j;
      end; {with Destination}
    if Sbackup then
      begin
      CopyCurve(tmpSCurve,wSource[ASource]);  {restore source}
      ClearCurve(tmpSCurve,True);
      end;
    if ADivisor=ADestination then
      begin
      ClearCurve(dPtr^,True);
      Dispose(dPtr);
      end
    else if dBackup then
      begin
      CopyCurve(tmpDCurve,wSource[ADivisor]);  {restore divisor}
      ClearCurve(tmpDCurve,True);
      end;
      {$IFDEF MEDIAN_SPEEDTEST}
       j:= MilliSecondOfTheDay(Now);
       for i:= 1 to 10000 do MedianFilter(FilterWidth,Destination,Buffer,False,True);
       StatusMessage(Num2Stg(round((MilliSecondOfTheDay(Now)-j)/10),0)+' ms per 1000 calculations');
      {$ENDIF}
    if PostFilter then
      MedianFilter(GetAdjustedFilterWidthCm(fDivisor),ADestination,ADestination,False,True);
    with wSource[ADestination] do
      begin
      twDataHistoryStg:= GetHistoryString(ADestination)+'/'+GetHistoryString(ADivisor);
      twComposite     := True;
      twFastScan      := False;
      twAnalysed      := False;
      twIsRelative    := IsRelative;
      twDataFirst     := twScanFirst;
      twDataLast      := twScanLast;
      twFirstDataPosCm:= twPosCm[twScanFirst];
      twFirstScanPosCm:= twFirstDataPosCm;
      twLastDataPosCm := twPosCm[twScanLast ];
      twLastScanPosCm := twLastDataPosCm;
      twRelatedSource := ASource;
      end;
    InitBorders(ADestination);
    Result:= Result and Analyse(ADestination);
    {$IFDEF COMPILED_DEBUG}
    FailInfo:= 'completed';
    {$ENDIF}
    wSource[ADestination].twAvgNormValue:= 100*wSource[ADestination].twAvgNormValue/NormFactor;
    Dec(FActiveCnt);
    end; {result}
  {$IFDEF COMPILED_DEBUG}
  if (not Result) and (LogLevel>1) then
    StatusMessage(Format('Divide %s failed at %s',[twcDataSourceNames[ASource],FailInfo]),False,1);
  {$ENDIF}
  end;
end; {~divide}
{$pop}


(* Taken from NCS report 18, page 70:
  The water to air mass stopping power ratios vary considerably with depth and in order to
  derive a depth dose curve from a measured depth ionisation curve with an ionisation
  chamber stopping power data as a function of beam quality and depth are needed. Burns et
  al. [31] derived from the same Monte Carlo simulations that led to equation (35) a more
  complicated equation for these data.
  Rogers [74] showed that for a wide range of accelerators these data are accurate to within
  1.0% except at very shallow depths or depths beyond 1.1 z/R50.

  Calculation of R50,dose from R50,ion (page 25):
  When departing from an ionisation curve, R50,dos is derived
  from R50,ion, the 50% ionisation level beyond the ionisation maximum, using the following
  generic expressions [32].

 All depth values should be interpreted as cm.

 [31] Burns D.T., Ding G.X. and Rogers D.W.O., R50 as a beam quality specifier for
      selecting stopping power ratios and reference depths for electron dosimetry, Med.
      Phys. 23 383-388, 1996.
 [32] Ding G.X., Rogers D.W.O., Mackie T.R., Calculation of stopping-power ratios using
      realistic clinical electron beams, Med. Phys. 22 489-501, 1995.
*)
{15/04/2015}
{20/07/2015 twComposite added}
{03/06/2018 initborders}
function TWellhoferData.Ionisation2Dose(ASource     :twcDataSource=dsMeasured;
                                        ADestination:twcDataSource=dsMeasured): Boolean;
const _a=1.075; _b= -0.5087; _c=0.0887; _d=-0.084; _e=-0.4281; _f=0.0646; _g=0.00309; _h=-0.125;
var R50ion,R50dose,ln_R50: twcFloatType;
    i                    : Integer;
begin
Result:= (ScanType in twcVertScans) and (not wSource[ASource].tw2DoseConv) and (BeamType in [Electrons,Other]);
if Result then
  begin
  Analyse(ASource);
  if ADestination<>ASource then
    CopyCurve(ASource,ADestination);
  with wSource[ASource] do
    begin
    R50ion:= twLevelPos[d50].Penumbra[twcRight].Calc;
    if R50ion<=10 then R50dose:= 1.029*R50ion-0.06
    else               R50dose:= 1.059*R50ion-0.37;
    ln_R50:= ln(R50dose);
    for i:= twDataFirst to twDataLast do
      wSource[ADestination].twData[i]:=
        twData[i]*
        (_a+_b*ln_R50+_c*Sqr(ln_R50)                   +_d*twPosCm[i]/R50dose)/
        ( 1+_e*ln_R50+_f*Sqr(ln_R50)+_g*Power(ln_R50,3)+_h*twPosCm[i]/R50dose);
    end;
  wSource[ADestination].twFastScan     := False;
  wSource[ADestination].tw2DoseConv    := True;
  wSource[ADestination].twComposite    := True;
  wSource[ADestination].twRelatedSource:= ASource;
  InitBorders(ADestination);
  Analyse(ADestination);
  end;
end; {~ionisation2dose}


{callback function for search of maximum}
function TWellhoferData.PDDfitMaxErrorResult(var cm:TaFunctionVertex): TaVertexDataType;
begin
with wSource[FNMPddSource] do
  Result:= 2*twMaxValue-TvSpddFunction(twPddFitData[FNMPddFit].twNMReport.BestVertex,cm[0]);
end; {~pddfitmaxerrorresult}


{13/02/2016 mupower decimalen}
{14/06/2017 pddfitOdim variant added for orthovolt/missing max}
{19/02/2018 wrong text for electrons}
function TWellhoferData.FitReport(GiveFormula:Boolean     =False;
                                  ASource    :twcDataSource=dsMeasured;
                                  AReport    :twcNMpddFits=NM_Primary): String;
var n: ^fitNormArray;

  function pars(Apar:Word): String;
  begin
  with wSource[ASource].twPddFitData[AReport] do
    Result:= FloatFormat(ifthen(n^[Apar],twFitNormalisation,1)*twNMReport.BestVertex[Apar],twcNMdigits);
  end;

begin
Result:= '';
with wSource[ASource].twPddFitData[AReport],twNMReport do if FitValid then
 if assigned(BestVertex) then
    case Length(BestVertex) of
     pddfitXdim  : if GiveFormula then
                     Result:= Format('pdd = %s*exp((-%s*m)+%s',[pddfitXnames[pddfit_I1],pddfitXnames[pddfit_mu1],pddfitXnames[pddfit_X_c]])
                   else
                     begin
                     n     := @pddfitXnorm;
                     Result:= Format('I1=%s; mu1=%s; c=%s for depths > %0.1fcm',
                                     [pars(pddfit_I1),pars(pddfit_mu1),pars(pddfit_X_c),twFitLowCm]);
                     end;
     pddfitOdim  : if GiveFormula then
                     Result:= Format('pdd = %s*exp((-%s+(%s+(%s+%s*m)*m)*m)*m)',
                                     [pddfitPnames[pddfit_I1 ],pddfitPnames[pddfit_mu1],pddfitPnames[pddfit_mu2],pddfitPnames[pddfit_mu3],pddfitPnames[pddfit_mu4]])
                   else
                     begin
                     n     := @pddfitPnorm;
                     Result:= Format('I1=%s; mu1..4=%s, %s, %s, %s',
                                     [pars(pddfit_I1),pars(pddfit_mu1),pars(pddfit_mu2),pars(pddfit_mu3),pars(pddfit_mu4)]);
                     end;
     pddfitPdim,
     pddfitPdim+1: if GiveFormula then
                     Result:= Format('pdd = %s*exp((-%s+(%s+(%s+%s*m)*m)*m)*m) - %s*exp(-%s*m^%0.4f)',
                                     [pddfitPnames[pddfit_I1 ],pddfitPnames[pddfit_mu1],pddfitPnames[pddfit_mu2],pddfitPnames[pddfit_mu3],
                                      pddfitPnames[pddfit_mu4],pddfitPnames[pddfit_Ib ],pddfitPnames[pddfit_mub],twcPddFitMubPower])
                   else
                     begin
                     n     := @pddfitPnorm;
                     Result:= Format('I1=%s; mu1..4=%s, %s, %s, %s; Ib=%s; mub=%s',
                                     [pars(pddfit_I1),pars(pddfit_mu1),pars(pddfit_mu2),pars(pddfit_mu3),pars(pddfit_mu4),
                                      pars(pddfit_Ib),pars(pddfit_mub)]);
                     end;
     pddfitEdim  : if GiveFormula then      {I0/(1 + I1*Exp((-mu1+(mu2+(mu3+mu4*(cm-d0))*(cm-d0))*(cm-d0))*(cm-d0))+Ib*Exp(-mub*cm)) + Ix*Exp((-mx1+mx2]*cm/100)*cm/100)}
                     Result:= Format('pdd = %s/( 1 + %s*Exp((-%s+(%s+(%s+%s*(cm-%s))*(cm-%s))*(cm-%s))*(cm-%s)) + %s*Exp(-%s*cm) ) + %s*exp((-%s+%s*cm/100)*cm/100)',
                                     [pddfitEnames[pddfit_I0 ],pddfitEnames[pddfit_I1 ],
                                      pddfitEnames[pddfit_mu1],pddfitEnames[pddfit_mu2],pddfitEnames[pddfit_mu3],pddfitEnames[pddfit_mu4],
                                      pddfitEnames[pddfit_d0 ],pddfitEnames[pddfit_d0 ],pddfitEnames[pddfit_d0 ],pddfitEnames[pddfit_d0 ],
                                      pddfitEnames[pddfit_Ib ],pddfitEnames[pddfit_mub],pddfitEnames[pddfit_Ix ],pddfitEnames[pddfit_mx1],pddfitEnames[pddfit_mx2]])
                   else
                     begin
                     n     := @pddfitEnorm;
                     Result:= Format('I0=%s; I1=%s; mu1..4=%s, %s, %s, %s; d0=%s, Ib=%s; mub=%s; Ix=%s; mx1..2=%s, %s',
                                     [pars(pddfit_I0),
                                      pars(pddfit_I1),pars(pddfit_mu1),pars(pddfit_mu2),pars(pddfit_mu3),pars(pddfit_mu4),
                                      pars(pddfit_d0),
                                      pars(pddfit_Ib),pars(pddfit_mub),
                                      pars(pddfit_Ix),pars(pddfit_mx1),pars(pddfit_mx2)                              ]);
                     end;
      end {case,if}
  else Result:='';
end; {~fitreport}


{16/05/2014
The extrapolation function is designed to model the down slope only with as few
as paramaters as possible:
   Result:= a[fit_I1]*Exp((-a[fit_mu1]+a[fit_mu2]*m)*m)                                     [1]
It should fit data below d50.
It needs extra post correction since it misses an offset like the full model:
   Result:= a[fit_I1]*Exp((-a[fit_mu1]+(a[fit_mu2]+(a[fit_mu3]+a[fit_mu4]*m)*m)*m)*m) -
            a[fit_Ib]*Exp( -a[fit_mub]*m)                                                   [2]
and therefore should be
   Result:= a[fit_I1]*Exp((-a[fit_mu1]+a[fit_mu2]*m)*m) + offset                            [3]
The post correction is done by scaling a[fit_I1] to give identical results for [1] and [2]
in the first data point of the extrapolated function
Model [1] outperforms model [3] spectacularly after this post correction.
Another reduced model is even better:
   Result:= a[fit_I1]*Exp(-a[fit_mu1]*cm)+a[fit_X_c]                                        [4]
and needs no post correction.}
{06/01/2016
 The primary photon model is amended to suppress oscillation of the model
 due to similar but opposing functions buildup region and attenuation region.
   Result:= a[fit_I1]*Exp((-a[fit_mu1]+(a[fit_mu2]+(a[fit_mu3]+a[fit_mu4]*m)*m)*m)*m) -
            a[fit_Ib]*Exp( -a[fit_mub]*m^twcPddFitMubPower)                                 [5]}

{14/06/2017 pddfitOdim variant added for orthovolt/missing max}
{17/06/2017 calculate intermediate results}
function TWellhoferData.TvSpddFunction(a :TaFunctionVertex;
                                      cm:TaVertexDataType): TaVertexDataType;
var d,e,f,m: TaVertexDataType;

  function calc_P_exponent: TaVertexDataType;
  begin
  Result:= (-a[pddfit_mu1]+(a[pddfit_mu2]+(a[pddfit_mu3]+a[pddfit_mu4]*m)*m)*m)*m;
  if Result>-1e-10 then
    Result:= 0;
  end;

begin
if (not Assigned(a)) then
  Result:= fitCalcErrorDef
else
  if cm>0 then
    try
      m:= cm/100;
      case Length(a) of
        pddfitEdim  : begin
                      d:= cm-a[pddfit_d0];
                      e:= (-a[pddfit_mu1]+(a[pddfit_mu2]+(a[pddfit_mu3]+a[pddfit_mu4]*d)*d)*d)*d;
                      f:= (-a[pddfit_mx1]+a[pddfit_mx2]*m)*m;
                      Result:= a[pddfit_I0]/( 1 + a[pddfit_I1]*Exp(e) + a[pddfit_Ib]*Exp(-a[pddfit_mub]*cm) ) + a[pddfit_Ix]*Exp(f);
                      end;
        pddfitOdim  : begin
                      e:= calc_P_exponent;
                      if e<0 then Result:= a[pddfit_I1]*Exp(e)
                      else        Result:= fitCalcErrorDef;
                      end;
        pddfitPdim  : begin
                      e:= calc_P_exponent;
                      if e<0 then Result:= a[pddfit_I1]*Exp(e) - a[pddfit_Ib]*Exp(-a[pddfit_mub]*Power(m,twcPddFitMubPower))
                      else        Result:= fitCalcErrorDef;
                      end;
        pddfitPdim+1: begin
                      e:= calc_P_exponent;
                      if e<0.5 then Result:= a[pddfit_I1]*Exp(e) - a[pddfit_Ib]*Exp( -a[pddfit_mub]*Power(m,1+a[pddfit_mubpower]))
                      else          Result:= fitCalcErrorDef;
                      end;
        pddfitXdim  : Result:= a[pddfit_I1]*Exp(-a[pddfit_mu1]*m)+a[pddfit_X_c];
        else          Result:= fitCalcErrorDef;
        end; {case}
     except
      Result:= fitCalcErrorDef;
     end
  else
    Result:= 0;
end; {~tvspddfunction}


{$push}{$warn 5091 off: model not initialised}
{17/12/2015 check on fitCalcErrorDef made more sensitive}
{06/01/2016 Result is error per point}
{26/06/2016 introduction of FNMPddFirst, FNMPddLast to limit range for error calculation}
{$IFDEF ENR_WEIGHTED_PDDFIT}
{12/01/2016 version including ENR}
{27/07/2017 subtle speed improvements through efficiency}
function TWellhoferData.TvSpddFitErrorResult(var a:TaFunctionVertex): TaVertexDataType;
var i,j,k,l: Integer;
    e      : TaVertexDataType;
    halt   : Boolean;
    model  : twcFloatArray;
    z_class: Byte;

  function powered_z(var cm:twcFloatType): TaVertexDataType;
  begin
  case z_class of
    0:  Result:= 1;
    1:  Result:= max(0.01,cm);
   else Result:= Power(max(0.01,cm),twcPddFitZWeightPower);
  end;
  end;

begin
if twcPddFitZWeightPower<0.01             then z_class:= 0
else if Abs(twcPddFitZWeightPower-1)<0.01 then z_class:= 1
else                                           z_class:= 2;
Result:= 0;
with wSource[FNMPddSource] do if assigned(a) then
  begin
  i:= FNMPddFirst;
  SetLength(model,Succ(FNMPddLast));
  try
    repeat
      model[i]:= TvSpddFunction(a,twPosCm[i])*FNMPddScaling;
      halt    := abs(model[i]/fitCalcErrorDef)>0.8;
      Inc(i);
    until halt or (i>FNMPddLast);
   except
     FNMreset:= True;
     halt    := True;
   end;
  if halt then
     Result:= fitCalcErrorDef
  else
    begin {ifthen(Length(a)=pddfitOdim,(FNMPddLast-i)/Succ(FNMPddLast),1)*}
    for i:= FNMPddFirst to FNMPddLast do
      Result:= Result+Sqr(model[i]-twData[i])/powered_z(twPosCm[i]); {point weighted error}
    if twcPddFitCostENRWeighted then
      begin
      k:= Max(2,Succ(FNMPddLast-FNMPddFirst) div (2*twcDefENRblocks));
      j:= FNMPddFirst+k;
      l:= Succ(2*k);
      repeat
        e:= 0;
        for i:= j to j+l do
          e:= e+(model[i]-twData[i])/powered_z(twPosCm[i]); {ENR weighted error}
        Result:= Result+Sqr(e)/l;
        Inc(j);
      until j>=FNMPddLast-l;
      end; {enrweighted}
    Result:= SqRt(Result/Succ(FNMPddLast-FNMPddFirst));
    end; {else}
  end
else
  Result:= 0;
Finalize(model);
end; {~tvspddfiterrorresult}
{$ELSE}
function TWellhoferData.TvSpddFitErrorResult(var a:TaFunctionVertex): VertexDataType;
var i: Word;
    r: VertexDataType;
    e: Boolean;
begin
Result:= 0;
with wSource[FNMPddSource] do if assigned(a) then
  begin
  i:= FNMPddFirst;
  repeat
    r:= TvSpddFunction(a,twPosCm[i]);
    try
      if abs(r/fitCalcErrorDef)<0.9 then Result:= Result+Sqr(r*FNMPddScaling-twData[i])
      else                               Result:= r;
     except
      Result  := fitCalcErrorDef;
      FNMreset:= True;
     end;
    Inc(i);
    e:= abs(Result/fitCalcErrorDef)>0.8;
  until (i>FNMPddLast) or e;
  if not e then
    Result:= SqRt(Result/Max(1,i-FNMPddFirst));
  end
else
  Result:= 0;
end; {~tvspddfiterrorresult}
{$ENDIF}
{$pop}


{wrap-around to pick up parameters for model}
{17/12/2015 check on fitCalcErrorDef made more sensitive}
function TWellhoferData.NMpddmodelResult(ASource   :twcDataSource;
                                         AfitVertex:twcNMpddFits;
                                         cm        :TaVertexDataType): TaVertexDataType;
begin
with wSource[ASource].twPddFitData[AfitVertex] do
  try
    Result:= TvSpddFunction(twNMReport.BestVertex,cm);
    if abs(Result/fitCalcErrorDef)<0.9 then
      Result:= Result*twFitNormalisation
    else
      Result:= 0;
   except
    Result:= 0
   end;
end; {~nmpddmodelresult}


{09/02/2021 added amoebeid}
procedure TWellhoferData.LoopReport(var AReport:NMReportRecord);
var s: String;
    i: Integer;
begin
with AReport do
  begin
  s:= '';
  for i:= 0 to Pred(Length(BestVertex)) do
    s:= s+FloatFormat(BestVertex[i],twcNMdigits)+', ';
  StatusMessage(Format('amoebe#%d: %d cycles %0.2f s, %s',[AmoebeID,Cycles,Seconds,s+FloatFormat(BestScore,twcNMdigits)]));
  end;
end; {~loopreport}


procedure TWellhoferData.TimeReport(var AReport:NMReportRecord);

begin
with wSource[FTimeRepSource] do
  StatusMessage(Format('%s, %s: %s... (%0.1f s)',
                       [GetCurveIDString(FNMPddSource),
                       FormatDateTime('dd-mmm-yyyy hh:nn',twMeasDateTime),twNMfitStg,AReport.Seconds]));
end; {~timereport}


{10/05/2016: try/excecpt added for twcPddFitMubPower}
{26/06/2016:
  twFitMaxScaling
  limit output to fitted range}
{16/06/2017
  twFitNormalisation: try except
  pddfitOdim variant added for orthovolt/missing max
  allow orthovolt-fit to start at z=0 or nearest positive position}
{12/07/2017 twcPddFitZWeightPower}
{27/07/2017 better administration of restarts through initial value of -1}
{29/07/2017 time-keeping}
{03/08/20017 check on legality of initial bestvertex in function RunNelderMead}
{28/01/2018 twcCoupledSources}
{14/04/2018 twFitScalingPointCm, set more limits}
{03/06/2018 FIndexingMode}
{27/08/2020 twMaxPosCm, twMaxValue}
{17/09/2020 introduction of FFrozen}
{21/02/2021 avoid division by zero for twFitNormalisation altogether insteed of relying on try..except}
procedure TWellhoferData.PddFit(ASource     :twcDataSource=dsMeasured;
                                ADestination:twcDataSource=dsCalculated);
const Photonvertex   : array[0..pddfit_mubpower ] of TaVertexDataType=(110,  4,  -7,  15,   -10, 60,    260,  0.1 ); {I1, mu1..4, Ib, mub,mubpower}
      Orthovoltvertex: array[0..pddfit_mubpower ] of TaVertexDataType=(110, 12, -50,  70,  -175, 10,    145,  0.95);
      Electronvertex : array[0..Pred(pddfitEdim)] of TaVertexDataType=(  4,  1,   0.3,       0.05, 0.01,-20,   2, 200, 1, 2, 0.05, 0.01);
var i       : Integer;                                              { I1 mu1  mu2  mu3   mu4   Ib   mub   I0 d0  Ix  mx1  mx2}
    f50,fmax: Single;
    Stg     : String;
    r       : twcNMpddFits;
    b       : Boolean;
    s       : twcDataSource;

  procedure AddExtraText(ADataSource:twcDataSource);
  var i: Byte;
  begin
  i:= ifthen((GetBeamType=Photons),4,2);
  with wSource[ASource] do
    begin
    SetLength(twExtraText,Length(twExtraText)+i);
    twExtraText[Length(twExtraText)-i  ]:= FitReport(True ,ADataSource);
    twExtraText[Length(twExtraText)-i+1]:= FitReport(False,ADataSource);
    if (i=4) and twPddFitData[NM_Extrapolation].twFitValid then
      begin
      twExtraText[Length(twExtraText)-i+2]:= FitReport(True ,ADataSource,NM_Extrapolation);
      twExtraText[Length(twExtraText)-i+3]:= FitReport(False,ADataSource,NM_Extrapolation);
      end;
    end;
  end;

  {$push}{$warn 5091 off: model not initialised}
  {03/08/2017 check on legality of initial bestvertex}
  {19/02/2018 improved TaNMsimplex object distributes all threads evenly over cpu's}
  {08/01/2020 extra try..except in enr evaluation}
  {26/05/2020 TaNMsimplex.Create}
  {14/07/2020 twFitOffsetCm is now individual per side to handle overlapping penumbras}
  function RunNelderMead(AReport   :twcNMpddFits;
                         AModel    :twcFitModels;
                         cDimension:Byte;
                         FitLimit  :Single): Boolean;
  var i,j,k  : Integer;
      p      : NelderMeadParam;
      e_sum,e: TaVertexDataType;
      model  : twcFloatArray;
      {$IFDEF PDDERROR_SPEEDTEST}
      o      : Cardinal;
      {$ENDIF PDDERROR_SPEEDTEST}

    function CenteredFraction(AStep:Integer): TaVertexDataType;
    begin
    Result:= (1+CenteredRandom(AStep*0.1));
    end;

  begin
  FPDDfit_simplex:= TaNMsimplex.Create(@TvSpddFitErrorResult,cDimension,{$IFDEF THREADED_AMOEBE}twNumCPU{$ELSE}1{$ENDIF},fitCalcErrorDef);
  CopyCurve(ASource,FNMPddSource);
  with FPDDfit_simplex,ResultData,wSource[FNMPddSource] do
    begin
    fTimeCallBackObj         := @TimeReport;
    if (AReport=NM_Primary) and (LogLevel>2) then
      fLoopCallBackObj       := @LoopReport;
    CallBackChangeFraction   := 0.1;
    CallBack_ms              := 500;
    twFittedData             := True; //(cDimension>fitXdim);
    twFastScan               := False;
    FNMPddScaling            := twMaxValue/100;
    MinScoreChangeFraction   := 1e-15;
    IllegalErrorFunctionValue:= fitCalcErrorDef;
    MaxSeconds               := twcNMseconds;
    MaxRestarts              := Ceil(twcNMrestarts/Max(1,(Amoebes-1)/2));
    MaxCycles                := twcNMCycles;
    RandomChangeFraction     := 0.7;
    FNMPddFirst              := twScanFirst;
    FNMPddLast               := twScanLast;
    while (twPosCm[FNMPddFirst]<FitLimit) and (twDataLast-FNMPddFirst>10) do
      Inc(FNMPddFirst);
    with twPddFitData[AReport],twNMReport do
      begin
      Cycles       := 0;
      Restarts     :=-1;
      Seconds      := 0;
      twFitModel   := AModel;
      twFitValid   := (twPosCm[FNMPddFirst]>=FitLimit);
      twFitOffsetCm:= 0;                                                        //not used for pdd model
      end; {with}
    Result:= twPddFitData[AReport].twFitValid;
    if twPddFitData[AReport].twFitValid then
      begin
      repeat
        StatusMessage(Format('%s, %s: %s... (%0.1f s)',
                             [GetCurveIDString(FNMPddSource),
                              FormatDateTime('dd-mmm-yyyy hh:nn',twMeasDateTime),twNMfitStg,
                              twPddFitData[AReport].twNMReport.Seconds]));
        {$IFDEF X_FIT_TEST}Dec(twDataLast,400);{$ENDIF}
        with twPddFitData[AReport].twNMReport do
          begin
          Inc(Restarts);
          if FNMreset or (Length(BestVertex)<>cDimension) then                  //only initialised when invalid or forced
            begin
            FNMreset:= False;
            j       := 0;
            SetLength(BestVertex,cDimension);
            VertexSmall[pddfit_I1]:= 1e-6;                                      //set limits
            if AModel=pddElectron then
              begin
              VertexSmall[pddfit_I0]:= 1e-6;                                    //set limits
              VertexLower[pddfit_Ix]:= 1e-6;                                    //set limits
              end;
            repeat                                                              //here initial values are set for bestvertex
              if AModel=pddElectron then                                        //-ElectronVertex-
                for i:= 0 to Pred(cDimension) do
                  BestVertex[i]:= CenteredFraction(j)*ifthen(twcPDDpar[i],ElectronVertex[i] ,0)
              else if FitLimit=0 then                                           //-OrthovoltVertex-
                begin
                RandomChangeFraction:= 0.3;
                for i:= 0 to Pred(cDimension) do
                  BestVertex[i]:= CenteredFraction(j)*ifthen(twcPDDpar[i],OrthovoltVertex[i],0);
                end
              else                                                              //-PhotonVertex-
                for i:= 0 to Pred(cDimension) do
                  BestVertex[i]:= CenteredFraction(j)*ifthen(twcPDDpar[i],PhotonVertex[i]   ,0);
              Inc(j);
            until (TvSpddFitErrorResult(BestVertex)<fitCalcErrorDef) or (j=10);
            end;
          {$IFDEF PDDERROR_SPEEDTEST}
          o:= MilliSecondOfTheDay(Now);
          with twPddFitData[AReport].twNMReport do
            for i:= 1 to 10000 do TvSpddFitErrorResult(BestVertex);
          o:= MilliSecondOfTheDay(Now)-o;
          StatusMessage(Format('%0f ms per error calculation (%d ms per %d)',[o/i,o,i]));
          {$ENDIF PDDERROR_SPEEDTEST}
          try
            StartAmoebe(twPddFitData[AReport].twNMReport.BestVertex,twPddFitData[AReport].twNMReport.Seconds,1);
           except
            CrawlReport.FitValid:= False;
           end;
          end;
        if fMaxRestarts>0 then
          fMaxRestarts:= fMaxRestarts-1;
        {$IFDEF X_FIT_TEST}Inc(twDataLast,400);{$ENDIF}
        twPddFitData[AReport].twNMReport.FitValid:= CrawlReport.FitValid;
        with twPddFitData[AReport],twNMReport do
          if FitValid then
            begin
            twFitLowCm        := twPosCm[FNMPddFirst];
            twFitHighCm       := twPosCm[FNMPddLast ];
            twFitNormalisation:= FNMPddScaling;
            BestVertex        := Copy(CrawlReport.BestVertex);
            BestScore         := CrawlReport.BestScore;
            AmoebeID          := CrawlReport.AmoebeID;
            Seconds           := CrawlReport.Seconds;
            e_sum             := 0;
            for p:= NMreflection to NMshrinkage do
              Inc(NMsteps[p],CrawlReport.NMsteps[p]);
            Inc(Restarts,CrawlReport.Restarts);         {repeat loop introduces by definition 1 extra restart but starts at -1}
            Inc(Cycles,CrawlReport.Cycles);
            if twSNR>0 then                                                                    {twSNR is calculated in QuadFilter}
              begin
              SetLength(model,Succ(FNMPddLast));
              try
                for i:= FNMPddFirst to FNMPddLast do
                  model[i]:= TvSpddFunction(BestVertex,twPosCm[i])*FNMPddScaling;    {vullen wSource[FNMsource].twdata met fitwaarde}
                k:= Max(2,twPoints div (2*twcDefENRblocks));
                for i:= FNMPddFirst+k to FNMPddLast-k do                        {This calculation differs from TvSpddFitErrorResult.}
                  begin
                  e:= 0;
                  for j:= i-k to i+k do
                    e:= e+(model[j]-wSource[ASource].twData[j]);   {Here a local block is taken for every point.}
                  e_sum:= e_sum+Sqr(e)/(2*k+1);          {In TvSpddFitErrorResult only overlapping blocks are observed, much faster.}
                  end;
                try
                  e_sum:= SqRT(e_sum/(FNMPddLast-FNMPddFirst-2*k+1))/(twMaxValue*twSNR);
                 except
                  e_sum:= SqRT(e_sum/(FNMPddLast-FNMPddFirst-2*k+1));
                 end;
               except
                 ENR:= 9e9;
               end;
              Finalize(model);
              end; {twsnr>0}
            ENR       := e_sum;
            twFitValid:= ENR<twcENRlimit*1000;
            Result    := twFitValid;
            if e_sum<twcENRlimit then
              fMaxRestarts:= 0;
            end
         else
           FNMReset:= True;
      LoopReport(CrawlReport);
      until (fMaxRestarts<1);
      wSource[Asource].twPddFitData[AReport]:= twPddFitData[AReport];
      end; {fitvalid}
    end; {with}
  try
    FreeAndNil(FPDDfit_simplex);
   except
    ExceptMessage('WH.PddFit!');
   end;
  end; {runneldermead}
  {$pop}

begin
if (not (FFrozen or FIndexingMode)) and Analyse(ASource) then
  begin
  Inc(FActiveCnt); {increase business}
  with wSource[ASource] do
    begin
    if twSNR=0 then
      begin
      if ASource in twcFilterSources then
        s:= twcCoupledSources[ASource]       {twSNR already available for filtered curve}
      else
        begin
        s:= ADestination;
        QuadFilter(-1,ASource,s,False,True); {create temporary result to calculate twSNR}
        end;
      twSNR:= wSource[s].twSNR;
      end;
    f50:= twLevelPos[d50].Penumbra[twcRight].Calc;
    Stg:= GetLastMessage;
    i  := Pos(', '+Copy(twForNMreport,1,4),Stg);
    if i>0 then
      SetLength(Stg,Pred(i));
    FNMPddSource  := ADestination;
    FTimeRepSource:= ADestination;
    fmax          := twMaxPosCm-Max(0,twPosCm[twScanFirst]); {the pdd-model is strictly limited for z>=0}
    if wSource[ASource].twBeamInfo.twBModality='E' then
      begin
      if fmax<twcPDDminTopCm then
        RunNelderMead(NM_Primary,pddElectron,pddfitOdim,0)
      else
       RunNelderMead(NM_Primary,pddElectron,pddfitEdim,0.2)
      end
    else
      begin
      b       := FNMreset;
      RunNelderMead(NM_Extrapolation,pddPhotonExtrapolation,pddfitXdim,f50);
      FNMreset:= b;
      if RunNelderMead(NM_Primary,pddPhoton,pddfitPdim+ifthen(twcPddFitMubPowerFixed,0,1),
                       ifthen(fmax<twcPDDminTopCm,0,Max(0.3,0.1*twPosCm[twMaxArr]))       ) and
         (not twcPddFitMubPowerFixed) then
        try
          twcPddFitMubPower:= 1+twPddFitData[NM_Primary].twNMReport.BestVertex[PddFit_MubPower];
         except
          twcPddFitMubPower:= 1;
         end;
      end;
    if (fmax<twcPDDminTopCm) and (twcPddFitZWeightPower<0.2) then
      StatusMessage('A Z-weigthing power of at least 0.2 is advised for low energy photons.',False);
    end; {with}
  with wSource[FNMPddSource],twPddFitData[NM_Primary],twNMReport do if twFitValid then
    begin
    twFitNormalisation:= 1;     {needed for PDDmaxNMFit}
    Analyse(FNMPddSource); { -> PDDmaxNMFit; sets twMaxPosCm, twMaxValue}
    try
      fMax:= TvSpddFunction(BestVertex,twMaxPosCm);
      if fMax>0 then
        twFitNormalisation:= 100/fMax;
     except
      twFitNormalisation:= 1;
     end;
    twDataFirst:= NearestPosition(twFitLowCm ,FNMPddSource);
    twDataLast := NearestPosition(twFitHighCm,FNMPddSource);
    twScanFirst:= twDataFirst;
    twScanLast := twDataLast;
    twfastScan := False;
    i          := twDataFirst;
    while i<=twDataLast do
      begin
      try
        twData[i]:= TvSpddFunction(BestVertex,twPosCm[i])*twFitNormalisation;
       except
        twData[i]:= 0;
       end;
      Inc(i);
      end;
    Analyse(FNMPddSource);
    twFitScalingPointCm:= twMaxPosCm;
    fmax:= TvSpddFunction(BestVertex,twFitScalingPointCm);
    if Abs(fmax)<1e-3 then
      fmax:= 100;
    try
      twFitMaxScaling:= 100/fmax;
     except
      twFitMaxScaling:= 1;
     end;
    StatusMessage(FitReport(True ,FNMPddSource));
    StatusMessage(FitReport(False,FNMPddSource));
    StatusMessage(Format('%0.2f',[twFitNormalisation*fmax]));
    if twPddFitData[NM_Extrapolation].twFitValid then
      begin
      twPddFitData[NM_Extrapolation].twFitNormalisation:=
        twFitNormalisation*TvSpddFunction(BestVertex,f50)/TvSpddFunction(twPddFitData[NM_Extrapolation].twNMReport.BestVertex,f50);
      StatusMessage(FitReport(True ,ASource,NM_Extrapolation));
      StatusMessage(FitReport(False,ASource,NM_Extrapolation));
      fmax:= TvSpddFunction(twPddFitData[NM_Extrapolation].twNMReport.BestVertex,twFitScalingPointCm);
      if Abs(fmax)<1e-3 then
        fmax:= 100;
      twPddFitData[NM_Extrapolation].twFitMaxScaling:= 100/fmax;
      end;
    StatusMessage(Format('%s, '+twForNMreport,[Stg,ENR,Cycles,Restarts,ifthen(Restarts=1,'','s'),Seconds,twNumCPU,twMaxValue,twFitScalingPointCm]));
    //StatusMessage(Format('pdd(%0.2f)=%0.2f%%',[twAbsNormPosCm,TvSpddFunction(BestVertex,twAbsNormPosCm)*twFitNormalisation]));
    end;
  AddExtraText(ASource);
  AddExtraText(ADestination);
  wSource[ADestination].twDataHistoryStg:= twNMfitStg+'('+wSource[ADestination].twDataHistoryStg+')';
  wSource[ADestination].twRelatedSource := ASource;
  for r:= NM_Primary to NM_Extrapolation do
    begin
    wSource[ASource].twPddFitData[r].twFitNormalisation := wSource[FNMPddSource].twPddFitData[r].twFitNormalisation;
    wSource[ASource].twPddFitData[r].twFitScalingPointCm:= wSource[FNMPddSource].twPddFitData[r].twFitScalingPointCm;
    wSource[ASource].twPddFitData[r].twFitMaxScaling    := wSource[FNMPddSource].twPddFitData[r].twFitMaxScaling;
    end;
  Dec(FActiveCnt); {decrease business}
  end; {if analyse}
end; {~pddfit}


{23/07/2015}
{27/07/2015 twComposite is set}
{11/07/2017 twMayneordApplied}
{09/08/2017 only apply if new differs from org, update twCurveIDString}
{27/08/2020 twMaxPosCm}
{17/09/2020 introduction of FFrozen}
function TWellhoferData.Mayneord(SSD_org_cm,SSD_new_cm:twcFloatType;
                                 Dmax_org_cm          :twcFloatType=0;
                                 ASource              :twcDataSource=dsMeasured): Boolean;
var MaxTerm: twcFloatType;
    i      : Integer;
begin
with wSource[ASource],twBeamInfo do
  begin
  if      SSD_org_cm<1 then SSD_org_cm:= twSSD_cm
  else if SSD_new_cm<1 then SSD_new_cm:= twSSD_cm;
  Result:= (not FFrozen) and twValid and (ScanType in twcVertScans) and
          (twBModality='X') and (not twMayneordApplied) and
          (SSD_org_cm>5)    and (SSD_new_cm>5)          and (SSD_org_cm<>SSD_new_cm);
  if Result then
    begin
    if Dmax_org_cm<0 then
      Dmax_org_cm:= twMaxPosCm;
    MaxTerm:= Sqr((SSD_new_cm+Dmax_org_cm)/(SSD_org_cm+Dmax_org_cm));
    for i:= twDataFirst to twDataLast do
      twData[i]:= twData[i]*MaxTerm*Sqr((SSD_org_cm+twPosCm[i])/(SSD_new_cm+twPosCm[i]));
    twSSD_cm         := SSD_new_cm;
    twMayneordApplied:= True;
    twDataHistoryStg := 'Mayneord('+twDataHistoryStg+')';
    twCurveIDString  := MakeCurveName(False,True,wDefaultIgnoreSet,True,ASource);
    twComposite      := True;
    end;
  end;
end; {~mayneord}


{$push}{$warn 5091 off: cm not initialised}
{called by: FastScan, PddFit}
{27/08/2020 twMaxPosCm, twMaxValue}
{17/09/2020 introduction of FFrozen}
{17/02/2021 singleamoebe}
procedure TWellhoferData.PDDmaxNMFit(ASource:twcDataSource=dsMeasured;
                                     AFit   :twcNMpddFits=NM_Primary);
var cm: TaFunctionVertex;
    NM: TaNMsimplex;
begin
Inc(FActiveCnt);
FNMPddSource:= ASource;
FNMPddFit   := AFit;
NM          := TaNMsimplex.Create(@PDDfitMaxErrorResult,1);
with NM,ResultData,wSource[ASource] do
  begin
  if (not FFrozen) and twFittedData and (twMaxArr>twScanFirst) then  with twPddFitData[FNMPddFit] do
    try
      Setlength(cm,1);
      MinScoreChangeFraction:= 1e-5;
      MaxCycles             := 500;
      MaxRestarts           := 4;
      cm[0]                 := twPosCm[twMaxArr];
      SingleAmoebe(cm);
      twMaxPosCm            := CrawlReport.BestVertex[0];
      twMaxValue            := TvSpddFunction(twNMReport.BestVertex,twMaxPosCm)*twFitNormalisation;
     except
      twMaxValue            := 100;
     end;
  Finalize(cm);
  end;
try
  NM.Free;
 except
  ExceptMessage('WH.PddMaxNMfit!');
 end;
Dec(FActiveCnt);
end; {~pddmaxnmfit}
{$pop}


{29/07/2015 bugfix: make copy when no filtering is done}
{06/08/2015 twIsFiltered added
             check on asource filterpoints instead of adestination}
{03/06/2018 initborders}
{11/09/2018 ResetBorderValues}
{24/05/2020 more discrete filterwidth limit in StepFilter}
{21/07/2020 GetAdjustedFilterWidthCm}
{03/09/2020 preserve fitted results}
{17/09/2020 introduction of FFrozen}
procedure TWellhoferData.QuadFilter(cm                    :twcFloatType =twcDefaultValue;
                                    ASource               :twcDataSource=dsMeasured;
                                    ADestination          :twcDataSource=dsCalculated;
                                    PostAnalysis          :Boolean     =False;
                                    AllowRepeatedFiltering:Boolean     =False;
                                    ResetBorderValues     :Boolean     =True);
var Q         : TQuadFit;
    P1,P2,Pc,n: Integer;
    tmpData   : twcFloatArray;
    DataPtr   : twcFloatArrayPtr;
    Check     : Boolean;
   {$IFDEF FIXED_DISTANCE_FILTER}
    {$IFDEF DISCRETE_FIXED_DISTANCE}
    kcm2      : Integer;
    {$ELSE}
    cm2       : twcFloatType;
    {$ENDIF DISCRETE_FIXED_DISTANCE}
   {$ENDIF FIXED_DISTANCE_FILTER}

   {$IFDEF FIXED_DISTANCE_FILTER}
    procedure StepFilter;   //fixed distance variant
    begin
    with wSource[ASource] do
      begin
      Inc(Pc);
     {$IFDEF DISCRETE_FIXED_DISTANCE}
      while Round(twcDefDiscretisationMult*Abs(twPosCm[P1]-twPosCm[Pc]))>kcm2 do                         //alleviate truncation errors
     {$ELSE}
      while Abs(twPosCm[P1]-twPosCm[Pc])>cm2 do
     {$ENDIF DISCRETE_FIXED_DISTANCE}
        begin
        Q.Del_XY(twPosCm[P1],DataPtr^[P1]);
        Inc(P1);
        end;
     {$IFDEF DISCRETE_FIXED_DISTANCE}
      while (P2<=twDataLast) and (Round(twcDefDiscretisationMult*Abs(twPosCm[P2]-twPosCm[Pc]))<kcm2) do  //alleviate truncation errors
     {$ELSE}
      while (P2<=twDataLast) and (Abs(twPosCm[P2]-twPosCm[Pc])<cm2) do
     {$ENDIF DISCRETE_FIXED_DISTANCE}
        begin
        Q.Add_XY(twPosCm[P2],DataPtr^[P2]);
        Inc(P2);
        end;
      end;
    end;
    {$ELSE}
    procedure StepFilter;   //fixed number points variant
    var i: Integer;
    begin
    with wSource[ASource],Q do
      begin
      Inc(Pc);
      i:= Pc+P1;
      if InRange(i,twDataFirst,twDataLast) then
        Add_XY(twPosCm[i],DataPtr^[i]);
      i:= Pc-P2;
      if InRange(i,twDataFirst,twDataLast) then
        Del_XY(twPosCm[i],DataPtr^[i]);
      end;
    end;
    {$ENDIF FIXED_DISTANCE_FILTER}

begin
if (not FFrozen) and wSource[ASource].twValid then
  begin
  if AllowRepeatedFiltering or (wSource[ASource].twFilterPoints=0) then
    begin
    {$IFDEF WELLHOFER_DUMPDATA}
     CopyCurve(ASource,dsUnrelated);
    {$ENDIF}
    if (cm<0.1) or (wSource[ASource].twSetFieldType=fcSmall) then
      cm:= GetAdjustedFilterWidthCm(ASource);
    if ASource=ADestination then
      begin
      tmpData:= Copy(wSource[ASource].twData);
      DataPtr:= @(tmpData);
      end
    else
      begin
      CopyCurve(ASource,ADestination); {kopieer parameters}
      DataPtr:= @wSource[ASource].twData;
      end;
    with wSource[ADestination] do
      begin
      if twStepSizeCm>0 then twFilterPoints:= Succ((Round(cm/twStepSizeCm) div 2)*2)
      else                   twFilterPoints:= 3;
      twIsFiltered     := True;
      twDerivativeValid:= False;
      twRelatedSource  := ASource;
      Check            := wOutlierFilter and (twFilterPoints<=twcOutlierPointLimit);
     {$IFDEF FIXED_DISTANCE_FILTER}
      {$IFDEF DISCRETE_FIXED_DISTANCE}
      kcm2  := Round(twcDefDiscretisationMult*cm/2);                                     //alleviate truncation errors
      {$ELSE}
      cm2  := cm/2;
      {$ENDIF DISCRETE_FIXED_DISTANCE}
      P1   := twDataFirst;
      P2   := P1;
      Pc   := Pred(P1);
     {$ELSE}
      P1   := twFilterPoints div 2;
      P2   := twFilterPoints-P1;
      Pc   := -P2-1;
     {$ENDIF FIXED_DISTANCE_FILTER}
      twSNR:= 0;
      n    := 0;
      if twFilterPoints>2 then
        begin
        Q:= TQuadFit.Create(twFilterPoints);
        while Pc<twDataLast do
          try
            StepFilter;
            if Pc>=0 then
              begin
              twData[Pc]:= Q.FitQuad(twPosCm[Pc],Check);
              Inc(n);
              twSNR:= twSNR+Sqr(wSource[ASource].twData[pc]-twData[pc]);
              end;
           except
            twData[Pc]:= 100;
           end;
        try
          FreeAndNil(Q);
         except
          ExceptMessage('WH.QuadFit!');
         end;
        twValid       := True;
        twFastScan    := False;
        twAnalysed    := False;
        twAvgNormValue:= GetQfittedValue(twAbsNormPosCm,ADestination)*twRefNormFactor;
        twFilterString:= twQuadFilterStg;
        if (n>0) and (twSNR>0) and (twMaxValue>0) then
          twSNR:= SqRt(twSNR/n)/twMaxValue;
        end
      else twFilterPoints:= 1;
      if ASource=ADestination then
        Finalize(tmpData);
      end; {with}
   {$IFDEF WELLHOFER_DUMPDATA}
    DumpData('QuadFilter',ADestination,ASource);
    DumpData('Filter Points',dsUnrelated);
   {$ENDIF}
    if PostAnalysis then
      Analyse(ADestination)
    else if ResetBorderValues then
      InitBorders(ADestination,False);                                          //preserve fitted results
    end {filtered}
  else if (ADestination<>ASource) then
    CopyCurve(ASource,ADestination);  {make copy if needed}
  end;
end; {~quadfilter}


{$push}{$warn 5091 off: meadian not initialised}
//http://fourier.eng.hmc.edu/e161/lectures/smooth_sharpen/node3.html
{29/07/2015 bugfix: make copy when no filtering is done}
{06/08/2015 twIsFiltered added}
{03/06/2018 initborders}
{11/09/2018 ResetBorderValues}
{21/07/2020 GetAdjustedFilterWidthCm}
{03/09/2020 preserve fitted results}
{17/09/2020 introduction of FFrozen}
procedure TWellhoferData.MedianFilter(cm                    :twcFloatType =twcDefaultValue;
                                      ASource               :twcDataSource=dsMeasured;
                                      ADestination          :twcDataSource=dsCalculated;
                                      PostAnalysis          :Boolean     =False;
                                      AllowRepeatedFiltering:Boolean     =False;
                                      ResetBorderValues     :Boolean     =True);
var MedianList        : twcFloatArray;
    Mmid,i,j,MedianCnt: Integer;
    tmpData           : twcFloatArray;
    DataPtr           : twcFloatArrayPtr;

    procedure AddFilter(AValue:twcFloatType);
    var i: Integer;
    begin
    i:= MedianCnt;
    while (i>0) and (MedianList[Pred(i)]>=AValue) do {vul van start en schuif hogere waardes door}
      begin
      MedianList[i]:= MedianList[Pred(i)];
      Dec(i);
      end;
    Inc(MedianCnt);
    MedianList[i]:= AValue;
    end;

begin
if (not FFrozen) and wSource[ASource].twValid then
  begin
  if AllowRepeatedFiltering or (wSource[ADestination].twFilterPoints=0) then
    begin
    if (cm<0.1) or (wSource[ASource].twSetFieldType=fcSmall) then
      cm:= GetAdjustedFilterWidthCm(ASource);
    if ASource=ADestination then
      begin
      tmpData:= Copy(wSource[ADestination].twData);
      DataPtr:= @tmpData;
      end
    else
      begin
      CopyCurve(ASource,ADestination);                                          //copy parameters
      DataPtr:= @wSource[ASource].twData;
      end;
    with wSource[ADestination] do
      begin
      if twStepSizeCm>0 then Mmid:= Round(cm/(2*twStepSizeCm))
      else                   Mmid:= 1;
      twIsFiltered             := True;
      twDerivativeValid        := False;
      MedianCnt                := Succ(2*Mmid);
      twFilterPoints           := MedianCnt;
      twRelatedSource          := ASource;
      if MedianCnt>0 then
        begin
        SetLength(MedianList,MedianCnt);
        for i:= twDataFirst to twDataLast do
          begin
          MedianCnt:= 0;                                                        //now mediancnt holds track of actually filled data in filter
          for j:= i-Mmid to i+Mmid do
            AddFilter(DataPtr^[Max(twDataFirst,Min(j,twDataLast))]);
          twData[i]:= MedianList[Mmid];
          end;
        twValid       := True;
        twAnalysed    := False;
        twAvgNormValue:= GetQfittedValue(twAbsNormPosCm,ADestination)*twRefNormFactor;
        twFilterString:= twMedianFilterStg;
        end;
      if ASource=ADestination then
        Finalize(tmpData);
      end; {with}
    if PostAnalysis then
      Analyse(ADestination)
    else if ResetBorderValues then
      InitBorders(ADestination,False);
    end {filtered}
  else if (ADestination<>ASource) then                                          //nothing to do, just copy
    CopyCurve(ASource,ADestination);                                            //make copy if needed
  end;
end; {~medianfilter}
{$pop}

(*  --GetNormalisedSigmoidLevel--
 wrap-around for logistic function, autoselect of side, applies twSigmoidOffsetCm and twFitNormalisation
*)
{08/07/2020}
{13/07/2020 twFitOffsetCm}
function TWellhoferData.GetNormalisedSigmoidLevel(cm     :twcFloatType;
                                                  ASource:twcDataSource=dsMeasured): twcFloatType;
var s    : twcSides;
    difCm: array[twcLeft..twcRight] of twcFloatType;
begin
with wSource[ASource] do if BordersValid(ASource,dInflection) then
  begin
  for s:= twcLeft to twcRight do
    difCm[s]:= abs(cm-twLevelPos[d50].Penumbra[s].Calc);
  if difCm[twcLeft]<difCm[twcRight] then
    s:= twcLeft
  else
    s:= twcRight;
  Result:= 100*twSigmoidFitData[s].twFitNormalisation*RawLogisticFunction(twSigmoidFitData[s].twNMReport.BestVertex,cm,twSigmoidFitData[s].twFitOffsetCm)/twAppliedNormVal;
  end
else
  Result:= 0;
end; {~getnormalisedsigmoidlevel}


(*   --RawLogisticFunction--
https://en.wikipedia.org/wiki/Generalised_logistic_function
https://www.myassays.com/four-parameter-logistic-regression.html
https://www.mathworks.com/matlabcentral/fileexchange/38122-four-parameters-logistic-regression-there-and-back-again

The following is the 4PL model equation where x is the concentration (in the case of ELISA analysis)
or the independent value and F(x) would be the response value (e.g. absorbance, OD, response value) or dependent value.
f(x) = ((D-A)/(1+((x/B)^C))) + A
Note: this function will not respond well to x=0.

The rearranged equation to solve x is:
x= B*( (D-A)/(y-A) -1 )^(1/C)

18/06/2020
x= InflectionMajor * power((y-High)/(Low-y),1/Slope) WolframAlpha online

A = minimum asymptote
    In an ELISA assay where you have a standard curve, this can be thought of as the response value at 0 standard concentration.
B = (almost the) inflection point
C = Hill slope
    The Hill Slope or slope factor refers to the steepness of the curve.
    It could either be positive or negative. As the absolute value of the Hill slope increases, so does the steepness of the curve.
D = maximum asymptote
    In an ELISA assay where you have a standard curve, this can be thought of as the response value for infinite standard concentration.

for the correct inflection point
x=B*((C-1)/(C+1))^(1/C)

Derivative
https://www.wolframalpha.com/input/?i=derivative+of+f%28x%29+%3D+%28D-A%29%2F%281%2B%28%28x%2FB%29%5EC%29%29+%2BA
f'(x) = (A*C*(x/B)^C)/(x*((x/B)^C + 1)^2) - (C*D*(x/B)^C)/(x*((x/B)^C + 1)^2)

Derivative in inflection point:
https://www.wolframalpha.com/input/?i=derivative+of+f%28x%29+%3D+%28D-A%29%2F%281%2B%28%28x%2FB%29%5EC%29%29+%2BA+for+x%3DB*%28%28C-1%29%2F%28C%2B1%29%29%5E%281%2FC%29
f'(IP) = (B ((C - 1)/(C + 1))^(1/C)) = (C (((C - 1)/(C + 1))^(1/C))^(C - 1) (A - D))/(B ((((C - 1)/(C + 1))^(1/C))^C + 1)^2)

5PL model
F(x) = A + (D/(1+(X/B)^C)^E)
    A is the MFI (Mean Fluorescent Intensity)/RLU (Relative Light Unit) value for the minimum asymptote
    B is the x value at (almost) the inflection point
    C is the Hill slope
    D is the MFI/RLU value for the maximum asymptote
    E is the asymmetry factor
The 5-PL model equation has the extra E parameter which the 4-PL model lacks and when E = 1 the 5-PL equation is identical to the 4-PL equation.

The output is the fit result on twData/twFitNormalisation.

*)
{27/11/2015}
{14/01/2017 avoid pos=0}
{10/11/2020 introduce twFitnormalisation}
function TWellhoferData.RawLogisticFunction(const a      :TaFunctionVertex;
                                            const cm     :TaVertexDataType;
                                            const shiftCm:twcFloatType=0): TaVertexDataType;
var x: TaVertexDataType;
begin
if (not Assigned(a)) then
  Result:= fitCalcErrorDef
else
  if Length(a)=SigmoidDim then
    try                                                                         //f(x) = (D-A)/(1+((x/B)^C)) + A
      x:= cm-shiftCm;
      if x=0 then
        Result:= a[sigmoid_HighVal]
      else
        Result:= ((a[sigmoid_HighVal]-a[sigmoid_LowVal])/(1+Power(x/a[sigmoid_InflectionMajor],a[sigmoid_Slope]))) + a[sigmoid_LowVal];
     except
      Result:= fitCalcErrorDef;
     end
  else
    Result:= 0;
end; {~rawlogisticfunction}


{05/03/2021 new}
function TWellhoferData.RawLogisticDerivative(const a      :TaFunctionVertex;
                                              const cm     :TaVertexDataType;
                                              const shiftCm:twcFloatType=0): TaVertexDataType;
var x,Pwr_x: TaVertexDataType;
begin
if (not Assigned(a)) then
  Result:= fitCalcErrorDef
else
  if Length(a)=SigmoidDim then                                                  //f(x)  = ((D-A)/(1+((x/B)^C))) + A
    try                                                                         //f'(x) = (A*C*P)/(x*(P + 1)^2) - (C*D*P)/(x*(P + 1)^2); P=(x/B)^C
      x:= cm-shiftCm;
      if x=0 then
        Result:= a[sigmoid_HighVal]
      else
        Pwr_x := Power(x/a[sigmoid_InflectionMajor],a[sigmoid_Slope]);
        Result:= a[sigmoid_LowVal]*a[sigmoid_Slope]*Pwr_x/(x*Sqr(Pwr_x+1)) - a[sigmoid_Slope]*a[sigmoid_HighVal]*Pwr_x/(x*Sqr(Pwr_x+1));
     except
      Result:= fitCalcErrorDef;
     end
  else
    Result:= 0;
end; {~rawlogisticderivative}


{18/06/2020 x= InflectionMajor * power((y-High)/(Low-y),1/Slope) WolframAlpha online}
{19/06/2020 checks}
{10/11/2020 introduce twFitnormalisation}
function TWellhoferData.RevRawLogisticFunction(const a      :TaFunctionVertex;
                                               const FxRaw  :TaVertexDataType;
                                               const shiftCm:twcFloatType=0): TaVertexDataType;
var InflectionLevel: TaVertexDataType;
begin
if (not Assigned(a)) then
  Result:= fitCalcErrorDef
else
  if Length(a)=SigmoidDim then
    try
      InflectionLevel:= RawLogisticFunction(a,a[sigmoid_InflectionMajor]);
      if (FxRaw/a[sigmoid_LowVal]>1.1) and (FxRaw/a[sigmoid_HighVal]<0.9) and
         InRange(FxRaw/InflectionLevel,1-twcMaxLogisticRange,1+twcMaxLogisticRange) then
        Result:= a[sigmoid_InflectionMajor]*Power((FxRaw-a[sigmoid_HighVal])/(a[sigmoid_LowVal]-FxRaw),1/a[sigmoid_Slope])+shiftCm
      else
        Result:= fitCalcErrorDef;
     except
      Result:= fitCalcErrorDef;
     end
  else
    Result:= 0;
end; {~revrawlogisticfunction}


{19/06/2020}
{13/07/2020 twFitOffsetCm}
{10/11/2020 introduce twFitnormalisation}
function TWellhoferData.GetNormalisedRevLogistic(ASide      :twcSides;
                                                 ASource    :twcDataSource=dsMeasured;
                                                 Apercentage:twcFloatType =50        ):twcFloatType;
begin
with wSource[ASource],twSigmoidFitData[ASide] do
  Result:= RevRawLogisticFunction(twNMReport.BestVertex,(Apercentage/100)*(twAbsNormValue/twFitNormalisation),twFitOffsetCm);
end; {~getnormalisedrevlogistic}


{27/11/2015 Note that this implementation works on unscaled data with a shift of twSigmoidOffset}
{17/12/2015 check on fitCalcErrorDef made more sensitive}
{14/01/2017 pos=0}
{13/07/2020 twFitOffsetCm}
{10/11/2020 introduce twFitnormalisation}
{12/11/2020 scaled error calculation}
function TWellhoferData.SigmoidFitErrorResult(var a:TaFunctionVertex): TaVertexDataType;  {callback function for edge fit}

var i            : Integer;
    r            : TaVertexDataType;
    p,ofs,scaling: twcFloatType;
    s            : twcSides;
begin
Result:= 0;
with wSource[FNMEdgeSource] do if assigned(a) then
  begin
  if Round((FNMEdgeFirst+FNMEdgeLast)/2)<twCenterArr then
    s:= twcLeft
  else
    s:= twcRight;
  i      := FNMEdgeFirst;
  ofs    := twSigmoidFitData[s].twFitOffsetCm;
  scaling:= twSigmoidFitData[s].twFitNormalisation;
  repeat
    p:= twPosCm[i]-ofs;
    if p<>0 then
      try
        r:= RawLogisticFunction(a,p);
        if abs(r/fitCalcErrorDef)<0.9 then Result:= Result+Sqr(r-twData[i]/scaling)
        else                               Result:= r;
       except
        Result  := fitCalcErrorDef;
        FNMreset:= True;
        r       := 0;
       end
    else
      r:= 0;
    Inc(i);
  until (i>FNMEdgeLast) or (abs(Result/fitCalcErrorDef)>0.8);
  if abs(Result/fitCalcErrorDef)<0.9 then
    Result:= Result/Succ(FNMEdgeLast-FNMEdgeFirst);
  if LogLevel>3 then
    StatusMessage(Format('Sigmoid[%s] %0.2f, %0.2f, %0.2f, %0.2f: %0.2f',[twDataHistoryStg,a[0],a[1],a[2],a[3],r]));
  end
else
  Result:= 0;
end; {~sigmoidfiterrorresult}


(*----------SigmoidPenumbraFit----------------
 See introduction to logistic function above: f(x) = (D-A)/(1+((x/B)^C)) + A
 We have to avoid x=0. Therefore twSigmoidOffset is intoduced which is initialised to the field center.
 Note that with this offset the C is negative for both penumbras.
 In this case a positive value of B will produce a ascending and descending curve for left and right side respectively.

 The results are put into twSigmoidFitData: array[twcSides] of twFitRecord;
 and are based on raw, unscaled data, but with twSigmoidOffset included (in C).

 For tiny fields the penumbras overlap severely and the standard model range exceeds the center position.
 There the data will not be suitable for the sigmoid model.
 Also experimented with keeping the range symmetrical around the initial inflection point, but this does not influence the result.
 Another complication was the use of one single sigmoid model position offset parameter, usually the center position.
 For tiny fields this introduces again a zero position within fitting range, which must be avoided at all costs. Therefore now
 each side has its individual offset valus. For large enough fields they will be identical.
----------------------------------------------*)
{$push}{$warn 5057 off}
{01/12/2015}
{04/12/2015 initial BestVertex[edgefit_HighVal] set to FNMEdgelast/FNMEdgeFirst}
{17/12/2015
  limit area to twScanFirst..twScanLast for local peaks
  accurate estimate for low value}
{09/06/2016 implementation of dInflection}
{14/01/2017
  bestscore limit set to relative value
  introduction twSigmoidOffset}
{18/01/2017 fitrange made symmetrical}
{26/06/2017 fitCalcErrorDef in TaNMsimplex.Create}
{03/06/2018 FIndexingMode}
{08/10/2018 reporting limited to loglevel 2 and higher}
{08/01/2020 extra try..except in tweede fit}
{09/05/2020 use correctly signed slope as starting value for BestVertex[sigmoid_Slope] (was absolute)}
{22/05/2020 twSigmoidDone}
{23/05/2020 improved initial slope value, should always be positive also}
{24/05/2020 check for minimal value of slope}
{26/05/2020
   apply variable wInflectionSigmoidRadiusCm instead of fixed constant twcDefSigmoidRadiusCm
   extra logging
   when left penumbra+sigmoidradius < 0 < right penumbra-sigmoidradius then twcSigmoidOffset:= 0}
{21/06/2020 added dSigmoid50}
{07/07/2020 real inflection point=InflectionMajor*((Slope-1)/(Slope+1))^(1/Slope)}
{09/07/2020 initial slope (yh-yl)/(xh-xl) should be multiplied with 1.7, not divided}
{13/07/2020 twFitOffsetCm}
{14/07/2020 several rules added for very small fields when penumbras overlap}
{17/09/2020 introduction of FFrozen}
{10/11/2020 set twFitnormalisation to twMaxValue/100 and apply this to fit parameters}
{12/11/2020 scaled error limit (h)}
{17/02/2021 singleamoebe}
{05/03/2021: added twFitResult1,2 for optional model results}
function TWellhoferData.SigmoidPenumbraFit(ASource     :twcDataSource=dsMeasured;
                                           ApplyModel  :Boolean      =False;
                                           ADestination:twcDataSource=dsMeasured): Boolean;
var s : twcSides;
    l : array[twcSides] of twcFloatType;
    i : Integer;
    m : twcFloatType;
    LM: String;
    NM: TaNMsimplex;

  function RunNelderMead(ASide :twcSides;
                         ARange:twcFloatType): twcFloatType;
  var n  : NelderMeadParam;
      h,f: twcFloatType;
      v  : TaFunctionVertex;
      i  : Integer;
      Stg: String;
  begin
  NM           := TaNMsimplex.Create(@SigmoidFitErrorResult,SigmoidDim,1,fitCalcErrorDef);
  FNMEdgeSource:= ASource;
  FNMreset     := True;
  h            := 100;
  Result       := 0;
  v            := nil;
  with NM,ResultData do
    begin
    MinScoreChangeFraction:= 1e-9;
    MaxSeconds            := 2;
    MaxRestarts           := 2;
    RandomChangeFraction  := 0.7;
    MaxCycles             := twcNMCycles;
    with wSource[ASource],twSigmoidFitData[ASide],twNMReport do
      if not twFitvalid then
        begin
        Cycles    := 0;
        Restarts  := 0;
        Seconds   := 0;
        twFitModel:= penumbraSigmoid;
        twFitValid:= twLevelPos[dDerivative].Penumbra[ASide].Valid;
        Result    := ARange+twStepSizeCm;
        if twFitValid then
          begin
          repeat
            if FNMreset or (Length(BestVertex)<>SigmoidDim) then                                             //only initialised when invalid or forced
              begin
              FNMreset:= False;
              SetLength(BestVertex,SigmoidDim);
              try
                twFitOffsetCm := twCenterPosCm;
                h             := GetPenumbraValue(ASource,dDerivative,ASide);                                //edge already evaluated from derivative
                FNMEdgeFirst  := NearestPosition(h-wInflectionSigmoidRadiusCm,FNMEdgeSource);
                FNMEdgeLast   := NearestPosition(h+wInflectionSigmoidRadiusCm,FNMEdgeSource);
                f             := ifthen((FNMEdgeFirst<twCenterArr) and (FNMEdgeLast>twCenterArr),0.1,1.7);   //slope estimation factor
               if ASide=twcLeft then                                                                         //stay away from top, hold symmetrical
                  begin
                  while (FNMEdgeLast>twCenterArr+1) and (twData[FNMEdgeLast]/twData[twCenterArr]<0.95) do
                    Dec(FNMEdgeLast);
                  while twFitOffsetCm<twPosCm[FNMEdgeLast+1] do                                              //keep offset point outside fitting range
                    twFitOffsetCm:= twFitOffsetCm+1;
                 {$IFDEF SIGMOID_RANGE_SYMMETRIC}
                  FNMEdgeFirst:= NearestPosition(2*h-twPosCm[FNMEdgeLast],FNMEdgeSource);
                 {$ENDIF}
                  end
                else
                  begin
                  FNMEdgeFirst:= Max(FNMEdgeFirst,twCenterArr);
                  while (FNMEdgeFirst<twCenterArr-1) and (twData[FNMEdgeFirst]/twData[twCenterArr]<0.95) do
                    Inc(FNMEdgeFirst);
                  while twFitOffsetCm>twPosCm[FNMEdgeFirst-1] do
                    twFitOffsetCm:= twFitOffsetCm-1;
                 {$IFDEF SIGMOID_RANGE_SYMMETRIC}
                  FNMEdgeLast    := NearestPosition(2*h-twPosCm[FNMEdgeFirst],FNMEdgeSource);
                 {$ENDIF}
                  end;
                i:= Sign(FNMEdgeLast-FNMEdgeFirst);
                if ARange=0 then
                  ARange:= Abs(twPosCm[FNMEdgeLast]-twPosCm[FNMEdgeFirst]);
                while (Abs(FNMEdgeLast-FNMEdgeFirst)>(SigmoidDim+1)) and
                      ((Abs(twPosCm[FNMEdgeFirst]+twPosCm[FNMEdgeLast]-2*h)>twStepSizeCm) or
                       (Abs(twPosCm[FNMEdgeLast]-twPosCm[FNMEdgeFirst])-ARange>twStepSizeCm)) do
                  if Abs(h-twPosCm[FNMEdgeFirst])>Abs(twPosCm[FNMEdgeLast]-h) then Inc(FNMEdgeFirst,i)
                  else                                                             Dec(FNMEdgeLast ,i);
                twFitNormalisation                 := twMaxValue/100;
                BestVertex[sigmoid_LowVal         ]:= Min(twData[FNMEdgeFirst],twData[FNMEdgeLast])/twFitNormalisation;
                BestVertex[sigmoid_InflectionMajor]:= h-twFitOffsetCm;
                BestVertex[sigmoid_Slope          ]:= f*Abs((twData[FNMEdgeLast]-twData[FNMEdgeFirst])/(twPosCm[FNMEdgeLast]-twPosCm[FNMEdgeFirst]))/twFitNormalisation;
                BestVertex[sigmoid_HighVal        ]:= Max(twData[FNMEdgeFirst],twData[FNMEdgeLast])/twFitNormalisation;
                twFitLowCm                         := twPosCm[FNMEdgeFirst];
                twFitHighCm                        := twPosCm[FNMEdgeLast];
                v                                  := Copy(BestVertex);
                h                                  := BestVertex[sigmoid_HighVal];
                Result                             := Abs(twFitHighCm-twFitLowCm);
               except
                twFitValid:= False;
                Result    := 0;
               end; {except}
             {$IFNDEF KEEP_SIGMOID_ESTIMATE}
              twFitValid:= (BestVertex[sigmoid_Slope]*wInflectionSigmoidRadiusCm>1);          //put some limitation on bad slopes
             {$ENDIF}
              end; {if fnmreset}
            if twFitValid then
              try
                SingleAmoebe(wSource[ASource].twSigmoidFitData[ASide].twNMReport.BestVertex); //only one cpu is called, no multithreading
                if fMaxRestarts>0 then
                  fMaxRestarts:= fMaxRestarts-1;
                BestVertex:= Copy(CrawlReport.BestVertex);
               {$IFDEF KEEP_SIGMOID_ESTIMATE}
                BestVertex:= Copy(v);
               {$ELSE}
                if not assigned(BestVertex) then
                  BestVertex:= Copy(v)
                else
                  v         := Copy(BestVertex);
               {$ENDIF}
               for n:= NMreflection to NMshrinkage do
                 Inc(NMsteps[n],CrawlReport.NMsteps[n]);
               Restarts  := Restarts+CrawlReport.Restarts;                      //repeat loop introduces by definition 1 extra restart
               Seconds   := Seconds +CrawlReport.Seconds;
               BestScore := CrawlReport.BestScore;
               twFitValid:= assigned(BestVertex);
               Inc(Cycles,CrawlReport.Cycles);
              except
               twFitValid := False;
              end
            else
              Inc(Restarts);
          until (fMaxRestarts<1) or (not twFitValid);
         {$IFNDEF KEEP_SIGMOID_ESTIMATE}
         twFitValid:= twFitValid and (BestScore<h/5);
         {$ENDIF}
          if twFitValid then
            begin
            with twLevelPos[dInflection].Penumbra[ASide] do
              begin
              h           := BestVertex[sigmoid_Slope];                                                       //just for short-writing in next line
              Calc        := BestVertex[sigmoid_InflectionMajor]*power((h-1)/(h+1),1/h)+twFitOffsetCm;        //inflection point: x=B*((C-1)/(C+1))^(1/C)
              Nearest     := NearestPosition(Calc,FNMEdgeSource);
              Valid       := True;
              twFitResult1:= Calc;                                                                            //infection point
              twFitResult2:= twFitNormalisation*RawLogisticDerivative(BestVertex,twFitResult1,twFitOffsetCm); //slope in inflection point
              if wEdgeDetect then
                begin
                h:= GetQfittedValue(Calc,ASource);
                if h>0.01*twMaxValue then Stg:= FormatFloat(' (0.0 %)',100*SqRt(BestScore)/h)
                else                      Stg:= '';
                if ASide=twcRight then
                  LM:= LastMessage;
                if (LogLevel>2) and (ASource=dsMeasured) then
                  StatusMessage(Format('%s %s: %0.2f cm %s ',[LM,ifthen(ASide=twcLeft,'left','right'),Calc,Stg]));
                end;
             {$IFDEF SIGMOID_REPORT}
              h:= twFitOffsetCm;
             {$ENDIF}
              end;  {with}
            with twLevelPos[dSigmoid50].Penumbra[ASide] do
              begin
              Calc   := GetNormalisedRevLogistic(ASide,Asource);
              Nearest:= NearestPosition(Calc,FNMEdgeSource);
              Valid  := True;
              end;
            end; {fitvalid}
           end; {fitvalid}
         end; {with,if}
    end; {with FNMobject}
  Finalize(v);
  try
    NM.Free;
   except
    ExceptMessage('WH.SigmoidFit!');
   end;
  end; {runneldermead}

begin
if not (FIndexingMode or FFrozen) then
  begin
  Inc(FActiveCnt);
  with wSource[ASource] do
    if (ScanType in twcHoriScans) and twDerivativeValid and (not (twIsRelative or twisDerivative)) and BordersValid(ASource) then
      begin
      if wEdgeDetect then
        LM:= Copy(LastMessage,1,Pos(']',LastMessage))+Format(' Sigmoid[%s] ',[twDataHistoryStg]);
      FillChar(l,SizeOf(l),0);
      for i:= 0 to 1 do
        for s:= twcLeft to twcRight do
          begin
          m:= Min(l[twcLeft],l[twcRight]);
          if (l[s]=0) or ((l[s]-m)>2*twStepSizeCm) then
            l[s]:= RunNelderMead(s,m);
          end;
      if ApplyModel or wApplySigmoidToBuffer then
        ApplySigmoidPenumbraFit(ASource,ADestination);
      twSigmoidDone:= True;
      end;
  {$IFDEF WELLHOFER_DUMPDATA}
  DumpData('SigmoidPenumbraFit',ADestination,ASource);
  {$ENDIF}
  Dec(FActiveCnt);
  end;
Result:= SigmoidFitAvailable(ASource);
end; {~sigmoidpenumbrafit}
{$pop}


{03/02/2017}
function TWellhoferData.SigmoidFitAvailable(ASource:twcDataSource=dsMeasured): Boolean;
begin
with wSource[ASource] do
  Result:= (twSigmoidFitData[twcLeft].twFitValid and twSigmoidFitData[twcRight].twFitValid);
end; {~sigmoidfitavailable}


{01/12/2015}
{14/01/2017
  avoid pos=0
  introduction twSigmoidOffset}
{25/05/2017 NearestPosition forced to valid range}
{17/09/2020 introduction of FFrozen}
function TWellhoferData.ApplySigmoidPenumbraFit(ASource     :twcDataSource=dsMeasured;
                                                ADestination:twcDataSource=dsMeasured): Boolean;
var s: twcSides;
    i: Integer;
begin
Result:= (not FFrozen) and (ScanType in twcHoriScans);
if Result and not SigmoidFitAvailable(ASource) then
  Result:= SigmoidPenumbraFit(ASource);
if Result then
  begin
  if (ASource<>ADestination) then
    CopyCurve(ASource,ADestination);
  wSource[ADestination].twComposite    := True;
  wSource[ADestination].twRelatedSource:= ASource;
  for s:= twcLeft to twcRight do
    with wSource[ADestination],twSigmoidFitData[s] do
      if twFitValid then
        for i:= NearestPosition(twFitLowCm,ADestination) to NearestPosition(twFitHighCm,ADestination) do
          if twPosCm[i]<>twFitOffsetCm then
            twData[i]:= twFitNormalisation*RawLogisticFunction(twNMReport.BestVertex,twPosCm[i],twFitOffsetCm);
  end;
end; {~applysigmoidpenumbrafit}


{23/11/2017}
{24/11/2017 positionweighting added, and changed to conditional feature}
{25/11/2017 resampling implemented}
{17/09/2020 introduction of FFrozen}
function TWellhoferData.Integrate(FirstPosCm,LastPosCm:twcFloatType;
                                  ASource             :twcDataSource=dsMeasured;
                                  UseResampling       :Boolean     =False
                                 {$IFDEF POSINTEGRAL};
                                  PositionWeighted    :Boolean     =False
                                 {$ENDIF}                                   ): twcFloatType;
var i,j,k       : Integer;
    StepCm,Limit: twcFloatType;
begin
Result:= 0;
if (not FFrozen) and (FirstPosCm<LastPosCm) then
  with wSource[ASource] do if twValid then
    begin
    if UseResampling then
      begin
      k    := 2048;
      Limit:= Max(0.005,twStepSizeCm/4);
      repeat
        k     := k shr 1;
        StepCm:= (LastPosCm-FirstPosCm)/k;
      until StepCm>=Limit;
      for i:= 0 to k do
        Result:= Result+GetQfittedValue(FirstPosCm+i*StepCm,ASource)*StepCm{$IFDEF POSINTEGRAL}*ifthen(PositionWeighted,StepCm,1){$ENDIF};
     {$IFDEF POSINTEGRAL}
      if PositionWeighted then
        Result:= Result/(LastPosCm-FirstPosCm);
     {$ENDIF}
      end
    else
      begin
      j:= NearestPosition(FirstPosCm,ASource);
      k:= NearestPosition(LastPosCm ,ASource);
      {$IFDEF POSINTEGRAL}
      if (j<k) or ((j=k) and (not PositionWeighted)) then
        begin
        for i:= j to k do
          Result:= Result+(twPosCm[Min(Succ(i),twScanLast)]-twPosCm[Max(twScanFirst,Pred(i))])*twData[i]*ifthen(PositionWeighted,twPosCm[i],1)/2;
        if PositionWeighted then
          Result:= Result/(LastPosCm-FirstPosCm);
        end;
      {$ELSE}
      if j<=k then
        for i:= j to k do
          Result:= Result+(twPosCm[Min(Succ(i),twScanLast)]-twPosCm[Max(twScanFirst,Pred(i))])*twData[i]/2;
      {$ENDIF}
      end;
    end;
end; {~integrate}


(* ****BistroMath core function****
This function is original work of Theo van Soest.
output: wSource[ADestination].twLevelPos[dDerivative], and (in most cases)
                               twCenterPosCm, twUsedEdgeLevel
Calculating a derivative by definition calculates differences, and therefore increases the noise.
For standard conventional profiles the process could stop there. One clear positive and negative peak aar found.
For wedges the situation is not obvious. The high dose side will be found without problems,
the tow side will drown in a huge number of nearly identical signals.
By using statistics the correct edge can be found. A 'dead band' is created to include the umbra region, also for wedges and FFF.
The peak in the derivative is modelled with a 2nd order polynomal to find the best possible peak position.
*)
{23/06/2009}
{14/07/2015 Partial edge detection is now accepted
  More strict on false edge detection by setting rules in twMinArr and twMaxArr}
{20/07/2015 twComposite added}
{29/07/2015 prefilter option added}
{31/07/2015 statistics added to validate twminarr and twmaxarr}
{04/08/2015 alleviated statistics rules for wedged profiles}
{06/08/2015 check on twFilterPoints}
{17/12/2015 repair for local peaks}
{22/07/2016 wCenterDefinition}
{15/11/2016 twCenterPosDefUse}
{22/11/2016 ReportDifferences added}
{11/02/2017 check for missing penumbra: MaxArr=twScanFirst or MinArr=twScanLast}
{13/04/2017 wCenterDefinition should be used with appriate fieldclass}
{13/06/2017 div0 save division could not handle case with extremely small value (orthovolt data)}
{26/01/2018 use dsMeasured/dsRefFiltered}
{28/01/2018 twcCoupledSources}
{12/06/2018 repeated filtering on first derivative tended to shift the peak, not understood in code, probably peaks are too sharp with available points}
{11/09/2018 preserve borders in quadfilter}
{30/10/2019 Sampler size on create corrected}
{10/04/2020 lowerbound set to twDataFirst: Pc:= Max(twDataFirst,Pred(twScanFirst)); }
{24/05/2020 more discrete filterwidth limit in StepFilter}
{14/07/2020 no check anymore on minimum filterwidth; any width>=0 is acceptable for StepFilter}
{21/07/2020 GetAdjustedFilterWidthCm}
{04/09/2020 at least 3 points symmetric in StepFilter; peak modelling only when more than 2 points outside dead band}
{09/09/2020 out of range error repaired: Valid:= InRange(Y,twPosCm[Max(twScanFirst,Nearest-n)],twPosCm[Min(Nearest+n,twScanLast)])}
{17/09/2020 introduction of FFrozen}
{23/10/2020 review of peak statistics and peak fitting loops; errors introduced between 2016 en 2018
 -NumBins was again (a second time) reduced for right side
 -when WedgeData was true, m for left side was obtained from right peak statistics
 -loops stopped at k=1 but had exit approval for k=0 situation}
{18/02/2021 limitations on edge range: twcDefEdgeRangeCm}
function TWellhoferData.Derive(cm          :twcFloatType =twcDefaultValue;
                               ASource     :twcDataSource=dsMeasured;
                               ADestination:twcDataSource=dsCalculated;
                               PreFilter   :Boolean     =False): twcFloatType;
const PeakRatio=2;
var L                   : TLinFit;
    i,j,k,m,P1,P2,Pc,
    MinArr,MaxArr       : Integer;
    tmpData             : twcFloatArray;
    DataPtr             : twcFloatArrayPtr;
    Sampler             : THistogramSampler;
    Q                   : TQuadFit;
   {$IFDEF FIXED_DISTANCE_DERIVATIVE}
    {$IFDEF DISCRETE_FIXED_DISTANCE}
    kcm2                : Integer;
    {$ELSE}
    cm2                 : twcFloatType;
    {$ENDIF DISCRETE_FIXED_DISTANCE}
   {$ENDIF FIXED_DISTANCE_DERIVATIVE}
    DeadBandLow,DeadBandHigh,
    LocalMin,LocalMax,
    Y,PeakWidth,RangeCm,
    GlobalMin,GlobalMax : twcFloatType;
    PeakAtMax,WedgedData,
    HighPassed,LowPassed: Boolean;
    fSource             : twcDataSource;

  {$IFDEF FIXED_DISTANCE_DERIVATIVE}
   procedure StepFilter;                                                        //fixed distance variant with at least 3 points
   begin
   with wSource[ASource] do
     begin
     Inc(Pc);
    {$IFDEF DISCRETE_FIXED_DISTANCE}
     while (Round(twcDefDiscretisationMult*Abs(twPosCm[P1]-twPosCm[Pc]))>kcm2) and (Pc-P1>1) do                  //alleviate truncation errors
    {$ELSE}
     while (Abs(twPosCm[P1]-twPosCm[Pc])>cm2) and (Pc-P1>1) do
    {$ENDIF DISCRETE_FIXED_DISTANCE}
       begin
       L.Del_XY(twPosCm[P1],DataPtr^[P1]);
       Inc(P1);
       end;
    {$IFDEF DISCRETE_FIXED_DISTANCE}
     while (P2<=twDataLast) and ((P2-P1<3) or (Round(twcDefDiscretisationMult*Abs(twPosCm[P2]-twPosCm[Pc]))<kcm2)) do  //alleviate 64 bit truncation errors
    {$ELSE}
     while (P2<=twDataLast) and ((P2-P1<3) or (Abs(twPosCm[P2]-twPosCm[Pc])<cm2)) do
    {$ENDIF DISCRETE_FIXED_DISTANCE}
       begin
       L.Add_XY(twPosCm[P2],DataPtr^[P2]);
       Inc(P2);
       end;
     end;
   end;
  {$ELSE}
   procedure StepFilter;                                                        //fixed number points variant
   var i: Integer;
   begin
   with wSource[ASource],L do
     begin
     Inc(Pc);
     i:= Pc+P1;
     if InRange(i,twDataFirst,twDataLast) then
       Add_XY(twPosCm[i],DataPtr^[i]);
     i:= Pc-P2;
     if InRange(i,twDataFirst,twDataLast) then
       Del_XY(twPosCm[i],DataPtr^[i]);
     end;
   end;
  {$ENDIF FIXED_DISTANCE_DERIVATIVE}


  procedure SetGlobalMax(Y:twcFloatType);
  begin
  if (LocalMax>GlobalMax) and (LocalMax>Y) then                                 //test previous localmax
    begin
    wSource[ADestination].twMaxArr:= MaxArr;
    GlobalMax                     := LocalMax;
    end;
  if (LocalMin<GlobalMin) and (LocalMin<Y) then                                 //test previous localmin
    begin
    wSource[ADestination].twMinArr:= MinArr;
    GlobalMin                     := LocalMin;
    end;
  end; {setglobalmax}

  procedure FindDeadBandMinMax(FindMax:Boolean);
  var i: Integer;
      Y: twcFloatType;
  begin
  with wSource[ADestination] do
    for i:= P1 to P2 do
      begin
      Y:= twData[i];
      if FindMax then
        begin
        if Y>DeadBandHigh then
          begin
          if (Y>LocalMax) and (PeakAtMax or (abs(twPosCm[MinArr]-twPosCm[i])>PeakWidth)) then
            begin                                                               //find localmax
            LocalMax:= Y;
            MaxArr  := i;
            end;
          end; {p>bandhigh}
        end
      else if Y<DeadBandLow then
        begin
        if (Y<LocalMin) and ((not PeakAtMax) or (abs(twPosCm[MaxArr]-twPosCm[i])>PeakWidth)) then
          begin
          LocalMin:= Y;
          MinArr  := i;
          end;
        end {p<bandlow}
      else
       SetGlobalMax(Y);
      end;
  SetGlobalMax(0);
  end; {finddeadbandminmax}

  procedure RunSampler(AStart,AStop:Integer);
  var i: Integer;
  begin
  Sampler.Initialize;
  with wSource[ADestination] do
    repeat                                                                      //make histogram of values of derivative with largest bin below limit
      if Sampler.Samples>0 then
        Sampler.NumBins:= Round(1.5*Sampler.NumBins); {implicit initialise}
      for i:= Max(twDataFirst,AStart) to Min(AStop,twDataLast) do
        Sampler.Add_X(twData[i]);
    until (Sampler.BinFraction[Sampler.LargestBin]<twcDeriveBinFraction) or (Sampler.NumBins>Sampler.Samples div 2);
  end; {runsampler}

begin
if (not FFrozen) and wSource[ASource].twValid then
  begin
  fSource:= ASource;
  Inc(FActiveCnt);
  if PreFilter then
    begin
    if ASource in twcFilterSources then
      fSource:= twcCoupledSources[ASource];
    if not wSource[fSource].twValid then
      QuadFilter(-1,ASource,fSource,False);
    end;
  if ADestination=fSource then
    begin
    tmpData:= Copy(wSource[fSource].twData,0,wSource[fSource].twPoints);
    DataPtr:= @tmpData;
    end
  else
    begin
    CopyCurve(fSource,ADestination);
    DataPtr:= @wSource[fSource].twData;
    end;
  with wSource[ADestination] do
    begin
    if (cm<0.1) or (twSetFieldType=fcSmall) then
      cm:= GetAdjustedFilterWidthCm(ADestination);
    twFilmData       := False;
    if twFilterPoints<1 then
      twFilterPoints:= Max(1,Ceil(Cm/Max(0.0001,Abs(twStepSizeCm))));
   {$IFDEF FIXED_DISTANCE_DERIVATIVE}
    {$IFDEF DISCRETE_FIXED_DISTANCE}
    kcm2:= Round(twcDefDiscretisationMult*cm/2);                                //alleviate truncation errors
    {$ELSE}
    cm2:= cm/2;
    {$ENDIF DISCRETE_FIXED_DISTANCE}
    P1 := twDataFirst;
    P2 := P1;
    Pc := Pred(P1);
   {$ELSE}
    P1 := twFilterPoints div 2;
    P2 := twFilterPoints-P1;
    Pc := -P2-1;
  {$ENDIF FIXED_DISTANCE_DERIVATIVE}
    L:= TLinFit.Create;
    while Pc<twDataLast do
      try
        StepFilter;
        if Pc>=0 then
          twData[Pc]:= L.Linear;         //======================first derivative is calculated here====================
       except
        twData[Pc]:= 0;
       end;
    FreeAndNil(L);
    {$IFDEF WELLHOFER_DUMPDATA}
    DumpData('Raw derivative',ADestination,ASource);
    {$ENDIF}
    Pc:= Max(twDataFirst,Pred(twScanFirst));                                    //may be local peak
    GlobalMin:= twData[Pc];
    GlobalMax:= GlobalMin;
    while Pc<twScanLast do
      try
        Inc(Pc);
        Y:= twData[Pc];
        if Y>GlobalMax      then
          begin
          GlobalMax:= Y;
          MaxArr   := Pc;
          end
        else if Y<GlobalMin then
          begin
          GlobalMin:= Y;
          MinArr   := Pc;
          end;
       except
        twData[Pc]:= 0;
       end;
    PeakAtMax:= GlobalMax>abs(GlobalMin);
    if PeakAtMax then
      begin
      if Abs(GlobalMin)<1e-2 then Y:= PeakRatio
      else                        Y:= abs(GlobalMax/GlobalMin);                 //div0 safe
      Pc:= MaxArr;
      end
    else
      begin
      if Abs(GlobalMax)<1e-2 then Y:= PeakRatio
      else                        Y:= abs(GlobalMin/GlobalMax);                 //div0 safe
      Pc:= MinArr;
      end;
    i := 0;
    Pc:= EnsureRange(Pc,Succ(twScanFirst),Pred(twScanLast));
    while ((Pc-i)>twScanFirst) and ((Pc+i)<twScanLast) and (abs(twData[Pc-i])+abs(twData[Pc+i])>abs(twData[Pc])) do
      Inc(i);
    PeakWidth        := ifthen(Y>=PeakRatio,3,0.5)*abs(twPosCm[Pc+i]-twPosCm[Pc-i]);
    try
      twDerivativeValid:= (DataPtr^[twScanFirst]/twMaxValue<twcDeriveMinMax) or (DataPtr^[twScanLast ]/twMaxValue<twcDeriveMinMax);
     except
      twDerivativeValid:= False;
     end;
    if twDerivativeValid then                                                   //create histogram to find most populated band
      begin
      Sampler:= THistogramSampler.Create(GlobalMin,GlobalMax,0,twDataLast);
      Q      := TQuadFit.Create;
      RunSampler(twScanFirst,twScanLast);
      if LogLevel>2 then
        begin
        DeadBandLow:= Sampler.BinRangeLow;
        P1         := Max(2,Sampler.NumBins);
        Y          := (Sampler.BinRangeHigh-DeadBandLow)/P1;
        P2         := Ceil(Log10(Pred(P1)));
        j          := Max(0,Pred(Sampler.LargestBin));
        i          := j;
        while (i>0) and (Sampler.BinCounts[i]>1) do
          Dec(i);
        repeat
          Pc:= Sampler.BinCounts[i];
          StatusMessage(Format('->Derivative[%d] bin[%*d]: %6.2f .. %6.2f: %d',
                        [Ord(ASource),P2,i,
                         DeadBandLow+i*Y,DeadBandLow+Succ(i)*Y,Pc]));
          Inc(i);
        until ((i>j) and (Pc<2)) or (i>P1);
        end; {loglevel}
      DeadBandLow    := Sampler.LargestBinValue-Sampler.BinSize/2;              //define band around largest bin
      DeadBandHigh   := DeadBandLow+Sampler.BinSize;
      LocalMin       := DeadBandLow;
      LocalMax       := DeadBandHigh;
      GlobalMin      := DeadBandLow;
      GlobalMax      := DeadBandHigh;
      twFilterString := twDerivativeStg;
      twValid        := True;
      twFastScan     := False;
      twIsDerivative := True;
      P1             := twScanFirst;
      P2             := twScanLast;
      HighPassed     := False;
      LowPassed      := False;
      if MaxArr<twScanFirst+twcDeriveLookAhead then
        P1:= MaxArr
      else
        while (((twData[P1]>DeadBandLow)  and (not HighPassed) and (twData[P1]>twData[P1+twcDeriveLookAhead])) or
               ((twData[P1]<DeadBandHigh) and (not LowPassed)  and (twData[P1]<twData[P1+twcDeriveLookAhead]))   ) and
               (P1+twcDeriveLookAhead+1<P2) do                                  //(above bandlow and decreasing) OR (below bandhigh and rising)
          begin
          if      twData[P1]<DeadBandLow  then LowPassed := True
          else if twData[P1]>DeadBandHigh then HighPassed:= True;
          Inc(P1);
          end;
      HighPassed     := False;
      LowPassed      := False;
      if MinArr>twScanLast-twcDeriveLookAhead then
        P2:= MinArr
      else
        while (((twData[P2]>DeadBandLow ) and (not HighPassed) and (twData[P2]>twData[P2-twcDeriveLookAhead])) or
               ((twData[P2]<DeadBandHigh) and (not LowPassed)  and (twData[P2]<twData[P2-twcDeriveLookAhead]))   ) and
               (P2-twcDeriveLookAhead-1>P1) do
         begin
         if      twData[P2]<DeadBandLow  then LowPassed := True
         else if twData[P2]>DeadBandHigh then HighPassed:= True;
         Dec(P2);
         end;
      MinArr:= twScanFirst;
      MaxArr:= MinArr;
      FindDeadBandMinMax(PeakAtMax);       //find max above dead band; set twMaxArr, twMinArr
      FindDeadBandMinMax(not PeakAtMax);   //find min below dead band
      twMaxPosCm:= twPosCm[twMaxArr];      //for robustness
      with twLevelPos[dDerivative] do      //now set left and right derivative border position
        begin                              //Some extra measures for seriously wedged profiles; the criteria are alleviated
        try                    //===================statistics for peaks=========================
          Y:= DataPtr^[twMaxArr]/DataPtr^[twMinArr]; {find range in original data at peaks of derivative}
          if Y<=0 then WedgedData:= False
          else         WedgedData:= Max(Y,1/Y)>4;
         except
          WedgedData:= False;
         end;
        if WedgedData then                 // data ordening assumed; check if band out of lowest 10% and highest 90%
           begin
           i:= Max(10,Abs(twMaxArr-twMinArr) div 5);
           j:= twcDeriveStatsBinWDiv;
           Sampler.NumBins:= Sampler.NumBins div 10;
           RunSampler(twMaxArr-i,twMaxArr+i);
           end                                                                  //run sampler for wedgeddata left side
        else
           j:= twcDeriveStatsBinDiv;
        Penumbra[twcLeft ].Valid:= (Sampler.LargestBin<(Pred(j)*Sampler.NumBins) div j);
        m                       := Sampler.CountAbove[Sampler.LargestBin];      //data points above dead band for left side
        if WedgedData then
           RunSampler(twMinArr-i,twMinArr+i);                                   //run sampler again for wedgeddata right side
        Penumbra[twcRight].Valid:= (Sampler.LargestBin>Sampler.NumBins div j);
        if not Penumbra[twcRight].Valid then
          twMinArr:= EnsureRange(twMinArr,Min(twMaxArr+5,twDataLast),twDataLast)
        else if not Penumbra[twcLeft].Valid then
          twMaxArr:= EnsureRange(twMaxArr,twDataFirst,Max(twMinArr-5,twDataFirst));
        Level  := 0;              //=================================fit of peaks==============================
        RangeCm:= twcDefEdgeRangeCm*twSDD2SSDratio*twSSD_cm/100;
        with Penumbra[twcLeft] do if Valid then                                 //left side
          begin
          Nearest:= twMaxArr;
          Valid  := False;
          k      := 2;
          while (Nearest<twMinArr) and (not Valid) and (k>=0) do                //try to fit peak, reduce points when needed
            begin
            i    := Max(twScanFirst,Nearest-k);
            while twPosCm[Nearest]-twPosCm[i]>RangeCm do Inc(i);
            j    := Min(Nearest+k,twScanLast);
            while twPosCm[j]-twPosCm[Nearest]>RangeCm do Dec(j);
            m    := Min(m,Succ(j-i));
            Calc := twPosCm[twMaxArr];
            if (k=0) and (i=twMaxArr) then
              Valid:= True
            else if m>2 then                                                    //enough points available
              begin
              Q.Initialize;
              for Pc:= i to j do                                                //test also that data are on penumbra
               Q.Add_XY(twPosCm[Pc],twData[Pc]);
              Y    := Q.TopX;
              Valid:= InRange(Y,twPosCm[Max(twScanFirst,Nearest-k)],twPosCm[Min(Nearest+k,twScanLast)]);
              if Valid then
                Calc:= Y;                                                       //best possible calculation when enough points availabe
              end
            else
              k:= 1;
            Dec(k);
            end;
          if wCenterDefinition[twSetFieldType]=CenterPenumbra then
            begin
            twCenterPosCm    := Calc;
            twCenterPosDefUse:= dUseDerivative;
            end;
          end;
        with Penumbra[twcRight] do if Valid then                                //repeat for right side
          begin
          Nearest:= twMinArr;
          Valid  := False;
          k      := 2;
          m      := Sampler.CountBelow[Sampler.LargestBin];                     //data points below dead band
          while (Nearest>twMaxArr) and (not Valid) and (k>=0) do
            begin
            i    := Max(twScanFirst,Nearest-k);
            while twPosCm[Nearest]-twPosCm[i]>RangeCm do Inc(i);
            j    := Min(Nearest+k,twScanLast);
            while twPosCm[j]-twPosCm[Nearest]>RangeCm do Dec(j);
            m    := Min(m,Succ(j-i));
            Calc := twPosCm[MinArr];
            if (k=0) and (j=MinArr) then
              Valid:= True
            else if m>2 then                                                    //enough points available
              begin
              Q.Initialize;
              for Pc:= i to j do
                Q.Add_XY(twPosCm[Pc],twData[Pc]);
              k    := Max(1,k div 2);
              Y    := Q.TopX;
              Valid:= InRange(Y,twPosCm[Max(twScanFirst,Nearest-k)],twPosCm[Min(Nearest+k,twScanLast)]);
              if Valid then
                Calc := Y;                                                      //best possible calculation when enough points availabe
              end
            else
              k:= 1;
            Dec(k);
            end;
          if wCenterDefinition[twSetFieldType]=CenterPenumbra then
            twCenterPosCm:= (Calc+twCenterPosCm)/2;
          twUsedEdgeLevel:= dDerivative;
          end;
        if LogLevel>2 then
          StatusMessage(Format('->Derivative curve[%d]: %0.3f cm (%d) / %0.3f cm (%d)',
                        [Ord(ASource),
                        Penumbra[twcLeft ].Calc,Penumbra[twcLeft ].Nearest,
                        Penumbra[twcRight].Calc,Penumbra[twcRight].Nearest]));
        end; {dEdge}
      FreeAndNil(Q);
      FreeAndNil(Sampler);
      end; {twEdgeDerivative, inner level}
    if fSource=ADestination then
      Finalize(tmpData);
    twComposite    := True;
    twRelatedSource:= ASource;
    if wCenterDefinition[twSetFieldType]=CenterPenumbra then
      begin
      with twLevelPos[dDerivative] do
        twCenterPosValid:= Penumbra[twcLeft].Valid and Penumbra[twcRight].Valid;
      if not twCenterPosValid then
        begin
        twCenterPosCm    := EnsureRange(0,twFirstDataPosCm,twLastDataPosCm);
        twCenterPosDefUse:= dUseUndefined;
        end;
      end;
    Result:= twCenterPosCm;
    end; {with ADestination}
  {$IFDEF WELLHOFER_DUMPDATA}
  DumpData('Derive',ADestination,ASource);
  DumpData('Derive Points',dsUnrelated);
  {$ENDIF}
  Dec(FActiveCnt);
  end {if}
else
  Result:= 0;
end; {~derive}


{20/07/2015 twComposite added}
{30/07/2015:
  Also twcCoordinate excluding beam-direction mirrored.
  twMirrored is inverted insted of set to true.}
{07/08/2015 Analyse replaced by setting to not analysed}
{18/01/2017 ARotationPoint}
{06/06/2017 ARotationPoint must be doubled!}
{03/06/2018 initborders}
{17/09/2020 introduction of FFrozen}
function TWellhoferData.Mirror(ASource       :twcDataSource=dsMeasured;
                               ADestination  :twcDataSource=dsMeasured;
                               ARotationPoint:twcFloatType =0         ): Boolean;
var i      : Integer;
    mAxis  : twcMeasAxis;
    ShiftCm: twcFloatType;
begin
Result:= (not FFrozen) and wSource[ASource].twValid;
if Result then
  begin
  if ASource<>ADestination then
    CopyCurve(ASource,ADestination); {kopieer parameters en data}
  with wSource[ADestination] do
    begin
    ARotationPoint   := 2*ARotationPoint;
    ShiftCm          := twShiftCm;
    twDerivativeValid:= False;
    twFastScan       := False;
    twMirrored       := not twMirrored;
    twComposite      := True;
    twRelatedSource  := ASource;
    Result           := twMirrored;
    Shift(0,AbsShift,ADestination);   {remove any shift}
    for i:= twDataFirst to twDatalast do
      begin
      for mAxis:= Inplane to CrossPlane do
        twCoordinates[i].m[mAxis]:= -twCoordinates[i].m[mAxis];
      twPosCm[i]:= ARotationPoint-twPosCm[i];
      end;
    CheckDataOrdering(ADestination);
    Shift(ShiftCm,AbsShift,ADestination);  {restore shift}
    InitBorders(ADestination);
    twFastScan:= False;
    twAnalysed:= False;
    end; {with}
  end;
end; {~mirror}


{17/07/2015}
{19/07/2015
  Scaleoverlap added as option.
  In loop: switch to aSource when last point of aDestination is done by using VerPos.}
{20/07/2015 twComposite added}
{21/07/2015 total shift reported}
{24/07/2015:
  usage of GetCurveIDString
  Mayneordcorrection
  shift and Mayneord always applied to lower part of data.}
{28/07/2015 analyse added}
{15/08/2015
  GetInterpolatedValue used}
{17/08/2015 sort source and destination on ssd for vertscans}
{13/10/2016 SourceID:= GetCurveIDString(ASource,[StripExtension=]True)}
{13/01/2017 sort source and destination on ssd for vertscans but do not exchange}
{03/06/2018 initborders}
{27/08/2020 twMaxPosCm, twMaxValue}
{17/09/2020 introduction of FFrozen}
function TWellhoferData.Merge(ASource      :twcDataSource=dsUnrelated;
                              ADestination :twcDataSource=dsMeasured;
                              ShiftSourceCm:twcFloatType =0;
                              DoMatch      :Boolean     =True;
                              ScaleOverlap :Boolean     =True): Boolean;
var SumSource,SumDest: twcFloatType;
    SourceID,DestID  : String;
    tSource,tDest    : twcDataSource;

  function NextCombinedPos(APos  :twcFloatType;
                           Source:twcDataSource=dsMeasured): twcFloatType;
  begin
  Result:= NextPos(APos,Source);
  if (Result=APos) then
    begin
    if Source=ADestination then Source:= ASource
    else                        Source:= ADestination;
    Result:= NextPos(APos,Source);
    end;
  end;

  procedure Loop(Preparation:Boolean);
  var CurPos,EndPos,VerifiedPos,c: twcFloatType;
      InDest,InSource            : Boolean;
      i                          : Integer;
  begin
  CurPos     := Min(wSource[aSource].twFirstDataPosCm,wSource[ADestination].twFirstDataPosCm);
  EndPos     := Max(wSource[aSource].twLastDataPosCm ,wSource[ADestination].twLastDataPosCm );
  VerifiedPos:= CurPos-1;
  if (not Preparation) and (SumSource<>0) then c:= SumDest/SumSource
  else                                         c:= 1;
  while CurPos<EndPos do {first test scaling in overlap zone}
    begin
    InSource:= InRange(CurPos,wSource[ASource     ].twFirstDataPosCm,wSource[ASource     ].twLastDataPosCm);
    InDest  := InRange(CurPos,wSource[ADestination].twFirstDataPosCm,wSource[ADestination].twLastDataPosCm);
    if InDest and (CurPos>VerifiedPos) then
      begin
      i          := NearestPosition(CurPos,ADestination);
      VerifiedPos:= wSource[ADestination].twPosCm[i];
      if InSource then
        begin
        if Preparation then
          begin {sumsource and sumdest are by defintion based on same number of points}
          SumSource:= SumSource+GetInterpolatedValue(VerifiedPos,ASource     );
          SumDest  := SumDest  +GetInterpolatedValue(VerifiedPos,ADestination);
          end
        else {merge overlap}
          wSource[ADestination].twData[i]:= (wSource[ADestination].twData[i]+c*GetInterpolatedValue(VerifiedPos,ASource))/2;
        end;
      CurPos:= NextCombinedPos(VerifiedPos,ADestination);
      end
    else
      begin {insource}
      CurPos:= NextCombinedPos(CurPos,ASource);
      i     := NearestPosition(CurPos,ASource);
      if not Preparation then                                                   //add non-overlapped portion of source
        InsertPoint(CurPos,c*GetInterpolatedValue(CurPos,ASource),wSource[ASource].twCoordinates[i],ADestination);
      end;
    end;
  end; {loop}

  function LimitedIDstg(AString:String): String;
  var i: Integer;
  begin
  i:= Pos('d',AString);
  if i>0 then Result:= Copy(AString,0,i)
  else        Result:= AString;
  end;

begin
Inc(FActiveCnt);
SourceID:= GetCurveIDString(ASource     ,True);
DestID  := GetCurveIDString(ADestination,True);
if ScanType in twcVertScans then
  begin
  SourceID:= LimitedIDstg(SourceID);
  DestID  := LimitedIDstg(DestID  );
  end;
Result:= (not FFrozen) and (SourceID=DestID);
if Result then
  begin
  if ScanType in twcVertScans then
    begin
    if wSource[ASource].twSSD_cm>wSource[ADestination].twSSD_cm then
      begin                                                                     //tsource must have smallest ssd, therfore exchange}
      tSource:= ADestination;
      tDest  := ASource;
      end
    else
      begin
      tSource:= ASource;
      tDest  := ADestination;
      end;
    Shift(ShiftSourceCm,RelShift,tSource);
    if (wSource[tSource].twSSD_cm<>wSource[tDest].twSSD_cm) then
      Mayneord(wSource[tSource].twSSD_cm,
               wSource[tDest  ].twSSD_cm,
               wSource[tDest  ].twMaxPosCm,tSource);
    end
  else                                                                          //horizontal
    begin
    Shift(ShiftSourceCm,RelShift,ASource);
    if DoMatch then
      ShiftSourceCm:= ShiftSourceCm+Match(ASource,ADestination);                //match asource on adestination
    end;
  SumSource:= 0;
  SumDest  := 0;
  if ScaleOverlap then
    Loop(True);                                                                 //evaluate scaling factor
  Loop(False); {now do the actual merge}
  wSource[ADestination].twDataHistoryStg:= Format('%s<%0.2f cm>%s',[wSource[ADestination].twDataHistoryStg,ShiftSourceCm,wSource[ASource].twDataHistoryStg]);
  wSource[ADestination].twComposite    := True;
  wSource[ADestination].twFastScan     := False;
  wSource[ADestination].twRelatedSource:= ASource;
  InitBorders(ADestination);
  Result:= Analyse(ADestination);
  end
else
  begin
  ReportDifferences(ASource,ADestination);
  StatusMessage(Format('Merging not accepted (%s <> %s)',[SourceID,DestID]));
  end;
Dec(FActiveCnt);
end; {~merge}


(*
This function is original work of Theo van Soest.
A complete gamma analysis produces for each measured point
-the dose difference (DD) with the reference (optionally with interpolation)
-the distance to agreement (DTA)
-the shortest vector (Gamma) in dose/position space scaled to unit vectors {n}% dose and {d} mm distance.
DD might be scaled to a global level or local dose.
The shortest vector should be found with some interpolation mechanism after a searching step.
For 1D data sets as in this unit this is relatively simple: from a number of point the Gamma value is interpolated.
Any gamma analysis implementation needs a number of parameters and makes interpolation choices. Therefore each implementation is different.
In this implementation
-twcGammaLocalDosePerc   : use local dose
-twcGammaCutoffPercent   : cutoff for profiles
-twcGammaCutoffDepth     : cutoff depth for pdd
-twcGammaDistCmStep      : step size for searching between measurerment points
-twcGammaSearchMultiplier: limit search to Gamma-value at distance 0 multiplied with this factor
-twcGammaDosePercBase    : unit vector for dose
-twcGammaDistCmBase      : unit vecor distance

See for instance
Li et al.: Investigation of gamma-index with surface based distance method; Med. Phys. 38 (12), December 2011
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3298565/
*)
{20/07/2015 twComposite added}
{17/09/2015 preparedata: force postanalysis QuadFilter(-1,ACurve,ACurve,True)}
{14/10/2018
  if InFieldAreaOnly AND (ScanType in twcHoriScans)
  set all invalid/out of range data points to -1 }
{26/11/2018 autoscaling, prefilter before scaling}
function TWellhoferData.GammaAnalysis(ASource        :twcDataSource=dsMeasured;
                                      AReference     :twcDataSource=dsReference;
                                      ADestination   :twcDataSource=dsCalculated;
                                      InFieldAreaOnly:Boolean      =True;
                                      AutoScaling    :Boolean      =True;
                                      SourceScaling  :twcFloatType =1;
                                      PreFilter      :Boolean      =True;
                                      PostFilter     :Boolean      =True): twcFloatType;
var tmpSCurve,tmpRCurve                                     : twCurveDataRec;
    fSource,fReference                                      : twcDataSource;
    i,Lpos,Rpos,a1,a2                                       : integer;
    Gamma,StartLimit,Limit,LastGamma,Distance,UniformScaling: twcFloatType;
    t                                                       : TStatssampler;
    IsPDD                                                   : Boolean;
    Q                                                       : TQuadFit;

  {28/04/2020 initcurve}
  //When there is no Autoscaling at least the curves should be uniformly normalised to 100 at some reference point;
  //therefore take this from the fSource value.
  procedure PrepareData(ACurve      :twcDataSource;
                        IsSource    :Boolean;
                        var tmpCurve:twCurveDataRec);
  begin
  InitCurve(tmpCurve);
  CopyCurve(wSource[ACurve],tmpCurve);
  if PreFilter then
    QuadFilter(-1,ACurve,ACurve,True);
  Multiply(100*ifthen(IsSource,SourceScaling,1)/ifthen(AutoScaling,Max(twcMinNormVal,wSource[ACurve].twAppliedNormVal),UniformScaling),ACurve,ACurve);
  end;

  procedure RestoreData(ACurve      :twcDataSource;
                        var tmpCurve:twCurveDataRec);
  begin
  CopyCurve(tmpCurve,wSource[ACurve]);
  ClearCurve(tmpCurve,True);
  end;

  function Set_f(tSource:twcDataSource): twcDataSource;
  begin
  if PreFilter then Result:= CheckAlternativeSource(tSource,True) {assures initialised source}
  else              Result:= tSource;
  end;

  //calculate normalised vector for x=pos[p]+offsetcm
  function CalcGamma(p       :integer;
                     OffsetCm:twcFloatType): twcFloatType;
  var x,y,s,
      nDistSqr,nDoseSqr: twcFloatType;
      j,k              : Integer;
  begin
  x:= wSource[fSource].twPosCm[p]+OffsetCm;
  j:= NearestPosition(x,fReference);
  y:= 0;
  with Q,wSource[fReference] do
    begin
    if FindCalcRange(x,Lpos,Rpos,fReference) then
      try
        Initialize;
        y        := twData[j];
        if Lpos=Rpos then
          begin
          s:= x-twPosCm[j];                                                     //s is DTA for x and nearest point in reference
          if s<>0 then
            begin                                                               //interpolation
            for k:= Max(twDataFirst,Pred(j)) to Min(Succ(j),twDataLast) do
              if (k=j) or ((twPosCm[k]-twPosCm[j])/s>1) then Add_XY(twPosCm[k],twData[k]);
            y:= FitLin(x);
            end;
          end {lpos=rpos}
        else
          begin
          for k:= Lpos to Rpos do
            Add_XY(twPosCm[k],twData[k]);
          y:= FitQuad(x,wOutlierFilter);
          end;
       except
         Lastgamma:= -1;
       end {if,try}
    else
      y:= -1;
    LastGamma:= y;
    if FitValid and (y>0) then
      begin
      if twcGammaDistCmBase  <=0    then nDistSqr:= 0
      else                               nDistSqr:= Sqr(OffsetCm/twcGammaDistCmBase);
      if twcGammaDosePercBase<=0    then nDoseSqr:= 0
      else if twcGammaLocalDosePerc then nDoseSqr:=  Sqr(100*(wSource[fSource].twData[p]/y-1)/twcGammaDosePercBase)
           else                          nDoseSqr:=  Sqr((    wSource[fSource].twData[p]-y  )/twcGammaDosePercBase);
      LastGamma:= SqRt(nDistSqr+nDoseSqr);
      end
    else
      LastGamma:= -1;
    end;
  Result:= LastGamma;
  end; {calcgamma}

{$push}{$warn 5091 off: Local variable "tmpDCurve" of a managed type does not seem to be initialized}
begin
Inc(FActiveCnt);
Gamma:= 0;
IsPDD:= ScanType in [snPDD,snFanline];
if wSource[ASource].twValid and wSource[AReference].twValid then
  begin
  if AlignReference then
    AlignCurves(AReference,ASource);
  t             := TStatsSampler.Create;
  Q             := TQuadFit.Create;
  fSource       := Set_f(ASource);
  fReference    := Set_f(AReference);
  UniformScaling:= wSource[fSource].twAppliedNormVal;                           //is used in PrepareData when Autoscaling is off
  PrepareData(fSource   ,True ,tmpSCurve);
  PrepareData(fReference,False,tmpRCurve);
  CopyCurve(ASource,ADestination);
  with wSource[ASource] do
    begin
    if InFieldAreaOnly and (ScanType in twcHoriScans) then
      begin
      a1:= twInFieldArr[twcLeft];
      a2:= twInFieldArr[twcRight];
      for i:= twDataFirst to a1         do
        wSource[ADestination].twData[i]:= -1;                                   //clear data out of range
      for i:= a2          to twDataLast do
        wSource[ADestination].twData[i]:= -1;
      end
    else
      begin
      a1:= twScanFirst;
      a2:= twScanLast;
      end;
    for i:= a1 to a2 do
      if (IsPDD       or (twData[i] >twcGammaCutoffPercent)) and
         ((not IsPDD) or (twPosCm[i]>twcGammaCutoffDepth)  ) and
         (GetQfittedValue(twPoscm[i],fReference)>0         ) then
        begin
        Gamma     := CalcGamma(i,0);                                            //calculate gamma at distance=0
        Distance  := -twcGammaDistCmStep;
        StartLimit:= Max(1,Gamma)*twcGammaSearchMultiplier;
        Limit     := StartLimit;
        while InRange(CalcGamma(i,Distance),0,Limit) do                         //loop with small steps to left while (last)gamma sufficiently small
          begin
          if InRange(LastGamma,0,Gamma) then                                    //if smaller set as lowest gamma
            begin
            Gamma:= LastGamma;
            Limit:= Max(1,Gamma)*twcGammaSearchMultiplier;
            end;
          Distance:= Distance-twcGammaDistCmStep;
          end;
        Distance:= twcGammaDistCmStep;
        Limit   := StartLimit;
        while InRange(CalcGamma(i,Distance),0,Limit) do                         //loop with small steps to right
          begin
          if InRange(LastGamma,0,Gamma) then                                    //if LastGamma smaller set as lowest gamma
            begin
            Gamma:= LastGamma;
            Limit:= Max(1,Gamma)*twcGammaSearchMultiplier;
            end;
          Distance:= Distance+twcGammaDistCmStep;
          end;
        wSource[ADestination].twData[i]:= Gamma;
        t.Add_X(Gamma);
        end
      else
        wSource[ADestination].twData[i]:= -1;
    Gamma:= t.ConfidenceLimit;
    try
      FreeAndNil(t);
     except
      ExceptMessage('WH.GammaAnalysis:t!');
     end;
    end; {with asource}
  with wSource[ADestination] do
    begin
    while ((twDataFirst<twDataLast ) and (twData[twDataFirst]<0)) do
      Inc(twDataFirst);
    while ((twDataLast >twDataFirst) and (twData[twDataLast ]<0)) do
      Dec(twDataLast );
    twScanFirst      := twDataFirst;
    twScanLast       := twDataLast;
    twDataHistoryStg := twGammaCalcStg+'('+wSource[ASource].twDataHistoryStg+','+wSource[AReference].twDataHistoryStg+')';
    twIsGamma        := True;
    twIsRelative     := True;
    twComposite      := True;
    twConfidenceLimit:= Gamma;
    end; {with adestination}
  RestoreData(fSource   ,tmpSCurve);
  RestoreData(fReference,tmpRCurve);
  if PostFilter then
    MedianFilter(0,ADestination,ADestination,False,True);
  wSource[ADestination].twRelatedSource:= ASource;
  FastScan(ADestination);
  end;
  try
    FreeAndNil(Q);
   except
    ExceptMessage('WH.!GammaAnalysis:Q');
   end;
Dec(FActiveCnt);
Result:= Gamma;
end; {~gammaanalysis}
{$pop}


{29/07/2015 postanalysis option added}
{24/06/2016 limit symmetry-correction to meaningful range}
{28/01/2018 twcCoupledSources}
{17/09/2020 introduction of FFrozen}
procedure TWellhoferData.CorrectSymmetry(ASource     :twcDataSource=dsMeasured;
                                         PostAnalysis:Boolean     =True);
var i,s: Integer;
    v  : twcFloatType;
begin
with wSource[ASource] do
  if (not FFrozen) and Analyse(ASource) and (twLinSlope<>0) and (abs(twLinSlope)<=twcSymCorrectionLimit)  then
    begin
    s:= NearestPosition(twAbsNormPosCm,ASource);
    v:= 1;
    for i:= s downto twDataFirst do
      begin
      if (twData[i]/twAbsNormValue)>twcSymCorrectionLevel then
        v:= 1-twLinSlope*(twPosCm[i]-twAbsNormPosCm)/twAbsNormValue;
      twData[i]:= twData[i]*v;
      end;
    for i:= Succ(s) to twDataLast do
      begin
      if (twData[i]/twAbsNormValue)>twcSymCorrectionLevel then
        v:= 1-twLinSlope*(twPosCm[i]-twAbsNormPosCm)/twAbsNormValue;
      twData[i]:= twData[i]*v;
      end;
    twSymCorrected:= True;
    twFastScan    := False;
    if ASource in twcFilterSources then
      begin
      PostAnalysis:= True;
      wSource[twcCoupledSources[ASource]].twValid:= False;
      end;
    if PostAnalysis then
      Analyse(ASource)
    else
      FastScan(ASource);
    end;
end;  {~correctsymmetry}


function TWellhoferData.ResetValues(ASource:twcDataSource=dsMeasured): Boolean;
begin
with wSource[ASource] do
  begin
  if twValid then
    begin
    twScanFirst:= twDataFirst;
    twScanLast := twDataLast;
    twFastScan := False;
    end;
  Result:= twValid;
  end;
end; {~resetvalues}


{16/01/2017}
function TWellhoferData.GetFieldWidthCm(ASource:twcDataSource=dsMeasured;
                                        ALevel :twcDoseLevel =d50      ): twcFloatType;
begin
if BordersValid(ASource,ALevel) then with wSource[ASource].twLevelPos[ALevel] do
  Result:= Abs(Penumbra[twcRight].Calc-Penumbra[twcLeft].Calc)
else
  Result:= 0;
end; {~getfieldwithcm}


{27/11/2017 twcDoseLevel=(dLow,dHigh,d20,d50,d80,d90,dUser,dTemp,dDerivative,dInflection,dSigmoid50)}
function TWellhoferData.GetFieldCenterCm(ASource:twcDataSource=dsMeasured;
                                         ALevel :twcDoseLevel =d50      ): twcFloatType;
begin
if BordersValid(ASource,ALevel) then with wSource[ASource].twLevelPos[ALevel] do
  Result:= (Penumbra[twcRight].Calc+Penumbra[twcLeft].Calc)/2
else
  Result:= 0;
end; {~getfieldcentercm}


function TWellhoferData.BordersValid(ASource:twcDataSource=dsMeasured;
                                     ALevel :twcDoseLevel =d50): Boolean;
begin
with wSource[ASource].twLevelPos[ALevel] do
  Result:= Penumbra[twcLeft].Valid and Penumbra[twcRight].Valid;
end; {~bordersvalid}


{11/01/2017}
function TWellhoferData.GetLevelDistance(Level1,Level2:twcDoseLevel;
                                         ASide        :twcSides;
                                         ASource      :twcDataSource=dsMeasured   ): twcFloatType;

begin
with wSource[ASource] do
  if twLevelPos[Level1].Penumbra[ASide].Valid and twLevelPos[Level2].Penumbra[ASide].Valid then
    Result:= Abs(twLevelPos[Level1].Penumbra[ASide].Calc-twLevelPos[Level2].Penumbra[ASide].Calc)
  else
    Result:= 0;
end; {~getleveldistance}


//****BistroMath core function****
//needs: twDataFirst,twDataLast,twMaxPosCm,twScanLength
{17/05/2015
  More stable and faster version by calculating the probable position just once and
  then reducing the search area very fast.
  Introduction of ForceAlwaysIn option. For any backward compatibility the default is off,
  resulting in -1 as out of range result.}
function TWellhoferData.NearestPosition(Position     :twcFloatType;
                                        ASource      :twcDataSource=dsMeasured;
                                        ForceAlwaysIn:Boolean=True          ): Integer;
var i,m,n,s: Integer;
begin
with wSource[ASource] do
  begin
  if (twPoints>2) and (ForceAlwaysIn or InRange(Position,twFirstDataPosCm,twLastDataPosCm)) then
    begin
    m:= twDataFirst;
    n:= twDataLast;
    s:= 1;
    i:= EnsureRange(m+round((n-m)*(Position-twPosCm[m])/(twPosCm[n]-twPosCm[m])),m,n);
    while (n-m>1) and (s<>0) do                                                 //reduce search range by replacing m or n boundary with estimate
      begin
      s:= Sign(Position-twPosCm[i]);
      if (s>0) then m:= i
      else          n:= i;
      if (s<>0) then
        i:= (n+m) div 2;                                                        //replace n or m with avg(n,m)
      end;
    Result:= ifthen(Abs(Position-twPosCm[m])<Abs(twPosCm[n]-Position),m,n);
    end
  else Result:= -1;
  end;
end; {~nearestposition}


//****BistroMath core function****
{19/05/2015}
function TWellhoferData.NextPos(APos   :twcFloatType;
                                ASource:twcDataSource=dsMeasured): twcFloatType;
var i: Integer;
begin
i:= NearestPosition(APos,ASource);
if i<wSource[ASource].twDataLast then
  Result:= wSource[ASource].twPosCm[Succ(i)]
else
  Result:= APos;
end; {~nextpos}


//****BistroMath core function****
{02/11/2020 Lpos not intitialised}
{05/11/2020 Lpos wrong adjustment rule for dec; force at least two points}
function TWellhoferData.FindCalcRange(CalcPosCm    :twcFloatType;
                                      var Lpos,Rpos:Integer;
                                      ASource      :twcDataSource=dsMeasured): Boolean;
var X,c: twcFloatType;
begin
with wSource[ASource] do
  begin
  Result:= twValid and InRange(CalcPosCm,twFirstDataPosCm,twLastDataPosCm);
  if Result then
    begin
    c:= CalcWidth_cm/2;
    X:= CalcPosCm-c;
    try
      Lpos:= Max(0,Trunc((X-twPosCm[twDataFirst])/twStepSizeCm))+twDataFirst;    //preliminary estimation
     except
      Lpos:= twDataFirst;
     end;
    while (Lpos>twDataFirst) and (twPosCm[Lpos]>=X) and (X-twPosCm[Lpos-1]<c) do //failsave for variable stepsize
      Dec(Lpos);
    while (Lpos<twDataLast) and (twPosCm[Lpos+1]<X) do                           //failsave for variable stepsize
      Inc(Lpos);
    X   := X+CalcWidth_cm;
    Rpos:= Min(Succ(Lpos),twDataLast);
    while (Rpos<twDataLast) and (twPosCm[Rpos]<=X) and (twPosCm[Rpos+1]-X<c) do
      Inc(Rpos);
    end;
  end;
end; {~findcalcrange}


{19/02/2021 new variant}
function TWellhoferData.FindCalcRange(ADataLevel   :twcFloatType;
                                      NearestPos   :Integer;
                                      ASide        :twcSides;
                                      var Lpos,Rpos:Integer;
                                      ASource      :twcDataSource=dsMeasured): Boolean;
var X,c: twcFloatType;
    i  : Integer;
begin
with wSource[ASource] do
  begin
  Result:= twValid and InRange(NearestPos,twScanFirst,twScanLast);
  if Result then
    begin
    c   := CalcWidth_cm/2;
    X   := twPosCm[NearestPos];
    i   := ifthen(ASide=twcLeft,1,-1);
    Lpos:= NearestPos;
    Rpos:= NearestPos;
    while (Lpos>twScanFirst) and ((X-twPosCm[Pred(Lpos)]<c) or (i*(ADataLevel-twData[Lpos])<=0)) do
      Dec(Lpos);
    while (Rpos<twScanLast ) and ((twPosCm[Succ(Rpos)]-X<c) or (i*(ADataLevel-twData[Rpos])>0)) do
      Inc(Rpos);
    end;
  end;
end; {~findcalcrange}


//****BistroMath core function****
//this version performs well for inverted data with vertical slopes
{26/10/2018 force use of linear fit for less than three points}
{05/11/2020 did not accept two points, this is now handled by tquadfit anyway}
function TWellhoferData.CalcValue(Lpos,Rpos  :Integer;
                                  X          :twcFloatType;
                                  ASource    :twcDataSource=dsMeasured;
                                  InverseCalc:Boolean=False): twcFloatType;
var Q           : TQuadFit;
    i,CentralPos: Integer;
    Xarr,Yarr   : twcFloatArray;
begin
with wSource[ASource] do
  begin
  if InverseCalc then begin  Xarr:= twData;   Yarr:= twPosCm;  end
  else                begin  Yarr:= twData;   Xarr:= twPosCm;  end;
  try
    if not InRange(LPos,twDataFirst,twDataLast) then
      Result:= 0
    else
      begin
      Rpos      := EnsureRange(Rpos,LPos,twDataLast);
      CentralPos:= (Lpos+Rpos) div 2;
      Q:= TQuadFit.Create(Rpos-Lpos+2);
      try
        Result:= Yarr[CentralPos];
        if Xarr[Lpos]=Xarr[Rpos] then                                           //avoid vertical lines
          Result:= (Yarr[Lpos]+Yarr[Rpos])/2                                    //use average in this case
        else
          begin                                                                 //interpolation between points
          if Lpos=Rpos then
            begin
            if Lpos=twScanFirst then
              begin
              Lpos:= Succ(Lpos);
              Rpos:= Lpos;
              end
            else if Rpos=twScanLast  then
              begin
              Lpos:= Pred(Rpos);
              Rpos:= Lpos;
              end;
            if Xarr[Lpos]>=X then
              begin
              if Xarr[Pred(Lpos)]<X then Dec(Lpos)
              else                       Inc(Rpos);
              end
            else
              begin
              if Xarr[Pred(Lpos)]>=X then Dec(Lpos)
              else                        Inc(Rpos);
              end;
            end;
          for i:= Lpos to Rpos do
            Q.Add_XY(Xarr[i],Yarr[i]);
          if Q.FitValid then
            Result:= Q.FitQuad(X,wOutlierFilter);
          end;
       except
        Result:= Yarr[CentralPos];
       end;
      try
        FreeAndNil(Q);
       except
        ExceptMessage('WH.CalcValue!');
       end;
      end;
   except
    Result:= 0;
   end;
  end;
end; {~calcvalue}



//****BistroMath core function****
{15/08/2015}
function TWellhoferData.GetInterpolatedValue(Position    :twcFloatType;
                                             ASource     :twcDataSource=dsMeasured;
                                             DefaultValue:twcFloatType=0          ): twcFloatType;
var i,j,k: Integer;
    d    : twcFloatType;
begin
i:= NearestPosition(Position,ASource,False);
with wSource[ASource] do
  if i>=twDataFirst then
    begin
    d:= Position-twPosCm[i];
    if Abs(d)<wSamePositionRadiusCm then
      Result:= twData[i]
    else
      begin
      k:= ifthen(d>0,1,-1);
      j:= i+k;
      if not InRange(j,twdataFirst,twDatalast) then
        j:= i-k;
      if twPosCm[j]=twPosCm[i] then
        Result:= (twData[i]+twData[j])/2
      else
        Result:= twData[i]+(twData[j]-twData[i])*d/(twPosCm[j]-twPosCm[i]);
      end;
    end
  else
    Result:= DefaultValue;
end; {~getinterpolatedvalue}


//****BistroMath core function****
{$push}{$warn 5057 off: Local variable does not seem to be initialized}
function TWellhoferData.GetQfittedValue(Position    :twcFloatType;
                                        ASource     :twcDataSource=dsMeasured;
                                        DefaultValue:twcFloatType =0): twcFloatType;
var Lpos,Rpos: Integer;
begin
if FindCalcRange(Position,Lpos,Rpos,ASource) then Result:= CalcValue(Lpos,Rpos,Position,ASource)
else                                              Result:= DefaultValue;
end; {~getqfittedvalue}
{$pop}


//****BistroMath core function****
//apply quadratic fit at certain position
function TWellhoferData.GetScaledQfValue(Position        :twcFloatType;
                                         RelativeToCenter:Boolean;
                                         Scaling         :twcScalingType;
                                         ASource         :twcDataSource=dsMeasured): twcFloatType;
var v: twcFloatType;
begin
with wSource[ASource] do
  begin
  case Scaling of
    scPlotScaling: if twPlotScaling=0 then v:= 1
                   else                    v:= 100/twPlotScaling;
    scNormalised : v:= twAppliedNormVal;
    scAvgNorm    : v:= twAvgNormValue;
    scMaximum    : v:= twMaxValue;
   else            v:= 1;
   end;
  Result:= 100*GetQfittedValue(Position+ifthen(RelativeToCenter and twCenterPosValid,twCenterPosCm,0),ASource)/
           ifthen(v>0,v,1);
  end;
end; {~getscaledqfvalue}


(*
QfitMaxPos fills the twTopModel with th results of a quadratic fit.
As default it takes the area around the twMaxArr position. When the fitted Xtop is within 1 cm of the already
found twMaxPosCm and within the fit range then twMaxPosCM//twMaxValue to Xtop/Ytop
*)
{12/08/2015}
{19/12/2015: large range for FFF}
{20/12/2015
   twTopModel
   support for general hoizontal scans}
{04/01/2016 split wLinacSymSign}
{21/07/2020 GetAdjustedFilterWidthCm}
{27/08/2020 introduced ForceFitCenter; explicit dependency on wTopModelRadiusCm}
{30/08/2020 set twMaxPosCm/twMaxValue if old value within fitrange and less than 1 cm from model.xtop}
{01/09/2020 RangeCm=0 -> no fit}
procedure TWellhoferData.QfitMaxPos(ASource       :twcDataSource=dsMeasured;
                                    ForceFitCenter:Boolean      =False      );
var Q                  : TQuadFit;
    RangeCm,FitCenterCm: twcFloatType;
    i,j,k,FitCenterArr : Integer;
begin
with wSource[ASource] do
  if twFastScan and ((ScanType in twcHoriScans) or (twMaxPosCm>twcPDDminTopCm)) then
    begin
    FitCenterArr:= ifthen(ForceFitCenter,twCenterArr,twMaxArr);
    FitCenterCm := twPosCm[FitCenterArr];
    if ScanType in twcHoriScans then
      begin
      RangeCm:= Min(wTopModelRadiusCm[twSetFieldType],(twInfieldPosCm[twcRight]-twInfieldPosCm[twcLeft])/2);
      if RangeCm>0 then
        k:= Min(FitCenterArr-NearestPosition(FitCenterCm-RangeCm,ASource),NearestPosition(FitCenterCm+RangeCm,ASource)-FitCenterArr)  //k is (half) range of fit
      else
        k:= 0;
      end
    else if twStepSizeCm>0 then
      k:= Round(GetAdjustedFilterWidthCm(ASource)/twStepSizeCm) div 2
    else
      k:= 2;
    if k>0 then                                                                 //zero points is defined as 'no fit'
      begin
      k:= Max(2,k);
      Q:= TQuadFit.Create(Succ(2*k));
      i:= Max(twScanFirst,FitCenterArr-k);                                      //find enough points around maximum
      j:= Min(FitCenterArr+k ,twDataLast);
      while (j-i<k) and (i>twDataFirst) do
        Dec(i);
      for k:= i to j do
        Q.Add_XY(twPosCm[k],twData[k]);
      if InRange(Q.TopX,ifthen(ScanType in twcVertScans,Max(0.001,twFirstDataPosCm),twFirstDataPosCm),twLastDataPosCm) and
         (Q.TopY>twcMinNormVal) then                                            //calculate maximum
        begin
        twTopModel:= Q.Report;
        with twTopModel do if Valid and (Abs(twMaxPosCm-Xtop)<1) and InRange(twMaxPosCm,Xmin,Xmax) then
           begin
           twMaxPosCm:= Xtop;
           twMaxValue:= Ytop;
           end;
        end
      else
        twTopModel.Valid:= False;
      try
        FreeAndNil(Q);
       except
        ExceptMessage('WH.QfitMaxPos!');
       end;
      end
    else
      twTopModel:= Default(TQuadFitReport);                                     //effectively twTopModel.Valid=false
    end;
end; {~qfitmaxpos}


//****BistroMath core function****
{$push}{$warn 5057 off}
(* This function is original work of Theo van Soest.
For a given level this function searches the left and right position where that level can be found.
This is stored in the twLevelPos array, indexed with levels described in
    twcDoseLevel      =(dLow,dHigh,d20,d50,d80,d90,dUser,dDerivative,dInflection,dSigmoid50,dTemp);
In this structure are stored the doselevel, and for both left and right: nearest position, calculated position, valid status.
By definition dDerivative,dInflection and dSigmoid50 are not level dependent, and will not be filled through this function.
This function needs:
- a from low to high ordered twPosCm array -> PrepareProfile
- twAbsNormPos, which may vary during the process due to changing normalisation rules -> FastScan, Analyse.
When twAbsNormPos changes (possibly induced by changing fieldclass), of course dependent levels must be recalulated.
Therefore this function is used heavily.
To improve speed, old results are checked to see if a certain actual level is already evaluated.
*)
{16/01/2011 this version starts at maximum looking outward}
{29/04/2011 valid:= ... and (t<=n)}
{06/05/2011 use derivative if no penumbra is found to calculate centerpos}
{20/10/2012 now based on true #points within calcwidth}
{08/01/2014 search of penumbra always inward to avoid deap underdosage areas}
{09/07/2015 old results now always used when valid}
{14/07/2015 partial edge detection results are now accepted}
{29/07/2015 reuse derivative results in buffer when possible}
{31/07/2015 renaming variables}
{03/12/2015 fitpenumbra}
{08/04/2016 do QfitMaxPos when wMaxAsCenterPos
            if wMaxAsCenterPos do not take twcenterpos from derivative}
{22/07/2016 wCenterDefinition}
{15/11/2016 twCenterDefUse}
{11/01/2017 toepassing wEdgeFallBackCm, twUsedEdgeLevel}
{16/01/2017 step inward search preceded by step outward prephase to handle multiple peaks}
{17/01/2017 LevelPos[dDerivative],LevelPos[dInflection],LevelPos[dSigmoid50] are always calculated when wFFFdetection is true}
{18/01/2017 wCenterDefinition=Centerorigin handled}
{03/02/2017 twCenterPosCm with edge detection and wCenterDefinition=Centerorigin}
{13/04/2017 wCenterDefinition should be used with appriate fieldclass}
{05/12/2017 wEdgeForce}
{26/10/2018 step inward valid changed to larger OR equal: (Nearest>=twMaxArr) and (t>=n)}
{05/06/2020 UserLevelAsBorder}
{17/06/2020 dTemp as new last element}
{09/07/2020 twEdgeDefUse:= GetRelatedEdgeType(twUsedEdgeLevel)
            twEdgeDefUse depends no longer on (wCenterDefinition[FieldClass]=CenterPenumbra)}
{05/11/2020 do not initialise Lpos and Rpos anymore for findcalcrange}
{19/02/2021 alternative version findcalcrange}
function TWellhoferData.FindLevelPos(ASource          :twcDataSource=dsMeasured;
                                     ALevel           :twcDoseLevel =d50;
                                     Symmetric        :Boolean     =True): Boolean;
var WantedLevel,DifLevel,MinDifLevel,CurrentLevel: twcFloatType;
    i,tmpL,tmpR,Lpos,Rpos                        : Integer;                     //array indexes
    b,StepOutward                                : Boolean;
    p                                            : twcDoseLevel;

begin
with wSource[ASource] do
  begin
  WantedLevel:= twAbsNormValue*DosePoint2Value(ALevel);                         //the actual search level
  Result     := True;                                                           //need for analysis
  b          := True;
  for p:= dLow to dTemp do
    if b and (p<>ALevel) and (twLevelPos[p].Level=WantedLevel) then             //search if result is already available
      begin
      twLevelPos[ALevel]:= twLevelPos[p];
      b                 := not BordersValid(ASource,ALevel);                    //when no valid result is found, further search is needed
      end;
  if (twAbsNormValue>0) and InRange(WantedLevel,0,twAbsNormValue) then
    begin
    if b then                                                                   //still search needed
      begin
      tmpL:= twScanFirst;
      tmpR:= twScanLast;
      if Symmetric then
        begin
        CurrentLevel:= 0.9*twData[tmpR];
        if twData[tmpL]<twData[tmpR] then
          while twData[Succ(tmpL)]<CurrentLevel do
            Inc(tmpL)
        else
          begin
          CurrentLevel:= 0.9*twData[tmpL];
          while twData[Pred(tmpR)]<CurrentLevel do
            Dec(tmpR);
          end;
        end;
      StepOutward:= DosePoint2Value(ALevel)<0.65;                               //for high levels an outward search is preferred; otherwise: start low
      with twLevelPos[ALevel].Penumbra[twcLeft] do //lllllllllllllllllllll LEFT side llllllllllllllllllllllllllllllllllllll
        begin
        if StepOutward then
          begin  //-------step outward------
          i           := twMaxArr;
          Nearest     := i;
          CurrentLevel:= twMaxValue;
          MinDifLevel := twAbsNormValue/2;
          while (CurrentLevel>=WantedLevel) and (i>tmpL)  do
            begin
            Dec(i);
            CurrentLevel:= twData[i];
            DifLevel    := abs(CurrentLevel-WantedLevel);
            if (DifLevel<=MinDifLevel) or (CurrentLevel>=WantedLevel) then      //this forces nearest to be lower than searchlevel
              begin
              Nearest    := i;
              MinDifLevel:= DifLevel;
              end;
            end;
          Valid:= (Nearest>=tmpL) and (CurrentLevel<=WantedLevel);
          end
        else
          begin  //-------step inward------
          i          := twMaxArr;
          MinDifLevel:= twAbsNormValue/2;
          while (i>tmpL) and (twData[i]>MinDifLevel) do
            Dec(i);
          Nearest     := i;
          CurrentLevel:= twData[i];
          while (CurrentLevel<=WantedLevel) and (i<tmpR)  do
            begin
            Inc(i);
            CurrentLevel:= twData[i];
            DifLevel    := abs(CurrentLevel-WantedLevel);
            if (DifLevel<=MinDifLevel) or (CurrentLevel<=WantedLevel) then      //this forces nearest to be lower than searchlevel
              begin
              Nearest    := i;
              MinDifLevel:= DifLevel;
              end;
            end;
          Valid:= (Nearest<=twMaxArr) and (CurrentLevel>=WantedLevel);
          end;
        Result:= Valid;
        if Valid then
          begin
          FindCalcRange(WantedLevel,Nearest,twcLeft,Lpos,Rpos,ASource);
          Calc:= CalcValue(Lpos,Rpos,WantedLevel,ASource,True);
          end
        else
          Calc:= twPosCm[Nearest];
        end;  {left}
      with twLevelPos[ALevel].Penumbra[twcRight] do   //rrrrrrrrrrrrrrrrrrrrrrrrr RIGHT side rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
        begin
        if StepOutward then
          begin  //-------step outward------
          i           := twMaxArr;
          Nearest     := i;
          CurrentLevel:= twData[i];
          MinDifLevel := twAbsNormValue/2;
          while (CurrentLevel>=WantedLevel) and (i<tmpR) do
            begin
            Inc(i);
            CurrentLevel:= twData[i];
            DifLevel    := abs(CurrentLevel-WantedLevel);
            if (DifLevel<=MinDifLevel) or (CurrentLevel>=WantedLevel) then
              begin
              Nearest    := i;
              MinDifLevel:= DifLevel;
              end;
            end;
          Valid:= (Nearest<=tmpR) and (CurrentLevel<=WantedLevel);
          end
        else
          begin  //-------step inward------
          i      := twMaxArr;
          MinDifLevel      := twAbsNormValue/2;
          while (i<tmpR) and (twData[i]>MinDifLevel) do
            Inc(i);
          Nearest     := i;
          CurrentLevel:= twData[i];
          while (CurrentLevel<=WantedLevel) and (i>twMaxArr) do
            begin
            Dec(i);
            CurrentLevel:= twData[i];
            DifLevel    := abs(CurrentLevel-WantedLevel);
            if (DifLevel<=MinDifLevel) or (CurrentLevel<=WantedLevel) then
              begin
              Nearest    := i;
              MinDifLevel:= DifLevel;
              end;
            end;
          Valid:= (Nearest>=twMaxArr) and (CurrentLevel>=WantedLevel);
          end;
        Result:= Result and Valid;                                              //result is true when both borders are valid
        if Valid then
          begin
          FindCalcRange(WantedLevel,Nearest,twcRight,Lpos,Rpos,ASource);
          Calc:= CalcValue(Lpos,Rpos,WantedLevel,ASource,True);
          end
        else
          Calc:= twPosCm[Nearest];
        end; {right}
      twLevelPos[ALevel].Level:= ifthen(Result,WantedLevel,0);                  //if not valid the Level is set to zero.
      end; {b}
    end; {inrange}
  if LogLevel>2 then with twLevelPos[ALevel] do
    StatusMessage(Format('->FindLevelPos curve[%d] at level %0.1f: %0.3f cm (%d) / %0.3f cm (%d)',
                  [Ord(ASource),Level,
                   Penumbra[twcLeft ].Calc,Penumbra[twcLeft ].Nearest,
                   Penumbra[twcRight].Calc,Penumbra[twcRight].Nearest]));
  end; {with}
Result:= Result or AcceptMissingPenumbra;
end; {~findlevelpos}
{$pop}


(*   FindEdge   ****BistroMath core function****
makes adjustments to twSetFieldType
calls derviative and sigmoid
sets edge based values dependent on field class
*)
{11/07/2020 taken out of findlevelpos; completely revieuwed}
{18/07/2020 tSource always defined}
{19/07/2020 wSmallFielddetection,wSmallFieldLimitCm}
{20/07/2020 implementation of fcSmall
            synchronise twSetFieldType}
{21/07/2020 GetAdjustedFilterWidthCm}
{27/08/2020 twMaxPosCm, twMaxValue}
{16/10/2020 fallback detetction still used d50-Derivative instead of fcFallBack-fcPrimary}
function TWellhoferData.FindEdge(ASource:twcDataSource=dsMeasured): Boolean;
var FieldClass   : twcFieldClass;
    SigmoidNeeded: Boolean;
    tSource      : twcDataSource;
    p            : twcDoseLevel;

    function SetCenterOnPenumbra(ALevel:twcDoseLevel): Boolean;
    begin
    with wSource[ASource],twLevelPos[ALevel] do
      begin
      Result          := Penumbra[twcLeft].Valid and Penumbra[twcRight].Valid;
      twCenterPosValid:= Result;
      if Result then
         begin
         twCenterPosCm    := GetFieldCenterCm(ASource,ALevel);
         twCenterPosDefUse:= GetRelatedPositionType(ALevel);
         end
      end;
    end; {setcenteronpenumbra}

begin
Result:= ScanType in twcHoriScans;
if Result then
  begin           //(dLow,dHigh,d20,d50,d80,d90,dUser,dDerivative,dInflection,dSigmoid50,dTemp)
  if not wSource[ASource].twFastScan then
    FastScan(ASource);
  if wFieldTypeDetection[fcSmall] then
    begin
    p:= wEdgeMethod[fcPrimary,fcSmall];
    if not BordersValid(ASource,p) then
      FindLevelPos(ASource,p);
    if Inrange(GetFieldWidthCm(ASource,p),0.05,wSmallFieldLimitCm) then
      wSource[ASource].twSetFieldType:= fcSmall;
    end;
  FieldClass                       := wSource[ASource].twSetFieldType;
  SigmoidNeeded                    := (wSource[ASource].twUsedEdgeLevel  in [dInflection,dSigmoid50]) or
                                      (wEdgeMethod[fcPrimary,FieldClass] in [dInflection,dSigmoid50]);
  wSource[ASource].twCenterPosValid:= False;                                    //reset key elements
  wSource[ASource].twInFieldAreaOk := False;
  tSource                          := ASource;                                  //initially tSource is equal to ASource
  if wApplyUserLevel then wSource[ASource].twUsedEdgeLevel:= dUser
  else                    wSource[ASource].twUsedEdgeLevel:= wEdgeMethod[fcPrimary,FieldClass];
  if (wEdgeDetect or wFieldTypeDetection[fcFFF] or (wSource[ASource].twUsedEdgeLevel=dDerivative) or SigmoidNeeded) and
     (not wSource[ASource].twIsRelative) and (ASource<>dsBuffer) then
    begin
    if (ASource in twcFilteredCopies) then
      begin
      tSource                        := twcCoupledFiltered[ASource];            //tSource is unfiltered data if available
      wSource[tSource].twSetFieldType:= wSource[ASource].twSetFieldType;        //synchronise fieldtype for robustness
      end;
    Derive(GetAdjustedFilterWidthCm(ASource),ASource,dsBuffer);                 //derivative of filtered curve is fully acceptable
    Result                            := wSource[dsBuffer].twDerivativeValid;   //set new function result
    wSource[ASource].twDerivativeValid:= Result;                                //import twDerivative results from buffer
    if Result then
      wSource[ASource].twLevelPos[dDerivative]:= wSource[dsBuffer].twLevelPos[dDerivative];
    if not BordersValid(tSource,d50) then
      FindLevelPos(ASource,d50);                                                //the 50% level should be available as basic feature
    Result:= Result and wEdgeDetect and                                         //further analysis depends on availability of fallback method and necessarity
            (wEdgeMethod[fcFallBack,FieldClass]<>wEdgeMethod[fcPrimary,FieldClass]) and
            ( (GetLevelDistance(wEdgeMethod[fcFallBack,FieldClass],wEdgeMethod[fcPrimary,FieldClass],twcLeft ,tSource)>wEdgeFallBackCm) or
              (GetLevelDistance(wEdgeMethod[fcFallBack,FieldClass],wEdgeMethod[fcPrimary,FieldClass],twcRight,tSource)>wEdgeFallBackCm)   );
    if Result then
      wSource[ASource].twUsedEdgeLevel:= wEdgeMethod[fcFallBack,FieldClass];
    if SigmoidNeeded then
      begin
      if not SigmoidPenumbraFit(tSource) then                                   //try to fit sigmoid function; always on unfiltered data (tSource)
        wSource[ASource].twUsedEdgeLevel:= dDerivative                          //sigmoidfit failed; fall back to derivative
       else if (ASource=dsMeasured) and wApplySigmoidToBuffer then              //improved speed
         ApplySigmoidPenumbraFit(tSource,dsBuffer);
      end;
    end; {tSource}
  if tSource<>ASource then
    begin
    for p:= dInflection to dSigmoid50 do
       wSource[ASource].twLevelPos[p]:= wSource[tSource].twLevelPos[p];
    wSource[ASource].twUsedEdgeLevel:= wSource[tSource].twUsedEdgeLevel;        //decisions are also based on unfiltered data
    end;
  with wSource[ASource] do
    begin
    if wCenterDefinition[FieldClass]=CenterPenumbra then                        //check center several definitions
      SetCenterOnPenumbra(twUsedEdgeLevel)
    else if (wCenterDefinition[FieldClass]=CenterOrigin)             and
            (twLevelPos[twUsedEdgeLevel].Penumbra[twcLeft ].Calc<=0) and
             (twLevelPos[twUsedEdgeLevel].Penumbra[twcRight].Calc>=0) then
      begin
      twCenterPosCm    := 0;
      twCenterPosDefUse:= dUseOrigin;
      twCenterPosValid := True;
      end
    else if wCenterDefinition[FieldClass]=CenterMax then
      begin
      twCenterPosCm    := twPosCm[twmaxArr];
      QfitMaxPos(ASource);
      twCenterPosCm    := wSource[ASource].twMaxPosCm;
      twCenterPosDefUse:= dUseMax;
      twCenterPosValid := True;
      end;
    Result:= twCenterPosValid;
    if not Result then
      twCenterPosDefUse:= dUseUndefined;
    twCenterArr:= NearestPosition(twCenterPosCm,ASource);
    twWidthCm  := GetFieldWidthCm(ASource,twUsedEdgeLevel);
    if not twDerivativeValid then
      twLevelPos[dDerivative]:= twLevelPos[twUsedEdgeLevel];
   end; {with}
 end; {if}
end; {~findedge}


(* ****BistroMath core function****
FastScan locates the position of the profile, nu asumptions are made other than
twScanFirst and twScanlast which can be used for a local peak search.
Profiles not crossing the origin at all are accepted, but unsupported in most dosimetry protocols.
As a pdd can be viewed as a "profile shape" it can follow almost all rules set
for real profiles.
The major output results are mostly preliminary and include:
  twValid (major data fail when false), twFastScan,
  twMinArr (index), twMaxArr (index),  twMaxPosCm, twMaxValue,
  twCenterArr, twCenterPosCm, twCenterPosDefUse,
  twAbsNormPosCm, twCenterPosCm, twlevelPos[d50], twlevelPos[d90], twOriginPosValid,
  twAppliedNormVal, twRelNormValue;
  diagonal detection -> twDiagonalFound;
  wedge detection, based on twlevelPos[d50], twlevelPos[d90] -> twSetFieldType:= fcWedge
*)
{11/06/2014}
{29/05/2015 wWedgeDetection added to speed up when not needed
 14/07/2015 Check on valid results for twCenterPos and twAbsNormVal
            When not acceptable twMaxPosCm / twMaxValue is taken.}
{28/07/2015 set twFirstPos and twLastPos for robustness}
{29/07/2015 use Derive with prefilter option, resulting in simpler code}
{31/07/2015 repair of wdetectedge}
{23/08/2015 if twIsDiagonal set then no further detection attempts}
{01/09/2015 twOriginPosValid added}
{17/09/2015 twValid:= (twMaxValue>twMinval) or twIsRelative}
{17/12/2015 repair for local peak analysis: loop from twSCANfirst to twSCANlast}
{15/11/2016 twCenterDefUse (administration purposes only)}
{16/06/2017 guarantee twMaxArr always to be a positive position for depth doses}
{28/01/2018 twAbsNormDefUse}
{31/01/2018 qfittedvalue of maximum should have reasonable ratio with maximum}
{03/04/2018 for diagonal detection field size should larger than 2 cm}
{29/09/2018 Remaining unused code found which created out of array boundary problems.
            For relative curves no new normalisation point is set.}
{15/05/2020 set twFirstScanPosCm,twLastScanPosCm}
{23/05/2020 reset twSigmoidDone}
{20/07/2020 twcFieldClass}
{21/07/2020 fcWedge}
{24/07/2020 wWedge90ShiftFactor}
{28/07/2020 FindEdge added}
{03/08/2020 definition of wWedge90ShiftFactor slightly changed for better interpretation}
{18/08/2020 wMRlinacTUlist}
{20/08/2020 sign of wWedge90ShiftFactor changed}
{27/08/2020 twMaxPosCm, twMaxValue}
{17/09/2020 introduction of FFrozen}
{04/03/2021 twIsDiagonal is evaluated anyway, but may be dropped for certain fieldtypes}
procedure TWellhoferData.FastScan(ASource:twcDataSource=dsMeasured);
var i,j                : Integer;
    lMin,lTmp,vmin,vmax: twcFloatType;
    p                  : twcDoseLevel;

begin
Inc(FActiveCnt);
{$IFDEF COMPILED_DEBUG}
with wSource[ASource] do
  FailInfo:= ifthen(twValid,ifthen(twFastScan,'done','init'),'invalid')+#32+wSource[ASource].twDataHistoryStg;
{$ENDIF}
with wSource[ASource] do
  if twValid and (not (twFastScan or FFrozen)) then
    try
      twFastScan       := True;
      twAnalysed       := False;
      twSigmoidDone    := False;
      twDerivativeValid:= False;
      twFirstDataPosCm := twPosCm[twDataFirst];
      twLastDataPosCm  := twPosCm[twDataLast ];
      twScanLength     := twLastDataPosCm-twFirstDataPosCm;
      twValid          := Abs(twScanLength)>twcDefMinScanLength;
      {$IFDEF COMPILED_DEBUG}
      FailInfo         := ifthen(twValid,'p1',Format('ScanLength=%0.1f',[twScanLength]));
      {$ENDIF}
      if twValid then
        begin
        for p:= dLow to dDerivative do with twLevelPos[p] do                    //preserve dInflection
          begin
          Level                   := 0;
          Penumbra[twcLeft ].Valid:= False;
          Penumbra[twcRight].Valid:= False;
          end;
        lTmp          := Sign(twScanLength);
        i             := twDataFirst;
        if not FZeroStepsOk then
          while twValid and (i<twDataLast) do
            begin
            Inc(i);
            twValid := (twPosCm[i]-twPosCm[i-1])*lTmp>0;
            {$IFDEF COMPILED_DEBUG}
            FailInfo:= 'zerosteps';
            {$ENDIF}
            end;
        if twValid then
          try
            if twLocalPeak then   //-------localpeak only-----------------
              begin                                                             //twAbsNormPos must be available, find twScanFirst en twScanFirst
              i   := NearestPosition(twAbsNormPosCm,ASource);
              lTmp:= twcSearchNoiseFactor*twData[i];
              lMin:= lTmp;
              j   := i;
              while (j>twDataFirst) and (twData[j]<lTmp) do
                begin
                Dec(j);
                if twData[j]<lMin then
                  begin
                  twScanFirst:= j;
                  lMin       := twData[j];
                  lTmp       := twcSearchNoiseFactor*lMin;
                  end
                end; {while}
              lTmp:= twcSearchNoiseFactor*twData[i];
              lMin:= lTmp;
              j   := i;
              while (j<twDataLast) and (twData[j]<lTmp) do
                begin
                Inc(j);
                if twData[j]<lMin then
                  begin
                  twScanLast:= j;
                  lMin      := twData[j];
                  lTmp      := twcSearchNoiseFactor*lMin;
                  end
                end; {while}
              end;                //-------localpeak only-----------------
            if ScanType in twcHoriScans then
              begin
              j   := twScanFirst;
              vmin:= twData[twScanLast          ];
              vmax:= twData[(j+twScanLast) div 2];
              end
            else
              begin
              j   := Max(twScanFirst,NearestPosition(0,ASource));
              vmin:= twData[twScanLast];
              vmax:= twData[j         ];
              end;
            twMinArr:= j;
            for i:= twScanFirst to twScanLast do
              begin
              if (twData[i]>vmax) and (i>=j) then
                begin
                vmax    := twData[i];
                twMaxArr:= i;
                end
              else if twData[i]<vmin then
                begin
                vmin    := twData[i];
                twMinArr:= i;
                end;
              end;
            twValid := (vmax>0) and (vmax>vmin);
            {$IFDEF COMPILED_DEBUG}
            FailInfo:= Format('p2 min=%0.1f max=%0.1f',[vmin,vmax]);
            {$ENDIF}
            if twValid then
              begin
              {$IFDEF COMPILED_DEBUG}
              FailInfo         := 'p3';
              {$ENDIF}
              twFirstScanPosCm := twPosCm[twScanFirst];
              twLastScanPosCm  := twPosCm[twScanLast ];
              if twIsRelative and (twAbsNormDefUse<>dUseUndefined) then
                begin
                twAbsNormValue:= GetQFittedValue(twAbsNormPosCm,ASource,vmax);
                twMaxValue    := twAbsNormValue;
                twMaxPosCm    := twAbsNormPosCm;
                end
              else
                begin
                twMaxValue      := vmax;
                twMaxPosCm      := twPosCm[twMaxArr];
                twAbsNormPosCm  := twMaxPosCm;                                  //set a default for robustness
                twAbsNormValue  := vmax;
                twAbsNormDefUse := dUseMax;
                twAppliedNormVal:= vmax;
                twRelNormValue  := vmax;
                end;
              {$IFDEF COMPILED_DEBUG}
              StatusMessage(Format('%s: max(%0.1f)=%0.1f',[twcDataSourceNames[ASource],twMaxPosCm,vmax]),False,1);
              {$ENDIF}
              if twFittedData then
                PDDmaxNMFit(ASource);                                           //sets twMaxPosCm, twMaxValue
              {$IFDEF COMPILED_DEBUG}
              FailInfo:= Format('p4: %0.1f=%0.1f',[twAbsNormPosCm,twAbsNormValue]);
              {$ENDIF}
              try
                twValid         := (twAbsNormValue>0) and InRange(GetQFittedValue(twAbsNormPosCm,ASource)/twAbsNormValue,0.1,10);
                twOriginPosValid:= (ScanType in twcHoriScans)                   and
                                   InRange(0,twFirstScanPosCm,twLastScanPosCm)  and
                                   (GetQFittedValue(0,ASource)>twcOriginMinNormFraction*twMaxValue);
               except
                twValid         := False;
                {$IFDEF COMPILED_DEBUG}
                FailInfo        := 'p5';
                {$ENDIF}
               end;
              if twValid and (not (twIsDerivative or (ASource=dsBuffer))) and wFullAnalysis then {edge detection}
                begin
                {$IFDEF COMPILED_DEBUG}
                FailInfo:= 'p6';
                {$ENDIF}
                if (ScanType in twcHoriScans) then
                  begin
                  if twOriginPosValid then
                    begin
                    twAbsNormPosCm := 0;
                    twAbsNormDefUse:= dUseOrigin;
                    end
                  else
                    begin
                    twAbsNormPosCm := twMaxPosCm;
                    twAbsNormDefUse:= dUseMax;
                    end;
                  twAbsNormValue   := Max(twcMinNormVal,GetQfittedValue(twAbsNormPosCm,ASource));
                  if not twIsRelative then
                    begin
                    //wSource[dsMeasured].twBeamInfo.twBWedge:= 0;  {test}
                    FindLevelPos(ASource,d50);
                    FindLevelPos(ASource,d90);
                    FindEdge(ASource);         //-----FindEdge--------uses current field type, sets twUsedEdgeLevel---------------
                    if not (twIsDerivative or twIsRelative or twIsGamma) then
                      begin
                      if wFieldTypeDetection[fcWedge] and (twSetFieldType<>fcElectron) then
                        begin
                        lTmp:= (twLevelPos[d50].Penumbra[twcLeft].Calc+twLevelPos[d50].Penumbra[twcRight].Calc)/2;
                        if BordersValid(ASource,d50) and BordersValid(ASource,d90) and
                          ((WedgeAngle>0) or
                           (wWedge90ShiftFactor*twLevelPos[d90].Penumbra[twcRight].Calc<lTmp) or
                           (wWedge90ShiftFactor*twLevelPos[d90].Penumbra[twcLeft ].Calc>lTmp)    ) then
                          begin
                          twSetFieldType:= fcWedge;
                          FindEdge(ASource);                                    //repeat with changed field type
                          end;
                        end;
                      end;
                    lTmp:= FieldLength;                                         //diagonal detection, may be dropped in Analyse for changed fieldtype
                    if (not (twIsDiagonal or (ASource in twcFilteredCopies) or (twSetFieldType=fcSmall))) and
                       (lTmp<>UndefinedVal)                                                               and
                       ( (abs(twBeamInfo.twBCollimator-45) mod 90<25) or
                         (twWidthCm*twPosScaling/twSDD2SSDratio>lTmp*(1+twcDefAccNormDif)) ) then
                      twIsDiagonal:= True;
                    end; {not relative}
                  end; {twhoriscans}
                if (not Inrange(twCenterPosCm,twFirstScanPosCm,twLastScanPosCm)) or
                   (ScanType in twcVertScans)  then
                  begin
                  twCenterArr      := twMaxArr;
                  twCenterPosCm    := twMaxPosCm;
                  twCenterPosDefUse:= dUseMax;
                  end;
                if (wNormalisation[twSetFieldType] in [NormOnCenter,NormOnInFieldArea]) or
                   (not InRange(0,twFirstScanPosCm,twLastScanPosCm))                    or
                   (GetQfittedValue(0,ASource,0)*10<twMaxValue)  then
                  begin
                  twAbsNormPosCm := twCenterPosCm;
                  twAbsNormDefUse:= twCenterPosDefUse;
                  end
                else if (wNormalisation[twSetFieldType]=NormOnMax) then
                  begin
                  twAbsNormPosCm := twMaxPosCm;
                  twAbsNormDefUse:= dUseMax;
                  end
                else
                  begin
                  twAbsNormPosCm := 0;
                  twAbsNormDefUse:= dUseOrigin;
                  end;
                end; {if not(derivative ...}
              if  not (twIsRelative or (CheckAlternativeSource(twAlignedTo)=CheckAlternativeSource(twSelf))) then
                begin
                lTmp:= wSource[twAlignedTo].twAbsNormPosCm;
                if InRange(lTmp,twFirstDataPosCm,twLastDataPosCm) then
                  begin
                  twAbsNormPosCm := lTmp;
                  twAbsNormDefUse:= wSource[twAlignedTo].twAbsNormDefUse;
                  end;
                end;
              twAbsNormValue:= Max(twcMinNormVal,GetQfittedValue(twAbsNormPosCm,ASource));
              twRelNormPosCm:= twAbsNormPosCm;
              if twIsGamma then
                twAbsNormValue:= 100;
              if twAbsNormValue=0 then  {fallback safety}
                begin
                twAbsNormValue := twMaxValue;
                twAbsNormPosCm := twMaxPosCm;
                twAbsNormDefUse:= dUseMax;
                end;
              twAvgNormValue  := twAbsNormValue;
              twAppliedNormVal:= twAbsNormValue; {may differ afterwards}
              end; {vmax>0 and vmax>vmin}
            if not twValid then
              StatusMessage(Format(twForMinMax,[twAbsNormPosCm,twAbsNormValue]))
           except
            twValid:= False;
           end {twvalid, legal scan}
        else
          StatusMessage(Format(twForIllegalScan,[i]));
        end {twvalid, scanlength ok}
      else
        StatusMessage(Format(twForMinScanLen,[Abs(twScanLength),twcDefMinScanLength]));
     except
      twValid:= False;
     end; {try, no fastscan}
{$IFDEF COMPILED_DEBUG}
if not wSource[ASource].twValid then
  StatusMessage(Format('FastScan %s failed at %s',[twcDataSourceNames[ASource],FailInfo]),False,1);
{$ENDIF}
Dec(FActiveCnt);
end; {~fastscan}


(* ****BistroMath core function****
Complete and final analysis of data sets.
FastScan must be completed succesfully.
There are a lot of variations:
-ASource (wSource[ASource])
-twSetFieldType
-twIsRelative status
-horizontal/vertical scan: major difference in analysis
-for horizontal scans FindEdge is called
-rules for normalisation and field center
-AutoCentering may be applied to horizontal scans
-when ASource in [dsMeasured,dsReference], a quad-filtered copy of the source is generated
When the center of the field (CoF) is defined as the average of the field borders and the normalisation is chosen in the same point,
this is no fixed strategy if the field edge also depends on the normalisation point. To overcome this, the analysis cycle is looped twice.
*)
{22/10/2014}
{15/04/2015 For relative vertical scans twAbsNormPos and twRelNormPos are copied from measurement.}
{22/07/2015 Use quadratic fit to find twMaxPosCm and twMaxValue of vertical scan}
{12/08/2015 Quadratic fit of maxpos done by QfitMaxPos}
{01/09/2015 twOriginPosValid used}
{11/12/2015 twFFFslope calculation, twLeft/RightFlatArr->twInFieldArr[side]}
{12/12/2015 twFFFdetected}
{06/07/2016 wFFFdetection and wFFFcentering implementation}
{07/07/2016 twcFFFRadiusCm and twcFFFInFieldExtCm implementation}
{23/07/2016 wCenterDefinition, twFFFslopeCenter}
{03/02/2017 wFFFcentering<>CenterFFFDefault supersedes all other centering choices}
{12/02/2017 wFFFdetection accepts missing penumbra(s)}
{28/03/2017 changes in wFFFcentering definition, introduction wFFFPeakType}
{30/03/2017 normalisation depended on fieldclass}
{08/11/2017 twAbsNormPosCm:= FModNormList.GetModDepth(Stg,twIsRelative,ifthen(not twIsRelative,-1,wSource[dsMeasured].twMaxPosCm))}
{23/11/2017 added twFlatPosCm,twSymAreaRatio}
{24/11/2017 twSymAreaRatio calculated on symmetrical range around twCenterPosCm, twPosIntegral moved from FastScan and now base on Integrate function}
{25/11/2017 apply optional SigmoidPenumbraFit at start of analysis for horizontal scans with absolute data}
{04/12/2017 wInflectionSigmoid[AppliedFieldType]}
{27/12/2017 if ASource=Measured then QuadFilter(Measured,MeasFiltered)}
{12/01/2018 added twAbsNormConfig to note used info from modlist}
{14/01/2018 atBorderuserdoselevel invalids twFastscan if needed}
{23/01/2018 twFFFdetected:= wFFFdetection AND (twBeamInfo.twBModality='X') and twCenterPosValid and twInFieldAreaOk and (lSize>10) and (ScanType in [snGT,snAB,snAngle]);}
{24/01/2018 for vertical scans: extend FindLevelPos to all levels    for lDP:= dLow to dTemp do FindLevelPos(ASource,lDP,False)}
{26/01/2018 use dsRefFiltered}
{28/01/2018 twAbsNormDefUse}
{28/10/2019 Autocenter only on dsMeasured}
{29/10/2019 Autocenter=AC_on forces analyse to start}
{15/05/2020 use twFirstScanPosCm,twLastScanPosCm}
{01/07/2020 code for horizontal and vertical scans in separate local functions}
{11/07/2020 FindEdge now handles edge detection and related tasks}
{20/07/2020 twcFieldClass}
{21/07/2020 AppliedFieldType now only used for temporary storage, fcWedge}
{18/08/2020 fcFFF detection only when set field type is fcStandard}
{27/08/2020 twMaxPosCm, twMaxValue, wTopModelRadiusCm}
{03/09/2020 also keep track which bands the derived data scan has passed with LowPassed and HighPassed}
{17/09/2020 introduction of FFrozen}
{19/10/2020 evaluate dUser always}
{20/10/2020 linac error calculation now based on twSetFieldtype (not fixed on d50 anymore)}
{30/01/2021 measured file type as priority message}
{23/02/2021 reintroduced twFFFdetected because MRLinac can also be fff; FFF specific works now linked to twFFFdetected, there is referred to AppliedFieldType}
{03/03/2021 twIsDiagonal now depends on fieldtypes}
{07/03/2021 review of IFA definition,wNominalIFA,twcDefaultSSD_MRcm}
function TWellhoferData.Analyse(ASource          :twcDataSource=dsMeasured;
                                AutoCenterProfile:twcAutoCenter=AC_default): Boolean;
var s: twcDataSource;

    procedure ProfileNormalisation(AFieldClass:twcFieldClass);
    var lDP: twcDoseLevel;
    begin
    with wSource[ASource] do
      begin
      if twAbsNormDefUse<>dUseUndefined then
        begin
        if InRange(0,twFirstScanPosCm,twLastScanPosCm) and
           ((twOriginPosValid and (wNormalisation[AFieldClass]=NormOnOrigin)) or (AFieldClass=fcWedge)) then
          begin
          twAbsNormPosCm := 0;
          twAbsNormDefUse:= dUseOrigin;
          end
        else if (wNormalisation[AFieldClass] in [NormOnCenter,NormOnInFieldArea]) and
                (abs(twAbsNormPosCm-twCenterPosCm)>twcSamePositionRangeCm)                           then
          begin
          twAbsNormPosCm := twCenterPosCm;
          twAbsNormDefUse:= dUseBorder;
          end;
        twAbsNormValue  := GetQfittedValue(twAbsNormPosCm,ASource);
        twAvgNormValue  := twAbsNormValue;
        twAppliedNormVal:= twAbsNormValue; {may differ afterwards}
        end; {not undefined}
      for lDP:= dLow to dUser do                                                //here all final dose dependent bordervalues  are set
        FindLevelPos(ASource,lDP);
      end;
    end; {profilenormalisation}


    function HorizontalScans: Boolean;
    var i,j,k,lc,RightInArr,RightOutArr: Integer;
        s                              : twcDataSource;
        lMin,lMax,lTmp1,lTmp2,lSize,IFA: twcFloatType;
        lDP                            : twcDoseLevel;
        side                           : twcSides;
        LinFit                         : TLinFit;
        AutoCenter                     : Boolean;
        AppliedFieldType               : twcFieldClass;


        procedure Set_IFA;
        begin
        with wSource[ASource] do
          begin
          twInFieldPosCm[twcLeft] := twCenterPosCm-IFA/2;
          twInFieldArr[twcLeft]   := Clip(NearestPosition(twInFieldPosCm[twcLeft],ASource,False),twScanFirst,Pred(twCenterArr));
          twInFieldPosCm[twcRight]:= twCenterPosCm+IFA/2;
          twInFieldArr[twcRight]  := Clip(NearestPosition(twInFieldPosCm[twcRight],ASource,False),Succ(twCenterArr),twScanLast);
          twInFieldAreaOk         := twInFieldAreaOk and (twInFieldArr[twcRight]-twInFieldArr[twcLeft]>2);
          end;
        end;

    begin
    with wSource[ASource] do                                                    //fcWedge,fcElectron,fcMRlinac already detected in FastScan
      begin
      lc:= 0;                   {loop count for analysis cycle}
      repeat
        Inc(lc);
        AutoCenter:= (not twIsRelative) and (not FCentered) and
                     ((ASource=dsMeasured) or ((ASource=dsCalculated) and (wSource[dsMeasured].twShiftCM<>0))); {autocenter is used in loop control}
        if not twIsRelative then
          begin
          if AutoCenter then
            begin
            if AutoCenterProfile=AC_default then AutoCenter:= wCenterProfiles
            else                                 AutoCenter:= AutoCenterProfile=AC_on;
            end;
          if AutoCenter then
            begin
            lTmp1:= -twCenterPosCm;
            Shift(lTmp1,RelShift,dsMeasured);  {related profiles shifted automatically}
            Shift(lTmp1,RelShift,dsReference); {related profiles shifted automatically}
            for s:= dsCalculated to twcLastRelated do
              Shift(lTmp1,RelShift,s);
            twAbsNormPosCm := 0;
            twAbsNormDefUse:= dUseOrigin;
            if ASource=dsMeasured then
              FCentered:= True;
            end;
          ProfileNormalisation(twSetFieldType);                                 //sets twAbsNormPosCm, twAbsNormVal, twAvgNormVal, twAppliedNormVal
          FindEdge(ASource);               //-----FindEdge--------uses current field type, sets twUsedEdgeLevel---------------
          end; {not twrelativedata}
        AppliedFieldType:= twSetFieldType; //+++++++++++++++++++from here the final FieldType is known++++++++++++++++++
        twAvgNormValue  := GetQfittedValue(twAbsNormPosCm,ASource);
        if twIsDiagonal then
          begin
          twIsDiagonal:= wDiagonalDetection[AppliedFieldType];                  //actual detection of diagonal depends on fieldtype
          if twIsDiagonal then
            StatusMessage(twDiagonalFound)
          else
            begin
            Warning:= twDiagonalFound;
            StatusMessage(LastMessage);                                         //possibly conflicting situation
            end
          end; {twisdiagonal}
        if twCenterPosValid or AcceptMissingPenumbra then
          begin                                                                 //derive twInFieldArr[Left,Rigtht] as 80% of fieldwidth
          LinFit:= TLinFit.Create;                                              //this object is reused several times
          if not twIsRelative then
            begin  {parsedata: twPosScaling:= ifthen(twScaleProfile,twSDD2SSDratio,1)}
            lTmp1          := GetFieldWidthCm(ASource,twUsedEdgeLevel);         //true field size
            lTmp2          := lTmp1*twPosScaling*FDefaultSSDcm/(twSSD_cm*twSDD2SSDratio); //calculate "real" field size at SSD=100
            lSize          := GetFieldLength;                                   //nominal size from header
            twInFieldAreaOk:= BordersValid(ASource,twUsedEdgeLevel) and twCenterPosValid;
            if (not wNominalIFA) or (Abs(lSize-lTmp2)>1) then                   //use only nominal when the difference with true field size < 1 cm
              lSize:= lTmp2;
            if (lSize)<10 then                                                  //------------------in-field area for field size < 10 cm----------------------
              IFA:= (lSize-Min(lSize,2))/twPosScaling
            else                                                                //------------------in-field area for field size >= 10 cm----------------------
              IFA:=ifthen(twIsDiagonal,twcNCSInFieldDiagonal,twcNCSInFieldAxis)*lTmp1;
            Set_IFA;
            try                                    //------------------FFF detection when fcStandard or fcMRlinac-----------------
              if (((AppliedFieldType=fcStandard) and wFieldTypeDetection[fcFFF]) or (AppliedFieldType=fcMRlinac)) and
                 (twBeamInfo.twBModality='X')                                                                     and
                 twCenterPosValid                                                                                 and
                 twInFieldAreaOk                                                                                  and
                 (lSize>10)                                                                                       and
                 (ScanType in [snGT,snAB,snAngle])                                                                then
                begin
                lTmp1:= GetLevelDistance(d50,dDerivative,twcLeft ,ASource);
                lTmp2:= GetLevelDistance(d50,dDerivative,twcRight,ASource);
                if (100-(50*(twData[twInFieldArr[twcLeft]]+twData[twInFieldArr[twcRight]])/twData[twCenterArr])>wFFFMinDoseDifPerc) and
                   ((lTmp1=0) or (lTmp1>wFFFMinEdgeDifCm) or (lTmp2=0) or (lTmp2>wFFFMinEdgeDifCm)) then
                    begin
                    twFFFdetected := True;
                    if AppliedFieldType=fcStandard then                         //when AppliedFieldType=fcMRlinac then keep it that way
                      twSetFieldType:= fcFFF;
                    end;
                end
              else
                twSetFieldType:= AppliedFieldType;
             except
              twSetFieldType:= AppliedFieldType;
             end;
            if not (ASource in twcFilteredCopies) then                          //now fieldtype is set
              QfitMaxPos(ASource);                                              //for filtered versions: rely on unfiltered result
            if twFFFdetected then           //------------------start FFF specific works-----------------------------------
              begin
              for side:= twcLeft to twcRight do {if (borders are symmetrical around origin) && (average twInFieldArr < 90%):  evaluate FFF slopes}
                with twFFFslope[side] do
                  begin
                  LinFit.Initialize;
                  k:= ifthen(side=twcLeft,-1,1);
                  i:= NearestPosition(twMaxPosCm+wTopModelRadiusCm[twSetFieldType]*k,ASource);
                  j:= NearestPosition(twMaxPosCm+Max(abs(twPosCm[twInFieldArr[side]]-twMaxPosCm)+twcFFFInFieldExtCm,wTopModelRadiusCm[twSetFieldType]+1)*k,ASource);
                  while Abs(i-j)<2 do
                    Inc(j,k);
                  twFFFfirst:= Min(i,j);
                  twFFFlast := Max(i,j);
                  twFFFvalid:= twFFFlast-twFFFfirst>2;
                  if twFFFvalid then
                    begin
                    for i:= twFFFfirst to twFFFlast do
                      LinFit.Add_XY(twPosCm[i],twData[i]);
                    twFFFoffset:= LinFit.Offset;
                    twFFFgain  := LinFit.Linear;
                    end;
                  end;
              lMax:= twCenterPosCm;
              try
                twFFFslopesTop:= (twFFFslope[twcLeft ].twFFFoffset-twFFFslope[twcRight].twFFFoffset)/
                                 (twFFFslope[twcRight].twFFFgain  -twFFFslope[twcLeft ].twFFFgain  );
               except
                twFFFslopesTop:= 0;
               end;
              case wCenterDefinition[AppliedFieldType] of
                CenterPenumbra:
                  begin
                  if wEdgeDetect and SigmoidFitAvailable(ASource) then begin lDP:= dInflection;  twCenterPosDefUse:= dUseInflection; end
                  else                                                 begin lDP:= dDerivative;  twCenterPosDefUse:= dUseDerivative; end;
                  twCenterPosCm:= GetFieldCenterCm(ASource,lDP);
                  end;
                CenterOrigin:
                  begin
                  twCenterPosCm    := 0;
                  twCenterPosDefUse:= dUseOrigin;
                  end;
                CenterMax:
                  try
                    case wFFFPeakDef of
                      CenterFFFTopModel: begin
                                         twCenterPosCm    := ifthen(twTopModel.Valid,twTopModel.Xtop,twMaxPosCm);
                                         twCenterPosDefUse:= dUseFFFtop;
                                         end;
                      CenterFFFSlopes  : begin
                                         twCenterPosCm    := twFFFslopesTop;
                                         twCenterPosDefUse:= dUseFFFslopes;
                                         end;
                     end; {case}
                   except
                    begin
                    twcenterPosCm    := lMax;
                    twCenterPosDefUse:= dUseMax;
                    end;
                   end; {except}
                 end; {case}
              Set_IFA;                                                          //center definition might be changed
              ProfileNormalisation(fcFFF);
              end; {twFFFdetected}              //------------------------------ end FFF specific works----------------------------
            if twInFieldAreaOk and (LogLevel>2) then
              StatusMessage(Format('->In-Field area curve[%d]: %0.1f cm',[Ord(ASource),abs(twPosCm[twInFieldArr[twcRight]]-twPosCm[twInFieldArr[twcLeft]])]));
            end; {not isrelative}
          lMin:= twMaxValue;
          lMax:= 0;
          try   {search min, max and calculate straight lijn over in-field area}
            if twSetFieldType=fcStandard then
              LinFit.Initialize;
            for i:= twInFieldArr[twcLeft] to twInFieldArr[twcRight] do
              begin
              if twSetFieldType=fcStandard then
                LinFit.Add_XY(twPosCm[i],twData[i]);
              lMin:= Min(lMin,twData[i]);
              lMax:= Max(lMax,twData[i]);
              end;
           except
            lMin:= 0;
            lMax:= 0;
           end; {try}
          if twSetFieldType=fcFFF then
            twLinSlope:= twFFFslope[twcLeft].twFFFgain+twFFFslope[twcRight].twFFFgain
          else if (twBeamInfo.twBWedge=0) and (lMax>0) then
            twLinSlope:= LinFit.Linear;
          if twAbsNormValue>0 then
            begin
            twRelAvgInField := LinFit.AverageY/twAbsNormValue;
            twAppliedNormVal:= ifthen(wNormalisation[AppliedFieldType]=NormOnInFieldArea,LinFit.AverageY,twAbsNormValue);
            twFlatness      := (lMax-lMin)/twAppliedNormVal;                      {compatibel met omnipro}
            twRelMaxInField := lMax/twAppliedNormVal;
            twRelMinInField := lMin/twAppliedNormVal;
            if twFlatness>1 then
              twFlatness:= 0;
            end;
          try
            FreeAndNil(LinFit);
           except
            ExceptMessage('WH.Analyze:LinFit!');
           end;
          twSymmetry:= 1;
          try {--------------- calculation of symmetry ----needs twcenterposcm => borders + fff top---}
            for i:= twCenterArr to Min(2*twCenterArr-twInFieldArr[twcLeft],twInFieldArr[twcRight]) do //symmetry: calculate max of ratio D[c-x]/D[c+x]
              begin
              lMin:= 2*twCenterPosCm-twPosCm[i];                                                      //lmin=position of opposite point
              try
                lMin:= GetQfittedValue(lMin,ASource);                                                 //lmin=value at position of opposite point
                if lMin=0 then lTmp1:= 0
                else           lTmp1:= GetQfittedValue(twPosCm[i],ASource,twData[i])/lMin;            //ltmp1=ratio
                if InRange(lTmp1,0.1,1) then
                  lTmp1:= 1/lTmp1;                                                                    //if smaller than 1 then invert
               except
                ltmp1:= 0;
               end; {try}
              twSymmetry:= Max(twSymmetry,lTmp1);                                                     //symmetry is defined as maximum over all values
              end; {for center to right}
           except
            twSymmetry:= 0;
           end; {try}
          lMin := (twInFieldPosCm[twcRight]-twInFieldPosCm[twcLeft])/2;
          lTmp1:= Integrate(twCenterPosCm-lMin,twCenterPosCm,ASource,True);                           //symmetrical range around twCenterPos
          lTmp2:= Integrate(twCenterPosCm,twCenterPosCm+lMin,ASource,True);
          try
            twSymAreaRatio:= 2*(lTmp2-lTmp1)/(lTmp1+lTmp2);
           except
            twSymAreaRatio:= 0;
           end;
         {$IFDEF POSINTEGRAL}
          lMax         := (GetPenumbraValue(ASource,lDP,twcRight)-GetPenumbraValue(ASource,lDP,twcLeft))/2;
          twPosIntegral:= Integrate(twCenterPosCm-lMax,twCenterPosCm+lMax,ASource,True)/twAbsNormVal;
         {$ENDIF}
          if Sign(twLevelPos[twUsedEdgeLevel].Penumbra[twcLeft].Calc)+Sign(twLevelPos[twUsedEdgeLevel].Penumbra[twcRight].Calc)=0 then
            begin {------ calculation of twSymLinacError: left and right border should at least have opposite sign ----------}
            lSize      := (twSSD_cm+twVector_ICD_cm[Start].m[Beam])*Sign(twLevelPos[twUsedEdgeLevel].Penumbra[twcRight].Calc)/100;
            j          := twLevelPos[twUsedEdgeLevel].Penumbra[twcLeft].Nearest;
            RightOutArr:= Min(NearestPosition(wLinacSymOuterRadiusCm*lSize,ASource),twLevelPos[d50].Penumbra[twcRight].Nearest);
            RightInArr := NearestPosition(wLinacSymInnerRadiusCm*lSize,ASource);
            lTmp1      := 0;
            ltmp2      := 0;
            if RightOutArr>RightInArr then
              try
                for i:= RightInArr to RightOutArr do
                  begin
                  lMin:= 2*twCenterPosCm-twPosCm[i];
                  if NearestPosition(lMin,ASource)>j then
                    begin
                    lTmp1:= lTmp1+GetQfittedValue(lMin,ASource);
                    lTmp2:= lTmp2+GetQfittedValue(twPosCm[i],ASource,twData[i]);
                    end;
                  end; {for}
                if (lTmp1+lTmp2)>0 then
                  twSymLinacError:= 2*(lTmp1-lTmp2)*ifthen(ScanType=snGT,wLinacSymSign[fInplane],ifthen(ScanType=snAB,wLinacSymSign[fCrossplane],1))/(lTmp1+lTmp2)
                else
                  twSymLinacError:= 999;
               except
                twSymLinacError:= 999;
               end; {try}
            end
          else {asymmetrical profile}
            twSymLinacError:= 999;
          end; {bordersfound or acceptmissingpenumbra}
        twRelNormValue:= twAbsNormValue;
        twRelNormPosCm:= twAbsNormPosCm;
      until (not AutoCenter) or (abs(twCenterPosCm)<0.001) or (lc>3);
      end; {with}
    Result:= True;
    end; {horizontalscans}


    function VerticalScans: Boolean;
    var lDP  : twcDoseLevel;
        lTmp1: twcFloatType;
        Stg  : String;
    begin
    with wSource[ASource] do
      begin
      QfitMaxPos(ASource);
      twAbsNormValue:= twMaxValue;
      for lDP:= dLow to dTemp do
        FindLevelPos(ASource,lDP,False);
      with twBeamInfo do
        begin
        Stg             := FModNormList.ModalityFormat(twBModality,twBEnergy);
        twRelNormPosCm  := FModNormList.GetModDepth(Stg,False,ifthen(twIsRelative,wSource[dsMeasured].twMaxPosCm,0));
        twRelNormValue  := FModNormList.GetModValue(Stg,False);
        twAbsNormPosCm  := FModNormList.GetModDepth(Stg,not twIsRelative,ifthen(twIsRelative,-1,wSource[dsMeasured].twMaxPosCm));
        lTmp1           := GetQfittedValue(twAbsNormPosCm,ASource,1);
        twAbsNormConfig := (lTmp1>0) and (not twIsRelative) and
                           (twMaxValue/lTmp1-1<twcYtopQfitRelDif);
        twAppliedNormVal:= twAbsNormValue;
        if Abs(twAbsNormPosCm-twMaxPosCm)<0.1 then
          twAbsNormDefUse:= dUseMax
        else
          twAbsNormDefUse:= dUseConfigured;
        if twMaxPosCm<twcPDDminTopCm then
          begin
          twAbsNormPosCm:= 0;
          twAbsNormDefUse:= dUseOrigin;
          end;
        if twAbsNormPosCm>=0 then
          begin
          lTmp1:= FModNormList.GetModValue(Stg,not twIsRelative);
          try
            if lTmp1<=0 then twRefNormFactor:= GetQfittedValue(twAbsNormPosCm,ASource)/twMaxValue;
           finally
            if lTmp1<=0 then twRefNormFactor:= 1
            else             twRefNormFactor:= 100/lTmp1;
           end;
          end
        else twRefNormFactor:= 1;
        end;
      if twIsRelative and (ASource<>dsMeasured) then
        begin
        twAbsNormPosCm:= EnsureRange(wSource[dsMeasured].twAbsNormPosCm,twFirstDataPosCm,twLastDataPosCm);
        twRelNormPosCm:= EnsureRange(wSource[dsMeasured].twRelNormPosCm,twFirstDataPosCm,twLastDataPosCm);
        end
      else
        if (twAbsNormPosCm>=0) and InRange(twAbsNormPosCm,twFirstDataPosCm,twLastDataPosCm) then
          begin
          twAbsNormValue:= Max(twData[twMaxArr]/10,GetQfittedValue(twAbsNormPosCm,ASource))*twRefNormFactor;
          if (Abs(twMaxValue/twAbsNormValue-1)>=twcYtopQfitRelDif) and
             (Abs(twMaxPosCm-twAbsNormPosCm)<twcSamePositionRangeCm) then
            twMaxValue:= twAbsNormValue; {keep relation between topmodel and normval when nomalised on top}
          if twAbsNormValue=0 then
            twAbsNormValue:= 100*twRefNormFactor;
          end
        else
          begin
          twAbsNormPosCm := twMaxPosCm;
          twAbsNormValue := twMaxValue*twRefNormFactor;
          twAbsNormDefUse:= dUseMax;
          end;
      if not ((twRelNormPosCm>0) and InRange(twRelNormPosCm,twFirstDataPosCm,twLastDataPosCm)) then
        begin
        twRelNormPosCm:= twPosCm[twMaxArr];
        twRelNormValue:= 100;
        end;
      if twBeamInfo.twBModality='X' then
        begin
        twPDD10:= GetQfittedValue(10,ASource)/twAbsNormValue;
        twPDD20:= GetQfittedValue(20,ASource)/twAbsNormValue;
        end;
      if twFilterPoints>1 then twAvgNormValue:= twAbsNormValue     {certify that twAvgNormval does some averaging}
      else                     twAvgNormValue:= GetQfittedValue(twAbsNormPosCm,ASource);
      twRelAvgInField  := twAvgNormValue;
      twCenterPosCm    := 0;
      twCenterPosDefUse:= dUseUndefined;
      end;  {with}
    Result:= True;
    end; {verticalscans}

begin
with wSource[ASource] do
  begin
  Result    := twValid;
  if (not FFrozen) and Result and ((not (twFastScan and twAnalysed)) or (AutoCenterProfile=AC_on)) then
    begin
    Inc(FActiveCnt);
    twFastScan:= False;
    FastScan(ASource);                                                          //might change twValid
    Result:= twValid;
    if Result then
      begin
      twFlatness     := 0;
      twSymmetry     := 0;
      twRelMaxInField:= 0;
      twRelMinInField:= 0;
      twRelAvgInField:= 0;
      if ScanType in twcHoriScans then
         Result:= HorizontalScans             //================HORIZONTAL scans=================
      else if (ScanType in twcVertScans) or ((ScanType=snFreescan) and wCurveInfo.twDesVaryingAxis[Beam]) then
         Result:= VerticalScans               //||||||||||||||||VERTICAL scans|||||||||||||||||||
      else
         Result:= False;
      end;
     if Result then
       begin
       if twIsGamma then
         begin
         twAbsNormValue:= 100;
         twAvgNormValue:= 100;
         end;
       if wRenormaliseData and (Abs(twAbsNormValue-100)>0.01) then
         Multiply(100/twAbsNormValue,ASource,ASource);
       if ASource in twcFilterSources then {assure valid coupling}
         begin
         s:= twcCoupledSources[ASource];
         if (not (wSource[s].twValid)) or (wSource[s].twFilterPoints=0) then
           QuadFilter(-1,ASource,s);
         Analyse(s,AutoCenterProfile);
         end;
       end; {result}
    twAnalysed:= Result;
    if (ASource=dsMeasured) or (LogLevel>1) then
      StatusMessage(Format('%s Field Type: %s',[ObjectCallSign,twcFieldClassNames[twSetFieldType]]),False,ifthen(ASource=dsMeasured,-1,LogLevel));
    Dec(FActiveCnt);
    end; {if}
  end; {with}
end; {~analyse}


procedure TWellhoferData.AddDefaultAliasList(AliasConfigList:TStrings);

  procedure AddDirectionText(AText,Vtext:String);
  var found: Boolean;
      i    : Integer;
  begin
  with AliasConfigList do
    begin
    found:= False;
    i:= Count;
    while (not found) and (i>0) do
      begin
      Dec(i);
      found:= LowerCase(AText)=Lowercase(Names[i]);
      end;
    if not found then
      Add(AText+NameValueSeparator+VText);
    end;
  end;

begin
with AliasConfigList do
  begin
  AddDirectionText('Inline'   ,'Inplane'   );
  AddDirectionText('Crossline','Crossplane');
  AddDirectionText('PDD'      ,'Depth'     );
  end;
end; {adddefaultaliaslist}


{01/09/2020 unset twApplied; trim}
procedure TWellhoferData.LoadAliasList(AliasConfigList:TStrings);
var i: integer;
    b: Boolean;
begin
b:= not Assigned(AliasConfigList);
if b then
  AliasConfigList:= TStringList.Create;
AliasConfigList.NameValueSeparator:= '=';
AddDefaultAliasList(AliasConfigList);
i:= AliasConfigList.Count;
SetLength(FAliasList,i);
while i>0 do
  begin
  Dec(i);
  with FAliasList[i] do
    begin
    twKey    := Trim(LowerCase(AliasConfigList.Names[i]));
    twValue  := Trim(AliasConfigList.Values[twKey]);
    twApplied:= False;
    end;
  end;
if b then
  try
    FreeAndNil(AliasConfigList);
   except
    ExceptMessage('WH.!LoadAliasList');
   end;
if assigned(FMultiScanList) then
  IndexMultiScan(FMultiScanList[0]);
end; {~loadaliaslist}


{01/09/2020 unset twApplied for all keys}
procedure TWellhoferData.ResetAliasList;
var i: integer;
begin
if Length(FAliasList)>0 then
  for i:= 0 to Pred(Length(FAliasList)) do
    FAliasList[i].twApplied:= False;
end; {~resetaliaslist}


{01/09/2020 informs wether a certain key is used}
function TWellhoferData.AliasListKeyApplied(AKey:string): Boolean;
var i: integer;
begin
AKey  := LowerCase(Trim(AKey));
Result:= False;
i     := Length(FAliasList);
while (not Result) and (i>0) do
  begin
  Dec(i);
  with FAliasList[i] do
     if twApplied and (AKey=twKey) then                                         //fastest implementation
       Result:= True;
  end;
end; {~aliaslistkeyapplied}


{01/09/2020 set twAplied when a key is used}
function TWellhoferData.ApplyAliasList(AKey:string): string;
var i: integer;
begin
AKey  := LowerCase(Trim(AKey));
i     := Length(FAliasList);
Result:= AKey;
while i>0 do
  begin
  Dec(i);
  with FAliasList[i] do
    if AKey=twKey then
      begin
      Result   := twValue;
      twApplied:= True;
      end;
  end;
end; {~applyaliaslist}


{09/11/2016}
function TWellhoferData.ApplyModBeamList(ADefault :string='';
                                         AModality:String=''): string;
begin
if wReferenceFromGeneric then
  begin
  if Length(AModality)=0 then
    AModality:= FModBeamList.ModalityFormat(wSource[dsMeasured].twBeamInfo.twBModality,Energy);
  Result:= FModBeamList.GetModValue(AModality);
  if Length(Result)=0 then
    Result:= ADefault;
  end
else
  Result:= ADefault;
end; {~applymodbeamlist}


function TWellhoferData.StopProcessing: Boolean;
begin
Result:= assigned(FPDDfit_simplex);
if Result then
  FPDDfit_simplex.StopAmoebe;
end; {~stopprocessing}


{$IFDEF WELLHOFER_DUMPDATA}
{19/05/2020}
procedure TWellhoferData.DumpData(const Info  :String       ='';
                                  ASource     :twcDataSource=dsMeasured;
                                  OriginSource:twcSourceEnum=dsDefault);
var Stg: String;
    i  : Integer;
begin
if (ASource in DumpDataFilter) or (OriginSource in DumpDataFilter) then
  begin
  StatusMessage('<'+Info+'>'+twcDataSourceNames[ASource]+
                ifthen((OriginSource<>dsDefault) and (OriginSource<>ASource),'('+twcDataSourceNames[OriginSource]+')',''),
                False);
  with wSource[ASource] do
    begin
    Stg:= '';
    if assigned(twPosCm) then
      begin
      for i:= twDataFirst to twDataLast do
        Stg:= Format('%s, %0.9f',[Stg,twPosCm[i]]);
      end
    else
      Stg:= 'twPosCm empty';
    StatusMessage(Stg,False);
    Stg:= '';
    if assigned(twPosCm) then
      begin
      for i:= twDataFirst to twDataLast do
        Stg:= Format('%s, %0.9f',[Stg,twData[i]]);
      end
    else
      Stg:= 'twData empty';
    StatusMessage(Stg,False);
    end;
  end;
end; {~dumpdata}
{$ENDIF}


{15/12/2015 FMRefOrgSrc}
{18/07/2016 wMultiScanReferences}
{20/07/2016 BinStream}
{11/05/2020 use FModNormLocal,FModFilmLocal,FModBeamLocal}
destructor TWellhoferData.Destroy;
var k: twcDataSource;
begin
try
  if FModNormLocal then
    FreeAndNil(FModNormList  );
  if FModFilmLocal then
    FreeAndNil(FModFilmList  );
  if FModBeamLocal then
    FreeAndNil(FModBeamList  );
  FreeAndNil(w2D_ArrayRefList);
  FreeAndNil(FRefOrgSrc      );
  if assigned(Fmcc) then
    FreeAndNil(Fmcc);
 except
   ExceptMessage('WH.Destroy!');
 end;
Finalize(FMultiScanList);
Finalize(FAliasList);
for k:= twcFirstDataSource to twcLastDataSource do
  ClearCurve(k,False);
inherited;
end; {~destroy}


{$IFDEF THREADED}

//----------TMathThread--------------------------------------------------------

{$IFDEF THREAD_GAMMA}
constructor TMathThread.Create(AWellhofer        :TWellhoferData;
                               ASource,AReference:twcDataSource;
                               ADestinationPtr   :twFloatArrayPtr;
                               AStatsSampler     :TStatssampler);                {gammawork}
begin
inherited Create(True);
CommonInit(AWellhofer);
FStatsSampler         := AStatsSampler;
FSource1              := ASource;
FSource2              := AReference;
FDataPtr              := ADestinationPtr;
FShiftCostFunctionStep:= 0;
FFit                  := TQuadFit.Create;
FCalculation          := twCalcGamma;
end; {~create}
{$ENDIF}


constructor TMathThread.Create(AWellhofer:TWellhoferData); {quadfitwork}
begin
inherited Create(True);
CommonInit(AWellhofer);
FCalculation:= twCalcQuadFit;
end; {~create}


procedure TMathThread.CommonInit(AWellhofer:TWellhoferData);
begin
FWellhofer     := AWellhofer;
FreeOnTerminate:= False;
FStatsSampler  := nil;
FFit           := nil;
Priority       := tpHigher;
FActive        := False;
FOk            := True;
{$IFDEF THREAD_USESTATS}
FStarts        := 0;
{$ENDIF}
end; {~commoninit}


procedure TMathThread.Execute;
{$IFDEF THREAD_GAMMA}
var Gamma,StartLimit,Limit,Distance: twcFloatType;
    Lpos,Rpos                      : Integer;
{$ENDIF}

  {$IFDEF THREAD_GAMMA}
  function CalcGamma(OffsetCm:twcFloatType): twcFloatType;
  var x,y,s,
      nDistSqr,nDoseSqr: twcFloatType;
      j,k              : Integer;
  begin  {calculate normalised vector for x=pos[p]+offsetcm}
  with FWellhofer do
    begin
    x   := wSource[FSource1].twPosCm[FPoint]+OffsetCm;
    j   := NearestPosition(x,FSource2);
    Lpos:= j-Ceil(CalcWidth/Min(1,wSource[FSource1].twStepSize));
    with FFit,wSource[FSource2] do
      begin
      if FindCalcRange(x,Lpos,Rpos,FSource2) then
        try
          Initialize;
          y        := twData[j];
          if Lpos=Rpos then
            begin
            s:= x-twPosCm[j]; {s is distance between x and nearest point in reference}
            if s<>0 then
              begin {interpolatie tussen twee punten}
              for k:= Max(twDataFirst,Pred(j)) to Min(Succ(j),twDataLast) do
                if (k=j) or ((twPosCm[k]-twPosCm[j])/s>1) then
                  Add_XY(twPosCm[k],twData[k]);
              y:= FitLin(x);
              end;
            end {twCalcPntsDist=0}
          else
            begin  {twCalcPntsDist>0}
            for k:= Lpos to Rpos do Add_XY(twPosCm[k],twData[k]);
            y:= FitQuad(x,wOutlierFilter);
            end;
         except
           y:= -1;
         end {if,try}
      else
        y:= -1;
      FResult:= y;
      if FitValid and (y>0) then {div0 safety check}
        begin
        if twcGammaDistCmBase  <=0    then nDistSqr:= 0
        else                               nDistSqr:= Sqr(OffsetCm/twcGammaDistCmBase);
        if twcGammaDosePercBase<=0    then nDoseSqr:= 0
        else if twcGammaLocalDosePerc then nDoseSqr:=  Sqr(100*(wSource[FSource1].twData[FPoint]/y-1)/twcGammaDosePercBase)   {div0 safety guaranteed}
             else                          nDoseSqr:=  Sqr((    wSource[FSource1].twData[FPoint]-y  )/twcGammaDosePercBase);
        FResult:= SqRt(nDistSqr+nDoseSqr);
        end
      else
        FResult:= -1;
      end;
    end;
  Result:= FResult;
  end; {calcgamma}
  {$ENDIF}

begin
case FCalculation of
  {$IFDEF THREAD_GAMMA}
  twCalcGamma:
    begin
    Gamma     := CalcGamma(0);       {calculate gamma at distance=0}
    Distance  := -twcGammaDistCmStep;
    StartLimit:= Max(1,Gamma)*twcGammaSearchMultiplier;
    Limit     := StartLimit;
    while InRange(CalcGamma(Distance),0,Limit) do {loop with small steps to left while (last)gamma sufficiently small}
      begin
      if InRange(FResult,0,Gamma) then {if smaller set as lowest gamma}
        begin
        Gamma:= FResult;
        Limit:= Max(1,Gamma)*twcGammaSearchMultiplier;
        end;
      Distance:= Distance-twcGammaDistCmStep;
      end;
    Distance:= twcGammaDistCmStep;
    Limit   := StartLimit;
    while InRange(CalcGamma(Distance),0,Limit) do {loop with small steps to right}
      begin
      if InRange(FResult,0,Gamma) then  {if smaller set as lowest gamma}
        begin
        Gamma:= FResult;
        Limit:= Max(1,Gamma)*twcGammaSearchMultiplier;
        end;
      Distance:= Distance+twcGammaDistCmStep;
      end;
    FDataPtr^[FPoint]:= Gamma;
    if assigned(FStatsSampler) then
      FStatsSampler.Add_X(Gamma);
    end;
  {$ENDIF}
  twCalcQuadFit:
    FWellhofer.QuadFilter(FDistance,FSource1,FSource1);
  end;
{$IFDEF THREAD_USESTATS}
Inc(FStarts);
{$ENDIF}
FActive:= False;
end; {~execute}


{$IFDEF THREAD_GAMMA}
procedure TMathThread.GammaWork(Point:Integer);
begin
if FCalculation=twCalcGamma then
  begin
  FActive:= True;
  FPoint := Point;
  if Suspended then Resume
  else              Execute;
  end;
end; {~gammawork}
{$ENDIF}


//https://wiki.freepascal.org/User_Changes_2.4.4#TThread.Suspend_and_TThread.Resume_have_been_deprecated
{21/02/2020 Resume->Start}
procedure TMathThread.QuadFitWork(FilterWidth            :twcFloatType;
                                  ASource_and_Destination:twcDataSource);
begin
if FCalculation=twCalcQuadFit then
  begin
  FActive  := True;
  FDistance:= FilterWidth;
  FSource1 := ASource_and_Destination;
  if Suspended then Start
  else              Execute;
  end;
end; {~quadfitwork}


destructor TMathThread.Destroy;
begin
if assigned(FFit) then
  try
    FreeAndNil(FFit);
   except
    FWellhofer.ExceptMessage('TMathThread.Destroy!');
   end;
FWellhofer   := nil;
FDataPtr     := nil;
FStatsSampler:= nil;
inherited;
end; {~destroy}

{$ENDIF}

initialization
AppVersionString:= GetAppVersionString(False,2,True);                           //TOtools
twNumCPU        := GetNumCPU;                                                   //TOtools
end.


