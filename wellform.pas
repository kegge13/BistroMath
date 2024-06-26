﻿unit WellForm;  {© Theo van Soest Delphi: 01/08/2005-29/05/2024 | Lazarus 3.2.0/FPC 3.2.2: 02/10/2022}
{$mode objfpc}{$h+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$I BistroMath_opt.inc}

(*
===========================================================================================
 This unit defines the user interface of BistroMath and is original work of Theo van Soest.
 Changed versions of this unit are not allowed to be published as "BistroMath".
 The new name should be significantly different.

 It is published under the GNU Lesser General Public License v3 (LGPL-3.0).
 https://tldrlegal.com/license/gnu-lesser-general-public-license-v3-%28lgpl-3%29

 The printing fuctionality (form2pdf) is kindly provided by Alan Chamberlain.
 https://github.com/alanphys/Form2PDF
===========================================================================================
*)

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, StdCtrls, ActnList, EditBtn, Spin,
  {$IFDEF Windows}
  htmlhelp,
  {$ENDIF}
  TAGraph, TASeries, TAFuncSeries, TATransformations, TAAxisSource, SpinEx,
  RTTICtrls, LCLType, Grids, ValEdit, LMessages, Buttons, FileIter,
  TOconfigStrings, TOnumparser, Wellhofer, PanelElements, TAChartAxisUtils,
  TATools;


//Work-around for backward compatibility with for LCL v2.0.10 and below.
//In a testversion of LcL v2.0.12 OnMarkToText was set as deprecated.
//but the object inspector v2.0.10 offers only OnMarkToText and the pre-production version of v2.0.12 only OnGetMarkText.
//Therefore setting this at design time WAS become problematic. Instead this event now is set at runtime in FormCreate.
//The solution below works in Laz v2.0.12, therefore I leave it as is for now. It will change with LcL 2.2
//https://forum.lazarus.freepascal.org/index.php/topic,53455.msg396003.html#msg396003
{$if declared(TChartGetAxisMarkTextEvent)}
 {$DEFINE LCL_2-3_Up}
{$endif}

const
  NumSpecialModes  =   3;
  NumSpecialModePar=   6;
  EvalDecimals     =   3;
  CxMaxCol         =   1;  {panelelements,starting from 0}
  CxDefaultRowCnt  =   9;
  OD2doseTableMax  =  13;
  FPrectLeft       =  20;
  FPRectOffset     = FPrectLeft+42;
  FPrectWidth      = 100;
  FPrectHalf       = FPrectWidth div 2;
  FPrectSpacing    = 185;
  FPtextOffset     =   2;
  FPdotRadius      =   2;
  FPreadyColor     = clLime;

type
  ConvNameItems=(ConvListStart,ConvListXType,ConvListEType,ConvListEnd,
                 ConvScanType,ConvEMV,ConvEkV,ConvDcm,ConvDmm,
                 ConvModality,ConvLcm,ConvXcm,ConvYcm,ConvLmm,ConvXmm,ConvYmm,
                 ConvFtype,ConvDet,ConvSeparator);
  ConvNameList = array[ConvNameItems] of String;
  ConvItemList = array of ConvNameItems;
  PlotItems    = (pMeasured,pCalculated,pReference,pBuffer{,pCombined});
  ExtSymType   = (ExtSymLinacError,ExtSymAreaRatio,ExtSymElevation);


{$IFDEF THREADED}
//http://www.experts-exchange.com/articles/239/Displaying-progress-in-the-main-form-from-a-thread-in-Delphi.html
type
  htObjectPlainProc = procedure                        of object;
  {$IFDEF THREAD_PLOT}
  htObjectFillProc  = procedure(ASeries  :PlotItems;
                                ASource  :twcDataSource;
                                AScaling :twcFloatType;
                                AnalRange:Boolean=False) of object;
  {$ENDIF THREAD_PLOT}

  {$IFDEF THREAD_FILES}
  htObjectEventProc = procedure(ASender:TObject)       of object;
  {$ENDIF THREAD_FILES}

  THelpPlainThread=class(TThread)
  protected
    procedure Execute;                                           override;
  public
    FPlainProc: htObjectPlainProc;
    constructor Create(APlainProc  :htObjectPlainProc;
                       FreeWhenDone:Boolean          =True);     reintroduce;
  end;

  {$IFDEF THREAD_PLOT}
  {12/04/2022 FAnalyse}
  THelpFillThread=class(TThread)
  protected
    procedure Execute;                                           override;
  public
    FFillProc : htObjectFillProc;
    FSeries   : PlotItems;
    FSource   : twcDataSource;
    FScaling  : twcFloatType;
    FAnalyse  : Boolean;
    constructor Create(AFillProc       :htObjectFillProc;
                       ASeries         :PlotItems;
                       ASource         :twcDataSource;
                       AScaling        :twcFloatType;
                       UseAnalysisRange:Boolean=False;
                       FreeWhenDone    :Boolean=True);           reintroduce;
  end;
  {$ENDIF THREAD_PLOT}

  {$IFDEF THREAD_FILES}
  THelpEventThread=class(TThread)
  protected
    procedure Execute;                                           override;
  public
    FEventProc: htObjectEventProc;
    FSender   : TObject;
    constructor Create(AEventProc  :htObjectEventProc;
                       ASender     :TObject;
                       FreeWhenDone:Boolean          =True);     reintroduce;
  end;
  {$ENDIF THREAD_FILES}
{$ENDIF THREADED}


type
  AxisMapValue = array[snGT..snAB] of ShortInt;                                 //see AxisRotationMapping, i:(GT,AB) for 0..270      ,c:(GT,AB) for 0..270
  ModModeType  = (ModMNorm,ModMFilm,ModMBeam);
  CxComponents = (CxTitle,CxValue);
  CxBlock      = array[CxComponents] of TLabel;
  CxLine       = array[0..CxMaxCol ] of CxBlock;
  OD2dose_Rec  = record
                   DCDoseBox    : TCheckBox;
                   DCModalityBox: TComboBox;
                   DCFilmTypeBox: TComboBox;
                   DCBgBox      : TCheckBox;
                   DCEdit       : TFloatSpinEdit;
                  end;
  {09/06/2016 replaced boolean Active with tmenuitem MenuItem}
  SpecModeRec  = record
                   MenuItem: TMenuItem;
                   Fpar    : array[1..NumSpecialModePar] of twcFloatType;
                   Spar    : array[1..NumSpecialModePar] of String;
                  end;

  {===================TAnalyseForm====================

  The analyseform is the graphical user interface of BistroMath and handles all user's choices.
  A series of instances of the TWellhoferData object ('engines') handle all data input, output and analysis.

  22/07/2015
    Mayneord-related items added.
    FilterWidthItem and NormWidthItem removed form options menu.
   01/08/2015
    TemprefFile and temprefType removed because of in-memory implementation
   04/08/2015
    ViewLeftAxisLowZeroItem
   05/08/2015
    CxLabels CxValues combined in CxResults record
   12/08/2015
    MeasMaxAsCenterItem added
   03/12/2015
    Ft_EdgeDetectionGroupBox, EdgeSimoidRadius_mm added
   11/12/2015
    static LeffSeries and ReffSeries replaced with dynamic InFieldIndicators
    added FFFSlopeLines
   15/12/2015
    added MeasDetectFFFItem
   06/01/2016
    added FitMubPowerNumEdit
   08/01/2016
    added FitMubPowerFixedCheckBox
   02/02/2016
    logics of set/unset temporary reference changed
   12/02/2016
    preloadstream, preloadtransfer
   14/02/2016
    replaced tmemorystream with tstringstream
   18/03/2016
    added ProcessIgnoreTUnameItem
   12/05/2016
    added MeasPreserveDataItem
   15/06/2016
    added badpenumbrawidthcm to advanced settings tab
   28/06/2016
    added SelectedFitCol
   05/07/2016
    added FFFcenter submenu and related strings
    added overloaded versions of SyncSetNormalisation and SyncSetFFFcenter
   07/07/2016
    added FFFcenterRadius_cm,FFFInFieldOffset_cm
   29/07/2016
    added MeasFFFpeakIsCenterItem
   04/11/2015
    added UseDoseConvTab,UseDoseAddButtonClick
   08/11/2016
    added ReferenceGenericBeamItem
    renamed DoseConv|Grid/AddButton/DelButton/EditButton to ModList|Grid/AddButton/DelButton/EditButton
    added ModListNormRadioButton,ModListFilmRadioButton,ModListBeamRadioButton
    added ReferenceGenericBeamClick,ModListRadioButtonClick,ModMode
   05/12/2016
    added ProcessSyntheticProfile
  25/12/2016
    InventoryReader
  29/03/2017
    SyncSetFFFcenter -> SyncSetFFFpeak
  30/03/2017
    MeasNormFFFSubMenu,MeasCenterFFFSubMenu
  21/06/2017
    RefAlignTopforFFF
  11/07/2017
    Mayneord transform changed from processing item to measurement item
  15/12/2017
    DefExtSymSubMenu
  02/01/2018
    PanelElements
  18/01/2017
    created MeasGenStrategySubMenu, MeasAxisSubMenu, MeasSignalSubMenu
  22/01/2018
    CxUsedRowMax
  24/01/2018
    ProcessClearMergeItem
    CheckWellhoferReady
  01/02/2018
    ViewMillimetersItem
  29/05/2018
     GammaLimitFFF, GammaLimitConventional
  31/05/2018
    InsertOriginCheckBox
  11/06/2018
    MeasReNormaliseDataItem
  12/10/2018
    ViewMeasNormAdjustMode
    MeasNormAdjustEdit
    MeasNormAdjustFactor
  25/10/2018
    SpecialModeValues
  06/11/2018
    MeasGenericToElectronItem
  23/11/2018
    SmartScaleElectronPDD
  25/11/2018
    ProcessAutoscalingItem
  10/12/2019
    ProcessSigmoid2BufferItem
  17/12/2019
    SimpleModeItem
  17/03/2020
    RawDataEditor internal as replacement for external editor window
  14/04/2020
    ===Lazarus implementation===
  15/04/2020
    PlotScanMin added
  25/04/2020
    ViewAutoUnzoomFFFitem added
  08/05/2020
    removed PreloadTransferThread
  13/05/2020
    added AxisAlignSource
  30/05/2020
    added InventoryPrepareCanvas, OnInventorySelect
  31/05/2020
    added AxisMarkToText
  02/06/2020
    added ConfigRepair, WriteMenuShortCuts
  03/07/2020
    removed SavedIgnoreState
  05/07/2020
   added FilePrintFormClick,FilePrintAllItem,FilePrintPageItem
  12/07/2020
   removed EdgeConvRadioInflection, EdgeConvRadioSigmoid50, EdgeSigmoidConvCheckBox, EdgeSigmoidFFFCheckBox
   added array EdgeMethodCombo
  15/07/2020
   removed GammaLimitFFF, GammaLimitConventional
   added array
   added ViewNoDefaultAnnotationItem
   added AppliedEdgeRefNorm
  19/07/2020
   added MeasDetectSmallFieldItem,MeasCenterSmallSubMenu,MeasNormSmallSubMenu
  20/07/2020
   added EdgeSmallFieldWidth_cm
  21/07/2020
   added AxisViewFieldTypeCheckBox
   added fcWedge as field type
  24/07/2020
   added EdgeWedge90ShiftFactor
  28/07/2020
   added Ft_EdgeMethodCombo,Ft_CoFMethodCombo,Ft_NormMethodCombo,Ft_SymCorrCheckbox
  18/08/2020
   added EdgeMRlinacTUcsvList
  25/08/2020
   CxResults as dynamic array for rows
  27/08/2020
   added Ft_CenterRadiusEdit_cm, removed FFFcenterRadius_cm
  29/08/2020
   added LabelPlacingActive
  01/09/2020
   added AliasTabExit and AliasListDrawCell
  03/09/2020
   added Ft_DynPenumbraWidthLabel,Ft_DynPenumbraCheckbox
  08/09/2020
    added overloaded variant for Reload and ReadEditor with DoClearScreen option
  14/09/2020
    added Engines, AddEngine, HistoryListSize_num
  16/09/2020
    added FileHistoryItem
  17/09/2020
    added HistoryListFreezeCheckBox
  26/09/2020
    added PenumbraSigmoids,ViewPenumbraItem
  29/09/2020
    added TempRefEngine,function PassRefOrg
  30/09/2020
    Wellhofer2Editor renamed to Engine2Editor with extra options
  13/10/2020
    added fcElectron as field type
    added AutoSetDecPointCheckBox, AutoDecPointList
  17/11/2020
    added FileMultipleInputItem,UsedDataTopLine
  08/12/2020
    added ShowLockItemCheckBox
  30/01/2021
    event DataPlotExtentChanged(Sender: TChart);
  21/02/2021
    replace AxisMarkToText for deprecated OnMarkToText event with
    RightAxisGetMarkText on OnGetMarkText
  02/03/2021
    added FFFfeatures
  03/03/2020
    added Ft_DetDiagonalLabel,Ft_DetDiagonalCheckbox, removed MeasDetectDiagItem
  07/03/2021
    added Nominal_IFA_CheckBox, DefaultMRlinacSSD_cm
  15/03/2021
    added Ft_Default_SSD_Label, Ft_Default_SSD_cm
    removed DefaultMRlinacSSD_cm
    added MeasMenuClick(Sender: TObject);
  14/05/2021
    ViewSwapXXitem -> SwapXXchecbox
    added ViewTopModelItem
  01/06/2021
    added FFFMinFieldSizeLabel,FFFMinFieldSize_cm,MeasGeneric_mm_Item
  31/01/2022
    Data type changed from Int64 to PtrInt
    in function HelpHandler(Command:Word; Data:PtrInt; var CallHelp:Boolean): Boolean;
    PtrInt is used in the THelpEvent (forms.pp) and avoids platform dependencies
  28/03/2022
    Introduced AppliedGammaScope
  10/04/2022
    added ProcessSetFFFItem, FFFinFileCheckBox
  14/10/2022
    added MeasCalcPDDfitItem as quick access to PDDfitCheckBox in Settings tab
    added FocusPositionTab,FocusPositionPanel
    added arrays FP_Center_button and FP_Center_mm
  16/10/2022
    added FocusPosition_IEC_Convention,FocusPositionDetectorRotates
  18/10/2022
    added FocusPosition_mapping_Box
  24/10/2022
    added FocusPositionUseClipBoard,FocusPositionSetupGroupBox,FocusPositionInputGroupBox
  28/10/2022
    added FP_Results_label,FP_Average_mm
  29/10/2022
    added FocusPosition_UpperBLD_ident
  01/11/2022
    added FocusPositionCollimFromName
  02/11/2022
    added FocusPositionResetButton,FocusPositionResetButtonClick
  03/11/2022
    added procedure FocusPositionProcessFile
  04/11/2022
    added FP_Results_Titles,FP_Calc_Dbfso_mm,FP_Calc_Drot_mm
  08/11/2022
    added FP_BankLevel_Label,FP_BankLevel_mm as array[twcBanks]
  14/11/2022
    added FocusPositionNamePatternEdit
  15/11/2022
    added FocusPositionDetSurfaceIsoc,FocusPositionDetectorInIsoc,FocusPositionDetectorOther: TRadioButton
  17/11/2022
    added ProcessSendToFPItem
  18/11/2022
    added OffAxisInclusionLabel,OffAxisInclusionRange_cm,FocusPositionDetAutoSet
  26/11/2022
    added FocusPositionFullScaleLabel,FocusPositionFullScale_mm
  28/11/2022
    added FocusPositionZeroRotButton,FocusPositionZeroFPButton,FocusPositionZeroClick
  09/12/2022
    added FP_BankFactor
  21/01/2023
    added StructuredDataSetsLabel,StructuredDataSetsList,FocusPositionStructuredData
  20/02/2023
    added GetCpuUsage for StatusBar.Panels[3] and FIdleTimer
  }

  {=========== TAnalyseForm =====================}

  { TAnalyseForm }
  TAnalyseForm = class(TForm)
    //menu
    MainMenu                    : TMainMenu;
    //-file menu
    FileMenu                    : TMenuItem;
    FileOpenItem                : TMenuItem;  {Ctrl+O}       //OnClick = FileOpen
    FileOpenTempRefItem         : TMenuItem;  {Ctrl+Alt+O}   //OnClick = FileOpenTempRefClick
    FileLoadDataItem            : TMenuItem;  {Ctrl+L}       //OnClick = FileLoadData
    FileSaveMeasurementItem     : TMenuItem;  {Ctrl+S}       //OnClick = FileSaveMeasurementAction
    FileSaveFilteredItem        : TMenuItem;  {Ctrl+Alt+S}
    FileSaveItem                : TMenuItem;  {Ctrl+A}
    FileIgnoreClipboardItem     : TMenuItem;  {Ctrl+I}       //OnClick = FileIgnoreClipboardClick
    FileSaveAsReferenceItem     : TMenuItem;                 //OnClick = FileSaveAsReferenceAction;  Tag=4
    FileLockCriticalItems       : TMenuItem;  {Ctrl+Alt+R}   //OnClick = OptionModeClick;            Tag=4
    FileHistoryItem             : TMenuItem;  {Ctrl+H}       //OnClick = HistoryListSizeClick, linked to HistoryListCheckBox
    FileExitItem                : TMenuItem;  {Alt+F4}       //OnClick = FileExitAction
    FileMultipleInputItem       : TMenuItem;
    //-processing menu
    ProcessingMenu              : TMenuItem;
    ProcessingDivisor1          : TMenuItem;
    ProcessingDivisor2          : TMenuItem;
    ProcessingDivisor3          : TMenuItem;
    ProcessSetFFFItem           : TMenuItem;  {Ctrl+F}       //OnClick = OnDataRead
    ProcessAutoscalingItem      : TMenuItem;  {none}         //OnClick = Reload;                     Tag=4
    ProcessSigmoid2BufferItem   : TMenuItem;  {Ctrl+B}       //OnClick = Reload                      >wApplySigmoidToBuffer
    ProcessReprocessItem        : TMenuItem;  {Ctrl+R}       //OnClick = OnDataRead
    ProcessSendToFPItem         : TMenuItem;  {Shift+Ctrl+R}
    ProcessResetFitItem         : TMenuItem;  {Ctrl+Z}       //OnClick = ProcessResetFitClick
    ProcessMergeItem            : TMenuItem;  {Ctrl+Q}       //OnClick = OnDataRead
    ProcessSetMergeSourceItem   : TMenuItem;  {Ctrl+Alt+Q}   //OnClick = ProcessMergeSourceClick
    ProcessClearMergeItem       : TMenuItem;  {Shift+Ctrl+O} //OnClick = ProcessMergeSourceClick
    ProcessMirrorMeasRefItem    : TMenuItem;  {Ctrl+X}       //OnClick = ProcessMirrorMeasRefClick
    ProcessSyntheticProfile     : TMenuItem;  {Shift+Ctrl+F} //OnClick = Reload                      >wDefaultIgnoreSet
    ProcessSetTempRefItem       : TMenuItem;  {Ctrl+T}       //Action  = TempRefAction               >wCheckRefCurveString
    ProcessUnsetTempRefItem     : TMenuItem;  {Shift+Ctrl+T} //OnClick = ProcessUnsetTempRefClick
    ProcessIgnoreTUnameItem     : TMenuItem;  {Ctrl+U}       //OnClick = ProcessUpdateDataRead       >wCheckRefCurveString,wCheckRefIgnoreLinac
    ProcessCheckTempTypeItem    : TMenuItem;  {Ctrl+Y}       //OnClick = ProcessUpdateDataRead       >wCheckRefCurveString
    //-view menu
    ViewMenu                    : TMenuItem;
    ViewDivisor1                : TMenuItem;
    ViewDivisor2                : TMenuItem;
    ViewDivisor4                : TMenuItem;
    ViewMeasuredItem            : TMenuItem;  {M}            //OnClick = ViewItems
    ViewPointsItem              : TMenuItem;  {P}            //OnClick = ViewItems
    ViewCalculatedItem          : TMenuItem;  {C}            //OnClick = OnDataRead
    ViewReferenceItem           : TMenuItem;  {R}            //OnClick = ViewItems
    ViewBufferItem              : TMenuItem;  {B}            //OnClick = ViewItems
    ViewIndicatorsItem          : TMenuItem;  {I}            //OnClick = ViewItems
    ViewTopModelItem            : TMenuItem;  {T}            //OnClick = ViewItems
    ViewPenumbraItem            : TMenuItem;  {E}
    ViewFFFIndicatorsItem       : TMenuItem;  {F}            //OnClick = ViewItems
    ViewValuesItem              : TMenuItem;  {V}            //OnClick = ViewItems
    ViewHighResValItem          : TMenuItem;  {H}            //OnClick = OnDataRead
    ViewMeasNormAdjustMode      : TMenuItem;  {N}
    ViewStandardPanelsetup      : TMenuItem;  {Ins}          //OnClick = SetDefaultPanel
    ViewZoomItem                : TMenuItem;  {Z}            //OnClick = ActivateZoom
    ViewUnZoomItem              : TMenuItem;  {U}            //OnClick = ActivateUnZoom
    ViewAutoUnzoomFFFitem       : TMenuItem;  {W}
    ViewAutoUnzoomPDDitem       : TMenuItem;  {Y}
    ViewLeftAxisLowZeroItem     : TMenuItem;  {K}            //OnClick = ViewItems
    ViewRightAxisToGridItem     : TMenuItem;  {G}            //OnClick = RightAxisToGridClick
    ViewScaleElectronPDDrange   : TMenuItem;  {S}            //OnClick = SmartScaleElectronPDD
    ViewMillimetersItem         : TMenuItem;  {X}            //OnClick = OnDataRead
    ViewClearItem               : TMenuItem;  {End}          //OnClick = ClearScreen
    ViewBottomAxisAlwaysBlack   : TMenuItem;  {none}         //OnClick = OnDataRead                   >wUserAxisSign
    ViewNoDefaultAnnotationItem : TMenuItem;  {O}            //OnClick = UImodeChange
    //-measurement menu
    MeasMenu                    : TMenuItem;  {Shift in use: ',','+','-','.',1,2,3,A,B,C,D,E,G,H,I,L,M,N,O,P,R,S,T,U,W,Y,Z}
    MeasDivisor1                : TMenuItem;
    MeasDivisor2                : TMenuItem;
    MeasDivisor4                : TMenuItem;
    MeasDivisor5                : TMenuItem;
    MeasSymCorrectItem          : TMenuItem;  {Shift+S}      //OnClick = Reload
    MeasMove2OriginItem         : TMenuItem;  {Shift+O}      //OnClick = Reload                       >wCenterProfiles
    MeasMirrorItem              : TMenuItem;  {Shift+T}      //OnClick = ReadEditor
    MeasResampleItem            : TMenuItem;  {Shift+R}      //OnClick = Reload
    MeasMayneordItem            : TMenuItem;  {Shift+M}      //OnClick = Reload
    MeasUserDoseItem            : TMenuItem;  {Shift+U}      //OnClick = OnDataRead                   >wApplyUserLevel
    MeasCalcPDDfitItem          : TMenuItem;                 //                                       linked to PDDfitCheckBox
    MeasUsePDDfitModelItem      : TMenuItem;  {Shift+P}      //OnClick = OnDataRead
    MeasMirrorToBufferItem      : TMenuItem;  {Shift+E}      //OnClick = ViewItems
    MeasLocalPeakItem           : TMenuItem;  {Shift+L}      //OnClick = LocalPeakClick
    MeasMoveLeftItem            : TMenuItem;  {Shift+,}      //OnClick = MeasMoveClick
    MeasMoveRightItem           : TMenuItem;  {Shift+.}      //OnClick = MeasMoveClick
    //--submenu
    MeasGenStrategySubMenu      : TMenuItem;
    MeasBadPenumbraItem         : TMenuItem;  {Shift+A}      //OnClick = Reload
    MeasMissingPenumbraItem     : TMenuItem;  {Shift+I}      //OnClick = OnDataRead
    MeasZeroStepsItem           : TMenuItem;  {Shift+Z}      //OnClick = OnMenu
    MeasGenericToPDDItem        : TMenuItem;  {Shift+G}      //OnClick = Reload                       >wGenericToPDD
    MeasGenericToElectronItem   : TMenuItem;  {Shift++}      //OnClick = Reload
    MeasGeneric_mm_Item         : TMenuItem;  {Shift+-}      //OnClick = Reload
    MeasPeakFFFSubMenu          : TMenuItem;
    MeasExtSymSubMenu           : TMenuItem;
    //--submenu
    MeasSSDsubmenu              : TMenuItem;
    MeasSDD2SSDItem             : TMenuItem;  {Shift+C}      //OnClick = Reload                       >wScaleSDD2SSD
    MeasScale2DefaultSSDitem    : TMenuItem;  {Shift+H}      //OnClick = ReadEditor                   >wScale2DefaultSSD
    //--submenu
    MeasSignalSubMenu           : TMenuItem;
    MeasBackgroundCorrItem      : TMenuItem;  {Shift+B}      //OnClick = Reload
    MeasOD2DoseConvItem         : TMenuItem;  {Shift+N}      //OnClick = ReadEditor
    MeasIon2DoseItem            : TMenuItem;  {Shift+Y}      //OnClick = Ionisation2DoseClick
    MeasReNormaliseDataItem     : TMenuItem;  {Shift+W}      //OnClick = ReadEditor                   >wRenormaliseData
    //--submenu
    MeasAxisSubMenu             : TMenuItem;
    MeasInvertGTitem            : TMenuItem;  {Shift+1}      //OnClick = ReadEditor
    MeasInvertABitem            : TMenuItem;  {Shift+2}      //OnClick = ReadEditor
    MeasInvertUDitem            : TMenuItem;  {Shift+3}      //OnClick = ReadEditor
    MeasRemapCoordinates        : TMenuItem;                 //OnClick = ReadEditor
    MeasPreserveDataItem        : TMenuItem;                 //OnClick = SetWellhoferValues;   Tag=4  >wAxisPreserveOnExport
    //-reference menu
    ReferenceMenu               : TMenuItem;
    RefAutoLoadItem             : TMenuItem;  {Alt+L}        //OnClick = ViewItems
    RefDeviceSpecificItem       : TMenuItem;  {Alt+I}        //OnClick = ReferenceDevSpecClick        >wMeas2TankMapping
    RefGenericBeamItem          : TMenuItem;  {Alt+R}        //OnClick = ReferenceGenericBeamClick    >wReferenceFromGeneric
    RefMakeIndexItem            : TMenuItem;  {Alt+X}        //OnClick = Reload
    RefAtDefaultSSDItem         : TMenuItem;  {Alt+H}        //OnClick = ReadEditor                   >wRefAtDefaultSSD
    RefBackgroundCorrItem       : TMenuItem;  {Alt+B}        //OnClick = OnDataRead
    RefSymCorrectItem           : TMenuItem;  {Alt+S}        //OnClick = ReadEditor
    RefAlignItem                : TMenuItem;  {Alt+M}        //OnClick = Reload
    RefAlignTopforFFF           : TMenuItem;  {Alt+F}        //OnClick = Reload
    RefNormaliseItem            : TMenuItem;  {Alt+N}        //OnClick = OnDataRead
    //--submenu
    RefCalcSubMenu              : TMenuItem;
    RefUseDivideByItem          : TMenuItem;  {Alt+D}        //OnClick = CalcSubMenuClick
    RefUseGammaItem             : TMenuItem;  {Alt+G}        //OnClick = CalcSubMenuClick
    RefUseAddToItem             : TMenuItem;  {Alt+A}        //OnClick = CalcSubMenuClick
    RefUseUnrelatedToItem       : TMenuItem;  {Alt+U}        //OnClick = CalcSubMenuClick
    //-calculation menu
    CalculationMenu             : TMenuItem;
    CalcPostFilterItem          : TMenuItem;
    //-options menu
    OptionsMenu                 : TMenuItem;
    AdvancedModeItem            : TMenuItem;                 //OnClick = OptionModeClick
    SimpleModeItem              : TMenuItem;                 //OnClick = OptionModeClick
    ConfigLoadItem              : TMenuItem;                 //OnClick = SelectConfig
    ConfigReadItem              : TMenuItem;                 //OnClick = ConfigLoad
    ConfigSaveItem              : TMenuItem;                 //OnClick = ConfigSave
    ConfigSaveAsItem            : TMenuItem;                 //OnClick = ConfigSaveAsItemClick;   Tag=4
    ConfigAutoSaveItem          : TMenuItem;                 //autocheck only
    //-presets menu
    PresetsMenu                 : TMenuItem;
    ConfigSavePresetAs          : TMenuItem;                 //OnClick = ConfigSaveAsItemClick
    //-help menu
    HelpMenu                    : TMenuItem;
    AboutItem                   : TMenuItem;                 //OnClick = OnMenu
    HelpItem                    : TMenuItem;                 //OnClick = OnMenu
    //inventory popup menu
    InventoryPopupMenu          : TPopupMenu;
    InventoryPopOpenItem        : TMenuItem;
    InventoryPopDelItem         : TMenuItem;
    InventoryPopExpandItem      : TMenuItem;
    InventoryPopReturnItem      : TMenuItem;
    //aliaslist popup menu
    AliasPopupMenu              : TPopupMenu;
    AliasListInsert             : TMenuItem;
    AliasListDelete             : TMenuItem;
    //tabs
    PageControl                 : TPageControl;
    //-analysis tab
    AnalysisTab                 : TTabSheet;
    DataPlot                    : TChart;                                       //set DataPlot.AutoFocus on to avoid trigger of PageControlRequestChange with VK_LEFT/RIGHT
    L_AxisTransforms            : TChartAxisTransformations;                    //needed to set the vertical axis otherwise than showing all data
    L_AxisTransform_AutoScale   : TAutoScaleAxisTransform;                      //see AutoZoom function
    R_AxisTransforms            : TChartAxisTransformations;                    //see C:\lazarus\components\tachart\demo\axistransf
    R_AxisTransform_AutoScale   : TAutoScaleAxisTransform;                      //see also AxisAlignSource object and AutoZoom function
    TopModelSeries              : TFuncSeries;                                  //TopModelSeries plots a function; for development ease two series are added at design time, all others at runtime
    ErrorSeries                 : TLineSeries;                                  //ErrorSeries shows residual of pddfit
    ResultsPanel                : TPanel;                                       //the placeholder for all analysis results
    MeasNormAdjustEdit          : TFloatSpinEditEx;                             //mostly hidden feature for changing normalisation level
    PositionLabel               : TLabel;
    PositionValue               : TLabel;
    //-histogram tab
    HistogramTab                : TTabSheet;
    HistogramPlot               : TChart;                                       //set HistoGramPlot.AutoFocus on to avoid trigger of PageControlRequestChange with VK_LEFT/RIGHT
    Histogram                   : TBarSeries;
    //-focus position tab
    FocusPositionTab            : TTabSheet;
    FocusPositionPanel          : TPanel;
    FocusPositionSetupGroupBox  : TGroupBox;
    FocusPositionMappingLabel   : TLabel;
    FocusPosition_mapping_Box   : TComboBox;
    FocusPositionDetectorRotates: TCheckBox;
    FocusPositionIECLabel       : TLabel;
    FocusPosition_IEC_Convention: TComboBox;
    FocusPositionUpperLabel     : TLabel;
    FocusPosition_UpperBLD_ident: TComboBox;
    FocusPositionFullScaleLabel : TLabel;
    FocusPositionFullScale_mm   : TFloatSpinEditEx;
    FocusPositionDetectorOther  : TRadioButton;
    FocusPositionDetSurfaceIsoc : TRadioButton;
    FocusPositionDetAutoSet     : TRadioButton;
    FocusPositionDetectorInIsoc : TRadioButton;
    FocusPositionInputGroupBox  : TGroupBox;
    FocusPositionUseCollimInfo  : TCheckBox;
    FocusPositionStructuredData : TStaticText;
    FocusPositionCollimFromName : TCheckBox;
    FocusPositionNamePatternEdit: TLabeledEdit;
    FocusPositionResetButton    : TButton;
    FocusPositionZeroRotButton  : TButton;
    FocusPositionZeroFPButton   : TButton;
    FocusPositionResultGroupBox : TGroupBox;
    //-fit results tab
    PDDFitResultsTab            : TTabSheet;                                    //here model parameter values can be observed and copied after a pdd fit
    PDDFitResultsPanel          : TPanel;                                       //only used to set background color
    PDDFitResultsAllCheckBox    : TCheckBox;
    PDDFitResultsGrid           : TStringGrid;
    PDDFitResultsHeaderCheckBox : TCheckBox;
    PDDFitresultsLabel          : TLabel;
    PDDFitResultsLabelsCheckBox : TCheckBox;
    //-alias tab
    AliasTab                    : TTabSheet;
    AliasListEditor             : TValueListEditor;
    //-Field Types tab
    FieldTypesTab               : TTabSheet;                                    //a series of settings is highly dependendent on the actual data being processed: the Field Types
    FieldTypesPanel             : TPanel;                                       //a place holder to ba able to set a background color
    Ft_DetectLabel              : TLabel;                                       //twcFieldClass=(fcStandard,fcFFF,fcSmall,fcMRlinac,fcWedge,fcElectron);
    Ft_DetDiagonalLabel         : TLabel;
    Ft_MeasSymCorrLabel         : TLabel;
    Ft_RefSymCorrLabel          : TLabel;
    Ft_DynPenumbraWidthLabel    : TLabel;
    Ft_EdgePrimaryLabel         : TLabel;
    Ft_EdgeFallBackLabel        : TLabel;
    Ft_CoFLabel                 : TLabel;
    Ft_NormLabel                : TLabel;
    Ft_CenterModelRadiusLabel   : TLabel;
    Ft_Default_SSD_Label        : TLabel;
    Ft_FFFDetectionGroupBox     : TGroupBox;
    FFFMinFieldSizeLabel        : TLabel;
    FFFMinFieldSize_cm          : TFloatSpinEditEx;                             //>wFFFMinFieldSizeCm
    FFFMinDoseDifLabel          : TLabel;
    FFFMinDoseDif_perc          : TFloatSpinEditEx;                             //>wFFFMinDoseDifPerc
    FFFMinEdgeDifLabel          : TLabel;
    FFFMinEdgeDif_mm            : TFloatSpinEditEx;                             //>wFFFMinEdgeDifCm
    FFFInFieldExtLabel          : TLabel;
    FFFInFieldExt_cm            : TFloatSpinEditEx;                             //>twcFFFInFieldExtCm
    //--Edge groupbox
    Ft_EdgeDetectionGroupBox    : TGroupBox;
    EdgeDetectionCheckBox       : TCheckBox;                                    //>wEdgeDetect
    EdgeDetectionError_mm       : TFloatSpinEditEx;                             //>wEdgeFallBackCm
    EdgeSigmoidRadiusLabel      : TLabel;
    EdgeSigmoidRadius_cm        : TFloatSpinEditEx;                             //>wInflectionSigmoidRadiusCm
    EdgeSmallFieldWidthLabel    : TLabel;
    EdgeSmallFieldWidth_cm      : TFloatSpinEditEx;                             //>wSmallFieldLimitCm
    EdgeWedge90ShiftLabel       : TLabel;
    EdgeWedge90ShiftFactor      : TFloatSpinEditEx;                             //>wWedge90ShiftFactor
    EdgeMRlinacTUcsvList        : TLabeledEdit;
    //-settings tab
    SettingsTab                 : TTabSheet;
    SettingsPanel               : TPanel;                                       //placeholder to set background color
    FilterWidthLabel            : TLabel;
    FilterWidth_mm              : TFloatSpinEditEx;
    CalcWidthLabel              : TLabel;
    CalcWidth_mm                : TFloatSpinEditEx;
    ResampleGridLabel           : TLabel;
    ResampleGrid_mm             : TFloatSpinEditEx;
    GlobalNormAdjustLabel       : TLabel;
    GlobalNormAdjust_perc       : TFloatSpinEditEx;
    UserBorderDoseLabel         : TLabel;
    UserBorderDose_perc         : TFloatSpinEditEx;
    XpenumbraLabel              : TLabel;
    XLpenumbra_perc             : TFloatSpinEditEx;                             //>wXPenumbraL
    XHpenumbra_perc             : TFloatSpinEditEx;                             //>wXPenumbraH
    EpenumbraLabel              : TLabel;
    ELpenumbra_perc             : TFloatSpinEditEx;                             //>wEPenumbraL
    EHpenumbra_perc             : TFloatSpinEditEx;                             //>wEPenumbraH
    DefaultEnergyLabel          : TLabel;
    DefaultEnergy_MeV           : TFloatSpinEditEx;
    OffAxisInclusionLabel       : TLabel;
    OffAxisInclusionRange_cm    : TFloatSpinEditEx;
    HistogramLimitLabel         : TLabel;
    HistogramLimit_num          : TFloatSpinEditEx;
    InsertOriginCheckBox        : TCheckBox;
    Nominal_IFA_CheckBox        : TCheckBox;                                    //>wNominalIFA
    //--shift settings groupbox
    ShiftGroupBox               : TGroupBox;
    ShiftStepLabel              : TLabel;
    ManualShiftStep_cm          : TFloatSpinEditEx;
    //--merge groupbox
    MergeGroupBox               : TGroupBox;
    MergeProfShiftLabel         : TLabel;
    MergeProfShift_cm           : TFloatSpinEditEx;
    MergeScaleOverlapCheckBox   : TCheckBox;
    MergePDDShiftLabel          : TLabel;
    MergePDDShift_cm            : TFloatSpinEditEx;
    MergeMatchCheckBox          : TCheckBox;
    //--mayneord groupbox
    MayneordGroupBox            : TGroupBox;
    MayneordDmaxLabel           : TLabel;
    MayneordDmax_cm             : TFloatSpinEditEx;
    MayneordSSD1Label           : TLabel;
    MayneordSSD1_cm             : TFloatSpinEditEx;
    MayneordSSD2Label           : TLabel;
    MayneordSSD2_cm             : TFloatSpinEditEx;
    //--gamma analysis groupbox
    GammaDepthCutoffLabel       : TLabel;
    GammaDepthCutoff_mm         : TFloatSpinEditEx;                             //>twcGammaCutoffDepthCm
    GammaDistNormLabel          : TLabel;
    GammaDistNorm_mm            : TFloatSpinEditEx;                             //>twcGammaDistCmBase
    GammaDoseCutoffLabel        : TLabel;
    GammaDoseCutoff_perc        : TFloatSpinEditEx;                             //twcGammaCutoffPercent
    GammaDoseNormLabel          : TLabel;
    GammaDoseNorm_perc          : TFloatSpinEditEx;                             //>twcGammaDosePercBase
    GammaSearchFactorLabel      : TLabel;
    GammaSearchMultiplier_num   : TFloatSpinEditEx;                             //>twcGammaSearchMaxFactor
    GammaStepsLabel             : TLabel;
    GammaEdit_Steps_per_mm      : TFloatSpinEditEx;                             //>twcGammaDistCmStep
    GammaSettingsGroupBox       : TGroupBox;
    GammaLimitAreaLabel         : TLabel;
    GammaLocalDoseCheckBox      : TCheckBox;                                    //>twcGammaLocalDosePerc
    //--pddfit groupbox
    PDDfitGroupBox              : TGroupBox;
    PDDfitCheckBox              : TCheckBox;
    FitCyclesLabel              : TLabel;
    FitCycles_num               : TSpinEditEx;                                  //>twcNMcycles
    FitENRLabel                 : TLabel;
    FitENRlimit_ratio           : TFloatSpinEditEx;
    FitENRweigthedCheckBox      : TCheckBox;                                    //>twcPddFitCostENRWeighted
    FitMaxTimeLabel             : TLabel;
    FitMaxTime_sec              : TFloatSpinEditEx;                             //>twcNMseconds
    FitMu3CheckBox              : TCheckBox;                                    //>pddfitEnames[pddfit_mu3]
    FitMu4CheckBox              : TCheckBox;                                    //>pddfitEnames[pddfit_mu3]
    FitMubPowerFixedCheckBox    : TCheckBox;
    FitMubPowerLabel            : TLabel;
    FitMubPower_exp             : TFloatSpinEditEx;                             //>twcPddFitMubPower
    FitMx2CheckBox              : TCheckBox;                                    //>pddfitEnames[pddfit_mx2]
    FitRestartsLabel            : TLabel;
    FitRestarts_num             : TSpinEditEx;                                  //>twcNMrestarts
    FitZWeightLabel             : TLabel;
    FitZWeight_val              : TFloatSpinEditEx;                             //>twcPddFitZWeightPower
    //-advanced settings tab
    AdvancedSettingsTab         : TTabSheet;
    AdvancedSettingsPanel       : TPanel;                                       //placeholder to set background color
    AdvancedModeStartCheckBox   : TCheckBox;
    FFFinFileCheckBox           : TCheckBox;
    ShowLockItemCheckBox        : TCheckBox;
    AddDateTimeCheckBox         : TCheckBox;
    ShowWarningCheckBox         : TCheckBox;
    LogLevelEdit                : TSpinEditEx;
    HistoryListCheckBox         : TCheckBox;                                    //onclick=HistoryListSizeClick; linked to FileHistoryItem
    HistoryListSize_num         : TSpinEditEx;
    HistoryListFreezeCheckBox   : TCheckBox;                                    //when checked data will not be reprocessed, despite changed circumstances/reference data
    ForceMatchingCheckBox       : TCheckBox;
    OutlierFilterStatsCheckBox  : TCheckBox;                                    //>wOutlierFilter
    OutlierMaxPoints_num        : TSpinEditEx;
    AutoSetDecPointCheckBox     : TCheckBox;
    AutoDecPointList            : TEdit;
    BadPenumbraLabel            : TLabel;
    BadPenumbraWidth_cm         : TFloatSpinEditEx;
    OriginMinLevelLabel         : TLabel;
    OriginMinLevel_perc         : TFloatSpinEditEx;                             //>twcOriginMinNormFraction
    PipsPixelSizeLabel          : TLabel;
    PipsPixelSize_mm            : TFloatSpinEditEx;                             //>wPipsPixelCm
    StructuredDataSetsLabel     : TLabel;
    StructuredDataSetsList      : TEdit;                                        //>w2D_ArrayRefList
    //--axis title groupbox
    AxisViewGroupBox            : TGroupBox;
    AxisViewFileTypeCheckBox    : TCheckBox;
    AxisViewCollAngleCheckBox   : TCheckBox;
    AxisViewSSDCheckBox         : TCheckBox;
    AxisViewFieldTypeCheckBox   : TCheckBox;
    AxisViewDetNameCheckBox     : TCheckBox;
    AxisViewCommentsCheckBox    : TCheckBox;
    AxisViewDetLengthLabel      : TLabel;
    AxisViewDetLength_num       : TSpinEditEx;
    AxisViewComLengthLabel      : TLabel;
    AxisViewComLength_num       : TSpinEditEx;
    //--colors groupbox
    ColorsBox                   : TGroupBox;
    PlotColorPanel              : TColorButton;
    GridColorPanel              : TColorButton;
    UIColorPanel                : TColorButton;
    //--axis swapping and remapping groupbox
    MeasRemappingBox            : TGroupBox;
    MeasReMappingString         : TComboBox;
    SwapGTcheckbox              : TCheckBox;                                    //OnClick = OnDataRead
    SwapABcheckbox              : TCheckBox;                                    //OnClick = OnDataRead
    SwapUDcheckbox              : TCheckBox;                                    //OnClick = OnDataRead
    SwapLRcheckbox              : TCheckBox;                                    //OnClick = OnDataRead
    //--match settings groupbox
    MatchGroupBox               : TGroupBox;
    MatchRangeDividerLabel      : TLabel;
    MatchRangeDivider_num       : TSpinEditEx;
    MatchStepFactorLabel        : TLabel;
    MatchSteps_num              : TSpinEditEx;
    MatchNormPercLabel          : TLabel;
    MatchNormDelta_perc         : TFloatSpinEditEx;
    MatchInclusionLabel         : TLabel;
    MatchInclusionLimit_perc    : TFloatSpinEditEx;
    //--linac symmetry error groupbox
    LinacErrorGroupBox          : TGroupBox;
    LinacErrInvertABCheckBox    : TCheckBox;
    LinacErrInvertGTCheckBox    : TCheckBox;
    LinacSymLabel               : TLabel;
    LinacSymInner_cm            : TFloatSpinEditEx;
    LinacSymOuter_cm            : TFloatSpinEditEx;
    //-configuration tab
    ConfigurationTab            : TTabSheet;
    ConfigurationPanel          : TPanel;
    ModListAddButton            : TButton;
    ModListBeamRadioButton      : TRadioButton;
    ModListDelButton            : TButton;
    ModListEditButton           : TButton;
    ModListFilmRadioButton      : TRadioButton;
    ModListGrid                 : TStringGrid;
    ModListNormRadioButton      : TRadioButton;
    //-logging tab
    LogTab                      : TTabSheet;
    LogTabMemo                  : TMemo;                                        //LogTabMemo.Tag is used for maximum number of lines, initial value 500
    //-raw data tab
    RawDataTab                  : TTabSheet;                                    //shows the data as received on the clipboard or from file. binary data are converted to text format
    RawDataEditor               : TMemo;
    //-inventory tab
    InventoryTab                : TTabSheet;
    InventoryAltAxisCheckBox    : TCheckBox;                                    //>wMeas2TankMapping
    InventoryDirBox             : TDirectoryEdit;                               //InventoryDirBoxAccept
    InventoryGrid               : TStringGrid;
    InventoryRadioRef           : TRadioButton;                                 //InventoryDirBoxChange
    InventoryRadioData          : TRadioButton;                                 //InventoryDirBoxChange
    InventoryRadioSelf          : TRadioButton;                                 //InventoryDirBoxChange
    InventorySetRefDirButton    : TButton;                                      //InventorySetRefDirClick
    //-file conversion tab
    FileConversionTab           : TTabSheet;
    FileConversionPanel         : TPanel;                                       //placeholder to set background color
    FileConvDestinationLabel    : TLabel;
    FileConvDestinationTypeLabel: TLabel;
    FileConvSourceLabel         : TLabel;
    FileConvSourceTypeLabel     : TLabel;
    FileConvSourcePath          : TDirectoryEdit;
    FileConvSourceListBox       : TListBox;
    FileConvSourceRecursive     : TCheckBox;
    FileConvIon2DoseCheckBox    : TCheckBox;
    FileConvDestinationPath     : TDirectoryEdit;
    FileConvDestinationListBox  : TListBox;
    FileConvSamePath            : TCheckBox;
    FileConvLowerCase           : TCheckBox;
    FileConvMakeFileName        : TCheckBox;
    FileConvOverWrite           : TCheckBox;
    FileConvNameMask            : TEdit;
    FileConvStartButton         : TButton;
    FileConvList                : TMemo;
    //-signal adapt tab
    ODconvTab                   : TTabSheet;
    ODConversionPanel           : TPanel;
    UseBackgroundValueLabel     : TLabel;
    UseDoseConvLabel            : TLabel;
    UseDoseFilmTypeLabel        : TLabel;
    UseDoseModalityLabel        : TLabel;
    UseSubtractLabel            : TLabel;
    UseBackGroundBox            : TGroupBox;
    UseDoseAddButton            : TButton;
    UseDoseDelButton            : TButton;
    //other elements
    StatusBar                   : TStatusBar;
    FileSaveDialog              : TSaveDialog;
    FileOpenDialog              : TOpenDialog;
    ColorDialog                 : TColorDialog;
    ApplicationProperties       : TApplicationProperties;
    //procedures and functions linked to GUI
    procedure FormCreate               (Sender         : TObject);
    procedure OnDataRead               (Sender         : TObject);                //BistroMath core function, call user dependent wellhofer function, fills graphics
    procedure AdjustHelpContext        (Sender         : TObject);                //linked to OnMouseEnter event for pages with different help contexts}
    procedure MeasMenuClick            (Sender         : TObject);
    procedure SetDefaultPanel          (Sender         : TObject);                //insert default panel display rules in results panel
    procedure SetCaption               (Sender         : TObject);      overload;
    procedure SelectConfig             (Sender         : TObject);                //selection of config file, implemented as application of fileopendialog
    procedure ConfigLoad               (Sender         : TObject);      overload;
    procedure ConfigSave               (Sender         : TObject);      overload;
    procedure SetWellhoferValues       (Sender         : TObject);                //set both global and twellhoferdata related values in wellhofer.pas
    procedure FormResize               (Sender         : TObject);
    procedure SettingsTabExit          (Sender         : TObject);
    procedure AdvancedSettingsTabExit  (Sender         : TObject);
    procedure ShowMenuItemStatus       (Sender         : TObject);
    procedure TopModelFunction         (const AX       : Double;
                                        out AY         : Double);
    procedure ViewItems                (Sender         : TObject);                //manage visibility of items on user input
    procedure UpdateSettings           (Sender         : TObject);                //passing user choices to various menu items
    procedure ClearScreen              (Sender         : TObject);                //clear graphics
    procedure ReadDroppedFiles         (Sender         : TObject;                 //uses DataFileOpen
                                        const FileNames: array of String);
    procedure Reload                   (Sender         : TObject);      overload;
    procedure ReadEditor               (Sender         : TObject);      overload; //read data from raw data tab and call ondataread
    procedure SyncSetExtSym            (Sender         : TObject);      overload; //manages submenu
    procedure SyncSetFFFpeak           (Sender         : TObject);      overload; //manages submenu
    procedure SyncSetNormalisation     (Sender         : TObject);
    procedure SyncSetCenterOfField     (Sender         : TObject);
    procedure SyncSetDetection         (Sender         : TObject);
    procedure SmartScaleElectronPDD    (Sender         : TObject);
    procedure FileOpenClick            (Sender         : TObject);                //uses DataFileOpen
    procedure FileOpenTempRefClick     (Sender         : TObject);                //uses TWellhoferData to open
    procedure FileSaveClick            (Sender         : TObject);
    {$IFDEF JwaWinBase}
    procedure OnIdleTimer              (Sender         : TObject);
    {$ENDIF}
    {$IFDEF form2pdf}
    procedure FilePrintFormClick       (Sender         : TObject);
    {$ENDIF}
    procedure FocusPositionPanelPaint  (Sender         : TObject);
    procedure FocusPositionProcessFile (Sender         : TObject);
    procedure FocusPositionRadioClick  (Sender         : TObject);
    procedure FocusPositionResetClick  (Sender         : TObject);
    procedure FocusPositionZeroClick   (Sender         : TObject);
    procedure FocusPositionChange_mm   (Sender         : TObject);
    procedure FocusPositionStateUpdate (Sender         : TObject);
    procedure MeasMoveClick            (Sender         : TObject);
    procedure CalcSubMenuClick         (Sender         : TObject);
    procedure SymCorrectClick          (Sender         : TObject);
    procedure UImodeChange             (Sender         : TObject);                //Enable parts of GUI on state and tab changes
    procedure ReferenceDevSpecClick    (Sender         : TObject);                //respond to RefDeviceSpecificItem
    procedure ReferenceGenericBeamClick(Sender         : TObject);
    procedure LocalPeakClick           (Sender         : TObject);                //limit twScanFirst/Last to area around peak
    procedure RightAxisToGridClick     (Sender         : TObject);                //align DataPlot right axis with grid set by left axis
    procedure ActivateZoom             (Sender         : TObject);
    procedure ActivateUnZoom           (Sender         : TObject);
    procedure DataPlotExtentChanged    (Sender         : TChart);                 //respond to DataPlot mouse zoom
    procedure MeasurementSaveClick     (Sender         : TObject);
    procedure OnMenu                   (Sender         : TObject);
    procedure EditEnter                (Sender         : TObject);                //AliasListEditor,EdgeMRlinacTUcsvList,ModListGrid,RawDataEditor,InventoryDirBox,FileConvSourcePath
    procedure HistoryListSizeClick     (Sender         : TObject);
    procedure ProcessSetTempRefClick   (Sender         : TObject);
    procedure ProcessUnsetTempRefClick (Sender         : TObject);
    procedure ProcessUpdateDataRead    (Sender         : TObject);
    procedure ProcessMergeSourceClick  (Sender         : TObject);
    procedure ProcessResetFitClick     (Sender         : TObject);
    procedure ProcessMirrorMeasRefClick(Sender         : TObject);
    procedure MeasCalcPDDfitClick      (Sender         : TObject);
    procedure PresetsMenuEnter         (Sender         : TObject);
    procedure PresetsItemClick         (Sender         : TObject);
    procedure UseDoseAddButtonClick    (Sender         : TObject);
    procedure UseDoseDelButtonClick    (Sender         : TObject);
    procedure PlotLabelClick           (Sender         : TObject);                //selects series
    procedure LabelCopyClick           (Sender         : TObject);
    procedure ConfigSaveAsItemClick    (Sender         : TObject);
    procedure Ionisation2DoseClick     (Sender         : TObject);
    procedure FitMubPowerFixedClick    (Sender         : TObject);
    procedure MeasReMappingStringChange(Sender         : TObject);
    procedure PageControlRequestChange (Sender         : TObject;
                                        var AllowChange: Boolean);
    procedure PageControlChange        (Sender         : TObject);
   {$IFDEF LCL_2-3_Up}
    procedure RightAxisGetMarkText     (Sender         : TObject;
                                        var AText      : String;
                                        AMark          : Double);
   {$ELSE}
    procedure AxisMarkToText           (var AText      : String;                  //introduce ability to change marks at will
                                        AMark          : Double );
   {$ENDIF}
    procedure FileConvPathBtnClick     (Sender         : TObject);
    procedure FileConvStartCheck       (Sender         : TObject);
    procedure FileConvStartClick       (Sender         : TObject);
    procedure FileConvDoFile           (Sender         : TObject;
                                        const AFileName: TFileName;
                                        const AFileInfo: TFileInfo);
    procedure FileConvNameMaskKeyPress (Sender         : TObject;
                                        var Key        : Char);
    procedure FileConvNameMaskEnter    (Sender         : TObject);
    procedure FileConvIteratorTerminate(Sender         : TObject);
    procedure InventoryPrepareCanvas   (Sender         : TObject;               //inventory is on files tab
                                        aCol, aRow     : Integer;
                                        aState         : TGridDrawState);
    procedure InventoryDoFile          (Sender         : TObject;
                                        const AFileName: TFileName;
                                        const AFileInfo: TFileInfo);
    procedure OnInventorySelect        (Sender         : TObject;
                                        aCol, aRow     : Integer);
    procedure InventoryGridDblClick    (Sender         : TObject);              //changed also at runtime
    procedure InventorySetRefDirClick  (Sender         : TObject);
    procedure InventoryDirBoxAccept    (Sender         : TObject;
                                        var Value      : String);
    procedure InventoryDirBoxChange    (Sender         : TObject);
    procedure AliasTabExit             (Sender         : TObject);
    procedure AliasListPrepareCanvas   (Sender         : TObject;
                                        aCol, aRow     : Integer;
                                        aState         : TGridDrawState);
    procedure AliasListDeleteClick     (Sender         : TObject);
    procedure AliasListInsertClick     (Sender         : TObject);
    procedure PDDFitResultsGridClick   (Sender         : TObject);
    procedure ModListAddClick          (Sender         : TObject);
    procedure ModListEditClick         (Sender         : TObject);
    procedure ModListUpdate            (Sender         : TObject);
    procedure ModListDelClick          (Sender         : TObject);
    procedure ModListGridSelectCell    (Sender         : TObject;
                                        ACol,ARow      : Integer;
                                        var CanSelect  : Boolean);
    procedure ModListRadioButtonClick  (Sender         : TObject);
    procedure FormKeyDown              (Sender         : TObject;               //user interface for keyboard (shortcuts)
                                        var Key        : Word;
                                        AShift         : TShiftState);
    procedure FormKeyUp                (Sender         : TObject;
                                        var Key        : Word);
    procedure FormKeyPress             (Sender         : TObject;
                                        var Key        : Char);
    procedure RunAboutBox              (Sender         : TObject);              //show about panel
   {$IFDEF SelfTest}
    procedure SelfTest                 (Sender         : TObject);
    procedure FormClose                (Sender         : TObject;
                                        var CloseAction: TCloseAction);
   {$ENDIF}
    procedure FormDestroy              (Sender         : TObject);
  public
    FileConvItemPatterns                               : ConvNameList;
    FileConvGeneralItems                               : ConvItemList;
    FileConvPhotonItems                                : ConvItemList;
    FileConvElectronItems                              : ConvItemList;
    Engines                                            : array of TWellhoferData; //a series of TWellhoferData-objects as historylist
    UsedEngine                                         : Integer;                 //engine in foreground
    UsedDataTopLine                                    : Integer;                 //pass datatop for multiple data sets in single profile data format file
    LoadEngine                                         : Integer;                 //last engine to load new data
    TempRefEngine                                      : Integer;                 //engine to take tempory reference from
    function  FileConvMakeName         (DefaultName    : String      ): String;
    function  DataFileOpen             (AFile          : String;
                                        ResetMultiScan : Boolean=True): Boolean;  //BistroMath core function
    procedure SetCaption               (Stg            : String='');             overload;
    procedure SetMessageBar            (Stg            : String;
                                        ALogLevel      : Integer=1);
    procedure ShowHistogram;
    procedure ShowFitResults;
    procedure SetBasicDefaults;
  private  //-------------------------------------------------------------------------
    //The series are created as arrays to get well-organised linked sets.
    CursorSeries          : array[PlotItems] of TLineSeries;                    //single point for each plotitem, created at runtime
    PlotSeries            : array[PlotItems] of TLineSeries;                    //the curves, created at runtime
    PlotLabels            : array[PlotItems] of TLabel;                         //labels on results panel, created at runtime
    PlotDates             : array[PlotItems] of TLabel;                         //labels on resultspanel, created at runtime
    PlotValues            : array[PlotItems] of TLabel;                         //value at cursor, created at runtime
    PenumbraSigmoids      : array[twcSides ] of TLineSeries;                    //penumbra model, created dynamically
    InFieldIndicators     : array[twcSides ] of TLineSeries;                    //IFA limits, created at runtime
    FFFSlopeLines         : array[twcSides ] of TLineSeries;                    //FFF slopes, created at runtime
   {$IFDEF form2pdf}
    FilePrintPageItem     : TMenuItem;                    {Ctrl+P}                      //OnClick = FilePrintFormClick
    FilePrintSelItem      : TMenuItem;                    {Ctrl+Shift+P}                //OnClick = FilePrintFormClick
    FilePrintAllItem      : TMenuItem;                    {Ctrl+Alt+P}                  //OnClick = FilePrintFormClick
   {$ENDIF}
    Ft_TypeLabel          : array[twcFieldClass                         ] of TLabel;     //Ft_xxx: used on FieldTypes tab         >|-
    Ft_DetectionCheckbox  : array[twcFieldClass                         ] of TCheckBox;  //SyncSetDetection                       >wFieldTypeDetection
    Ft_DetDiagonalCheckbox: array[twcFieldClass                         ] of TCheckBox;  //SyncSetDetection                       >wDiagonalDetection
    Ft_DynPenumbraCheckbox: array[twcFieldClass                         ] of TCheckBox;  //no synchonisation needed               >-
    Ft_SymCorrCheckbox    : array[twcFieldClass,dsMeasured..dsReference ] of TCheckBox;  //local sync with menu ondataread        >-
    Ft_EdgeMethodCombo    : array[twcFieldClass,twcEdgeClass            ] of TComboBox;  //SetWellhofervalues/GetWellhofervalues  >wEdgeMethod
    Ft_CenterMethodCombo  : array[twcFieldClass                         ] of TComboBox;  //SyncSetCenterOfField                   >wCenterDefinition
    Ft_NormMethodCombo    : array[twcFieldClass                         ] of TComboBox;  //SyncSetNormalisation                   >wNormalisation
    Ft_CenterRadius_cm    : array[twcFieldClass                         ] of TFloatSpinEditEx; //SetEnginevalues                  >wTopModelRadiusCm
    Ft_Default_SSD_cm     : array[twcFieldClass                         ] of TFloatSpinEditEx; //SetEnginevalues                  >wTopModelRadiusCm
    FP_RectTop            :                                                  Integer;          //value depends on top and height of FocusPositionInputGroupBox
    FP_BankLevel_Label    : array[twcBanks                              ] of TLabel;           //FocusPosition
    FP_BankLevel_mm       : array[twcBanks                              ] of TFloatSpinEditEx; //FocusPosition
    FP_BankFactor         : array[twcBanks                              ] of TLabel;           //FocusPosition
    FP_Center_button      : array[snGT..snAB   ,twcCardinalAngles       ] of TRadioButton;     //FocusPosition
    FP_Center_mm          : array[snGT..snAB   ,twcCardinalAngles       ] of TFloatSpinEditEx; //FocusPosition
    FP_Results_label      : array[InPlane..CrossPlane                   ] of TLabel;           //FocusPosition
    FP_Average_Titles     :                                                  TLabel;           //FocusPosition
    FP_Average_mm         : array[InPlane..CrossPlane,twcUpper..twcLower] of TLabel;           //FocusPosition
    FP_Dbfso_Title        :                                                  TLabel;           //FocusPosition
    FP_Drot_Title         :                                                  TLabel;           //FocusPosition
    FP_Calc_Dbfso_mm      : array[InPlane..CrossPlane                   ] of Double;           //FocusPosition
    FP_Dbfso_mm           : array[InPlane..CrossPlane                   ] of TLabel;           //FocusPosition
    FP_Calc_Drot_mm       : array[InPlane..CrossPlane                   ] of Double;           //FocusPosition
    FP_Drot_mm            : array[InPlane..CrossPlane                   ] of TLabel;           //FocusPosition
    GammaInFieldLimits    : array[twcFieldClass                         ] of TCheckBox;
    ExtSymSubItems        : array[ExtSymType                            ] of TMenuItem;
    FFFpSubItems          : array[CenterFFFTopModel..CenterFFFSlopes    ] of TMenuItem;
    SpecialMode           : array[1..NumSpecialModes                    ] of SpecModeRec;
    ShiftLabels           : array[twcMeasAxis                           ] of TLabel; //                                      >wAutoShiftCm
    ShiftValues_cm        : array[twcMeasAxis                           ] of TFloatSpinEditEx;
    CxResults             : array of CxLine;                                    //implementation PANEL DISPLAY RULES; binding to TPanelConfig.FElements; CxUsedRowMax=highest row number; see InitCxBlock, PublishResults
    UseDoseConvTable      : array of OD2dose_Rec;
    NextClipboardOwner    : THandle;                                            //BistroMath inserts iself in the chain of clipboardviewers
    SelectedPlot          : PlotItems;
    SelectedSeries        : TBasicChartSeries;                                  //this might be a series outside the plotitems
    FileIterator          : TFileIterator;
    SpecialModeValues     : TStringList;
    DetectedFileType      : twcFiletype;
    DoseConvTableNr       : Integer;
    EditorFilename        : String;
    EditorFileTime        : TDateTime;
    DataFromEditor        : Boolean;
    IndicatorsOk          : Boolean;
    CurveString           : String;
    ConfigName            : String;                                             //name of default configuration file
    IniSectionName        : String;                                             //name of section in ini files for this form
    CursorPos_cm          : twcFloatType;
    MeasNormAdjustFactor  : twcFloatType;                                       //last value of MeasNormAdjustEdit/100
    ZoomRange             : Single;
    PlotScaleMin          : Single;
    PlotScaleMax          : Single;
    SelectPlot            : Boolean;
    FFFfeatures           : Boolean;                                            //set in OnDataRead as wSource[dsMeasured].twFFFdetected
    FileConvSourceDir     : String;
    FileConvDestDir       : String;
    FileConvDestExt       : String;
    FileConvDestType      : twcFileType;
    FileConvOkCount       : Integer;
    FileConvMultCount     : Integer;                                            //counts found extra scans in multiscan file
    PlotPending           : Boolean;                                            //keep track of grapph plot activities
    PrevTab               : TTabSheet;
    DoseCLastRow          : Integer;
    DoseCLastModality     : String;
    DoseCLastFilmType     : String;
    DataChanged           : Boolean;
    HistogramSource       : twcDataSource;
   {$IFDEF SelfTest}
    SelftestLevel         : Byte;
   {$ENDIF}
    OnDataReadBusy        : Boolean;                                            //true if OnDataRead is triggered
    SyntheticMade         : Boolean;
    SwapAxis              : Boolean;
    PresetName            : String;
    PrevKey               : Char;
    ModMode               : ModModeType;
    ShiftStepCount        : Integer;                                            //count manual shift steps in same direction
    AdvancedModeOk        : Boolean;                                            //check if advancedmode items can be enabled in during startup
    MinClipBoardBytes     : Integer;                                            //ignore clipboard when too small
    ClipBoardLock         : Boolean;
    LastProfileZoomState  : Boolean;                                            //preserves the zoomstate for profiles when automatically is unzoomed for pdd's
    SelectedFitCol        : Integer;                                            //remember last clicked column in PDDFitResultsTab}
    FFFdataSource         : twcDataSource;                                      //maps on one of standard sources to plot TopModel
    FKeyboardReady        : Boolean;                                            //check with onkeyup event when there are no more keys to be processed
    InventoryListReady    : Boolean;                                            //See Files tab
    InventoryMultiScanList: Boolean;                                            //true if file with multiple scan is added to inventory
    InventoryReader       : TWellhoferData;
    AppliedFieldClass     : twcFieldClass;
    AppliedEdgeRefNorm    : twcDoseLevel;
    AppliedGammaScope     : twcGammaScope;
    ExtSym                : ExtSymType;
    SM2_Infotype          : Integer;                                            //infostring used for specialmode2  : 1=profile, 2=wedge, 3=DefaultSSD_cm, 4=pdd
    CxUsedLineMax         : Integer;                                            //actual number of elements per column (+1)
    PanelElements         : TPanelConfig;
    AxisAlignSource       : TCustomAxisChartSource;                             //alignment of left and right graph axis
    ActiveMemo            : TMemo;                                              //holds address of any of the gui memo's for saving to file
    WriteMenuShortCuts    : Boolean;                                            //if true shortcuts of the menu items are written into the configuration file
    ConfigRepairFound     : Boolean;
    LabelPlacingActive    : Boolean;                                            //prevent loops between FormResize and PlaceResultsLabels
   {$IFDEF JwaWinBase}                                                          //problematic on other platforms
    FLastIdleTime         : Int64;
    FLastKernelTime       : Int64;
    FPerformanceFrequency : Int64;
    FLastUserTime         : Int64;
    FIdleTimer            : TIdleTimer;
   {$ENDIF}
   {$IFDEF SelfTest}
    SelfTestItem          : TMenuItem;
   {$ENDIF}
   {$IFDEF THREAD_FILES}
    ScanListThread        : THelpEventThread;
   {$ENDIF THREAD_FILES}
   {$IFDEF PRELOAD}
    {when data are read directly, they are sent to the editor in a separate thread}
    PreLoadStream         : TStringStream;                                      //In FPC 3.2.0 TStringSream is descendant of TMemoryStream; the latter was completely separate <= FPC 3.0.4
    procedure PreloadTransfer(Sender       :TObject                );
   {$ENDIF PRELOAD}
   {when data are read directly, they are sent to the Editor in a separate thread}
    function  AddEngine(ForceExpand        :Boolean=False          ): Integer;
    procedure SetEngineValues(aEngine      :Integer                );
    function  SelectEngine(aEngine         :Integer;
                           aShift          :Integer=0;
                           Synchronise     :Boolean=True           ): Integer;
    procedure SetHistoryListSize(NewLength :Word                   );
    function  PassRefOrg(ReceivingEngine   :Integer                ): Boolean;
    function  LinkPlotItem(aSource         :twcDataSource          ): PlotItems;
    procedure InitCxBlock(NewLineMax       :Integer                );
    procedure ClearAllCx(MakeVisible       :Boolean=False          );           //Clear all captions from values of panel rules related visual objects; fill labels with ID's
    procedure SetConfigName(AName          :String;
                           TestFile        :Boolean=False          );
    function  PresetToName(AFileName       :String;
                          IncludeExtension :Boolean=False          ): String;
    function  NameToPreset(AName           :String;
                          IncludeExtension :Boolean=False          ): String;
    procedure PresetLoad(AFileName         :String                 );           overload;
    procedure PresetLoad(CF                :TConfigStrings         );           overload;
    function  ConfigRepair(CF              :TConfigStrings         ): Boolean;
    procedure ConfigLoad(Sender            :TObject;
                         AFileName         :String;
                         ForceInit         :Boolean=False          );           overload;
    procedure ConfigLoad(AStream           :TStream                );           overload;
    procedure ConfigSave(Sender            :TObject;
                         AFileName         :String;
                         PresetsOnly       :Boolean=False          );           overload;
    procedure ConfigSave(AStream           :TStream;
                         AFileName         :String='';
                         PresetsOnly       :Boolean=False          );           overload;
    function  CheckWellhoferReady                                   : Boolean;
    procedure Engine2Editor(ASource        :twcDataSource=dsMeasured;
                           ReloadReference :Boolean=False          );
    procedure PublishValue(ALabel          :TLabel;
                           AValue          :Single;
                           AColor          :TColor=clBlack;
                           Decimals        :Integer=2;
                           AUnit           :String=''              );           overload;
    procedure PublishValue(ABlock          :CxBlock;
                           AValue          :Single;
                           AColor          :TColor=clBlack;
                           Decimals        :Integer=2;
                           AUnit           :String=''              );           overload;
    procedure FocusPositionSetIndicators;
    procedure FocusPositionSquarePaint(Id  :Integer=-1             );
    function  FocusPositionAxisValue(AScan :twcMeasAxis;
                                     AAngle:twcCardinalAngles      ): Double;
    function  ScanInclusionTest(aEngine    :Integer                ): Boolean;
    procedure PublishResults;                                                   //evaluate and display all panel rules ****BistroMath core function
    procedure PlaceResultsLabels(ASize     :ShortInt=0             );           //adjusts font size to available window size
    function  EvaluateInfoRecord(var ARec  :ResultsInfoRecord      ): twcFloatType;
    function  EvaluateResultText(ACommaText:String;
                                 Decimals  :Integer=EvalDecimals   ): String;
    function  GetDisplayedPositionScale(cm :twcFloatType=1         ): twcFloatType;
    function  DataPlotZoomed                                        : Boolean;
    procedure DataPlotUnZoom;
    function  GetPositionUnitsStg                                   : String;
    procedure GetWellhoferValues;
    procedure AutoZoom(FullAuto            :Boolean=True           );
    procedure SourceAxisSync;                                                   //updates axis settings for engine, based on information from GUI
    procedure PlotCursor(Sender            :TObject                );
    procedure PlotIndicators;
    procedure SetPlotDate(APlotItem        :PlotItems;
                          AString          :String;
                          Composite        :Boolean=False          );
    procedure DoSpecialMode2;
    procedure FileSave(ACurve              :PlotItems;
                       Filtered            :Boolean=False          );
    function  SetFileType(AExtension       :String                 ): twcFileType;
    procedure FillCheckListCombo;
    procedure DataEditorAddLine(ALine      :String                 );
    procedure MemoSaveToFile(AMemo         :TMemo;
                             AFileName     :String=''              );
    procedure Reload(Sender                :TObject;
                     DoClearScreen         :Boolean                );           overload;
    procedure ReadEditor(Sender            :TObject;
                         DoClearScreen     :Boolean                );           overload;
    procedure SyncSetExtSym(AExtSym        :ExtSymType             );           overload; //manages submenu
    procedure SyncSetFFFpeak(APeakDef      :twcFFFPeakType         );           overload; //manages submenu
    function  DataEditorOpenFile(AFileName :String                 ): Boolean;
    procedure InventoryAddFile;                                                           //files tab
    procedure InventorySort(AColumn        :Integer=0              );                     //files tab
    function  InventoryReaderSetup(irCreate:Boolean=True           ): Boolean;
    procedure ThreadSaveFillPlot(ASeries   :PlotItems;                                    //graph filling for single and multiple threads
                                 ASource   :twcDataSource;
                                 AScaling  :twcFloatType;
                                 AnalRange :Boolean=False          );
    procedure OnSeriesSelected(AMouseButton:TMouseButton=mbLeft    );
    procedure ControlsEnable(AControl      :TControl;                                     //menu management
                             Enable        :Boolean                );
    procedure EnableMenuSystem(AEnabled    :Boolean                );                     //menu management
    procedure EnableMenu(AMenu             :TMenuItem;
                         AEnabled          :Boolean                );
    function  FileConvNameCheck                                     : Boolean;            //conversion tab
    function  FileConvGetFileExt(AListBox  :TlistBox               ): String;             //conversion tab
    procedure FileConvFileNameDisplay(AEdit:TDirectoryEdit;                               //conversion tab
                                      AName:TFileName              );
    procedure PopulateDoseConvList(Index   :Integer                );                     //signal adapt tab
    procedure ModListGridFill(ACommaText   :String;                                       //configuration tab
                              AddToModData :Boolean=False          );
    procedure ExceptMessage(AMessage       :String                 );
    procedure InventoryDBChgAction(Sender  :TObject                );
    procedure InventoryDBChgComplete(Sender:TObject                );
    procedure ChangeZPosition(ASeries      :TBasicChartSeries;                           //graph series z-ordering
                              MoveUp       :Boolean=True           );
    {$IFDEF JwaWinBase}
    procedure GetCpuUsage(Display          :Boolean     =True;
                          MinReset_sec     :twcFloatType=1         );
    {$ENDIF}
    {$IFDEF Windows}                                                                     //problematic on other platforms
    function  HelpHandler(Command          :Word;
                          Data             :PtrInt;                                      //PtrInt is used in the THelpEvent (forms.pp) and avoids platform dependencies
                          var CallHelp     :Boolean                ): Boolean;
    function  ExecuteHelp(Data             :PtrInt;
                          Command          :Word=HH_HELP_CONTEXT   ): Boolean;
    {$ENDIF}
  protected
    {$IFDEF THREAD_FILES}
    procedure WMADDINVENTORY (var Msg:TLMessage);    message LM_USER;                    //multithreaded binding of output of file search to InventoryAddFile
    {$ENDIF THREAD_FILES}
    {$IFDEF Windows}
    procedure WMChangeCBChain(AwParam      :WParam;
                              AlParam      :LParam                 );                    //https://wiki.lazarus.freepascal.org/Clipboard
    procedure WMDrawClipboard(AwParam      :WParam;
                              AlParam      :LParam                 );
    {$ENDIF}
  end;

var
  AnalyseForm: TAnalyseForm;

implementation

{$R *.lfm}

uses {$IFDEF Windows}
      Windows,
     {$ENDIF Windows}
     {$IFDEF JwaWinBase}
      JwaWinBase,                                                               //GetSystemTimes (see GetCpuUsage)
     {$ENDIF JwaWinBase}
     {$IFDEF Unix}
      LCLintf,
     {$ENDIF}
      StrUtils, Math, MD5, Clipbrd, Keyboard, DateUtils,
      lclproc, IniFiles, lazFileUtils,
      TAChartUtils, TAChartAxis, TACustomSeries,
      TOfile, TOtools, TOmath,
     {$IFDEF form2pdf}
      form2pdf,
     {$ENDIF}
      TObaseDef,
      KlemLogo;

const DefAppName           ='BistroMath';
      DefMaxFlatness       =   0.5;
      ConvTabMargin        =   5; {pixels}
      ConvDefMask          ='{X:SEEFddd}{E:SEEllddd}';
      DefLabelDmax         = 'dmax';
      DefDoseGridModCol    =   0;
      DefDoseGridFilmCol   =   1;
      DefNormGridTypeCol   =   5;
      DefNormGridLinacCol  =   6;
      DefErrorLimit        = 9e8;
      DefZoomRange         =   1.03;
      DefAxisMaxExtension  =   1.05;                                            //extension of graph axis to accommodate head room
      DefBgLinesOfs        =  10;
      DefBgLinesStep       =  30;
      DefWedgeCol          = clTeal;
      DefCenteredCol       = clGreen;
      DefEstimatedCol      = clRed;
      DefSymCorrected      = clBlue;
      ZoomText             = 'ZoomRange';
      AliasText            = 'Alias';
      DefaultName          = 'default.txt';
      DefInstituteCfg      = 'institute.ini';
      DefInventoryListReady= 'List completed';
      DefComments          = 'Comments';
      DefSpecialMode       = '-specialmode';
      DefLogLevel          = '-loglevel';
      DefConfigFile        = '-config';
      DefRefPath           = '-references';
      DefDataPath          = '-data';
      DefPreset            = '-preset';
      DefAdvanced          = '-advanced';
      DefMultiScanSep      = '>';
      DefShiftLeft         = '<';
      DefShiftRight        = '>';
      ExtraVersionInfo     = '';
      DefLogMaxLines       ='logmax';
      DefXsourceSelectors  ='MCRB';
      DefNumValueChars     =  9;
      DefChartAxL          =  0;                                                //These values are only valid in standard setup of chart
      DefChartAxB          =  1;
      DefChartAxR          =  2;                                                //DefChartAxT=3;
      DefMinFPCbuild       =755;
      DefConfigRepairFile  ='BM780_renamed_elements.ini';
      XtypeFilter          = csNumeric+[EmptyXtype];
      AxisRotationMapping: array[InPlane..CrossPlane,twcCardinalAngles] of AxisMapValue = (((1,0),(0,1),(-1,0),(0,-1)),((0,1),(-1,0),(0,-1),(1,0)));
      PlotSeriesColors   : array[PlotItems] of TColor       = (clRed,clBlue,clGreen,clMaroon{,clNavy});
      PlotDataMapping    : array[PlotItems] of twcDataSource= (dsMeasured,dsCalculated,dsReference,dsBuffer);
      DefCenterAnnot     : array[twcPositionUseType] of Char= ('b','e','i','s','o','m','T','S','u','c'); {dUseBorder,dUseDerivative,dUseInflection,dUseSigmoid50,dUseOrigin,dUseMax,dUseFFFtop,dUseFFFslopes,dUseUndefined)}
      DefConvItemStrings : ConvNameList                     = ('{' ,'X:','E:','}' ,'S' ,'EE','eee','DD','ddd','M',
                                                              'LL','XX','YY','ll','xx','yy','F'  ,'I' ,'_');

{TWindowsVersion = (Win16_31    , Win32_95 , Win32_95R2 , Win32_98     , Win32_98SE,Win32_ME,
                    Win32_Future, WinNT_31 , WinNT_35   , WinNT_351    ,
                    WinNT_40    ,
                    WinNT5_2000 , WinNT5_XP, WinNT5_2003, WinNT5_Future,
                    WinNT6_Vista, WinNT7   , WinNT8     , WinNT_Future , Win_Future)}

resourcestring
      DefMirrorText     ='mirrored';
      DefSymCorrText    ='symmetric';
      DefDepthText      ='depth=%0.1f';
      DefSavedText      ='%s saved';
      DefCenterText     ='Center';
      DefTopText        ='Top';
      DefWidthText      ='Width';
      DefLeftText       ='Left';
      DefRightText      ='Right';
      DefNormValText    ='Normalisation';
      DefNormPosText    ='Norm position';
      DefAnnotShift     ='~';                                                   {changed 24/01/2020}
      ModifiedText      ='%s is modified by the user and therefore not saved';
      SameFileText      ='%s: pathname equals source file; cannot be saved';
      MayneordItemText  ='May&neord transformation to SSD=%0.1f';
      BadPenumbraText   ='Penumbra too wide';
      PosLabelPosText   ='Position';
      PosLabelDepthText ='Depth';
      TempRefText       ='Temporary reference: %s %s';
      WritingMessage    ='writing %s...';
      CurveStringsDif   ='The reference (%s » %s) differs from source (%s).';
      DefInventoryTitles='File,Scans,Linac,Energy,Field,Type,Depth,Date';
      DefConvInTypes    ='"OmniPro|SN|plain text (*.txt)","PTW mcc (*.mcc)","RFB (*.rfb)","WMS ascii (*.wtx)","WMS binary (*.wda)","RFA300|w2CAD (*.asc)","SN (*.snctxt)","hdf-1D (*.hdf)"';
      DefConvOutTypes   ='"OmniPro (*.txt)","PTW mcc (*.mcc)","WMS ascii (*.wtx)","WMS binary (*.wda)","RFA300 (*.asc)"';
      DefSubtracted     ='background subtracted';
      DefComposite      ='composite';
      Def2Dose          ='DoseConv';
      DefModListEdit    ='Edit';
      DefModListReady   ='Ready';
      DefPlotNames      ='Measurement,Calculation,Reference,Buffer,Combined';
      DefExtSymSubMenu  ='&Extended symmetry (%s)...';
      DefCoFDefaults    ='B,O,M,O,B,O';                                         //twcMeasType list
      DefNormDefaults   ='C,O,M,O,C,O';                                         //twcMeasType list
      DefExtSymSubNames ='ExtSymLEItem,ExtSymARItem,ExtSymLiItem';
      DefExtSymTexts    ='&Linac Error,&Area Ratio,&Elevation';
      DefFFFpSubMenu    ='FFF Peak (%s)...';
      DefFFFpSubNames   ='MeasFFFpTopItem,MeasFFFSlopesItem';
      DefFFFpSubTexts   ='&Top Model,&Slopes';
      DefFFFpSubKeys    ='T,S';
      DefModFilmKeys    ='Modality,Film,a,b,c,d,e,f';
      DefModNormKeys    ='Modality,RelDepth,RelValue,AbsDepth,AbsValue,"Field Types",Linacs';
      DefModBeamKeys    ='Modality,Linac';
      DefDoseFilmValues ='X0.0,none,0,1,0,0,0,0';
      DefDoseNormValues ='X0.0,0,100,0,100';
      DefDoseBeamValues ='X0.0,LinacA';
      DefRefSubMenu     ='&Calculation (%s)...';
      DefFlatness       ='. flatness';
      DefSymNCS         ='Symmetry';
      DefMaximumText    ='Maximum';
      DefMinimumText    ='Minimum';
      DefInFieldAreaTxt =' In-Field area';
      DefAxisViewColl   ='coll=%d°';
      DefAliasInfo      ='keys: <Insert>, <Ctrl+Delete>; submenu: right-click';
      ConvOkText        ='Ok';
      ConvErrText       ='read error (%s)';
      ConvResultText    ='%d files converted, %d failures';
      ConvNoFilesText   ='No files found';
      ConvExistText     ='%s already exists';
      ConvErrKeyText    ='Invalid key "%s"';
      ConvSyntax01      ='Syntax\tValue\t\tResult';
      ConvSyntax02      ='{ | }\tnone\t\tgrouping';
      ConvSyntax03      ='X: | E:\tgroup label\tphotons | electrons (optional)';
      ConvSyntax04      ='S\tscantype\t\tI\t\t(Inplane, GT)';
      ConvSyntax05      ='\t\t\tC\t\t(Crossplane, AB)';
      ConvSyntax06      ='\t\t\tD\t\t(PDD)';
      ConvSyntax07      ='\t\t\tB\t\t(Fanline)';
      ConvSyntax08      ='\t\t\tF\t\t(Freescan)';
      ConvSyntax09      ='\t\t\tP\t\t(Plane)';
      ConvSyntax10      ='EE\tEnergy [MV/MeV]\t2 digits';
      ConvSyntax11      ='eee\tEnergy [kV/keV]\t3 digits';
      ConvSyntax12      ='M\tModality\t\tX | E | O';
      ConvSyntax13      ='DD\tProfile depth [cm]\t2 digits';
      ConvSyntax14      ='ddd\tProfile depth [mm]\t3 digits';
      ConvSyntax15      ='LL|XX|YY\tField Size [cm]\t2 digits';
      ConvSyntax16      ='ll|xx|yy\tField Size [mm]\t3 digits';
      ConvSyntax17      ='F\tField type\t\tW | O';
      ConvSyntax18      ='I\tDetector\t\t(free text)';
      ConvSyntax19      ='_\tfield separator\t(literal)';
      ConvSyntax99      ='example';

{$IFDEF Windows}
var   PrevWndProc: windows.WNDPROC;                                             //https://wiki.lazarus.freepascal.org/Clipboard | size depends on platform
{$ENDIF}


function DCname(NameIndex,Nr:Integer): String;
const DCnames: array[0..4] of String = ('DC_Dose','DC_Bg','DC_Film','DC_Mod','DC_Edit');
begin
if InRange(NameIndex,0,4) then
  Result:= DCnames[NameIndex]+'_'+Num2Stg(Abs(Nr),2,'0')
else
  Result:= '';
end; {dcname}


{06/10/2020 delphi fundamentals alternative}
{20/10/2020 trim not good enough}
function CleanUpCaption(ACaption:String): String;
begin
Result:= ACaption.Replace('&','',[rfReplaceAll]);
end; {cleanupcaption}


{$IFDEF THREADED}
//----------THelpThread------multithreading-------------------------------------

constructor THelpPlainThread.Create(APlainProc  :htObjectPlainProc;
                                    FreeWhenDone:Boolean=True);
begin
inherited Create(False);
FreeOnTerminate:= FreeWhenDone;
Priority       := tpHigher;
FPlainProc     := APlainProc;
end; {~create}


procedure THelpPlainThread.Execute;
begin
if assigned(FPlainProc) then FPlainProc;
end; {~execute}


{$IFDEF THREAD_PLOT}
//each curve is filled throug a separate thread
{12/04/2022 FAnalyse,UseAnalysisRange}
constructor THelpFillThread.Create(AFillProc       :htObjectFillProc;
                                   ASeries         :PlotItems;
                                   ASource         :twcDataSource;
                                   AScaling        :twcFloatType;
                                   UseAnalysisRange:Boolean=False;
                                   FreeWhenDone    :Boolean=True);
begin
inherited Create(False);
FreeOnTerminate:= FreeWhenDone;
Priority       := tpHigher;
FFillProc      := AFillProc;
FSeries        := ASeries;
FSource        := ASource;
FScaling       := AScaling;
FAnalyse       := UseAnalysisRange;
end; {~create}


{12/04/2022 FAnalyse}
procedure THelpFillThread.Execute;
begin
if assigned(FFillProc) then FFillProc(FSeries,FSource,FScaling,FAnalyse);
end; {~execute}
{$ENDIF THREAD_PLOT}


{$IFDEF THREAD_FILES}
//inventory in separate thread
constructor THelpEventThread.Create(AEventProc  :htObjectEventProc;
                                    ASender     :TObject;
                                    FreeWhenDone:Boolean=True);
begin
inherited Create(False);
FreeOnTerminate:= FreeWhenDone;
Priority       := tpHigher;
FEventProc     := AEventProc;
FSender        := ASender;
end; {~create}


procedure THelpeventThread.Execute;
begin
if  assigned(FEventProc) then FEventProc(FSender);
end; {~execute}
{$ENDIF THREAD_FILES}
{$ENDIF THREADED}


{****************************** TAnalyseForm ********************************************}
{$IFDEF Windows}
//needed for insertion in the windows clipboard chain
function WndCallback(Ahwnd:HWND; uMsg:UINT; wParam:WParam; lParam:LParam): LRESULT; stdcall;
begin
if uMsg=WM_CHANGECBCHAIN then
  begin
  AnalyseForm.WMChangeCBChain(wParam,lParam);
  Result:= 0;
  Exit;
  end
else if uMsg=WM_DRAWCLIPBOARD then
  begin
  AnalyseForm.WMDrawClipboard(wParam,lParam);
  Result:= 0;
  Exit;
  end;
Result:= CallWindowProc(PrevWndProc,Ahwnd,uMsg,WParam,LParam);
end; {wndcallback}
{$ENDIF}


(*
FormCreate
initialisations
a signifcant part of the user interface is built at runtime to accommodate for much more efficient coding
*)
{21/07/2015 helpfile defined}
{22/07/2015 set default to mayneordssd2numedit when zero after loadconfig}
{28/07/2015 autosize of plotdates set to false}
{11/12/2015 static LeffSeries and ReffSeries replaced with InFieldIndicators}
{06/01/2016 ErrorSeries.Tag set}
{12/02/2016 preloadstream, preloadtransfer}
{30/03/2016 different listbox defs for FileConvSourceListBox and FileConvDestinationListBox}
{09/06/2016 replaced SpecialModes boolean Active with TMenuItem MenuItem, incorporated in Processingmenu}
{28/06/2016 SelectedFitCol}
{05/07/2016 added FFFcenter submenu}
{22/07/2016 added Center of Field submenu}
{29/07/2016 removed FFFcenterDefault from MeasPeakFFFSubMenu}
{04/11/2016 UseDoseAddButtonClick}
{08/11/2016 ModMode}
{25/12/2016 InventoryReader}
{19/01/2017 SetConfigName(ParamStr(Succ(i)),True) (using testfile feature)}
{08/06/2017 sha256 checksum put on messagebar}
{17/01/2018 Delimiter}
{22/01/2018 CxBlock init in separate procedure to allow reinit}
{03/07/2018 specialmode1: message if file does not exist}
{12/10/2018 NormAdjustFactor}
{25/10/2018 SpecialModeValues}
{18/01/2019 FileIterator}
{09/01/2019 check on decimal separator}
{23/01/2020 negative value of addmode clears all rules}
{--port to Lazarus--}
{17/03/2020 RawDataEditor}
{16/04/2020 tagging of series simplified}
{19/05/2020 Wellhofer.StatusProcedure:= @SetMessageBar}
{02/06/2020 WriteMenuShortCuts}
{03/07/2020 removed SavedIgnoreState}
{05/07/2020 FilePrintAllItem,FilePrintPageItem}
{12/07/2020 EdgeMethodCombo}
{15/07/2020 GammaInFieldLimits,AppliedEdgeRefNorm}
{19/07/2020 MeasCenterSmallSubMenu,MeasNormSmallSubMenu}
{20/07/2020 field types}
{21/07/2020 fcWedge}
{25/05/2020 names of submenu items, defaults added}
{28/07/2020 Ft_XXXX[twcFieldClass] elements}
{02/08/2020 HelpContext:= Parent.HelpContext}
{25/08/2020 no limit on number of lines in panelelements, init CxResults}
{26/08/2020 added ConfigRepairFound}
{27/08/2020 added Ft_CenterRadiusEdit_cm}
{29/08/2020 LabelPlacingActive}
{03/09/2020 Ft_DynPenumbraCheckbox}
{14/09/2020 Engines}
{16/09/2020 multiple items for SpecialMode1}
{26/09/2020 PenumbraSigmoids}
{29/09/2020 TempRefEngine}
{17/11/2020 UsedDataTopLine}
{02/03/2021 FFFfeatures}
{03/03/2020 added Ft_DetDiagonalCheckbox, removed MeasDetectDiagItem}
{15/03/2021 added Ft_Default_SSD_cm}
{16/02/2022 specialmode1 support for filepath on other drive}
{15/10/2022 init FP_Center_button and FP_Center_mm}
{16/10/2022 fill FocusPosition_IEC_Convention}
{18/10/2022 fill FocusPosition_mapping_Box}
{28/10/2022 init FP_Results_label,FP_Average_mm}
{29/10/2022 init FocusPosition_UpperBLD_ident}
{04/11/2022 init FP_Results_Titles,FP_Dbfso_mm,FP_Drot_mm}
{08/11/2022 init FP_BankLevels_Label,FP_BankLevel_mm}
{28/11/2022 alignment FocusPositionResetButton,FocusPositionZeroFPButton,FocusPositionZeroRotButton}
{09/12/2022 FP_BankFactor}
procedure TAnalyseForm.FormCreate(Sender: TObject);
var k         : PlotItems;
    i,j       : LongInt;
    TempPath  : String;
    aa,ab,ac  : array of String;
    f         : twcFieldClass;
    m         : twcFFFPeakType;
    n         : twcNormalisation;
    o         : twcCenterType;
    e         : ExtSymType;
    side      : twcSides;
    bank      : twcBanks;
    p         : twcDoseLevel;
    q         : twcEdgeClass;
    sidechar  : Char;
    s         : twcDataSource;
    angle     : twcCardinalAngles;
    iec       : twcIECdefs;
    x         : twcMeasAxis;
    scan      : twcScanTypes absolute x;

  {fpc implementation}
  function MakeChecksum: String;
  begin
  Result:= MDprint(MD5file(ParamStr(0),500000));
  end;

  (*
  InitHelperLine
  Initial settings for all additional indicators (in-fiels area, topmodel, FFF-slopes, penumbra sigmoids)
  *)
  procedure InitHelperLine(ASeries      :TLineSeries;
                           AName        :String;
                           APenWidth    :Integer=1;
                           APenColor    :TColor =clYellow and $00aaaaaa);
  begin
  DataPlot.AddSeries(ASeries);
  with ASeries do
    begin
    AxisIndexX   := DefChartAxB;
    AxisIndexY   := DefChartAxL;                                                //make assignments to axis (and scales!) explicite
    Active       := False;
    ShowPoints   := False;
    ShowLines    := True;
    ShowInLegend := False;
    SeriesColor  := APenColor;
    ColorEach    := ceNone;
    LinePen.Color:= SeriesColor;
    LinePen.Style:= psDot;
    LinePen.Width:= APenWidth;
    Name         := AName;
    end;
  end;

 {$IFDEF form2pdf}
  procedure AddPrintItem(var AMenuItem           :TMenuItem;
                         const ACaption,AShortCut:String);
  begin
  AMenuItem         := TMenuItem.Create(FileMenu);
  AMenuItem.Caption := Format('Print %s to pdf',[ACaption]);
  AMenuItem.OnClick := @FilePrintFormClick;
  AMenuItem.ShortCut:= TextToShortCut(AShortCut);
  FileMenu.Insert(FileMenu.IndexOf(FileExitItem),AMenuItem);
  end;
 {$ENDIF}

  procedure FatalError(AMessage:String);
  begin
  MessageDlg(Format('%s %s and will exit now.',[DefAppName,AMessage]),mtInformation,[mbOk],0);
  Application.Terminate;
  end;

begin
inherited;
if DefaultFormatSettings.DecimalSeparator<>'.' then                             //BistroMath is very strict on the decimal separator
  FatalError('needs "." (period) as decimal separator in the regional settings');
{$IFDEF PRELOAD}
PreLoadStream             := TStringStream.Create('');
{$ENDIF PRELOAD}
CommonAppData             := AppendPathDelim(GetCommonAppdataRoot);             //TOtools.pas
SetLength(Engines,1);                                                           //we start with one engine
UsedEngine                :=  0;
LoadEngine                :=  0;
Engines[UsedEngine]       := TWellhoferData.Create;                             //primary analysis engine
UsedDataTopLine           :=  0;
TempRefEngine             := -1;
PanelElements             := TPanelConfig.Create(CxMaxCol,0);                   //the visual results panel elements, created dynamically
ClipBoardLock             := True;                                              //keep clipboard locked during initialisation
CxUsedLineMax             := 0;                                                 //early initialisation needed
LabelPlacingActive        := False;                                             //prevent loops between FormResize and PlaceResultsLabels
SetLength(CxResults,CxUsedLineMax);                                             //initialise cxresults to zero lines
{$IFDEF JwaWinBase}
StatusBar.Panels.Add;                                                           //extra panel for cpu usage
{$ENDIF}
{$IFDEF Windows}
//https://wiki.lazarus.freepascal.org/Win32/64_Interface#Processing_non-user_messages_in_your_window
{$push}{$warn 4055 off: Conversion between ordinals and pointers is not portable and will not compile on 64 bits platform}
PrevWndProc       := Windows.WNDPROC(SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrUInt(@WndCallback)));
{$pop}
NextClipboardOwner:= SetClipboardViewer(Self.Handle);
{$ENDIF}
FileIterator:= TFileIterator.Create(Self);                                      //fileiterator has multiple uses
with FileIterator do
  begin
  Options     := [fiPath,fiRecurseFolders];
  RelativePath:= False;
  RootFolder  := ExtractFileDrive(ParamStr(0));
  end;
for side:= twcLeft to twcRight do                    {-----------create indicators graph object-------------------------}
  begin
  if side=twcLeft then sidechar:= 'L'
  else                 sidechar:= 'R';
  InFieldIndicators[side]      := TLineSeries.Create(Self);                     //in-field indicators
  FFFSlopeLines[side]          := TLineSeries.Create(Self);                     //slope model for FFF
  PenumbraSigmoids[side]       := TLineSeries.Create(Self);                     //sigmoid shapes
  InitHelperLine(InFieldIndicators[side],'AnalysisLimit_'+sidechar,1);
  InitHelperLine(FFFSlopeLines[side]    ,'FFFslope_'     +sidechar,2);
  InitHelperLine(PenumbraSigmoids[side] ,'Penumbra_'     +sidechar,3,clSkyBlue);
  end;
for k:= Low(PlotItems) to High(PlotItems) do         {-----------curve graph objects: labels, dates, plotvalues, plotseries, cursorseries----}
  begin
  PlotLabels[k]:= TLabel.Create(Self);               {-----------plotlabels, parent is resultspanel--------------}
  with PlotLabels[k] do                                                         //position and size set in PlaceResultsLabels, called by initcxblock
    begin
    Parent     := ResultsPanel;
    HelpContext:= Parent.HelpContext;
    Alignment  := taRightJustify;
    AutoSize   := False;
    ParentFont := True;
    HelpContext:= ResultsPanel.HelpContext;
    Tag        := Ord(k);                                                       //extra linking through tag
    end;
  PlotDates[k]:= TLabel.Create(Self);               {-----------plotdates[k], parent is resultspanel------------}
  with PlotDates[k] do
    begin
    Parent     := ResultsPanel;
    HelpContext:= Parent.HelpContext;
    Alignment  := taLeftJustify;
    AutoSize   := False;
    Font.Color := PlotSeriesColors[k];                                          //colors are predefined in array
    HelpContext:= ResultsPanel.HelpContext;
    Onclick    := @PlotLabelClick;                                              //In objfpc mode, the explicit @ is required to disambiguate some shady cases.
    Tag        := Ord(k);                                                       //extra linking through tag
    end;
  PlotValues[k]:= TLabel.Create(Self);               {----------plotvalues[k], parent is resultspanel------------}
  with PlotValues[k] do
    begin
    Parent     := ResultsPanel;
    HelpContext:= Parent.HelpContext;
    Alignment  := taRightJustify;
    AutoSize   := False;
    Caption    := '-';
    Font.Color := PlotSeriesColors[k];                                          //colors are predefined in array
    OnClick    := @LabelCopyClick;
    HelpContext:= ResultsPanel.HelpContext;
    Tag        := Ord(k);                                                       //extra linking through tag
    end;
  PlotSeries[k]:= TLineSeries.Create(Self);          {---------plotseries[k], added to dataplot-------------------}
  with PlotSeries[k] do
    begin
    AxisIndexX         := DefChartAxB;
    AxisIndexY         := DefChartAxL;
    Active             := True;
    Marks.Visible      := False;
    SeriesColor        := PlotSeriesColors[k];                                  //colors are predefined in array
    ShowInLegend       := False;
    LineType           := ltFromPrevious;
    ShowLines          := True;
    LinePen.Color      := SeriesColor;
    Pointer.Pen.Color  := SeriesColor;
    Pointer.Brush.Color:= SeriesColor;
    Pointer.HorizSize  := 1;
    Pointer.VertSize   := 2;
    Name               := twcDataSourceNames[PlotDataMapping[k]];
    Tag                := Ord(k);                                               //extra linking through tag
    end;
  DataPlot.AddSeries(PlotSeries[k]);
  CursorSeries[k]:= TLineSeries.Create(Self);        {--------cursorseries[k], added to dataplot------------------}
  with CursorSeries[k],Pointer do
    begin
    AxisIndexX := DefChartAxB;
    AxisIndexY := DefChartAxL;
    SeriesColor:= clBlack;
    Visible    := True;
    ShowLines  := False;
    LineType   := ltNone;
    HorizSize  := 3;
    VertSize   := 3;
    Pen.Width  := 0;
    Name       := 'cursor_'+PlotSeries[k].Name;
    Tag        := Ord(k);                                                       //extra linking through tag
    end;
  DataPlot.AddSeries(CursorSeries[k]);
  end; {for k}
aa:= String(DefPlotNames).Split(',');                                           //plotseries and label names
for k:= Low(PlotItems) to High(PlotItems) do
  begin
  PlotSeries[k].Title  := aa[Ord(k)];
  PlotLabels[k].Caption:= aa[Ord(k)]+':';
  end;
{$IFDEF form2pdf}                                                               //form2pdf
if FormToPDF=0 then
  begin
 {$IFDEF form2pdfcurrent}
  AddPrintItem(FilePrintPageItem,'Active tab','Ctrl+P');
 {$ENDIF}
 {$IFDEF form2pdfsel}
  AddPrintItem(FilePrintSelItem,'Selected tabs','Ctrl+Shift+P');
 {$ENDIF}
 {$IFDEF form2pdfall}
  AddPrintItem(FilePrintAllItem,'All tabs','Ctrl+Alt+P');
 {$ENDIF}
  end;
{$ENDIF}
{$IFDEF SelfTest}                                                               //selftest
SelfTestItem        := TMenuItem.Create(HelpMenu);
SelfTestItem.Caption:= 'Selftest';
SelfTestItem.Enabled:= FileExists('selftest01_theoretical.txt');
SelftestItem.OnClick:= @SelfTest;
OnClose             := @FormClose;
SelftestLevel       := 0;
HelpMenu.Insert(1,SelfTestItem);
{$ENDIF}
for i:= 1 to NumSpecialModes do with SpecialMode[i] do                          //add specialmodes to processing menu
  begin
  MenuItem:= TMenuItem.Create(ProcessingMenu);
  MenuItem.AutoCheck:= True;
  MenuItem.Caption  := Format('SpecialMode[%d]',[i]);
  MenuItem.Name     := Format('SpecialModeItem%d',[i]);
  MenuItem.Checked  := False;
  MenuItem.OnClick  := @SetCaption;
  ProcessingMenu.Add(MenuItem);
  end;
i := 100;                                           {------------------------field types tab----------------------}
j := i+20;
aa:= String('Prim,Fallback').Split(',');
ab:= String(DefCoFDefaults ).Split(',');                                        //defaults for each class
ac:= String(DefNormDefaults).Split(',');                                        //defaults for each class = 'C,O,M,C';
if (Length(aa)<>Ord(High(twcEdgeClass))+1) or (Length(ab)<>Ord(High(twcFieldClass))+1) or (Length(ac)<>Ord(High(twcFieldClass))+1) then
  FatalError('wrong number of parameters for EdgeTypes, DefCoFDefaults or DefNormDefaults');
for f:= Low(twcFieldClass) to High(twcFieldClass) do
  begin
  Ft_TypeLabel[f]:= TLabel.Create(AnalyseForm);                                 //Field types: Ft_TypeLabel
  with Ft_TypeLabel[f] do
    begin
    Parent     := FieldTypesPanel;
    HelpContext:= Parent.HelpContext;
    Top        := 5;
    Width      := i;
    Left       := Ft_EdgePrimaryLabel.Left+Ft_EdgePrimaryLabel.Width+Ord(f)*j;
    Caption    := twcFieldClassNames[f];                                        //twcFieldClassNames:array[twcFieldClass] of String=('Standard','FFF','Small','MRlinac','Wedge','Electrons');
    Name       := 'Ft_Edge_'+Caption;                                           //name is needed for storage in config files
    end;
  Ft_DetectionCheckbox[f]:= TCheckBox.Create(AnalyseForm);                      //Field types: Ft_DetectionCheckbox
  with Ft_DetectionCheckbox[f] do
    begin
    Parent     := FieldTypesPanel;
    HelpContext:= Parent.HelpContext;
    Name       := Format('Ft_%sDetect',[twcFieldClassNames[f]]);                //name is needed for storage in config files
    Left       := Ft_TypeLabel[f].Left;
    Top        := Ft_DetectLabel.Top;
    Checked    := True;
    OnClick    := @SyncSetDetection;
    Caption    := '';
    end;
  Ft_DetDiagonalCheckbox[f]:= TCheckBox.Create(AnalyseForm);                    //Field types: Ft_DetDiagonalCheckbox
  with Ft_DetDiagonalCheckbox[f] do
    begin
    Parent     := FieldTypesPanel;
    HelpContext:= Parent.HelpContext;
    Name       := Format('Ft_%sDetDiagonal',[twcFieldClassNames[f]]);           //name is needed for storage in config files
    Left       := Ft_TypeLabel[f].Left;
    Top        := Ft_DetDiagonalLabel.Top;
    Checked    := False;
    OnClick    := @SyncSetDetection;
    Caption    := '';
    end;
  for s:= dsMeasured to dsReference do
    begin
    Ft_SymCorrCheckbox[f,s]:= TCheckBox.Create(AnalyseForm);                    //Field types: Ft_SymCorrCheckbox
    with Ft_SymCorrCheckbox[f,s] do
      begin
      Parent     := FieldTypesPanel;
      HelpContext:= Parent.HelpContext;
      Name       := Format('Ft_%s_%sSymCorr',[twcFieldClassNames[f],twcDataSourceNames[s]]);
      Left       := Ft_TypeLabel[f].Left;
      Top        := ifthen(s=dsMeasured,Ft_MeasSymCorrLabel.Top,Ft_RefSymCorrLabel.Top);
      Caption    := '';
      end;
    end;
  Ft_DynPenumbraCheckbox[f]:= TCheckBox.Create(AnalyseForm);                    //Field types: Ft_DynPenumbraCheckbox
  with Ft_DynPenumbraCheckbox[f] do
    begin
    Parent     := FieldTypesPanel;
    HelpContext:= Parent.HelpContext;
    Name       := Format('Ft_%sDynPen',[twcFieldClassNames[f]]);
    Left       := Ft_TypeLabel[f].Left;
    Top        := Ft_DynPenumbraWidthLabel.Top;
    Checked    := False;
    Caption    := '';
    end;
  for q:= fcPrimary to fcFallBack do
    begin
    Ft_EdgeMethodCombo[f,q]:= TComboBox.Create(FieldTypesPanel);                //Field types: Ft_EdgeMethodCombo
    with Ft_EdgeMethodCombo[f,q] do
      begin
      Parent       := FieldTypesPanel;
      HelpContext  := Parent.HelpContext;
      Top          := ifthen(q=fcPrimary,Ft_EdgePrimaryLabel.Top,Ft_EdgeFallBackLabel.Top)-3;
      Width        := i;
      Left         := Ft_TypeLabel[f].Left;                                     //align with labels
      Name         := Format('Ft_%s_%s_EdgeCombo',[twcFieldClassNames[f],aa[Ord(q)]]);
      AutoComplete := True;
      AutoDropDown := True;
      DropDownCount:= Ord(dSigmoid50)-Ord(dLow)+1;
      for p:= dLow to dSigmoid50 do                                             //twDoseLevel=(dLow,dHigh,d20,d50,d80,d90,dUser,dDerivative,dInflection,dSigmoid50,dTemp)
        AddItem(twcDoseLevelNames[p],nil);
      ItemIndex    := 8;                                                        //dInflection is most used default, see overides below
      end;
    end;
  Ft_CenterMethodCombo[f]:= TComboBox.Create(AnalyseForm);                      //Field types: Ft_CenterMethodCombo
  with Ft_CenterMethodCombo[f] do
    begin
    Parent       := FieldTypesPanel;
    HelpContext  := Parent.HelpContext;
    Top          := Ft_CoFLabel.Top-3;
    Width        := i;
    Left         := Ft_TypeLabel[f].Left;                                       //align with labels
    Name         := Format('Ft_%s_CoFCombo',[twcFieldClassNames[f]]);           //name is needed for storage in config files
    AutoComplete := True;
    AutoDropDown := True;
    DropDownCount:= Ord(CenterMax)-Ord(CenterPenumbra)+1;
    for o:= CenterPenumbra to CenterMax do
      begin
      AddItem(twcCenterTypeNames[o],nil);
      if LeftStr(twcCenterTypeNames[o],1)=ab[Ord(f)] then                       //DefCoFDefaults='B,O,M,O,B,O'
        ItemIndex:= Items.Count-1;
      end;
    OnChange     := @SyncSetCenterOfField;
    end;
  Ft_NormMethodCombo[f]:= TComboBox.Create(AnalyseForm);                        //Field types: Ft_Norm
  with Ft_NormMethodCombo[f] do
    begin
    Parent       := FieldTypesPanel;
    HelpContext  := Parent.HelpContext;
    Top          := Ft_NormLabel.Top-3;
    Width        := i;
    Left         := Ft_TypeLabel[f].Left;                                       //align with labels
    Name         := Format('Ft_%s_NormCombo',[twcFieldClassNames[f]]);          //name is needed for storage in config files
    AutoComplete := True;
    AutoDropDown := True;
    DropDownCount:= Ord(NormOnInFieldArea)-Ord(NormOnCenter)+1;
    for n:= NormOnCenter to NormOnInFieldArea do
      begin
      AddItem(twcNormalisationNames[n],nil);
      if LeftStr(twcNormalisationNames[n],1)=ac[Ord(f)] then                    //DefNormDefaults='C,O,M,O,C,O'
        ItemIndex:= Items.Count-1;
      end;
    OnChange     := @SyncSetNormalisation;
    end;
  Ft_CenterRadius_cm[f]:= TFloatSpinEditEx.Create(AnalyseForm);                 //Field types: Ft_radius
  with Ft_CenterRadius_cm[f] do
    begin
    Parent       := FieldTypesPanel;
    HelpContext  := Parent.HelpContext;
    Top          := Ft_CenterModelRadiusLabel.Top-3;
    Width        := i;
    Left         := Ft_TypeLabel[f].Left;                                       //align with labels
    Name         := Format('Ft_%s_CenterRadiusEdit',[twcFieldClassNames[f]]);   //name is needed for storage in config files
    MinValue     :=  0;
    MaxValue     := 50;
    Value        :=  2.5;
    Increment    :=  0.1;
    DecimalPlaces:= 1;
    NullValue    := Value;
    NumbersOnly  := True;
    OnChange     := @SetWellhoferValues;
    end;
  Ft_Default_SSD_cm[f]:= TFloatSpinEditEx.Create(AnalyseForm);                  //Field types: Ft_default_SSD
  with Ft_Default_SSD_cm[f] do
    begin
    Parent       := FieldTypesPanel;
    HelpContext  := Parent.HelpContext;
    Top          := Ft_Default_SSD_Label.Top-3;
    Width        := i;
    Left         := Ft_TypeLabel[f].Left;                                       //align with labels
    Name         := Format('Ft_%s_Default_SSD_Edit',[twcFieldClassNames[f]]);   //name is needed for storage in config files
    MinValue     :=  10;
    MaxValue     := 900;
    Value        := twcDefaultSSDcm[f];
    DecimalPlaces:= 1;
    Increment    :=   0.1;
    NullValue    := Value;
    NumbersOnly  := True;
    OnChange     := @SetWellhoferValues;
    end;
  end;
FP_RectTop:= FocusPositionResultGroupBox.Top+FocusPositionResultGroupBox.Height+30; {-------------focus position tab---------------}
for angle:= Rot000 to Rot270 do
  FocusPosition_mapping_Box.AddItem(Format('%u°',[Ord(angle)*90]),nil);         //see also FocusPositionPanelPaint
FocusPosition_mapping_Box.ItemIndex:= 0;
for iec:= twcIEC601 to twcIEC1217 do
  begin
  FocusPosition_IEC_Convention.AddItem('IEC'+twcIECnames[iec],nil);
  FocusPosition_UpperBLD_ident.AddItem(twcBLDupper[iec]      ,nil);
  end;
FocusPosition_IEC_Convention.ItemIndex:= Ord(twcIEC1217);
FocusPosition_UpperBLD_ident.ItemIndex:= FocusPosition_IEC_Convention.ItemIndex;
for bank:= twcUpper to twcReference do
  begin
  FP_BankLevel_Label[bank]:= TLabel.Create(AnalyseForm);
  with FP_BankLevel_Label[bank] do
    begin
    Parent   := FocusPositionSetupGroupBox;
    Top      := FocusPositionIECLabel.Top+(Ord(bank) mod 2)*30;
    Width    := 125;
    Left     := ifthen(bank=twcReference,FocusPositionDetectorOther.Left,
                                         FocusPosition_IEC_Convention.Left+FocusPosition_IEC_Convention.Width+20);
    Alignment:= taLeftJustify;
    AutoSize := False;
    Caption  := ifthen(bank=twcReference,'Reference',Format('%ser BLD',[ifthen(bank=twcUpper,'Upp','Low')]))+'  level (mm):';
    end;
  FP_BankLevel_mm[bank]:= TFloatSpinEditEx.Create(AnalyseForm);
  with FP_BankLevel_mm[bank] do
    begin
    Parent       := FocusPositionSetupGroupBox;
    Top          := FocusPosition_IEC_Convention.Top+(Ord(bank) mod 2)*30;
    Left         := FP_BankLevel_Label[bank].Left+FP_BankLevel_Label[bank].Width+2;
    Width        := 56;
    Name         := Format('FocusPosition_%sLevel_mm',[ifthen(bank=twcReference,'SSD_',ifthen(bank=twcUpper,'Upper','Lower'))]);
    MinValue     :=    0;
    MaxValue     := 1000;
    DecimalPlaces:=    1;
    Value        :=    0;
    Increment    :=    0.1;
    NullValue    := Value;
    OnChange     := @FocusPositionChange_mm;
    end;
  FP_BankFactor[bank]:= TLabel.Create(AnalyseForm);
  with FP_BankFactor[bank] do
    begin
    Parent    := FocusPositionSetupGroupBox;
    Top       := FP_BankLevel_Label[bank].Top;
    Width     := 100;
    Left      := FP_BankLevel_mm[bank].Left+FP_BankLevel_mm[bank].Width+10;
    Alignment := taLeftJustify;
    AutoSize  := False;
    Caption   := 'm';
    Font.Color:= clGray;
    end;
  end;
for scan:= snGT to snAB do
  begin
  for angle:= Rot000 to Rot270 do
    begin
    FP_Center_button[scan,angle]:= TRadioButton    .Create(AnalyseForm);
    FP_Center_mm    [scan,angle]:= TFloatSpinEditEx.Create(AnalyseForm);
    with FP_Center_button[scan,angle] do
      begin
      Parent     := FocusPositionResultGroupBox;
      HelpContext:= Parent.HelpContext;
      Top        := 15+Ord(scan)*30;
      Width      := 104;
      Left       :=   0;                                                        //settings dependedent; see FocusPositionPanelPaint
      OnClick    := @FocusPositionRadioClick;
      end;
    with FP_Center_mm[scan,angle] do
      begin
      Parent       := FocusPositionResultGroupBox;
      HelpContext  := Parent.HelpContext;
      Top          := FP_Center_button[scan,angle].Top-2;
      Width        := 56;
      Left         :=  0;                                                       //settings dependedent; see FocusPositionPanelPaint
      Name         := Format('FP_Center_mm_%s_%.3d',[ifthen(scan=snGT,'GT','AB'),Ord(angle)*90]);
      Color        := clYellow;
      DirectInput  := True;
      MinValue     := -100;
      MaxValue     :=  100;
      DecimalPlaces:=    2;
      Value        :=    0;
      Increment    :=    0.1;
      NullValue    := Value;
      NumbersOnly  := False;
      OnChange     := @FocusPositionChange_mm;
      OnEditingDone:= @FocusPositionChange_mm;
      Tag          := 1;
      end;
    end;
  end; {scan}
for x:= InPlane to CrossPlane do
  begin
  FP_Calc_Dbfso_mm[x]:= 0;
  FP_Calc_Drot_mm[x] := 0;
  FP_Results_label[x]:= TLabel.Create(AnalyseForm);
  with FP_Results_label[x] do
    begin
    Parent   := FocusPositionResultGroupBox;
    Top      := FP_Center_button[scan,Rot000].Top+2;                            //scan and x refer to same memory location
    Left     := 4*FPrectSpacing;
    AutoSize := False;
    Width    := 60;
    Alignment:= taRightJustify;
    Caption  := Format('%splane:',[ifthen(x=Inplane,'in','cross')]);
    end;
  for bank:= twcUpper to twcLower do
    begin
    FP_Average_mm[x,bank]:= TLabel.Create(AnalyseForm);
    with FP_Average_mm[x,bank] do
      begin
      Parent    := FocusPositionResultGroupBox;
      Top       := FP_Results_label[x].Top;
      AutoSize  := False;
      Width     := 40;
      Left      := FP_Results_label[x].Left+FP_Results_label[x].Width+Ord(bank)*(Width+2);
      Alignment := taRightJustify;
      Font.Color:= clGray;
      end;
    end; {bank}
  FP_Dbfso_mm[x]:= TLabel.Create(AnalyseForm);
  with FP_Dbfso_mm[x] do
    begin
    Parent    := FocusPositionResultGroupBox;
    Top       := FP_Results_Label[x].Top;
    Left      := FP_Average_mm[x,twcLower].Left+FP_Average_mm[x,twcLower].Width+2;
    AutoSize  := False;
    Width     := 40;
    Alignment := taRightJustify;
    Font.Style:= [fsBold];
    Font.Color:= clMaroon;
    end;
  FP_Drot_mm[x] := TLabel.Create(AnalyseForm);
  with FP_Drot_mm[x] do
    begin
    Parent   := FocusPositionResultGroupBox;
    Top      := FP_Results_Label[x].Top;
    Left     := FP_Dbfso_mm[x].Left+FP_Dbfso_mm[x].Width+2;
    AutoSize := False;
    Width    := 40;
    Alignment:= taRightJustify;
    end;
  end;
FP_Average_Titles:= TLabel.Create(AnalyseForm);
with FP_Average_Titles do
  begin
  Parent    := FocusPositionResultGroupBox;
  Top       := FP_Average_mm[Inplane,twcUpper].Top -20;
  Left      := FP_Average_mm[Inplane,twcUpper].Left+12;
  Alignment := taLeftJustify;
  Caption   := 'avg U     avg L';
  Font.Color:= clGray;
  end;
FP_Dbfso_Title:= TLabel.Create(AnalyseForm);
with FP_Dbfso_Title do
  begin
  Parent    := FocusPositionResultGroupBox;
  AutoSize  := False;
  Top       := FP_Average_Titles.Top;
  Left      := FP_Dbfso_mm[Inplane].Left;
  Width     := FP_Dbfso_mm[Inplane].Width;
  Alignment := taRightJustify;
  Font.Style:= [fsBold];
  Font.Color:= clMaroon;
  Caption   := 'Dbfso';
  end;
FP_Drot_Title:= TLabel.Create(AnalyseForm);
with FP_Drot_Title do
  begin
  Parent    := FocusPositionResultGroupBox;
  AutoSize  := False;
  Top       := FP_Average_Titles.Top;
  Left      := FP_Drot_mm[Inplane].Left;
  Width     := FP_Drot_mm[Inplane].Width;
  Alignment := taRightJustify;
  Caption   := 'Drot';
  end;
aa:= String(DefFFFpSubTexts).Split(',');                                        //FFF peak submenu: texts
ab:= String(DefFFFpSubKeys ).Split(',');                                        //FFF peak submenu: shortcuts
ac:= String(DefFFFpSubNames).Split(',');
for m:= CenterFFFTopModel to CenterFFFSlopes do      {---------------------------FFF peak sub menu------------------------}
    begin
    FFFpSubItems[m]:= TMenuItem.Create(MeasPeakFFFSubMenu);
    with FFFpSubItems[m] do
      begin
      Name     := ac[Ord(m)];
      Caption  := aa[Ord(m)];
      Checked  := m=CenterFFFTopModel;
      RadioItem:= True;
      AutoCheck:= True;
      OnClick  := @SyncSetFFFpeak;
      ShortCut := TextToShortCut('Alt+Shift+'+ab[Ord(m)]);
      end;
    end;
MeasPeakFFFSubMenu.Add(FFFpSubItems);                                           //insert fff peak submenu
aa:= String(DefExtSymTexts   ).Split(',');                                      //ExtSym submenu: texts
ac:= String(DefExtSymSubNames).Split(',');                                      //ExtSym submenu: names
for e:= ExtSymLinacError to ExtSymElevation do       {---------------------------extended symmetry sub menu----------------}
    begin
    ExtSymSubItems[e]:= TMenuItem.Create(MeasExtSymSubMenu);
    with ExtSymSubItems[e] do
      begin
      Name     := ac[Ord(e)];
      Caption  := aa[Ord(e)];
      Checked  := Ord(e)=0;
      RadioItem:= True;
      AutoCheck:= False;
      OnClick  := @SyncSetExtSym;
      end;
    end;
MeasExtSymSubMenu.Add(ExtSymSubItems);                                          //insert submenu
SyncSetExtSym(ExtSymAreaRatio);                                                 //set default to area ratio; set caption of parent menu item
i:= GammaLimitAreaLabel.Top+GammaLimitAreaLabel.Height;
j:= GammaLimitAreaLabel.Left;
for f:= Low(twcFieldClass) to High(twcFieldClass) do {-------------------GammaSettingsGroupBox checkbox for each FT----------------}
  begin
  GammaInFieldLimits[f]:= TCheckBox.Create(AnalyseForm);
  with GammaInFieldLimits[f] do
    begin
    Parent     := GammaSettingsGroupBox;
    HelpContext:= Parent.HelpContext;
    Top        := i+Ord(f)*17;
    Left       := j;
    Caption    := twcFieldClassNames[f];
    Name       := 'GammaInFieldLimits_'+Caption;
    end;
  end;
for x:= Inplane to Beam do                           {-------------------ShiftGroupBox labels and values----------------}
  begin
  ShiftLabels[x]:= TLabel.Create(AnalyseForm);
  with ShiftLabels[x] do
    begin
    Parent     := ShiftGroupBox;
    HelpContext:= Parent.HelpContext;
    Top        :=  2;
    Width      := 63;
    Left       := Ord(x)*135;
    Caption    := twcMeasAxisNames[x]+':';
    Alignment  := taRightJustify;
    Name       := 'ShiftLabel_'+twcMeasAxisNames[x];
    end;
  ShiftValues_cm[x]:= TFloatSpinEditEx.Create(AnalyseForm);
  with ShiftValues_cm[x] do
    begin
    Parent       := ShiftGroupBox;
    HelpContext  := Parent.HelpContext;
    Top          :=    0;
    Width        :=   59;
    MinValue     := -100;
    MaxValue     := -100;
    NullValue    :=    0;
    Increment    :=    0.01;
    DecimalPlaces:=    3;
    Left         := ShiftLabels[x].Left+ShiftLabels[x].Width+2;
    Name         := Format('Shift%s_cm',[twcMeasAxisNames[x]]);
    NumbersOnly  := True;
    end;
  end;
Finalize(aa);                                                                   //these helpers are not used anymore
Finalize(ab);
Finalize(ac);
DataPlot.LeftAxis         .Range.Max      := 101;               {------------------dataplot--------------------}
DataPlot.LeftAxis         .Range.Min      :=  99;
DataPlot.LeftAxis         .Range.UseMax   := True;
DataPlot.LeftAxis         .Range.UseMin   := True;
FFFdataSource                             := dsMeasured;
MeasNormAdjustFactor                      := 1;
AxisAlignSource                           := TCustomAxisChartSource.Create(Self);
AxisAlignSource           .AxisFrom       := DataPlot.LeftAxis;
AxisAlignSource           .AxisTo         := DataPlot.AxisList[DefChartAxR];    //used in AutoZoom
SM2_Infotype                              := 1;
AppliedFieldClass                         := fcStandard;
FFFfeatures                               := False;
AppliedEdgeRefNorm                        := d50;
PlotPending                               := False;                             //state init
OnDataReadBusy                            := False;                             //state init
SelectedSeries                            := nil;
ModMode                                   := ModMNorm;
Visible                                   := True;
AdvancedModeOk                            := True;
InventoryListReady                        := True;                              //see Files tab
InventoryMultiScanList                    := False;
DoseCLastRow                              :=  -1;
PrevKey                                   := #0;                                //keyboard handling
MinClipBoardBytes                         := 100;                               //ignore clipboard when too small
LogLevelEdit              .Value          := 1;                                 //set logging to standard level
MeasReMappingString       .Items.CommaText:= twcMeasAxisPermutations;
MeasReMappingString       .Text           := MeasReMappingString.Items[0];
FileConvSourcePath        .Directory      := ExtractFilePath(ParamStr(0));
FileConvDestinationPath   .Directory      := FileConvSourcePath.Directory;
InventoryDirBox           .Directory      := CommonAppData;
InventoryDirBox           .RootDir        := ExtractFileDir(InventoryDirBox.Directory);
FileConvSourcePath        .Text           := FileConvSourcePath.Directory;
FileConvDestinationPath   .Text           := FileConvDestinationPath.Directory;
FileConvSourceListBox     .Items.CommaText:= DefConvInTypes;
FileConvDestinationListBox.Items.CommaText:= DefConvOutTypes;
FileConvSourceListBox     .ItemIndex      := 0;
FileConvDestinationListBox.ItemIndex      := 0;
FileConvDestExt                           := '.txt';
FileConvNameMask.Text                     := ConvDefmask;
FileConvItemPatterns                      := DefConvItemStrings;
ConfigRepairFound                         := FileExists(DefConfigRepairFile);
PresetName                                := '';
PipsPixelSize_mm          .Value          := twcDefPipsPixel_cm*10;
FitMu3CheckBox            .Caption        := pddfitEnames[pddfit_mu3];
FitMu4CheckBox            .Caption        := pddfitEnames[pddfit_mu4];
FitMx2CheckBox            .Caption        := pddfitEnames[pddfit_mx2];
ModListGrid               .Options        := ModListGrid.Options + [goEditing];
SpecialModeValues                         := TStringList.Create;                //preserve values from ini file
FocusPositionResetButton  .Left           := FPrectOffset+3*FPrectSpacing-FocusPositionInputGroupBox.Left-2;
FocusPositionZeroFPButton .Left           := FocusPositionResetButton.Left+FPrectSpacing;
FocusPositionZeroRotButton.Left           := FocusPositionZeroFPButton.Left;
ConfigName                                := ExtractFilePath(ParamStr(0));      //temporary use of configname
if (Pos('Program Files',ConfigName)>0) and
   DirectoryExists(CommonAppData+DefAppName) then                               //bistromath installed in standard location *and* commonappdata exists
  CommonAppData:= AppendPathDelim(CommonAppData+DefAppName)
else
  CommonAppData:= AppendPathDelim(ConfigName);                                  //CommonAppdata should be adjusted before initialisation of Wellhofer
SetMessageBar('Verify this checksum online: SHA-256 = '+MakeCheckSum);
SetMessageBar('CommonAppdata='+CommonAppData);
ConfigName:= CommonAppdata+DefInstituteCfg;                                     //default path to ini file institute.ini
if FileExists(ConfigName) then SetMessageBar('config='+ConfigName)
else                           SetConfigName('');
with Engines[UsedEngine] do
  begin
  ObjectCallSign   := 'current';
  StatusProcedure  := @SetMessageBar;
  AutoLoadReference:= True;
  end;
CurveString        := '';
IniSectionName     := Name;                                                     //name of form (analyseform)
DataChanged        := True;
DataFromEditor     := False;
HistogramSource    := dsCalculated;
TempPath           := '';
DetectedFileType   := twcUnknown;
WriteMenuShortCuts := False;
InventoryReader    := nil;
SetBasicDefaults;
SetLength(UseDoseConvTable,0);
GetWellhoferValues;
UseDoseAddButtonClick(Self);
for i:= 1 to ParamCount do                                                      //handle configuration file related params
  begin
  if ParamStr(i).IndexOf(DefSpecialMode)>=0 then
    begin
    j:= StrToIntDef(Copy(ParamStr(i),Length(DefSpecialMode)+1,1),0);
    if j in [1..NumSpecialModes] then
      SpecialMode[j].MenuItem.Checked:= True;
    end
  else if ParamStr(i).IndexOf(DefLogLevel)>=0 then
    Engines[UsedEngine].LogLevel:= StrToIntDef(Copy(ParamStr(i),Length(DefLogLevel)+1,1),0)
  else if (i<ParamCount) and (LowerCase(ParamStr(i))=DefConfigFile) then
    SetConfigName(ParamStr(Succ(i)),True);
  end;
FileOpenDialog.InitialDir:= CommonAppData;
if Length(DefAnnotTypeString)<>Ord(High(AnnotationTypes)) then                  //this should not lead to any code
 {$push}{$warn 6018 off:the compiler has determined there is, or might be, unreachable code}
  begin
  SetMessageBar(DefAnnotTypeString+' has wrong length');                        //this message is only displayed when there is something wrong
  FormDestroy(Sender);
  end;
 {$pop}
ConfigLoad(Self,ConfigName,True);                   {--------------load config-------------initcxblock is called here-------}
if PanelElements.AddMode>0 then
  begin
  ViewStandardPanelsetup.Checked:= (PanelElements.Count=0);
  SetDefaultPanel(Sender);                          {---------panelsetup dependent on config--------------------------------}
  end;
SetConfigName('');                                                              //set config name to default: bistromath.ini
for i:= 1 to ParamCount do                                                      //handle configuration other params to overrule configuration
  begin
  if LowerCase(ParamStr(i))=DefAdvanced then
    begin
    AdvancedModeItem.Checked:= True;
    UImodeChange(Self);
    end
  else if ParamStr(i)='-md5' then
    begin
    ClipBoard.AsText:= MakeChecksum;
    Application.Terminate;
    end
  else if i<ParamCount then
    begin
    if LowerCase(ParamStr(i))=DefRefPath then
      TempPath     := ParamStr(Succ(i))
    else if LowerCase(ParamStr(i))=DefDataPath then
      CommonAppData:= AppendPathDelim(ParamStr(Succ(i)))
    else if LowerCase(ParamStr(i))=DefPreset then
      PresetName   := ParamStr(Succ(i));
    end;
  end;
ModListUpdate(Sender);
Visible               := True;
PrevTab               := PageControl.ActivePage;
PageControl.ActivePage:= AnalysisTab;
LastProfileZoomState  := ViewZoomItem.Checked;
SelectedFitCol        := 1;
FKeyboardReady        := True;                                                  //check with onkeyup event whether there are more keys to be processed
//&& DragAcceptFiles(Handle,True);                                              //form is ready to accept files; should not be necessary with property AllowDropFiles=True
ClearScreen(ViewClearItem);
if Length(TempPath)>0 then
  Engines[UsedEngine].ReferenceDirectory:= TempPath;                            //AltRefPath will get lost when a config is (re)loaded
{$IFDEF Windows}
TempPath:= ChangeFileExt(Application.ExeName,'.chm');
if FileExists(TempPath) then
  begin
  Application.HelpFile:= TempPath;
  Application.OnHelp  := @HelpHandler;
  end;
{$ENDIF}
SetMessageBar('DataDir='+CommonAppData);
InventorySetRefDirClick(Sender);                                                //'Reference='+Wellhofer.ReferenceDirectory
if Length(PresetName)>0 then
  PresetLoad(ifthen(Pos(PathSeparator,PresetName)=0,CommonAppData,'')+PresetName)
else
  PresetName:= 'preset';
{$IFDEF LCL_2-3_Up}
DataPlot.AxisList[DefChartAxR].OnGetMarkText:= @RightAxisGetMarkText;
{$ELSE}
DataPlot.AxisList[DefChartAxR].OnMarkToText := @AxisMarkToText;
{$ENDIF}
SetCaption(ExtraVersionInfo);
SetForegroundWindow(Handle);
{$IFNDEF MULTIREF_INDEX}
ReferenceMakeIndexItem.Visible:= False;
{$ENDIF}
{$IFDEF X_FIT_TEST}
DataEditorOpenFile('selftest03_pdd.txt');
{$ELSE}
with SpecialMode[1] do
  if MenuItem.Checked then
    begin
    i:= 1;
    j:= HistoryListSize_num.Value-1;
    repeat
      if (Length(Spar[i])>0) and ((not Engines[0].IsValid) or (HistoryListCheckBox.Checked and (UsedEngine<j))) then
        begin
        if FileExists(Spar[i]) then
          DataFileOpen(ifthen(Pos(PathSeparator,Spar[i])+Pos(':',Spar[i])=0,CommonAppData,'')+Spar[i])
        else
          SetMessageBar('SpecialMode1 file not found ('+Spar[i]+').');
        end;
      Inc(i);
    until i>NumSpecialModePar;
    end
  else
    SetCaption;
{$ENDIF}
ClipBoardLock        := False;                                                  //finally unlock clipboard
{$IFDEF JwaWinBase}
FIdleTimer:= TIdleTimer.Create(Self);
FIdleTimer.Interval   := 10000;
FIdleTimer.AutoEnabled:= True;
FIdleTimer.OnTimer    := @OnIdleTimer;
{$ENDIF}
end; {~formcreate}


{18/11/2022 implementation of OffAxisInclusionRange_cm}
function TAnalyseForm.ScanInclusionTest(aEngine:Integer): Boolean;
begin
with Engines[Clip(aEngine,0,Length(Engines)-1)] do
  Result:= IsValid and (Abs(OffAxis_cm)<OffAxisInclusionRange_cm.Value);
end; {~scaninclusiontest}


{$IFDEF JwaWinBase}
procedure TAnalyseForm.GetCpuUsage(Display     :Boolean=True;
                                   MinReset_sec:twcFloatType=1);
{$PUSH}
{$CODEALIGN LOCALMIN=8}
var
  IdleTimeRec  : TFileTime;
  KernelTimeRec: TFileTime;
  UserTimeRec  : TFileTime;
  IdleTime     : Int64 absolute IdleTimeRec;
  KernelTime   : Int64 absolute KernelTimeRec;
  UserTime     : Int64 absolute UserTimeRec;
  IdleDiff     : Int64;
  KernelDiff   : Int64;
  UserDiff     : Int64;
  SysTime      : Int64;
{$POP}
begin
if GetSystemTimes(@IdleTimeRec, @KernelTimeRec, @UserTimeRec) then
  begin
  if (FLastKernelTime=0) and (not QueryPerformanceFrequency(LARGE_INTEGER(FPerformanceFrequency))) then
    FPerformanceFrequency:= 10000000;
  IdleDiff       := IdleTime  -FLastIdleTime;
  KernelDiff     := KernelTime-FLastKernelTime;
  UserDiff       := UserTime  -FLastUserTime;
  SysTime        := KernelDiff+UserDiff;
  if SysTime/FPerformanceFrequency>Max(0.001,MinReset_sec) then
    begin
    FLastIdleTime  := IdleTime;
    FLastKernelTime:= KernelTime;
    FLastUserTime  := UserTime;
    end;
  with StatusBar.Panels[3] do
    begin
    if Display and (Systime>0) then
      Text := Format('CPU %.1f%%',[(SysTime-IdleDiff)/SysTime*100])
    else
      Text := '';
    end;
  end;
end; {~getcpuusage}
{$ENDIF}

(* 18/04/2015
****BistroMath core function****
{=> ProcessReprocessItem, ProcessMergeItem,
    ViewCalculatedItem, ViewHighResValItem, ViewSwapGTItem, ViewSwapABItem, ViewSwapUDItem, ViewSwapLRItem, ViewBottomAxisAlwaysBlack,
    MeasUserDoseItem, MeasUsePDDfitModelItem, MeasMissingPenumbraItem, RefNormaliseItem, CalcPostFilterItem

This truly is the core of the GUI. It handles the consequences of all settings upon a newly read data set.
It calls a lot of functions from the TWellhoferdata object (engines[usedengine]) to apply these choices.
Direct access of the data is only used to copy them to the graphical elements.
Needs properly loaded dataset. A lot of corrections are already done at file read time by TWellhoferData.
Applies background correction, mirroring, merge, division, pdd fit, histogram analysis, sets extra options for analysis.
Fills graph and triggers display of analysis results (PublishResults).
*)
{02/06/2015 threading rewritten
  Called by ReadEditor (clipboard).
  Note that a file open event is transfered directly to the editor for
  text files and indirectly through the wellhofer object for binary files
  as text output.}
{14/07/2015 Partial edge detection is now handled.
  In these cases the normalisation is temporarily set to NormOnMax. Therefore
  the original normalisation must be preserved. Any change of normalisation is
  passed to relevant structures.
  Merge introduced.}
{28/07/2015
  After merge smart reload of reference.
  Use SetPlotDates to clip too long strings.}
{29/07/2015 Ensure that buffer contains derivative of measurement}
{30/07/2015 ProcessMirrorMeasRefItem}
{01/08/2015 Nieuwe implementatie wellhofer.loadreference}
{05/08/2015 normalisation changed => analyse(reference)}
{12/08/2015 merging reorganised}
{01/09/2015 twOriginPosValid used}
{21/09/2015 test on negative values of UserBorderDose_perc removed}
{11/12/2015
  static LeffSeries and ReffSeries replaced with dynamic InFieldIndicators
  added FFFSlopeLines}
{12/12/2015 twFFFdetected}
{15/12/2015 FFFslopeSource, MeasDetectFFFItem}
{13/02/2016 reviewed presentation relative flatness}
{10/05/2016 ErrorState used}
{28/06/2016 fill clibboard with pddfit results, using last options}
{10/09/2016 try..except}
{05/12/2016 ProcessSyntheticProfile}
{30/12/2016
  FFFmessage only when Wellhofer.ShowWarning
  Flatness splitted in Abs. and Rel}
{10/01/2017 value relative flatness only shown if calculated curve is visible}
{18/01/2017 mirror around twcenterPosCm}
{10/02/2017 ApplySigmoidPenumbraFit only when sigmoidfit is already done}
{11/07/2017 included Mayneord transform}
{21/07/2017 if Wellhofer.Ready}
{09/08/2017
  if Mayneord then LoadReference('',TempRefAction.Checked);
  ViewReferenceItem.Enabled:= ReferenceValid;  placed just after Mayneord}
{11/09/2017
  after pddfit: set validity of buffer: wSource[Buffer].twValid:= wSource[Measured].twPddFitData[NM_Primary].twFitValid;
  check validity of buffer}
{03/11/2017 for angle scans swapaxis is always false; stick to OmniPro v6 definitions}
{06/12/2017 DoCorrections gets bgsubtract value from relevant menu}
{27/12/2017 make use of MeasFiltered curve}
{05/01/2018 CxResults rewritten}
{16/01/2018 ClearAllCx}
{24/01/2018 CheckWellhoferReady}
{30/01/2018 don't change twApliedNormVal in Set_PlotScaling; it should be set Analyse only}
{01/02/2018 ViewMillimetersItem}
{29/05/2018 application GammaLimitFFF, GammaLimitConventional}
{21/09/2018 units (cm/mm) in bottomaxis.title.caption}
{08/10/2018 errorseries: apply mm-mode}
{12/10/2018 ViewMeasNormAdjustMode,MeasNormAdjustEdit,MeasNormAdjustFactor}
{27/10/2018 ViewMeasNormAdjustMode only in advancedmode}
{23/11/2018 SmartScaleElectronPDD}
{25/11/2018 ProcessAutoscalingItem}
{10/12/2019 ProcessSigmoid2BufferItem}
{17/03/2020 RawDataEditor changes}
{11/04/2020 ========FreePascal TAchart related rewrites==========}
{15/04/2020 revised implementation of PlotScaleMax and PlotScaleMin}
{16/04/2020 new implementation of LastProfileZoomState (formerly LastZoomState)}
{17/04/2020 always fill pBuffer with data when valid}
{21/04/2020 explicitely nil the PlotFillThread array}
{25/04/2020 do analysis of unfilterded measurement at early stage}
{27/04/2020 message when temporary reference is applied}
{02/05/2020 measmirroritem implemented}
{02/05/2020 DCModalityBox.Text='' also valid for dose converion / bg subtraction}
{04/05/2020 set viewbufferitem when there are fitted data in buffer}
{05/05/2020 after corrections: if not wSource[dsMeasFiltered].twValid then QuadFilter(0,dsMeasured,dsMeasFiltered)}
{13/05/2020 show confirmed wMultiScanNr}
{17/06/2020 reset dataplot.logicalextent when plot itself is zoomed with mouse}
{13/07/2020 Wellhofer.wApplyUserLevel:= MeasUserDoseItem.Checked}
{15/07/2020 GammaInFieldLimits,AppliedEdgeRefNorm}
{16/06/2020 check ProcessSigmoid2BufferItem before ApplySigmoidPenumbraFit to buffer}
{20/07/2020 EdgeSmallFieldWidth_cm}
{21/07/2020 AxisViewFieldTypeCheckBox}
{21/07/2020 AdjustedFilterWidthCm, fcWedge}
{28/07/2020 Ft_XXXX[twcFieldClass] elements}
{18/08/2020 set AppliedFieldClass after formal Analyse statement, in earlier phase it will depend on reference}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{17/09/2020 Freeze}
{06/10/2020 fundamentals library alternative}
{15/10/2020 set FieldType name in axis title as is, without brackect or lowercase}
{29/10/2020 ReportDifferences added for failing reference}
{14/01/2020 PriorityMessage shown at the very end}
{24/02/2021 FFF detected added to bottom axis title}
{02/03/2021 FFFfeatures}
{03/03/2020 removed MeasDetectDiagItem}
{01/08/2021 OnDataReadBusy:= True moved to earlier phase}
{28/03/2022 introduction AppliedGammaScope}
{10/04/2022 can be triggered through ProcessSetFFFItem}
{14/02/2023 OnDataReadBusy placed at earliest point}
procedure TAnalyseForm.OnDataRead(Sender:TObject);
var i                         : Integer;
    m                         : twcMeasAxis;
    Br,Er,Tr                  : String;
    tmpE,tmpS,tMin,tMax,tmpX,
    x,PlotScaling             : twcFloatType;
    b                         : Boolean;
    d                         : twcDoseLevel;
    {$IFDEF SPEEDTEST}
    {$IF DEFINED(PANEL_SPEEDTEST) OR DEFINED(DIVIDE_SPEEDTEST) OR DEFINED(GAMMA_SPEEDTEST)}
    k                         : Cardinal;
    {$ENDIF}
    {$IFDEF ONDATA_SPEEDTEST}
    o                         : Cardinal;
    j                         : Word;
    {$ENDIF ONDATA_SPEEDTEST}
    {$ENDIF SPEEDTEST}
    {$IFDEF THREAD_PLOT}
    PlotFillThread            : array[PlotItems] of THelpFillThread;
    p                         : PlotItems;
    {$ENDIF THREAD_PLOT}

  {apply dose conversion and background subtraction}
  procedure DoCorrections(ACurve    :twcDataSource;
                          BGsubtract:Boolean);
  var m,f: String;
  begin
  if DoseConvTableNr>=0 then
    with Engines[UsedEngine],wSource[ACurve],UseDoseConvTable[DoseConvTableNr] do if (not Freeze) and twValid then
      begin
      if DCDoseBox.Checked and MeasOD2DoseConvItem.Checked and MeasOD2DoseConvItem.Enabled then
        begin
        m:= DCModalityBox.Text;
        f:= DCFilmTypeBox.Text;
        SubtractBackGround(0,ACurve);
        OD2doseConversion(m,f,ACurve);
        end;
      if BGsubtract and DCBgBox.Checked then
        SubtractBackGround(DCEdit.Value,ACurve);
      end; {with wellhofer}
  end; {docorrections}

  {29/02/2016 introduction of selectable source for output (Wsrc, Fpar[3])}
  {16/10/2019 introduction of FPar[4]}
  {09/03/2021 set filesavedialog.defaultext}
  procedure DoSpecialMode3;
  var i,p,q: integer;
      s    : twcFloatType;
      Fs,Ds: String;
      Wsrc : twcDataSource;
  begin
  with Engines[UsedEngine],SpecialMode[3] do
    begin
    if (Length(SPar[2])>0) and (FileSaveDialog.DefaultExt<>SPar[2]) and (Pos(Spar[2],FileSaveDialog.Filter)>0) then
      begin
      SPar[2]                  := SPar[2].Trim(['*',',']);                      //TStringhelper function
      FileSaveDialog.DefaultExt:= SPar[2];
      end;
    QuadFilter(Fpar[1],dsMeasured);                                             //FPar[1]=filter width in cm, output to dsCalculated
    Analyse(dsMeasured,twcAutoCenter(Min(Max(Ord(Low(twcAutoCenter)),Round(FPar[5])),Ord(High(twcAutoCenter))))); //FPar[5]=twcAutoCenter:(0=AC_default,1=AC_on,2=AC_off)
    if PDDfitCheckBox.Enabled and PDDfitCheckBox.Checked and (ScanType in twcVertScans) then
      PddFit(dsMeasured,dsBuffer);
    Analyse(dsCalculated);
    if Round(FPar[3])<1 then Wsrc:= dsMeasured
    else                     Wsrc:= dsCalculated;
    with wSource[Wsrc],twBeamInfo do if twValid then
      begin
      DateTimeToString(Ds,'yyyymmdd_hhnn',twMeasDateTime);
      Fs:= ifthen(Length(Spar[1])>0,AppendPathDelim(Spar[1]),'') +
           StripCharSet(GetCurveIDString,csIllegalWinName) +
           ifthen(AddDateTimeCheckBox.Checked,'_'+Ds,'');
      WriteData(Fs,SetFileType(FileSaveDialog.DefaultExt));
      RawDataEditor.Clear;
      EditorFileName:= ChangeFileExt(Fs+'_data',DefaultExtension);
      s:= Math.Max(0.1,Fpar[2]);
      if (ScanType in twcVertScans) then                                        //FPar[2]=step width in cm
        begin
        p:= Ceil( Math.Max(1,GetPosition(dsCalculated,twDataRange[ptFirst]) )/s);
        q:= Trunc(Math.Min(  GetPosition(dsCalculated,twDataRange[ptLast ]),30)/s);
        end
      else
        begin
        p:= Trunc(GetPosition(dsCalculated,twInfieldRange[twcLeft] )/s);
        q:= Trunc(GetPosition(dsCalculated,twInfieldRange[twcRight])/s);
        end;
      for i:= p to q do
        DataEditorAddLine(Format('%5.2f  %6.2f %s %s%0.0f %s',
                                [i*s,GetScaledQfValue(i*s,False,scNormalised,dsCalculated),
                                 Linac,LowerCase(wCurveInfo.twDesTypeString[1]),FieldLength_cm,Ds]));
      if FPar[4]>0 then                                                         //FPar[4]=1:write resampled data to disk
        MemoSaveToFile(RawDataEditor);
      end;
    end;
  end; {dospecialmode3}

  procedure Set_PlotScaling(ASource:twcDataSource=dsMeasured);
  begin
  with Engines[UsedEngine],wSource[ASource] do
    begin
    try
      PlotScaling:= GetQfittedValue(wSource[dsMeasured].twRelNormPos_cm,ASource,100);
      if not ProcessAutoscalingItem.Checked then
        PlotScaling:= 1
      else if ScanType in twcVertScans then
        PlotScaling:= Abs(wSource[dsMeasured].twRelNormValue/GetQfittedValue(wSource[dsMeasured].twRelNormPos_cm,ASource,100))
      else if twMaxValue=0 then
        PlotScaling:= 1
      else
        PlotScaling:= ifthen((ASource in [dsMeasured,dsReference,dsCalculated]) or twIsRelative,GlobalNormAdjust_perc.Value,100)/
                      ifthen(twAppliedNormVal>0,twAppliedNormVal,twMaxValue);  {readdata->prepareprofile->fastscan}
     except
      PlotScaling:= 1;
    end;
   twPlotScaling:= PlotScaling;
   end;
  end; {set_plotscaling}

  {Direct access of an shared object through a thread should be avoided.
  It will lead to memory leaks because of failing memory disposal of the thread-object.

  HELP
  Execute
  Provides an abstract or pure virtual method to contain the code which executes when the thread is run.

  Description
  Override Execute and insert the code that should be executed when the thread runs.
  Execute is responsible for checking the value of the Terminated property to determine if the thread needs to exit.
  A thread executes when Create is called if CreateSuspended set to false, or when Resume is first called after the thread is created if CreateSuspended set to true.
  Note:	Do not use the properties and methods of other objects directly in the Execute method of a thread.
  Instead, separate the use of other objects into a separate procedure call, and call that procedure by passing it as a parameter to the Synchronize method.}

  {06/08/2015 twLocked is set}
  {12/04/2022 AnalRange}
  procedure FillPlotSeries(ASeries  :PlotItems;                                                         //initiate plotting of data using ThreadSaveFillPlot
                           ASource  :twcDataSource;
                           AScaling :twcFloatType;
                           AnalRange:Boolean=False);
  begin
  with Engines[UsedEngine].wSource[ASource] do
    begin
    PlotScaleMin:= Math.Min(PlotScaleMin,twData[twMinArr]*AScaling);
    PlotScaleMax:= Math.Max(PlotScaleMax,twData[twMaxArr]);
    end;
  {$IFDEF THREAD_PLOT}
  Engines[UsedEngine].wSource[ASource].twLocked:= True;                         //creating the thread also takes some time: do manual lock first in main thread
  PlotFillThread[ASeries]:= THelpFillThread.Create(@ThreadSaveFillPlot,ASeries,ASource,AScaling,AnalRange,False); //no freeonterminate, cleanup on end of ondataread
  {$ELSE}
  ThreadSaveFillPlot(ASeries,ASource,AScaling,AnalRange);
  {$ENDIF THREAD_PLOT}
  end; {fillplotseries}

begin
if PollKeyEvent<>0 then
  FKeyboardReady:= False;                                                                               //there are still keys in buffer
x:= ifthen(ViewMeasNormAdjustMode.Checked and AdvancedModeItem.Checked,MeasNormAdjustEdit.Value/100,1); //new value for NormAdjustFactor
b:= not ((Sender=MeasNormAdjustEdit) and (x=MeasNormAdjustFactor));                                     //check if action is needed
{$IFDEF ONDATA_SPEEDTEST}
o:= MilliSecondOfTheDay(Now);
for j:= 1 to 20 do
{$ENDIF ONDATA_SPEEDTEST}
if b and CheckWellhoferReady and FKeyboardReady and (not OnDataReadBusy) then   //check with onkeyup event for keys to be processed
  begin
  OnDataReadBusy:= True;                                                        //set busy state now
  {$IFDEF JwaWinBase}
  GetCpuUsage(False,10);
  {$ENDIF}
  LogTabMemo.Lines.BeginUpdate;
  {$IFDEF THREAD_PLOT}
  for p:= pMeasured to pBuffer do PlotFillThread[p]:= nil;                      //Unlike Delphi7, a nil value is not guaranteed, but is needed later when not used.
  {$ENDIF THREAD_PLOT}
  ClearAllCx;                                                                   //clear results panel
  ShowMenuItemStatus(Sender);                                                   //when sender is menuitem its state will be shown on the statusbar
  if RawDataEditor.Modified then                                                //if the raw data are changed, full detection is forced
    DetectedFileType      := twcUnknown;
  PlotScaleMin            :=  0;                                                //reset plot scaling
  PlotScaleMax            :=  0;
  DoseConvTableNr         := -1;                                                //unload dose conversion table
  MeasNormAdjustFactor    := x;                                                 //update actual value of NormAdjustFactor
  SelectedPlot            := pMeasured;
  SelectPlot              := True;
  MeasIon2DoseItem.Checked:= False;                                             //this item does not autocheck, checking it should be a one-time event
  with Engines[UsedEngine],wCurveInfo do  //===================== current engine is Engines[UsedEngine] ==========================
    if IsValid then                                                             //do nothing if still busy
      begin
      if Sender is TMenuItem then with Sender as TMenuItem do
        begin
        if RadioItem then
          Checked:= True;
        UpdateSettings(Sender);                                                 //this sets also the normalisation of wellhofer
        MeasNormAdjustEdit.Visible:= False;                                     //reset visibility of MeasNormAdjustEdit
        if Sender=ProcessSetFFFItem then                                        //FieldType changed by user
          begin
          if ProcessSetFFFItem.Checked then                                     //when checked new FieldType=fcFFF
            begin
            if wSource[dsMeasured].twSetFieldType=fcStandard then wSource[dsMeasured].twSetFieldType:= fcFFF;
            end
          else                                                                  //otherwise FieldType=fcStandard
            if wSource[dsMeasured].twSetFieldType=fcFFF      then wSource[dsMeasured].twSetFieldType:= fcStandard;
          end;
        ResetAnalysis;                                                          //because FieldType is changed analysis results are invalid
        end;
      ProcessSetFFFItem.Checked:= wSource[dsMeasured].twSetFieldType=fcFFF;     //reflect current FFF state also in menu
      tmpX                     := GetDisplayedPositionScale;
      Br                       := Format('%s%0.2f',[wSource[dsMeasured].twBeamInfo.twBModality,Energy]);
      MeasMenuClick(Sender);                                                    //update measurement menu
      if not Freeze then                                                        //if current engine is not in frozen state
        wApplyUserLevel:= MeasUserDoseItem.Checked;                             //keep unchanged otherwise
      if assigned(UseDoseConvTable) then
        i:= Length(UseDoseConvTable)
      else
        i:= 0;
      while (i>0) and (DoseConvTableNr<0) do                                    //try to find applicable dose conversion table
        begin
        Dec(i);
        with UseDoseConvTable[i] do
          if (DCDoseBox.Checked or DCBgBox.Checked) and ((DCModalityBox.Text='') or (DCModalityBox.Text=Br)) then
            DoseConvTableNr:= i;
        end;
      if (not Freeze) and MeasMirrorItem.Checked and (ScanType in twcHoriScans) then
        Mirror(dsMeasured,dsMeasured,wSource[dsMeasured].twCenterPos_cm);       //execute mirror of measurement when applicable
      if (ProcessMirrorMeasRefItem.Checked) and (ScanType in twcHoriScans) then //option "Mirrored measurement as Reference"
        begin
        if (not Freeze) then
          begin
          SetReferenceOrg(dsMeasured,True);                                     //store measurement as RefOrg
          if not wSource[dsMeasured].twMirrored then
            Mirror(dsMeasured,dsRefOrg,wSource[dsMeasured].twCenterPos_cm);     //execute mirror of measurement to RefOrg
          Analyse;
          end;
        LoadReference;                                                          //is by definition identical and therefore will load
        end {mirror}
      else
        if ProcessMergeItem.Checked and ProcessSetMergeSourceItem.Checked and   {***********merging**********}
           Merge(dsUnrelated,dsMeasured,
                 ifthen(ScanType in twcVertScans,MergePDDShift_cm.Value,MergeProfShift_cm.Value),
                 MergeMatchCheckBox.Checked,
                 MergeScaleOverlapCheckBox.Checked) then
          begin
          if RefAutoLoadItem.Checked then
            LoadReference('',ProcessSetTempRefItem.Checked);                    //smart reload reference
          ProcessMergeItem.Checked:= False;
          end; {merge}
      if not Freeze then
        begin
        AcceptMissingPenumbra:= MeasMissingPenumbraItem  .Checked;              //pass setting to engine
        AcceptZeroSteps      := MeasZeroStepsItem        .Checked;              //pass setting to engine
        wApplySigmoidToBuffer:= ProcessSigmoid2BufferItem.Checked;              //pass setting to engine
        Analyse; //first time dsMeasured is analysed; all options have been set, therefore nothing needs to be forced, FFF detection is needed to find FFF
        end;
      AppliedFieldClass         := wSource[dsMeasured].twSetFieldType;          //from here you can depend on the correct AppliedFieldClass value
      MeasSymCorrectItem.Checked:= Ft_SymCorrCheckBox[AppliedFieldClass,dsMeasured ].Checked;            //sync menuitems
      RefSymCorrectItem .Checked:= Ft_SymCorrCheckBox[AppliedFieldClass,dsReference].Checked;
      if not Freeze then
        for d:= dLow to dTemp do
          if twcDoseLevelNames[d]=Ft_EdgeMethodCombo[AppliedFieldClass,fcPrimary].Text then
            AppliedEdgeRefNorm:= d;
      if (ScanType in twcVertScans) or ProcessSyntheticProfile.Checked then     //smart zooming rule
        begin
        LastProfileZoomState:= ViewZoomItem.Checked;
        ViewZoomItem.Checked:= not ViewAutoUnzoomPDDitem.Checked;
        end
      else
        begin
        ViewZoomItem.Checked:= LastProfileZoomState and not (FFFfeatures and ViewAutoUnzoomFFFitem.Checked);
        if (not Freeze) and ReferenceValid then
          Analyse(dsReference);
        end; {if (ScanType in twcVertScans) ..}
      try
        Er         := ExtractFileName(MakeCurveName);
       except
        Er         := '?';
       end;
      Br           := Er;
      if AddDateTimeCheckBox.Checked then                                       //add datetime to curve information
        begin
        DateTimeToString(Tr,'_yyyymmdd_hhnn',wSource[dsMeasured].twMeasDateTime);
        Insert(Tr,Br,Pos('.',Br));
        end;
      FileSaveDialog.FileName:= Br;
      ClearScreen(Sender);
      DoCorrections(dsMeasured,MeasBackgroundCorrItem.Checked);                 //background correction on measurement when applicable
      if MeasMayneordItem.Checked and (ScanType in twcVertScans)                     and   //Mayneord transformation
        (not Freeze)                                                                 and
         Mayneord(MayneordSSD1_cm.Value,MayneordSSD2_cm.Value,MayneordDmax_cm.Value) and
         (RefAutoLoadItem.Checked or ProcessSetTempRefItem.Checked)                  then  //test order critical
        LoadReference('',ProcessSetTempRefItem.Checked);
      ViewReferenceItem.Enabled:= ReferenceValid;                               //set state of ViewReferenceItem
      if ReferenceValid then
        DoCorrections(dsReference,RefBackgroundCorrItem.Checked);               //background correction on reference when applicable
      if SpecialMode[3].MenuItem.Checked {$IFDEF SelfTest}and (SelfTestLevel=0){$ENDIF} then
        DoSpecialMode3;
      Analyse;                                                                  //final check if analysis is completed
      if not wSource[dsMeasFiltered].twValid then                               //publishresults relies on filtered version, should be ok
        QuadFilter(0,dsMeasured,dsMeasFiltered,True);                           //generate dsMeasFiltered if not available
      if not ViewReferenceItem.Enabled then
        CopyCurve(dsMeasFiltered,dsCalculated);                                 //start calculated as filtered version of measurement
      if Engines[UsedEngine].ScanType=snAngle then  //----------------------------- Axis swapping ----------------------------------------
        SwapAxis:= False                            //applied on nowadays outdated OmniPro v6 definition of scanangle and axis directions
      else
        case ScanLeftSide[1] of
         'G': SwapAxis:= SwapGTcheckbox.Checked;                                //Omnipro v6: G to T, now mostly from T to G, therefore standard is Checked
         'A': SwapAxis:= SwapABcheckbox.Checked;                                //Omnipro v6: A to B, still standard direction
         'U': SwapAxis:= SwapUDcheckbox.Checked;                                //Omnipro v6: U to D, still standard direction
         else     SwapAxis:= SwapLRcheckbox.Checked;
         end;
      with wSource[dsMeasured],twBeamInfo do        //----------------------------- Tr: FileName ------------------------------------------
        begin
        if not DataFromEditor               then Tr:= FileName                  //filename taken from wellhofer-object
        else if EditorFileName<>DefaultName then Tr:= EditorFileName            //try editor filename
             else
               try
                 Tr:= GetCurveIDString; {created from data}
                 if ExtractFilename(Tr)=Er then
                   Tr:= '';
                 if (not ErrorState) and (Tr<>'') then
                   SetMessageBar(Format('%s (%s » %s)',[LastMessage,ExtractFileName(Tr),Er]));
                except
                 Tr:= '';
                end;
        Set_PlotScaling;                                                        //set scaling for measured----------------------------------
        b          := False;
        FFFfeatures:= twFFFdetected;
        with DataPlot do
          begin                    //------------------Br: bottom axis title handling --------------------------------------------
          if not ViewBottomAxisAlwaysBlack.Checked then                         //wUserAxisSign is applied in Prepareprofile}
            for m:= Inplane to Beam do b:= b or ((wUserAxisSign[m]<0) and twDesVaryingAxis[m]);
          if (Round(FieldGT_cm)=FieldGT_cm) and (Round(FieldAB_cm)=FieldAB_cm) then i:= 0
          else                                                                      i:= 1;
          Tr:= ifthen(Scantype=snAngle,Format('%0.0f° ',[ScanAngle]),'')+
                                       twDesTypeString                  +
                                       ifthen(FieldLength_cm>0,Format(' %0.*fx%0.*f',[i,FieldGT_cm,i,FieldAB_cm]),
                                       '');
          PositionLabel.Caption:= ifthen(ScanType=snPDD,PosLabelDepthText,PosLabelPosText)+':';
          PlotScaleMax         := twMaxValue*PlotScaling*DefAxisMaxExtension;
          if LeftAxis.Range.Min>PlotScaleMax then
            LeftAxis.Range.Min:= PlotScaleMax/2;
          if (not (Sender is TMenuItem)) and (CurveString<>GetCurveIDString) then
            LeftAxis.Range.Max:= PlotScaleMax;
          if Energy=0 then
            Er:= ''
          else
            begin
            if Energy>1 then  begin  tmpE:= Energy;       Er:= 'M';  end
            else              begin  tmpE:= Energy*1000;  Er:= 'k';  end;
            if AppliedFieldClass=fcElectron then
              Er+= 'e';
            Er:= Format(' %0.0f %sV,',[tmpE,Er]);
            end;
          Br:= Format('%s%s %s%s%s%s%s%s%s%s%s',
                      [ifthen(Linac=twcDefUnknown,'',Linac+','),
                       Er,Tr,
                       ifthen((ScanType in [snGT,snAB,snFreescan,snAngle]) and
                              (not twDesVaryingAxis[Beam]) and (twVector_ICD_cm[ptFirst].m[Beam]>0),
                              Format(', '+DefDepthText,[twVector_ICD_cm[ptFirst].m[Beam]*tmpX])               ,''),
                       ifthen(AxisViewFileTypeCheckBox  .Checked,
                              ', '+Identity                                                                   ,''),
                       ifthen(AxisViewFieldTypeCheckBox .Checked,
                              ', '+twcFieldClassNames[AppliedFieldClass]                                      ,''),
                       ifthen(FFFfeatures and (AppliedFieldClass<>fcFFF),
                              ' (fff)'                                                                        ,''),
                       ifthen(AxisViewDetNameCheckBox   .Checked,
                              ', '+wDetectorInfo.twDetName.SubString(0,Round(AxisViewDetLength_num.Value))    ,''),
                       ifthen(AxisViewCommentsCheckBox  .Checked and (Length(wCurveInfo.twDesMeasComment)>0)  ,
                              ', '+wCurveInfo.twDesMeasComment.SubString(0,Round(AxisViewComLength_num.Value)),''),
                       ifthen(AxisViewCollAngleCheckBox .Checked,
                              Format(', '+DefAxisViewColl,[twBeamInfo.twBCollimator])                         ,''),
                       ifthen(AxisViewSSDCheckBox       .Checked,
                              Format(', SSD%0.0f'        ,[twSSD_cm*TmpX           ])                         ,'')
                                            ]);
          if tw2DoseConv      then
            Br+= ' ('+Def2Dose+ifthen(Length(twOD2doseFilm)>0,'/'+twOD2doseFilm,'')+')';  //add more text to Br
          if twBackground<>0  then
            Br:= Format('%s (%s)',[Br,DefSubtracted]);                                       //add more text to Br
          BottomAxis.Title.LabelFont.Color:= ifthen(SwapAxis and (not ViewBottomAxisAlwaysBlack.Checked),clRed,LeftAxis.AxisPen.Color);
          BottomAxis.Title.Caption        := Format('%-10s %s %10s  [%s]',[ifthen(SwapAxis,ScanRightSide,ScanLeftSide)+'<',Br,'>'+ifthen(SwapAxis,ScanLeftSide,ScanRightSide),GetPositionUnitsStg]);
          CurveString                     := GetCurveIDString;
          end; {bottomaxis--------------------------------------------------------------------------------------------}
        end; {with measured}
      if PDDfitCheckBox.Enabled and PDDfitCheckBox.Checked and
        (not SpecialMode[3].MenuItem.Checked) and (ScanType in twcVertScans) then  //=========pdd fit==============
        begin
        if not wSource[dsBuffer].twFittedData then
          PddFit(dsMeasured,dsBuffer);                                             //multithreaded, but not in background; must be done before publishresults
        wSource[dsBuffer].twValid:= wSource[dsMeasured].twPddFitData[NM_Primary].twFitValid;
        end;
      SyntheticMade:= (ProcessSyntheticProfile.Checked and
                       SyntheticProfile(dsMeasFiltered,dsRefFiltered,ProcessAutoscalingItem.Checked));
      if GammaInFieldLimits[AppliedFieldClass].Checked then AppliedGammaScope:= gGammaLimited
      else                                                  AppliedGammaScope:= gGammaComplete;
      if not wSource[dsMeasured].twSymCorrected then
        with MeasSymCorrectItem do                                              //set state of MeasSymCorrectItem
          begin
          Enabled:= ScanType in [snGT,snAB,snAngle];                            //only enabled for relevant scantypes
          if Enabled and Checked then
            CorrectSymmetry(dsMeasured);                                        //======symmetry correction of measurement========
          end;
      if RefSymCorrectItem.Enabled         and
         RefSymCorrectItem.Checked         and
         wSource[dsReference].twValid      and
         (ScanType in [snGT,snAB,snAngle]) and
         ((not SyntheticMade) or MeasSymCorrectItem.Checked) then
        CorrectSymmetry(dsReference,False);                                     //======symmetry correction of reference==========
      if (not Freeze)                                   and
         ((AppliedFieldClass=fcWedge) or
          (wSource[dsBuffer].twValid and (wSource[dsBuffer].twFilename<>wSource[dsMeasured].twFilename))) then
        Derive(AdjustedFilterWidthCm,dsMeasured,dsBuffer,True);                 //assure authenticity of buffer to hold derivative
      FillPlotSeries(pMeasured,dsMeasured,PlotScaling*MeasNormAdjustFactor);    //====================== plot MEASURED ===========
      {$IFDEF PANEL_SPEEDTEST}
          k:= MilliSecondOfTheDay(Now);
          for i:= 1 to 100 do PublishResults;
          MessageBar:= Num2Stg(MilliSecondOfTheDay(Now)-k,0)+' ms per 100 Panel builds';
      {$ENDIF PANEL_SPEEDTEST}
      PublishResults;              //========= publish now analysis results ====== perform division if needed ====================
      {$IFDEF THREAD_PLOT}
      PlotFillThread[pMeasured].WaitFor;
      {$ENDIF THREAD_PLOT}
      if SyntheticMade then        //========= handling of reference =============================================================
        ViewReferenceItem.Enabled:= ReferenceValid
      else
        begin
        if RefUseDivideByItem.Checked and ReferenceValid then  //========== use reference for division ===========================
          begin
          if (ScanType in twcVertScans) or (not RefNormaliseItem.Checked) then
            PlotScaling:= 1
          else
            try
              PlotScaling:= wSource[dsReference].twAppliedNormVal/wSource[dsMeasured].twAppliedNormVal;
             except
              PlotScaling:= 1;
             end;
         {$IFDEF DIVIDE_SPEEDTEST}
          k:= MilliSecondOfTheDay(Now);
          for i:= 1 to 100 do Divide(dsMeasFiltered,dsReference,dsCalculated,ProcessAutoscalingItem.Checked,PlotScaling);
          MessageBar:= Num2Stg(MilliSecondOfTheDay(Now)-k,0)+' ms per 100 divides';
         {$ENDIF}
          if Divide(dsMeasFiltered,dsReference,dsCalculated,ProcessAutoscalingItem.Checked,PlotScaling) then //division with user dependent scaling
            begin
            with wSource[dsCalculated] do
              try
                twPlotScaling:= 100.0/ifthen(twRelNormValue>0,twRelNormValue,twMaxValue);
               except
                twPlotScaling:= 1;
               end;
            HistogramTab.TabVisible:= True;
            end {divide}
          else
            SetMessageBar('Divide failed',2);
          end {ReferenceDivideByItem}
        else if RefUseGammaItem.Checked and wSource[dsReference].twValid then   //== use reference for gamma analysis ============
          begin
          PlotScaling:= MeasNormAdjustFactor; //*wSource[dsReference].twAppliedNormVal/wSource[dsMeasured].twAppliedNormVal;
         {$IFDEF GAMMA_SPEEDTEST}
          k:= MilliSecondOfTheDay(Now);
          for i:= 1 to 100 do
            begin
            ResetAnalysis(Calculated);
            GammaAnalysis(Measured,Reference,Calculated,False,gGammaComplete,ProcessAutoscalingItem.Checked,PlotScaling);
            end;
          MessageBar:= Num2Stg(round((MilliSecondOfTheDay(Now)-k)/100),0)+' ms per calculation';
         {$ENDIF}
          GammaAnalysis(dsMeasured,dsReference,dsCalculated,False,AppliedGammaScope,ProcessAutoscalingItem.Checked,PlotScaling,True);
          HistogramTab.TabVisible:= True;
          end {referencegammaitem}
        else if RefUseAddToItem.Checked then                                    //== use reference for addition ==================
          begin
          Add(dsMeasured,dsReference,dsCalculated,
                         ifthen(RefNormaliseItem.Checked,
                                wSource[dsMeasured].twAppliedNormVal/
                                ifthen(wSource[dsReference].twAppliedNormVal>0,
                                       wSource[dsReference].twAppliedNormVal,
                                       Math.Max(1,wSource[dsMeasured].twAppliedNormVal)),
                         1));
          HistogramTab.TabVisible:= True;
          end {refuseaddtoitem}
        end;                                                                    //------- end of reference handling -------
      if (SpecialMode[2].MenuItem.Checked) {$IFDEF SelfTest}and (SelfTestLevel=0){$ENDIF} then
        DoSpecialMode2;
      with wSource[dsCalculated] do                //================= plot CALCULATED ===========================================
        if twValid then
          begin
          PlotSeries[pCalculated].Active:= ViewCalculatedItem.Checked;
          if ViewCalculatedItem.Checked then
            begin
            if twIsGamma then
              begin
              PlotScaling  := 1;
              twPlotScaling:= 1;
              end
            else if twIsRelative then
              begin
              Set_PlotScaling(dsCalculated);
              PlotScaling:= PlotScaling*MeasNormAdjustFactor;
              end
            else
              begin
              twPlotScaling:= wSource[dsMeasured].twPlotScaling;
              PlotScaling  := twPlotScaling*MeasNormAdjustFactor;
              end;
            if CalcPostFilterItem.Checked and (wSource[dsCalculated].twFilterString='') then
              QuadFilter(-1,dsCalculated);
            SetPlotDate(pCalculated,ifthen(Length(wSource[dsCalculated].twFilterString)>0,twFilterString+'(','')+
                                           wSource[dsCalculated].twDataHistoryStg+
                                           ifthen(Length(wSource[dsCalculated].twFilterString)>0,')',''));
            FillPlotSeries(pCalculated,dsCalculated,PlotScaling,twIsGamma and GammaInFieldLimits[AppliedFieldClass].Checked);
            end;
          end
        else
          SetMessageBar('Calculated invalid',2);
      with wSource[dsReference] do if twValid and ViewReferenceItem.Enabled and ViewReferenceItem.Checked then
        begin                                //================= plot REFERENCE ==================================================
        if ProcessSetTempRefItem.Checked then
          SetMessageBar(Format(TempRefText,[twDevice,twMeasTime]));
        Set_PlotScaling(dsReference);
        FillPlotSeries(pReference,dsReference,PlotScaling);
        end;
      if MeasMirrorToBufferItem.Checked and (not Freeze) and (ScanType in twcHoriScans) then
        begin
        Mirror(dsMeasured,dsBuffer,wSource[dsMeasured].twCenterPos_cm);
        Set_PlotScaling(dsMeasured);
        end; {end of plot reference}
      with wSource[dsBuffer] do if twValid {and (wSource[dsMeasured].twIsWedgedProfile or ViewBufferItem.Checked)} then
          begin
          FastScan(dsBuffer,False);         //================= plot BUFFER ====================================================
          Set_PlotScaling(dsBuffer);
          if twFittedData then
            begin
            ViewBufferItem.Checked                := True;
            PlotSeries[pBuffer].AxisIndexY        := DefChartAxL;
            DataPlot.AxisList[DefChartAxR].Visible:= True;
            ErrorSeries.Active                    := True;
            tMin                                  := 100;
            tMax                                  := 100;
            tmpS                                  := GetQfittedValue(wSource[dsMeasured].twMaxPos_cm);
            x                                     := GetDisplayedPositionScale;
            with twpddFitData[{$IFDEF SHOW_X_FIT}NM_Extrapolation{$ELSE}NM_Primary{$ENDIF}] do
              if twFitValid then
                for i:= NearestPosition(twFitLow_cm,dsBuffer) to twDataRange[ptLast] do
                  try
                    if abs(wSource[dsMeasured].twData[i])<0.00001 then
                      tmpE:= 0
                    else
                      tmpE:= tmpS*NMpddmodelResult(dsBuffer,
                                                   {$IFDEF SHOW_X_FIT}NM_Extrapolation{$ELSE}NM_Primary{$ENDIF},
                                                   twPos_cm[i])/wSource[dsMeasured].twData[i];
                    tMin:= Math.Min(tmpE,tMin);
                    tMax:= Math.Max(tmpE,tMax);
                    ErrorSeries.AddXY(twPos_cm[i]*x,tmpE);
                   except
                   end;
            end
          else
            begin
            if BordersValid(dsMeasured,dInflection) and ProcessSigmoid2BufferItem.Checked and ApplySigmoidPenumbraFit(dsMeasured,dsBuffer) then
              PlotScaling:= twPlotScaling
            else
              begin
              if not (twIsDerivative and (twRelatedSource in [dsMeasured,dsMeasFiltered]))  then    //speed up things
                Derive(-1,dsMeasured,dsBuffer);
              Set_PlotScaling(dsBuffer);
              twPlotScaling:= PlotScaling;
              end;
            end;
          FillPlotSeries(pBuffer,dsBuffer,PlotScaling);
          end; {end of plot buffer}
      {$IFDEF THREAD_PLOT}
      for p:= pMeasured to pBuffer do                                           //wait for any running threads
        if assigned(PlotFillThread[p]) then
          begin
          PlotFillThread[p].WaitFor;
          try
            PlotFillThread[p].Free;
           except
            ExceptMessage(':OnDataRead:ht!');
           end;
          end;
      {$ENDIF THREAD_PLOT}
      if (not ViewReferenceItem.Enabled) and wSource[dsReference].twValid then  //valid files that fail as ref are made invisible
        begin
        if LogLevelEdit.Value<=1 then
          SetMessageBar('Increase loglevel to get more details.')
        else
          ReportDifferences(dsMeasured,dsReference);
        SetMessageBar(Format(CurveStringsDif,[CompressedFilename(wSource[dsReference].twFileName),
                                              GetCurveIDString(dsReference),
                                              GetCurveIDString]));
        end;
      if Length(PriorityMessage)>0 then
        SetMessageBar(PriorityMessage);
      end; {isvalid, usedengine ================================================================================}
  if Sender=nil then                                                            //the sender is nilled for calls through clipboard
    begin
    if Enabled then
      SetFocus;
    SetForegroundWindow(Handle);
    end;
  if DataPlot.IsZoomed then
    DataPlot.LogicalExtent:= DataPlot.GetFullExtent;  //reset zooming of chart itself; see https://wiki.freepascal.org/TAChart_documentation#Extents_and_margins
  AutoZoom;                     //====================== set axis and indicators ===============================
  SmartScaleElectronPDD(Sender);
  DataChanged:= False;
  {$IFDEF ONDATA_SPEEDTEST}
  MessageBar:= Format('%0.0f ms per run of analysis and presentation (%d)',[(MilliSecondOfTheDay(Now)-o)/j,j]);
  {$ENDIF ONDATA_SPEEDTEST}
  StatusBar.Panels[1].Text:= twcFieldClassNames[AppliedFieldClass];
  if Engines[UsedEngine].wSource[dsMeasured].twOriginalFormat in twcMultiFiles then
    StatusBar.Panels[2].Text:= Format(' #%d',[Engines[UsedEngine].wMultiScanNr])
  else
    StatusBar.Panels[2].Text:= Format(' %dp',[Engines[UsedEngine].GetNumPoints]);
  {$IFDEF JwaWinBase}
  GetCpuUsage;
  {$ENDIF}
  OnDataReadBusy:= False;
  end; {read}
end; {~ondataread}


//Convert a TWellhoferData twcDataSource to the equivalent graphical element (if available)
function TAnalyseForm.LinkPlotItem(aSource:twcDataSource): PlotItems;
begin
case aSource of
  dsCalculated: Result:= pCalculated;
  dsReference : Result:= pReference;
  dsBuffer    : Result:= pBuffer;
 else           Result:= pMeasured;
 end
end; {~linkplotitem}


{28/07/2020 Called from FormCreate and SelfTest}
procedure TAnalyseForm.SetBasicDefaults;
begin
Ft_EdgeMethodCombo[fcStandard,fcPrimary  ].ItemIndex:= 3;                       //d50
Ft_EdgeMethodCombo[fcElectron,fcPrimary  ].ItemIndex:= 3;                       //d50
Ft_EdgeMethodCombo[fcWedge   ,fcPrimary  ].ItemIndex:= 7;                       //dDerivative
Ft_EdgeMethodCombo[fcSmall   ,fcPrimary  ].ItemIndex:= 3;                       //d50
Ft_EdgeMethodCombo[fcElectron,fcFallBack ].ItemIndex:= 7;                       //dDerivative
Ft_EdgeMethodCombo[fcFFF     ,fcFallBack ].ItemIndex:= 2;                       //d20
Ft_SymCorrCheckbox[fcStandard,dsReference].Checked  := True;
Ft_DetectionCheckbox[fcStandard          ].Enabled  := False;
Ft_DetDiagonalCheckbox[fcSmall           ].Enabled  := False;
Ft_DynPenumbraCheckbox[fcFFF             ].Checked  := True;
Ft_DynPenumbraCheckbox[fcStandard        ].Checked  := False;
MeasZeroStepsItem                         .Checked  := True;
MeasGenericToPDDItem                      .Checked  := False;
RefDeviceSpecificItem                     .Checked  := True;
AdvancedModeItem                          .Checked  := True;
SimpleModeItem                            .Checked  := True;
MeasNormAdjustEdit                        .Visible  := False;
PlotScaleMin                                        := 0;
PlotScaleMax                                        := 100;
ZoomRange                                           := DefZoomRange;
CursorPos_cm                                        := 0;
end; {~setbasicdefaults}


{$IFDEF LCL_2-3_Up}
//linked to OnMarkToText event of right axis only to improve alignment of labels
{31/05/2020 OnMarkTiText event}
{21/02/2021 replacement OnGetMarkText event for future deprecated OnMarkToText event}
procedure TAnalyseForm.RightAxisGetMarkText(Sender   : TObject;
                                                    var AText: String;
                                                    AMark    : Double);
begin
AText:= Format('%s%6.2f',[ifthen(AMark<100,' ',''),AMark]);
end; {~rightaxisgetmarktext}
{$ELSE}
//linked to OnMarkToText event of right axis only (at runtime) to improve alignment of labels
{31/05/2020}
procedure TAnalyseForm.AxisMarkToText(var AText: String;
                                      AMark    : Double);
begin
AText:= Format('%s%6.2f',[ifthen(AMark<100,' ',''),AMark]);
end; {~axismarktotext}
{$ENDIF}


//add default display rules to results panel
{05/01/2018}
{16/01/2018 ClearAllCx}
{17/01/2018 added energy selector}
{20/01/2018 bonus rules}
{22/01/2018 assumes CxDefaultRowCnt=9}
{28/01/2018 added annot:s to min/max in-field}
{20/01/2018 symmetry rul split for DefaultSSD_cm/conventional}
{08/10/2018 implementation of addmode}
{29/03/2022 added gamma pass rate}
procedure TAnalyseForm.SetDefaultPanel(Sender:TObject);

  procedure AddFormattedElement(aID,acol,arow           :Integer;
                                part1,alabel,part2,part3:String);
  begin
  if not ((PanelElements.AddMode=1) and PanelElements.IDexists(aID)) then
    PanelElements.AddElement(Format('b%d,%d,%s,"%s",%s,%d,%d,%s',
                                    [BMBuildNumber,aID,part1,alabel,part2,acol,arow,part3]),@SetMessageBar);
  end;

 (* AnnotationType=(pa_synthetic=1,pa_symmetric=2,pa_fitted=3,pa_fff=4,pa_normdif=5,pa_ssd=6,pa_centered=7,pa_resampled=8,pa_shifted=9,pa_edge=10,pa_RDD)
    linked to DefAnnotTypeString='!sfFnzcrSeR', both lists indexed on 1
    twcDataSource   =(dsMeasured=0,dsMeasFiltered,dsCalculated,dsReference,dsBuffer,dsRefOrg,dsUnrelated); default:-1 *)
begin
if Sender=ViewStandardPanelsetup then
  PanelElements.AddMode:= -1;
if PanelElements.AddMode<1 then
  PanelElements.Clear; //id,col,row,curve sel,eval.type,multiplier,errorval,"label",    deci,unit,  mod,energy,scan type[,cond:-][,annot:-][,color:-]
AddFormattedElement( 1,0,0,'-1,n,1,0'      ,DefNormValText                             ,'2,'    ,'XEPO,0,A,annot:rcnS');                {norm value}
AddFormattedElement( 2,0,1,'-1,N,1,9e9'    ,DefNormPosText                             ,'2,'    ,'XEPO,0,A,annot:rcnS');                {norm position}
AddFormattedElement( 3,0,2,'-1,m,1,-9e9'   ,DefMaximumText                             ,'1,%'   ,'XEPO,0,A,annot:rnS');                 {max value}
AddFormattedElement( 4,0,3,'-1,f,1,-1'     ,'Abs'+DefFlatness                          ,'1,%'   ,'XEPO,0,H,annot:rzs,cond:-F,color:n'); {flatness conventional}
AddFormattedElement( 5,0,3,'-1,f,1,-1'     ,'Abs'+DefFlatness                          ,'2,%/cm','XP,0,H,annot:rzs,cond:F,color:n');    {flatness DefaultSSD_cm}
AddFormattedElement( 6,0,4,'2,d,1,-1'      ,'Rel'+DefFlatness                          ,'1,%'   ,'XEPO,0,H,annot:rzs,cond:d');          {rel. flatness any profile}
AddFormattedElement( 7,0,5,'-1,s,1,0'      ,DefSymNCS                                  ,'1,%'   ,'XEPO,0,H,annot:rzs,cond:-F');         {NCS8 symmetry}
AddFormattedElement( 8,0,5,'-1,l,1,0'      ,DefSymNCS                                  ,'1,%'   ,'XEPO,0,H,annot:rzs,cond:F');          {elevation as symmetry}
AddFormattedElement( 9,0,6,'-1,S,1,9e9'    ,'extended symmetry'                        ,'1,%'   ,'XP,0,H,annot:rzs');                   {extended symmetry}
AddFormattedElement(10,0,7,'-1,+F,1,0'     ,LeftStr(DefMaximumText,3)+DefInFieldAreaTxt,'1,%'   ,'XEPO,0,H,annot:rnSs');                {max in-field}
AddFormattedElement(11,0,8,'-1,-F,1,0'     ,LeftStr(DefMinimumText,3)+DefInFieldAreaTxt,'1,%'   ,'XEPO,0,H,annot:rnSs');                {min in-field}
AddFormattedElement(12,1,0,'-1,c,1,9e9'    ,DefCenterText                              ,'2,'    ,'XEPO,0,H,annot:!rzcCS,color:c');      {profile center}
AddFormattedElement(13,1,1,'-1,T,1,9e9'    ,DefTopText                                 ,'2,'    ,'XEPO,0,H,annot:!rzcST,cond:F');       {top, DefaultSSD_cm}
AddFormattedElement(14,1,2,'-1,w,1,0'      ,DefWidthText                               ,'2,'    ,'XEPO,0,H,annot:!urzeS');              {profile width}
AddFormattedElement(15,1,3,'-1,-b,1,9e9'   ,DefLeftText                                ,'2,'    ,'XEPO,0,H,annot:!urzecS');             {profile L}
AddFormattedElement(16,1,4,'-1,+b,1,9e9'   ,DefRightText                               ,'2,'    ,'XEPO,0,H,annot:!urzecS');             {profile R}
AddFormattedElement(17,1,5,'-1,-p,1,0'     ,'L'                                        ,'2,'    ,'XEPO,0,H,annot:!rz,cond:-W');         {penumbra L}
AddFormattedElement(18,1,6,'-1,+p,1,0'     ,'R'                                        ,'2,'    ,'XEPO,0,H,annot:!rz,cond:-W');         {penumbra R}
AddFormattedElement(19,1,0,'-1,M,1,-9e9'   ,DefLabelDmax                               ,'2,'    ,'XPE,0,V,annot:!rfS');                 {pdd max}
AddFormattedElement(20,1,1,'-1,P10,100,-1' ,'-'                                        ,'2,'    ,'XP,0,V,annot:!rfRS*,color:n');        {pdd10}
AddFormattedElement(21,1,2,'-1,P20,100,-1' ,'-'                                        ,'2,'    ,'XP,0,V,annot:!rfRS*,color:n');        {pdd20}
AddFormattedElement(22,1,3,'-1,P20/10,1,-1','-'                                        ,'3,'    ,'XP,0,V,annot:!rfRS,color:n');         {pdd20/pdd10}
AddFormattedElement(23,1,1,'-1,D80,1,-1'  ,'-'                                         ,'2,'    ,'E,0,V,annot:!rfRS*,color:n');         {electron pdd d80}
AddFormattedElement(24,1,2,'-1,D50,1,-1'  ,'-'                                         ,'2,'    ,'E,0,V,annot:!rfRS*,color:n');         {electron pdd d50}
AddFormattedElement(25,1,8,'2,G0,1,0'     ,'Conf.Limit'                                ,'2,'    ,'XEPO,0,A,annot:rzs,cond:rg');         {gamma CL with default from settings tab}
AddFormattedElement(26,1,7,'2,V0,1,0'     ,'Pass Rate'                                 ,'0,%'   ,'XEPO,0,A,annot:rzs,cond:rg');         {gamma pass rate with default from settings tab}
{$IFDEF TEST_RULES}
AddFormattedElement(26,1,7,'-1,x+21,1,0'  ,'-'                                         ,'2,%'   ,'XEPO,0,H,annot:rzs');                 {Y+50}
AddFormattedElement(27,1,8,'-1,X+21,1,0'  ,'-'                                         ,'2,%'   ,'XEPO,0,H,annot:rzs');                 {Y+50}
AddFormattedElement(28,1,7,'-1,y+50,1,0'  ,'-'                                         ,'2, cm' ,'XEPO,0,V,annot:rzs');                 {Y+50}
AddFormattedElement(29,1,8,'-1,Y+50,1,0'  ,'-'                                         ,'2, cm' ,'XEPO,0,V,annot:rzs');                 {Y+50}
{$ENDIF TEST_RULES}
InitCxBlock(PanelElements.RowMax);
ClearAllCx(True);            {this will show all rules per position}
end; {setdefaultpanel}


{10/01/2017 code path for event}
{30/04/2021 added updatesettings}
procedure TAnalyseForm.SetCaption(Sender:TObject);
begin
UpdateSettings(Sender);
SetCaption;
end; {~setcaption}


{$push}{$warn 5092 off}
{09/06/2016 replaced SpecialModes boolean Active with tmenuitem MenuItem}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{17/09/2020 Freeze}
{18/09/2020 display also number of engines in use}
{02/12/2022 if assigned(SpecialMode[i].MenuItem) to avoid problems during formcreate}
procedure TAnalyseForm.SetCaption(Stg:String='');
var i: Integer;
    b: Boolean;
begin
Stg:= '-- '+Stg;
with Engines[UsedEngine] do
  if HistoryListCheckBox.Checked and IsValid and Freeze then
    Stg:= '*'+Stg;
if HistoryListCheckBox.Checked then
  Stg:= Format('%d/%d%s',[Succ(((Length(Engines)-UsedEngine+LoadEngine) mod Length(Engines))),Length(Engines),Stg]);
Stg:= Format('BistroMath %s --%s',[AppVersionString,Stg]);                      //var AppVersionString in wellhofer.pas
b  := False;
try
  for i:= 2 to NumSpecialModes do if assigned(SpecialMode[i].MenuItem) and SpecialMode[i].MenuItem.Checked then
    begin
    if not b then Stg:= Stg+' -specialmodes: '
    else          Stg:= Stg+';';
    Stg:= Format('%s%d',[Stg,i]);
    b  := True;
    end;
  except
  end;
Caption:= Stg;
end; {~setcaption}
{$pop}


(* Create labels for results panel, one per column and line; PanelElements[i] fills the label/value at set Col/Row.
  Multiple PanelElements migth use the same label. This is both efficient and clean. *)
{22/01/2018}
{13/08/2020 onclick}
{25/08/2020 CxResults with variable number of lines}
procedure TAnalyseForm.InitCxBlock(NewLineMax:Integer);
var i,j,k: Integer;
    c    : CxComponents;
begin
if NewLineMax>=0 then                                            {negative values not allowed}
  begin
  if NewLineMax<CxUsedLineMax then                               {less lines needed}
    if CxUsedLineMax>0 then
      for i:= NewLineMax to CxUsedLineMax do                     {for all lines...}
        for j:= 0 to CxMaxCol do                                 {for all columns...}
          for c:= CxTitle to CxValue do
            CxResults[i][j][c].Visible:= False;                  {...hide unneeded blocks, freeing shows to be problematic for ununderstood reasons}
  k:= Max(Length(CxResults),CxUsedLineMax);                      {k: actual number of created lines might be larger then lines in use}
  if NewLineMax>k then
    begin
    SetLength(CxResults,Succ(NewLineMax));                       {----set new length}
    for i:= k to NewLineMax do
      for j:= 0 to CxMaxCol do                                   {----CxResults}
        for c:= CxTitle to CxValue do
          begin
          CxResults[i][j][c]:= TLabel.Create(Self);              {----create additonal blocks}
          with CxResults[i][j][c] do
            begin
            Parent     := ResultsPanel;                          {positions and size handled in PlaceResultsLabels}
            Alignment  := taRightJustify;
            AutoSize   := False;
            ParentFont := True;
            HelpContext:= ResultsPanel.HelpContext;
            OnClick    := @LabelCopyClick;
            end;
          end;
      end;
  CxUsedLineMax:= NewLineMax;
  PlaceResultsLabels;
  ClearAllCx(True);                                              {enumerate all realted rules for each CXblock}
  end;
end; {~initcxblock}


//clear all panel display rules
{16/01/2018 ClearAllCx changed to object procedure}
{22/01/2018 CxUsedRowMax}
{30/01/2018 added panel rules mapping}
{31/01/2018 clDkGray}
procedure TAnalyseForm.ClearAllCx(MakeVisible:Boolean=False);
var i,j: Integer;
    c : CxComponents;
begin
for i:= 0 to CxUsedLineMax do for j:= 0 to CxMaxCol do
  for c:= CxTitle to CxValue do with CxResults[i][j][c] do
    begin
    Visible   := MakeVisible;
    Font.Color:= clBlack;
    Caption   := '';
    end;
if MakeVisible and (PanelElements.Count>0) then
  begin
  SetMessagebar(Format('%d panel elements configured',[PanelElements.Count]));
  for i:= 0 to Pred(PanelElements.Count) do
    with PanelElements.FElements[i] do
      if PCRvalid then
        with CXresults[PCRrow][PCRcol][CxTitle] do
          begin
          Font.Color:= clDkGray;
          Caption   := Caption+Num2Stg(PCRid)+chSpace;
          end;
  end;
end; {~clearallcx}


{=> ConfigLoadItem}
{13/01/2017}
procedure TAnalyseForm.SelectConfig(Sender:TObject);
var f,s,i: String;
begin
with FileOpenDialog do
  begin
  s         := Filter;                                                          //preserve settings of FileOpenDialog
  f         := FileName;
  i         := InitialDir;
  InitialDir:= CommonAppdata;                                                   //CommonAppdata is configurable and set in FormCreate
  Filter    := 'Ini files (*.ini)|*.ini';
  FileName  := Configname; //'*.ini';
  if Execute then
    ConfigLoad(Sender,FileName);
  InitialDir:= i;
  Filename  := s;
  Filter    := f;
  end;
end; {~selectconfig}


{13/01/2017 use configname, institute.ini will be changed to getconfigstg}
{19/01/2017 testfile}
{12/05/2022 display full path of config file}
{$push}{$warn 5092 off}
procedure TAnalyseForm.SetConfigName(AName   :String;
                                     TestFile:Boolean=False);

begin
if  TestFile or (AName='') or (AName<>ConfigName) then
  begin
  if (AName='') or (ExtractFilename(AName)=DefInstituteCfg) then
    AName:= GetConfigStg;
  if (not TestFile) or FileExists(AName) then
    begin
    ConfigName            := AName;
    AName                 := LowerCase(PresetToName(ExtractFileName(AName),True));
    ConfigSaveItem.Caption:= 'Save '+AName;
    ConfigReadItem.Caption:= 'Load '+AName;
    SetMessageBar('config='+ConfigName);
    end;
  end;
end; {~setconfigname}
{$pop}

//update renamed visual elements in old inifiles based on a repairfile
{02/06/2020}
{04/06/2020 added message, ReadSectionValues gets "key=value" lines}
{15/06/2020 show the number of changed elements}
{26/07/2020 support for multiple sections and removing keys}
{26/08/2020 ConfigRepairFound}
{09/05/2024 avoid FreeAndNil}
function TAnalyseForm.ConfigRepair(CF:TConfigStrings): Boolean;
var OldKey,NewKey,TargetSection: String;
    m                          : TMemIniFile;
    v                          : TStringList;
    i,j,k,p                    : Integer;
begin
i     := CF.ReadInteger(IniSectionName,Application.Title,0);
Result:= (not CF.ValueExists(IniSectionName,DefConfigRepairFile)) and (i<DefMinFPCbuild) and ConfigRepairFound;
if Result then for p:= 0 to 1 do
  begin
  TargetSection:= ifthen(p=0,IniSectionName,twcWellhoferKey);
  CF.WriteInteger(TargetSection,DefConfigRepairFile,1);                         //add name of reaparfile tot targetsection
  CF.WriteInteger(TargetSection,Application.Title,BMBuildNumber);               //add buildnumber to targetsection
  m:= TMemIniFile.Create(DefConfigRepairFile);
  v:= TStringList.Create;
  m.ReadSectionValues(TargetSection,v);                                         //v receives strings with "key=value" content for targetsection in file DefConfigRepairFile
  i:= v.Count;
  k:= 0;
  while i>0 do
    begin
    Dec(i);
    j     := Pos('=',v.Strings[i]);                                             //j:= pos of "="
    OldKey:= Copy(v.Strings[i],1,j-1);
    Inc(k);
    if CF.ValueExists(TargetSection,OldKey) then                                //OldKey from repairfile exists in config
      begin
      NewKey:= Copy(v.Strings[i],j+1);                                          //take new key starting at j+1
      if Length(NewKey)>0 then                                                  //if not empty copy value part of old key (no new key is legal)
        CF.WriteString(TargetSection,NewKey,CF.ReadString(IniSectionName,OldKey,''));
      CF.DeleteKey(TargetSection,OldKey);                                       //remove old key anyway
      end;
    end;
  m.Free;
  v.Free;
  if p=0 then
    SetMessageBar(Format('%s applied with build %s on %d elements',[DefConfigRepairFile,CF.ReadString(TargetSection,Application.Title,'?'),k]));
  end;
end; {~configrepair}


{=> ConfigReadItem}
procedure TAnalyseForm.ConfigLoad(Sender:TObject);
begin
ConfigLoad(Sender,ConfigName);
end; {~configload}


procedure TAnalyseForm.ConfigLoad(Sender   :TObject;
                                  AFileName:String;
                                  ForceInit:Boolean=False);
var S: TMemoryStream;
    b: Boolean;
begin
if Length(AFileName)=0 then
  AFileName:= GetConfigStg;
S:= TMemoryStream.Create;
b:= FileExists(AFileName);
if b then
  begin
  SetConfigName(AFileName);
  S.LoadFromFile(AFileName);
  end;
if b or ForceInit then
  ConfigLoad(S);
try
  S.Free
 except
  ExceptMessage('ConfigLoad:S!');
 end;
end; {~configload}


{22/07/2015 mayneord-related items}
{04/08/2015 ViewLeftAxisLowZero}
{09/06/2016 replaced SpecialModes boolean Active with tmenuitem MenuItem}
{07/07/2016 added FFFcenterRadius_cm,FFFInFieldExt_cm}
{25/10/2018 SpecialModeValues}
{29/04/2020 MinClipBoardBytes added}
{02/06/2020 ConfigRepair,WriteMenuShortCuts}
{28/08/2020 removed FFFcenterRadius_cm}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{15/09/2020 HistoryList}
{08/12/2020 ShowLockItemCheckBox}
{$push}{$warn 5058 off}
procedure TAnalyseForm.ConfigLoad(AStream:TStream);
var CF : TConfigStrings;
    t  : TStringList;
    i,j: integer;
    key: string;

  procedure ReadDialog(ADialog:TOpenDialog);
  begin
  with CF,ADialog do
    begin
    DefaultExt := ReadString (Name,'DefaultExt','txt'      );
    InitialDir := ReadString (Name,'InitialDir',InitialDir );
    FileName   := ReadString (Name,'FileName'  ,DefaultName);
    FilterIndex:= ReadInteger(Name,'Index'     ,FilterIndex);
    end;
  end;

  procedure ReadSpecialModes(ASection:String);
  var i,j: Integer;
  begin
  with CF do
    for i:= 1 to NumSpecialModes do with SpecialMode[i] do
      for j:= 1 to NumSpecialModePar do
        begin
        try
          Fpar[j]:= ReadFloat(ASection,Format('SpecialMode[%d]Float[%d]',[i,j]),Fpar[j]);
         except
          Fpar[j]:= 0;
         end;
        try
          Spar[j]:= ReadString(ASection,Format('SpecialMode[%d]String[%d]',[i,j]),Spar[j]);
         except
          Spar[j]:= '';
         end;
        end;
  end;

begin
CF:= TConfigStrings.Create;
with CF do
  begin
  t:= TStringList.Create;
  t.LoadFromStream(AStream);
  SetStrings(t);
  t.Clear;
  ConfigRepair(CF);
  Engines[UsedEngine].ReadConfig(CF);                                           //read wellhoferdata first
  GetWellhoferValues;                                                           //and transfer back from wellhofer
  ReadControl(Self,IniSectionName);
  WriteMenuShortCuts:= ReadBool(IniSectionName,'ShortCuts',False);
  LogTabMemo.Tag    := EnsureRange(ReadInteger(IniSectionName,DefLogMaxLines,500),20,10000);
  for i:= 1 to NumSpecialModes do ShortRead(IniSectionName,SpecialMode[i].MenuItem);
  ReadInteger(IniSectionName,'MinClipBoardBytes',MinClipBoardBytes);
  ShortRead(IniSectionName,AdvancedModeStartCheckBox);
  ShortRead(IniSectionName,ShowLockItemCheckBox);
  ShortRead(IniSectionName,HistoryListCheckBox);
  ShortRead(IniSectionName,HistoryListFreezeCheckBox);
  ShortRead(IniSectionName,HistoryListSize_num);
  ShortRead(IniSectionName,PipsPixelSize_mm);
  ShortRead(IniSectionName,ConfigAutoSaveItem);
  ShortRead(IniSectionName,FileConvSourcePath);
  ShortRead(IniSectionName,FileConvDestinationPath);
  ShortRead(IniSectionName,FileConvNameMask);
  ShortRead(IniSectionName,FileConvSourceListBox);
  ShortRead(IniSectionName,FileConvLowerCase);
  ShortRead(IniSectionName,FileConvMakeFileName);
  ShortRead(IniSectionName,FileConvOverWrite);
  ShortRead(IniSectionName,FileConvSourceRecursive);
  ShortRead(IniSectionName,ShowWarningCheckBox);
  ShortRead(IniSectionName,AddDateTimeCheckBox);
  ShortRead(IniSectionName,BadPenumbraWidth_cm);
  ShortRead(IniSectionName,AutoSetDecPointCheckBox);
  ShortRead(IniSectionName,AutoDecPointList);
  ShortRead(IniSectionName,InventoryDirBox);
  ShortRead(IniSectionName,FileConvSourcePath);
  ShortRead(IniSectionName,FileConvDestinationPath);
  if SectionExists(AliasText) then
    begin
    ReadSectionValues(AliasText,t);
    Engines[UsedEngine].AddDefaultAliasList(t);
    with t do if Count>0 then
      begin
      for i:= 0 to Pred(Count) do
        begin
        key:= Names[i];
        if not AliasListEditor.FindRow(key,j) then
          AliasListEditor.InsertRow(key,Values[key],True);
        end;
      AliasListEditor.Refresh;
//      Engines[UsedEngine].LoadAliasList(AliasListEditor.Strings);
      end;
    end;
  ZoomRange:= Clip(ReadFloat(IniSectionName,ZoomText,DefZoomRange),1.005,DefZoomRange+0.2);
  ReadDialog(FileOpenDialog);
  ReadDialog(FileSaveDialog);
  PresetLoad(CF);
  ReadSpecialModes(IniSectionName);
  if SectionExists('SpecialModes') then
   begin
   ReadSectionValues('SpecialModes',SpecialModeValues);
   ReadSpecialModes('SpecialModes');
   end;
  FillCheckListCombo;                                                           //transfers modalitydata, just read above here, to UseDoseConvTable
  for i:= 0 to OD2doseTableMax do if ValueExists(IniSectionName,DCname(0,i)) then
    begin
    while Length(UseDoseConvTable)<Pred(i) do
      UseDoseAddButtonClick(Self);
    with UseDoseConvTable[i] do
      begin
      ShortRead(IniSectionName,DCDoseBox    );
      ShortRead(IniSectionName,DCBgBox      );
      ShortRead(IniSectionName,DCModalityBox);
      ShortRead(IniSectionName,DCFilmTypeBox);
      ShortRead(IniSectionName,DCEdit       );
      end;
    end;
  AdvancedModeItem.Checked:= AdvancedModeStartCheckBox.Checked and AdvancedModeOk;
  SetWellhoferValues(Self);                                                     //first transfer settings to wellhofer
  if AdvancedModeOk then
    UImodeChange(Self);
  try
    t.Free
   except
    ExceptMessage('ConfigLoad:t!');
   end;
  try
    Free
   except
    ExceptMessage('ConfigLoad:CF!');
   end;
  end; {with CF}
SettingsTabExit(Self);
AdvancedSettingsTabExit(Self);
end; {~configload}
{$pop}


//variant on configload
procedure TAnalyseForm.PresetLoad(AFileName:String);
var CF : TConfigStrings;
begin
AFileName:= NameToPreset(AFileName,True);
if FileExists(AFileName) then
  begin
  if AFileName<>ConfigName then
    PresetName:= AFileName;
  CF:= TConfigStrings.Create(AFileName);
  PresetLoad(CF);
  try
    CF.Free
   except
    ExceptMessage('PresetLoad:CF1!');
   end;
  SetMessageBar('Preset: '+AFileName);
  end;
end; {~presetload}


//presetload is also called by configload(cf)
{21/09/2015 menumessagebar}
{15/12/2105 penwidth}
{18/03/2016 added ProcessIgnoreTUnameItem}
{04/12/2017 wInflectionSigmoid[fieldclass]}
{15/12/2017 extsysm}
{16/01/2018 panelelements included}
{30/05/2018 GammaLimitConventional,GammaLimitFFF)}
{31/05/2018 InsertOriginCheckBox}
{27/04/2020 gridcolor}
{05/05/2020 UIColorPanel}
{04/06/2020 read simplemodeitem}
{12/07/2020 EdgeMethodCombo}
{15/07/2020 GammaInFieldLimits,AppliedEdgeRefNorm}
{21/07/2020 ReadGroup}
{16/11/2020 FileMultipleInputItem}
{23/02/2021 readgroup ComboBox added}
{14/05/2021 readgroup(MeasRemappingBox) added}
{01/06/2021 ReadGroup(Ft_FFFDetectionGroupBox)}
{30/10/2022 ReadGroup(FocusPositionSetupGroupBox)}
{14/11/2022 support for TLabeledEdit}
{19/11/2022 OffAxisInclusionRange_cm}
{28/11/2022 specific call on exit for focuspositiontab}
{01/12/2022 added TRadioButton to readgroup}
procedure TAnalyseForm.PresetLoad(CF:TConfigStrings);
var b    : Boolean;
    i    : Integer;
    scan : twcScanTypes;
    angle: twcCardinalAngles;

  procedure RefCalcSubMenuRead(AMenu:TMenuItem);
  var i: Integer;
  begin
  for i:= 0 to Pred(AMenu.Count) do
    begin
    CF.ShortRead(IniSectionName,AMenu.Items[i]);
    if AMenu.Items[i].Checked then
      CalcSubMenuClick(AMenu.Items[i]);
    end;
  end;

  procedure ReadMenu(AMenu:TMenuItem);
  var i: Integer;
  begin
  for i:= 0 to Pred(AMenu.Count) do
    if AMenu.Items[i].Count>0 then
      ReadMenu(AMenu.Items[i])
    else
     {$IFDEF COMPILED_DEBUG}
      begin
     {$ENDIF}
      CF.ShortRead(IniSectionName,AMenu.Items[i]);
     {$IFDEF COMPILED_DEBUG}
      SetMessageBar(Format('%s %s: %s',[ifthen(CF.ValueExists(IniSectionName,AMenu.Items[i].Name),'found','missing'),AMenu.Items[i].Name,ifthen(AMenu.Items[i].Checked,CheckedText,UnCheckedText)]));
      end;
     {$ENDIF}
  end;

  procedure ReadGroup(AGroup:TWinControl);
  begin
  with AGroup do
    begin
    i:= ControlCount;
    while i>0 do
      begin
      Dec(i);
      if Controls[i] is TCheckBox then
        CF.ShortRead(IniSectionName,TCheckBox(Controls[i]))
      else if Controls[i] is TComboBox then
        CF.ShortRead(IniSectionName,TComboBox(Controls[i]))
      else if Controls[i] is TSpinEditEx then
        CF.ShortRead(IniSectionName,TSpinEditEx(Controls[i]))
      else if Controls[i] is TFloatSpinEditEx then
        CF.ShortRead(IniSectionName,TFloatSpinEditEx(Controls[i]))
      else if Controls[i] is TEdit then
        CF.ShortRead(IniSectionName,TEdit(Controls[i]))
      else if Controls[i] is TLabeledEdit then
        CF.ShortRead(IniSectionName,TLabeledEdit(Controls[i]))
      else if Controls[i] is TRadioButton then
        CF.ShortRead(IniSectionName,TRadioButton(Controls[i]));
      end;
    end;
  end;

begin
b                       := AdvancedModeItem.Checked;                            //preserve advanced mode state...
AdvancedModeItem.Checked:= True;                                                //... and activate it
ConfigRepair(CF);
with CF do
  begin
  SetMessageBar('presets...');
  ShortRead(IniSectionName,PlotColorPanel);
  ShortRead(IniSectionName,GridColorPanel);
  ShortRead(IniSectionName,UIColorPanel  );
  ShortRead(IniSectionName,FileMultipleInputItem);
  Readmenu(ViewMenu);
  Readmenu(MeasMenu);
  Readmenu(ReferenceMenu);
  RefCalcSubMenuRead(RefCalcSubMenu);
  SyncSetExtSym(ExtSymType(ReadInteger(IniSectionName,'ExtSym',Max(0,Min(Ord(ExtSym),Ord(ExtSymElevation))))));
  ReadGroup(AxisViewGroupBox);
  ReadGroup(MergeGroupBox);
  ReadGroup(MatchGroupBox);
  ReadGroup(Ft_EdgeDetectionGroupBox);
  ReadGroup(MayneordGroupBox);
  ReadGroup(PDDfitGroupBox);
  ReadGroup(MeasRemappingBox);
  ReadGroup(LinacErrorGroupBox);
  ReadGroup(GammaSettingsGroupBox);
  ReadGroup(Ft_FFFDetectionGroupBox);
  ReadGroup(FieldTypesPanel);
  ReadGroup(FocusPositionSetupGroupBox);
  ReadGroup(FocusPositionInputGroupBox);
  ReadGroup(FocusPositionResultGroupBox);
  ShortRead(IniSectionName,SimpleModeItem);
  ShortRead(IniSectionName,ProcessCheckTempTypeItem);
  ShortRead(IniSectionName,ProcessIgnoreTUnameItem);
  ShortRead(IniSectionName,FileIgnoreClipboardItem);
  ShortRead(IniSectionName,UserBorderDose_perc);
  ShortRead(IniSectionName,HistogramLimit_num);
  ShortRead(IniSectionName,OffAxisInclusionRange_cm);
  ShortRead(IniSectionName,ManualShiftStep_cm);
  ShortRead(IniSectionName,InsertOriginCheckBox);
  ShortRead(IniSectionName,FilterWidth_mm);
  ShortRead(IniSectionName,MeasRemapCoordinates);
  ShortRead(IniSectionName,CalcPostFilterItem);
  ShortRead(IniSectionName,ForceMatchingCheckBox);
  ShortRead(IniSectionName,PDDFitResultsLabelsCheckBox);
  ShortRead(IniSectionName,PDDFitResultsHeaderCheckBox);
  ShortRead(IniSectionName,FFFinFileCheckBox);
  ShortRead(IniSectionName,LogLevelEdit);
  end;
with DataPlot do
  for i:= 0 to Pred(SeriesCount) do if Series[i] is TLineSeries then
    with Series[i] as TLineSeries do
      LinePen.Width:= CF.ReadInteger(IniSectionName,Name,LinePen.Width);
ViewItems(Self);
if PageControl.ActivePage=FocusPositionTab then
  FocusPositionChange_mm(Self);
AdvancedSettingsTabExit(Self);
AdvancedModeItem.Checked:= b;                                                   //restore advanced mode
PanelElements.ConfigLoad(CF);
InitCxBlock(PanelElements.RowMax);
Ft_DetectionCheckbox[fcStandard].Checked:= True;                                //just for clarity, has no consequences at all because unused
for scan:= snGT to snAB do
  for angle:= Rot000 to Rot270 do
    with FP_Center_mm[scan,angle] do
      if Value<>0 then
        Tag:= 0;
end; {~presetload}


{13/01/2017}
procedure TAnalyseForm.ConfigSave(Sender:TObject);
begin
ConfigSave(Sender,ifthen(ExtractFilename(ConfigName)=DefInstituteCfg,GetConfigStg,ConfigName));
end; {~configsave}


{13/01/2017 setconfigname}
{20/11/2022 preload data from existing file for preset}
{21/01/2023 savetofile might fail}
procedure TAnalyseForm.ConfigSave(Sender     : TObject;
                                  AFileName  : String;
                                  PresetsOnly: Boolean=False);
var S: TMemoryStream;
begin
if PageControl.ActivePage=SettingsTab then
  SettingsTabExit(Sender);                                                      //gather data from wellhoferobject
S:= TMemoryStream.Create;                                                       //create stream s
if Length(AFileName)=0 then
  AFileName:= GetConfigStg;
AFileName:= PresetToName(AFileName,True);                                       //first revert to basic file name in all cases
SetConfigName(AFilename);
if PresetsOnly then
  begin
  AFileName:= NameToPreset(AFileName,True);                                     //convert from basic name to preset name if applicable
  if FileExists(AFileName) then
    S.LoadFromFile(AFileName);                                                  //preload any data into stream for preset
  end;
ConfigSave(S,AFileName);                                                        //save to stream s
try
  S.SaveToFile(AFileName);
 except
  AFileName:= AFileName+' failed';
end;
SetMessageBar(Format(WritingMessage,[AFileName]));
try
  S.Free
 except
  ExceptMessage('ConfigSave:S!');
 end;
end; {~configsave}


{22/07/2015 mayneord-related items}
{04/08/2015 ViewLeftAxisLowZeroItem}
{15/12/2105 penwidth}
{18/03/2016 added ProcessIgnoreTUnameItem}
{09/06/2016 replaced SpecialModes boolean Active with tmenuitem MenuItem}
{07/07/2016 added FFFcenterRadius_cm,FFFInFieldExt_cm}
{10/09/2016 try..except}
{03/02/2017 edgesigmoid added as preset}
{05/02/2017 ingnore items with caption='-'}
{04/12/2017 wInflectionSigmoid[fieldclass]}
{15/12/2017 extsysm}
{30/05/2018 GammaLimitConventional,GammaLimitFFF}
{31/05/2018 InsertOriginCheckBox}
{25/10/2018 SpecialModeValues}
{17/12/2019 SimpleModeItem}
{27/04/2020 gridcolor}
{05/05/2020 UIColorPanel}
{02/06/2020 WriteMenuShortCuts, BuildNumber}
{12/07/2020 EdgeMethodCombo}
{15/07/2020 GammaInFieldLimits,AppliedEdgeRefNorm}
{21/07/2020 WriteGroupBox}
{28/08/2020 removed FFFcenterRadius_cm}
{11/09/2020 InsertSpecialModesInfo}
{15/09/2020 HistoryList}
{16/11/2020 FileMultipleInputItem}
{08/12/2020 ShowLockItemCheckBox}
{23/02/2021 writegroup ComboBox added}
{14/05/2021 writegroup(MeasRemappingBox) added}
{01/06/2021 WriteGroup(Ft_FFFDetectionGroupBox)}
{30/10/2022 WriteGroup(FocusPositionSetupGroupBox)}
{14/11/2022 support for TLabeledEdit}
{19/11/2022 OffAxisInclusionRange_cm}
{20/11/2022 read preloaded data; dependency on activepage}
{01/12/2022 added TRadioButton to writegroup}
procedure TAnalyseForm.ConfigSave(AStream    :TStream;
                                  AFileName  :String='';
                                  PresetsOnly:Boolean=False);
const SM= 'SpecialModes';
var CF : TConfigStrings;
    t  : TStringList;
    i,j: Integer;

  function CheckPage(AControl:TControl): Boolean;
  begin
  Result:= (PageControl.ActivePage=AnalysisTab) or PageControl.ActivePage.IsParentOf(AControl);
  end;

  procedure WriteDialog(ADialog:TOpenDialog);
  begin
  with CF,ADialog do
    begin
    WriteString (Name,'DefaultExt','*.txt'    );
    WriteString (Name,'InitialDir',InitialDir );
    WriteString (Name,'FileName'  ,DefaultName);
    WriteInteger(Name,'Index'     ,FilterIndex);
    end;
  end;

  procedure WriteMenu(AMenu:TMenuItem);
  var i: Integer;
  begin
  for i:= 0 to Pred(AMenu.Count) do
    if AMenu.Items[i].Count>0           then
      WriteMenu(AMenu.Items[i])
    else if AMenu.Items[i].Caption<>'-' then
      CF.ShortWrite(IniSectionName,AMenu.Items[i]);
  end;

  procedure RemoveKey(AName:String);
  begin
  if CF.ValueExists(IniSectionName,AName) then
    CF.DeleteKey(IniSectionName,AName);
  end;

  procedure WriteGroup(AGroup:TWinControl);
  begin
  if CheckPage(Agroup) then
    with AGroup do
      begin
      i:= 0;
      while i<ControlCount do
        begin
        if Controls[i] is TCheckBox then
          CF.ShortWrite(IniSectionName,TCheckBox(Controls[i]))
        else if Controls[i] is TComboBox then
          CF.ShortWrite(IniSectionName,TComboBox(Controls[i]))
        else if Controls[i] is TSpinEditEx then
          CF.ShortWrite(IniSectionName,TSpinEditEx(Controls[i]))
        else if Controls[i] is TFloatSpinEditEx then
          CF.ShortWrite(IniSectionName,TFloatSpinEditEx(Controls[i]))
        else if Controls[i] is TEdit then
          CF.ShortWrite(IniSectionName,TEdit(Controls[i]))
        else if Controls[i] is TLabeledEdit then
          CF.ShortWrite(IniSectionName,TLabeledEdit(Controls[i]))
        else if Controls[i] is TRadioButton then
          CF.ShortWrite(IniSectionName,TRadioButton(Controls[i]));
        Inc(i);
        end;
      end;
  end;

  procedure InsertSpecialModesInfo;
  var f  : String;
      s  : TStringList;
      t  : TMemIniFile;
      i,j: Integer;
      a  : array of String;
  begin
  f:= CommonAppData+SM+'_info.ini';
  if FileExists(f) then
    begin
    t:= TMemIniFile.Create(f);
    s:= TStringList.Create;
    t.ReadSectionRaw(SM,s);
    if s.Count>0 then
      for i:= 0 to s.Count-1 do
        if (s.Strings[i][1]<>';') and (Pos('=',s.Strings[i])>0) then            //string can be splitted
          begin
          a:= String(s.Strings[i]).Split('=');
          f:= a[1];
          if Length(a)>2 then                                                   //in value might also be '='
            for j:= 2 to Length(a)-1 do
              f:= f+'='+a[j];
          CF.WriteString(SM,a[0],f);
          end;
    s.Free;
    t.Free;
    end;
  end;

begin
CF:= TConfigStrings.Create;
with CF do
  begin
  MenuWithShortCut:= WriteMenuShortCuts;
  if AStream.Size>0 then                                                        //if there are preloaded data
    try
      t:= TStringList.Create;
      t.LoadFromStream(AStream);                                                //load preloaded data into strings
      SetStrings(t);                                                            //load strings into CF
     finally
      t.Free;
      AStream.Size:= 0;                                                         //clear stream
     end;
  try
    if Length(AFileName)>0 then
      WriteString(DefComments,'file',ExtractFileName(AFileName));
    WriteDateTime(DefComments,'time',Now);
    WriteInteger(IniSectionName,Application.Title,BMBuildNumber);
    if (not PresetsOnly) and (PageControl.ActivePage=AnalysisTab) then          //general settings
      begin
      if SpecialModeValues.Count>0 then
        begin
        InsertSpecialModesInfo;
        for i:= 0 to Pred(SpecialModeValues.Count) do
          WriteString(SM,SpecialModeValues.Names[i],SpecialModeValues.Values[SpecialModeValues.Names[i]]);
        end;
      WriteBool(IniSectionName,'ShortCuts',WriteMenuShortCuts);
      WriteControl(Self,IniSectionName);
      WriteInteger(IniSectionName,DefLogMaxLines,LogTabMemo.Tag);
      for i:= 1 to NumSpecialModes do
        ShortWrite(IniSectionName,SpecialMode[i].MenuItem);
      ShortWrite(IniSectionName,AdvancedModeStartCheckBox);
      ShortWrite(IniSectionName,ShowLockItemCheckBox);
      ShortWrite(IniSectionName,HistoryListCheckBox);
      ShortWrite(IniSectionName,HistoryListFreezeCheckBox);
      ShortWrite(IniSectionName,HistoryListSize_num);
      ShortWrite(IniSectionName,PipsPixelSize_mm);
      ShortWrite(IniSectionName,HistogramLimit_num);
      ShortWrite(IniSectionName,OffAxisInclusionRange_cm);
      ShortWrite(IniSectionName,AutoSetDecPointCheckBox);
      ShortWrite(IniSectionName,AutoDecPointList);
      ShortWrite(IniSectionName,FFFinFileCheckBox);
      ShortWrite(IniSectionName,ManualShiftStep_cm);
      WriteFloat(IniSectionName,ZoomText,ZoomRange);
      WriteInteger(IniSectionName,'MinClipBoardBytes',MinClipBoardBytes);
      WriteDialog(FileOpenDialog);
      WriteDialog(FileSaveDialog);
      j:= Length(UseDoseConvTable);
      for i:= 1 to Pred(j) do with UseDoseConvTable[i] do
        begin
        ShortWrite(IniSectionName,DCDoseBox    );
        ShortWrite(IniSectionName,DCBgBox      );
        ShortWrite(IniSectionName,DCModalityBox);
        ShortWrite(IniSectionName,DCFilmTypeBox);
        ShortWrite(IniSectionName,DCEdit       );
        end;
      while j<OD2doseTableMax do
        begin
        for i:= 0 to 4 do RemoveKey(DCname(i,j));
        Inc(j);
        end;
      if SectionExists(AliasText) then
        EraseSection(AliasText);
      with AliasListEditor do if RowCount>1 then
        for i:= 1 to Pred(RowCount) do
          WriteString(AliasText,Keys[i],Values[Keys[i]]);
      WriteInteger(IniSectionName,MeasExtSymSubMenu.Name,Ord(ExtSym));
      ShortWrite(IniSectionName,BadPenumbraWidth_cm);
      ShortWrite(IniSectionName,ProcessCheckTempTypeItem);
      ShortWrite(IniSectionName,ProcessIgnoreTUnameItem);
      ShortWrite(IniSectionName,FileConvSourcePath);
      ShortWrite(IniSectionName,FileConvDestinationPath);
      ShortWrite(IniSectionName,FileConvNameMask);
      ShortWrite(IniSectionName,FileConvSamePath);
      ShortWrite(IniSectionName,FileConvLowerCase);
      ShortWrite(IniSectionName,FileConvMakeFileName);
      ShortWrite(IniSectionName,FileConvOverWrite);
      ShortWrite(IniSectionName,FileConvSourceRecursive);
      ShortWrite(IniSectionName,AxisViewFileTypeCheckBox);
      ShortWrite(IniSectionName,AxisViewSSDCheckBox);
      ShortWrite(IniSectionName,AxisViewDetNameCheckBox);
      ShortWrite(IniSectionName,AxisViewDetLength_num);
      ShortWrite(IniSectionName,AxisViewCollAngleCheckBox);
      ShortWrite(IniSectionName,AxisViewCommentsCheckBox);
      ShortWrite(IniSectionName,AxisViewComLength_num);
      ShortWrite(IniSectionName,AddDateTimeCheckBox);
      ShortWrite(IniSectionName,ShowWarningCheckBox);
      ShortWrite(IniSectionName,ConfigAutoSaveItem);
      ShortWrite(IniSectionName,LogLevelEdit);
      ShortWrite(IniSectionName,InventoryDirBox);
      ShortWrite(IniSectionName,FileConvSourcePath);
      ShortWrite(IniSectionName,FileConvDestinationPath);
      with DataPlot do
        for i:= 0 to Pred(SeriesCount) do if Series[i] is TLineSeries then
          with Series[i] as TLineSeries do
            WriteInteger(IniSectionName,Name,LinePen.Width);
      ShortWrite(IniSectionName,SimpleModeItem);
      ShortWrite(IniSectionName,InsertOriginCheckBox);
      ShortWrite(IniSectionName,FilterWidth_mm);
      ShortWrite(IniSectionName,FileIgnoreClipboardItem);
      ShortWrite(IniSectionName,FileMultipleInputItem);
      WriteMenu(ViewMenu);
      WriteMenu(MeasMenu);
      WriteMenu(ReferenceMenu);
      ShortWrite(IniSectionName,CalcPostFilterItem);
      ShortWrite(IniSectionName,UserBorderDose_perc);
      ShortWrite(IniSectionName,PDDfitCheckBox);
      ShortWrite(IniSectionName,ForceMatchingCheckBox);
      ShortWrite(IniSectionName,InventoryAltAxisCheckBox);
      ShortWrite(IniSectionName,PDDFitResultsLabelsCheckBox);
      ShortWrite(IniSectionName,PDDFitResultsHeaderCheckBox);
      Engines[UsedEngine].WriteConfig(CF);
      end; {not presetsonly}
    WriteGroup(AxisViewGroupBox);
    WriteGroup(MeasRemappingBox);
    WriteGroup(MergeGroupBox);
    WriteGroup(MatchGroupBox);
    WriteGroup(LinacErrorGroupBox);
    WriteGroup(GammaSettingsGroupBox);
    WriteGroup(MayneordGroupBox);
    WriteGroup(Ft_EdgeDetectionGroupBox);
    WriteGroup(PDDfitGroupBox);
    WriteGroup(FieldTypesPanel);
    WriteGroup(Ft_FFFDetectionGroupBox);
    WriteGroup(FocusPositionSetupGroupBox);
    WriteGroup(FocusPositionInputGroupBox);
    WriteGroup(FocusPositionResultGroupBox);
    if CheckPage(PlotColorPanel) then
      ShortWrite(IniSectionName,PlotColorPanel    );
    if CheckPage(GridColorPanel) then
      ShortWrite(IniSectionName,GridColorPanel    );
    if CheckPage(UIColorPanel) then
      ShortWrite(IniSectionName,UIColorPanel      );
    if CheckPage(FocusPositionPanel) then
      ShortWrite(IniSectionName,FocusPositionPanel);
    WriteShortCuts(IniSectionName,MainMenu);
    PanelElements.ConfigSave(CF);
   except
    ExceptMessage('ConfigSave!');
   end;
  try
    t:= TStringList.Create;
    GetStrings(t);
    t.SaveToStream(AStream);
   finally
    t.Free;
    Free;
   end;
  end; {with CF}
end; {~configsave}


//transfer values from welhofer.pas/engines[usedengine] to GUI
{12/12/2015 FFF-settings}
{04/01/2016 split wLinacSymSign}
{06/01/2016 added FitMubPowerNumEdit}
{08/01/2016 twcPddFitMubPowerFixed}
{12/01/2016 twcPddFitCostENRWeighted}
{12/07/2017 twcPddFitZWeightPower}
{04/12/2017 wInflectionSigmoid[fieldclass],EdgeSigmoidConvCheckBox}
{31/05/2018 twcMccInsertOrigin}
{11/06/2018 MeasReNormaliseDataItem}
{12/07/2020 EdgeMethodCombo}
{20/07/2020 EdgeSmallFieldWidth_cm, field types}
{21/07/2020 fcWedge}
{24/07/2020 EdgeWedge90ShiftFactor}
{18/08/2020 EdgeMRlinacTUcsvList}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{17/09/2020 Freeze}
{18/09/2020 MeasSDD2SSDItem,MeasScale2defaultSSDitem,MeasMissingPenumbraItem,MeasZeroStepsItem,MeasDetectDiagItem}
{13/10/2020 AutoSetDecPointCheckBox,AutoDecPointList,AutoDecimalPoint,AutoDecimalList}
{03/03/2020 removed MeasDetectDiagItem}
{07/03/2021 DefaultMRlinacSSD_cm}
{30/03/2021 MeasResampleItem.Checked:= wResampleData}
{01/06/2021 FFFMinFieldSize_cm}
{21/01/2023 StructuredDataSetsList -> w2D_ArrayRefList}
procedure TAnalyseForm.GetWellhoferValues;
var q: twcEdgeClass;
    f: twcFieldClass;
begin
FitMu3CheckBox           .Checked:= twcPDDpar[pddfit_mu3];
FitMu4CheckBox           .Checked:= twcPDDpar[pddfit_mu4];
FitMx2CheckBox           .Checked:= twcPDDpar[pddfit_mx2];
DefaultEnergy_MeV        .Value  := twcDefaultEnergy_MeV;
GammaDoseCutoff_perc     .Value  := twcGammaCutoffPercent;
GammaDepthCutoff_mm      .Value  := twcGammaCutoffDepth_cm    *10;
GammaDoseNorm_perc       .Value  := twcGammaDosePercBase;
GammaLocalDoseCheckBox   .Checked:= twcGammaLocalDosePerc;
GammaDistNorm_mm         .Value  := twcGammaDistCmBase        *10;
GammaEdit_Steps_per_mm   .Value  := 1/Math.Max(0.1,twcGammaDistCmStep*10);
GammaSearchMultiplier_num.Value  := twcGammaSearchMaxFactor;
MatchRangeDivider_num    .Value  := twcMatchRangeDivider;
MatchSteps_num           .Value  := twcMatchStepsNumber;
MatchNormDelta_perc      .Value  := twcMatchNormDeltaPercent;
MatchInclusionLimit_perc .Value  := twcMatchInclusionLimit    *100;
OriginMinLevel_perc      .Value  := twcOriginMinNormFraction  *100;
OutlierMaxPoints_num     .Value  := twcOutlierPointLimit;
FitENRlimit_ratio        .Value  := twcENRlimit;
FitMubPower_exp          .Value  := twcPddFitMubPower;
FitZWeight_val           .Value  := twcPddFitZWeightPower;
FitMubPowerFixedCheckBox .Checked:= twcPddFitMubPowerFixed;
FitENRweigthedCheckBox   .Checked:= twcPddFitCostENRWeighted;
InsertOriginCheckBox     .Checked:= twcMccInsertOrigin;
FitCycles_num            .Value  := twcNMcycles;
FitMaxTime_sec           .Value  := twcNMseconds;
FitRestarts_num          .Value  := twcNMrestarts;
with Engines[UsedEngine] do
  if not FReeze then
    begin
    AutoSetDecPointCheckBox   .Checked:= AutoDecimalPoint;
    AutoDecPointList          .Text   := AutoDecimalList;
    RefDeviceSpecificItem     .Enabled:= w2D_ArrayRefList.Count>0;
    XHpenumbra_perc           .Value  := wXPenumbraH        *100;
    XLpenumbra_perc           .Value  := wXPenumbraL        *100;
    EHpenumbra_perc           .Value  := wEPenumbraH        *100;
    ELpenumbra_perc           .Value  := wEPenumbraL        *100;
    UserBorderDose_perc       .Value  := UserBorderDoseLevel*100;
    LogLevelEdit              .Value  := LogLevel;
    FilterWidth_mm            .Value  := FilterWidth_cm     *10;
    LinacErrInvertGTCheckBox  .Checked:=(wLinacSymSign[fInplane   ]=-1);
    LinacErrInvertABCheckBox  .Checked:=(wLinacSymSign[fCrossplane]=-1);
    LinacSymInner_cm          .Value  := wLinacSymInnerRadius_cm;
    LinacSymOuter_cm          .Value  := wLinacSymOuterRadius_cm;
    ResampleGrid_mm           .Value  := ResampleGridSize_cm*10;
    OutlierFilterStatsCheckBox.Checked:= wOutlierFilter;
    CalcWidth_mm              .Value  := CalcWidth_cm       *10;
    EdgeSigmoidRadius_cm      .Value  := wInflectionSigmoidRadius_cm;
    FFFMinFieldSize_cm        .Value  := wFFFMinFieldSize_cm;
    FFFMinDoseDif_perc        .Value  := wFFFMinDoseDifPerc;
    FFFMinEdgeDif_mm          .Value  := wFFFMinEdgeDif_cm  *10;
    EdgeSmallFieldWidth_cm    .Value  := wSmallFieldLimit_cm;
    EdgeWedge90ShiftFactor    .Value  := wWedge90ShiftFactor;
    EdgeMRlinacTUcsvList      .Text   := wMRlinacTUlist;
    MeasReNormaliseDataItem   .Checked:= wRenormaliseData;
    MeasResampleItem          .Checked:= wResampleData;
    MeasSDD2SSDItem           .Checked:= wScaleSDD2SSD;
    MeasScale2defaultSSDitem  .Checked:= wScale2DefaultSSD;
    MeasMissingPenumbraItem   .Checked:= AcceptMissingPenumbra;
    MeasZeroStepsItem         .Checked:= AcceptZeroSteps;
    if Length(w2D_ArrayRefList.CommaText)>0 then
      StructuredDataSetsList  .Text   := w2D_ArrayRefList.CommaText;
    end;
if not Engines[UsedEngine].FReeze then
  for q:= fcPrimary to fcFallBack do
    for f:= Low(twcFieldClass) to High(twcFieldClass) do with Ft_EdgeMethodCombo[f,q] do
      begin
      ItemIndex:= 0;
      while (ItemIndex<Items.Count) and (Items[ItemIndex]<>twcDoseLevelNames[Engines[UsedEngine].wEdgeMethod[q,f]]) do
        ItemIndex:= ItemIndex+1;
      end;
end; {~getwellhofervalues}


//transfer values from GUI to welhofer.pas/engines[usedengine]
{21/09/2015}
{12/12/2015 DefaultSSD_cm-settings}
{04/01/2016 split wLinacSymSign}
{06/01/2016 added FitMubPower_exp}
{08/01/2016 twcPddFitMubPowerFixed}
{12/01/2016 twcPddFitCostENRWeighted}
{12/05/2016 wAxisPreserveOnExport}
{06/07/2016 added wFFFdetection, SyncSetFFFcenter}
{07/07/2016 added FFFcenterRadius_cm,FFFInFieldExt_cm}
{22/07/2016 SyncSetNormalisation, SyncSetCenterOfField}
{11/01/2016 wEdgeFallBackCm}
{12/07/2017 twcPddFitZWeightPower}
{21/07/2017 if Wellhofer.Ready}
{04/12/2017 wInflectionSigmoid[fieldclass]}
{31/05/2018 twcMccInsertOrigin}
{11/06/2018 MeasReNormaliseDataItem}
{06/11/2018 MeasGenericToElectronItem,MeasGenericToPDDItem}
{12/07/2020 EdgeMethodCombo}
{20/07/2020 EdgeSmallFieldWidth_cm}
{24/07/2020 EdgeWedge90ShiftFactor}
{28/07/2020 Ft_XXXX[twcFieldClass] elements}
{18/08/2020 EdgeMRlinacTUcsvList}
{27/08/2020 Ft_CenterRadiusEdit_Cm}
{15/09/2020 split off setenginevalues}
{07/03/2021 DefaultMRlinacSSD_cm}
{15/03/2021 twcDefaultSSDcm[f]}
{26/01/2023 error in transfer of GammaEdit_Steps_per_mm.Value to twcGammaDistCmStep with migration from Delphi to Lazarus}
procedure TAnalyseForm.SetWellhoferValues(Sender:TObject);
var f: twcFieldClass;
begin
SetEngineValues(UsedEngine);
for f:= Low(twcFieldClass) to High(twcFieldClass) do
  twcDefaultSSDcm[f]:= Ft_Default_SSD_Cm[f].Value;
twcNMcycles                   := Round(FitCycles_num           .Value);
twcNMseconds                  := FitMaxTime_sec                .Value;
twcNMrestarts                 := Round(FitRestarts_num         .Value);
twcFFFInFieldExt_cm           := FFFInFieldExt_cm              .Value;
twcOutlierPointLimit          := Round(Abs(OutlierMaxPoints_num.Value));
twcOriginMinNormFraction      := OriginMinLevel_perc           .Value/100;
twcPDDpar[pddfit_mu3]         := FitMu3CheckBox                .Checked;
twcPDDpar[pddfit_mu4]         := FitMu4CheckBox                .Checked;
twcPDDpar[pddfit_mx2]         := FitMx2CheckBox                .Checked;
twcDefaultEnergy_MeV          := DefaultEnergy_MeV             .Value;
twcGammaCutoffDepth_cm        := GammaDepthCutoff_mm           .Value/10;
twcGammaCutoffPercent         := GammaDoseCutoff_perc          .Value;
twcGammaDosePercBase          := GammaDoseNorm_perc            .Value;
twcGammaLocalDosePerc         := GammaLocalDoseCheckBox        .Checked;
twcGammaDistCmBase            := GammaDistNorm_mm              .Value/10;
twcGammaDistCmStep            := 1/(Max(0.1,GammaEdit_Steps_per_mm.Value)*10);  //was 1/Round(GammaEdit_Steps_per_mm.Value)*10 | Delphi: 1/(Max(GammaSteps_per_mmEdit.Value,1)*10)
twcGammaSearchMaxFactor       := GammaSearchMultiplier_num     .Value;
twcMatchRangeDivider          := MatchRangeDivider_num         .Value;
twcMatchStepsNumber           := Abs(MatchSteps_num            .Value);
twcMatchNormDeltaPercent      := MatchNormDelta_perc           .Value;
twcMatchInclusionLimit        := MatchInclusionLimit_perc      .Value/100;
twcENRlimit                   := FitENRlimit_ratio             .Value;
twcPddFitZWeightPower         := FitZWeight_val                .Value;
twcPddFitCostENRWeighted      := FitENRweigthedCheckBox        .Checked;
twcPddFitMubPower             := FitMubPower_exp               .Value;
twcPddFitMubPowerFixed        := FitMubPowerFixedCheckBox      .Checked;
twcMccInsertOrigin            := InsertOriginCheckBox          .Checked;
if twcPddFitMubPowerFixed then
  twcPddFitMubPower           := Round(twcPddFitMubPower*100)/100;
if Sender is TMenuItem then with Sender as TMenuItem do
  if AutoCheck then
    ShowMenuItemStatus(Sender);
end; {~setwellhofervalues}


//transfer engines[usedengine] values to wellhofer.pas/twellhoferdata
{15/09/2020 added AutoLoadReference for robustness}
{17/09/2020 Freeze}
{18/09/2020 MeasSDD2SSDItem,MeasScale2defaultSSDitem}
{13/10/2020 AutoSetDecPointCheckBox,AutoDecPointList,AutoDecimalPoint,AutoDecimalList}
{21/10/2020 more settings to transfer: AcceptMissingPenumbra,AcceptZeroSteps,DetectDiagonalScans,wApplySigmoidToBuffer}
{03/03/2020 removed MeasDetectDiagItem}
{07/03/2021 Nominal_IFA_CheckBox}
{30/03/2021 wResampleData, wMeas2TankMapping}
{01/06/2021 FFFMinFieldSize_cm,MeasGeneric_mm_Item}
{10/04/2022 FFFinFileCheckBox}
{14/11/2022 switch off AutoLoadReference}
procedure TAnalyseForm.SetEngineValues(aEngine:Integer);
var p: twcDoseLevel;
    q: twcEdgeClass;
    f: twcFieldClass;
    x: twcMeasAxis;
begin
with Engines[Clip(aEngine,0,Length(Engines)-1)] do
  if (not Freeze) and EngineReady then
    begin
    SyncSetCenterOfField(nil);
    SyncSetNormalisation(nil);
    SyncSetDetection(nil);
    SyncSetFFFpeak(nil);
    AutoLoadReference          := ((RefAutoLoadItem.Checked and RefAutoLoadItem.Enabled) or ProcessSetTempRefItem.Checked) and
                                  (PageControl.ActivePage<>FocusPositionTab);
    AcceptMissingPenumbra      := MeasMissingPenumbraItem        .Checked;
    AcceptZeroSteps            := MeasZeroStepsItem              .Checked;
    wApplySigmoidToBuffer      := ProcessSigmoid2BufferItem      .Checked;
    wReferenceFromGeneric      := RefGenericBeamItem             .Checked;
    wApplyUserLevel            := MeasUserDoseItem               .Checked;
    wResampleData              := MeasResampleItem               .Checked;
    twcGenericToElectron       := MeasGenericToElectronItem      .Checked;
    twcGenericPos_mm           := MeasGeneric_mm_Item            .Checked;
    wGenericToPDD              := MeasGenericToPDDItem           .Checked;
    MatchOverride              := ForceMatchingCheckBox          .Checked;
    wOutlierFilter             := OutlierFilterStatsCheckBox     .Checked;
    wFFFinFile                 := FFFinFileCheckBox              .Checked;
    ResampleGridSize_cm        := ResampleGrid_mm                .Value/10;
    Loglevel                   := LogLevelEdit                   .Value;
    wSmallFieldLimit_cm        := EdgeSmallFieldWidth_cm         .Value;
    wWedge90ShiftFactor        := EdgeWedge90ShiftFactor         .Value;
    wMRlinacTUlist             := EdgeMRlinacTUcsvList           .Text;
    ShowWarning                := ShowWarningCheckBox            .Checked;
    wMeas2TankMapping          := ifthen(MeasRemapCoordinates.Checked,MeasReMappingString.Text,twcMeasAxisStandard);
    if EdgeDetectionCheckBox.Checked then
      wEdgeFallBack_cm         := EdgeDetectionError_mm          .Value/10
    else
      wEdgeFallBack_cm         := -1;
    wXPenumbraH                := Round(XHpenumbra_perc          .Value)/100;
    wXPenumbraL                := Round(XLpenumbra_perc          .Value)/100;
    wEPenumbraH                := Round(EHpenumbra_perc          .Value)/100;
    wEPenumbraL                := Round(ELpenumbra_perc          .Value)/100;
    UserBorderDoseLevel        := UserBorderDose_perc            .Value/100;
    FilterWidth_cm             := FilterWidth_mm                 .Value/10;
    CalcWidth_cm               := CalcWidth_mm                   .Value/10;
    wEdgeDetect                := EdgeDetectionCheckBox          .Checked;
    for f:= Low(twcFieldClass) to High(twcFieldClass) do
      begin
      for q:= fcPrimary to fcFallBack do
        for p:= dLow to dTemp do                                                //twDoseLevel=(dLow,dHigh,d20,d50,d80,d90,dUser,dDerivative,dInflection,dSigmoid50,dTemp)
          if Ft_EdgeMethodCombo[f,q].Text=twcDoseLevelNames[p] then
            wEdgeMethod[q,f]   := p;
      wTopModelRadius_cm[f]    := Ft_CenterRadius_Cm[f]          .Value;
      end;
    for x:= Inplane to Beam do
      wAutoShift_cm[x         ]:= ShiftValues_cm[x]              .Value;
    wLinacSymSign[fInplane   ] := ifthen(LinacErrInvertGTCheckBox.Checked,-1,1);
    wLinacSymSign[fCrossplane] := ifthen(LinacErrInvertABCheckBox.Checked,-1,1);
    wInflectionSigmoidRadius_cm:= EdgeSigmoidRadius_cm           .Value;
    wLinacSymInnerRadius_cm    := LinacSymInner_cm               .Value;
    wLinacSymOuterRadius_cm    := LinacSymOuter_cm               .Value;
    wFFFMinFieldSize_cm        := FFFMinFieldSize_cm             .Value;
    wFFFMinDoseDifPerc         := FFFMinDoseDif_perc             .Value;
    wNominalIFA                := Nominal_IFA_CheckBox           .Checked;
    wFFFMinEdgeDif_cm          := FFFMinEdgeDif_mm               .Value/10;
    ArrayScanRefUse            := RefDeviceSpecificItem          .Checked;
    wAxisPreserveOnExport      := MeasPreserveDataItem           .Checked;
    wPipsPixel_cm              := PipsPixelSize_mm               .Value/10;
    MultiRefIndex              := RefMakeIndexItem               .Checked;
    wRefAlignPeak              := RefAlignTopforFFF              .Checked;
    AlignReference             := RefALignItem                   .Checked;
    wRenormaliseData           := MeasReNormaliseDataItem        .Checked;
    wScaleSDD2SSD              := MeasSDD2SSDItem                .Checked;
    wScale2DefaultSSD          := MeasScale2defaultSSDitem       .Checked;
    AutoDecimalPoint           := AutoSetDecPointCheckBox        .Checked;
    AutoDecimalList            := AutoDecPointList               .Text;
    end;
end; {~setenginevalues}


{29/09/2020 transfer temporary reference between engines}
{18/02/2021 when historylistsize is reduced, the lowest temprefengine has the highest chance to survive}
function TAnalyseForm.PassRefOrg(ReceivingEngine:Integer): Boolean;
begin
Result:= assigned(Engines[ReceivingEngine]) and
         (not Engines[ReceivingEngine].Freeze);
if Result then
  begin
  Result:= (TempRefEngine>=0)          and
           assigned(Engines[TempRefEngine]);
  if Result then                                                                //keep it safe
    begin
    Engines[ReceivingEngine].SetReferenceOrg(dsRefOrg,True,Engines[TempRefEngine]);
    TempRefEngine:= Min(TempRefEngine,ReceivingEngine);                         //always keep lowest copy of reforg
    end
  else
    Engines[ReceivingEngine].UnSetReferenceOrg;
  end;
end; {~passreforg}


{14/09/2020 will expand the number of engines until the maximum and then resuse the oldest, the index is stored in LoadEngine}
{17/09/2020 Freeze}
{29/09/2020 PassRefOrg}
{15/02/2021 forceexpand mode added}
function TAnalyseForm.AddEngine(ForceExpand:Boolean=False): Integer;
begin
Result:= 0;
if HistoryListCheckBox.Checked then
  begin
  if not Engines[UsedEngine].IsValid then
    Result:= UsedEngine
  else
    ForceExpand:= True;
  if ForceExpand then
    begin
    Result:= Length(Engines);
    if Result<HistoryListSize_num.Value then
      begin
      Engines[Result-1].Purge;                                                  //remove data that can be reconstructed (when not frozen)
      SetLength(Engines,Result+1);
      Engines[Result]:= TWellhoferData.Create(Engines[0].ModalityNormList,Engines[0].ModalityFilmList,Engines[0].ModalityBeamList);
      Engines[0].PassSettings(Engines[Result]);
      PassRefOrg(Result);                                                       //pass dsRefOrg from TempRefEngine to Result (if applicable)
      end
    else
      Result:= SelectEngine(LoadEngine,1,False);
    end; {expand}
  end;
LoadEngine                := Result;
Engines[LoadEngine].Freeze:= False;
SetEngineValues(Result);
end; {~addengine}


{16/09/2020 acitivate engine and update its stae with the current users choices}
{17/09/2020 make robust for destroy phase; Freeze}
{28/09/2020 setmessagebar}
{29/09/2020 PassRefOrg}
{14/10/2020 if not froozen then force reload}
{17/11/2020 UsedDataTopLine}
{14/01/2020 PriorityMessage shown at the very end}
{11/02/2021 not needed, double work: if not Engines[UsedEngine].Freeze then Reload(Self);}
{18/02/2021 call PassRefOrg only when tempref is set}
function TAnalyseForm.SelectEngine(aEngine    :Integer;
                                   aShift     :Integer=0;
                                   Synchronise:Boolean=True): Integer;
{$IFDEF PRELOAD}
var i: Integer;
{$ENDIF}
begin
Result:= Length(Engines);
if Result>0 then
  begin
  Result:= (Result+aEngine+Clip(aShift,-Result,Result)) mod Result;             //UsedEngine must always be positive
  if (Result<>UsedEngine) and Synchronise then
    begin
    if (UsedEngine<Length(Engines)) and assigned(Engines[UsedEngine]) then
      Engines[UsedEngine].Freeze:= HistoryListFreezeCheckBox.Enabled and HistoryListFreezeCheckBox.Checked;
    UsedEngine                 := Result;
    RawDataEditor     .Modified:= False;
    DataFromEditor             := False;
    MeasNormAdjustEdit.Value   := 100;
    PrevKey                    := #0;
    FileOpenDialog    .Filename:= Engines[UsedEngine].FileName;
    DetectedFileType           := Engines[UsedEngine].LastDetectedFileType;
    UsedDataTopLine            := Engines[UsedEngine].ParserTopLine;            //restore full state including starting point for reading
    Engines[UsedEngine].LoadAliasList(AliasListEditor.Strings);
    if ProcessSetTempRefItem.Checked then
      PassRefOrg(UsedEngine);                                                   //pass dsRefOrg from TempRefEngine to UsedEngine (if applicable)
    ClearScreen(Self);
    RawDataEditor.Clear;
   {$IFDEF PRELOAD}
    PreLoadStream.Clear;
    with Engines[UsedEngine] do
      if Parser.LineCount>0 then                                                //if text data are available, show them
        begin
        for i:= 0 to Parser.LineCount-1 do
          PreLoadStream.WriteString(Parser.Strings[i]+LineEnding);              //also the preloadstream is filled again
        Parser.PreLoaded:= False;
        end;
   {$ENDIF}
    SetCaption(FileOpenDialog.Filename);
    Engine2Editor(dsMeasured,True);                                             //reloads reference, calls ondataread
    end; {result<>usedengine}
  with Engines[UsedEngine] do
    if Freeze or (Length(PriorityMessage)=0) then                               //show type, or priority message when this is not already displayed
      SetMessageBar(ifthen(Length(PriorityMessage)>0,PriorityMessage,Format('Field Type: %s',[twcFieldClassNames[AppliedFieldClass]])));
  end;
end; {~selectengine}


//responds both manual and programmed resizing actions
{13/05/2020 statusbar.panels[1]}
{29/08/2020 LabelPlacingActive}
procedure TAnalyseForm.FormResize(Sender: TObject);
begin
if Visible then
  begin
  Inherited;
  if PageControl.ActivePage=FileConversionTab then
    begin
    with FileConvList do
      begin
      Width := FileConversionTab.Width -2*ConvTabMargin;
      Height:= FileConversionTab.Height-Top-ConvTabMargin;
      end;
    with FileConvSourceListBox do
      begin
      Left:= FileConversionTab.Width-Width-ConvTabMargin;
      FileConvDestinationListBox  .Left := Left;
      FileConvSourceTypeLabel     .Left := Left;
      FileConvDestinationTypeLabel.Left := Left;
      FileConvSourcePath          .Left := ConvTabMargin;
      FileConvSourcePath          .Width:= Left-3*ConvTabMargin;
      FileConvDestinationPath     .Left := FileConvSourcePath.Left;
      FileConvDestinationPath     .Width:= FileConvSourcePath.Width;
      end;
    end;
  if not LabelPlacingActive then                                                //prevent loops between FormResize and PlaceResultsLabels
    PlaceResultsLabels;
  StatusBar.Panels[0].Width:= Width-{$IFDEF JwaWinBase}Round(22.5{$ELSE}(15{$ENDIF}*Font.Size);
  end;
end; {~formresize}


{=> OnMouseEnter}
{24/08/2020}
procedure TAnalyseForm.AdjustHelpContext(Sender: TObject);
begin
with Sender as TWinControl do
  begin
  Parent                .HelpContext:= HelpContext;
  PageControl.ActivePage.HelpContext:= HelpContext;
  ActiveControl         .HelpContext:= HelpContext;
  end;
end; {~adjusthelpcontext}


{18/09/2020 functionality moved from pagecontrolchange}
procedure TAnalyseForm.MeasReMappingStringChange(Sender: TObject);
var w: TWinControl;
begin
w:= MeasRemappingString.Parent;
while not (w is TTabSheet) do                                                   //find out on which tabsheet this control is
  w:= w.Parent;
if w.Name=PageControl.ActivePage.Name then
  MeasRemapCoordinates.Enabled:= MeasRemappingString.Text<>twcMeasAxisStandard; //only set MeasRemapCoordinates when intentionally changed
end; {~measremappingstringchange}


{=> SettingsTab}
{21/09/2015 introduction setwellhofervalues}
{11/07/2017 ProcessMayneordItem changed to MeasMayneordItem}
{04/05/2020 setting viewbufferitem moved to ondataread}
procedure TAnalyseForm.SettingsTabExit(Sender:TObject);
begin
UpdateSettings(Sender);
SetWellhoferValues(Sender);
MeasMenuClick(Sender);
end; {~settingstabexit}


{15/03/2021}
{12/10/2022 MeasCalcPDDfitClick}
procedure TAnalyseForm.MeasMenuClick(Sender: TObject);
begin
MeasMayneordItem.Caption:= Format(MayneordItemText,[ifthen(MayneordSSD2_cm.Value>0,MayneordSSD2_cm.Value,Engines[UsedEngine].DefaultSSD_cm)]);
MeasCalcPDDfitClick(Sender);
end; {~measmenuclick}


{$IFDEF JwaWinBase}
{20/02/2023}
procedure TAnalyseForm.OnIdleTimer(Sender: TObject);
begin
GetCpuUsage(True,10);
end; {~onidletimer}
{$ENDIF}


{18/09/2015 loglevel}
{17/12/2015 DataPlot.xxAxis.Font.Color change removed}
{05/05/2020 dataplot colors synchronised}
{28/11/2022 changed to TColorButton}
{21/01/2023 StructuredDataSetsList -> w2D_ArrayRefList}
procedure TAnalyseForm.AdvancedSettingsTabExit(Sender:TObject);
var i: Integer;
begin
DataPlot.BottomAxis.Minors[0]       .TickColor:= GridColorPanel.ButtonColor;
DataPlot.BottomAxis.Grid            .Color    := GridColorPanel.ButtonColor;
DataPlot.BottomAxis.AxisPen         .Color    := GridColorPanel.ButtonColor;
DataPlot.LeftAxis  .Grid            .Color    := GridColorPanel.ButtonColor;
DataPlot                            .BackColor:= PlotColorPanel.ButtonColor;
DataPlot                            .Color    := PlotColorPanel.ButtonColor;
DataPlot.BottomAxis.Title.LabelBrush.Color    := PlotColorPanel.ButtonColor;
AdvancedSettingsPanel               .Color    := UIColorPanel  .ButtonColor;
ResultsPanel                        .Color    := UIColorPanel  .ButtonColor;
PDDFitResultsPanel                  .Color    := UIColorPanel  .ButtonColor;
FileConversionPanel                 .Color    := UIColorPanel  .ButtonColor;
SettingsPanel                       .Color    := UIColorPanel  .ButtonColor;
Statusbar                           .Color    := UIColorPanel  .ButtonColor;
GlobalNormAdjust_perc               .Value    := ifthen(GlobalNormAdjust_perc.Value>1,GlobalNormAdjust_perc.Value,100);
SetWellhoferValues(Sender);
StructuredDataSetsList.Text:= UpperCase(AnsiReplaceStr(AnsiReplaceStr(Trim(StructuredDataSetsList.Text),' ,',','),', ',','));
if Length(StructuredDataSetsList.Text)>0 then
  begin
  i:= Length(Engines);
  while i>0 do
    begin
    Dec(i);
    Engines[i].w2D_ArrayRefList.CommaText:= StructuredDataSetsList.Text;
    end;
  end;
end; {~advancedsettingstabexit}


{$push}{$warn 5036 off}
{31/03/2020 Introduced from delphi unit tobaseform}
{17/11/2022 show on/off status only for autocheck items}
procedure TAnalyseForm.ShowMenuItemStatus(Sender:TObject);
var Stg: String;
    a,c: Boolean;
begin
Stg:= '';
if (Sender is TMenuItem)      then with Sender as TMenuItem do begin  Stg:= Caption;  a:= AutoCheck;  c:= Checked;  end
else if (Sender is TAction  ) then with Sender as TAction   do begin  Stg:= Caption;  a:= AutoCheck;  c:= Checked;  end;
if Stg<>'' then
  SetMessageBar(CleanUpCaption(Stg)+ifthen(a,': '+CheckStrings[c],''));
end; {~showmenuitemstatus}
{$pop}


(*see onclick-event of multiple items in mainmenu
=> LockCriticalOptionsItem,
   ViewMeasuredItem, ViewPointsItem, ViewReferenceItem, ViewBufferItem, ViewIndicatorsItem, ViewFFFIndicatorsItem, ViewValuesItem, ViewEditorItem, ViewLeftAxisLowZeroItem,
   MeasMirrorToBufferItem,
   RefAutoLoadItem, RefAlignItem
*)
{13/07/2015 autoloadref now depends also on state of tempref to prevent autoloadref before temprefload}
{01/08/2015 in-memory implementation of tempref}
{04/08/2015 ViewLeftAxisLowZeroItem}
{12/08/2015:
  wRefAtDefaultSSD,wTakeCurrentRefSource,wCheckRefCurveString
  moved to updatesettings
  wMaxAsCenterPos,wCenterProfiles added}
{11/12/2015:
  static LeffSeries and ReffSeries replaced with InFieldIndicators
  added FFFSlopeLines}
{20/03/2016 ProcessMirrorMeasRefItem unchecked}
{26/11/2108 ProcessAutoscalingItem.Enabled}
{23/06/2020 PDDFitResultsTab.tabvisible}
{28/07/2020 Ft_XXXX[twcFieldClass] elements}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{26/09/2020 PenumbraSigmoids}
{02/03/2021 FFFSlopeLines depend on FFFfeatures}
{14/05/2021 applied ViewTopModelItem, added PlotIndicators}
{19/02/2023 call ProcessMirrorMeasRefClick if needed}
procedure TAnalyseForm.ViewItems(Sender:TObject);
var b,c,r,f: Boolean;
    side   : twcSides;

  procedure SetActiveStatus(k           :PlotItems;
                            ActiveStatus:Boolean);
  begin
  if (ActiveStatus<>PlotSeries[k].Active) then
    begin
    PlotSeries  [k].Active:= ActiveStatus;
    CursorSeries[k].Active:= ActiveStatus;
    r:= True;
    end;
  if (k=pMeasured) and ActiveStatus then with PlotSeries[pMeasured] do
    begin
    ShowPoints:= ViewPointsItem.Checked;
    if ViewMeasuredItem.Checked then LineType:= ltFromPrevious
    else                             LineType:= ltNone;
    end;
  end;

begin
UpdateSettings(Sender);
r                               := ProcessMirrorMeasRefItem.Checked;
ProcessMirrorMeasRefItem.Checked:= r and (not ProcessSetTempRefItem.Checked);
if (r<>ProcessMirrorMeasRefItem.Checked) then
  ProcessMirrorMeasRefClick(Sender);
r:= False;                                                                      //changed by setactivestatus
SetActiveStatus(pReference ,ViewReferenceItem .Checked and (RefAutoLoadItem.Checked or ProcessSetTempRefItem.Checked));
SetActiveStatus(pMeasured  ,ViewMeasuredItem  .Checked or ViewPointsItem.Checked);
SetActiveStatus(pCalculated,ViewCalculatedItem.Checked);
SetActiveStatus(pBuffer    ,ViewBufferItem    .Checked);
PDDFitResultsTab         .TabVisible := PDDfitCheckBox.Checked and (Engines[UsedEngine].ScanType in twcVertScans);
RefAutoLoadItem          .Enabled    := not (ProcessSetTempRefItem  .Checked or
                                             ProcessSyntheticProfile.Checked or
                                             MeasMirrorToBufferItem .Checked or
                                             Engines[UsedEngine].wSource[dsUnrelated].twLocked);
b                                    := (RefAutoLoadItem.Checked and RefAutoLoadItem.Enabled) or
                                         ProcessSetTempRefItem.Checked;
c                                    := (Engines[UsedEngine].AutoLoadReference<>b);
f                                    := ViewFFFIndicatorsItem.Checked and FFFfeatures;
for side:= twcLeft to twcRight do
  begin
  InFieldIndicators[side].Active     := ViewIndicatorsItem.Checked and IndicatorsOk;
  FFFSlopeLines[side]    .Active     := f;
  PenumbraSigmoids[side] .Active     := ViewPenumbraItem.Checked   and IndicatorsOk;
  end;
TopModelSeries           .Active     := (f or ViewTopModelItem.Checked) and Engines[UsedEngine].wSource[FFFdataSource].twTopModel.Valid;
Engines[UsedEngine].AutoLoadReference:= b;
if Sender is TMenuItem then with Sender as TMenuItem do
  begin
  if ((Sender=RefAlignItem) or (GroupIndex=135) or c) and Engines[UsedEngine].IsValid then //ReferenceCalcSubMenu items have groupindex 135
     Reload(Sender)
  else if Sender=ViewLeftAxisLowZeroItem then with DataPlot do
    begin
    LeftAxis.Range.UseMin:= ViewLeftAxisLowZeroItem.Checked;
    LeftAxis.Range.Min   := ifthen(ViewLeftAxisLowZeroItem.Checked,0,PlotScaleMin);
    end;
  end;
PlotIndicators;
if r then
  PlotCursor(Sender);
ShowMenuItemStatus(Sender);
end; {~viewitems}


//=> EdgeDetectionCheckBox, PDDfitCheckBox, LinacErrInvertGTCheckBox, LinacErrInvertABCheckBox, MergeScaleOverlapCheckBox, MergeMatchCheckBox
{14/07/2015}
{30/07/2015 ProcessMirroredMeasRef dependencies}
{12/08/2015: wRefAtDefaultSSD,wTakeCurrentRefSource,wCheckRefCurveString
             moved from viewitems
             wMaxAsCenterPos,wCenterProfiles added}
{24/12/2015 transfer normalisation to Wellhofer}
{18/03/2016 added ProcessIgnoreTUnameItem}
{12/05/2016 MeasPreserverDataItem}
{06/07/2016 MeasCenterFFFSubMenu}
{29/07/2016 MeasFFFpeakIsCenterItem}
{05/12/2016 wDefaultIgnoreSet}
{03/02/2017 set Wellhofer.wFFFdetection}
{30/03/2017 MeasCenterFFFSubMenu,MeasPeakFFFSubMenu,MeasNormFFFSubMenu}
{21/06/2017 RefAlignTopforFFF}
{04/12/2017 EdgeSigmoidConvCheckBox}
{11/06/2018 MeasReNormaliseDataItem}
{21/06/2020 EdgeConvRadioInflection,EdgeConvRadioSigmoid50}
{28/07/2020 Ft_XXXX[twcFieldClass] elements}
{14/09/2020 Engines}
{17/09/2020 HistoryListFreezeCheckBox}
{16/11/2020 FileMultipleInputItem}
{02/03/2021 FFF elements depend on both fcFFF and fcMRlinac}
{02/11/2022 FocusPositionCollimFromName}
{14/11/2022 FocusPositionNamePatternEdit}
{17/11/2022 ProcessSendToFPItem}
procedure TAnalyseForm.UpdateSettings(Sender:TObject);
var b: Boolean;
begin
UsedEngine                          := Clip(UsedEngine,0,Length(Engines)-1);
RefAutoLoadItem             .Enabled:= not MeasMirrorToBufferItem .Checked;
ProcessSendToFPItem         .Enabled:= PageControl.ActivePage=AnalysisTab;
ProcessSetTempRefItem       .Enabled:= not MeasMirrorToBufferItem .Checked;
b                                   := (not MeasMirrorToBufferItem.Checked) and
                                       (RefAutoLoadItem.Checked or ProcessSetTempRefItem.Checked);
ViewMeasNormAdjustMode      .Enabled:= ProcessAutoscalingItem.Enabled and (not ProcessAutoscalingItem.Checked);
MeasUsePDDfitModelItem      .Enabled:= PDDfitCheckBox             .Checked;
MeasPeakFFFSubMenu          .Enabled:= Ft_DetectionCheckBox[fcFFF].Checked or Ft_DetectionCheckBox[fcMRlinac].Checked;
RefAlignTopforFFF           .Enabled:= MeasPeakFFFSubMenu         .Enabled and RefAlignItem.Checked;
ViewBufferItem              .Checked:= ViewBufferItem             .Checked or MeasMirrorToBufferItem.Checked;
RefCalcSubMenu              .Enabled:= b;
RefNormaliseItem            .Enabled:= b;
RefSymCorrectItem           .Enabled:= b and (not ProcessMirrorMeasRefItem.Checked);
RefUseDivideByItem          .Enabled:= b;
RefUseAddToItem             .Enabled:= b;
RefUseGammaItem             .Enabled:= b;
RefUseUnrelatedToItem       .Enabled:= b;
RefAlignItem                .Enabled:= b;
RefAtDefaultSSDItem         .Enabled:= b and MeasScale2defaultSSDitem.Checked;
ViewReferenceItem           .Enabled:= b and Engines[UsedEngine].ReferenceValid;
HistoryListCheckBox         .Checked:= HistoryListCheckBox      .Checked and (HistoryListSize_num.Value>1);
HistoryListFreezeCheckBox   .Enabled:= HistoryListCheckBox      .Checked;
ProcessResetFitItem         .Enabled:= PDDfitCheckBox           .Checked and AdvancedModeOk;
CalcPostFilterItem          .Enabled:= RefUseAddToItem          .Checked and b;
FileMultipleInputItem       .Enabled:= (SpecialMode[2].MenuItem.Checked) or (SpecialMode[3].MenuItem.Checked) or (HistoryListSize_num.Value>1);
FocusPositionCollimFromName .Enabled:= FocusPositionUseCollimInfo.Checked;
FocusPositionNamePatternEdit.Enabled:= FocusPositionCollimFromName.Enabled and FocusPositionCollimFromName.Checked;
with Engines[UsedEngine] do
  begin
  ProcessIgnoreTUnameItem   .Enabled:= wCheckRefCurveString and (not ProcessMirrorMeasRefItem.Checked);
  wRefAtDefaultSSD                  := RefAtDefaultSSDItem     .Enabled and RefAtDefaultSSDItem.Checked;
  wCenterProfiles                   := MeasMove2OriginItem     .Checked;
  wTakeCurrentRefSource             := ProcessSetTempRefItem   .Checked;
  wCheckRefCurveString              := ProcessCheckTempTypeItem.Checked or (not ProcessSetTempRefItem.Checked); {here limited to tempref only}
  wCheckRefIgnoreLinac              := ProcessIgnoreTUnameItem .Checked and wCheckRefCurveString and ProcessSetTempRefItem.Checked;
  wRenormaliseData                  := MeasReNormaliseDataItem .Checked;
if ProcessSyntheticProfile  .Checked then
    wDefaultIgnoreSet               :=   wDefaultIgnoreSet+[twiFieldSize,twiDiagonal]
else
    wDefaultIgnoreSet               :=   wDefaultIgnoreSet-[twiFieldSize,twiDiagonal];
  end;
end; {~updatesettings}


(* UImode change responds to tab changing and other state changes with dis/enabling relevant parts of the GUI *)
//=> AdvancedModeItem, SimpleModeItem
{03/12/2015 edgedetectiongroupboox added}
{17/12/2015 Ft_FFFDetectionGroupBox added
            setting font color}
{09/06/2016 replaced SpecialModes boolean Active with tmenuitem MenuItem}
{27/10/2018 EnableMenu(ViewMeasNormAdjustMode,b)}
{17/12/2019 implementation of SimpleMode}
{13/04/2020 removed reread of config, replaced by MeasOD2DoseConvItem.Enabled:= a}
{23/04/2020 ControlsEnable only for tabs, not for panels on tabs}
{14/05/2020 EnableMenu(OptionsMenu,PageControl.ActivePage=AnalysisTab)}
{16/05/2020 enable all menus initially}
{29/06/2020 ConfigSaveAsItem.enabled linked to c (was s)}
{15/07/2020 ViewNoDefaultAnnotationItem}
{24/07/2020 MeasCenterSmallSubMenu,MeasNormSmallSubMenu,MeasCenterWedgeSubMenu,MeasNormWedgeSubMenu}
{28/07/2020 FieldTypesTab}
{08/12/2020 ShowLockItemCheckBox}
{10/04/2022 ProcessSetFFFItem}
{23/05/2022 InventoryTab depends on advancedmode}
{12/10/2022 MeasCalcPDDfitItem}
{22/01/2023 StructuredDataSetsList}
procedure TAnalyseForm.UImodeChange(Sender:TObject);
var a,b,c,s,p: Boolean;
    i        : Integer;

  procedure PanelEnable(AControl:TGroupBox);
  begin
  AControl.Font.Color:= ifthen(a,AnalyseForm.Font.Color,clGray);
  end;

begin
a:= AdvancedModeItem.Checked;
c:= not FileLockCriticalItems.Checked;
s:= not SimpleModeItem.Checked;
b:= a or s;
p:= b and (PageControl.ActivePage=AnalysisTab);
i:= Length(UseDoseConvTable);
ControlsEnable(ConfigurationTab    ,a);
ControlsEnable(SettingsTab         ,a);
ControlsEnable(FileConversionTab   ,a);
ControlsEnable(FieldTypesTab       ,a);
ControlsEnable(InventoryTab        ,s);
ControlsEnable(ODconvTab           ,a);
ControlsEnable(AdvancedSettingsTab ,a);
EnableMenuSystem(True);                                                         //reset all menus to available and change from here
EnableMenu(MeasMenu                ,p);
EnableMenu(ReferenceMenu           ,p);
EnableMenu(CalculationMenu         ,a);
EnableMenu(OptionsMenu             ,PageControl.ActivePage=AnalysisTab);
EnableMenu(ConfigSaveItem          ,a);
EnableMenu(ConfigSaveAsItem        ,c);
EnableMenu(ConfigReadItem          ,a);
EnableMenu(ConfigAutoSaveItem      ,a);
EnableMenu(ViewHighResValItem      ,a);
EnableMenu(ViewIndicatorsItem      ,a);
EnableMenu(ViewValuesItem          ,a);
EnableMenu(ViewHighResValItem      ,a);
EnableMenu(ProcessCheckTempTypeItem,a);
EnableMenu(ProcessSetFFFItem       ,a);
EnableMenu(MeasCalcPDDfitItem      ,a);
EnableMenu(ProcessResetFitItem     ,a and PDDfitCheckBox.Checked);
EnableMenu(FileSaveAsReferenceItem ,c);
EnableMenu(MeasPreserveDataItem    ,c);
EnableMenu(ProcessAutoscalingItem  ,a and c);
UseDoseDelButton         .Enabled   := a and (i>1);
UseDoseAddButton         .Enabled   := a and (i<OD2doseTableMax);
StructuredDataSetsList   .Enabled   := a and c;                                 //after ControlsEnable(AdvancedSettingsTab ,a);
FileLockCriticalItems    .Visible   := FileSaveAsReferenceItem.Enabled or ShowLockItemCheckBox.Checked;
InventoryTab             .TabVisible:= s;
FileConversionTab        .TabVisible:= s;
ODconvTab                .TabVisible:= s;
SettingsTab              .TabVisible:= b;
AdvancedSettingsTab      .TabVisible:= b;
FieldTypesTab            .TabVisible:= b;
ConfigurationTab         .TabVisible:= b;
RawDataTab               .TabVisible:= s;
MeasOD2DoseConvItem      .Enabled   := a;
MeasOD2DoseConvItem      .Visible   := b;
MeasExtSymSubMenu        .Visible   := s;
MeasGenStrategySubMenu   .Visible   := s;
MeasAxisSubMenu          .Visible   := s;
MeasSignalSubMenu        .Visible   := s;
MeasDivisor4             .Visible   := s;
MeasSSDsubmenu           .Visible   := s;
MeasMayneordItem         .Visible   := s;
MeasUserDoseItem         .Visible   := s;
MeasLocalPeakItem        .Visible   := s;
MeasUsePDDfitModelItem   .Visible   := s;
RefBackgroundCorrItem    .Visible   := s;
RefAlignTopforFFF        .Visible   := s;
RefMakeIndexItem         .Visible   := s;
RefGenericBeamItem       .Visible   := s;
PresetsMenu              .Visible   := b;
ViewPointsItem           .Visible   := b;
ViewIndicatorsItem       .Visible   := b;
ViewFFFIndicatorsItem    .Visible   := b;
ViewValuesItem           .Visible   := b;
ViewBottomAxisAlwaysBlack.Visible   := b;
ViewDivisor2             .Visible   := a;
ProcessSigmoid2BufferItem.Visible   := b;
ProcessResetFitItem      .Visible   := b;
ProcessMergeItem         .Visible   := s;
ProcessSetMergeSourceItem.Visible   := s;
ProcessClearMergeItem    .Visible   := s;
ProcessMirrorMeasRefItem .Visible   := s;
ProcessSyntheticProfile  .Visible   := s;
FileOpenTempRefItem      .Visible   := s;
FileSaveMeasurementItem  .Visible   := s;
FileSaveItem             .Visible   := s;
FileIgnoreClipboardItem  .Visible   := s;
FileSaveAsReferenceItem  .Visible   := a;
ShiftGroupBox            .Visible   := s;
MayneordGroupBox         .Visible   := s;
PDDfitGroupBox           .Visible   := s;
Ft_FFFDetectionGroupBox  .Visible   := s;
GammaSettingsGroupBox    .Visible   := s;
ConfigSaveAsItem         .Visible   := s or c;
ConfigReadItem           .Visible   := s;
ConfigAutoSaveItem       .Visible   := s;
{$IFDEF SelfTest}
SelfTestItem             .Visible   := a;                                       //verify that it is created when called
{$ENDIF}
AdvancedModeOk                      := True;
for i:= 1 to NumSpecialModes do
  with SpecialMode[i].MenuItem do                                               //verify that it is created when called
    begin
    Enabled:= a;
    Visible:= a;
    end;
UpdateSettings(Sender);
ShowMenuItemStatus(Sender);
if (Sender=SimpleModeItem) or (Sender=ViewNoDefaultAnnotationItem) then
  begin
  ClearAllCx;
  PublishResults;
  end;
end; {~uimodechange}


//clear the analysis results and graph
{11/12/2015 static LeffSeries and ReffSeries replaced with InFieldIndicators}
{30/01/2018 clearallcx added}
{15/09/2020 added conditional SetCaption}
{26/09/2020 PenumbraSigmoids}
{04/11/2022 keep page also on FocusPositionTab}
procedure TAnalyseForm.ClearScreen(Sender:TObject);
var k   : PlotItems;
    side: twcSides;
begin
if (DataPlot.BottomAxis.Title.Caption<>'-') then
  begin
  if Sender=ViewClearItem then
    SetCaption;
  BringToFront;
  ClearAllCx(Sender=ViewClearItem);
  if DataChanged then
    HistogramTab.TabVisible:= False;
  if (PageControl.ActivePage<>HistogramTab) and (PageControl.ActivePage<>FocusPositionTab) then
    PageControl.ActivePage:= AnalysisTab;
  for k:= Low(PlotItems) to High(PlotItems) do
    begin
    PlotLabels[k]  .Visible:= False;
    PlotDates[k]   .Visible:= False;
    PlotValues[k]  .Visible:= False;
    PlotSeries[k]  .Clear;
    CursorSeries[k].Clear;
    end;
  for side:= twcLeft to twcRight do
    begin
    InFieldIndicators[side].Clear;
    PenumbraSigmoids[side] .Clear;
    FFFSlopeLines[side]    .Clear;
    FFFSlopeLines[Side]    .Active:= False;
    end;
  ErrorSeries              .Clear;
  ErrorSeries              .Active := False;
  DataPlot.BottomAxis.Title.Caption:= '-';
  end;
end; {~clearscreen}


{22/04/2020 Unlike Delphi7 direct implementation available in Lazarus}
{16/09/2020 multiple file implementation}
{15/11/2020 some delay for multiple file drop}
{16/11/2020 number of files dependent on FileMultipleInputItem}
{14/11/2022 accept multiple files for FocusPositionTab}
procedure TAnalyseForm.ReadDroppedFiles(Sender         : TObject;
                                        const FileNames: array of String);
var i,j,k,l: Integer;
begin
i                       := 0;
j                       := 0;
k                       := ifthen((PageControl.ActivePage=FocusPositionTab) or
                                  (FileMultipleInputItem.Checked and FileMultipleInputItem.Enabled),
                                  Length(FileNames),1);
l                       := ifthen(PageControl.ActivePage=FocusPositionTab,k,HistoryListSize_num.Value);
MeasNormAdjustEdit.Value:= 100;                                                 //MeasNormAdjustEdit is intended for temporary use, reset to default
if CheckWellhoferReady and (k>0) then
      while (i<k) and (j<l) do                                                  //read until whichever limit is reached first
        try
          FileOpenDialog.FileName:= FileNames[i];
          if j>0 then
            WaitLoop(100);                                                      //some delay for multiple file drop
          if DataFileOpen(FileNames[i]) then
            Inc(j);
          Inc(i);
         except
          SetMessageBar('drop failed for '+FileNames[i]);
         end;
end; {~readdroppedfiles}


{09/06/2016 avoiding memory leak in editor/tmemo by reading lines into intermediate object localtext}
{17/03/2020 from editor.pas}
function TAnalyseForm.DataEditorOpenFile(AFileName:String): Boolean;
var {$IFDEF Windows}
    EventMask: DWORD;
    {$ENDIF}
    b        : Boolean;
    LocalText: TStringList;
begin
b:= (Length(AFileName)>0);
if b then
  begin
  if (not FileExists(AFileName)) then
    AFileName:= FileExtExists(AFileName,'.txt');
  b:= FileExists(AFileName);
  if b then with RawDataEditor do
    begin
    EditorFileName:= AFileName;
   {$IFDEF Windows}
    EventMask    := SendMessage(Handle,EM_GETEVENTMASK,0,0);
   {$ENDIF}
    LocalText    := TStringList.Create;
    Clear;
    try
     {$IFDEF Windows}
      SendMessage(Handle,EM_SETEVENTMASK,0,EventMask and (not ENM_REQUESTRESIZE));  //refuse to resize
     {$ENDIF}
      Localtext.LoadFromFile(AFileName);
      Lines.Text:= LocalText.Text;
     finally
     {$IFDEF Windows}
      SendMessage(Handle,EM_SETEVENTMASK,0,EventMask);
     {$ENDIF}
      LocalText.Free;
      end; {try}
    EditorFileTime:= FileDateToDateTime(FileAge(AFileName));
    Modified      := False;
    end; {exist}
  end;
DataFromEditor:= b;
Result        := b;
end; {~dataeditoropenfile}


procedure TAnalyseForm.Reload(Sender:TObject);
begin
Reload(Sender,True);
end; {~reload}


{=> ProcessSyntheticProfile, MeasSymCorrectItem, MeasMove2OriginItem, MeasSDD2SSDItem, RefMakeIndexItem}
{22/09/2015 preserve IsFile}
{23/06/2017 SetWellhoferValues toegevoegd}
{27/11/2017 ShowMenuItemStatus}
{29/07/2020 preserve shift when switching between tabs}
{14/08/2020 reset MeasNormAdjustEdit always when ProcessAutoscalingItem.Checked}
{08/09/2020 added DoClearScreen option}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{17/11/2020 UsedDataTopLine supports all context changes through SelectEngine}
{17/03/2021 For multiscan files the scannr might change and no shift should be applied in that case.}
{30/03/2021 resample and coordinateordering parameter dropped from advstreamdata}
{03/11/2022 protect against hangups by checking OnDataReadBusy}
procedure TAnalyseForm.Reload(Sender       :TObject;
                              DoClearScreen:Boolean);
var b: Boolean;
    x: twcFloatType;
    s: String;
begin
if not OnDataReadBusy then
  begin
  {$IFDEF JwaWinBase}
  GetCpuUsage(False,1);
  {$ENDIF}
  x:= ifthen(Sender=FileLoadDataItem,0,Engines[UsedEngine].wSource[dsMeasured].twShift_cm);
  s:= Engines[UsedEngine].wSource[dsMeasured].twCurveIDString;
  if ProcessAutoscalingItem.Checked then
    MeasNormAdjustEdit.Value:= 100;
  ShowMenuItemStatus(Sender);
  SetWellhoferValues(Sender);
  UpdateSettings(Sender);
  DetectedFileType:= Engines[UsedEngine].LastDetectedFileType;
  if Engines[UsedEngine].wSource[dsMeasured].twOriginalFormat=twcWellhoferRFb then //***binary data***
    begin                                                                          //wMultiScanNr is used!
    if DoClearScreen then
      ClearScreen(Sender);
    if (Engines[UsedEngine].Freeze and HistoryListCheckBox.Checked) or
       Engines[UsedEngine].AdvStreamData(nil,
                                         UsedDataTopLine,                         //this is also restored in selectengine
                                         True,
                                         DetectedFileType,
                                         FileOpenDialog.FileName) then
       Engine2Editor;                                                             //includes shift
    end
  else
    begin                                                                         //***ascii data***
    b:= Engines[UsedEngine].IsFile;
    ReadEditor(Sender,DoClearScreen);
    if (x<>0) and (s=Engines[UsedEngine].wSource[dsMeasured].twCurveIDString) then
      begin                                                                       //apply shift only on same scan for multiscan files
      Engines[UsedEngine].Shift(x);                                               //preserve shift
      OndataRead(Sender);
      end;
    Engines[UsedEngine].IsFile:= b;
    end;
  end;
end; {~reload}


{$IFDEF Windows}
{26/03/2020 FPC implementation}
procedure TAnalyseForm.WMChangeCBChain(AwParam:WParam; AlParam:LParam);
var HandleRemove,HandleNext: THandle;
begin
HandleRemove:= AwParam;
HandleNext  := AlParam;
if NextClipboardOwner=HandleRemove then
  NextClipboardOwner:= HandleNext
else if NextClipboardOwner<>0 then
  SendMessage(NextClipboardOwner,WM_ChangeCBChain,HandleRemove,HandleNext);
end; {~wmchangecbchain}
{$ENDIF}


{$IFDEF Windows}
(* ****BistroMath core function****
  A Windows API conformal function to intercept the clipboard.
  It will accept text formatted data when >MinClipBoardBytes and not FileIgnoreClipboardItem.Checked.
  In its current design it can continue to read when there are still more data and its not a multi-scan format and FileMultipleInputItem.Checked.
  This will happen when in OmniPro multiple scans are selected.
  In that situation the data will be spread over all available engines and/or processed with SpecialMode2/3.
  This is only meaningful when either more than one engine is available or automated processing is activated. FileMultipleInputItem.Enabled reflects these rules. *)
{12/02/2016 preloadstream, preloadtransfer}
{14/02/2016 direct transfer from clipboard to preloadstream}
{23/01/2018 setmessagebar}
{15/10/2018 MeasNormAdjustEdit.Value:= 100}
{14/01/2020 Ignore clipboard when text starts with DefAppName or size<MinClipBoardBytes}
{28/01/2020 set default filename always}
{30/01/2020 check clipboard.hasformat}
{17/03/2020 RawDataEditor}
{26/03/2020 ====FPC implementation====}
{09/05/2020 more subtle messaging}
{15/05/2020 no message if not CF_TEXT}
{03/07/2020 exchanged ordering of test on locking with length test}
{14/09/2020 addengine}
{17/11/2020 support automated continuous reading of multiple data sets in single text data set file format}
{30/04/2021 fail for next scan in iba-multiscan resolved}
{11/11/2022 activate also for FocusPositionTab}
{$push}{$warn 5024 off:wellform.pas(860,31) Hint: Parameter "AlParam" not used}
procedure TAnalyseForm.WMDRAWCLIPBOARD(AwParam:WParam;
                                       AlParam:LParam);
const InvalidStg='clipboard data invalid';
var LocalDataTopLine: Integer;                                                  //support for automated continous reading in single file format type
begin
if ((PageControl.ActivePage=AnalysisTab) or (PageControl.ActivePage=FocusPositionTab)) and
    Clipboard.HasFormat(CF_TEXT)                                                       and
   (AnsiLeftStr(ClipBoard.AsText,Length(DefAppName))<>DefAppName) then
  begin
  if not (ClipBoardLock or FileIgnoreClipboardItem.Checked or OnDataReadBusy) then
    begin
    if Length(ClipBoard.AsText)>MinClipBoardBytes then
      begin
      LocalDataTopLine:= 0;                                                     //A new drop: start at top of file
      repeat
        UsedEngine     := AddEngine;                                            //selecting another engine sets UsedDataTopLine
        UsedDataTopLine:= LocalDataTopLine;                                     //set global UsedDataTopLine from local value
       {$IFDEF PRELOAD}
        Engines[UsedEngine].Parser.PreLoaded:= False;
       {$ENDIF PRELOAD}
        RawDataEditor.Clear;
        DetectedFileType            := twcUnknown;
        PrevKey                     := #0;
        MeasNormAdjustEdit.Value    := 100;                                     //MeasNormAdjustNumEdit is intended for temporary use, reset to default
        DataChanged                 := True;
        Engines[UsedEngine].FileName:= DefaultName;
        EditorFileName              := DefaultName;
        try
          if LocalDataTopLine=0 then                                            //already in editor when LocalDataTopLine>0
            begin
           {$IFDEF PRELOAD}
            PreloadStream.Clear;
            PreloadStream.WriteString(ClipBoard.AsText);                        //copy clipboard to stream
            Engines[UsedEngine].FileName:= DefaultName;
            PreloadTransfer(Self);
           {$ELSE}
            RawDataEditor.PasteFromClipboard;
            RawDataEditor.Modified:= False;
           {$ENDIF}
            end
          else
            begin
           {$IFDEF PRELOAD}
            Engines[UsedEngine].FileName:= DefaultName;
            PreloadTransfer(Self);                                              //preloadstream unchanged and still available
           {$ELSE}
            RawDataEditor.Modified:= False;
           {$ENDIF}
            end;
          ReadEditor(nil);                                                      //here the text data are send to engine[usedengine] and analysed, using UsedDataTopLine
          SetCaption(Engines[UsedEngine].MakeCurveName);
         except
          SetMessageBar(InvalidStg);
         end; {try}
        if FileMultipleInputItem.Enabled        and FileMultipleInputItem.Checked and
          (Engines[UsedEngine].wMultiScanMax=1) and Engines[UsedEngine].FindMoreData then //for all multi-scan formats: wMultiScanMax>1
          begin                                                                 //there are more data in this file, not being of multi-scan type format
          LocalDataTopLine:= Engines[UsedEngine].Parser.CurrentLineNumber;      //local file pointer is updated from last read line
          WaitLoop(100);                                                        //some delay for multiple data sets in single date set text format file
          end
        else
          LocalDataTopLine:= 0;                                                 //noting more to read, or not wanting to read it
      until LocalDataTopLine=0;                                                 //ready when we decide to ignore data or no more dat available
      end {long enough}
    else
      SetMessageBar(InvalidStg);
    end
  else
    SetMessageBar('clipboard locked');
  end; {activepage...}
SendMessage(NextClipboardOwner,WM_DRAWCLIPBOARD,0,0);                           //pass message
end; {~wmdrawclipboard}
{$pop}
{$ENDIF}


procedure TAnalyseForm.ReadEditor(Sender:TObject);
begin
ReadEditor(Sender,True);
end; {~readeditor}


(* ****BistroMath core function****
The data are read from the editor (raw data) tab. Therefore they are ascii by definition.
The format can be a known multi-scan format or one or more single scan format data sets.
See WMDRAWCLIPBOARD for explanation of the latter.
There are multiple pathways to readeditor: as all processing is done through engines and called in the right order in OnDataRead,
a lot of times changing users choices will result in rereading raw data.
=> MeasMirrorItem, MeasSDD2SSDItem, MeasScale2defaultSSDitem, MeasResampleItem, MeasUserDoseItem, DataFileOpen *)
{12/08/2015 CenterProfiles option removed in Wellhofer.AdvReadData, see UpdateSettings}
{12/02/2016 preload uses stream}
{19/03/2020 internal RawDataEditor}
{01/05/2020 check size of data}
{08/09/2020 added DoClearScreen option}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{17/11/2020 UsedDataTopLine set in WMDRAWCLIPBOARD or DataFileOpen}
{30/03/2021 resample and coordinateordering parameter dropped from advreaddata/advstreamdata}
{03/11/2022 timeout to prevent hanging}
procedure TAnalyseForm.ReadEditor(Sender       :TObject;
                                  DoClearScreen:Boolean);
var b,f  : Boolean;
    i,j: Integer;
begin
i:= 100;
while OnDataReadBusy and (i>0) do
  begin
  WaitLoop(100);
  Dec(i);
  end;
if i>0 then
  begin
  {$IFDEF JwaWinBase}
  GetCpuUsage(False,1);
  {$ENDIF}
  f:= PageControl.ActivePage=FocusPositionTab;
  if not f then
    begin
    UpdateSettings(Sender);
    if DoClearScreen then
      ClearScreen(Sender);
    end;
  Engines[UsedEngine].FileTime:= EditorFileTime;
  DataFromEditor              := True;
  SourceAxisSync;
  b:= (Engines[UsedEngine].Freeze and HistoryListCheckBox.Checked);
  if not b then
    begin
   {$IFDEF PRELOAD}
    if (not RawDataEditor.Modified) and ((Engines[UsedEngine].Parser.PreLoaded) or (Sender=nil)) then
      begin
      DataFromEditor:= False;
      b:= (PreloadStream.Size>MinClipBoardBytes) and
           Engines[UsedEngine].AdvStreamData(PreLoadStream,
                                             UsedDataTopLine,
                                             True,                                //unfreeze
                                             DetectedFileType,
                                             Engines[UsedEngine].FileName);
      end
    else
      begin
     {$ENDIF PRELOAD}
      i:= RawDataEditor.Lines.Count;
      j:= 0;
      while (i>0) and (j<MinClipBoardBytes) do  {fast count of characters}
        begin
        Dec(i);
        Inc(j,Length(RawDataEditor.Lines.Strings[i]));
        end;
      b:= (j>MinClipBoardBytes) and
           Engines[UsedEngine].AdvReadData(RawDataEditor.Lines,
                                           UsedDataTopLine,
                                           True,                                  //unfreeze
                                           DetectedFileType,
                                           EditorFileName);
     {$IFDEF PRELOAD}
      end;
     {$ENDIF PRELOAD}
    end;
  if b and ScanInclusionTest(UsedEngine) then
    begin
    if f then
      FocusPositionProcessFile(Sender)
    else
      begin
      OnDataRead(Sender);
      if Sender is TMenuItem then with Sender as TMenuItem do
        if AutoCheck then
          ShowMenuItemStatus(Sender);
      end; {other pages}
    end; {b}
  end; {i>0}
end; {~readeditor}


{15/12/2017}
procedure TAnalyseForm.SyncSetExtSym(Sender:TObject);
var e: ExtSymType;
begin
if Sender is TMenuItem then with Sender as TMenuItem do
  Checked:= True;
for e:= ExtSymLinacError to ExtSymElevation do if ExtSymSubItems[e].Checked then
  begin
  SyncSetExtSym(e);
  if Sender<>nil then
    PublishResults;
  end;
end; {~syncsetextsym}


//update of submenu
{15/12/2017}
procedure TAnalyseForm.SyncSetExtSym(AExtSym:ExtSymType);
begin
with ExtSymSubItems[AExtSym] do
  begin
  Checked       := True;
  Parent.Caption:= Format(DefExtSymSubMenu,[CleanUpCaption(Caption).ToLower]);
  ExtSym        := AExtSym;
  end;
end; {~syncsetextsym}


//update of submenu
{05/07/2016}
{19/07/2020 field types}
{21/07/2020 fcWedge}
{28/07/2020 Ft_NormMethodCombo}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.SyncSetNormalisation(Sender:TObject);
var f: twcFieldClass;
    n: twcNormalisation;
begin
for f:= Low(twcFieldClass) to High(twcFieldClass) do
  for n:= NormOnCenter to NormOnInFieldArea do
    if Ft_NormMethodCombo[f].Caption=twcNormalisationNames[n] then
      Engines[UsedEngine].wNormalisation[f]:= n;
end; {~syncsetnormalisation}


//synchronisation of ability to detect the selected field types
{28/07/2020 Ft_DetectionCheckBox}
{03/09/2020 Ft_DynPenumbraCheckBox}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{03/03/2021 wDiagonalDetection}
procedure TAnalyseForm.SyncSetDetection(Sender:TObject);
var f: twcFieldClass;
begin
for f:= Low(twcFieldClass) to High(twcFieldClass) do
  begin
  Engines[UsedEngine].wFieldTypeDetection[f]:= Ft_DetectionCheckBox[f]  .Checked;
  Engines[UsedEngine].wDiagonalDetection[f] := Ft_DetDiagonalCheckBox[f].Checked and (not ProcessSyntheticProfile.Checked);
  end;
end; {~syncsetdetection}


//synchronisation of CoF definition, originally part of Measurement menu
{22/07/2016}
{20/07/2020 field types}
{21/07/2020 fcWedge}
{28/07/2020 Ft_CenterMethodCombo}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.SyncSetCenterOfField(Sender:TObject);
var f: twcFieldClass;
    o: twcCenterType;
begin
for f:= Low(twcFieldClass) to High(twcFieldClass) do
  for o:= CenterPenumbra to CenterMax do
    if Ft_CenterMethodCombo[f].Caption=twcCenterTypeNames[o] then
      Engines[UsedEngine].wCenterDefinition[f]:= o;
if Sender is TMenuItem then with Sender as TMenuItem do
  Checked:= True;
end; {~syncsetcenteroffield}


//synchronisation of FFF peak definition
{05/07/2016}
{29/07/2016 changed logics to combination of MeasFFFpeakIsCenterItem and FFFcSubItems}
procedure TAnalyseForm.SyncSetFFFpeak(Sender:TObject);
var m: twcFFFPeakType;
begin
for m:= CenterFFFTopModel to CenterFFFSlopes do
    if FFFpSubItems[m].Checked then
      SyncSetFFFpeak(m);
if Sender<>nil then
  ReadEditor(Sender);
end; {~syncsetfffpeak}


//update of submenu
{05/07/2016}
{29/07/2016 changed logics to combination of MeasFFFCenterItem and FFFpSubItems}
{29/03/2017 syncsetfffcenter -> SyncSetCenter / SyncSetFFFpeak}
procedure TAnalyseForm.SyncSetFFFpeak(APeakDef:twcFFFPeakType);
begin
FFFpSubItems[APeakDef].Checked:= True;
MeasPeakFFFSubMenu    .Caption:= Format(DefFFFpSubMenu,[CleanUpCaption(FFFpSubItems[APeakDef].Caption).ToLower]);
end; {~syncsetfffpeak}


{26/11/2018}
{24/04/2020 ---small adaptations for tachart---}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.SmartScaleElectronPDD(Sender:TObject);
var b: Boolean;
begin
with Engines[UsedEngine] do
  begin
  b:= ViewScaleElectronPDDrange.Checked and IsValid and (ScanType in twcVertScans) and (AppliedFieldClass=fcElectron);
  DataPlot.BottomAxis.Range.UseMax:= not b;
  if b then
    DataPlot.BottomAxis.Range.Max:= GetDisplayedPositionScale(NiceStep(2*wSource[dsMeasured].twLevelPos[d50].Limit[twcRight].CalcPos,[2,4,5]));
  end;
end; {~smartscaleelectronpdd}


{15/02/2021}
{18/02/2021 do not add empty engines immediately}
{07/06/2021 check for legal value usedengine}
{15/06/2021 first unset tempref}
{02/12/2022 reduce number of engines if necessary}
{09/05/2024 avoid FreeAndNil}
{29/05/2024 allow listsize of 0}
procedure TAnalyseForm.SetHistoryListSize(NewLength:Word);
var i,j: Integer;
begin
j:= Length(Engines);
if TempRefEngine>=NewLength then
  ProcessUnsetTempRefClick(Self);
if NewLength<Length(Engines) then
  for i:= j-1 downto 0 do
    if i>=NewLength then
      Engines[i].Free
    else
      Engines[i].Freeze:= HistoryListCheckBox.Checked;
SetLength(Engines,Min(Length(Engines),NewLength));
HistoryListSize_num.Value:= NewLength;
UsedEngine:= Min(UsedEngine,NewLength-1);
if (NewLength>0) and (not (fsFirstShow in FormState)) then                      //is excluded after OnCreate event
     SetCaption(Engines[UsedEngine].FileName);
end; {~sethistorylistsize}


{16/09/2020}
{17/09/2020 HistoryListFreezeCheckBox}
{18/09/2020 when unfrozen relaod current data if possible}
{15/02/2021 handling engines size moved to SetHistoryListSize}
{07/06/2021 do not remove Engine[0] from list, because of ownership ModalityLists}
{20/05/2022 reduced logging (p)}
procedure TAnalyseForm.HistoryListSizeClick(Sender: TObject);
var i,j  : Integer;
    c,s,p: Boolean;
begin
c:= FileHistoryItem.Checked<>HistoryListCheckBox.Checked;                       //prevent reentrance by changing checked within procedure
if c or (Sender=HistoryListSize_num) then
  begin
  p:= HistoryListCheckBox.Checked;                                              //p = initial checkstate
  if Sender=HistoryListCheckBox then                                            //synchronise checkbox and menu item
    FileHistoryItem    .Checked:= HistoryListCheckBox.Checked
  else if Sender=FileHistoryItem then
    HistoryListCheckBox.Checked:= FileHistoryItem    .Checked;
  c:= HistoryListCheckBox.Checked;                                              //c = current checkstate
  s:= (Sender=HistoryListSize_num);                                             //s = true when listsize is changed
  if c and (not s) then                                                         //when switched to checked a size of 2 is the smallest meaningful
    HistoryListSize_num.Value:= Max(2,HistoryListSize_num.Value);
  i:= Max(1,ifthen(FileHistoryItem.Checked,HistoryListSize_num.Value,1));
  j:= UsedEngine;
  SetHistoryListSize(i);                                                        //check also usedengine
  if j>i then
    begin
    UsedEngine:= i-1;
    SelectEngine(UsedEngine);
    end
  else if not (c or s or ClipBoardLock) then                                    //ClipBoardLock is set during FormCreate
    Reload(Sender);                                                             //reload current data in unfrozen state when meaningful
  if not ClipBoardLock then                                                     //ClipBoardLock is set during FormCreate
    UpdateSettings(Sender);
  if (c<>p) then
    ShowMenuItemStatus(FileHistoryItem);
  end;
end; {~historylistsizeclick}


{16/09/2020 multiple file support}
{17/11/2020 FileMultipleInputItem}
{14/11/2022 accept multiple files for FocusPositionTab}
procedure TAnalyseForm.FileOpenClick(Sender:TObject);
var i,j,k,l: Integer;
begin
SetCaption;
with FileOpenDialog do
  if CheckWellhoferReady and Execute then
    begin
    i                       := 0;                                               //number of files read
    j                       := 0;                                               //number engines used to read data
    k                       := ifthen((PageControl.ActivePage=FocusPositionTab) or
                                      (FileMultipleInputItem.Checked and FileMultipleInputItem.Enabled),
                                      Files.Count,1);                           //k= number of files or 1
    l                       := ifthen(PageControl.ActivePage=FocusPositionTab,k,HistoryListSize_num.Value);
    MeasNormAdjustEdit.Value:= 100;                                             //MeasNormAdjustEdit is intended for temporary use, reset to default
    while (i<k) and (j<l) do                                                    //read until whichever limit is reached first
      try
        if DataFileOpen(Files[i]) then
          Inc(j);                                                               //if succesfully read increment j
        Inc(i);
       except
        SetMessageBar('file open failed for '+Files[i]);
       end;
    end;
end; {~fileopenclick}


{=> OpenTempRefItem}
{04/08/2016}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.FileOpenTempRefClick(Sender: TObject);
begin
with FileOpenDialog do if Execute then
  begin
  ProcessSetTempRefItem  .Checked:= Engines[UsedEngine].LoadReference(FileName);
  ProcessUnsetTempRefItem.Checked:= not ProcessSetTempRefItem.Checked;
  end;
end; {~fileopentemrefclick}


{02/04/2019}
procedure TAnalyseForm.FileSaveClick(Sender:TObject);
begin
if ActiveMemo=nil then FileSave(SelectedPlot,Sender=FileSaveItem)
else                   MemoSaveToFile(ActiveMemo);
end; {~filesaveclick}


{$IFDEF form2pdf}
{05/07/2020 adapted code of Alan Chamberlain 2020}
procedure TAnalyseForm.FilePrintFormClick(Sender: TObject);
var OldExt,OldFilter,sTabs: string;
    ObjectCount,i,TabIndex: integer;
begin
OldExt                   := FileSaveDialog.DefaultExt;
OldFilter                := FileSaveDialog.Filter;
FileSaveDialog.DefaultExt:= '.pdf';
FileSaveDialog.Filter    := 'PDF files|*.pdf|All files|*';
FileSaveDialog.FileName  := ChangeFileExt(FileSaveDialog.FileName,'.pdf');
if FileSaveDialog.Execute then
  begin
  if Sender=FilePrintAllItem then
    begin
    ShowHistogram;
    ObjectCount:= FormToPDF(AnalyseForm,FileSaveDialog.FileName);
    end
  else
    if Sender = FilePrintPageItem then
      begin
      ObjectCount:= FormToPDF(PageControl.ActivePage,FileSaveDialog.FileName);
      end
  else
    if Sender = FilePrintSelItem then
       begin
       {get list of visible tabs}
       sTabs:= '';
       for i:= 0 to PageControl.PageCount-1 do
          begin
          if PageControl.Pages[i].TabVisible then sTabs:= STabs+IntToStr(i+1)+',';
          end;
       if InputQuery('Select tabs to be printed','Enter the tab numbers delimited by commas',sTabs) then
          try
            while sTabs <> '' do
              begin
              TabIndex:= StrToInt(Copy2SymbDel(sTabs,',')) - 1;
              if TabIndex=PageControl.IndexOf(HistogramTab) then
                ShowHistogram;
              ObjectCount:= FormToPDF(PageControl.Pages[TabIndex]);
              end;
            ObjectCount:= FormToPDF(FileSaveDialog.FileName);
           except
            SetMessageBar('Invalid tab list');
           end;
       end;
  end;
SetMessageBar(Format(ifthen(ObjectCount>0,'%d objects printed','error %d encountered while trying to print'),[ObjectCount]));
FileSaveDialog.DefaultExt:= OldExt;
FileSaveDialog.Filter    := OldFilter;
end; {~fileprintformclick}
{$ENDIF}


//convert menu action for MeasMoveLeftItem and MeasMoveRightItem to keyboard action "<" or ">"
procedure TAnalyseForm.MeasMoveClick(Sender:TObject);
var c: Char;
begin
if Sender is TMenuItem then
  with Sender as TMenuItem do
    begin
    if Pos(DefShiftLeft,Caption)>0 then c:= DefShiftLeft
    else                                c:= DefShiftRight;
    FormKeyPress(Sender,c);
    end;
end; {~measmoveclick}


{06/12/2022}
procedure TAnalyseForm.FocusPositionStateUpdate(Sender: TObject);
begin
FocusPositionSetIndicators;
FocusPositionPanelPaint(Sender);
end; {~focuspositionstateupdate}


{03/12/2022}
{06/12/2022 make double quotes dependent of FocusPositionDetectorRotates}
procedure TAnalyseForm.FocusPositionSetIndicators;
var angle: twcCardinalAngles;
    scan : twcScanTypes;
    Istg : String[4];
    b    : Boolean;
begin
for scan:= snGT to snAB do
  begin
  if scan=snGT then b:= SwapGTcheckbox.Checked
  else              b:= SwapABcheckbox.Checked;
  Istg:= ifthen(scan=snGT,'GT','AB');
  if b then
    Istg:= ReverseString(Istg);
  if FocusPositionDetectorRotates.Checked then
    Istg:= AnsiQuotedStr(Istg,'"');
  for angle:= Rot000 to Rot270 do
    FP_Center_button[scan,angle].Caption:= Format('%s %3u° (mm):',[Istg,Ord(angle)*90]);
  end;
end; {~focuspositionsetindicators}


//twcBLD_T:array[twcIECdefs,twcCardinalAngles ] of String[2]=(('X2','Y2','X1','Y1'),('Y1','X1','Y2','X2'));
{17/10/2022 create graphics on focuspositionpanel}
{18/10/2022 added effects of mapping}
{25/11/2022 add measurement dot}
{01/12/2022 FocusPositionPanel.Visible}
procedure TAnalyseForm.FocusPositionPanelPaint(Sender: TObject);
var angle: twcCardinalAngles;
    scan : twcScanTypes;
begin
if (PageControl.ActivePage=FocusPositionTab) and (not (fsFirstShow in FormState)) then //is excluded after OnCreate event
  begin
  for scan:= snGT to snAB do
    for angle:= Rot000 to Rot270 do
      begin
      FP_Center_button[scan,angle].Left:= 5+((Ord(angle)+4-FocusPosition_mapping_Box.ItemIndex) mod 4)*FPrectSpacing;
      FP_Center_mm[scan,angle]    .Left:= FP_Center_button[scan,angle].Left+FP_Center_button[scan,angle].Width;
      end;
  if sender = FocusPositionFullScale_mm then
      FocusPositionSquarePaint;
  FocusPositionSquarePaint;
  FocusPositionSquarePaint(4);
  if Sender=FocusPosition_IEC_Convention then
    begin
    FocusPosition_UpperBLD_ident.ItemIndex:= FocusPosition_IEC_Convention.ItemIndex;
    FocusPositionChange_mm(Sender);
    end;
  end; {focuspositiontab}
end; {~focuspositionpanelpaint}


{26/11/2022 independent, formerly part of focuspositionpanelpaint}
{01/12/2022 clOrange as inner color for rotated detector}
procedure TAnalyseForm.FocusPositionSquarePaint(Id:Integer=-1);

  procedure DrawRect(AIndex:Word);
  const SideChars:array of Char=('T','A','G','B');
  var i,j,Ri,Ty: Integer;
      c        : TColor;
      x,y      : Double;

    procedure OuterText(SideIndex:Integer);
    var x,y,Tx: Integer;
        Stg   : String;
    begin
    Stg:= SideChars[SideIndex]+'='+twcBLDnames[twcIECdefs(FocusPosition_IEC_Convention.ItemIndex),twcCardinalAngles((AIndex+SideIndex) mod 4)];
    Tx := FocusPositionPanel.Canvas.TextWidth(Stg);
    case SideIndex of
      0,2: x:= FPrectHalf - Tx div 2;
      1  : x:= -Tx-FPtextOffset;
      else x:= FPrectWidth+FPtextOffset;
      end;
    case SideIndex of
      0  : y:= FPrectWidth;
      1,3: y:= FPrectHalf-Ty div 2;
      else y:= -Ty;
      end;
    FocusPositionPanel.Canvas.TextOut(Ri+x,FP_RectTop+y,Stg);
    end;

    procedure InnerText(SideIndex:Integer);
    var x,y,Tx: Integer;
        Stg   : String;
    begin
    Stg:= '"'+SideChars[(SideIndex + ifthen(FocusPositionDetectorRotates.Checked,AIndex,0)) mod 4]+'"';
    Tx := FocusPositionPanel.Canvas.TextWidth(Stg);
    case SideIndex of
      0,2: x:= FPrectHalf - Tx div 2;
      1  : x:= +FPtextOffset;
      else x:= FPrectWidth-FPtextOffset-Tx;
      end;
    case SideIndex of
      0  : y:= FPrectWidth-FPtextOffset-Ty;
      1,3: y:= FPrectHalf-Ty div 2;
     else  y:= FPtextOffset;
      end;
    FocusPositionPanel.Canvas.TextOut(Ri+x,FP_RectTop+y,Stg);
    end;

  begin
  Ri:= FPrectOffset+ifthen(AIndex>3,4,(AIndex+4-FocusPosition_mapping_Box.ItemIndex) mod 4)*FPrectSpacing; //figure out which rect to plot
  with FocusPositionPanel.Canvas do
    begin
    Ty       := TextHeight('A');
    Pen.Color:= clBlack;
    c        := Brush.Color;
    if AIndex<4 then Brush.Color:= clGray
    else             Brush.Color:= clMaroon;
    FillRect(Ri,FP_RectTop,Ri+FPrectWidth,FP_RectTop+FPrectWidth);              //make square
    if AIndex<4 then                                                            //innertext
      begin
      if FocusPositionDetectorRotates.Checked and (AIndex>0) then Font.Color:= clAqua
      else                                                        Font.Color:= clYellow;
      for j:= 0 to 3 do
        InnerText(j);
      end
    else
      begin {aindex>3}
      Brush.Color:= clWhite;
      Line(Ri+FPrectHalf,FP_RectTop           ,Ri+FPrectHalf ,FP_RectTop+FPrectWidth);
      Line(Ri           ,FP_RectTop+FPrectHalf,Ri+FPrectWidth,FP_RectTop+FPrectHalf);
      end;
    if FocusPositionDetectorRotates.Checked then
      Font.Color:= clYellow;
    if AIndex<4 then
      TextOut(Ri+FPrectHalf-TextWidth(FocusPosition_mapping_Box.Items[AIndex]) div 2,
              FP_RectTop+FPrectHalf-Ty div 2,
              FocusPosition_mapping_Box.Items[AIndex]);                         //in center: "0" | "90" | "180" | "270"
    if AIndex>3 then
      begin
      Pen.Color:= clLime;
      x        := FP_Calc_Dbfso_mm[CrossPlane];
      y        := FP_Calc_Dbfso_mm[InPlane   ];
      end
    else if (FP_Center_mm[snGT,twcCardinalAngles(AIndex)].Color=FPreadyColor) and (FP_Center_mm[snAB,twcCardinalAngles(AIndex)].Color=FPreadyColor) then
      begin
      Pen.Color:= clRed;
      x        := FocusPositionAxisValue(CrossPlane,twcCardinalAngles(AIndex));
      y        := FocusPositionAxisValue(InPlane   ,twcCardinalAngles(AIndex));
      end
    else
      begin
      x:= 1000;
      y:= 1000;
      end;
    if x+y<100 then
      begin
      i:= Round(FPrectHalf*Clip(x/FocusPositionFullScale_mm.Value,-1,1));
      j:= Round(FPrectHalf*Clip(y/FocusPositionFullScale_mm.Value,-1,1));
      Ellipse(Ri+FPrectHalf+i-FPdotRadius,FP_RectTop+FPrectHalf-j-FPdotRadius,  //negative j because in graphics TG is in negative direction
              Ri+FPrectHalf+i+FPdotRadius,FP_RectTop+FPrectHalf-j+FPdotRadius);
      end;
    Brush.Color:= c;
    Font .Color:= clBlack;
    for j:= 0 to 3 do
      OuterText(j);
    end;
  end; {drawrect}

begin
if Id<0 then
  for Id:= 0 to 3 do
    DrawRect(Id)
else if Id>3 then
   DrawRect(4)
else
   DrawRect(Id);
end; {~focuspositionsquarepaint}


//FP_Center_button[scan,angle]
//twcScanTypes     =(snGT,snAB,...
//twcCardinalAngles=(Rot000,Rot090,Rot180,Rot270);
{24/10/2022}
{30/10/2022 rescaling to SSD}
{02/11/2022 FocusPositionUseCollimInfo}
{04/11/2022 support for file processing}
{14/11/2022 support for FocusPositionCollimFromName}
{15/11/2022 FocusPositionDetSurfaceIsoc implemented}
procedure TAnalyseForm.FocusPositionRadioClick(Sender:TObject);
var angle: twcCardinalAngles;
    scan : twcScanTypes;
    i,j  : Integer;
    found: Boolean;
begin
with Engines[UsedEngine],wSource[dsMeasured] do
  begin
  angle:= Rot000;
  found:= False;
  if FocusPositionUseCollimInfo.Checked    and (not OnDataReadBusy) and
     IsValid and (ScanType in [snGT,snAB]) and (twBeamInfo.twBCollimator mod 90=0) then
    begin
    scan:= ScanType;
    if FocusPositionCollimFromName.Checked then
       begin
       i:= Pos('%',FocusPositionNamePatternEdit.Text);
       j:= Pos('d',FocusPositionNamePatternEdit.Text,i);
       if (i>0) and Inrange(j-i,1,2) then
         begin
         i:= 4;
        repeat
          Dec(i);
          try
            found:= Pos(Format(FocusPositionNamePatternEdit.Text,[i*90]),FileName)>0;
           except
            found:= False;
          end;
        until (found or (i=0));
        if found then
          angle:= twcCardinalAngles(i);
        end;
      end; {FocusPositionCollimFromName.Checked}
    if not found then
      begin
      i:= ((twBeamInfo.twBCollimator+360) mod 360) div 90;
      if InRange(i,0,3) then
        begin
        found:= True;
        angle:= twcCardinalAngles(i);
        end;
      end {not found}
    end {FocusPositionUseCollimInfo.Checked}
  else
    begin  //found is false here
    i:= Ord(Rot000);
    repeat
      angle:= twcCardinalAngles(i);
      j    := Ord(snGT);
      Inc(i);
      repeat
        scan := twcScanTypes(j);
        found:= FP_Center_button[scan,angle].Checked;
        Inc(j);
      until found or (scan=snAB);
    until found or (angle=Rot270);
    end;
  if found then
    begin
    FP_Center_mm[scan,angle]    .Value  := 10*GetFieldCenter_cm(dsMeasured,dInflection,True)/ifthen(FocusPositionDetSurfaceIsoc.Checked,twActDet2NormRatio,1);
    FP_Center_button[scan,angle].Checked:= True;
    end;
  end; {with}
end; {~focuspositionradioclick}


{04/11/2022}
{17/11/2022 ProcessSendToFPItem, OffAxis_cm}
{19/11/2022 OffAxisInclusionRange_cm.Value}
{22/01/2023 message FocusPositionStructuredData}
procedure TAnalyseForm.FocusPositionProcessFile(Sender: TObject);
var i: Integer;
    s: set of twcScanTypes;
    b: Boolean;
begin
with Engines[UsedEngine] do
   begin
   i:= ifthen(ArrayScanRefOk,wMultiScanMax,1);
   b:= False;
   s:= [];
   if Sender=ProcessSendToFPItem then
     ShowMenuItemStatus(Sender);
   repeat
     if Abs(OffAxis_cm)<Min(1,OffAxisInclusionRange_cm.Value) then
       begin
       FocusPositionRadioClick(Sender);
       s:= s+[ScanType];
       end;
     if i>1 then
       begin
       Inc(wMultiScanNr);
       AdvReadData(RawDataEditor.Lines,
                   UsedDataTopLine,
                   True,                                                        //unfreeze
                   DetectedFileType,
                   EditorFileName);
       end;
     Dec(i);
     if (snGT in s) and (snAB in s) then
       begin
       i:= 0;
       b:= True;
       end;
   until i=0;
   with FocusPositionStructuredData.Font do
     if b then begin Color:= FPreadyColor and $00C000;  Style:= [fsBold]; end
     else      begin Color:= clSilver;                  Style:= [];       end;
   end;
end; {~focuspositionprocessfile}


(*
for detector rotation with head
inline    =  cos(a) * "TG det" + sin(a) * "AB det"
crossline = -sin(a) * "TG det" + cos(a) * "AB det"
  a,axis
  0,i:  1, 0
    c:  0, 1
 90,i:  0, 1
    c: -1, 0
180,i: -1, 0
    c:  0,-1
270,i:  0,-1
    c:  1, 0
*)
function TAnalyseForm.FocusPositionAxisValue(AScan :twcMeasAxis;
                                             AAngle:twcCardinalAngles): Double;
var AxisMapAngle: twcCardinalAngles;
    m           : twcScanTypes;
begin
if FocusPositionDetectorRotates.Checked then AxisMapAngle:= AAngle
else                                         AxisMapAngle:= Rot000;             //without detector rotation fixed mapping i -> TG and c -> AB
if AxisRotationMapping[AScan,AxisMapAngle,snGT]<>0 then m:= snGT
else                                                    m:= snAB;
Result:= AxisRotationMapping[AScan,AxisMapAngle,m]*FP_Center_mm[m,AAngle].Value;
end; {~focuspositionaxisvalue}


{02/11/2022}
{22/01/2023 FocusPositionStructuredData}
procedure TAnalyseForm.FocusPositionResetClick(Sender: TObject);
var scan : twcScanTypes;
    angle: twcCardinalAngles;
begin
for scan:= snGT to snAB do
  for angle:= Rot000 to Rot270 do
    with FP_Center_mm[scan,angle] do
      begin
      Tag:= 1;
      if Value=0 then
        FocusPositionChange_mm(FP_Center_mm[scan,angle])
      else
        Value:= 0;
      end;
with FocusPositionStructuredData.Font do
  begin
  Color:= clSilver;
  Style:= [];
  end;
end; {~focuspositionresetclick}


(*
With this procedure the rotation point value can be subtracted from the measurements.
For a rotating detector the rotation point is already in the origin.
Therefore a straightforward mapping from Inplane to snTG and Crossplane to snAB can be done.


no detector rotation
rot-axis: straight mapping to TG/AB
fp      : straight mapping but depends on upper/lower for magnification

detector rotation
rot-axis: straight mapping to TG/AB
fp      : mapping from inline/crossline to TG/AB depending on angle and upper/lower

AxisRotationMapping: array[InPlane..CrossPlane,twcCardinalAngles] of AxisMapValue = (((1,0),(0,1),(-1,0),(0,-1)),((0,1),(-1,0),(0,-1),(1,0)));

function  FocusPositionAxisValue(AScan :twcMeasAxis;
                                 AAngle:twcCardinalAngles      ): Double;

FP_Center_mm: array[snGT..snAB,twcCardinalAngles       ] of TFloatSpinEditEx; //FocusPosition

if AxisRotationMapping[AScan,AxisMapAngle,snGT]<>0 then m:= snGT
else                                                    m:= snAB;
Result:= AxisRotationMapping[AScan,AxisMapAngle,m]*FP_Center_mm[m,AAngle].Value;

u:= ifthen(twcBLDnames[twcIECdefs(FocusPosition_IEC_Convention.ItemIndex),Rot000][1]=FocusPosition_UpperBLD_ident.Text[1],0,1);
FocusPositionAxisValue(x,twcCardinalAngles((Ord(b)+Ord(x)+u) mod 4)
*)
{28/11/2022}
procedure TAnalyseForm.FocusPositionZeroClick(Sender: TObject);
var scan,m       : twcScanTypes;
    x            : twcMeasAxis;
    b            : twcBanks;
    s,u          : Integer;
    angle        : twcCardinalAngles;
    vector_mm    : array[InPlane..CrossPlane] of Double;
    magnification: array[twcUpper..twcLower ] of Double;
begin
u:= ifthen(twcBLDnames[twcIECdefs(FocusPosition_IEC_Convention.ItemIndex),Rot000][1]=FocusPosition_UpperBLD_ident.Text[1],0,1);
if Sender=FocusPositionZeroRotButton then
  begin
  vector_mm:= FP_Calc_Drot_mm;
  for b:= twcUpper to twcLower do
    magnification[b]:= 1;
  end
else {focal point to zero}
  begin
  vector_mm:= FP_Calc_Dbfso_mm;
  for b:= twcUpper to twcLower do
    magnification[b]:= 1-FP_BankLevel_mm[twcReference].Value/FP_BankLevel_mm[b].Value;
  end;
for scan:= snGT to snAB do
  for angle:= Rot000 to Rot270 do
    begin
    x:= twcMeasAxis(Ord(scan));
    if FocusPositionDetectorRotates.Checked and (Sender=FocusPositionZeroFPButton) then
      begin
      if AxisRotationMapping[x,angle,snGT]<>0 then m:= snGT
      else                                         m:= snAB;
      s:= AxisRotationMapping[x,angle,m]
      end
    else
      begin
      m:= scan;
      s:= 1;
      end;
    with FP_Center_mm[m,angle] do
      Value:= Value-s*vector_mm[x]*magnification[twcBanks((Ord(scan)+Ord(angle)+u) mod 2)];;
    end;
FocusPositionZeroRotButton.Enabled:= False;
FocusPositionZeroFPButton .Enabled:= False;
end; {~focuspositionzeroclick}


(*
The calculation of Dbfso is based on the paper of Chojnowski et al.
 Beam focal spot position determination for an Elekta linac
 with the Agility® head
https://aapm.onlinelibrary.wiley.com/doi/full/10.1002/acm2.12344
*)
{=> FP_Center_mm.OnChange}
{28/10/2022}
{29/10/2022 FocusPosition_UpperBLD_ident}
{07/11/2022 support for device rotation}
{12/11/2022 FP_BankLevel_mm[twcDetector]}
{15/11/2022 FocusPositionDetectorOther implemented}
{18/11/2022 FocusPositionDetAutoSet implemented}
{26/11/2022 FocusPositionSquarePaint implemented}
{01/12/2022 if visible}
{09/12/2022 FP_BankFactor}
procedure TAnalyseForm.FocusPositionChange_mm(Sender:TObject);
var x                   : twcMeasAxis;
    b                   : twcBanks;
    i,n,u               : Integer;
    scan                : twcScanTypes;
    angle               : twcCardinalAngles;
    FP_avg              : array[InPlane..CrossPlane,twcBanks] of Double;
    FP_abs_sum          : Double;
    a,d_epi,d_high,d_low: Double;
    c                   : TColor;
    ok,PaintResult      : Boolean;
    bfso,rot            : String;
begin
ok:= not (fsFirstShow in FormState);
if ok then                                                                      //is excluded after OnCreate event
  begin
  n:= 0;         //compare IEC name of BLD at T ('X2'|'Y2) at rot000 with upper BLD ident name.
  u:= ifthen(twcBLDnames[twcIECdefs(FocusPosition_IEC_Convention.ItemIndex),Rot000][1]=FocusPosition_UpperBLD_ident.Text[1],0,1);
  if Sender is TFloatSpinEditEx then
    with Sender as TFloatSpinEditEx do
      if Pos('FP_Center_mm_',Name)>0 then
        begin
        if Tag=1 then
          begin
          Color:= clYellow;
          Tag  := 0;
          end
        else
          Color:= FPreadyColor;
        i:= 4;
        repeat
          Dec(i);
          if Pos(IntToStr(i*90),Name)>0 then
            begin
            FocusPositionSquarePaint(i);
            PaintResult:= (i=3) and (Pos('AB',Name)>0);
            i:= 0;
            end;
        until i=0;
        end;
  if not (FocusPositionDetectorOther.Checked or FP_BankLevel_mm[twcReference].Enabled) then
    with Engines[UsedEngine] do
      FP_BankLevel_mm[twcReference].Value:= ifthen(FocusPositionDetAutoSet.Checked,wSource[dsMeasured].twSSD_cm,DefaultSSD_cm)*10;
  with FocusPosition_UpperBLD_ident do
    begin
    if FocusPosition_IEC_Convention.ItemIndex=ItemIndex then c:= clSilver
    else                                                     c:= clBlack;
    if Font.Color<>c then
      begin
      Font.Color:= c;
      SelLength := 0;
      end;
    end;
  FP_BankLevel_mm[twcReference].Enabled:= FocusPositionDetectorOther.Checked;
  FP_abs_sum                           := 0;
  for b:= twcUpper to twcLower do
    for x:= InPlane to CrossPlane do
      begin
      FP_avg[x,b]               := (FocusPositionAxisValue(x,twcCardinalAngles((Ord(b)+Ord(x)+u  ) mod 4))+
                                    FocusPositionAxisValue(x,twcCardinalAngles((Ord(b)+Ord(x)+u+2) mod 4)) )/2;
      FP_Average_mm[x,b].Caption:= FormatFloat('0.00',FP_avg[x,b]);
      FP_abs_sum                := FP_abs_sum+Abs(FP_avg[x,b]);
      end;
  for scan:= snGT to snAB do
    for angle:= Rot000 to Rot270 do
      if FP_Center_mm[scan,angle].Color=FPreadyColor then Inc(n);
  d_epi := FP_BankLevel_mm[twcReference].Value;
  d_high:= Clip(10    ,FP_BankLevel_mm[twcUpper].Value,d_epi);
  d_low := Clip(d_high,FP_BankLevel_mm[twcLower].Value,d_epi);
  ok    := (d_low>d_high) and (d_high>0);                                       //now also d_epi > 0
  if ok then
    try
      for b:= twcUpper to twcLower do
        FP_BankFactor[b].Caption:= Format('m=%0.3f',[FP_BankLevel_mm[twcReference].Value/FP_BankLevel_mm[b].Value-1]);
      a := 1/(d_epi*(1/d_low-1/d_high));                                        //a-factor of Chojnowski
      FP_BankFactor[twcReference].Caption:= Format('a=%0.3f',[a]);
     except
      a := 0;
      ok:= False;
     end;
  if ok and (n mod 8=0) then
    begin
    for x:= InPlane to CrossPlane do
      begin
      if ok then
        begin
        FP_Calc_Dbfso_mm[x]:= (FP_avg[x,twcUpper]-FP_avg[x,twcLower])*a;
        FP_Calc_Drot_mm[x] := (FP_avg[x,twcLower]*d_low*(d_epi-d_high) - FP_avg[x,twcUpper]*d_high*(d_epi-d_low))/(d_epi*(d_low-d_high));
        bfso               := FormatFloat('0.00',FP_Calc_Dbfso_mm[x]);
        rot                := FormatFloat('0.00',FP_Calc_Drot_mm[x]);
        end
      else
        begin
        bfso:= '-';
        rot := '-';
        end;
      FP_Dbfso_mm[x].Caption:= bfso;
      FP_Drot_mm[x] .Caption:= rot;
      end;
    if (n=8) or PaintResult then
      FocusPositionSquarePaint(4);
    FocusPositionZeroRotButton.Enabled:= (n=8) and ok and (Abs(FP_Calc_Drot_mm[Inplane])+Abs(FP_Calc_Drot_mm[Crossplane])>0.001) and
                                         (not FocusPositionDetectorRotates.Checked);
    FocusPositionZeroFPButton .Enabled:= (n=8) and ok and (Abs(FP_Calc_Dbfso_mm[Inplane])+Abs(FP_Calc_Dbfso_mm[Crossplane])>0.001); //and (not FocusPositionDetectorRotates.Checked);
    end; {n mod 8 =0}
  end; {ok}
for b:= twcUpper to twcReference do
  FP_BankFactor[b].Visible:= ok;
end; {~focuspositionchange_mm}


{=> RefUseGammaItem, RefUseAddToItem, RefUseUnrelatedToItem}
procedure TAnalyseForm.CalcSubMenuClick(Sender:TObject);
begin
with Sender as TMenuItem do
  begin
  Checked       := True;
  Parent.Caption:= Format(DefRefSubMenu,[CleanUpCaption(Caption).ToLower]);
  end;
if Visible then
  ViewItems(Sender);                                                            //ignored during initialisation
end; {~calcsubmenuclick}


{28/07/2020 Ft_XXXX[twcFieldClass] elements}
procedure TAnalyseForm.SymCorrectClick(Sender: TObject);
var s: twcDataSource;
begin
if Sender is TMenuItem then
  with Sender as TmenuItem do if Pos('SymCorrectItem',Name)>0 then
    begin
    if Pos('Meas',Name)>0 then s:= dsMeasured
    else                       s:= dsReference;
    Ft_SymCorrCheckbox[AppliedFieldClass,s].Checked:= Checked;
    Reload(Sender);
    end;
end; {~symcorrectclick}


//respond to RefDeviceSpecificItem
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.ReferenceDevSpecClick(Sender:TObject);
begin
Engines[UsedEngine].ArrayScanRefUse:= RefDeviceSpecificItem.Checked and RefDeviceSpecificItem.Enabled;
end; {~referencedevspecclick}


{09/11/2016}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.ReferenceGenericBeamClick(Sender: TObject);
begin
ModListBeamRadioButton.Visible           := RefGenericBeamItem.Checked;
Engines[UsedEngine].wReferenceFromGeneric:= RefGenericBeamItem.Checked;
if ModListBeamRadioButton.Checked and not RefGenericBeamItem.Checked then
  ModlistNormRadioButton.Checked         := True;
if Engines[UsedEngine].IsValid then
  Reload(Sender);
end; {~referencegenericbeamclick}


{14/01/2017 removed superfluous code}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.LocalPeakClick(Sender:TObject);
var b: Boolean;
    t: twcDataSource;
begin
b:= (Sender=MeasLocalPeakItem);
for t:= dsMeasured to twcLastRelated do
  with Engines[UsedEngine].wSource[t] do
    begin
    twFastScan := False;
    twLocalPeak:= b;
    if b then
      twAbsNormPos_cm:= CursorPos_cm;
    end;
OnDataRead(Sender);
end; {~localpeakclick}


//respond to ViewRightAxisToGridItem
{13/08/2020}
procedure TAnalyseForm.RightAxisToGridClick(Sender: TObject);
begin
AutoZoom(True);
ShowMenuItemStatus(Sender);
end; {~rightaxistogridclick}


procedure TAnalyseForm.MeasurementSaveClick(Sender:TObject);
begin
FileSave(pMeasured);
end; {~measurementsaveclick}


//build menu on the fly
{$push}{$warn 5092 off}
{10/09/2016 try..except}
{09/06/2020 add divisor}
procedure TAnalyseForm.PresetsMenuEnter(Sender:TObject);
var F: TSEarchRec;
    s: String;

    procedure AddItem(AString:String);
    var m: TMenuItem;
    begin
    m:= TMenuItem.Create(PresetsMenu);
    m.Checked:= LowerCase(Astring)=s;
    if AString[1] in ['a'..'z'] then
      AString[1]:= Chr(Ord(AString[1])-32);
    m.Caption:= PresetToName(AString);
    m.OnClick:= @PresetsItemClick;
    PresetsMenu.Insert(1,m);                                                    //there are two base items in this menu (savepresetitem and presetdivider1)
    end;

begin
try
  s:= Lowercase(ExtractFileName(PresetName));
  with PresetsMenu do
    while Count>1 do Delete(Pred(Count));                                       //clear old list
  if FindFirst(CommonAppData+'*!.ini',faArchive+faReadOnly,F)=0 then
   begin
   AddItem(F.Name);
   while FindNext(F)=0 do
     AddItem(F.Name);
   AddItem('-');                                                                //add divisor
   end;
 except
 end;
end; {~presetsmenuenter}
{$pop}


{22/09/2015 publishresults}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.PresetsItemClick(Sender:TObject);
begin
if Sender is TMenuItem then
  with Sender as TMenuItem do
    PresetName:= CleanUpCaption(Caption).ToLower;
PresetLoad(CommonAppData+PresetName);
if Engines[UsedEngine].IsValid then Reload(Self) {force complete processing}
else                                PublishResults;
end; {~presetsitemclick}


{=> ExitAction, MeasZeroStepsItem, FileSaveAsReferenceAction, HelpItem}
{16/06/2016 save reference: UnSetReferenceOrg}
{02/08/2016 makefilename replaced with makecurvename}
{03/08/2016 repaired usage of makecurvename which is not automatic anymore}
{10/12/2019 save as ref: invalidate reference}
{17/03/2020 RawDataEditor}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.OnMenu(Sender:TObject);
var Stg: string;
    l  : word;
    t1 : TControl;
    t2 : TWinControl;
    p  : TPoint;
begin
inherited;
l:= 100;
while OnDataReadBusy and (l>0) do
  begin
  WaitLoop(100);
  Dec(l);
  end;
if Sender=FileExitItem then
  Close
else if Sender=AboutItem then
  RunAboutBox(Sender)
else if Sender=HelpItem then
  begin
  if ActiveControl=nil then
    l:= 1
  else
    begin
    GetCursorPos(p);   {ControlAtPos(const Pos:TPoint;AllowDisabled:Boolean;AllowWinControls:Boolean=False): TControl;}
    t2:= ActiveControl;
    repeat
      t1:= t2.ControlAtPos(ScreenToClient(p),True,True); {Returns the child control located at a specified position within the control}
      if t1=nil then
        begin
        t1:= t2;
        t2:= nil;
        end
      else
        if t1 is TWinControl then
          t2:= TWinConTrol(t1)
        else
          t2:= nil;
    until t2=nil;
    l:= t1.HelpContext;
    end;
 {$IFDEF Windows}
  ExecuteHelp(l);
 {$ENDIF}
  end
else if Sender=FileSaveAsReferenceItem then                                     //*********save aas reference***************
  with Engines[UsedEngine] do
    begin
    Stg:= ReferenceDirectory+MakeCurveName(ArrayScanRefOk);
    SetMessageBar(Format(DefSavedText,[Stg]));
    if ((not ArrayScanRefOk) and (FileFormat<>twcWellhoferAscii_v6)) or MeasRemapCoordinates.Checked then
       WriteData(Stg,twcWellhoferAscii_v6,dsMeasured)
    else if RawDataEditor.Modified then
      SetMessageBar(Format(ModifiedText,[Stg]))
    else if (Stg<>EditorFilename) then
        begin
        l:= LogLevel;
        LogLevel:= 0;
        if FileExists(Stg) then
          DeleteFile(PChar(Stg));
        MemoSaveToFile(RawDataEditor,Stg);
        WaitLoop(500);
        UnSetReferenceOrg;
        ReadEditor(Sender);
        LogLevel:= l;
        end
      else
       SetMessageBar(Format(SameFileText,[Stg]));
    end
else OnDataRead(Sender);
end; {~onmenu}

(*
The menu system uses plain key shortcuts. In the current situation these are
handled by the menu when editing in a TEdit. These keys will not appear into the text.
When the OnEnter event of the TEdit calls EditEnter, the comple menusystem is disabled.
*)
{20/11/2020}
procedure TAnalyseForm.EditEnter(Sender: TObject);
begin
EnableMenuSystem(False);                                                        //disable all keyboard shortcuts because the view menu uses plain key shortcuts
end; {~editenter}


{20/07/2015 usage of IsCompositeData}
{01/08/2015 in-memory implementation of tempref}
{16/12/2015 ShowMenuItemStatus(ProcessSetTempRefItem) removed}
{02/02/2016 Logics changed to set-only}
{25/11/2018 NormAdjustFactor}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{29/09/2020 TempRefEngine}
{02/10/2020 do not wait for updatesettings}
procedure TAnalyseForm.ProcessSetTempRefClick(Sender:TObject);
begin
if MeasNormAdjustFactor<>1 then
  begin
  Engines[UsedEngine].Multiply(MeasNormAdjustFactor,dsMeasured,dsMeasured);
  MeasNormAdjustEdit.Value:= 100;
  MeasNormAdjustFactor    :=   1;
  end;
with Engines[UsedEngine] do
  begin
  wCheckRefCurveString          := ProcessCheckTempTypeItem.Checked;
  wCheckRefIgnoreLinac          := ProcessIgnoreTUnameItem .Checked and wCheckRefCurveString;
  end;
ProcessSetTempRefItem   .Checked:= Engines[UsedEngine].SetReferenceOrg(dsMeasured,True);  //sets wTakeCurrentRefSource True
ProcessUnsetTempRefItem .Checked:= not ProcessSetTempRefItem.Checked;
if ProcessSetTempRefItem.Checked then
  begin
  TempRefEngine:= UsedEngine;
  OnDataRead(Sender);                                                           //calls UpdateSettings -> wTakeCurrentRefSource:= ProcessSetTempRefItem.Checked
  end;
end; {~processsettemprefclick}


{=> ProcessUnsetTempRefItem}
{10/09/2015
  Make ProcessRedefineTempItem.Checked temporarily true for more logic
  reporting in statusbar}
{02/02/2016 rewritten to unset function}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{29/09/2020 TempRefEngine}
{30/09/2020 UnSetReferenceOrg for all engines}
{11/06/2021 do not isvalid test before UnSetReferenceOrg}
procedure TAnalyseForm.ProcessUnsetTempRefClick(Sender:TObject);
var i: Integer;
begin
ProcessUnsetTempRefItem.Checked:= True;
ProcessSetTempRefItem  .Checked:= False;
for i:= 0 to Length(Engines)-1 do
  Engines[i].UnSetReferenceOrg;
TempRefEngine:= -1;
ViewItems(Sender);
end; {~processunsettemprefclick}


{=> ProcessIgnoreTUnameItem, ProcessCheckTempTypeItem}
{13/08/2015}
{14/12/2020 call readeditor for reprocessing of reference-loading}
procedure TAnalyseForm.ProcessUpdateDataRead(Sender: TObject);
begin
UpdateSettings(Sender);
ReadEditor(Sender,True);
ShowMenuItemStatus(Sender);
end; {processupdatedataread}


{18/02/2023}
procedure TAnalyseForm.ProcessMirrorMeasRefClick(Sender: TObject);
var i: Integer;
begin
UpdateSettings(Sender);
i:= Length(Engines);
while i>0 do
  begin
  Dec(i);
  Engines[i].UnSetReferenceOrg;
  if not ProcessMirrorMeasRefItem.Checked then
    Engines[i].LoadReference;
  end;
Reload(Sender);
end; {~processmirrormeasrefclick}


{14/07/2015}
{12/08/2015 reorganised}
{24/01/2018 introduced ProcessClearMergeSourceItem}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.ProcessMergeSourceClick(Sender:TObject);
begin
with Engines[UsedEngine] do
  begin
  ProcessSetMergeSourceItem.Checked:= (not ProcessClearMergeItem.Checked) and IsValid;
  if ProcessSetMergeSourceItem.Checked then
    begin
    wSource[dsUnrelated].twLocked:= False;
    CopyCurve(dsMeasured,dsUnrelated);
    end;
  wSource[dsUnrelated] .twLocked:= ProcessSetMergeSourceItem.Checked;
  ProcessMergeItem     .Enabled := ProcessSetMergeSourceItem.Checked;
  ProcessClearMergeItem.Enabled := ProcessSetMergeSourceItem.Checked;
  ProcessClearMergeItem.Checked := False;
  ShowMenuItemStatus(Sender);
  end;
end; {~processmergeclick}


{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.ProcessResetFitClick(Sender:TObject);
begin
Engines[UsedEngine].ResetFit:= True;
OnDataread(Sender);
end; {~processresetfitclick}


{=> MeasCalcPDDfitItem}
{12/10/2022}
procedure TAnalyseForm.MeasCalcPDDfitClick(Sender: TObject);
begin
if Sender=MeasCalcPDDfitItem then
  PDDfitCheckBox.Checked:= not PDDfitCheckBox.Checked;
MeasCalcPDDfitItem.Checked:= PDDfitCheckBox.Checked;
end; {~meascalcpddfitclick}


{=> UseDoseAddButton}
{05/11/2016}
procedure TAnalyseForm.UseDoseAddButtonClick(Sender: TObject);
var i: Integer;
begin
i:= Length(UseDoseConvTable);
if i<OD2doseTableMax then
  begin
  SetLength(UseDoseConvTable,i+1);
  PopulateDoseConvList(i);
  end;
UseDoseDelButton.Enabled:= (i>0);
UseDoseAddButton.Enabled:= (i<Pred(OD2doseTableMax));
end; {~usedoseaddbuttonclick}


{=> UseDoseDelButton}
{05/11/2016}
procedure TAnalyseForm.UseDoseDelButtonClick(Sender: TObject);
var i: Integer;
begin
i:= Pred(Length(UseDoseConvTable));
if i>=0 then
  begin
  with UseDoseConvTable[i] do
    try
      DCDoseBox    .Free;
      DCBgBox      .Free;
      DCFilmTypeBox.Free;
      DCModalityBox.Free;
      DCEdit       .Free;
     except
      ExceptMessage('UseDoseDelButtonClick!');
     end;
  SetLength(UseDoseConvTable,i);
  end;
UseDoseDelButton.Enabled:= (i>1);
UseDoseAddButton.Enabled:= (i<OD2doseTableMax);
end; {~usedosedelbuttonclick}

(*
PlotLabelClick: responds to click on plotlabel, event set at runtime in FormCreate
*)
{15/12/2015 selectedseries}
{17/03/2020 Lazarus-port}
procedure TAnalyseForm.PlotLabelClick(Sender:TObject);
begin
SelectPlot:= True;
with Sender as TLabel do
  SelectedPlot:= PlotItems(Tag);                                                //tag is set represent value plotitem at initialisation
SelectedSeries:= PlotSeries[SelectedPlot];
OnSeriesSelected;
PlotCursor(Sender);
end; {~plotlabelclick}


{31/01/2020}
{17/03/2020 Lazarus-port}
procedure TAnalyseForm.LabelCopyClick(Sender: TObject);
var s: String;
    b: Boolean;
begin
if Sender is TLabel then
  with Sender as TLabel do
    begin
    s               := Trim(Caption);
    b               := ClipBoardLock; {preserve lockstate}
    ClipBoardLock   := True;
    ClipBoard.AsText:= s;
    ClipBoardLock   := b;
    end
else
  s:= '-';
SetMessageBar('Result: '+s);
end; {~labelcopyclick}


{=> ConfigSaveAsItem}
procedure TAnalyseForm.ConfigSaveAsItemClick(Sender:TObject);
var i,s,f: string;
begin
with FileSaveDialog do
  begin
  s         := FileName;  {save measurement file settings}
  f         := Filter;
  i         := InitialDir;
  InitialDir:= CommonAppdata;
  FileName  := ifthen(Sender=ConfigSaveAsItem,ConfigName,NameToPreset(PresetToName(PresetName),True));
  Filter    := 'Ini files (*.ini)|*.ini';
  if Execute then
    ConfigSave(Sender,FileName,Sender<>ConfigSaveAsItem);
  InitialDir:= i;        {restore measurement file settings}
  Filename  := s;
  Filter    := f;
  end;
end; {~configsaveasitemclick}


{18/04/2015}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.Ionisation2DoseClick(Sender:TObject);
begin
if Engines[UsedEngine].Ionisation2Dose then                                     //True if data appropriate for operation
  begin
  OnDataRead(Sender);
  MeasIon2DoseItem.Checked:= Engines[UsedEngine].wSource[dsMeasured].tw2DoseConv;
  end;
end; {~ionisation2doseclick}


{=> FitENRweigthedCheckBox, FitMubPowerFixedCheckBox}
{08/01/2016}
procedure TAnalyseForm.FitMubPowerFixedClick(Sender: TObject);
begin
FitMubPower_exp.Enabled:= AdvancedModeItem.Checked and FitMubPowerFixedCheckBox.Checked;
end; {~fitmubpowerfixedclick}


{25/12/2016 InventoryReader}
procedure TAnalyseForm.PageControlRequestChange(Sender         : TObject;
                                                var AllowChange: Boolean);
begin
if PageControl.ActivePage=InventoryTab then
  InventoryReaderSetup(False);
AllowChange:= not ModListGrid.EditorMode;
end; {~pagecontrolrequestchange}


{22/07/2015 OD to dose message removed}
{01/09/2015 activepage=analysistab and prevtab=advancedsettingstab ==> reload}
{20/03/2016 added DefAliasInfo}
{16/06/2016
  moved iterator-setting for conversion to ConvStartClick
  UnSetReferenceOrg}
{22/10/2018 if PageControl.ActivePage=SettingsTab then HistogramTab.TabVisible:= False}
{04/05/2020 last-moment filling RawDataEditor}
{14/08/2020 menus on for fieldtypestab}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{23/10/2020 removed PlaceResultsLabels(ifthen(PageControl.ActivePage=AnalysisTab,0,DefaultFontSize))}
{02/03/2021 when the reference is dropped, call LoadReference}
{30/11/2022 focuspositiontab}
procedure TAnalyseForm.PageControlChange(Sender:TObject);
var i: Integer;
    b: Boolean;

  procedure MenusOnOff(OnValue:Boolean);
  begin
  if OnValue then
    UImodeChange(Sender)
  else
    begin
    EnableMenu(ProcessingMenu ,False);
    EnableMenu(ViewMenu       ,False);
    EnableMenu(MeasMenu       ,False);
    EnableMenu(ReferenceMenu  ,False);
    EnableMenu(CalculationMenu,False);
    EnableMenu(OptionsMenu    ,False);
    end;
  UpdateSettings(Sender);
  end;

begin
ClipBoardLock:= False;
{$IFDEF PRELOAD}
PreloadTransfer(Sender);
{$ENDIF}
if PageControl.ActivePage=AnalysisTab then
  begin
  if (not ProcessSetTempRefItem.Checked) and RefAutoLoadItem.Checked then
    begin
    Engines[UsedEngine].UnSetReferenceOrg;   {force a reload of any automatic reference}
    Engines[UsedEngine].LoadReference;
    end;
  MenusOnOff(True);
  PDDFitResultsTab.TabVisible:= MeasUsePDDfitModelItem.Enabled;
  if PrevTab=InventoryTab then
    OnDataRead(Sender)
  else if not ((PrevTab=HistogramTab) or (PrevTab=LogTab) or (PrevTab=PDDFitResultsTab) or OnDataReadBusy) then
    Reload(Sender);
  end
else if PageControl.ActivePage=FieldTypesTab then
  MenusOnOff(True)
else if PageControl.ActivePage=ConfigurationTab then
  ModListRadioButtonClick(Sender)
else if PageControl.ActivePage=FileConversionTab then
  FileConvStartCheck(Sender)
else if PageControl.ActivePage=LogTab then with LogTabMemo do
  begin
  ClipBoardLock:= True;
  MenusOnOff(False);
  SelStart:= Length(LogTabMemo.Text)-Length(Lines[Lines.Count-1])-2;
  SelLength:= 0;
  end
else if PageControl.ActivePage=FileConversionTab then
  FormResize(Sender)
else
  begin
  MenusOnOff(False);
  FitMubPowerFixedClick(Sender);
  MeasRemappingString.Enabled:= MeasRemappingBox.Enabled;
  if PageControl.ActivePage=HistogramTab then
    ShowHistogram
  else if PageControl.ActivePage=PDDFitResultsTab then
    ShowFitResults
  else if PageControl.ActivePage=AliasTab then
    SetMessageBar(DefAliasInfo)
  else if PageControl.ActivePage=SettingsTab then
    HistogramTab.TabVisible:= False
  else if PageControl.ActivePage=FocusPositionTab then
    FocusPositionStateUpdate(Sender)
  else if PageControl.ActivePage=InventoryTab then
    begin
    with InventoryGrid do
      begin
      FormResize(Sender);
      RowCount                        := 1;
      RowHeights[0]                   := 30;
      ColCount                        := Succ(String(DefInventoryTitles).CountChar(','));
      Rows[0].CommaText               := DefInventoryTitles;
      FileIterator.OnAddFile          := @InventoryDoFile;
      FileIterator.OnTerminate        := nil;
      FileIterator.Options            := [fiPath];
      InventoryAltAxisCheckBox.Enabled:= MeasRemappingString.Text<>twcMeasAxisStandard;
      InventoryMultiScanList          := False;
      if      InventoryRadioRef .Checked then Sender:= InventoryRadioRef
      else if InventoryRadioSelf.Checked then Sender:= InventoryRadioSelf
      else if InventoryRadioData.Checked then Sender:= InventoryRadioData;
      InventoryDirBoxChange(Sender);
      end;
    end
  else if PageControl.ActivePage<>LogTab then
    begin
    FillCheckListCombo;
    GetWellhoferValues;        //MessageBar:= 'dose= a + b*OD/f + c*(OD/f)^2 + d*(OD/f)^3 + e*(OD/f)^4';
    end;
  end;
PrevTab:= PageControl.ActivePage;
i      := PageControl.ActivePage.ControlCount;
b      := False;
while (not b) and (i>0) do
  begin
  Dec(i);
  b:= PageControl.ActivePage.Controls[i] is TMemo;
  end;
if b then ActiveMemo:= TMemo(PageControl.ActivePage.Controls[i])
else      ActiveMemo:= nil;
FileSaveMeasurementItem.Caption:= '&Save '+ifthen(b,'text','Measurement')+' as...';
inherited;
end; {~pagecontrolchange}


//conversion tab implementation
{=> FileConvSourceListBox, FileConvDestinationListBox, FileConvSamePath, FileConvSourceRecursive, FileConvLowerCase, FileConvMakeFileName, FileConvIon2DoseCheckBox}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.FileConvStartCheck(Sender:TObject);
var b: Boolean;

begin
if FileConvSamePath.Checked then
  FileConvDestinationPath.Text     := FileConvSourcePath.Text;
Engines[UsedEngine].AcceptZeroSteps:= MeasZeroStepsItem.Checked;
FileConvSourceDir                  := FileConvSourcePath.Text;
FileConvDestDir                    := FileConvDestinationPath.Text;
FileConvNameMask   .Visible        := FileConvMakeFileName.Checked;
FileConvDestExt                    := FileConvGetFileExt(FileConvDestinationListBox);
FileConvStartButton.Enabled        :=
  (FileConvSourceListBox.ItemIndex>=0) and (FileConvDestinationListBox.ItemIndex>=0)  and
   FileConvNameCheck                   and  DirectoryExists(FileConvSourceDir)        and
  (DirectoryExists(FileConvDestDir) or FileConvSamePath.Checked);
with FileConvDestinationPath do
  begin
  b:= Enabled;
  Enabled:= not FileConvSamePath.Checked;
  if Enabled<>b then
    if Enabled then
      SetFocus
    else
      FileConvFileNameDisplay(FileConvDestinationPath,FileConvSourcePath.Text);
  end;
UImodeChange(Sender);
end; {~fileconvstartcheck}


//conversion tab implementation
{$push}{$warn 5091 off: c not initialised}
function TAnalyseForm.FileConvNameCheck: Boolean;
var i,j,n: ShortInt;
    b    : Boolean;
    x    : ConvNameItems;
    c    : ConvItemList;
    p    : ^ConvItemList;
begin
SetLength(c,0);
i     := 0;
n     := 0;
Result:= True;
if FileConvMakeFileName.Checked then
  begin
  with FileConvNameMask do
    while (i<Length(Text)) and Result do
      begin
      b:= False;
      for x:= ConvListStart to ConvSeparator do if (not b) and (Length(FileConvItemPatterns[x])>0) then
        begin
        j:= String(Text).IndexOf(FileConvItemPatterns[x],i);                    //zero-based
        b:= (j=i);
        if b then
          begin
          i:= j+Length(FileConvItemPatterns[x]);
          case x of
            ConvListStart: begin
                           SetLength(c,0);
                           Inc(n);
                           end;
            ConvListEnd  : if (Length(c)>0) and (n>0) then
                             begin
                             case c[0] of
                               ConvListXType: p:= @FileConvPhotonItems;
                               ConvListEType: p:= @FileConvElectronItems;
                              else            p:= @FileConvGeneralItems;
                              end;
                             p^:=  Copy(c,0,Length(c));
                             Dec(n);
                           end;
            else           if n>0 then
                             begin
                             SetLength(c,Length(c)+1);
                             c[Pred(Length(c))]:= x;
                             end;
           end; {case}
          end; {if b}
        end;
      Result:= b;
      end; {while}
    Result:= b and (Length(FileConvPhotonItems)+Length(FileConvElectronItems)+Length(FileConvGeneralItems)>0);
    end;
end; {~fileconvnamecheck}
{$pop}


//conversion tab implementation
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{26/04/2022 include FFF in name}
function TAnalyseForm.FileConvMakeName(DefaultName:string): String;
var i,j: Integer;
    p  : ^ConvItemList;
    s,a: string;
begin
s:= '';
with Engines[UsedEngine] do
  begin
  p:= @FileConvGeneralItems;
  case BeamType of
    Photons  : if Length(FileConvPhotonItems  )>0 then p:= @FileConvPhotonItems;
    Electrons: if Length(FileConvElectronItems)>0 then p:= @FileConvElectronItems;
   end;
  j:= Length(p^)-1;
  if j>=0 then
    for i:= 0 to j do
      begin
      case p^[i] of
        ConvScanType : case ScanType of
                         snGT      : a:= 'I';
                         snAB      : a:= 'C';
                         snPDD     : a:= 'D';
                         snFanline : a:= 'B';
                         snFreescan: a:= 'F';
                         snPlane   : a:= 'P';
                        else         a:= 'U';
                        end;
        ConvModality : case BeamType of
                         Photons  : a:= 'X';
                         Electrons: a:= 'E';
                        else        a:= 'O';
                        end;
        ConvEMV      : a:= Format('%2.2d',[Round(Energy           )])+ifthen(IsFFF,'FFF','');
        ConvEkV      : a:= Format('%3.3d',[Round(Energy*1000      )]);
        ConvDcm      : a:= Format('%2.2d',[Round(FieldDepth_cm    )]);
        ConvDmm      : a:= Format('%3.3d',[Round(FieldDepth_cm*10 )]);
        ConvLcm      : a:= Format('%2.2d',[Round(FieldLength_cm   )]);
        ConvXcm      : a:= Format('%2.2d',[Round(FieldGT_cm       )]);
        ConvYcm      : a:= Format('%2.2d',[Round(FieldAB_cm       )]);
        ConvLmm      : a:= Format('%3.3d',[Round(FieldLength_cm*10)]);
        ConvXmm      : a:= Format('%3.3d',[Round(FieldGT_cm*10    )]);
        ConvYmm      : a:= Format('%3.3d',[Round(FieldAB_cm*10    )]);
        ConvFtype    : if WedgeAngle>0 then a:= 'W'
                       else                 a:= 'O';
        ConvDet      : a:= Format('%s',[wDetectorInfo.twDetType ]);
        ConvSeparator: a:= '_';
       else       a:= '';
       end; {case}
      s:= s+a;
      end;
  end; {with}
if s='' then
  Result:= DefaultName
else
  Result:= ExtractFilePath(DefaultName)+s+ExtractFileExt(DefaultName);
end; {~fileconvmakename}


//conversion tab implementation
{=> FileConvNameMask}
procedure TAnalyseForm.FileConvNameMaskEnter(Sender:TObject);

  procedure AddSyntax(Stg:string);
  begin
  FileConvList.Lines.Add(Stg.Replace('\t',chTab));
  end;

begin
EnableMenuSystem(False);
if FileConvMakeFileName.Checked then
  begin
  inherited;
  FileConvList.Lines.Clear;
  AddSyntax(ConvSyntax01);
  AddSyntax(ConvSyntax02);
  AddSyntax(ConvSyntax03);
  AddSyntax(ConvSyntax04);
  AddSyntax(ConvSyntax05);
  AddSyntax(ConvSyntax06);
  AddSyntax(ConvSyntax07);
  AddSyntax(ConvSyntax08);
  AddSyntax(ConvSyntax09);
  AddSyntax(ConvSyntax10);
  AddSyntax(ConvSyntax11);
  AddSyntax(ConvSyntax12);
  AddSyntax(ConvSyntax13);
  AddSyntax(ConvSyntax14);
  AddSyntax(ConvSyntax15);
  AddSyntax(ConvSyntax16);
  AddSyntax(ConvSyntax17);
  AddSyntax(ConvSyntax18);
  AddSyntax(ConvSyntax19);
  AddSyntax(ConvSyntax99+': '+ConvDefMask);
  end;
FileConvStartCheck(Sender);
end; {~fileconvnamemaskenter}


//conversion tab implementation
{=> FileConvSourcePath, FileConvDestinationPath}
procedure TAnalyseForm.FileConvPathBtnClick(Sender:TObject);
var s: string;
begin
inherited;
with Sender as TEdit do
  begin
  s:= Text;
  if not DirectoryExists(s) then
    GetDir(0,s);
  SelectDirectory(s,[sdAllowCreate, sdPerformCreate, sdPrompt],0);
  Text:= s;
  end;
end; {~fileconvpathbtnclick}


//conversion tab implementation
{=> FileConvStartButton}
procedure TAnalyseForm.FileConvStartClick(Sender:TObject);
var i: byte;
begin
FileConvStartCheck(Sender);
FileConvList.Clear;
i:= Pos(FileConvGetFileExt(FileConvDestinationListBox),'.txt.mcc.wtx.wda.asc') div 4;
case i of
   0: FileConvDestType:= twcWellhoferAscii_v6;
   1: FileConvDestType:= twcMccProfile;
   2: FileConvDestType:= twcWTX;
   3: FileConvDestType:= twcWDA;
   4: FileConvDestType:= twcRFA_ascii;
 else FileConvDestType:= twcUnknown;
 end;
ClipBoardLock:= True;
with FileIterator do
  begin
  SetMessagebar(FileConvSourceDir);
  OnAddFile        := @FileConvDoFile;
  OnTerminate      := @FileConvIteratorTerminate;
  RootFolder       := FileConvSourceDir;
  Filter           := '*'+FileConvGetFileExt(FileConvSourceListBox);
  FileConvOkCount  := 0;
  FileConvMultCount:= 0;
  if FileConvSourceRecursive.Checked then Options:= [fiPath,fiRecurseFolders]
  else                                    Options:= [fiPath                 ];
  ControlsEnable(FileConversionTab,False);
  Iterate;
  FileConvList.Lines.Add(Format(ConvResultText,[FileConvOkCount,FoundFiles+FileConvMultCount-FileConvOkCount]));
  end;
end; {~fileconvstartclick}


//conversion tab implementation
{23/11/2015 multiscan suport}
{16/06/2016 wMultiScanMax:= 0}
{09/09/2020 add scan number to file name when wMultisScanMax>1 and filename is not generated}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{$push}{$warn 5024 off:parameters not used}
procedure TAnalyseForm.FileConvDoFile(Sender         : TObject;
                                      const AFileName: TFileName;
                                      const AFileInfo: TFileInfo);
var p,f,r: string;
begin
FileConvFileNameDisplay(FileConvSourcePath     ,'-');
FileConvFileNameDisplay(FileConvDestinationPath,'-');
with Engines[UsedEngine] do
  begin
  wMultiScanNr:= 0;
  try
    repeat
      Inc(wMultiScanNr);
      if ReadData(AFileName) then
        begin
        if wMultiScanNr>1 then
          Inc(FileConvMultCount);
        if FileConvIon2DoseCheckBox.Checked then
          Ionisation2Dose;
        FileConvFileNameDisplay(FileConvSourcePath,AFileName);
        if wMultiScanMax>1 then
          r:= '_s'+Num2Stg(wMultiScanNr,Trunc(Log10(wMultiScanMax))+1,'0')
        else
          r:= '';
        if FileConvMakeFileName.Checked then
          f:= FileConvMakeName(AFileName)
        else
          begin
          f:= AFileName;
          if Length(r)>0 then
            Insert(r,f,Pos(ExtractFileExt(AFileName),AFileName));
          end;
        p:= ExtractFileName(f);
        if IsPathDelimiter(p,1) then
          Delete(p,1,1);
        if FileConvLowerCase.Checked then
          p:= LowerCase(p);
        f:= AppendPathDelim(FileConvDestDir)+ChangeFileExt(p,FileConvDestExt);  //input of ChangeFileExt cannot be output of other function
        if (f<>AFileName) and (FileConvOverWrite.Checked or (not FileExists(f))) then
          begin
          if Length(r)>0 then
            FileConvList.Lines.Add(f);
          p:= ExtractFilePath(f);
          if CreatePath(p) and WriteData(f,FileConvDestType) then
            begin
            FileConvFileNameDisplay(FileConvDestinationPath,f);
            Inc(FileConvOkCount);
            if FileConvMakeFileName.Checked then r:= ExtractFileName(FileName)
            else                                 r:= ConvOkText;
            end;
          end
        else r:= Format(ConvExistText,[ExtractFileName(f)+ifthen(wMultiScanMax>1,' ('+Num2Stg(wMultiScanNr)+'/'+Num2Stg(wMultiScanMax)+')','')]);
        end
      else r:= Format(ConvErrText,[LastMessage]);
    until (wMultiScanNr=wMultiScanMax) or (not Engines[UsedEngine].MultiScanCapable);
   except
    r:= 'unforseen exception';
   end;
  end;
FileConvList.Lines.Add(AFileName+': '+r);
end; {~fileconvdofile}
{$pop}


//conversion tab implementation
{16/05/2020 UImodeChange added}
procedure TAnalyseForm.FileConvIteratorTerminate(Sender:TObject);
begin
ControlsEnable(FileConversionTab,True);
UImodeChange(Sender);
ClipBoardLock:= False;
FileConvFileNameDisplay(FileConvSourcePath     ,FileConvSourceDir);
FileConvFileNameDisplay(FileConvDestinationPath,FileConvDestDir  );
FileConvStartCheck(Sender);
if FileIterator.FoundFiles=0 then
  SetMessageBar(ConvNoFilesText);
end; {~fileconviteratorterminate}


//conversion tab implementation
{=> FileConvNameMask}
procedure TAnalyseForm.FileConvNameMaskKeyPress(Sender :TObject;
                                                var Key:Char);
var b    : Boolean;
    x    : ConvNameItems;
    j,k,l: ShortInt;
begin
if Sender is TEdit then with Sender as TEdit do
  begin
  b:= (Key in csAsciiCtl);
  if b then
    Key:= #0;
  for x:= ConvListStart to ConvSeparator do if (not b) then
    begin
    j:= FileConvItemPatterns[x].IndexOf(Key);                                   //zero-based
    k:= j;
    if SelLength=0 then l:= k
    else                l:= 0;
    b:= (j=0) or
        ((k>0) and (Length(Text)>k) and
         (Copy(FileConvItemPatterns[x],1,k)=Copy(Text,SelStart-l,Max(k,SelLength))) );
    end;
  if not b then
    begin
    Key:= #0;
    SetMessageBar(Format(ConvErrKeyText,[Key]));
    end;
  end;
Inherited;
end; {~fileconvnamemaskkeypress}


//files tab implementation
{see PageControlChange for coupling to separate thread}
{29/03/2016 ignore unregistered files}
{10/09/2016 try..except}
{25/12/2016 InventoryReader}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{$push}{$warn 5024 off:Parameter "AFileInfo" not used}
procedure TAnalyseForm.InventoryDoFile(Sender         : TObject;
                                       const AFileName: TFileName;
                                       const AFileInfo: TFileInfo);
begin
try
  {$IFDEF THREAD_FILES}
  if assigned(ScanListThread) and ScanListThread.Terminated then
    FileIterator.Cancel
  else
    if Wellhofer.IsRegisteredFileType(AFileName) and assigned(InventoryReader) and InventoryReader.ReadData(AFileName) then
      begin
      PostMessage(self.Handle,WM_USER,0,0); {links to WMADDINVENTORY}
      //Application.ProcessMessages; this triggers an exception in fpc: execution of mainthread code
      end;
  {$ELSE}
  if Engines[UsedEngine].IsRegisteredFileType(AFileName) and
     assigned(InventoryReader)                           and
     InventoryReader.ReadData(AFileName)                 then
    InventoryAddFile;
  {$ENDIF THREAD_FILES}
 except
  {do nothing}
 end;
end; {~inventorydofile}
{$pop}


//files tab implementation
{called through fileiterator->InventoryDoFile or directly by InventoryDBChgAction}
{25/12/2016 InventoryReader}
{17/05/2020 display no contents for unexpanded multiscan file}
procedure TAnalyseForm.InventoryAddFile;
var i: Integer;
    s: String;
    b: Boolean;

  procedure AddCellText(const ACol,ARow:Integer;
                        const AText    :String);
  begin
  with InventoryGrid do
    begin
    Cells[ACol,ARow]:= AText;
    ColWidths[ACol]:= Max(ColWidths[ACol],Canvas.TextWidth(Atext)+6);
    end;
  end;

  function FormatParameter(AValue  :twcFloatType;
                           Decimals:Integer=2;
                           aFixed  :Boolean=False): String;
  begin
  Result:= Format('%0.*f',[ifthen((not aFixed) and (Round(AValue)=AValue),0,Decimals),AValue]);
  if DefaultFormatSettings.DecimalSeparator<>'.' then
    Result:= AnsiReplaceStr(Result,'.',DefaultFormatSettings.DecimalSeparator);
  end;

begin
if assigned(InventoryReader) then with InventoryReader do
  begin
  s:= FileName;
  if InventoryMultiScanList then
    s:= s+DefMultiScanSep+Num2Stg(wMultiScanNr,1+Trunc(Log10(wMultiScanMax)),'0');
  if Length(s)>0 then
    begin
    if LoglevelEdit.Value>1 then
      SetMessagebar(s);
    i                     := InventoryGrid.RowCount;
    InventoryGrid.RowCount:= Succ(i);
    if i=1 then
      InventoryGrid.FixedRows:= 1; {can only be set when number of rows > fixedrows}
    b:= InventoryMultiScanList xor (wMultiScanMax=1);
    AddCellText(0,i,ExtractFileName(s));
    AddCellText(1,i,Num2Stg(ifthen(InventoryMultiScanList,1,wMultiScanMax)));
    AddCellText(2,i,Linac);
    AddCellText(3,i,ifthen(b,wSource[dsMeasured].twBeamInfo.twBModality+FormatParameter(Energy,3),'-'));
    AddCellText(4,i,ifthen(b,Format('%sx%s',[FormatParameter(FieldGT_cm),FormatParameter(FieldAB_cm)]),'-'));
    AddCellText(5,i,ifthen(b,wCurveInfo.twDesTypeString,'-'));
    AddCellText(6,i,ifthen(b,ifthen(ScanType in twcHoriScans,FormatParameter(FieldDepth_cm,2,True),'-'),'-'));
    AddCellText(7,i,ifthen(b,wSource[dsMeasured].twMeasTime,'-'));
    end;
  end;
end; {~inventoryaddfile}


//files tab implementation: set alignment of selected columns in inventorygrid
{30/05/2020}
{$push}{$warn 5024 off}
procedure TAnalyseForm.InventoryPrepareCanvas(Sender   : TObject;
                                              aCol,aRow: Integer;
                                              aState   : TGridDrawState);
var ts: TTextStyle;
begin
if aCol in [1,4,6] then
  begin
  ts                            := InventoryGrid.Canvas.TextStyle;
  if aCol=4 then ts.Alignment   := taCenter
  else           ts.Alignment   := taRightJustify;
  InventoryGrid.Canvas.TextStyle:= ts;
  end;
end; {~inventorypreparecanvas}
{$pop}


//files tab implementation
//regulate availability of options in inventorypopupmenu
{30/05/2020}
{07/09/2020 safety catch}
{$push}{$warn 5024 off}
procedure TAnalyseForm.OnInventorySelect(Sender   : TObject;
                                         aCol,aRow: Integer);
begin
if (aRow>0) and (aRow<InventoryGrid.RowCount) then
  begin
  InventoryPopExpandItem.Enabled:= (Pos(InventoryGrid.Cells[1,aRow],'-1')=0);
  InventoryPopReturnItem.Enabled:= (Pos(DefMultiScanSep,InventoryGrid.Cells[0,aRow])>0);
  InventoryPopDelItem   .Enabled:= not InventoryPopReturnItem.Enabled;
  end;
end; {~inventoryselect}
{$pop}


//files tab implementation
{$IFDEF THREAD_FILES}
{$push}{$warn 5024 off:wellform.pas(860,31) Hint: Parameter "Msg" not used}
{Multithreaded binding of output of file search to InventoryAddFile
 started from InventoryDoFile}
procedure TAnalyseForm.WMADDINVENTORY(var Msg:TMessage);
begin
InventoryAddFile;
end; {~wmaddinventory}
{$pop}
{$ENDIF THREAD_FILES}


//files tab implementation
{07/09/2020 only for rowcount>1}
procedure TAnalyseForm.InventorySort(AColumn:Integer=0);
const Sep= '@';
var TL : TStringList;
    i,n: Integer;
    Stg: String;
begin
n:= InventoryGrid.RowCount;
if n>1 then
  try
    AColumn  := Clip(AColumn,0,Pred(InventoryGrid.ColCount));
    TL       := TStringList.Create;
    TL.Sorted:= False;
    for i:= 1 to n-1 do
      TL.Add(InventoryGrid.Rows[i].Strings[AColumn]+Sep+InventoryGrid.Rows[i].DelimitedText);
    TL.Sort;
    for i:= 0 to n-2 do
      begin
      Stg:= TL.Strings[i];
      InventoryGrid.Rows[i+1].Text:= ''; {prevent memory problems by first clearing the string}
      InventoryGrid.Rows[i+1].DelimitedText:= Copy(Stg,Pos(Sep,Stg)+1,Length(Stg));
      end;
   finally
    try
      TL.Free
     except
      ExceptMessage('InventorySort!');
     end;
   end;
end; {inventorysort}


//files tab implementation
{onclick in dirbox grid}
{=> InventoryGrid, InventoryPopupMenu, InventoryPopOpenItem, InventoryPopDelItem}
{25/12/2016 InventoryReader}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.InventoryGridDblClick(Sender:TObject);
var i,j      : Integer;
    s        : String;
    SubSelect: Boolean;
    tp       : TPoint;
begin
with InventoryGrid do
  begin
  i:= 0;
  j:= 0;
  tp:= ScreenToClient(Mouse.CursorPos);
  MouseToCell(tp.X,tp.Y,j,i);
  end;
if InventoryListReady and (i=0) then
   InventorySort(j)
else
  begin
  s        := InventoryGrid.Cells[0,i];
  i        := Pos(DefMultiScanSep,s);                                           //from now i is used for multiscans
  SubSelect:= (i>0);
  if SubSelect then
    begin
    j:= Length(s)-i;
    Val(Copy(s,Succ(i),j),Engines[UsedEngine].wMultiScanNr);
    Delete(s,i,Succ(j));
    end
  else
    s:= AppendPathDelim(InventoryDirBox.Directory)+s;
  DataFromEditor:= False;  {read into wellhofer only}
  InventoryMultiScanList:= Engines[UsedEngine].GetFiletype(s) in twcMultiFiles;
  if Sender=InventoryPopDelItem then
    begin
    if SubSelect then
      SetMessageBar('Scans from a multiple data file cannot be deleted')
    else
      begin
      if DeleteFile(PChar(s)) then
        with InventoryGrid do
          begin
          for i:= Selection.Top to InventoryGrid.RowCount-2 do
            Rows[i].Assign(Rows[i+1]);
          RowCount:= RowCount-1;
          end
      else
        SetMessageBar(s+' could not be deleted');
      end;
    end
  else if SubSelect or (not InventoryMultiScanList) then
    begin {transfer file/scan to editor}
    DataFileOpen(s,not SubSelect);
    PageControl.ActivePage:= Analysistab;
    PageControlChange(Sender);
    end
  else
    begin {just read and explore a multiscan file}
    InventoryReader.Filename:= s;
    InventoryGrid  .RowCount:= 1;
    InventoryDirBoxChange(Sender);
    end;
  end;
end; {~inventorygriddblclick}


//files tab implementation
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.InventorySetRefDirClick(Sender:TObject);
begin
if Sender=InventorySetRefDirButton then
  begin
  Engines[UsedEngine].ReferenceDirectory:= InventoryDirBox.Directory;
  InventorySetRefDirButton.Enabled      := False;
  end;
SetMessageBar('ReferenceDir='+Engines[UsedEngine].ReferenceDirectory);
end; {~inventorysetrefdirclick}

(*
files tab implementation
Is invoked when on map icon clicked and some path is chosen/entered.
It seems that is not passed to InventoryDirBox.Directory automatically.}
*)
{07/09/2020}
procedure TAnalyseForm.InventoryDirBoxAccept(Sender   : TObject;
                                             var Value: String);
begin
InventoryDirBox.Directory:= Value;
InventoryDirBox.Text     := Value;
InventoryDirBoxChange(Sender);
end; {~inventorydirboxaccept}


//files tab implementation
{see PageControlChange for initialisation of iterator}
{=> InventoryDirBox, InventoryRadioRef, InventoryRadioData, InventoryRadioSelf}
{25/12/2016 InventoryReader}
{30/05/2020 added InventoryPopReturnItem as possible sender}
{07/09/2020 when nothing is checked: InventoryDirBox.Directory:= InventoryDirBox.Text; always make text and directory equal}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.InventoryDirBoxChange(Sender:TObject);
var i: integer;
begin
if not InventoryListReady then
  begin
  {$IFDEF THREAD_FILES}
  i:= 0;
  while (not InventoryListReady) and (i<10) do
    begin
    try
      FileIterator.Cancel;
      WaitLoop(100);
      FileIterator.RootFolder:= '';
     except
       waitLoop(100);
     end;
     Inc(i);
     end;
  {$ENDIF THREAD_FILES}
  end;
if InventoryTab.Enabled and InventoryReaderSetup then
  begin
  InventoryListReady        := False;
  InventoryGrid  .OnDblClick:= nil;
  RePaint;
  if      Sender=InventoryRadioRef  then InventoryDirBox.Directory:= Engines[UsedEngine].ReferenceDirectory
  else if Sender=InventoryRadioSelf then InventoryDirBox.Directory:= ExtractFilePath(ParamStr(0))
  else if Sender=InventoryRadioData then InventoryDirBox.Directory:= CommonAppdata
  else                                   InventoryDirBox.Directory:= InventoryDirBox.Text;
  if DirectoryExists(InventoryDirBox.Directory) and
    ((InventoryGrid.RowCount=1) or (FileIterator.RootFolder<>InventoryDirBox.Directory) or (Sender=InventoryDirBox) or (Sender=InventoryPopReturnItem)) then
    begin
    InventoryDirBox.Text:= InventoryDirBox.Directory;                           //always make text and directory equal
    with InventoryGrid do
      begin
      FixedCols:= 0;
      RowCount:= 1;
      for i:= 0 to Pred(ColCount) do
        ColWidths[i]:= Canvas.TextWidth(Cells[i,0])+5;
      WaitLoop(20);
      end;
    {$IFDEF THREAD_FILES}
    try
      ScanListThread:= THelpEventThread.Create(@InventoryDBChgAction,Sender);
     except
      InventoryListReady:= True;
     end
    {$ELSE}
    InventoryDBChgAction(Sender);
    {$ENDIF THREAD_FILES}
    end
  else
    InventoryDBChgComplete(Sender);
  end;
end; {~inventorydirboxchange}


//files tab implementation
{See InventoryDirBoxChange; dirbox update through either main thread or separate thread
 Separate branch for multiscanfiles.}
{25/12/2016 InventoryReader}
{29/04/2020 adapted for fpc: InventoryDirBox holds one item only}
{14/08/2020 check for looping before end}
procedure TAnalyseForm.InventoryDBChgAction(Sender:TObject);
var LastNumber: Integer;
begin
if InventoryMultiScanList then
  begin
  InventoryReaderSetup;
  with InventoryReader do
    begin
    wMultiScanNr:= 0;
    LastNumber  := 0;
    while (wMultiScanNr>=LastNumber) and (wMultiScanNr<wMultiScanMax) do
      begin
      Inc(LastNumber);                                                          //safety catch: wMultiScanNr might loop early
      Inc(wMultiScanNr);
      if ReadData(FileName) and (wMultiScanNr>=LastNumber) then                 //wMultiScanNr might be set to start by ReadData when last valid scan is found
        {$IFDEF THREAD_FILES}
        begin
        PostMessage(self.Handle,WM_USER,0,0);
        Application.ProcessMessages;
        end;
        {$ELSE}
        InventoryAddFile;
        {$ENDIF THREAD_FILES}
      end {while}
    end;
  end
else
  begin
  FileIterator.RootFolder:= InventoryDirBox.Directory;
  {$IFDEF Windows}
  FileIterator.Filter    := '*.*';
  {$ELSE}
  FileIterator.Filter    := '*';                                                //Alan Chamberlain for Fedora platform
  {$ENDIF}
  FileIterator.Iterate;
  end;
InventoryDBChgComplete(Sender);
end; {~inventorydbchgaction}


//files tab implementation
{31/03/2016 SetMessageBar(DefInventoryListReady);}
{25/12/2016 InventoryReader}
{28/09/2017 set InventoryGrid font color to clWindowText}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.InventoryDBChgComplete(Sender:TObject);

  function TestPath(APath:String): Boolean;
  begin
  Result:= IncludeTrailingBackslash(LowerCase(InventoryDirBox.Directory))=IncludeTrailingBackslash(LowerCase(APath))
  end;

begin
InventorySort;
InventoryRadioRef       .Checked   := InventoryRadioRef .Checked or  TestPath(Engines[UsedEngine].ReferenceDirectory);
InventoryRadioData      .Checked   := InventoryRadioData.Checked and TestPath(CommonAppdata);
InventorySetRefDirButton.Enabled   := InventoryRadioRef .Checked and (not TestPath(Engines[UsedEngine].ReferenceDirectory));
InventoryGrid           .OnDblClick:= @InventoryGridDblClick;
InventoryGrid           .Font.Color:= clWindowText;
InventoryMultiScanList             := False;
InventoryListReady                 := True;
OnInventorySelect(Sender,0,1);
{$IFDEF THREAD_FILES}
ScanListThread                     := nil;
InventoryReaderSetup(False);
{$ENDIF THREAD_FILES}
SetMessageBar(DefInventoryListReady);
end; {~inventorydbchgcomplete}


//files tab implementation
{25/12/2016}
{28/09/2017 set InventoryGrid font color to clGray, set function result}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
function TAnalyseForm.InventoryReaderSetup(irCreate:Boolean=True): Boolean;
begin
if irCreate then InventoryGrid.Font.Color:= clGray
else             InventoryGrid.Font.Color:= clWindowText;
if irCreate and (not assigned(InventoryReader)) then
  begin
  try
    InventoryReader:= TWellhoferData.Create;
   except
    ExceptMessage('InventoryReaderSetup failed');
   end;
  Engines[UsedEngine].PassSettings(InventoryReader);
  InventoryReader.AutoLoadReference:= False;
  InventoryReader.wMeas2TankMapping:= ifthen(InventoryAltAxisCheckBox.Checked,MeasReMappingString.Text,twcMeasAxisStandard);
  InventoryReader.wFullAnalysis    := False;
  end
else if (not irCreate) and (assigned(InventoryReader)) then
  begin
  try
    InventoryReader.Free
   except
    ExceptMessage('InventoryReaderSetup!');
   end;
  InventoryReader:= nil;
  end;
Result:= not (irCreate xor assigned(InventoryReader));
end; {~inventoryreadersetup}


//for each engine an activity counter is maintained, which should be zero when ready
{24/01/2018}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{24/05/2022 wDebugInfo}
function TAnalyseForm.CheckWellhoferReady: Boolean;
begin
Result:= Engines[UsedEngine].EngineReady;
if not Result then
  SetMessageBar('Analysis object reports busy state: '+Engines[UsedEngine].wDebugInfo);
end; {checkwellhoferready}


{10/09/2016 try..except}
{13/01/2017 IncludeExtension}
function TAnalyseForm.PresetToName(AFileName       :String;
                                   IncludeExtension:Boolean=False): String;
var Ext: String;
    i  : Integer;
begin
try
  Ext   := ExtractFileExt(AFileName);
  i     := Length(AFileName)-Length(Ext);
  Result:= Copy(AFileName,1,ifthen(AFilename[i]='!',Pred(i),i));
 except
  Result:= 'default';
 end;
if IncludeExtension then
  Result:= Result+'.ini';
end; {~presettoname}


{10/09/2016 try..except}
function TAnalyseForm.NameToPreset(AName           :String;
                                   IncludeExtension:Boolean=False): String;

var Ext: String;
    i  : Integer;
begin
try
  if Length(AName)>0 then
    begin
    Ext   := ExtractFileExt(AName);
    i     := Length(AName)-Length(Ext);
    Result:= Copy(AName,1,i)+ifthen(AName[i]='!','','!');
    end
  else
    Result:= '';
 except
  Result := 'default!';
 end;
if IncludeExtension then
  Result:= Result+'.ini';
end; {~nametopreset}


{04/11/2016 rebuild}
{16/07/2020 handle empty lists}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{07/06/2021 check if ModalityFilmList is available}
procedure TAnalyseForm.FillCheckListCombo;
var i  : Integer;
    m,f: String;
begin
with Engines[UsedEngine] do
  if assigned(ModalityFilmList) then
    begin
    m:= ModalityFilmList.GetModalityList;
    f:= ModalityFilmList.GetFilmTypeList;
    end
  else
    begin
    m:= '';
    f:= '';
    end;
i:= Length(UseDoseConvTable);
while i>0 do
  begin
  Dec(i);
  UseDoseConvTable[i].DCModalityBox.Items.CommaText:= ifthen(Length(m)>0,m,'-');
  UseDoseConvTable[i].DCFilmTypeBox.Items.CommaText:= ifthen(Length(f)>0,f,'-');;
  end;
end; {~fillchecklistcombo}


{28/07/2015 width of plotdates calculated}
{31/07/2015 cx column 0: 125-> 120, w 35->36}
{12/01/2016 relative to constraints.minwidth, wider setup}
{22/01/2018 CxUsedRowMax}
{28/05/2018 width of plotvalues set to p}
{28/09/2018 extended width of second data column}
{25/10/2018 plotvalues.width, MeasNormAdjustEdit.width}
{20/04/2020 small adjustments for lazarus}
{12/07/2020 reviewed resultpanel.height and form height}
{16/07/2020 positionlabel}
{29/08/2020 LabelPlacingActive, changed form.height rule}
{29/07/2021 sizes and positions not derived from defaultfontsize(=8) anymore}
procedure TAnalyseForm.PlaceResultsLabels(ASize:ShortInt=0);
var k                     : PlotItems;
    NumLines,h,l,w,i,j,p,q: Integer;

  procedure SetCxResults(ALine                         :CxLine;
                         TxtPos,TxtWidth,Gap,ValWidth,Y:Integer);
  var j: Integer;
  begin
  for j:= 0 to CxMaxCol do
    begin
    with ALine[j,CxTitle] do
      begin
      Font.Size:= ASize;
      Left     := TxtPos;
      Top      := Y;
      Width    := TxtWidth;
      Height   := h;
      Visible  := True;
      end;
    with ALine[j,CxValue] do
      begin
      Font.Size:= ASize;
      Left     := TxtPos+TxtWidth+Gap;
      Top      := Y;
      Width    := ValWidth;
      Height   := h;
      Visible  := True;
      TxtPos   := Left+Width+2;
      end;
   end;
  end;

begin
if CxUsedLineMax>0 then                                                         //depends on early initialisation in formcreate
  begin
  LabelPlacingActive := True;                                                   //avoid looping through formresize when height is changed
  ASize              := Max(7,Min(ifthen(ASize=0,Round(Width*8/Constraints.MinWidth),ASize),20));
  Font.Size          := ASize;
  h                  := Round(1.35*ASize*Font.PixelsPerInch/72);                //height of elements
  l                  := h+1;                                                    //line spacing
  w                  := Round(4.5*ASize);                                       //base data width
  NumLines           := Succ(Max(Ord(High(PlotItems)),PanelElements.RowMax));   //derive number text lines needed in resultspanel
  i                  := ResultsPanel.Height;
  ResultsPanel.Height:= Round(NumLines*(l+0.4));                                //here the resultspanel height is set
  SetMessagebar(Format('Font size= %d, Line height=%d, #lines=%d, panel height=%d',[ASize,h,NumLines,ResultsPanel.Height]),3);
  Height             := Min(Height+ResultsPanel.Height-i,Screen.Height);        //set height of form
  Top                := Min(Top,Screen.Height-Height);
  p                  := Round(1.7*w);                                           //width value column
  q                  := Round(15.6*ASize);                                      //width label column
  for i:= 0 to CxUsedLineMax do
    SetCxResults(CxResults[i],1,q,2,p,4+i*l);
  with CxResults[0][CxMaxCol,CxValue] do
    j:= Left+Width+25;
  p:= Round(9.4*ASize);
  w:= Round(1.3*w);
  for k:= Low(PlotItems) to High(PlotItems) do
    begin
    with PlotLabels[k] do
      begin
      Font.Size:= ASize;
      Left     := j;
      Top      := 4+Ord(k)*l;
      Width    := p;
      Height   := h;
      end;
    with PlotDates[k] do
      begin
      Font.Size:= ASize;
      Left     := PlotLabels[k].Left+PlotLabels[k].Width+8;
      Top      := PlotLabels[k].Top;
      Width    := Parent.Width-w-Left;
      Height   := h;
      end;
    with PlotValues[k] do
      begin
      Font.Size:= ASize;
      Top      := PlotLabels[k].Top;
      Width    := w;
      Left     := Parent.Width-Width-(ASize div 2)-2;
      Height   := h;
      end;
    end;
  PositionLabel.Left:= j;
  PositionLabel.Width:= p;
  with MeasNormAdjustEdit do
    begin
    Width := Round(1.3*w);
    Left  := Parent.Width-Width-1;
    Height:= h+4;
    Top   := 1;
    end;
  with PositionLabel do
    begin
    Font.Size := ASize;
    Top       := CxResults[CxUsedLineMax  ][0,CxTitle].Top;
    Left      := PlotLabels[Low(PlotItems)]           .Left;
    Width     := PlotLabels[Low(PlotItems)]           .Width;
    Height    := h;
    Font.Color:= clBlack;
    end;
  with PositionValue do
    begin
    Font.Size:= ASize;
    Top      := PositionLabel             .Top;
    Left     := PlotValues[Low(PlotItems)].Left;
    Width    := PlotValues[Low(PlotItems)].Width;
    Height   := h;
    end;
  Screen.MenuFont.Size:= ASize;
  for i:= DefChartAxL to DefChartAxB do
    DataPlot.AxisList[i].Marks.LabelFont.Size:= ASize;
  DataPlot.BottomAxis.Title.LabelFont.Size:= ASize+2;
  LabelPlacingActive                      := False;
  end;
end; {~placeresultslabels}


{31/05/2015 intermediate procedure which can be called from any thread}
{31/07/2015 use twLocked}
{20/06/2017 replaced twDataRange[ptFirst]/Last with twScanRange[ptFirst]/Last to avoid non-useful data}
{01/02/2018 ViewMillimetersItem}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{12/04/2022 analrange}
procedure TAnalyseForm.ThreadSaveFillPlot(ASeries  :PlotItems;
                                          ASource  :twcDataSource;
                                          AScaling :twcFloatType;
                                          AnalRange:Boolean=False);
var i: Integer;
    x: twcFloatType;
    r: twcRange;
begin
with Engines[UsedEngine],wSource[ASource] do
  begin
  if AnalRange then r:= twAnalysisRange
  else              r:= twScanRange;
  twLocked:= True;
  x       := GetDisplayedPositionScale;                                         //multiplier for mm
  for i:= r[ptFirst] to r[ptLast] do
   PlotSeries[ASeries].AddXY(x*twPos_cm[i],AScaling*twData[i]);
  twLocked:= False;
  end;
end; {~threadsavefillplot}



{26/09/2016 avoid using the original filename}
{10/09/2016 try..except}
{16/09/2020 with historylist all data are needed}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{30/09/2020 option ReloadReference added}
{02/10/2020 do not wait for updatesettings}
procedure TAnalyseForm.Engine2Editor(ASource        :twcDataSource=dsMeasured;
                                     ReloadReference:Boolean      =False);
begin
try
  EditorFileName:= Engines[UsedEngine].FileName;
  if Engines[UsedEngine].FileFormat=twcWellhoferRfb then
    Engines[UsedEngine].WriteData(EditorFileName,RawDataEditor.Lines,ASource)
  else
    begin
    with Engines[UsedEngine] do
      if Parser.LineCount>0 then
        begin
        RawDataEditor.Lines.AddStrings(Parser.Strings);
        FileOpenDialog.FileName:= Parser.Filename;
        end;
    end;
  with Engines[UsedEngine] do
    if (ReloadReference or (ASource<>dsMeasured)) and RefAutoLoadItem.Checked and (not Freeze) and (ReferenceValid or ProcessSetTempRefItem.Checked) then
      begin
      wCheckRefCurveString:= ProcessCheckTempTypeItem.Checked;
      wCheckRefIgnoreLinac:= ProcessIgnoreTUnameItem .Checked and wCheckRefCurveString and ProcessSetTempRefItem.Checked;
      LoadReference;
      end;
  RawDataEditor.Modified:= False;
  OnDataRead(Self);
  Engines[UsedEngine].IsFile:= True;
 except
  Engines[UsedEngine].IsFile:= False;
 end;
end; {wellhofer2editor}


{$push}{$warn 5092 off}
function TAnalyseForm.SetFileType(AExtension:String): twcFileType;
var i: integer;
begin
AExtension:= Lowercase(AExtension);
i         := Pos('.',AExtension);
if i>0 then
  AExtension:= copy(AExtension,Succ(i),Length(AExtension)-i);
if         AExtension='wtx' then Result:= twcWTX
else if    AExtension='wda' then Result:= twcWDA
 else if   AExtension='asc' then Result:= twcRFA_ascii
   else if AExtension='mcc' then Result:= twcMccProfile
     else                        Result:= twcWellhoferAscii_v6;
end;
{$pop}


(*
26/12/2017
Here a complete string of evaluation types is processed and the results are inserted in the output text:
'c,w,-b,+b,-p,+p,-i,+i,-I,+I,-e,+e'
will result in
'c=20.199,w=418.672,-b=-189.137,+b=229.535,-p=7.451,+p=7.457,-i=-188.781,+i=229.089,-I=-189.193,+I=229.533,-e=-188.475,+e=229.348'
*)
{27/12/2017 make use of MeasFiltered curve}
{20/11/2018 increased decimals to 3}
{19/07/2019 implement Xsource selectors}
{05/12/2019 repair of +/- side selector}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{06/10/2020 fundamentals alternative}
function TAnalyseForm.EvaluateResultText(ACommaText:String;
                                         Decimals  :Integer=EvalDecimals): String;
var tmpParser: toTNumParser;
    Stg      : String;
    iRec     : array[0..2] of ResultsInfoRecord;
    sn       : array[0..2] of twcSides;
    b        : Boolean;
    cOp      : Char;
    s        : twcSides;
    f        : twcFloatType;
    n        : Byte;

    function EvaluateP(n:Byte): Boolean;  {all supported items for specialmode}
    var j       : Integer;
        VirtualX: Boolean;
    begin
    with tmpParser do
      begin
      Result:= Length(CurrentLine)>CurrentLinePos;
      if Result then
        begin
        VirtualX:= CurrentLine[Length(CurrentLine)] in csSign; {assume side selection}
        if VirtualX then
          CurrentLine:= CurrentLine+'1';
        iRec[n].Xerrorval  := 0;
        iRec[n].Xsign      := 0;
        iRec[n].Iparse     := 1;
        iRec[n].Ymultiplier:= 1;
        if VirtualX then
          iRec[n].X        := 0
        else
          iRec[n].X        := NextFloat;    {sets also ConversionFirstPos}
        cOp                := CurrentLine[1];
        if ConversionResult then
          j:= ConversionFirstPos
        else
          j:= Succ(CurrentLinePos);
        if ((Length(Currentline)>1) and (cOp in csSign) and (not (CurrentLine[2] in csNumeric))) then
          begin
          Inc(iRec[n].Iparse);
          iRec[n].Xsign:= ifthen(cOp='-',-1,1);
          end;
        try
          if j-iRec[n].Iparse>0 then
            iRec[n].Xtype:= CurrentLine[iRec[n].Iparse]
          else
            iRec[n].Xtype:= EmptyXtype;
          if Length(CurrentLine)>0 then
            begin
            iRec[n].Xchar:= CurrentLine[Min(2,Length(CurrentLine))];
            if (Pos(iRec[n].Xchar,DefXsourceSelectors)=0) or (Length(CurrentLine)=1) then
              iRec[n].Xchar:= 'F';
              case iRec[n].Xchar of
                'M': iRec[n].Xsource:= dsMeasured;
                'C': iRec[n].Xsource:= dsCalculated;
                'R': iRec[n].Xsource:= dsReference;
                'B': iRec[n].Xsource:= dsBuffer;
              else   iRec[n].Xsource:= dsMeasFiltered;
             end; {case}
            end;
         except
          iRec[n].Xtype:= EmptyXtype;
         end; {try}
        Result:= (iRec[n].Xtype in EvaluationXtypes);
        end; {if result}
      end; {with}
    if Result then
      begin
      iRec[n].ConvStg:= tmpParser.ConversionString;
      if (iRec[n].Xtype='P') and (iRec[n].X=20) and (tmpParser.RemainderOfLine='/10') then
        begin
        tmpParser.CurrentLine:= '';
        iRec[n].ConvStg      := '20/10';
        end;
      if (tmpParser.ConversionResult) and (iRec[n].Xsign=0) then
        iRec[n].Xsign:= ifthen(tmpParser.ConversionString[1] in csSign,Sign(iRec[n].X),0);
      EvaluateInfoRecord(iRec[n]);
      end;
    end; {getp}

    function Operation(p,q: twcFloatType;
                       Op : Char         ): twcFloatType;
    begin
    try
      case Op of
        '-': Result:= p-q;
        '*': Result:= p*q;
        '+': Result:= p+q;
        '&': Result:= (p+q)/2;
        '/': Result:= p/q
       else  Result:= 0;
       end; {case}
      except
       Result:= 0;
      end; {try}
    end;

    function GetXtypeString(ARec:ResultsInfoRecord): String;
    begin
    with ARec do
      begin
      Result:= ifthen(Xtype in XtypeFilter,'',Xtype);
      if (Sidedness) and (not (Engines[UsedEngine].ScanType in twcVertScans)) then
        begin
        if      XSign>0 then Result:= '+'+Result
        else if XSign<0 then Result:= '-'+Result;
        end;
      end;
    end;

    function GetParameterValue(ARec:ResultsInfoRecord): String;
    begin
    Result:= ARec.ConvStg;
    if (ARec.Sidedness) and (not (Engines[UsedEngine].ScanType in twcVertScans)) then
      Result:= Result.Trim(['-','+']);                                          //TStringHelper function
    end;

begin
tmpParser:= toTNumParser.Create(ACommaText,True); {second parameter needed here}
with tmpParser do
  begin
  SetDecPointChars(['.']);
  b  := True;
  Stg:= '';
  while b do
    begin
    b:= NextLine and EvaluateP(0);
    if b then
      begin
      Stg:= Stg+','+ifthen(iRec[0].Xchar='F','',iRec[0].Xchar)+GetXtypeString(iRec[0])+Getparametervalue(iRec[0]);
      if EvaluateP(1) then
        begin {operation on combined results}
        cOp:= CurrentLine[Pred(iRec[1].Iparse)]; {<constant|variable><cOp><constant|variable>: constant includes sign}
        Stg:= Stg+ifthen(iRec[1].Xchar='F','',iRec[1].Xchar)+ifthen(iRec[1].Xtype=EmptyXtype,'',cOp)+GetXtypeString(iRec[1])+Getparametervalue(iRec[1]);
        for s:= twcLeft to twcRight do
          begin
          for n:= 0 to 1 do with iRec[n] do
            if Xsign=0      then sn[n]:= s
            else if Xsign<0 then sn[n]:= twcLeft
              else               sn[n]:= twcRight;
          iRec[2].Y[s]:= Operation(iRec[0].Y[sn[0]],iRec[1].Y[sn[1]],cOp);
          end;
        iRec[0].Y:= iRec[2].Y;
        end; {getp(1)}
      with iRec[0] do
        begin   {decision to use left,right or average is taken here}
        if (X<0) or (Xsign=-1) then s:= twcLeft  {explicite side selector}
        else                        s:= twcRight;
        if (SM2_InfoType in [1..3]) and (Xsign=0) then
          f:= (Y[twcLeft]+Y[twcRight])/2
        else
          f:= Y[s];
        end;
      Stg:= Stg+Format('=%0.*f',[Decimals,f]);
      end; {b}
    end;
  try
    Free
   except
    ExceptMessage('EvaluateResultText!');
   end;
  end; {with tmpParser}
Result:= Copy(Stg,2,Length(Stg)-1);
end; {~evaluateresulttext}


{DoSpecialMode2 takes data "as is" by user's choices but assumes no availability of gamma analysis or division.}
{20/11/2015 ExtractFilename_no_ext(twCurveIDString), file naming repair}
{10/06/2016 implemented i,u,e in specialmode2}
{11/06/2016 added &-operation, implemented f,s,l}
{14/06/2016 usage of editor replaced with separate tstringlist in specialmode2}
{28/11/2017 specialmode support repaired (string was interpreted as file name); support for area ratio}
{14/12/2017 implemented "l" (elevation)}
{21/12/2017 Calculated contains data of main stream
            Datapoints can be annotated for source selection
            MeasFiltered is default}
{10/07/2018 Spar[5] holds extension}
{12/07/2018 Fpar[5] selects datasource}
{30/11/2018 Spar[3] for DefaultSSD_cm, Spar[4] for pdd, Spar[5] for directory, Spar[6]/Fpar[6] for extension/source number}
{20/08/2019 use Spar[6] and Fpar[6] for file-export}
{27/11/2019 s5pathok introduced to prevent memory leaks}
{06/12/2019 Fpar[5] as key for putting result on clipboard}
{17/12/2019 config error messaging; clipboard output not dependant on file/directory settings}
{03/01/2020 Fpar[5] as key for export; Fpar[3] for putting result on clipboard}
{07/01/2020 more messaging}
{22/05/2020 added wMultiScanNr}
{09/07/2020 S5IsDir logics improved}
{10/08/2020 reference information added}
{11/09/2020 wedge detection by standard procedures only}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.DoSpecialMode2;
const _ParDecimals =1;
      _ParClipBoard=3;
      _ParExport   =5;
      _ParCurve    =6;
var Fs,Ds,Ps,Rs,OutStg: String;
    S5IsDir,S5PathOk  : Boolean;
    OutFile           : TStringList;
    Source            : twcDataSource; //0=dsMeasured,dsReference,dsMeasFiltered,dsRefFiltered,dsCalculated,dsBuffer
    Xscaling          : Integer;

    procedure CheckParameter(AValue  :Integer;
                             AMessage:String);
    begin
    if SpecialMode[2].Fpar[AValue]=0 then
      begin
      SetMessageBar(Format('%s needs "SpecialMode[2]Float[%d]=1" in configuration file.',[AMessage,AValue]));
      SpecialMode[2].Fpar[AValue]:= -1;
      end;
    end;

begin
with Engines[UsedEngine],wSource[dsCalculated],SpecialMode[2] do
  begin
  Ds      := StringReplace(Format('date=%4d%2d%2d,time=%2d:%2d',[YearOf(twMeasDateTime),MonthOf(twMeasDateTime),DayOf(twMeasDateTime),HourOf(twMeasDateTime),MinuteOf(twMeasDateTime)]),' ','0',[rfReplaceAll]);
  Ps      := '';
  S5PathOk:= True;
  Xscaling:= ifthen(ViewMillimetersItem.Checked,10,1);
  if (Length(Spar[_ParExport])>0) then
    begin
    Fs     := Spar[_ParExport];
    S5IsDir:= DirectoryExists(Fs);
    if S5IsDir then
      Ps:= AppendPathDelim(Fs)
    else
      begin
      Ps:= ExtractFilePath(Fs);
      if DirectoryExists(Ps) then
        Ps:= AppendPathDelim(Fs)
      else
        begin
        S5PathOk:= False;
        SetMessageBar('SpecialMode2: path not found for '+Fs);
        end;
      end;
    end
  else
    S5IsDir:= False;
  if (Spar[_ParCurve]<>'') and (Fpar[_ParExport]>0) and S5pathOk then
    begin
    Source:= twcDataSource(EnsureRange(Round(Fpar[_ParCurve]),Ord(dsMeasured),Ord(dsBuffer)));
    if not wSource[Source].twValid then
      Source:= dsMeasured;
    WriteData(Format('%s%s.txt',[Ps,ExtractFilename_no_ext(twCurveIDString)]),SetFileType(Spar[_ParCurve]),Source);
    end
  else
    CheckParameter(_ParExport,'File export');
  if (SM2_Infotype=1) and (twSetFieldType=fcWedge) then                         //infostring used for specialmode2  : 1=profile, 2=wedge, 3=DefaultSSD_cm, 4=pdd
    SM2_Infotype:= 2;
  if (ScanType in twcHoriScans) and (not SigMoidFitAvailable) then
    SigmoidPenumbraFit;
  Rs:= EvaluateResultText(Spar[SM2_Infotype],ifthen(Fpar[_ParDecimals]>0,Round(Fpar[_ParDecimals]),EvalDecimals));  {--------here the work is done}
  if ReferenceValid then
    Ps:= wSource[dsReference].twFileName+' ('+wSource[dsReference].twMeasTime+')'
  else
    Ps:= '-';
  if Length(Rs)=0 then
    Rs:= Format('Specialmode[2]String[%d] not configured',[SM2_Infotype]);
  with twBeamInfo do
    OutStg:= Format('%s,analysis-time=%s,%s,file=%s%s,ref=%s,linac=%s,modality=%s,energy=%d,wedge=%d,scan=%s:%s->%s,scale=%sm,depth=%s,field=%0.0fx%0.0f,%s',
                    [DefAppName,DateTimeToStr(Now),Ds,FileName,ifthen(wMultiScanMax>1,',#'+Num2Stg(wMultiScanNr),''),Ps,
                     Linac,twBModality,Round(Energy),twBWedge,
                     LowerCase(wCurveInfo.twDesTypeString[1]),
                     ifthen(SwapAxis,ScanRightSide,ScanLeftSide),ifthen(SwapAxis,ScanLeftSide,ScanRightSide),
                     ifthen(ViewMillimetersItem.Checked,'m','c'),
                     ifthen(ScanType in twcVertScans,'-',Format('%0.1f',[Xscaling*twVector_ICD_cm[ptFirst].m[Beam]])),
                     Xscaling*FieldGT_cm,Xscaling*FieldAB_cm,Rs]);
  SetMessageBar(OutStg);
  if Fpar[_ParClipBoard]>0 then
    begin
    ClipBoardLock   := True;
    ClipBoard.AsText:= OutStg;
    ClipBoardLock   := False;
    end
  else
    CheckParameter(_ParClipBoard,'Clipboard pasting');
  if (not S5IsDir) and (Fpar[_ParExport]>0) then
    begin
    Fs     := Spar[_ParExport];
    OutFile:= TStringList.Create;
    try
      if FileExists(Fs) then
        OutFile.LoadFromFile(Fs);
     finally
     end;
    OutFile.Add(OutStg);
    if PDDfitCheckBox.Enabled and PDDfitCheckBox.Checked and (ScanType in twcVertScans) then
      begin
      OutFile.Add(FitReport);
      OutFile.Add(FitReport(False,dsMeasured,NM_Extrapolation));
      end;
    if S5PathOk then
      OutFile.SaveToFile(Fs);
    try
      OutFile.Free
     except
      ExceptMessage('DoSpecialMode2!');
     end;
    end; {not isdir}
  with RawDataEditor do
    begin
    SelStart := 0;
    SelLength:= 0;
    end;
  end;
end; {~dospecialmode2}


(*
****BistroMath core function****
This function is called through several routes:
-FormCreate, specialmode1
-FileOpenClick (File menu -> File open)
-ReadDroppedFiles (AnalyseForm.OnDropFiles event)
It relies completely on the underlying TWellhoferdata object.
The fact that TWellhoferData itself handles all currently known binary types can be exploited here:
-first it checks on possibly binary data; the data are read into a memorystream.
-when binary: read the data and export them in text format to the editor but don not rely on these data
-otherwise: push to editor and go from there through the parsing structures
*)
{12/08/2015 CenterProfiles option removed in Wellhofer.AdvReadData, see UpdateSettings}
{13/08/2015 UpdateSettings added here}
{12/02/2016 preloadstream, preloadtransfer}
{21/07/2017 if Wellhofer.Ready}
{23/01/2018 setmessagebar}
{24/01/2018 CheckWellhoferReady}
{15/10/2018 MeasNormAdjustEdit.Value:= 100}
{04/05/2020 RawDataEditor.Clear}
{03/07/2020 removed SavedIgnoreState}
{14/09/2020 addengine}
{18/09/2020 unfreeze}
{13/10/2020 set wMultiScanNr to 1}
{19/10/2020 BinStream was held in memory but ascii data were read from the file again; now there is a direct transfer}
{22/20/2020 SourceAxisSync more early}
{17/11/2020 support automated continuous reading of multiple data sets in single text data set file format}
{30/03/2021 resample and coordinateordering parameter dropped from advstreamdata}
function TAnalyseForm.DataFileOpen(AFile         :String;
                                   ResetMultiScan:Boolean=True): Boolean;
var
  {$IFNDEF PRELOAD}
   S              : TMemoryStream;
  {$ENDIF}
  LocalDataTopLine: Integer;                                                    //support for automated continous reading in single file format type
begin
Result:= FileExists(AFile) and CheckWellhoferReady;
if Result then
  begin
  LocalDataTopLine:= 0;                                                         //set local file pointer at top of file
  repeat
    UsedEngine     := AddEngine;                                                //selecting another engine sets UsedDataTopLine
    UsedDataTopLine:= LocalDataTopLine;                                         //set new UsedDataTopLine from local value
    with Engines[UsedEngine] do
      begin
      SourceAxisSync;
      ClearScreen(Self);
      UpdateSettings(Self);
      RawDataEditor.Clear;
      RawDataEditor.Modified  := False;
      DataFromEditor          := False;
      MeasNormAdjustEdit.Value:= 100;
      PrevKey                 := #0;
      FileTime                := FileDateToDateTime(FileAge(AFile));
      wMultiScanNr            := 1;
      SetMessageBar('file: '+AFile);
      if IsBinary(AFile) then                                                   //stream already loaded here
        begin               //*bin**bin**bin**bin**bin**bin**bin**bin**bin*--binary--*bin**bin**bin**bin**bin**bin**bin*
        ClipBoardLock   := True;
        DetectedFileType:= BinStreamType;
        if ResetMultiScan then
          ResetMultiScanCounters;
        Result:= AdvStreamData(nil,
                               0,                                               //multiple data sets should be structured within file, setting scancount
                               True,                                            //unfreeze
                               DetectedFileType,
                               AFile);
        if Result then
          begin
          FileOpenDialog.Filename:= AFile;
          Engine2Editor;
          end;
        ClipBoardLock:= False;
        end
      else
        begin                //*asci**asci**asci**asci**asci**asci**asci*--text data--*asci**asci**asci**asci**asci**asci*
        DetectedFileType  := LastDetectedFileType;
        FileName          := AFile;
       {$IFDEF PRELOAD}
        Parser.Assign(AFile,BinStream);                                         //direct transfer from binary stream to parser parser when possible; clears binstream
        Parser.PreLoaded:= True;
        PreloadStream.Clear;
        Parser.Strings.SaveToStream(PreLoadStream);                             //copy from parser to stringstream
        PreloadTransfer(Self);
       {$ELSE} //no preload
        S:= TMemoryStream.Create;
        if BinStream.Size>0 then
          begin
          S.LoadFromStream(BinStream);
          BinStream.Size:= 0;
          end
        else
          S.LoadFromFile(AFile);
        RawDataEditor.Lines.LoadFromStream(S);
        S.Free;
       {$ENDIF PRELOAD}
        ReadEditor(Self);                                                       //here the text data are send to engine[usedengine] and analysed
        Result:= Engines[UsedEngine].IsValid;
        IsFile:= True;
        end;
      end; {with}
    if FileMultipleInputItem.Checked         and FileMultipleInputItem.Enabled and
       (Engines[UsedEngine].wMultiScanMax=1) and Engines[UsedEngine].FindMoreData then
      begin                                                                     //there are more data in file of single scan format
      LocalDataTopLine:= Engines[UsedEngine].Parser.CurrentLineNumber;          //update file pointer
      WaitLoop(100);                                                            //some delay for multiple data sets in single date set text format file
      end
    else
      LocalDataTopLine:= 0;                                                     //not willing to read or no more data available (in file of single scan format)
  until LocalDataTopLine=0;
  end;
if Result then
  SetCaption(AFile);
end; {~datafileopen}


{05/11/2018}
function TAnalyseForm.GetPositionUnitsStg: String;
begin
Result:= ifthen(ViewMillimetersItem.Checked,'m','c')+'m';
end; {~getpositionunitsstg}


function TAnalyseForm.GetDisplayedPositionScale(cm:twcFloatType=1): twcFloatType;
begin
Result:= cm*ifthen(ViewMillimetersItem.Checked,10,1);                           //if mm wanted then multiply with 10
end; {~getdisplayedpositionscale}


{17/03/2020}
procedure TAnalyseForm.DataEditorAddLine(ALine:String);
begin
RawDataEditor.Lines.Add(ALine);
end; {~dataeditoraddline}


{17/03/2020 from editor.pas}
{04/05/2020 last-moment filling}
{19/05/2020 multiple memo's}
procedure TAnalyseForm.MemoSaveToFile(AMemo    :TMemo;
                                      AFileName:String='');

begin
{$IFDEF PRELOAD}
PreloadTransfer(Self);
{$ENDIF}
if (AMemo=RawDataEditor) then
  begin
  if Length(AFileName)>0 then EditorFileName:= AFileName
  else                        AFileName     := EditorFileName;
  end;
if Length(AFilename)=0 then
  with FileSaveDialog do
    begin
    FileName:= AFileName;                                                       //save measurement file settings
    if Execute then
      AFileName:= FileName;
    end;
try
  if (AMemo<>nil) and (Length(AFileName)>0) then
    AMemo.Lines.SaveToFile(AFileName);
  except
 end;
end; {~memosavetofile}


(*
SourceAxisSync updates axis settings for engine, based on information from GUI;
When changed from a previous state, the reference will become invalid.
*)
{10/05/2016 make reference invalid when axissetup is changed, otherwise do nothing}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{19/03/2021 keep reforg valid}
procedure TAnalyseForm.SourceAxisSync;

  function Transfer(t     :twcMeasAxis;
                    Invert:Boolean    ): Boolean;
  var i: ShortInt;
  begin
  with Engines[UsedEngine] do
    begin
    i               := wUserAxisSign[t];
    wUserAxisSign[t]:= ifthen(Invert,-1,1);
    Result          := i=wUserAxisSign[t];
    end;
  end;

begin
if not (Transfer(Inplane,MeasInvertGTitem.Checked) and Transfer(Crossplane,MeasInvertABitem.Checked) and Transfer(Beam,MeasInvertUDitem.Checked)) then
  Engines[UsedEngine].wSource[dsReference].twValid:= false;
end; {~sourceaxissync}


{08/08/2019 port from Delphi, now autoscrollbar available}
{19/05/2020 limit on number of lines in logtabmemo}
{06/06/2020 ALogLevel>=LoglevelEdit.Value}
procedure TAnalyseForm.SetMessageBar(Stg      : String;
                                     ALogLevel: Integer=1);

begin
if ALogLevel<=LoglevelEdit.Value then
  begin
  StatusBar.Panels[0].Text:= Stg;
  Application.ProcessMessages;
  with LogTabMemo do
    try
      Lines.BeginUpdate;
     {$IFDEF MEMO_ALLCLEAR}
      if Lines.Count>Tag then
        Lines.Clear;
     {$ELSE}
      while Lines.Count>Max(0,Tag-50) do
        Lines.Delete(0);
     {$ENDIF}
     finally
      Lines.Add(Stg);
      Lines.EndUpdate;
     end;
  end;
end; {~setmessagebar}


{$push}{$warn 5092 off}
(* PublishResults         ****BistroMath core function****
PublishResults evaluates and displays all panel rules.
This function expects at least measured data and the filterd version.
The local procedure RUNPANELELEMENTS generates values for all panel rules
(if applicable). For each panel element in one large loop the set conditions in
a panel element are compared with the choices made in the GUI. If there is a
match the bare values for that panel element are fetched by EvaluateInfoRecord.
Then in this loop the bare value are enriched with titles, colors annotations
and scaling.
EvaluateInfoRecord is also called by SpecialMode2 to present the very same
results.
*)
{20/07/2015 plotdates[pmeasured] informs on composite results}
{31/07/2015 getpos reviewed on basis of reviewed wellhofer.fastscan}
{12/08/2015 MeasMaxAsCenterItem.Checked -> 'm'}
{11/12/2015 static LeffSeries and ReffSeries replaced with InFieldIndicators}
{15/12/2015 FFFslope in symmetry and flatness}
{20/12/2015 twTopModel}
{10/02/2016 labels repaired for electron pdd, shift in labels}
{12/02/2016 'cm-1' changed to '%/cm'}
{07/07/2016 annotation for center of DefaultSSD_cm beam (C2Results[0])}
{29/07/2016 introduction of TopM for DefaultSSD_cm}
{10/09/2016 try..except}
{06/12/2016 synthetic profiles, topsource for top position}
{11/01/2017 toepassing EdgeDetectionError_mm verplaatst van GetPos naar Wellhofer.SetLeftRight}
{17/01/2017 DefaultSSD_cm penumbra is sigmoid_slope}
{05/05/2017 to avoid invalid results added: if twSigmoidFitData[side].twFitValid}
{08/06/2017 refinement of non-zere shift detection: b:= Abs(Rshift)>0.0001}
{16/06/2017 DefAnnotNormDif norm position rule is only for profiles}
{23/11/2017 use twInFieldPosCm for InFieldIndicators}
{24/11/2017 introduction of Area ratio}
{27/11/2017 label annotation of Area ratio}
{21/12/2017 SpecialMode2 moved to separate procedure}
{27/12/2017 make use of MeasFiltered curve}
{15/01/2018 completely rewritten for configured cxblocks}
{17/01/2018 added energy selector}
{18/01/2018 changed edge annotation, PCRenergy round to units of DefEnergyUncertainty = 0.01; MeV}
{19/01/2018 if PCRdefaultsource OR (not Wellhofer.wSource[PCRxrecord.XSource].twValid) then...}
{19/01/2018 shift annotation for vertical scans special treatment for 'n' and 'N'}
{24/01/2018 BadPenumbraText only for conventional profiles}
{01/02/2018 ViewMillimetersItem, Ymultiplier, Y_mm}
{14/02/2018 negative positions in labels were displayed as "--position"}
{12/04/2018 use only datasourcse with twvalid state}
{28/05/2018 apply twAbsNormConfig}
{28/09/2018 maximized decimals to 1 for DefaultSSD_cm penumbra}
{15/10/2018 CursorPosCm:= ifthen((ScanType in twcVertScans) AND (not ViewMeasNormAdjustMode.Checked),twRelNormPosCm,twAbsNormPosCm)}
{27/10/2018 ViewMeasNormAdjustMode only in advancedmode}
{17/12/2019 SimpleModeItem}
{13/01/2020 AddAnnotation(pa_userlevel,MeasUserDoseItem.Checked or (PCRxrecord.Xtype='u'),....}
{27/04/2020 shift annototion of reference with explicit sign}
{05/06/2020 annotations: apply also Ylevel<>d50}
{17/06/2020 implementation of PCRxrecord.Xedge}
{20/06/2020 pa_userlevel adjusted to MeasUserDoseItem.Checked or (PCRxrecord.Xtype='u') or (PCRxrecord.Ylevel=dUser)}
{30/06/2020 penumbra labeling changed ('p','q')}
{04/07/2020 AddAnnotation(pa_edge,... or (PCRxrecord.Xedge<>dUseBorder),...}
{08/07/2020 add GetNormalisedSigmoidlevel to 'i'}
{14/07/2020 improved display for 'r','R' (Profile Evaluation Points)}
{15/07/2020 ViewNoDefaultAnnotationItem,AppliedEdgeRefNorm}
{20/07/2020 field types}
{21/07/2020 fcWedge}
{03/07/2020 added conditions PCRsmall and PCRnormal; PCRfieldSize}
{04/09/2020 additional marking for dynamic penumbra width ('p')}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{06/10/2020 fundamentals alternative}
{09/02/2021 added conditions PCRMRlinac and PCRelectron}
{23/02/2021 introduced PCRfffShape, renamed PCRfff to PCRfffType}
{02/03/2021 FFFfeatures}
{01/04/2021 RunPanelElements: fieldsize needs its own local value (w)}
procedure TAnalyseForm.PublishResults;
var s                                 : String;
    ZeroShift,L,NotNormCenter         : Boolean;
    Mshift,Rshift                     : twcFloatType;
    ScalingColor,PDDColor,DefaultColor: TColor;
    DataSource                        : twcDataSource;

  function TimeString(AString :String;
                      Mirrored:Boolean=False;
                      SymCorr :Boolean=False;
                      Optional:String =''): String;
  var s: String;
  begin
  s:= '';
  if Mirrored then
    s:= DefMirrorText;
  if SymCorr then
    s:= s+ifthen(s='','',', ')+DefSymCorrText;
  Result:= AString+#32+ifthen(s='','','(')+s+ifthen(Optional='','','('+Optional+')')+ifthen(s='','',')');
  end; {timestring}

  procedure SetLabel(ALabel     :TLabel;
                     ACaption   :String;
                     Annotations:String='');  overload;
  begin
  Annotations:= Annotations.Trim(chNull);                                       //TStringHelper function
  if Length(Annotations)>0 then
    Annotations:= '('+Annotations+')';
  ALabel.Caption:= ACaption+Annotations+':';
  ALabel.Visible:= True;
  end; {setlabel}

  procedure SetLabel(ABlock     :CxBlock;
                     ACaption   :String;
                     Annotations:String='');  overload;
  begin
  SetLabel(ABlock[CxTitle],ACaption,Annotations);
  end; {setlabel}

  procedure SetCxResultVisible(ABlock   :CxBlock;
                               IsVisible:Boolean);
  var i: CxComponents;
  begin
  for i:= CxTitle to CxValue do ABlock[i].Visible:= IsVisible;
  end; {setcxresultvisible}

  procedure RunPanelElements;
  var i,j                      : Integer;
      decimals                 : Word;
      annotations,sLocal       : String;
      ccolor,cdefault          : TColor;
      cblock                   : CxBlock;
      cbeam                    : Char;
      tval,cval,w,cmulti,esteps: twcFloatType;
      cvertical,cnm_NxorAbs    : Boolean;

    function CheckCond(AFlag:Boolean;
                       ACond:SmallInt): Boolean;
    begin
    Result:= (ACond=0) or (not (AFlag xor (ACond=1)));
    end; {checkcond}

    function CheckFieldSize(SizeLimits   :PCRfieldSizeArray;
                            AFieldSize_cm:twcFloatType): Boolean;
    begin
    Result:= ((SizeLimits[0]<0) or (AFieldSize_cm>=SizeLimits[0])) and
             ((SizeLimits[1]<0) or (AFieldSize_cm<=SizeLimits[1]))
    end; {checkfieldsize}

    procedure AddAnnotation(AAnnotation:AnnotationTypes;
                            ATest      :Boolean;
                            Acolor     :TColor;
                            AAltText   :String=#0);
    begin
    if ATest then
      begin
      if AAltText=#0 then
        AAltText:= DefAnnotTypeString[Ord(AAnnotation)];
      if AAnnotation in PanelElements.FElements[i].PCRannotations then
        annotations:= annotations+AAltText;
      if AAnnotation in PanelElements.FElements[i].PCRannotations+PanelElements.FElements[i].PCRcolors then
        ccolor:= Acolor;
      end;
    end; {addannotation}

  begin
  i       := 0;
  j       := PanelElements.Count;
  cbeam   := Engines[UsedEngine].wSource[dsMeasured].twBeamInfo.twBModality;
  try
    esteps:= Round(Engines[UsedEngine].wSource[dsMeasured].twBeamInfo.twbEnergy/DefEnergyUncertainty);
    if Engines[UsedEngine].ScanType in twcVertScans then
      w:= Engines[UsedEngine].GetFieldLength
    else
      w:= Engines[UsedEngine].GetFieldWidth_cm(dsMeasured,dDerivative)/Engines[UsedEngine].wSource[dsmeasured].twSDD2SSDratio;
   except
    w:= 0;
   end;
  while i<j do
    begin
    with PanelElements.FElements[i] do
      begin
      if PCRvalid and (Pos(cbeam,PCRmodalities)>0) and
         (Engines[UsedEngine].ScanType in PCRscans) then
        begin
        if PCRdefaultsource or (not Engines[UsedEngine].wSource[PCRxrecord.XSource].twValid) then
          PCRxrecord.Xsource:= DataSource;   {Usource changed as needed}
        if CheckCond(AppliedFieldClass=fcStandard      ,PCRconditions[PCRstandard      ]) and
           CheckCond(AppliedFieldClass=fcFFF           ,PCRconditions[PCRfffType       ]) and
           CheckCond(AppliedFieldClass=fcSmall         ,PCRconditions[PCRsmall         ]) and
           CheckCond(AppliedFieldClass=fcMRlinac       ,PCRconditions[PCRMRlinac       ]) and
           CheckCond(AppliedFieldClass=fcWedge         ,PCRconditions[PCRwedge         ]) and
           CheckCond(AppliedFieldClass=fcElectron      ,PCRconditions[PCRelectron      ]) and
           CheckCond(Engines[UsedEngine].ReferenceValid,PCRconditions[PCRrefvalid      ]) and
           CheckCond(FFFfeatures                       ,PCRconditions[PCRfffShape      ]) and
           CheckCond(RefUseDivideByItem .Checked       ,PCRconditions[PCRisDivision    ]) and
           CheckCond(RefUseGammaItem    .Checked       ,PCRconditions[PCRisGamma       ]) and
           CheckCond(not SimpleModeItem .Checked       ,PCRconditions[PCRSimpleViewHide]) and
           CheckFieldSize(PCRfieldSize,w)                                                 and
           ((PCRenergySteps=0)                                     or
            InRange(esteps,ifthen(PCRenergyDif<0,0,PCRenergySteps+PCRenergyDif),ifthen(PCRenergyDif>0,MaxInt,PCRenergySteps+PCRenergyDif)))
           then
          with Engines[UsedEngine].wSource[PCRxrecord.XSource] do
            begin
            if PCRsimple then
              begin
              decimals   := PCRdecimals;
              annotations:= '';
              cdefault   := ifthen(twPosScaling<>1,clRed,clBlack);
              ccolor     := cdefault;
              cval       := EvaluateInfoRecord(PCRxrecord);
              tval       := twShift_cm;
              cblock     := CxResults[PCRrow][PCRcol];
              cvertical  := Engines[UsedEngine].ScanType in twcVertScans;
              if (PCRxrecord.Xsign<>0) and ((PCRxrecord.Xtype='i') or ((PCRxrecord.Xtype='b') and (PCRxrecord.Ylevel=dInflection))) then
                sLocal:= Num2Stg(Round(Engines[UsedEngine].GetNormalisedSigmoidLevel(cval*ifthen(PCRxrecord.Y_mm=Units_in_numerator,0.1,1),PCRxrecord.USource)))
              else
                sLocal:= '';
              cnm_NxorAbs:= (not (PCRxrecord.Xtype in ['n','N'])) or ((PCRxrecord.Xtype='N') xor twAbsNormConfig);
              AddAnnotation(pa_resampled,
                            twResampled,
                            ccolor);
              AddAnnotation(pa_synthetic,
                            SyntheticMade,
                            ccolor);
              AddAnnotation(pa_fff,
                            FFFfeatures,
                            ccolor);
              AddAnnotation(pa_ssd,
                            Engines[UsedEngine].wSource[PCRxrecord.XSource].twPosScaling<>1,
                            ccolor);
              AddAnnotation(pa_centertype,
                            twCenterPosDefUse<>dUseBorder,
                            ccolor,DefCenterAnnot[twCenterPosDefUse]);
              AddAnnotation(pa_normdif,
                            (NotNormCenter and (Engines[UsedEngine].ScanType in twcHoriScans)) or (GlobalNormAdjust_perc.Value<>100),
                            ScalingColor);
              AddAnnotation(pa_edge,
                            ((twCenterPosDefUse<>dUseBorder) or (PCRxrecord.Ylevel<>AppliedEdgeRefNorm) or (PCRxrecord.Xedge<>dUseBorder)) and
                             (not ViewNoDefaultAnnotationItem.Checked),
                            DefWedgeCol,
                            DefCenterAnnot[PCRxrecord.Xedge]+sLocal);
              AddAnnotation(pa_fitted,
                            PDDFitResultsTab.TabVisible and MeasUsePDDfitModelItem.Checked,
                            clRed);
              AddAnnotation(pa_config,
                            twAbsNormConfig,
                            ccolor);
              AddAnnotation(pa_topmodel,
                            FFFfeatures,
                            ccolor,ifthen(FFFpSubItems[CenterFFFTopModel].Checked,'T','S'));
              AddAnnotation(pa_shifted,
                            (tval<>0) and
                            ( (cvertical and cnm_NxorAbs)                    or
                              (not (cvertical or MeasMove2OriginItem.Checked)) ),
                            clRed,DefAnnotShift);
              AddAnnotation(pa_centered,
                            MeasMove2OriginItem.Checked,
                            ifthen(twAppliedEdgeLevel in [dDerivative,dInflection],DefWedgeCol,DefCenteredCol));
              AddAnnotation(pa_symmetric,
                            twSymCorrected,
                            DefSymCorrected);
              AddAnnotation(pa_userlevel,
                            MeasUserDoseItem.Checked or (PCRxrecord.Xtype='u') or (PCRxrecord.Ylevel=dUser),
                            ccolor,Num2Stg(UserBorderDose_perc.Value,0,0));
              if PCRxrecord.Xtype='p' then                                          //----add penumbra to label
                begin
                PCRunits     := '';
                PCRlabelResult:= PCRlabel[1]+ifthen(PCRxrecord.Ylevel=dTemp,'*','')+
                                 Num2Stg(Engines[UsedEngine].PenumbraHi*100,0,0)+'-'+Num2Stg(Engines[UsedEngine].PenumbraLo*100,0,0);
                end
              else if PCRxrecord.Xtype='S' then                                     //----change label to extended symmetry
                PCRlabelResult:= CleanUpCaption(ExtSymSubItems[ExtSym].Caption)
              else if (PCRxrecord.Xtype in ['D','X','x','Y','y','Z','z']) then      //----add convstg to label
                PCRlabelResult:= PCRxrecord.Xtype+ifthen(cvertical,'',ifthen(PCRxrecord.Xsign=1,'+',''))+
                                 Num2Stg(GetDisplayedPositionScale(ifthen(pa_Xactual in PCRannotations,PCRxrecord.X_actual,PCRxrecord.X)),0,ifthen(PCRxrecord.Xtype='D',0,-1))+
                                 GetPositionUnitsStg
              else if (PCRxrecord.Xtype in ['r','R']) then                          //----add convstg to label for PEP
                begin
                if pa_Xactual in PCRannotations then
                  begin
                  tval:= PCRxrecord.X_actual/twActDet2NormRatio;
                  if (tval<2) or ViewMillimetersItem.Checked then
                    begin
                    sLocal:= 'mm';
                    tval  := tval*10;
                    end
                  else
                    sLocal:= 'cm';
                  end
                else
                  begin
                  tval  := PCRxrecord.X;
                  sLocal:= '%';
                  end;
                PCRlabelResult:= PCRlabel+ifthen(PCRxrecord.Xsign=1,'+','')+Num2Stg(tval,0,-1)+sLocal;
                end
              else if (PCRxrecord.Xtype='P') and (pa_RDD in PCRannotations) then    //----change label to RDD
                 begin
                 try
                   with Engines[UsedEngine].wSource[PCRxrecord.XSource] do
                    PCRlabelResult:= ifthen({twAbsNormConfig and }(abs(twMaxValue/twAbsNormValue-1)>twcYtopQfitRelDif),'RDD','PDD');
                  except
                   PCRlabelResult:= 'PDD';
                  end;
                 PCRlabelResult:= PCRlabelResult+PCRxrecord.ConvStg;
                 end;
              SetLabel(cblock,PCRlabelResult,annotations);                          //display label and annotations
              if cval=PCRxrecord.Xerrorval then
                begin
                cblock[CxValue].Visible:= True;
                cblock[CxValue].Caption:= '-'
                end
              else
                begin
                sLocal:= PCRunits;
                cmulti:= PCRxrecord.Ymultiplier;
                if PCRxrecord.Y_mm in [Units_in_numerator,Units_in_denominator] then
                  begin
                  if ((cval>1) and (Length(sLocal)<=3)) or (PCRxrecord.Y_mm=Units_in_numerator) then
                    begin
                    decimals:= Max(NeededDecimals(cval,ifthen(Length(sLocal)>2,2,3)),decimals-1);
                    sLocal  := PCRunits.Replace('cm','mm');
                    end
                  else if PCRxrecord.Y_mm=Units_in_denominator then
                   cmulti:= 10*cmulti;
                  end;
               PublishValue(cblock,cmulti*cval,ccolor,Max(0,decimals),sLocal);
               end;
              end {conditions}
            else
             PublishValue(cblock,0,0,0,'');
            end; {with wSource}
        end;
      end; {with panelelements}
    Inc(i);
    end; {while}
  end; {runpanelelements}

{---main publishresults---}
begin {syntheticmade set in ondataread}
with Engines[UsedEngine],wCurveInfo do
  begin
  PDDFitResultsTab.TabVisible:= (ScanType in twcVertScans) and PDDfitCheckBox.Checked and wSource[dsBuffer].twValid;
  if PDDFitResultsTab.TabVisible and MeasUsePDDfitModelItem.Checked then
    DataSource:= dsBuffer   {fitted pdd}
  else if SyntheticMade and wSource[dsCalculated].twValid then
    DataSource:= dsCalculated
  else
    DataSource:= dsMeasFiltered;
  if Analyse(DataSource) then
    begin
    with wSource[DataSource] do  {see statement above for value of datasource, mostly calculated}
      begin
      Mshift       := twShift_cm;
      NotNormCenter:= wNormalisation[wSource[dsMeasured].twSetFieldType]<>NormOnCenter;
      Rshift       := wSource[dsReference].twShift_cm;
      L            := ScanType in twcHoriscans;
      CursorPos_cm := ifthen((ScanType in twcVertScans) and (not (ViewMeasNormAdjustMode.Checked and AdvancedModeItem.Checked)),twRelNormPos_cm,twAbsNormPos_cm);
      DefaultColor := ifthen(twPosScaling<>1,clRed,clBlack);
      PDDcolor     := ifthen((DataSource=dsCalculated) and (Abs(MShift)<0.0001),clBlack,clRed);
      ScalingColor := ifthen(ScanType in twcVertScans,PDDcolor,
                             ifthen(L and (NotNormCenter or (GlobalNormAdjust_perc.Value<>100)),clRed,DefaultColor));
      with wSource[dsMeasured] do
        begin
        s:= Linac+' '+TimeString(twMeasTime,twMirrored,twSymCorrected,ifthen(twDataHistoryStg<>twcDataSourceNames[dsMeasured],twDataHistoryStg,''));
        if twMayneordApplied then
          begin
          s:= StringReplace(twDataHistoryStg,twcDataSourceNames[dsMeasured],s,[rfReplaceAll]);
          SetPlotDate(pMeasured,s,False);
          end
        else SetPlotDate(pMeasured,s,twComposite);
        end;
      if MeasOD2DoseConvItem.Checked and (DoseConvTableNr>0) then
        with UseDoseConvTable[DoseConvTableNr] do
          if DCDoseBox.Checked then
            begin
            DateTimeToString(s,'  dd-mmm-yyyy  hh:nn',twMeasDateTime);
            SetPlotDate(pMeasured,DCFilmTypeBox.Text+s);
            end;
      SM2_InfoType                     := 1;
      wSource[dsMeasured].twSNR        := twSNR;
      PlotLabels[pMeasured ].Font.Color:= ifthen(MeasMove2OriginItem.Checked,DefCenteredCol,clBlack);
      if L then {horizontal scans}
        begin
        if (WedgeAngle>0) or (AppliedFieldClass=fcWedge) then
          SM2_InfoType:= 2
        else if FFFfeatures then
          SM2_InfoType:= 3
        else if ((not MeasBadPenumbraItem.Checked) and
                 (GetPenumbraWidth(dsMeasFiltered,twcLeft)+GetPenumbraWidth(dsMeasFiltered,twcRight)>2*BadPenumbraWidth_cm.Value)) then
          SetMessageBar(BadPenumbraText);
        end
      else if ScanType in twcVertScans then
        SM2_InfoType:= 4;
      {SetMessageBar(Format('Min/Max in-field: %0.1f%%/%0.1f%%   Max: %0.1f%% at %0.2f cm',
                           [100*twMinInField,100*twMaxInField,100*twMaxValue/twNormVal,twPos_cm[twMaxArr]]));}
      end; {with}
    try
      with wSource[dsReference] do
        if twValid then
          begin
          s                                := twDevice+' '+twMeasTime;
          ZeroShift                        := Abs(Rshift)<0.0001;
          PlotLabels[pReference].Font.Color:= ifthen(ZeroShift,clBlack,DefCenteredCol);
          if twSymCorrected then
            s:= TimeString(s,twMirrored,True);
            if not ZeroShift then
              s:= s+Format(' %s %s%0.*f %s',[DefAnnotShift,ifthen(Rshift>0,'+',''),
                                           ifthen(ViewMillimetersItem.Checked,1,2),
                                           GetDisplayedPositionScale(Rshift),GetPositionUnitsStg]);
          SetPlotDate(pReference,s,twComposite); {info reference}
          end;
      with wSource[dsBuffer] do
        if twValid then
          SetPlotDate(pBuffer,TimeString(ifthen(MeasMirrorToBufferItem.Checked,Linac,ExtractFileName(twCurveIDString)),
                                                twMirrored or MeasMirrorToBufferItem.Checked,twSymCorrected),twComposite);
     except
     end;
    RunPanelElements;
    end;
  end; {with wellhofer}
end; {~publishresults}
{$pop}


{02/04/2019}
{31/05/2020 preset of filename}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.FileSave(ACurve  :PlotItems;
                                Filtered:Boolean=False);
var Source: twcDataSource;
begin
case ACurve of
  pMeasured  : Source:= dsMeasured;
  pCalculated: Source:= dsCalculated;
  pReference : Source:= dsReference;
  pBuffer    : Source:= dsBuffer;
 else          Source:= dsUnrelated;
 end;
if Filtered and (Source in twcFilterSources) then
  Source:= twcCoupledFiltered[Source];
if Engines[UsedEngine].wSource[Source].twValid then
  with FileSaveDialog do
    begin
    with Engines[UsedEngine].wSource[Source] do
      FileName:= ifthen(Source in [dsMeasured,dsReference],twFilename,twCurveIDstring);
    if Execute then
      begin
      InitialDir:= ExtractFileDir(FileSaveDialog.FileName);
      DefaultExt:= ExtractFileExt(FileName);
      Engines[UsedEngine].WriteData(FileSaveDialog.FileName,SetFileType(DefaultExt),Source);
      end;
    end;
end; {~filesave}

(*
EvaluateInfoRecord implements the retrieval of data from TWellhoferData through
identifiers defined in PanelElements:
EvaluationXtypes = ['a','b','c','C','d','D','e','f','F','g','G','i','I','K','l','L','m','M','n','N','p','P','q','Q','r','R','s','S','t','T','u','U','v','V','w','x','X','y','Y','z','Z',EmptyXtype];
------
Parameters with the exclamation symbol have a left and right result. Therefore the left result is obtained as -"evaluation type" and the right results as +"evaluation type". When no sign is given the result will be the average of left and right value.
    a                Area ratio
                       wSource[Xsource].twSymAreaRatio
  ! b                Border position based on edges user choices
                       wSource[Xsource].twLevelPos[twUsedEdgeLevel].Penumbra[side].Calc;
    c                Center position as defined by user choices
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
    G|V              Gamma analysis within IFA
                       GammaAnalysis(dsMeasured,dsReference,dsCalculated,False,gGammaComplete,True,NormAdjustNumEdit.Value/100);
    g|v              Gamma analysis over complete profile
                       GammaAnalysis(dsMeasured,dsReference,dsCalculated,False,gGammaComplete,True,NormAdjustNumEdit.Value/100);
  ! i                Sigmoid edge (inflection point)
                       wSource[Xsource].twLevelPos[dSigmoid].Penumbra[side].Calc;
  ! I                Sigmoid 50%,
                       wSource[Xsource].twLevelPos[dSigmoid50].Penumbra[side].Calc;
    K                relative heigth of Top position for Top model.
                      100*wSource[Xsource].twTopModel.Ytop/wSource[Xsource].twAbsNormVal
    L                Linac error
                       100*wSource[Xsource].twSymLinacError
    l                Elevation
                       100*wSource[Xsource].twLinSlope*
                           (GetPosition(Xsource,twFlatArr[twcRight])-GetPosition(Xsource,twFlatArr[twcLeft]))/twAbsNormVal
    m                Relative maximum
                       100*wSource[Xsource].twTopModel.Ytop/max(1,wSource[Xsource].twAbsNormVal)
    M                position of maximum
                       wSource[Xsource].twMaxPosCm
    N                Norm position
                       wSource[Xsource].twAbsNormPosCm
    n                Norm value
                       wSource[Xsource].twAbsNormVal
  ! p [|1|2]         Penumbra width
                       GetPenumbraWidth(XSource,side,ApplyDynamicWidth)
                         1: ApplyDynamicWidth:= False | according to standard definitions for conventional fields
                         2: ApplyDynamicWidth:= True  | based on relative dose of inflection point
                        (nothing) or other: value according to Field Types tab
    P [10|20|20/10]  Percentage depth dose
                       wSource[Xsource].twPDD10,twPDD20,twPDD20/twPDD10
  ! q                Slope of sigmoid model (always positive in this implementation)
                       Y[side]:= wSource[Xsource].twSigmoidFitData[side].twNMReport.BestVertex[sigmoid_Slope]/Ynorm
  ! Q                Sigmoid slope in the inflection point
                       Y[side]:= Abs(wSource[Xsource].twSigmoidFitData[side].twFitResult2)/Ynorm
  ! r [nn]           Profile Evaluation Point at fraction nn% of border relative to center of field
                       GetScaledQfValue((2*Ord(side)-1)*abs(n.mm)*edge,relative,scNormalised,Xsource)
  ! R [nn]           Profile Evaluation Point at fraction nn% of border, with border and center rounded to steps of 5 mm
    s                Symmetry
                       100*wSource[Xsource].twSymmetry
    S                Extended symmetry according to menu choices
    T                Top position according to user choices
    			wSource[Xsource].twTopModel.Xtop or wSource[Xsource].twFFFslopesTop
    t                Top position for Top model.
                       wSource[Xsource].twTopModel.Xtop
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
                          Y[side]:= twPos_cm[twMaxArr]
                        else
       user scaling       Y[side]:= GetPenumbraValue(Xsource,X,side);
    y  [[-|+]value]  x value at y
                        if (ScanType in twcVertScans) and (ConvStg='100') and (Selection='Y') then
                          Y[side]:= twPos_cm[twMaxArr]
                        else
       standard scaling   Y[side]:= GetPenumbraValue(Xsource,X/twAbsNormVal,side);
-----
In the ResultsInfoRecord (see also PanelElements) the result is returned, including the actual applied twEdgeUseType (Wellhofer.pas)

*)
{27/12/2017 implementation of all EvaluationXtypes}
{14/01/2018 more extensions}
{16/01/2018 Y[sides] should be filled at all times}
{31/01/2018 m: div 0}
{01/02/2018 ViewMillimetersItem, Ymultiplier, Y_mm}
{12/04/2018 'D': handle also non-predefined levels}
{12/10/2018 ViewMeasNormAdjustMode,MeasNormAdjustEdit}
{27/10/2018 ViewMeasNormAdjustMode only in advancedmode}
{02/12/2019 Sidedness introduced}
{05/12/2019 'T' again implemented}
{10/12/2019 check validity of sigmoid results}
{10/01/2020 do not rewrite Xtype with selection}
{22/05/2020 do sigmoid analysis when not twSigmoidDone}
{05/06/2020 Ylevel,FindLevelPos}
{17/06/2020 implementation of PCRxrecord.Xedge}
{18/06/2020 implementation of 50% based on sigmoid (I)}
{19/06/2020 GetNormalisedRevLogistic}
{21/06/2020 'I': twLevelPos[Ylevel].Penumbra[side].Calc
            'u': if Xedge=dUseSigmoid50}
{22/06/2020 implementation of r, R}
{29/06/2020 more subtle rounding for R}
{30/06/2020 penumbra changed ('p','q')}
{04/07/2020 local function AssureSigmoidData, added 'U'}
{08/07/2020 change xsource for 'i','I' and 'q'}
{09/07/2020 Usource}
{25/08/2020 added 'C'}
{03/09/2020 'p' and 'q' completely splitted, implementation ft_DynPenumbraCheckbox}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{16/10/2020 fill ConvStg with dose level of border for u,U}
{05/03/2021 added 'Q'}
{02/04/2021 X_actual,twActDet2NormRatio}
{14/05/2021 implemented 'K', 't'}
{28/03/2022 expanded twConfidenceLimit}
{29/03/2022 support for gamma pass rate 'V'}
{06/04/2022 more efficient use of gammaanalysis by using calculated for output}
{14/04/2022 for gammaanalysis parameters G and V support additional 0 to take default from settings tab}
{11/07/2022 implementation of sigmoid slope output changed (Q,q)}
function TAnalyseForm.EvaluateInfoRecord(var ARec:ResultsInfoRecord): twcFloatType;
var side      : twcSides;
    Ynorm,r,c : twcFloatType;
    Divisor   : Integer;
    Selection : Char;
    b         : Boolean;
    GammaScope: twcGammaScope;

   function Yeff(const ASource:twcDataSource): twcFloatType;
   begin
   with Engines[UsedEngine],wSource[ASource] do
     try
       Result:= GlobalNormAdjust_perc.Value/
                Math.Max(0.01,ifthen((ScanType in twcHoriScans) and (wNormalisation[twSetFieldType]=NormOnInFieldArea),twRelAvgInField,1));
      except
       Result:= 0
      end;
   end;

  function SetLengthUnits(const AUnitsType:UnitsType): twcFloatType;
  begin
  if ViewMillimetersItem.Checked and (ARec.Ymultiplier=1) then ARec.Y_mm:= AUnitsType
  else                                                         ARec.Y_mm:= no_Units;
  case ARec.Y_mm of
    Units_in_numerator  : Result:= 10;
    Units_in_denominator: Result:=  0.1;
   else                   Result:=  1;
   end;
  end;

  function AssureSigmoidData(const ASource:twcDataSource): Boolean;
  var i: Integer;
  begin
  with Engines[UsedEngine],wSource[ASource] do
    begin
    if not twSigmoidDone then
      begin
      SigmoidPenumbraFit(ASource);
      i:= 100;
      while (not EngineReady) and (i>0) do
        begin
        WaitLoop(100);
        Dec(i);
        end;
      end;
    Result:= BordersValid(ASource,dInflection);
    end;
  end;

begin
with Engines[UsedEngine],ARec do                                                //Y_mm is changed from its default no_Units value here
  begin
  USource  := Xsource;
  Result   := X;
  Sidedness:= True;
  Ynorm    := Math.Max(0.01,wSource[USource].twAppliedNormVal);
  Xedge    := GetRelatedPositionType(wSource[USource].twAppliedEdgeLevel);      //Xedge holds confirmed edge type, source Wellhofer with user settings, and may be freely changed
  if Xtype='S' then                                                             //Extended symmetry according to menu choices
    case ExtSym of
      ExtSymLinacError: Selection:= 'L';
      ExtSymAreaRatio : Selection:= 'a';
     else               Selection:= 'l';
     end
  else
    Selection:= Xtype;
  if (ScanType in twcVertScans) and (Xsign=0) then
    Xsign:= 1;
  for side:= twcLeft to twcRight do if Sidedness then
    case Selection of
      'a': begin //twSymAreaRatio
           Result   := 100*wSource[USource].twSymAreaRatio;
           Sidedness:= False;
           end;
      'b': begin //border as based on edges user choices
           if MeasUserDoseItem.Checked then YLevel:= dUser
           else                             YLevel:= Engines[UsedEngine].wSource[USource].twAppliedEdgeLevel;
           if (Xedge in [dUseDerivative,dUseInflection,dUseSigmoid50]) and (USource in twcFilteredCopies) then
             USource:= twcCoupledSources[USource];
           if (Ylevel=dUser) and (Xedge=dUseSigmoid50) then
             Y[side]:= SetLengthUnits(Units_in_numerator)*GetNormalisedRevLogistic(Side,USource,UserBorderDose_perc.Value)
           else
             Y[side]:= SetLengthUnits(Units_in_numerator)*wSource[Usource].twLevelPos[YLevel].Limit[side].CalcPos;
           end;
      'c': begin //Center position as defined by user choices
           Result   := SetLengthUnits(Units_in_numerator)*wSource[USource].twCenterPos_cm;
           Sidedness:= False;
           end;
      'C': begin
           Result   := ifthen(wSource[USource].twTopModel.Valid,100*wSource[USource].twTopModel.Qquad/wSource[USource].twAbsNormValue,Xerrorval);  //always %/cm2 as units
           Sidedness:= False;
           end;
      'd': begin //relative flatness
           if ReferenceValid and (BordersValid or MeasMissingPenumbraItem.Checked) then
             begin
             if ProcessSetMergeSourceItem.Checked then Usource:= dsCalculated
             else                                      Usource:= dsUnrelated;
             Divide(dsMeasFiltered,dsRefFiltered,Usource);
             Result:= wSource[Usource].twFlatness*100;
             end
           else
             Result:= Xerrorval;
           Sidedness:= False;
           end;
      'D': begin //The result is the depth for a pdd or the edge of a profile
           if ScanType in twcVertScans then
             Xsign:= 1;
           YLevel:= String2DosePoint(ConvStg,dUser);
           if YLevel<>dUser then with wSource[USource].twLevelPos[YLevel].Limit[side] do
             Y[side]:= ifthen(Valid,SetLengthUnits(Units_in_numerator)*CalcPos,0)
           else
             Y[side]:= SetLengthUnits(Units_in_numerator)*GetPenumbraValue(USource,X,Side);
           end;
      'e': begin
           YLevel := dDerivative;
           Y[side]:= SetLengthUnits(Units_in_numerator)*wSource[USource].twLevelPos[Ylevel].Limit[side].CalcPos; //Derivative Edge
           Xedge  := dUseDerivative;
           end;
      'F': begin
           Result   := Yeff(USource)*ifthen(Xsign<0,wSource[USource].twRelMinInField,wSource[USource].twRelMaxInField);
           Sidedness:= False;
           end;
      'f': begin //Absolute flatness
           with wSource[USource] do
             if FFFfeatures then Result:= SetLengthUnits(Units_in_denominator)*50*(abs(twFFFslope[twcLeft].twFFFgain)+abs(twFFFslope[twcRight].twFFFgain))/Ynorm
             else                Result:= wSource[USource].twFlatness*100;
           Sidedness:= False;
           end;
      'V',
      'v',
      'G',        //Gamma analysis within InField area
      'g': begin  //Gamma analysis over complete profile
           if ReferenceValid then
             begin
             if ConvStg='0' then {if '0' is added, the default of the user interface is taken}
               begin
               if GammaInFieldLimits[wSource[dsMeasured].twSetFieldType].Checked then Selection:= UpCase(Selection)
               else                                                                   Selection:= LowerCase(Selection);
               end;
             if Selection in ['G','V'] then GammaScope:= gGammaLimited
             else                           GammaScope:= gGammaComplete;
             if not wSource[USource].twIsGamma then
               begin
               if ProcessSetMergeSourceItem.Checked or RefUseGammaItem.Checked then Usource:= dsCalculated
               else                                                                 Usource:= dsUnrelated;
               GammaAnalysis(dsMeasured,dsReference,Usource,False,GammaScope,ProcessAutoscalingItem.Checked,
                             ifthen(ViewMeasNormAdjustMode.Checked and AdvancedModeItem.Checked,MeasNormAdjustFactor,1));
               end;
             Result:= ifthen(Selection in ['G','g'],wSource[USource].twConfidenceLimit[GammaScope],wSource[USource].twGammaPassRate[GammaScope])
             end
           else
             Result:= Xerrorval;
           Sidedness:= False;
           end;
      'i': begin {inflection point based on sigmoid}
           if USource in twcFilteredCopies then
             USource:= twcCoupledSources[USource];
           Xedge := dUseInflection;
           with wSource[USource] do
             if AssureSigmoidData(USource) then
               begin
               Ylevel := dInflection;
               Y[side]:= SetLengthUnits(Units_in_numerator)*ifthen(twLevelPos[Ylevel].Limit[side].Valid,twLevelPos[Ylevel].Limit[side].CalcPos,0);   //sigmoid penumbra
               end
             else
               Y[side]:= Xerrorval;
           end;
      'I': begin {position 50% level based on sigmoid}
           if USource in twcFilteredCopies then
             USource:= twcCoupledSources[USource];
           Xedge:= dUseInflection;
           with wSource[USource] do
            if AssureSigmoidData(USource) then
               begin
               Ylevel := dSigmoid50;
               Xedge  := dUseSigmoid50;
               Y[side]:= SetLengthUnits(Units_in_numerator)*twLevelPos[Ylevel].Limit[side].CalcPos;
               end
             else
               Y[side]:= Xerrorval;
           end;
      'K': begin
           with wSource[Xsource] do
             Result:= 100*ifthen(twTopModel.Valid,twTopModel.Ytop,0)/twAbsNormValue;
           Sidedness:= False;
           end;
      'L': begin  //symlinacerror
           Result   := 100*wSource[USource].twSymLinacError;
           Sidedness:= False;
           end;
      'l': begin  //elevation
           Result   := 100*wSource[USource].twLinSlope*(GetPosition(USource,wSource[USource].twInfieldRange[twcRight])-GetPosition(USource,wSource[USource].twInfieldRange[twcLeft]))/Ynorm;
           Sidedness:= False;
           end;
      'M': begin  //max position
           Result   := SetLengthUnits(Units_in_numerator)*wSource[USource].twMaxPos_cm;
           Sidedness:= False;
           end;
      'm': begin  //relative maximum
           if wSource[USource].twAbsNormValue<0 then
             Result:= Xerrorval
           else
             Result:= Yeff(USource)*wSource[Xsource].twMaxValue/wSource[Xsource].twAbsNormValue;
           Sidedness:= False;
           end;
      'N': begin  //Norm position
           Result   := SetLengthUnits(Units_in_numerator)*wSource[Xsource].twAbsNormPos_cm;
           Sidedness:= False;
           end;
      'n': begin  //Norm value
           Result   := wSource[Xsource].twAbsNormValue;
           Sidedness:= False;
           end;
      'p': begin
           if      ConvStg='1' then b:= False
           else if ConvStg='2' then b:= True
           else                     b:= ft_DynPenumbraCheckbox[wSource[Xsource].twSetFieldType].Checked;
           if b then Ylevel:= dTemp
           else      Ylevel:= d50;
           Y[side]:= SetLengthUnits(Units_in_numerator)*GetPenumbraWidth(XSource,side,b);
           end;
      'P': begin  //Percentage depth dose | PDD20/10
           with wSource[Xsource] do
             if      ConvStg='10'                      then Result:= twPDD10
             else if ConvStg='20'                      then Result:= twPDD20
             else if (ConvStg='20/10') and (twPDD10>0) then Result:= twPDD20/twPDD10
             else                                           Result:= Xerrorval;
           Sidedness:= False;
           end;
      'q',
      'Q': begin
           if Usource in twcFilteredCopies then
             Xsource:= twcCoupledSources[Usource];                              //use unfiltered
           with wSource[Xsource] do
             if AssureSigmoidData(Xsource) then
               try
                 with twSigmoidFitData[side],twNMReport do                      //normalisation on local value instead of global Ynorm
                   if Selection='Q' then c:= abs(twFitTrueInflSlope/twFitTrueInflVal)
                   else                  c:= abs(2*BestVertex[sigmoid_Slope]/(BestVertex[sigmoid_HighVal]+BestVertex[sigmoid_LowVal]));
                 Xedge  := dUseInflection;                                      //change confirmed Xedge to sigmoid-related value
                 Y[side]:= SetLengthUnits(Units_in_denominator)*100*c;
                 if not Inrange(Y[side],0,1000) then
                   Y[side]:= 0;
                except
                 Y[side]:= Xerrorval;
                end {try}
             else
               Y[side]:= Xerrorval;
           end;
      'r',
      'R': if ScanType in twcHoriScans then
             begin
             Ylevel:= wSource[Xsource].twAppliedEdgeLevel;
             c     := wSource[Xsource].twCenterPos_cm;                          //center [cm]
             r     := wSource[Xsource].twLevelPos[YLevel].Limit[side].CalcPos-c;//radius of field [cm]
             if (Selection='R') and (Abs(r)>=1) then
               begin
               Divisor:= ifthen(r>5.5,1,2);                                     //round to 1 cm when r>5.5 or to 0.5 cm for r<=5.5
               r      := Round(Divisor*r)/Divisor;                              //multiply with divisor, and after rounding, divide again
               c      := Round(Divisor*c)/Divisor;
               end;
             X_actual := X*r/100;
             Y[side]  := GetScaledQfValue((2*Ord(side)-1)*abs(X_actual)+c,False,scNormalised,Xsource);
             Result   := Y[side];
             Sidedness:= (Xsign<>0);
             end
           else
             begin
             Result   := 0;
             Sidedness:= False;
             end;
      's': begin  //Symmetry
           Result   := 100*wSource[Xsource].twSymmetry;
           Sidedness:= False;
           end;
      't',
      'T': begin
           if SyntheticMade then Usource:= dsMeasured
           else                  Usource:= Xsource;
           with wSource[Usource] do
             Result:= SetLengthUnits(Units_in_numerator)*ifthen((Selection='t') or FFFpSubItems[CenterFFFTopModel].Checked,ifthen(twTopModel.Valid,twTopModel.Xtop,twMaxPos_cm),twFFFslopesTop);
           Sidedness:= False;
           end;
      'u',
      'U': begin //border or sigmoid at user value
           Ylevel:= dUser;
           if (Xedge=dUseSigmoid50) or (Selection='U') then
             begin
             Xedge:= dUseSigmoid50;
             if Usource in twcFilteredCopies then
               Usource:= twcCoupledSources[Usource];
             if AssureSigmoidData(Usource) then
               Y[side]:= SetLengthUnits(Units_in_numerator)*GetNormalisedRevLogistic(Side,Usource,UserBorderDose_perc.Value)
             else
               Y[side]:= Xerrorval;
             r:= 50;
             end
           else
             with wSource[Xsource],twLevelPos[dUser],Limit[side] do
               begin
               if not Valid then
                 FindLevelPos_cm(Xsource,Ylevel,True);
               Y[side]:= SetLengthUnits(Units_in_numerator)*CalcPos;
               r:= 100*Level/twAbsNormValue;
               end;
          if (Side=twcLeft) and (Length(ConvStg)>0) and (CountChars(ConvStg,'0123456789.')=Length(ConvStg)) then
             ConvStg:= Num2Stg(r,0,0);
           end;
      'w': begin  //Width according menu choices
           if MeasUserDoseItem.Checked then YLevel:= dUser
           else                             YLevel:= Engines[UsedEngine].wSource[Usource].twAppliedEdgeLevel;
           if (YLevel=dInflection) and (Usource in twcFilteredCopies) then
             Usource:= twcCoupledSources[Usource];
             Result:= SetLengthUnits(Units_in_numerator)*GetFieldWidth_cm(USource,Ylevel);
           Sidedness:= False;
           end;
      'x',
      'z',
      'X',
      'Z': with wSource[Usource] do
             begin
             if (LowerCase(Selection)='z') and (ScanType in twcHoriscans) then
               r:= twActDet2NormRatio                                           //ratio of used SDD to standard SSD, corrected for already applied scaling
             else
               r:= 1;
             if (Selection in ['X','Z']) and twCenterPosValid then
               c:= twCenterPos_cm                                               //relative to center
             else
               c:= 0;
             X_actual := (X+c)*r;                                               //for Z-scaling the best choice is to apply it also on the center position
             Y[side]  := GetScaledQfValue((2*Ord(side)-1)*abs(X_actual),False,scNormalised,Usource);
             Result   := Y[side];
             Sidedness:= (Xsign<>0);
             end;
      'y',
      'Y': begin
           with wCurveInfo,wSource[USource] do
             if (ScanType in twcVertScans) and (ConvStg='100') and (Selection='Y') then
               Result:= SetLengthUnits(Units_in_numerator)*twPos_cm[twMaxArr]
             else
               Result:= SetLengthUnits(Units_in_numerator)*GetPenumbraValue(USource,X/ifthen(Selection='y',twAbsNormValue,1),side);
           Sidedness:= False;
           end;
     else
       begin
       Result   := 0;
       Sidedness:= False;
       SetMessageBar(Format('No evaluation result for "%s"',[Selection]));
       end;
     end; {case/if/for}
  if Sidedness then
    begin
    if Xsign=0      then Result:= (Y[twcLeft ]+Y[twcRight])/2
    else if Xsign<0 then Result:=  Y[twcLeft ]
    else                 Result:=  Y[twcRight];
    end
  else
    begin
    Y[twcLeft ]:= Result;
    Y[twcRight]:= Result;
    end;
  end; {with}
end; {~evaluateinforecord}


{25/02/2015}
{27/04/2020 --Freepascal: DecimalSeparator (deprecated), use DefaultFormatSettings.DecimalSeparator --}
procedure TAnalyseForm.PublishValue(ALabel  :TLabel;
                                    AValue  :Single;
                                    AColor  :TColor=clBlack;
                                    Decimals:Integer=2;
                                    AUnit   :String='');
var Stg: String;
begin
if ViewHighResValItem.Checked then
  Inc(Decimals);
if Abs(AValue)>DefErrorLimit then Stg:= Format('%*s',[DefNumValueChars,'-'])
else                              Stg:= Format('%*.*f%s',[DefNumValueChars,Decimals,AValue,AUnit]);
if DefaultFormatSettings.DecimalSeparator<>'.' then
  ALabel.Caption:= AnsiReplaceStr(Stg,'.', DefaultFormatSettings.DecimalSeparator)
else
  ALabel.Caption:= Stg;
ALabel.Font.Color:= AColor;
ALabel.Visible   := True;
end; {~publishvalue}


{05/08/2015}
procedure TAnalyseForm.PublishValue(ABlock  :CxBlock;
                                    AValue  :Single;
                                    AColor  :TColor=clBlack;
                                    Decimals:Integer=2;
                                    AUnit   :String='');
begin
PublishValue(ABlock[CxValue],AValue,AColor,Decimals,AUnit);
end; {~publishvalue}


{14/04/2020 Lazarus replacement for TeeChart Zoomed function}
{15/04/2020 full extent based on PlotScaleMax and PlotScaleMin}
function TAnalyseForm.DataPlotZoomed: Boolean;
begin
with DataPlot.LeftAxis do
 Result:= Range.Max-Range.Min<PlotScaleMax-ifthen(ViewLeftAxisLowZeroItem.Checked,0,PlotScaleMin);
end; {~dataplotzoomed}


//responds to to Zoom-item in View menu
//responds also to Zoom-event of DataPlot to handle right-axis
{=> ViewZoomItem}
{14/04/2020 Lazarus adaptation}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.ActivateZoom(Sender:TObject);
begin
inherited;
with ViewZoomItem do
  begin
  if Engines[UsedEngine].wSource[dsMeasured].twValid and (not DataPlotZoomed) then
    Checked:= True
  else
    Checked:= not Checked;
  end;
AutoZoom(not (Sender=DataPlot));
end; {~activatezoom}


//Responds to View menu/Unzoom-item event and UndoZoom event of DataPlot.
{=> ViewUnZoomItem}
{15/04/2020 Lazarus adaptation}
procedure TAnalyseForm.ActivateUnZoom(Sender:TObject);
begin
inherited;
if (Sender=DataPlot) then AutoZoom(False)
else                      DataPlotUnZoom;
end; {~activateunzoom}


{14/04/2020 Lazarus replacement for TeeChart UndoZoom function}
procedure TAnalyseForm.DataPlotUnZoom;
begin
with DataPlot do
  begin
  ViewUnZoomItem.Enabled:= False;
  LeftAxis.Range.UseMin := ViewLeftAxisLowZeroItem.Checked;
  LeftAxis.Range.UseMax := False;
  LeftAxis.Range.Min    := ifthen(ViewLeftAxisLowZeroItem.Checked,0,PlotScaleMin);
  if PlotScaleMax>LeftAxis.Range.Min+1 then
    LeftAxis.Range.Max  := PlotScaleMax;
  end;
end; {~dataplotunzoom}


{29/06/2015
Called indirect by Zoom-click event in view menu and file open/read event.
When called indirect by the OnZoom event of DataPlot the FullAuto parameter must
be false because the user then decides on the right amount of zoom. Moreover this
avoids a loop (UndoZoom).
The usage of the right axis can be claimed by the Calculated curve or,
in second instance, by the Buffer curve.}
{29/07/2015:
  removed LeftAxis  .AutomaticMinimum:= not z;
  removed BottomAxis.AutomaticMinimum:= not z;
  removed BottomAxis.AutomaticMaximum:= not z;}
{04/08/2015
  LeftAxis.AutomaticMinimum:= not (z or ViewLeftAxisLowZeroItem)}
{07/12/2015
  SetAxis: if not (twIsRelative or twIsDerivative) then Result:= chartLeftAxis (TObaseDef.pas)
           (or IsDerivative added)}
{12/06/2018 normalising profiles with peak < 100: vertical axis maximum at least 105}
{13/11/2018 apply GetDisplayedPositionScale with input for BottomAxis}
{14/04/2020 =================Lazarus adaptation=====================}
{17/04/2020 check if calculated and buffer data are valid}
{20/04/2020 found solution for double axis scaling: switch on/off both autotransforms}
{08/05/2020 update LastProfileZoomState}
{14/05/2020 alignment of rightaxis}
{15/05/2020 twScanPosCm[ptFirst],twScanPosCm[ptLast], smarter setting bottom axis range}
{10/06/2020 maximise horizontal scale when syntheticmade}
{21/07/2020 fcWedge}
{29/07/2020 switch on title for Gamma, no range adaption for Gamma}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{21/02/2021 avoid potentional log(0) situations}
procedure TAnalyseForm.AutoZoom(FullAuto:Boolean=True);
var isProfile,isDepthDose,ZoomWanted,CalcIsGamma: Boolean;
    r,x                                         : twcFloatType;

  procedure MinMaxZoomed(amin,amax,arng:twcFloatType;
                         AxisNumber    :integer);  {see TAChartAxis.pas}
  var tavg: twcFloatType;
  begin
  arng:= Abs(arng*(amax-amin)/2);
  tavg:= (amax+amin)/2;
  amin:= tavg-arng;
  amax:= tavg+arng;
  with DataPlot.AxisList[AxisNumber] do
    begin
    if amin>Range.Max then
      begin
      Range.Max:= amax;
      Range.Min:= amin;
      end
    else
      begin
      Range.Min:= amin;
      Range.Max:= amax;
      end;
    Range.UseMin:= True;
    Range.UseMax:= True;
    end; {with}
  end;

  function SetAxis(ASource:PlotItems): Integer;   {see DefChartAx constants}
  var r: twcFloatType;

    function DeciStg(AValue:twcFloatType): String;
    begin
    Result:= Num2Stg(AValue,0,ifthen(AValue-Trunc(AValue)>0.05,1,0));
    end;

  begin
  with Engines[UsedEngine],wSource[PlotDataMapping[ASource]] do
    try
      r:= Math.Max(1,ifthen(twAbsNormValue>0,twAbsNormValue,twMaxValue));
      if  (Plotseries[ASource].Active                                                                           and
          ( (DataPlot.LeftAxis.Range.Min/Math.Max(DataPlot.LeftAxis.Range.Min,DataPlot.LeftAxis.Range.Max)<0.5) or
            ( (ScanType in twcVertScans) and twIsRelative )                                          or
            (twMaxValue/r<0.5                                                                   )    )   ) or
           ErrorSeries.Active                                            then
        begin
        if not (twIsRelative or twIsDerivative) then Result:= DefChartAxL
        else                                         Result:= DefChartAxR;
        if twIsGamma then with DataPlot.AxisList[DefChartAxR] do
          begin
          Range.Min            := 0;
          Range.Max            := Math.Max(0.2,HistogramLimit_num.Value);
          Intervals.Count      := 10;
          Title.LabelFont.Color:= Marks.LabelFont.Color;                        //Title.LabelFont.Orientation=2700
          Title.Visible        := True;
          Title.Caption        := Format('Gamma (%s%% / %s mm%s)',
                                         [DeciStg(GammaDoseNorm_perc.Value),DeciStg(GammaDistNorm_mm.Value),
                                          ifthen(twcGammaLocalDosePerc,', local dose','')] );
          end
        else
          begin
          if ((ASource=pCalculated) and twIsRelative) or (Result=DefChartAxL) then
            begin
            r:= 0.5+ZoomRange/2;
            MinMaxZoomed(100/r,100*r,1,DefChartAxR)
            end
          else
            MinMaxZoomed(twPlotScaling*twData[twMinArr],twPlotScaling*twData[twMaxArr],ZoomRange,DefChartAxR);
            with DataPlot.AxisList[DefChartAxR] do
              begin
              Title.Visible        := False;
              Title.Caption        := '';
              Marks.LabelFont.Color:= ifthen(ErrorSeries.Active,Errorseries.SeriesColor,PlotSeries[ASource].SeriesColor);
              end;
            end; {else not gamma}
          end {plotseries[asource].active}
      else
        Result:= DefChartAxL;
     except
        Result:= DefChartAxL;
     end;
  DataPlot.AxisList[DefChartAxR].Visible:= (Result=DefChartAxR) or ErrorSeries.Active;
  end; {setaxis}

begin
with DataPlot,Engines[UsedEngine] do if IsValid then
  begin
  with wSource[dsCalculated] do
    CalcIsGamma:= twValid and twIsGamma;
  if FullAuto then
    begin
    x:= GetDisplayedPositionScale;
    with wSource[dsMeasured] do
      begin
      isProfile:= (twScanPos_cm[ptLast]-twScanPos_cm[ptFirst])/(twDataPos_cm[ptLast]-twDataPos_cm[ptFirst])>0.5; //local use of isProfile here
      try
        BottomAxis.Range.Min:= Math.Min(ifthen(isProfile,twDataPos_cm[ptFirst],twScanPos_cm[ptFirst])*x,BottomAxis.Range.Max-1);
        BottomAxis.Range.Max:= Math.Max(BottomAxis.Range.Min+1,ifthen(isProfile,twDataPos_cm[ptLast],twScanPos_cm[ptLast])*x);
       except
        BottomAxis.Range.Min:= -20*x;
        BottomAxis.Range.Max:=  20*x;
       end;
      end;
    ZoomRange:= Clip(ZoomRange,0.8,10);
    isProfile:= (ScanType in twcHoriscans) and (wSource[dsMeasured].twSetFieldType<>fcWedge); //p set for
    with wSource[dsCalculated] do
      isDepthDose:= (ScanType=snPDD) and twValid and twIsRelative;                            //depth dose
    ZoomWanted:= (isProfile or isDepthDose);
    with LeftAxis do
      begin
      Range.Max   := Math.Max(Range.Min+1,PlotScaleMax);
      Range.UseMin:= not (ZoomWanted or ViewLeftAxisLowZeroItem.Checked);
      Range.UseMax:= (not ZoomWanted) or RefUseAddToItem.Checked;
      end;
     try
      if ZoomWanted and ViewZoomItem.Checked then
        begin
        r:= 0.5+ZoomRange/2;
        MinMaxZoomed(100/r,100*r,1,DefChartAxL);
        end
      else
        DataPlotUnZoom;
     except
      LeftAxis.Range.Min:=   0;
      LeftAxis.Range.Max:= 105;
     end;
    with wSource[dsMeasured] do
      if DataPlotZoomed and (not ViewZoomItem.Checked) and
        (isDepthDose or
         (isProfile and ((twLevelPos[d90].Limit[twcLeft ].CalcPos*x<BottomAxis.Range.Min) or
                         (twLevelPos[d90].Limit[twcRight].CalcPos*x>BottomAxis.Range.Max)    ))) then
        DataPlotUnZoom;
    ZoomWanted            := DataPlotZoomed;
    ViewUnZoomItem.Enabled:= ZoomWanted;
    LeftAxis.Marks.Format := Format('%%%d.%df',[Ceil(Log10(LeftAxis.Range.Max))+2,ifthen(ZoomWanted,1,0)]);
    end;
  if (ScanType in twcHoriScans) and (wSource[dsMeasured].twSetFieldType<>fcWedge) then
    LastProfileZoomState:= DataPlotZoomed;
  if wSource[dsCalculated].twValid then
    PlotSeries[pCalculated].AxisIndexY:= SetAxis(pCalculated);
  if wSource[dsBuffer].twValid and (PlotSeries[pCalculated].AxisIndexY<>DefChartAxR) and (not ErrorSeries.Active) then
     PlotSeries[pBuffer].AxisIndexY:= SetAxis(pBuffer);
  end;
L_AxisTransform_AutoScale.Enabled:= (not ZoomWanted) or CalcIsGamma;            //https://wiki.freepascal.org/TAChart_Tutorial:_Dual_y_axis,_Legend#Setting_up_auto-scale_axis_transformations
R_AxisTransform_AutoScale.Enabled:= L_AxisTransform_AutoScale.Enabled;          //in single axis mode *both* autoscaling transforms must be disabled!
if SyntheticMade then with DataPlot.AxisList[DefChartAxB],Engines[UsedEngine] do
  begin
  Range.Min:= GetDisplayedPositionScale(Math.Min(wSource[dsMeasured].twScanPos_cm[ptFirst],wSource[dsReference].twScanPos_cm[ptFirst]));
  Range.Max:= GetDisplayedPositionScale(Math.Max(wSource[dsMeasured].twScanPos_cm[ptLast] ,wSource[dsReference].twScanPos_cm[ptLast] ));
  end;
with DataPlot,AxisList[DefChartAxR] do
  if Visible then
    begin
    if ViewRightAxisToGridItem.Checked then
      Marks.Source:= AxisAlignSource                                            //set alignment of axis marks
    else
      Marks.Source:= nil;
    if not CalcIsGamma then
      begin
      r          := Range.Max-Range.Min;                                        //and nice limits (Marks.Format='%4.1f' in designer)
      r          := RoundDeci(r,ifthen(r>2.5,0,1));
      Range.Min  := Round((Range.Max+Range.Min)/2)-r/2;
      if (not Odd(Round(r))) and (r>3) then
        Range.Min:= Round(Range.Min);
      Range.Max  := Range.Min+r*DefAxisMaxExtension;
      end;
    Marks.Format:= Format('%%%d.1f',[Ceil(Log10(Max(10,Range.Max)))+2]);
    end;
PlotIndicators;
PlotCursor(Self);
end; {~autozoom}


{30/01/2021 smart label format leftaxis}
procedure TAnalyseForm.DataPlotExtentChanged(Sender: TChart);
var ex: TDoubleRect;                                                            //requires TAChartUtils in "uses"
begin
if Sender is TChart then with Sender as TChart do
  begin                                                                         //Range.Max/Min does not respond to manual zoom with mouse
  ex:= CurrentExtent;                                                           //see https://forum.lazarus.freepascal.org/index.php/topic,46381.msg330445.html#msg330445
  with LeftAxis do                                                              //with autoscaling ex.b.Y and ex.a.Y are in range [0 .. 1.01]
    Marks.Format:= Format('%%0.%df',[Max(0,2-Round(Log10(ifthen(L_AxisTransform_AutoScale.Enabled,100,1)*Abs(ex.b.Y-ex.a.Y))))]);
  if LoglevelEdit.Value>2 then
    with Dataplot.LeftAxis do SetMessageBar(Format('%0.2f, %0.2f, %0.2f',[ex.b.Y, ex.a.Y,Log10(ifthen(L_AxisTransform_AutoScale.Enabled,100,1)*Abs(ex.b.Y-ex.a.Y))]));
  end;
end; {~dataplotextentchanged}


{16/04/2020 LastZoomState replaced with ViewZoomItem.Checked}
{18/08/2020 changed IndicatorsOk}
{26/09/2020 PenumbraSigmoids}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{10/11/2020 twFitnormalisation*RawLogisticFunction}
{02/03/2021 FFFIndicators depend on FFFfeatures}
{14/05/2021 implemented ViewTopModelItem}
procedure TAnalyseForm.PlotIndicators;
var F,Y1,Y2,PosL,PosR,PosM,tmpX: twcFloatType;
    InFieldColor               : TColor;
    side                       : twcSides;
begin
case SelectedPlot of
  pReference : FFFdataSource:= dsReference;
  pCalculated: FFFdataSource:= dsCalculated;
 else          FFFdataSource:= dsMeasured;
 end;
with Engines[UsedEngine],wSource[FFFdataSource] do
  begin
  IndicatorsOk         := twValid      and (ScanType in twcHoriScans) and ((twFlatness<DefMaxFlatness) or (AppliedFieldClass<>fcStandard));
  TopModelSeries.Active:= IndicatorsOk and ((FFFfeatures and ViewFFFIndicatorsItem.Checked) or ViewTopModelItem.Checked) and wSource[dsMeasured].twTopModel.Valid;
  if IndicatorsOk then
    begin
    F           := twPlotScaling;
    Y1          := ifthen(ViewZoomItem.Checked,(DataPlot.LeftAxis.Range.Max-DataPlot.LeftAxis.Range.Min)/10,1);
    Y2          := Math.Min(DataPlot.LeftAxis.Range.Max-Y1,twMaxValue*F*DefZoomRange);
    Y1          := Math.Max(DataPlot.LeftAxis.Range.Min+Y1,Math.Min(80,Math.Min(twData[twInfieldRange[twcLeft]],twData[twInfieldRange[twcRight]]))*F/DefZoomRange);
    tmpX        := GetDisplayedPositionScale;
    InFieldColor:= ifthen(twInFieldAreaOk,
                          ifthen(twSymCorrected,
                                 DefSymCorrected,
                                 ifthen(((wNormalisation[wSource[dsMeasured].twSetFieldType]<>NormOnCenter) or (GlobalNormAdjust_perc.Value<>100)),
                                        clRed,
                                        clBlack)),
                          DefEstimatedCol);
    for side:= twcLeft to twcRight do        //------------------------ plot vertical lines for InField area ------------------------------
      begin
      InFieldIndicators[side].Clear;
      InFieldIndicators[side].Active       := ViewIndicatorsItem.Checked and IndicatorsOk;
      InFieldIndicators[side].SeriesColor  := InFieldColor;
      InFieldIndicators[side].LinePen.Color:= InFieldColor;
      InFieldIndicators[side].AddXY(tmpX*twInFieldPos_cm[side],Y1);
      InFieldIndicators[side].AddXY(tmpX*twInFieldPos_cm[side],Y2);
      PenumbraSigmoids[side] .Clear;
      PenumbraSigmoids[side] .Active       := ViewPenumbraItem.Checked and IndicatorsOk and twSigmoidFitData[side].twFitValid;
      if PenumbraSigmoids[side].Active then
         with twSigmoidFitData[side] do
           begin
           PenumbraSigmoids[side].SeriesColor  := PlotSeries[LinkPlotItem(FFFdataSource)].SeriesColor and $8f8f8f;
           PenumbraSigmoids[side].LinePen.Color:= PenumbraSigmoids[side].SeriesColor;
           PosM:= Math.Min(twStepSize_cm,0.025);
           PosL:= twFitLow_cm;
           PosR:= twFitHigh_cm;
           repeat
             PenumbraSigmoids[side].AddXY(tmpX*PosL,twFitNormalisation*RawLogisticFunction(twNMReport.BestVertex,PosL,twFitOffset_cm)*F);
             PosL:= PosL+PosM;
           until PosL>=PosR;
           end;
      end;
      if FFFfeatures then                    //------------------------ plot FFF slopes ------------------------------
        for side:= twcLeft to twcRight do with twFFFslope[side] do
         if twFFFvalid then
           begin
           FFFSlopeLines[side].Clear;
           FFFSlopeLines[side].Active:= True;
           FFFSlopeLines[side].AddXY(tmpX*twPos_cm[twFFFfirst],(twFFFoffset+twFFFgain*twPos_cm[twFFFfirst])*F);
           FFFSlopeLines[side].AddXY(tmpX*twPos_cm[twFFFlast ],(twFFFoffset+twFFFgain*twPos_cm[twFFFlast ])*F);
           end;
      if TopModelSeries.Active then          //------------------------ plot Top model, not strictly limited to FFF---
        with TopModelSeries.DomainExclusions do
          begin
          Clear;
          AddRange(NegInfinity         ,twTopModel.Xmin*tmpX);
          AddRange(twTopModel.Xmax*tmpX,Infinity            );
          ChangeZPosition(TopModelSeries,False);
          end;
    end; {if indicatorsok}
  end;
end; {~plotindicators}


{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.TopModelFunction(const AX: Double;
                                        out   AY: Double);
var X: Double;
begin
with Engines[UsedEngine].wSource[FFFdataSource],twTopModel do
  if Valid then
    begin
    X:= AX/GetDisplayedPositionScale;
    AY:= (Qofs+(Qquad*X+Qlin)*X)*twPlotScaling;
    end;
end; {~topmodelfunction}


{28/07/2015}
{$push}{$warn 5092 off}
procedure TAnalyseForm.SetPlotDate(APlotItem:PlotItems;
                                   AString  :String;
                                   Composite:Boolean=False);
const InsertStg='  ... ';
var b: Boolean;
begin
with PlotDates[APlotItem] do
  begin
  if Composite then
    AString:= DefComposite+'('+AString+')';
  b:= Canvas.TextWidth(AString)>Width;
  if b then
    begin
    while Canvas.TextWidth(InsertStg+AString)>Width do Delete(AString,Length(AString) div 2,1);
    Insert(InsertStg,AString,Length(AString) div 2);
    end;
  Caption:= AString;
  end;
end; {~setplotdate}
{$pop}


{16/04/2020 new}
procedure TAnalyseForm.ChangeZPosition(ASeries:TBasicChartSeries;
                                       MoveUp :Boolean=True);
var i: Integer;
begin
for i:= 0 to Pred(DataPlot.SeriesCount) do
  DataPlot.Series[i].ZPosition:= i+1;
ASeries.ZPosition:= ifthen(MoveUp,DataPlot.SeriesCount+2,0);
end; {~changezposition}


{15/12/2015}
{27/09/2020 PlotIndicators added to plot penumbra also}
procedure TAnalyseForm.OnSeriesSelected(AMouseButton:TMouseButton=mbLeft);
begin
if assigned(SelectedSeries) then
  begin
  ChangeZPosition(SelectedSeries,AMouseButton=mbLeft);
  if ViewPenumbraItem.Checked then
    PlotIndicators;
  SetMessageBar(SelectedSeries.Name+'...');
  end;
end; {~onseriesselected}


{01/02/2018 ViewMillimetersItem}
{12/10/2018 ViewMeasNormAdjustMode,MeasNormAdjustEdit,NormAdjustFactor}
{27/10/2018 ViewMeasNormAdjustMode only in advancedmode}
{18/04/2020 ---small adaptations for tachart---}
{14/08/2020 added (not ProcessAutoscalingItem.Checked) for visibility of MeasNormAdjustEdit}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.PlotCursor(Sender:TObject);
var k  : PlotItems;
    b,p: Boolean;
    x  : twcFloatType;

  procedure AddPoint(ASource    :twcDataSource;
                     var ACursor:TLineSeries;
                     var ALabel :TLabel);
  var v: twcFloatType;
  begin
  with Engines[UsedEngine],wSource[ASource] do if ALabel.Visible and ACursor.Active and twValid then
    begin
    v:= GetScaledQfValue(CursorPos_cm,False,scPlotScaling,ASource)*
        ifthen((ASource=dsMeasured) or ((ASource=dsCalculated) and (not twIsGamma)),MeasNormAdjustFactor,1);
    ACursor.Clear;
    ACursor.AddXY(x,v);
    PublishValue(ALabel,v);
    if (wSource[dsMeasured].twAbsNormConfig) and
       (not wSource[ASource].twIsrelative)   and
       (Ord(pa_config)<=Length(DefAnnotTypeString)) then
      ALabel.Caption:= DefAnnotTypeString[Ord(pa_config)]+Trim(ALabel.Caption);
    end;
  end;

begin
p:= False;
x:= GetDisplayedPositionScale(CursorPos_cm);
for k:= Low(PlotItems) to High(PlotItems) do
  begin
  PlotSeries[k]  .LinePen.Width  := ifthen((k=SelectedPlot) and SelectPlot,2,1);
  b                              := (PlotSeries[k].Count>0) and PlotSeries[k].Active;
  PlotLabels[k]  .Visible        := b;
  PlotDates[k]   .Visible        := b;
  PlotValues[k]  .Visible        := b and ViewValuesItem.Checked;
  CursorSeries[k].AxisIndexY     := PlotSeries[k].AxisIndexY;
  CursorSeries[k].Pointer.Visible:= PlotValues[k].Visible;
  p                              := p or b;
  end;
p                    := ViewValuesItem.Checked and p;
PositionLabel.Visible:= p;
PositionValue.Visible:= p;
with Engines[UsedEngine],wSource[dsMeasured] do
  begin
  if twValid then
    begin
    CursorPos_cm:= Clip(CursorPos_cm,twScanPos_cm[ptFirst],twScanPos_cm[ptLast]);
    for k:= Low(PlotItems) to High(PlotItems) do
      AddPoint(PlotDataMapping[k],CursorSeries[k],PlotValues[k]);
    PublishValue(PositionValue,GetDisplayedPositionScale(CursorPos_cm),clBlack,2);
    end;
  MeasNormAdjustEdit.Visible:= twValid and (not ProcessAutoscalingItem.Checked) and
                               (Abs(CursorPos_cm-twAbsNormPos_cm)<0.01)         and
                               ViewMeasNormAdjustMode.Checked                   and
                               AdvancedModeItem.Checked;
  end;
end; {~plotcursor}


{26/04/2020}
procedure TAnalyseForm.EnableMenuSystem(AEnabled:Boolean);
var i: Integer;
begin
if MainMenu.Items.Count>0 then
  for i:= 0 to MainMenu.Items.Count-1 do
    EnableMenu(MainMenu.Items[i],AEnabled);
end; {~enablemenusystem}


{04/04/2020 introduced from TTObaseForm}
{08/12/2020 changed implementation of Tag value}
procedure TAnalyseForm.EnableMenu(AMenu   :TMenuItem;
                                  AEnabled:Boolean);
var i: Integer;
begin
with AMenu do
  begin
  if Assigned(Action) then
     with Action do
       Enabled:= AEnabled;
  Enabled:= AEnabled;
  if Count>0 then
    for i:= 0 to Count-1 do
      if (Items[i].Tag and 4)=0 then                                            //ignore subitems with special Tag when state of whole menu changes
        EnableMenu(Items[i],AEnabled);
  end;
end; {~enablemenu}


{04/04/2020 === fpc:introduced from TTObaseForm which needed installation in Lazarus}
procedure TAnalyseForm.ControlsEnable(AControl:TControl;
                                      Enable  :Boolean);
var i: integer;
begin
if AControl is TWinControl then with AControl as TWinControl do
  begin
  if ControlCount>0 then
    for i:= 0 to Pred(ControlCount) do
      Controls[i].Enabled:= Enable;
  Enabled:= Enable;
  end;
end; {~controlsenable}


{06/01/2016 Power mub added}
{26/06/2016 twFitMaxScaling}
{28/06/2016 SelectedFitCol}
{03/08/2017 total result for fitted mub_power}
{11/09/2017 if not Wellhofer.wSource[Measured].twpddFitData[NM_Primary].twFitValid then RowCount:= 1}
{23/04/2020 set activepage}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.ShowFitResults;
var r        : twcNMpddFits;
    i,j,k,l,w: Integer;
    p        : ^fitNameArray;
    n        : ^fitNormArray;
begin
if not Engines[UsedEngine].wSource[dsMeasured].twpddFitData[NM_Extrapolation].twFitValid then
  PDDFitResultsAllCheckBox.State:= cbGrayed
else if PDDFitResultsAllCheckBox.Checked then
  PDDFitResultsAllCheckBox.State:= cbChecked
else
  PDDFitResultsAllCheckBox.State:= cbUnChecked;
PDDFitResultsGrid.RowCount:= 1;
w                      := Max(90,PDDFitResultsGrid.Canvas.TextWidth(Engines[UsedEngine].wSource[dsMeasured].twCurveIDString)+10);
if not Engines[UsedEngine].wSource[dsMeasured].twpddFitData[NM_Primary].twFitValid then
  PDDFitResultsGrid.RowCount:= 1;
for r:= NM_Primary to NM_Extrapolation do
  with PDDFitResultsGrid,Engines[UsedEngine].wSource[dsMeasured],twpddFitData[r] do if twFitValid then
    begin
    i       := Ord(r)*2;
    ColCount:= i+2;
    l       := Length(twNMReport.BestVertex);
    RowCount:= Max(RowCount,l+1);
    case twFitModel of
      pddPhotonExtrapolation: begin  p:= @pddfitXnames;  n:= @pddfitXnorm;  end;
      pddPhoton             : begin  p:= @pddfitPnames;  n:= @pddfitPnorm;  end;
     else                     begin  p:= @pddfitEnames;  n:= @pddfitEnorm;  end;
     end;
    ColWidths[Succ(i)]:= w;
    Cells[Succ(i),0]  := twCurveIDString;
    for k:= 1 to l do
      begin
      j               := Pred(k);
      Cells[i      ,k]:= p^[j];
      Cells[Succ(i),k]:= FloatFormat(ifthen(n^[j],twFitMaxScaling,1)*twNMReport.BestVertex[j],12);
      end;
    if twFitModel=pddPhoton then
      begin
      RowCount:= RowCount+1;
      if twcPddFitMubPowerFixed then
        begin
        RowCount              := RowCount+1;
        Cells[Succ(i),Succ(l)]:= FloatFormat(twcPddFitMubPower,3);
        end
      else
        Cells[Succ(i),l]:= FloatFormat(1+twNMReport.BestVertex[Pred(l)],12);    //deviation from 1 is optimised, show total result
      end;
    if r=NM_Primary then
      begin
      RowCount        := RowCount+1;
      k               := RowCount-1;
      Cells[i      ,k]:= 'dmax';
      Cells[Succ(i),k]:= FloatFormat(twFitScalingPoint_cm,4);
      end;
     Col:= SelectedFitCol;
    end; {with}
PageControl.ActivePage:= PDDFitResultsTab;
end; {~showfitresults}


{20/06/2017 replaced twDataRange[ptFirst]/Last with twScanRange[ptFirst]/Last to avoid non-useful data}
{14/10/2018 ignore data points <0}
{22/04/2020 ====Lazarus: minor adaptations====}
{23/04/2020 use InField area only for horizontal scans}
{14/05/2020 use Lo/Hi to set HistogramPlot.BottomAxis.Range.Min/Max}
{09/06/2020 improved histogram normalisation}
{07/09/2020 axis title in all modes with source identification}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{28/03/2022 introduction AppliedGammaScope}
procedure TAnalyseForm.ShowHistogram;
var i,j,k      : Integer;
    F,Lo,Hi,Ofs: twcFloatType;
    Sampler    : THistogramSampler;
    Stg        : String;
    aSource    : twcDataSource;
begin
if Engines[UsedEngine].wSource[HistogramSource].twValid then aSource:= HistogramSource
else                                                         aSource:= dsMeasured;
with Engines[UsedEngine],wSource[aSource] do if twValid then
  begin
  Analyse(aSource);
  j:= twScanRange[ptFirst];
  k:= twScanRange[ptLast];
  if twIsGamma then {data are gamma-analysis}
    begin
    F  := 1;
    Hi := HistogramLimit_num.Value;
    Lo := 0;
    Ofs:= 0;
    end
  else
    begin
    if twIsRelative then
      begin
      if ScanType in twcHoriScans then
        begin
        j:= twInfieldRange[twcLeft];
        k:= twInfieldRange[twcRight];
        F:= 100/twAbsNormValue;
        end
      else
        F:= twPlotScaling;
      Hi:= HistogramLimit_num.Value;
      Lo:= -Hi;
      Ofs:= 100;
      end
    else
      begin
      Hi := Ceil(twMaxValue);
      Lo := Trunc(twData[twMinArr]);
      F  := 100/Math.Max(1,Hi);
      Ofs:= 0;
      end;
    end;
  HistogramPlot.BottomAxis.Range.Min:= Lo;
  HistogramPlot.BottomAxis.Range.Max:= Hi;
  Sampler:= THistogramSampler.Create(F*Lo,F*Hi,0,Succ(k-j));
  Histogram.Clear;
  for i:= j to k do if twData[i]>=0 then
    Sampler.Add_X(F*twData[i]-Ofs);
  for i:= 0 to Pred(Sampler.NumBins) do
    Histogram.AddXY(Sampler.BinRangeLow+(2*i+1)*Sampler.BinSize/2,Sampler.BinCounts[i]);
  if      RefUseDivideByItem.Checked then Stg:= 'ratio error'
  else if RefUseGammaItem   .Checked then Stg:= 'gamma'
  else                                    Stg:= 'value';
  HistogramPlot.BottomAxis.Title.Caption:=
    Format('%s distribution (%s) CL=%0.2f%s %s',
           [Stg,twcDataSourceNames[aSource],Sampler.ConfidenceLimit,
            ifthen(RefUseDivideByItem.Checked,'%',''),
            ifthen(twIsGamma,Format('(raw data: %0.2f)',[twConfidenceLimit[AppliedGammaScope]]),'')]);
  try
    Sampler.Free
   except
    ExceptMessage('ShowHistogram!');
   end;
  end;
end; {~showhistogram}


procedure TAnalyseForm.AliasListDeleteClick(Sender:TObject);
var SendingObject: TObject;
begin
SendingObject:= AliasListEditor;
with SendingObject as TValueListEditor do
 DeleteRow(Selection.Top);
end; {~aliaslistdeleteclick}


procedure TAnalyseForm.AliasListInsertClick(Sender:TObject);
var SendingObject: TObject;
begin
SendingObject:= AliasListEditor;
with SendingObject as TValueListEditor do
  InsertRow('','',True);
end; {~aliaslistinsertclick}


{$push}{$warn 5024 off:parameters not used}
{01/09/2020 informs wether a certain key is used}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.AliasListPrepareCanvas(Sender   : TObject;
                                              aCol,aRow: Integer;
                                              aState   : TGridDrawState);
begin
if Engines[UsedEngine].AliasListKeyApplied(AliasListEditor.Keys[aRow]) then
  begin
  AliasListEditor.Font.Style:= [fsBold];
  AliasListEditor.Font.Color:= clRed;
  end
else
  begin
  AliasListEditor.Font.Color:= clBlack;
  AliasListEditor.Font.Style:= [];
  end;
inherited;
end;{~aliaslistpreparecanvas}
{$pop}


{01/09/2020 forgot to copy from Delphi}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{03/05/2021 pass new list to all enegines and do reload when not frozen}
procedure TAnalyseForm.AliasTabExit(Sender: TObject);
var i,j: Integer;

  procedure ReloadEngine(AEngine:Integer);
  begin
  SelectEngine(AEngine);
  if not Engines[AEngine].Freeze then
    Reload(self);
  end;

begin
i:= AliasListEditor.RowCount;
j:= UsedEngine;
if (PageControl.ActivePage=AnalysisTab) or (i=0) or (not AliasListEditor.IsEmptyRow(i-1)) then
  begin                                                                         //adding an emty row trigers onexit
  i:= Length(Engines);
  while i>0 do
    begin
    Dec(i);
    if i<>j then
      ReloadEngine(i);
    end;
  ReloadEngine(j);
  end;
end; {~aliastabexit}


//put fit results on the clipboard
{=> PDDFitResultsGrid}
{28/06/2016 SelectedFitCol}
{13/03/2018 return to AnalysisTab}
{05/05/2020 use AsciiCRLF for colu,ns}
{06/10/2020 fundamentals alternative}
procedure TAnalyseForm.PDDFitResultsGridClick(Sender:TObject);
var i,j,k,l,m: LongInt;
    Stg      : String;
begin
with PDDFitResultsGrid do
  begin
  Stg:= '';
  k  := Pred(RowCount);
  if PDDFitResultsAllCheckBox.Checked then
    begin
    while Length(Rows[k].CommaText.Trim(','))=0 do                              //TStringHelper function
      Dec(k);
    l:= 0;
    m:= Pred(ColCount);
    end
  else
    begin
    l:= ifthen(Odd(Col),Pred(Col),Col);
    while (Length(Cells[l,k])=0) and (k>0) do Dec(k);
    m:= Succ(l);
    end;
  for j:= ifthen(PDDFitResultsHeaderCheckBox.Checked,0,1) to k do
    for i:= l to m do
      if Odd(i) then
        Stg:= Stg+Cells[i,j]+ifthen(i<m,chTab,ifthen(j<k,AsciiCRLF,''))
      else if PDDFitResultsLabelsCheckBox.Checked then
        Stg:= Stg+Cells[i,j]+chTab;
  ClipBoard.AsText:= Stg;
  SetMessageBar(Cells[1,0]);
  SelectedFitCol:= Col;
  PageControl.ActivePage:= AnalysisTab;
  end;
end; {~fitresultsgridclick}


{=> ModListNormRadioButton, ModListFilmRadioButton, ModListBeamRadioButton}
{08/11/2016}
{29/07/2020 adjust number of columns}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{07/06/2021 check if list is available}
procedure TAnalyseForm.ModListRadioButtonClick(Sender: TObject);
var i: Integer;
    s: String;
    L: ^TModalityList;
begin
if Sender=ModListFilmRadioButton then
  begin
  ModMode:= ModMFilm;
  L      := @Engines[UsedEngine].ModalityFilmList;
  s      := DefModFilmKeys;
  end
else if (Sender=ModListBeamRadioButton) and RefGenericBeamItem.Checked then
  begin
  ModMode:= ModMBeam;
  L      := @Engines[UsedEngine].ModalityBeamList;
  s      := DefModBeamKeys;
  end
else
  begin
  ModMode:= ModMNorm;
  L      := @Engines[UsedEngine].ModalityNormList;
  s      := DefModNormKeys;
  end;
ModListGrid           .ColCount  := s.CountChar(',')+1;
ModListGrid           .RowCount  := 1;
ModListGrid           .EditorMode:= False;
ModListBeamRadioButton.Visible   := RefGenericBeamItem.Checked;
ModListGrid.Rows[0]   .CommaText := s;
if assigned(L) then
  begin
  if L^.DataCount>0 then
    for i:= 0 to Pred(L^.DataCount) do
      ModListGridFill(L^.CommaText[i])
  else
    ModListAddClick(Sender);
  end;
ModListUpdate(Sender);
end; {~modlistradiobuttonclick}


{=> ModListAddButton}
{09/11/2016}
procedure TAnalyseForm.ModListAddClick(Sender:TObject);
var r: TGridRect;
begin
with ModListGrid do
  begin
  r.Top    := RowCount;
  r.Left   := 0;
  r.Bottom := r.Top;
  r.Right  := Pred(ColCount);
  RowCount := RowCount+1;
  Selection:= r;
  with Rows[r.Top] do
    case ModMode of
      ModMNorm: CommaText:= DefDoseNormValues;
      ModMFilm: CommaText:= DefDoseFilmValues;
      ModMBeam: CommaText:= DefDoseBeamValues;
    end;
  end;
end; {~modlistaddclick}


{08/11/2016 dual list implementation}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{02/04/2022 extension for multiple identical modalities with normalisation but differences in info}
procedure TAnalyseForm.ModListGridFill(ACommaText  :String;
                                       AddToModData:Boolean=False);
var i,j  : Integer;
    found: Boolean;
    f,m,s: String;
    L    : TModalityList;
begin
with ModListGrid do if Length(ACommaText)>0 then
  begin
  s:= ACommaText;
  m:= copy(s,1,Pred(Pos(',',s)));  {m=modality}
  case ModMode of
    ModMFilm: begin
              for i:= 1 to DefDoseGridFilmCol do
                Delete(s,1,Pos(',',s)); {extract film type in two steps}
              f:= copy(s,1,Pred(Pos(',',s))); {f=filmtype}
              L:= Engines[UsedEngine].ModalityFilmList;
              end;
    ModMBeam: begin
              f:= '';
              L:= Engines[UsedEngine].ModalityBeamList;
              end;
   else
              begin
              for i:= 1 to DefNormGridTypeCol do
                Delete(s,1,Pos(',',s)); {extract info}
              f:= s.Trim(',');
              L:= Engines[UsedEngine].ModalityNormList;
              end;
   end; {case}
  i:= 0;
  j:= RowCount-1;
  if j>0 then
    repeat                                                                      {search for identical items}
      Inc(i);
      if ModMode in [ModMFilm,ModMBeam] then s:= ''
      else                                   s:= (Rows[i].Strings[DefNormGridTypeCol]+Rows[i].Strings[DefNormGridLinacCol]).Trim(',');
      found:= ((Rows[i].Strings[DefDoseGridModCol]=m) and (((ModMode=ModMFilm) and (Rows[i].Strings[DefDoseGridFilmCol]=f)) or (s=f)));
    until found or (i>=j)
  else
    found:= False;
  if not found then
    begin
    i       := RowCount;
    RowCount:= i+1;
    end;
  if AddToModData then
    begin
    L.AddModData(ACommaText);
    if ModMode=ModMFilm then with L as TModFilmList do j:= FindModData(m,f)
    else                                               j:= L.FindModData(m);
    if j>=0 then
      ACommaText:= L.CommaText[j];
    end;
  Rows[i].CommaText:= ACommaText;
  end;
end; {~modlistgridfill}


{$push}{$warn 5024 off:parameters not used}
procedure TAnalyseForm.ModListGridSelectCell(Sender       :TObject;
                                             ACol,ARow    :Integer;
                                             var CanSelect:Boolean);
begin
if ARow<>DoseCLastRow then
  with ModListGrid do
    begin
    if ModListGrid.EditorMode then
      ModListEditClick(Sender);
    DoseClastRow             := ARow;
    DoseCLastModality        := Rows[ARow].Strings[DefDoseGridModCol];
    DoseCLastFilmType        := Rows[ARow].Strings[DefDoseGridFilmCol];
    ModListEditButton.Enabled:= ARow>0;
    ModListDelButton .Enabled:= ModListAddButton.Enabled and (ARow>0);
    end;
end; {~doseconvgridselectcell}
{$pop}


{=> ModListEditButton}
{07/11/2016 dual list implementation}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.ModListEditClick(Sender:TObject);
var sm,sf  : String;
    ODmode : Boolean;
begin
ODmode:= ModListFilmRadioButton.Checked;
with ModListGrid do
  begin
  if DoseClastRow=-1 then
    DoseClastRow:= Selection.Top;
  with Rows[DoseClastRow] do
    begin
    sm:= Strings[DefDoseGridModCol];
    sf:= ifthen(ODmode,Strings[DefDoseGridFilmCol],'');
    if EditorMode then
      begin
      if ODmode then
        begin
        if DoseCLastFilmType<>sf then
          Engines[UsedEngine].ModalityFilmList.DelModData(DoseCLastModality,DoseCLastFilmType);
        Engines[UsedEngine].ModalityFilmList.AddModData(CommaText);
        end
      else
        Engines[UsedEngine].ModalityNormList.AddModData(CommaText);
      end
    else
      begin
      DoseCLastModality:= sm;
      DoseCLastFilmType:= sf;
      end;
    end; {with rows}
  EditorMode:= not EditorMode;
  end;
ModListUpdate(Sender);
end; {~modlisteditclick}


{09/11/2016}
{09/10/2018 EnableMenu(MeasMenu,KeyPreview AND AdvancedModeItem.Checked)}
procedure TAnalyseForm.ModListUpdate(Sender:TObject);
begin
ModListAddButton.Enabled:= AdvancedModeItem.Checked and not ModListGrid.EditorMode;
ModListDelButton.Enabled:= ModListAddButton.Enabled and (ModlistGrid.Selection.Top>0);
with ModListGrid do
  begin
  if EditorMode then
    begin
    Options:= Options-[goRowSelect];
    ModListEditButton.Caption:= DefModListReady;
    end
  else
    begin
    Options:= Options+[goRowSelect];
    ModListEditButton.Caption:= DefModListEdit;
    end;
  KeyPreview                    := not EditorMode;
  ModListNormRadioButton.Enabled:= KeyPreview;
  ModListFilmRadioButton.Enabled:= KeyPreview;
  ModListBeamRadioButton.Enabled:= KeyPreview;
  ModListEditButton     .Enabled:= Selection.Top>0;
  EnableMenu(ViewMenu,KeyPreview);
  EnableMenu(MeasMenu,KeyPreview and AdvancedModeItem.Checked);
  end;
end; {~modlistupdate}


{=> ModListDellButton}
{07/11/2016 dual list implementation}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.ModListDelClick(Sender:TObject);
var i,j: Integer;
begin
with ModListGrid do
  begin
  i:= Selection.Top;
  if ModListFilmRadioButton.Checked then
    Engines[UsedEngine].ModalityFilmList.DelModData(Cells[DefDoseGridModCol,i],Cells[DefDoseGridFilmCol,i])
  else
    Engines[UsedEngine].ModalityNormList.DelModData(Cells[DefDoseGridModCol,i]);
  j:= Pred(RowCount);
  if j>i then
    Rows[i]:= Rows[j];
  RowCount:= j
  end;
end; {~modlistdelclick}


{05/11/2016 rebuild}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.PopulateDoseConvList(Index:Integer);
var h: THelpContext;
    t: Integer;

  procedure SetWinControl(AControl    :TWinControl;
                          ALeft,AWidth:Integer;
                          NameSuffix  :String);
  begin
  with AControl do
    begin
    Parent     := UseBackGroundBox;   Name:= NameSuffix;
    Left       := ALeft;              Width:= AWidth;
    HelpContext:= h;
    end;
  end;

  procedure MakeCheckBox(var ACheckBox:TCheckBox;
                         ALeft,AWidth :Integer;
                         NameSuffix   :String);
  begin
  if not assigned(ACheckBox) then
    ACheckBox:= TCheckBox.Create(Self);
  SetWinControl(ACheckBox,ALeft,AWidth,NameSuffix+'ChkBx');
  with ACheckBox do
    begin
    Top   := t;   Width  := AWidth;
    Height:= 17;  Caption:= '';
    end;
  end;

  procedure MakeComboBox(var AComboBox:TComboBox;
                         ALeft,AWidth :Integer;
                         NameSuffix   :String);
  begin
  if not assigned(AComboBox) then
    AComboBox:= TComboBox.Create(Self);
  SetWinControl(AComboBox,ALeft,AWidth,NameSuffix+'CmbBx');
  with AComboBox do
    begin
    Top:= t-1;  Height:= 21;  Text:= '';
    end;
  end;

begin
with UseDoseConvTable[Index] do
  begin
  t:= DefBgLinesOfs+DefBgLinesStep*Succ(Index);
  h:= UseBackGroundBox.HelpContext;
  MakeCheckBox(DCDoseBox    , 25,90,DCname(0,Index));
  MakeCheckBox(DCBgBox      ,330,20,DCname(1,Index));
  MakeComboBox(DCFilmTypeBox,190,60,DCname(2,Index));
  MakeComboBox(DCModalityBox,115,65,DCname(3,Index));
  if not assigned(DCEdit) then
    DCEdit:= TFloatSpinEdit.Create(Self);
  SetWinControl(DCEdit,356,70,DCname(4,Index));
  with DCEdit do
    begin
    Top     :=  t-1;
    Height  :=   21;
    Caption :=   '';
    MaxValue:= 2000;
    MinValue:= -MaxValue;
    end;
  DCModalityBox.Items.CommaText:= Engines[UsedEngine].ModalityFilmList.GetModalityList;
  DCFilmTypeBox.Items.CommaText:= Engines[UsedEngine].ModalityFilmList.GetFilmTypeList;
  end;
end; {~populatedoseconvelist}


{10/04/2020 ===lazarus-implementation in messages tab===}
procedure TAnalyseForm.ExceptMessage(AMessage:String);
begin
SetMessageBar('=> exception message: '+AMessage);
end; {~exceptmessage}


{13/08/2015 Cycle through multiple scan file}
{17/08/2015 repaired cycling wMultiScanNr}
{22/12/2016 Histogram range}
{06/06/2017 if (PageControl.ActivePage<>ConfigurationTab)...}
{15/12/2017 wMultiScanLooping}
{16/04/2020 if [removed:(ViewValuesItem.Checked) and] (Key in [VK_LEFT,VK_RIGHT]) then}
{16/04/2020 minor adaptations for port to Lazarus}
{08/09/2020 reviewed autolooping logics}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
{15/09/2020 historylist}
{16/09/2020 SelectEngine}
{04/05/2021 added paging support for multiple-single-scan files}
{01/07/2021 check OnDataReadBusy}
procedure TAnalyseForm.FormKeyDown(Sender :TObject;
                                   var Key:Word;
                                   AShift :TShiftState);
const PlusKeys  = [Ord('='),Ord('+'),VK_ADD,VK_NEXT,VK_F4];                     //vk_next=PageDown
      MinusKeys = [Ord('-'),VK_SUBTRACT,VK_PRIOR,VK_F3];                        //vk_prior=PageUp
      HomeKeys  = [VK_HOME];
var i,j,k: Integer;
    c    : Char;
    b    : Boolean;
begin
b:= (PageControl.ActivePage=ConfigurationTab) or (PageControl.ActivePage=AliasTab) or
    (PageControl.ActivePage=SettingsTab)      or (PageControl.ActivePage=AdvancedSettingsTab);
k:= 0;                                                                          //limit autolooping
{$IFDEF SelfTest}
if (SelfTestLevel>0) and (Key in [VK_ESCAPE,VK_F5,VK_F6,VK_F7,VK_F8]) then
  case Key of
    VK_ESCAPE: SelftestLevel:= 0;
    VK_F5    : SelftestLevel:= 1; {halt at failed test}
    VK_F6    : SelftestLevel:= 3-(SelftestLevel mod 2); {step mode}
    VK_F7    : SelftestLevel:= 8; {fast}
    VK_F8    : SelftestLevel:= 9; {fast, countinuous}
   end
else
{$ENDIF}
if (not((Key=VK_ESCAPE) and Engines[UsedEngine].StopProcessing)) or b then
  inherited;
if not b then
  begin
  if (Key in PlusKeys+MinusKeys+HomeKeys) and (not OnDataReadBusy) then         //-------------next scan-----------
    begin
    if AShift=[ssCtrl] then
      begin
      if Length(Engines)>1 then
        UsedEngine:= SelectEngine(UsedEngine,ifthen(Key in PlusKeys,1,-1))
      else
        SetMessageBar('No history available');
      end
    else with Engines[UsedEngine],wSource[dsMeasured] do
      if twOriginalFormat in twcMultiFiles then                                 //support
        begin
        b                := True;
        wMultiScanLooping:= not (ssAlt in AShift);
        while b and
              (( (Key in PlusKeys )          and (wMultiScanNr<=wMultiScanMax) ) or
               ( (Key in MinusKeys+HomeKeys) and (wMultiScanNr>=1            ) )    ) do
          begin
          wMultiScanStep:= ifthen(Key in PlusKeys,1,-1);
          i             := wMultiScanNr;
          wMultiScanNr  := ifthen(Key=VK_HOME,1,wMultiScanNr+wMultiScanStep);
          if wMultiScanMax>1 then
            wMultiScanNr:= Succ((wMultiScanNr+wMultiScanMax-1) mod wMultiScanMax);
          b:= b and (ssAlt in AShift) and (wMultiScanNr<>1);
          j:= wMultiScanNr;
          if (i<>wMultiScanNr) then
            begin
            Engines[UsedEngine].Freeze:= False;                                 //otherwise nothing happens
            Reload(Sender,not b);                                               //due to looping wMultiScanNr might be changed
            end;
          b:= b and (j=wMultiScanNr) and (wMultiScanNr<>wMultiScanMax);         //extra safety precaution
          if not ScanInclusionTest(UsedEngine) then
            begin
            Inc(k);
            b:= (k<wMultiScanMax);
            end;
          end;
        end
    else if (Engines[UsedEngine].wMultiScanMax=1) and Engines[UsedEngine].FindMoreData then
      begin
      UsedDataTopLine:= ifthen(Key in PlusKeys,Engines[UsedEngine].Parser.CurrentLineNumber,0);
      Reload(Sender);
      end
    end {key in pluskeys+minuskeys+homekeys}
  else if Engines[UsedEngine].IsValid then
    begin
    i:= ifthen(Key in [VK_LEFT,VK_DOWN],-1,ifthen(Key in [VK_RIGHT,VK_UP],1,0));
    if PageControl.ActivePage=AnalysisTab then                                  //set AutoFocus true for DataPlot to avoid trigger of PageControlRequestChange
      begin
      if ssShift in AShift then i:= i* 2;
      if ssCtrl  in AShift then i:= i*ifthen((Engines[UsedEngine].ScanType in twcVertScans) and (AppliedFieldClass=fcElectron),1,5)*Round(GetDisplayedPositionScale);
      if ssAlt   in AShift then i:= i*20;
      if Key in [VK_LEFT,VK_RIGHT] then                                         //-------------shift-----------
        begin
        CursorPos_cm:= Round((CursorPos_cm/ManualShiftStep_cm.Value+i)/i)*i*ManualShiftStep_cm.Value;
        Key         := 0;                                                       //block any further processing of the key
        AShift      := [];
        if (not PlotPending) then
          begin
          PlotPending:= True;
          PlotCursor(Sender);
          Sleep(50);
          PlotPending:= False;
          end;
        end
      else if {DataPlot.Zoomed and} (Key in [VK_DOWN,VK_UP]) then               //-------------zoomrange-----------
        begin
        if Engines[UsedEngine].wSource[dsCalculated].twIsGamma then
          HistogramLimit_num.Value:= Math.Max(0.5,HistogramLimit_num.Value+i/2)
        else
          ZoomRange:= Max(((ZoomRange-1)+i/100)+1,1.005);
        AutoZoom;
        end;
      end {analysistab}
    else if PageControl.ActivePage=HistogramTab then                            //set AutoFocus true for HistoGramPlot to avoid trigger of PageControlRequestChange
      begin
      if i<>0 then
        begin
        HistogramLimit_num.Value:= Math.Max(0.5,HistogramLimit_num.Value+i/4);
        ShowHistogram;
        end;
      c:= Upcase(Char(Key));
      i:= Pred(Pos(c,DefXsourceSelectors));
      if i>=0 then
        begin
        HistogramSource:= twcDataSource(i);
        ShowHistogram;
        end;
      end {histogramtab}
    end; {valid}
  end; {with}
end; {~formkeydown}


{$push}{$warn 5024 off:parameters not used}{$warn 5057 off:Msg not initialised}
{check with onkeyup event when there are no more keys to be processed
BOOL PeekMessage(
  LPMSG lpMsg,
  HWND  hWnd,
  UINT  wMsgFilterMin,
  UINT  wMsgFilterMax,
  UINT  wRemoveMsg);

lpMsg: A pointer to an MSG structure that receives message information.

hWnd: A handle to the window whose messages are to be retrieved.
  The window must belong to the current thread.
  If hWnd is NULL, PeekMessage retrieves messages for any window that belongs to the current thread,
  and any messages on the current thread's message queue whose hwnd value is NULL (see the MSG structure).
  Therefore if hWnd is NULL, both window messages and thread messages are processed.

  If hWnd is -1, PeekMessage retrieves only messages on the current thread's message queue whose hwnd value is NULL,
  that is, thread messages as posted by PostMessage (when the hWnd parameter is NULL) or PostThreadMessage.}
{17/01/2018}
procedure TAnalyseForm.FormKeyUp(Sender :TObject;
                                 var Key:Word);
var {$IFDEF Windows}
     Msg: TMSG;
    {$ELSE}
     Msg: LCLType.TMSG;
    {$ENDIF}
begin
if not (PeekMessage(Msg,0,LM_KEYFIRST,LM_KEYLAST,PM_NOREMOVE) or FKeyboardReady) then //LM_KEYFIRST/LAST has same value as WM_KEYFIRST/LAST
  begin
  FKeyboardReady:= True;
  OnDataRead(Sender);
  end;
end; {~formkeyup}
{$pop}


{=> OnKeyPress}
{06/01/2016 also pointseries are handled now}
{24/04/2020 ---small adaptations for fpc---}
{14/09/2020 Wellhofer changed to Engines[UsedEngine]}
procedure TAnalyseForm.FormKeyPress(Sender :TObject;
                                    var Key:Char);
var cm   : Single;
    i,j,w: Integer;
begin
if (not OnDataReadBusy) and (PageControl.ActivePage=AnalysisTab) then
  begin
  if Key in [DefShiftLeft,DefShiftRight] then
    begin
    if Engines[UsedEngine].IsValid and (not OnDataReadBusy) then
      begin
      OnDataReadBusy:= True;
      ShiftStepCount:= ifthen(Key=PrevKey,Succ(ShiftStepCount),2);
      cm            := ifthen(Key=PrevKey,Power(10,Trunc(Log10(ShiftStepCount div 2))),1)*ifthen(Key='<',-1,1)*ManualShiftStep_cm.Value;
      Engines[UsedEngine].Shift(cm);
      OnDataReadBusy:= False;
      OndataRead(Sender);
      SetMessageBar(Format('shift: %0.*f %s',[ifthen(ViewMilliMetersItem.Checked,1,2),GetDisplayedPositionScale(Engines[UsedEngine].wSource[dsMeasured].twShift_cm),GetPositionUnitsStg]));
      PrevKey:= Key;
      end;
    end
  else if Key=#32 then                                                          //spacebar selects series
    begin
    if not assigned(SelectedSeries) then
      SelectedSeries:= DataPlot.Series[0];
    i:= DataPlot.SeriesCount;                                                   //find selectedseries in SeriesList...
    repeat
      Dec(i);
    until (i=0) or (DataPlot.Series[i].Name=SelectedSeries.Name);
    if i=0 then
      i:= DataPlot.SeriesCount;
    repeat                                                                      //... and get next one
      Dec(i);
    until (i=0) or DataPlot.Series[i].Active;
    SelectedSeries:= DataPlot.Series[i];
    if DataPlot.Series[i].Active then
      OnSeriesSelected;
    end
  else if Key in ['(',')'] then with PlotSeries[pMeasured],Pointer do           //rounded brackets change points size of measured curve
    begin
    i                     := HorizSize+ifthen(Key=')',1,-1);                    //horizsize must be >0
    ShowPoints            := i>0;
    ViewPointsItem.Checked:= i>0;
    if i>0 then
      begin
      HorizSize           := i;
      VertSize            := Max(1,i div 2);
      end;
    end
  else if (Key in ['{','}']) and assigned(SelectedSeries) and (SelectedSeries is TLineSeries) then
    begin                                                                       //curly brackets wok on any series
    with SelectedSeries as TLineSeries do
      begin
      LinePen.Width:= Max(1,LinePen.Width+ifthen(Key='}',1,-1));                //width must be >0
      w            := LinePen.Width;
      end;
    j:= Pred(Length(SelectedSeries.Name));                                      //copy to similar series
    for i:= 0 to Pred(DataPlot.SeriesCount) do
       if DataPlot.Series[i] is TLineSeries then
         with DataPlot.Series[i] as TLineSeries do
           begin
           if Copy(Name,0,j)=Copy(SelectedSeries.Name,0,j) then
             LinePen.Width:= w;
           end;
    end; {curly brackets}
  end;
inherited;
end; {~formkeypress}


{$IFDEF PRELOAD}
{08/05/2020 and assigned(PreloadStream)}
procedure TAnalyseForm.PreloadTransfer(Sender:TObject);
begin
if (RawDataEditor.Lines.Count=0) and assigned(PreloadStream) and (PreloadStream.Size>MinClipBoardBytes) then
  begin                                                                         //preserve data created through other methods, and not yet cleared
  PreloadStream.Position:= 0;                                                   //it seems to be critical to set the stream position to zero
  RawDataEditor.Lines.LoadFromStream(PreloadStream);                            //this might take a long time...
  RawDataEditor.Modified:= False;                                               //the data are already processed so modified should be false
  end;
end; {~preloadtransfer}
{$ENDIF}


procedure TAnalyseForm.FileConvFileNameDisplay(AEdit:TDirectoryEdit;
                                               AName:TFileName);
begin
AEdit.Text:= AName;
AEdit.Refresh;
end; {~fileconvfilenamedisplay}


function TAnalyseForm.FileConvGetFileExt(AListBox:TlistBox): String;
var s:string;
begin
Result:= '.txt';
with AListBox do if Items.Count>0 then
  begin
  s:= Items[Max(0,Min(ItemIndex,Items.Count-1))];
  if Length(s)>4 then
    Result:= Copy(s,Succ(Pos('*',s)),4);
  end;
end; {~fileconvgetfilext}


{09/05/2024 avoid FreeAndNil}
procedure TAnalyseForm.RunAboutBox(Sender:TObject);
begin
AboutBox                      := TAboutBox.Create(Self);                        //owner is TAnalyseForm
AboutBox.Left                 := Left;
AboutBox.Top                  := Top;
Aboutbox.Color                := UIColorPanel.ButtonColor;
AboutBox.Constraints.MaxHeight:= Height;
AboutBox.Constraints.MinHeight:= Height;
AboutBox.Constraints.MaxWidth := Width;
AboutBox.Constraints.MinWidth := Width;
AboutBox.ShowModal;
AboutBox.Free;
end; {~runaboutbox}


{$IFDEF SelfTest}
{21/07/2015 Removed test 29-30.}
{11/12/2015 static LeffSeries and ReffSeries replaced with InFieldIndicators}
{17/12/2015 Sigmoid model}
{19/12/2015 emptytest 37-39 removed}
{25/07/2016 added SetWellhoferValues to proceedure CheckMenuItem}
{10/09/2016 try..except}
{08/11/3027 checking ModRec}
{14/01/2018 adapted for CxBlocks}
{03/12/2018 adapted for autoscaling}
{04/09/2020 updated}
{16/03/2021 updated}
procedure TAnalyseForm.SelfTest(Sender:TObject);
const tNormal =1;
      tDefault=3;
var hev,tdmm,tdbf,tgcp,tgdp,tgdc,tgsf,tmnp,tssd,v: twcFloatType;
    tmrf,tmsf                                    : Integer;
    tAutoScale,tGenericToElectron                : Boolean;
    S                                            : TMemoryStream;
    smod,sfilm,sbase,scombi,hftxt,hmtxt,sX10,Stg : String;
    i,iX10,Fail,Pass,tdla                        : Integer;
    ModRec                                       : TModalityNorm;
    OrgNorm                                      : twcNormalisation;


  procedure AddMessage(AMessage:String;
                       Secs    :Byte=tDefault;
                       PreStg  :String='-> ');
  begin
  if SelftestLevel>0 then
    begin
    SetMessageBar(PreStg+AMessage);
    repeat
      if Secs>0 then
        begin {selftestlevel is changed dynamically by user}
        WaitLoop(1000 div Max(1,SelfTestLevel));
        if SelftestLevel=3 then
          Secs:= 1
        else
          Dec(Secs);
        end;
    until Secs=0;
   if SelftestLevel=2 then
     SelftestLevel:= 3;
    end;
  end;

  function LoadSelftestFile(AFile:String;
                            Extra:String='';
                            Secs : Byte=0): Boolean;
  var s: String;
  begin
  AddMessage(Format('Loading "%s" %s',[AFile,Extra]),Secs,'<- ');
  try
    if SelftestLevel=0 then
      Result:= False
    else
      if ExtractFileDrive(AFile)='' then
        begin
        s:= CommonAppData;
        if FileExists(s+AFile) then
          AFile:= s+AFile
        else
          AFile:= ExtractFilePath(ParamStr(0))+AFile;
        end;
      Result:= DataFileOpen(AFile) and  Engines[UsedEngine].Analyse;
   except
    Result:= False;
   end;
  end;

  function B2Stg(ABool:Boolean): String;
  begin
  Result:= ifthen(ABool,'C','Unc')+'hecked';
  end;

  procedure CheckMenuItem(AItem :TMenuItem;
                          AValue:Boolean;
                          Secs  :Byte=0);
  begin
  AddMessage(Format('"%s" => %s',[CleanUpCaption(AItem.Caption),B2Stg(AValue)]),Secs,'  --> ');
  AItem.Checked:= AValue;
  SetWellhoferValues(AItem);
  end;

  function TestResult(ABool   :Boolean;
                      AMessage:String;
                      Secs    :Byte=1;
                      Fatal   :Boolean=False): Boolean;
  begin
  if SelftestLevel=0 then
    ABool:= False
  else if ABool then
    begin
    Inc(Pass);
    AddMessage(Format('%s: passed [%d/%d]   <Esc>',[AMessage,Pass,Pass+Fail]),Secs);
    end
  else
    begin
    Inc(Fail);
    if SelftestLevel<9 then
      SelfTestLevel:= 3;
    AddMessage(Format('%s: failed [%d/%d]  <Esc%s',[AMessage,Fail,Pass+Fail,ifthen(Fatal,'> (fatal error)','|F5=continue|F6=step|F7=fast|F8=fast,no stop>')]),5);
    if Fatal then
      SelftestLevel:= 0;
    end;
  Result:= ABool;
  end;

  function FloatResult(AValue,AReference,MaxDif:twcFloatType;
                       AMessage                :String;
                       Secs                    :Byte=1): Boolean;   overload;
  var d: integer;
  begin
  if AValue=0 then d:= 3
  else             d:= Max(0,3-Trunc(Log10(Abs(AValue))));
  Result:= TestResult(abs(AValue-Areference)<MaxDif,Format('%s=%0.*f',[AMessage,d,AValue]),Secs);
  end;

  function FloatResult(AValue           :String;
                       AReference,MaxDif:twcFloatType;
                       AMessage         :String;
                       Secs             :Byte=1): Boolean;   overload;
  var f: twcFloatType;
  begin
  try
    if Avalue= '-' then f:= 0
    else                f:= StrToFloat(AValue.Trim('%'));                       //TStringHelper function
    Result:= FloatResult(f,AReference,MaxDif,AMessage,Secs);
   except
    Result:= TestResult(False,Format('%s could not be evaluated',[AMessage]),Secs);
   end;
  end;

  function FloatResult(ABlock           :CxBlock;
                       AReference,MaxDif:twcFloatType;
                       AMessage         :String;
                       Secs             :Byte=1): Boolean;   overload;
  begin
  Result:= FloatResult(ABlock[CxValue].Caption,AReference,MaxDif,AMessage,Secs);
  end;

  procedure AddEmptyTest(n:byte=1);
  begin
  while n>0 do
    begin
    TestResult(True,'Reserved for future use',0);
    Dec(n);
    end;
  end;

  function LocateModality(AModality,AFilmType:String): Integer;
  var i: Integer;
      b: Boolean;
  begin
  with ModListGrid do
    begin
    i:= RowCount;
    if i>0 then
      repeat
        Dec(i);
        b:= (Cells[DefDoseGridModCol,i]=AModality) and
            ((AFilmType='') or (Cells[DefDoseGridFilmCol,i]=AFilmType));
      until b or (i=0)
    else b:= false;
    end;
  if b then Result:= i
  else      Result:= -1;
  end;

  procedure DeleteModality(ARow:Integer);
  begin
  if ARow>=0 then
    begin
    ModListGrid.Row:= ARow;
    ModListDelClick(Self);
    end;
  end;

begin
inherited;
RePaint;
S:= TMemoryStream.Create;
ConfigSave(S);
Selftestlevel           := 1;
ModRec                  := nil;
sbase                   := '';
scombi                  := '';
Fail                    := 0;
Pass                    := 0;
tdmm                    := twcDeriveMinMax;
tdbf                    := twcDeriveBinFraction;
tdla                    := twcDeriveLookAhead;
tgcp                    := twcGammaCutoffPercent;
tgdp                    := twcGammaDosePercBase;
tgdc                    := twcGammaDistCmBase;
tgsf                    := twcGammaSearchMaxFactor;
tmrf                    := twcMatchRangeDivider;
tmsf                    := twcMatchStepsNumber;
tmnp                    := twcMatchNormDeltaPercent;
tssd                    := twcDefaultSSDcm[fcStandard];
tAutoScale              := ProcessAutoscalingItem.Checked;
tGenericToElectron      := MeasGenericToElectronItem.Checked;
twcDeriveMinMax         := 0.9;
twcDeriveBinFraction    := 0.25;
twcDeriveLookAhead      := 3;
twcGammaCutoffPercent   := 5;     {%}
twcGammaDosePercBase    := 0.5;   {%}
twcGammaDistCmBase      := 0.1;   {cm}
twcGammaSearchMaxFactor:= 5;
twcMatchRangeDivider    := 2;
twcMatchStepsNumber     := 6;
twcMatchNormDeltaPercent:= 2;
smod                    := 'X6.00';
sfilm                   := 'selftest';
SetBasicDefaults;
UImodeChange(Self);
for i:= 0 to Pred(Length(UseDoseConvTable)) do with UseDoseConvTable[i] do
  begin
  with DCBgBox do
    begin
    Tag    := ifthen(Checked,1,0);
    Checked:= False;
    end;
  with DCDoseBox do
    begin
    Tag    := ifthen(Checked,1,0);
    Checked:= False;
    end;
  end;
GetWellhoferValues;
with UseDoseConvTable[0] do
  begin
  hev                   := DCEdit        .Value;
  hftxt                 := DCFilmTypeBox.Text;
  hmtxt                 := DCModalityBox.Text;
  DCDoseBox     .Checked:= True;
  DCBgBox       .Checked:= True;
  DCEdit        .Value  := 22;
  DCFilmTypeBox.Text    := sfilm;
  DCModalityBox.Text    := smod;
  end;
PageControl.ActivePage:= AnalysisTab;
CheckMenuItem(MeasZeroStepsItem,True,0);
AddMessage('Look for messages here');
ResampleGrid_mm       .Value     :=  0;
XHpenumbra_perc       .Value     := 80;
XLpenumbra_perc       .Value     := 20;
FilterWidth_mm        .Value     :=  6;
CalcWidth_mm          .Value     :=  2;
Engines[UsedEngine]   .LogLevel  :=  3;
ProcessAutoscalingItem.Checked   := True;
MeasGenericToElectronItem.Checked:= False;
SettingsTabExit(Self);
AdvancedSettingsTabExit(Self);
PDDfitCheckBox.Checked:= True;
if FileHistoryItem.Checked then
  begin
  FileHistoryItem.Checked:= False;
  HistoryListSizeClick(FileHistoryItem);
  end;
CheckMenuItem(MeasUsePDDfitModelItem       ,False);
CheckMenuItem(MeasInvertGTitem          ,True);
CheckMenuItem(MeasInvertABitem          ,False);
CheckMenuItem(MeasInvertUDitem          ,False);
CheckMenuItem(MeasRemapCoordinates      ,False);
CheckMenuItem(MeasSymCorrectItem        ,False);
CheckMenuItem(MeasUserDoseItem          ,False);
CheckMenuItem(ProcessSetTempRefItem     ,False);
CheckMenuItem(ViewReferenceItem         ,True );
CheckMenuItem(MeasZeroStepsItem         ,True );
CheckMenuItem(ViewValuesItem            ,True );
CheckMenuItem(MeasMissingPenumbraItem   ,True );
CheckMenuItem(MeasBadPenumbraItem       ,True );
CheckMenuItem(MeasOD2DoseConvItem       ,True );
CheckMenuItem(MeasBackgroundCorrItem    ,False);
CheckMenuItem(ViewZoomItem              ,True );
CheckMenuItem(ViewMillimetersItem       ,False);
CheckMenuItem(RefNormaliseItem          ,True );
CheckMenuItem(RefSymCorrectItem         ,True );
CheckMenuItem(RefAlignItem              ,True );
CheckMenuItem(MeasMove2OriginItem       ,False);
CheckMenuItem(MeasResampleItem          ,False);
CheckMenuItem(RefAutoLoadItem           ,True );
CheckMenuItem(MeasMirrorToBufferItem    ,False);
CheckMenuItem(RefUseDivideByItem        ,True );
CheckMenuItem(MeasGenericToPDDItem      ,False);
CheckMenuItem(MeasMove2OriginItem       ,False);
CheckMenuItem(FileIgnoreClipboardItem   ,False);
CheckMenuItem(MeasSDD2SSDItem           ,False);
CheckMenuItem(MeasScale2defaultSSDitem  ,False);
CheckMenuItem(FileHistoryItem           ,False);
SyncSetFFFpeak(CenterFFFTopModel);
OrgNorm                                           := Engines[UsedEngine].wNormalisation[fcStandard];
Engines[UsedEngine]    .wNormalisation[fcStandard]:= NormOnCenter;
Engines[UsedEngine]    .ShowWarning               := True;
ViewStandardPanelsetup .Checked                   := True;
AutoSetDecPointCheckBox.Checked                   := True;                      //some legacy reference files
AutoDecPointList       .Text                      := ',.';
SetDefaultPanel(Sender);                                                        //some test depend on specific cxblocks
ViewItems(Self);
AddEmptyTest(2);
LoadSelftestFile('selftest16_snc.snctxt','Sun Nuclear disk file text format');
CursorPos_cm:= -11;
PlotCursor(Sender);
TestResult(Engines[UsedEngine].wSource[dsReference].twValid,'Comma separated reference file loaded',1,True);  {1}
FloatResult(PlotValues[pCalculated].Caption,99.59,0.2,Format('Value at %0.1f cm: ',[CursorPos_cm]));           {2}
AddEmptyTest(1);                                                                                              {3}
LoadSelftestFile('selftest01_theoretical.txt','should fail on Decimal Separator detection',2);
AutoSetDecPointCheckBox.Checked:= False;                                        //some legacy reference files
TestResult(not Engines[UsedEngine].IsValid,'Decimal Separator detection');                                    {4}
ViewItems(Self);
CheckMenuItem(MeasZeroStepsItem,False);
LoadSelftestFile('selftest01_theoretical.txt','should fail on Zero Steps detection',2);
TestResult(Pos(LeftStr(twForIllegalScan,5),Engines[UsedEngine].LastMessage)>0,'Zero Steps detection');        {5}
PageControl.ActivePage:= ConfigurationTab;
ModListNormRadioButton.Checked:= True; {excutes event code}
ModListGridFill(Format('%s,0,100,0,0',[smod]),True);
i:= LocateModality(smod,'');
FloatResult(ModListGrid.Cells[Succ(DefDoseGridModCol),i],0,1,'Relative norm position for '+smod);   {6}
ModListFilmRadioButton.Checked:= True;
ModListGridFill(Format('%s,%s,0,0.667,0,0,0,10',[smod,sfilm]),True);
i:= LocateModality(smod,sfilm);
TestResult(ModListGrid.Cells[DefDoseGridFilmCol,i]=sfilm    ,'Temporary configuration data added'); {7}
PageControl.ActivePage:= AnalysisTab;
CheckMenuItem(MeasZeroStepsItem ,True );                                        //accept zero-steps on
CheckMenuItem(RefAutoLoadItem   ,True );
CheckMenuItem(RefUseDivideByItem,False);
CheckMenuItem(RefUseGammaItem   ,True );
GammaLocalDoseCheckBox.Checked:= True;
GammaDoseNorm_perc    .Value  := 1;
GammaDistNorm_mm      .Value  := 1;
SettingsTabExit(Self);
if LoadSelftestFile('selftest28_FFF.mcc') then                  {ref selftestx7i30_ssd090_d100m.txt  8}
  begin
  CursorPos_cm:= 5;
  PlotCursor(Sender);
  FloatResult(PlotValues[pCalculated].Caption,1.93,0.5,'Gamma at 5 cm',3);                          {9}
  end;
CheckMenuItem(RefUseGammaItem   ,False);
CheckMenuItem(RefUseDivideByItem,True );
if TestResult(LoadSelftestFile('selftest01_theoretical.txt'),'Theoretical profile in Wellhöfer text format') then {10}
  begin
  CheckMenuItem(ViewMeasuredItem  ,False);
  CheckMenuItem(ViewIndicatorsItem,False);
  ViewItems(Self);
  TestResult(PlotSeries[pMeasured].Active=False                                       ,'Measured off');         {11}
  TestResult((InFieldIndicators[twcLeft].Active or InFieldIndicators[twcRight].Active)=False,'Indicators off'); {12}
  with Engines[UsedEngine] do
    begin
    ResetValues;
    CopyCurve(wSource[dsMeasured],wSource[dsBuffer]);
    with wSource[dsMeasured] do for i:= twDataRange[ptFirst] to twDataRange[ptLast] do twData[i]:= i;
    CopyCurve(wSource[dsBuffer],wSource[dsMeasured]);
    end;
  CheckMenuItem(ViewCalculatedItem,False);
  CheckMenuItem(ViewMeasuredItem  ,True );
  CheckMenuItem(ViewBufferItem    ,True );
  ViewItems(Self);
  OnDataRead(Self);
  TestResult(PlotSeries[pCalculated].Active=False,'Calculated off');                               {14}
  TestResult(PlotSeries[pBuffer    ].Active=True ,'Buffer on');                                    {15}
  CheckMenuItem(ViewBufferItem    ,False);
  CheckMenuItem(ViewCalculatedItem,True );
  CheckMenuItem(ViewIndicatorsItem,True );
  ViewItems(Self);
  CursorPos_cm:= -20;
  PlotCursor(Sender);
  FloatResult(Engines[UsedEngine].GetPenumbraValue(dsMeasured,d50,twcLeft ),-20,0.1,'Left');       {16}
  FloatResult(Engines[UsedEngine].GetPenumbraValue(dsMeasured,d50,twcRight), 20,0.1,'Right');      {17}
  FloatResult(CxResults[5][1]                                    ,1.2  ,0.1,'Left 80-20');         {18}
  FloatResult(CxResults[6][1]                                    ,0,0.1,'Right 80-20');            {19}
  TestResult(PlotValues[pMeasured].Caption<>'-'                  ,'Cursor values visible');        {20}
  if FloatResult(PlotValues[pMeasured].Caption                   ,50,0.001 ,'Left value') then     {21}
    begin
    PageControl.ActivePage:= SettingsTab;
    AddMessage('Setting Xpenubra High value to 85',0);
    XHpenumbra_perc.SetFocus;
    XHpenumbra_perc.Value:= 85;
    XLpenumbra_perc.SetFocus;
    XLpenumbra_perc.Value:= 15;
    PageControl.ActivePage:= AnalysisTab;
    ReadEditor(Self);
    FloatResult(Engines[UsedEngine].GetPenumbraWidth(dsMeasured,twcLeft),1.4,0.0001,'Left 85-15'); {22}
    end;
  EdgeDetectionCheckBox.Checked          := False;
  Engines[UsedEngine].ResampleGridSize_cm:= 0.2;
  CheckMenuItem(MeasResampleItem,True);
  ReadEditor(Self);
  FloatResult(Engines[UsedEngine].GetPenumbraValue(dsMeasured,d50,twcRight),19.2,0.2,'Right, resampled'); {23}
  CheckMenuItem(MeasResampleItem,False);
  EdgeDetectionCheckBox.Checked:= True;
  end;
AddEmptyTest(2);                                                                                   {24}
with Engines[UsedEngine] do ModalityNormList.FindModData(smod,wSource[dsMeasured].twSetFieldType,Linac,ModRec);
TestResult(Assigned(ModRec),'Obtaining modality data for '+smod);                                  {26}
if TestResult(LoadSelftestFile('selftest15_missing_penumbra.txt'),'Missing penumbra') then         {27}
  begin
  Engines[UsedEngine].Parser.CurrentLine:= PlotDates[pReference].Caption;
  if Engines[UsedEngine].Parser.Search(DefAnnotShift,False,True) then
    v:= Abs(Engines[UsedEngine].Parser.NextFloat)
  else
    v:= 1;
  FloatResult(v,0.09,0.10,'Alignment by matching');                                                {28}
  FloatResult(CxResults[0][1],0.0,0.05,'Center position');                                         {29}
  ViewItems(Self);
  end;
AddEmptyTest(4);                                                                                   {31}
if TestResult(LoadSelftestFile('selftest01_asym.txt'),'Asymmetric') then
  begin
  FloatResult(CxResults[3][0],3.4,0.1,'Flatness(%)');                                              {35}
  CheckMenuItem(MeasSymCorrectItem,True,0);
  Ft_SymCorrCheckBox[fcStandard,dsMeasured].Checked:= True;
  ReadEditor(Self);
  FloatResult(CxResults[3][0],0.1,0.1,'Flatness(%)');                                              {36}
  end
else CheckMenuItem(MeasSymCorrectItem,True);
ViewItems(Self);
if TestResult(LoadSelftestFile(Engines[UsedEngine].ReferenceDirectory+'selftestx6c40_ssd100_d050m.txt'),'Reference available') then
  begin
  FloatResult(100*Engines[UsedEngine].wSource[dsCalculated].twFlatness,0.05,0.05,'Flatness(%)'); {38}
  CheckMenuItem(MeasSymCorrectItem,False,0);
  Ft_SymCorrCheckBox[fcStandard,dsMeasured].Checked:= False;
  CheckMenuItem(RefAutoLoadItem   ,False);
  ViewItems(Self);
  if TestResult(LoadSelftestFile('selftest02_real.txt'),'Real profile in Wellhöfer text format') then
    begin
    TestResult(PlotSeries[pReference].Active=False,'Reference off');            {40}
    FloatResult(CxResults[1][0],2.02,0.1,'Normalized to x');                    {41}
    CheckMenuItem(RefAutoLoadItem           ,True);
    Engines[UsedEngine].wNormalisation[fcStandard]:= NormOnOrigin;
    ViewItems(Self);
    ReadEditor(Self);
    TestResult(PlotSeries[pReference].Active=True,'Reference on');
    TestResult((Pos('ilter',PlotDates[pCalculated].Caption)>0) and
               (Pos('/',PlotDates[pCalculated].Caption)>0),'Calculated is division');
    FloatResult(CxResults[1][0],0.0,0.01,'Normalized to x');
    Engines[UsedEngine].wNormalisation[fcStandard]:= NormOnCenter;
    CheckMenuItem(ViewMeasuredItem ,False);
    CheckMenuItem(ViewReferenceItem,False);
    CheckMenuItem(ViewBufferItem   ,True );
    ReadEditor(Self);
    FloatResult(Engines[UsedEngine].GetPenumbraValue(dsMeasured,d50        ,twcLeft ),-18.91,0.02,'Left, d50');              {45}
    FloatResult(Engines[UsedEngine].GetPenumbraValue(dsMeasured,dDerivative,twcLeft ),-18.85,0.02,'Left, Edge, derivative'); {46}
    ReadEditor(Self);
    FloatResult(Engines[UsedEngine].GetPenumbraValue(dsMeasured,dDerivative,twcLeft ),-18.88,0.05,'Left, Edge sigmoïd');     {47}
    CheckMenuItem(ViewMeasuredItem ,True );
    CheckMenuItem(ViewReferenceItem,True );
    CheckMenuItem(ViewBufferItem   ,False);
    end
  else AddEmptyTest(2);
  end
else
CheckMenuItem(MeasSymCorrectItem,False,0    );
CheckMenuItem(ViewZoomItem,False      ,0    );
PageControl.ActivePage:= AnalysisTab;
if TestResult(LoadSelftestFile('selftest03_pdd_theoretical.txt'),'PDD') then {48}
  with Engines[UsedEngine],wSource[dsMeasured].twBeamInfo do
    begin
    FloatResult(CxResults[2][0],104.2,10,'Scaled on maximum');                {49}
    TestResult(CxResults[3][1,CxTitle].Caption[1]='P','text is PDD20/10');    {50}
    FloatResult(CxResults[3][1],0.582,0.002,'PDD20/10');                      {51}
    end;
AddMessage('Setting absolute scaling point to 10 cm...',0);
CheckMenuItem(MeasUsePDDfitModelItem,True);
ModRec.NormRec.Depth[True]:= 10; {should be set befor file read because analysis is done immediately}
AutoSetDecPointCheckBox.Checked:= True;                                         //some legacy reference files
if TestResult(LoadSelftestFile('selftest03_pdd.txt'),'PDD') then              {52}
  with Engines[UsedEngine],wSource[dsMeasured].twBeamInfo do
    begin
    Stg:= ' axis maximum';
    TestResult(CxResults[1][1][CxTitle].Caption[1]='R','text is RDD10');      {53}
    FloatResult(CxResults[1][1],100,0.01,'RDD10');                            {54}
    AddMessage('Setting relative scaling point to 10 cm...',0);
    ModRec.NormRec.Depth[False]:= 10;
    ReadEditor(Self);
    FloatResult(DataPlot.LeftAxis.Range.Max  ,156,6,'Left'  +Stg);            {55}
    FloatResult(DataPlot.BottomAxis.Range.Max, 40,9,'Bottom'+Stg);            {56}
    end;
AddEmptyTest(2);
PDDfitCheckBox.Checked:= False;
TestResult(LoadSelftestFile('selftest08_scanditronic.wda'),'Scanditronix binary format (wda)');      {59}
AddEmptyTest;
if TestResult(LoadSelftestFile('selftest10_generic.txt'  ),'Generic profile') then
  begin
  CursorPos_cm:= -7;
  PlotCursor(Sender);
  AddMessage(Format('Filter=%0.1f mm, Calculation width=%0.1f mm',[FilterWidth_mm.Value,CalcWidth_mm.Value]),tNormal);
  FloatResult(PlotValues[pMeasured  ].Caption,99.6,0.1,'Measured interpolated at -7');               {62}
  FloatResult(PlotValues[pCalculated].Caption,99.9,0.1,'Calculated interpolated at -7');             {63}
  CalcWidth_mm.Value:= 0;
  SettingsTabExit(Self);
  ReadEditor(Self);
  CursorPos_cm:= -7;
  PlotCursor(Sender);
  AddMessage(Format('Filter=%0.1f mm, Calculation width=%0.1f mm',[FilterWidth_mm.Value,CalcWidth_mm.Value]),tNormal);
  FloatResult(PlotValues[pMeasured  ].Caption,99   ,0.01  ,'Measured real at -7');                    {64}
  end;
CheckMenuItem(ViewZoomItem,False,0);
if TestResult(LoadSelftestFile('selftest04_xio.txt'       ),'XiO profile') then                       {65}
  FloatResult(CxResults[2][0],110,5,'Toe of wedge on G-side');                                        {66}
if TestResult(LoadSelftestFile('selftest12_xio_pdd.txt'   ),'XiO depth dose') then                    {67}
  FloatResult(CxResults[0][1],2,1                           ,'Dmax');                                 {68}
if TestResult(LoadSelftestFile('selftest13_monaco_pdd.txt'),'Monaco depth dose') then                 {69}
  FloatResult(CxResults[0][1],2,1                           ,'Dmax');                                 {70}
TestResult(LoadSelftestFile('selftest06_pips.txt'      ),'Pips Pro profile');                         {71}
if TestResult(LoadSelftestFile('selftest07_scanditronic.wtx'),'Scanditronix text format (.wtx)') then {72}
  begin
  FloatResult(CxResults[0][1],-0.02,0.1,'Center of field');                                           {73}
  CheckMenuItem(MeasMove2OriginItem,True,0);
  ReadEditor(Self);
  AddEmptyTest;                                                                                       {74}
  CheckMenuItem(MeasMove2OriginItem,False,0);
  end;
TestResult(LoadSelftestFile('selftest09_schuster.txt'),'Schuster profile');                           {75}
with Engines[UsedEngine] do
  begin
  if TestResult(LoadSelftestFile('selftest05_hdf.txt'),'HDF profile') then                            {76}
    begin
    AddEmptyTest(2);                                                                                  {78}
    CursorPos_cm:= 24.33;
    PlotCursor(Sender);
    FloatResult(PlotValues[pMeasured].Caption,20,20,Format('Measured at %0.2f cm',[CursorPos_cm]));    {79}
    FloatResult(CxResults[0][0],1830,20,'Normalisation value');                                       {80}
    CursorPos_cm:= 15.33;
    PlotCursor(Sender);
    LocalPeakClick(MeasLocalPeakItem);
    FloatResult(CxResults[0][1],15.39,0.3,'Center local peak');                                       {81}
    end;
  end;
if TestResult(LoadSelftestFile('selftest11_18MeV.txt'),'Electron depth dose') then
  FloatResult(CxResults[0][1],3.39,0.1,'Dmax');  {83}
if TestResult(LoadSelftestFile('selftest14_wedge_across.wtx'),'Wedge across profile') then
  FloatResult(CxResults[3][0],4.8,0.2,'Flatness(%)');                                                 {85}
CheckMenuItem(RefAutoLoadItem,True);
CheckMenuItem(ViewZoomItem   ,True);
CheckMenuItem(ViewBufferItem ,True);
if TestResult(LoadSelftestFile('selftest14_wedge.wtx'),'Wedge profile') then                          {86}
  begin
//  Engines[UsedEngine].Derive(0,dsMeasured,dsBuffer);
  ViewItems(Self);
  CursorPos_cm:= Engines[UsedEngine].wSource[dsMeasured].twLevelPos[dDerivative].Limit[twcLeft].CalcPos;
  PlotCursor(Sender);
  FloatResult(CursorPos_cm,-15.77,0.2,'Left edge');
  FloatResult(PlotValues[pReference].Caption,165,70,Format('Reference at %0.2f cm',[CursorPos_cm]));   {88}
  FloatResult(PlotValues[pBuffer   ].Caption,150,90,Format('Derived at %0.2f cm',[CursorPos_cm]));     {89}
  CursorPos_cm:= Engines[UsedEngine].wSource[dsMeasured].twLevelPos[dDerivative].Limit[twcRight].CalcPos;
  PlotCursor(Sender);
  FloatResult(CursorPos_cm,15.65,0.2,'Right edge');                                                    {90}
  FloatResult(PlotValues[pBuffer].Caption,-7,5,Format('Derived at %0.2f cm',[CursorPos_cm]));          {91}
  AddEmptyTest(1);                                                                                    {92}
  CursorPos_cm:= -5;
  PlotCursor(Sender);
  FloatResult(PlotValues[pMeasured].Caption,141,5,'Measured at -5 cm');                               {93}
  CheckMenuItem(MeasMirrorItem,True);
  OnDataRead(Sender);
  CursorPos_cm:= 5;
  PlotCursor(Sender);
  FloatResult(PlotValues[pMeasured].Caption,141,5,'Mirrored at +5 cm');                               {94}
  CheckMenuItem(MeasMirrorItem,False);
  end;
AddEmptyTest(2);                                                                                      {95}
CheckMenuItem(RefUseGammaItem,True);
iX10:= Engines[UsedEngine].ModalityFilmList.FindModData('X10.0','');
if iX10>0 then
  begin
  sX10:= Engines[UsedEngine].ModalityFilmList.CommaText[iX10];
  Engines[UsedEngine].ModalityFilmList.DelModData('X10.0','');
  end
else
  sX10:= '';
if TestResult(LoadSelftestFile('selftest16_snc_clipboard_pdd.txt'),'SNC clipboard profile') then      {97}
  begin  {LoadReference->references\selftest9x10d1010_ssd100.txt}
  OnDataRead(Sender);
  CursorPos_cm:= 10;
  PlotCursor(Sender);
  FloatResult(ifthen(Engines[UsedEngine].wSource[dsCalculated].twIsGamma,
                     PlotValues[pCalculated].Caption,'0'),0.4,0.5,'Gamma at 10 cm');                  {98}
  end;
if iX10>0 then
  Engines[UsedEngine].ModalityFilmList.AddModData(sX10);
CheckMenuItem(RefUseGammaItem,False);
Engines[UsedEngine].wMultiScanNr:= 1;
if TestResult(LoadSelftestFile('selftest19_PTW.mcc'),'mcc') then                                      {99}
  begin
  for i:= 1 to 3 do
    begin
    Inc(Engines[UsedEngine].wMultiScanNr);
    Reload(Sender);
    end;
  FloatResult(Engines[UsedEngine].wSource[dsMeasured].twWidth_cm,14.22,0.2,'Double wegde FWHM');      {100}
  end;
TestResult(LoadSelftestFile('selftest17_OmniPro_v7.txt'),'OmniPro-Accept v7x');                      {101}
TestResult(LoadSelftestFile('selftest18_RFA300.asc'),'RFA300'); {102}
TestResult(LoadSelftestFile('selftest20_OmniPro_v74.txt'),'OmniPro-Accept v74');                     {103}
TestResult(LoadSelftestFile('selftest20_OmniPro_v74_pdd.txt'),'OmniPro-Accept v74');                 {104}
if TestResult(LoadSelftestFile('selftest20_OmniPro_v74_PM.txt'),'OmniPro-Accept v74 AM/PM notation') then
  FloatResult(Engines[UsedEngine].wSource[dsMeasured].twMeasDateTime,41292.730718,0.02,'AM/PM notation');
TestResult(LoadSelftestFile('selftest25_OmniPro_v6_mm.txt'),'OmniPro-Accept v6, positions in mm');   {107}
TestResult(LoadSelftestFile('selftest26_w2CAD.asc'),'Varian w2CAD format');                          {108}
AddEmptyTest(2);
CheckMenuItem(MeasGenericToPDDItem,True);
Stg:= ' as pdd interpreted';
if LoadSelftestFile('selftest21_freescan.txt') then
  TestResult(CxResults[0][1,CxTitle].Caption=DefLabelDmax+':','Freescan'+Stg);                       {111}
if LoadSelftestFile('selftest22_fanline.txt') then
  TestResult(CxResults[0][1,CxTitle].Caption=DefLabelDmax+':','Fanline'+Stg);                        {112}
CheckMenuItem(MeasGenericToPDDItem,False);
TestResult(LoadSelftestFile('selftest22_fanline.txt'),'Fanline');                                    {113}
EdgeDetectionCheckBox.Checked:= True;
if LoadSelftestFile('selftest23_GirafTool.txt') then
  FloatResult(CxResults[2][1],7.5,0.5,'Width of filmdata');                                          {114}
if LoadSelftestFile('selftest24_ScanAngle135.txt') then
  begin
  TestResult(Pos('135°',DataPlot.BottomAxis.Title.Caption)>0,'Scan angle = 135°');                   {115}
  Stg:= 'selftestx6d135_40d_ssd100_d016m.txt';
  AddEmptyTest;
  end;
twcDefaultSSDcm[fcStandard]             := 100;
Ft_Default_SSD_Cm[fcStandard].Value:= 100;
if LoadSelftestFile('selftest29_FFF.mcc') then
  FloatResult(CxResults[2][1],20.00,0.5,'Width');                                                    {117}
CheckMenuItem(MeasScale2defaultSSDitem,True);
CheckMenuItem(RefAtDefaultSSDItem     ,True);
if LoadSelftestFile('selftest27_ssd90.txt') then
  begin
  Stg:= 'selftestx10c40_ssd100_d050m.txt';  {rescaled to ssd100}
  AddEmptyTest;
  FloatResult(CxResults[2][1],42.37,0.5,'Width');                                                    {119}
  end;
AddEmptyTest(8);
CheckMenuItem(ViewBufferItem,False);
UImodeChange(Self);
ProcessAutoscalingItem   .Checked             := tAutoScale;
MeasGenericToElectronItem.Checked             := tGenericToElectron;
Engines[UsedEngine].wNormalisation[fcStandard]:= OrgNorm;
Engines[UsedEngine].ShowWarning               := ShowWarningCheckBox.Checked;
ModListGridFill(sbase ,True);
ModListGridFill(scombi,True);
AddEmptyTest;                                                                                        {128}
{$IFDEF Windows}
if TestResult(FileExists(Application.HelpFile) and Executehelp(SelfTestItem.HelpContext),
              Format('Help file (%s) available',[Application.HelpFile])) then                        {129}
  begin
  WaitLoop(100);
  LoadSelftestFile('selftest01_theoretical.txt');
  AddMessage('Help started',tNormal);
  end;
{$ELSE}
AddEmptyTest;
LoadSelftestFile('selftest01_theoretical.txt');
{$ENDIF}
FloatResult(Fail,0,0.01,'Failures'); {130}
with UseDoseConvTable[0] do
  begin
  DCEdit    .Value  := hev;
  DCFilmTypeBox.Text:= hftxt;
  DCModalityBox.Text:= hmtxt;
  end;
for i:= 0 to Pred(Length(UseDoseConvTable)) do with UseDoseConvTable[i] do
  begin
  with DCBgBox   do Checked:= Tag=1;
  with DCDoseBox do Checked:= Tag=1;
  end;
SettingsTabExit(Self);
twcDeriveMinMax                    := tdmm;
twcDeriveBinFraction               := tdbf;
twcDeriveLookAhead                 := tdla;
twcGammaCutoffPercent              := tgcp;
twcGammaDosePercBase               := tgdp;
twcGammaDistCmBase                 := tgdc;
twcGammaSearchMaxFactor            := tgsf;
twcMatchRangeDivider               := tmrf;
twcMatchStepsNumber                := tmsf;
twcMatchNormDeltaPercent           := tmnp;
twcDefaultSSDcm[fcStandard]        := tssd;
Ft_Default_SSD_Cm[fcStandard].Value:= tssd;
GetWellhoferValues;
AddMessage('See the help file, <F1>, for information.',2);
{$IFDEF Windows}
ExecuteHelp(1,HH_CLOSE_ALL);
{$ENDIF}
ConfigLoad(S);
SelftestLevel:= 0;
S.Free;
end; {~selftest}
{$ENDIF}


{$IFDEF Windows}
{$push}{$warn 5024 off}
// Call online-help
//https://forum.lazarus.freepascal.org/index.php/topic,38487.msg261731.html#msg261731
{01/06/2020}
{31/01/2022 changed data type from Int64 to PtrInt to avoid platform dependenciea}
function TAnalyseForm.HelpHandler(Command     :Word;
                                  Data        :PtrInt;
                                  var CallHelp:Boolean): Boolean;
begin
Result  := ExecuteHelp(Data);
CallHelp:= False;                                                               // Don't call regular help
end; {~helphandler}
{$pop}


{01/06/2020}
{31/01/2022 changed data type from Int64 to PtrInt to avoid platform dependenciea}
function TAnalyseForm.ExecuteHelp(Data   :PtrInt;
                                  Command:Word=HH_HELP_CONTEXT): Boolean;
begin
Result:= length(Application.HelpFile)>0;
if Result then
  htmlHelp.HtmlHelp(0,PChar(Application.HelpFile),Command,Math.Max(1,Data));    // Call HTML help (.chm file)
end; {~executehelp}
{$ENDIF}

{$IFDEF SelfTest}
{$push}{$warn 5024 off}
procedure TAnalyseForm.FormClose(Sender:TObject;
                       var CloseAction :TCloseAction);
begin
SelftestLevel:= 0;
inherited;
end; {~formclose}
{$pop}
{$ENDIF}


{01/08/2015 tempreffile buiten gebruik gesteld}
{12/02/2016 preloadstream, preloadtransfer}
{25/12/2016 InventoryReaderSetup}
{25/10/2018 SpecialModeValues}
{04/05/2020 PRELOADTRANSFER}
{14/09/2020 EngineCleanUp}
{09/05/2024 avoid FreeAndNil}
procedure TAnalyseForm.FormDestroy(Sender:TObject);
begin
{$IFDEF PRELOAD}
PreLoadStream.Free;
{$ENDIF PRELOAD}
{$IFDEF THREAD_FILES}
if assigned(ScanListThread) then
  ScanListThread.Terminate;
{$ENDIF THREAD_FILES}
if ConfigAutoSaveItem.Checked then
  ConfigSave(Self);
{$IFDEF Windows}
ChangeClipboardChain(Handle,NextClipboardOwner);                                //handle to remove, handle of next window in the chain
{$ENDIF}
InventoryReaderSetup(False);
SpecialModeValues.Free;
PanelElements.Free;
Finalize(FileConvPhotonItems);
Finalize(FileConvElectronItems);
Finalize(FileConvGeneralItems);
SetHistoryListSize(0);                                                          //finalize all engines
inherited;
end; {~formdestroy}


end.
