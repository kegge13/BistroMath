{14/07/2020}

{$INCLUDE TOunits\TOmath_opt.inc}

//use list of scans in multiple scan reference}
{$DEFINE MULTIREF_INDEX}

//derivative over a fixed distance independent of the steps with at least 3 points
{$DEFINE FIXED_DISTANCE_DERIVATIVE}

//filter over a fixed distance independent of the steps with at least 3 points
{$DEFINE FIXED_DISTANCE_FILTER}

{25/05/2020}
//discrete fixed distance stepping filter to avoid floating point rounding errors (QuadFilter and Derive)
//needed for port to FPC to get comparable results with Delphi7
{$DEFINE DISCRETE_FIXED_DISTANCE}

//use threaded versions of calculation intensive code
{$DEFINE THREADED}


{09/08/2015: THREAD_PLOT and THREAD_FILES added}
{$IFDEF THREADED}
  {$I TOunits/TOmath_opt.inc}
  //for development/testing only
  {.DEFINE THREAD_USESTATS}

  //use threads for fillseries proves to be stable, marginally faster
  {$DEFINE THREAD_PLOT}

  //use thread for filesearch, problematic
  {.DEFINE THREAD_FILES}

  //show messages on locked curves
  {.DEFINE LOCKING_MSSG}
{$ENDIF}


{09/12/2015: preload parser in WellForm}
{$DEFINE PRELOAD}

//various time consuming speed tests, only for testing purposes
{.DEFINE SPEEDTEST}

{$IFDEF SPEEDTEST}
  {.DEFINE MEDIAN_SPEEDTEST}
  {.DEFINE GAMMA_SPEEDTEST}
  {.DEFINE DIVIDE_SPEEDTEST}
  {.DEFINE PANEL_SPEEDTEST}
  {$DEFINE ONDATA_SPEEDTEST}
{$ENDIF SPEEDTEST}


{19/05/2020: deleting one line at a time shows to be very slow}
{$DEFINE MEMO_ALLCLEAR}

{21/05/2020}
{$DEFINE FPC_SUPPORT_X87_TYPES_ON_WIN64}

{15/12/2015: fff-analysis based symmetry}
{.DEFINE FFF_SYMMETRY}

{24/11/2017: research on position integral stopped}
{.DEFINE POSINTEGRAL}

//show extrapolation of pdd fit
{.DEFINE SHOW_X_FIT}
{.DEFINE X_FIT_TEST}


//alternative model for pddfit costfunction
{$DEFINE ENR_WEIGHTED_PDDFIT}

//set up printing
{$DEFINE form2pdf}
{$IFDEF form2pdf}
  {$DEFINE form2pdfsel}
  {$DEFINE form2pdfall}
  {$DEFINE form2pdfcurrent}
{$ENDIF}


//general debug
{.DEFINE COMPILED_DEBUG}
{.DEFINE TEST_RULES}
{.DEFINE READRFB_TEST}
{.DEFINE WELLHOFER_DUMPDATA}
{$DEFINE SIGMOID_REPORT}
{.DEFINE SIGMOID_RANGE_SYMMETRIC}
{.DEFINE KEEP_SIGMOID_ESTIMATE}
{$DEFINE SelfTest}
{$IFOPT D-}
  {$UNDEF SelfTest}
{$ENDIF}

