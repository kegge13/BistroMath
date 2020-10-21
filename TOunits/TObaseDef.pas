unit TObaseDef;  {Theo van Soest 22/04/2020}

interface

type
  toExtMsgProc =procedure(s:string; i:integer=0) of object;

resourcestring
  CheckedText   ='on';
  UnCheckedText ='off';

type
  chartAxisSides=(cdefL,cdefB,cdefR,cdefT);

const
  CheckStrings: array[False..True] of string=(UnCheckedText,CheckedText);
  BoolStrings : array[False..True] of string=('False','True');

  chNull            =  #0;
  chTab             =  #9;
  chLF              = #10;
  chCR              = #13;
  chSpace           = #32;

  AsciiCRLF         = chCR+chLF;

  csComplete        = [#0..#255];
  csAscii           = [#0..#127];
  csAsciiCtl        = [#0..#31];
  csWhiteSpace      = [#0..#32];
  csAsciiText       = [#32..#127];
  csAlphaLow        = ['a'..'z'];
  csAlphaUp         = ['A'..'Z'];
  csNumeric         = ['0'..'9'];
  csSign            = ['-','+'];
  caAnsi            = csComplete;
  csNotAscii        = csComplete     -csAscii;
  csAlpha           = csAlphaLow     +csAlphaUp;
  csNotAlpha        = csComplete     -csAlpha;
  csNotNumeric      = csComplete     -csNumeric;
  csAlphaNumeric    = csNumeric      +csAlpha;
  csNotAlphaNumeric = csComplete     -csAlphaNumeric;
  csIllegalWinPath  = csWhiteSpace    +['|','?','*','<','"','>','/'];
  csIllegalWinName  = csIllegalWinPath+[':','\'];


implementation

end.
