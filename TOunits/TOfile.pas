{$WARN SYMBOL_PLATFORM	OFF}
unit TOfile;   {© Theo van Soest 2000 - 18/01/2019 | FPC3.2.0: 19/08/2020}

interface      {11/06/2020}
{11/06/2020 executefile removed}

uses Classes;

{ Delphi TTextRec
File types are represented as records.
, which are laid out as follows:

type
    TTextBuf = array[0..127] of Char;

Typed files and untyped files occupy 332 bytes:  | Text files occupy 460 bytes:
                                                 |
  TFileRec = packed record                       |  TTextRec = packed record
    Handle: Integer;                             |    Handle   : Integer;
    Mode  : word;                                |    Mode     : word;
    Flags : word;                                |    Flags    : word;
    case Byte of                                 |
      0: (RecSize  : Cardinal);                  |
      1: (BufSize  : Cardinal;                   |    BufSize  : Cardinal;
          BufPos   : Cardinal;                   |    BufPos   : Cardinal;
          BufEnd   : Cardinal;                   |    BufEnd   : Cardinal;
          BufPtr   : PChar;                      |    BufPtr   : PChar;
          OpenFunc : Pointer;                    |    OpenFunc : Pointer;
          InOutFunc: Pointer;                    |    InOutFunc: Pointer;
          FlushFunc: Pointer;                    |    FlushFunc: Pointer;
          CloseFunc: Pointer;                    |    CloseFunc: Pointer;
          UserData : array[1..32] of Byte;       |    UserData : array[1..32] of Byte;
          Name     : array[0..259] of Char; );   |    Name     : array[0..259] of Char;
                                                 |    Buffer   : TTextBuf;
    end;                                         |  end;

Handle contains the file’s handle (when the file is open).
The Mode field can assume one of the values

const

  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;

where fmClosed indicates that the file is closed, fmInput and fmOutput indicate a text file that has been reset (fmInput) or rewritten (fmOutput), fmInOut indicates a typed or untyped file that has been reset or rewritten. Any other value indicates that the file variable is not assigned (and hence not initialized).
The UserData field is available for user-written routines to store data in.
Name contains the file name, which is a sequence of characters terminated by a null character (#0).

For typed files and untyped files, RecSize contains the record length in bytes, and the Private field is unused but reserved.
For text files, BufPtr is a pointer to a buffer of BufSize bytes, BufPos is the index of the next character in the buffer to read or write, and BufEnd is a count of valid characters in the buffer. OpenFunc, InOutFunc, FlushFunc, and CloseFunc are pointers to the I/O routines that control the file; see Device functions. Flags determines the line break style as follows:

bit 0 clear	LF line breaks
bit 0 set	CRLF line breaks
All other Flags bits are reserved for future use. See also DefaultTextLineBreakStyle and SetLineBreakStyle.
}


type

  EInvalidDest   = class(EStreamError);
  EFCantMove     = class(EStreamError);
  tofFileIOmode  = (tofRead,tofReadWrite,tofReWrite,tofAppend);
  tofFileErrLevel= (tofContinue,tofShowMessage,tofTerminate);

function  CreatePath(DirPath    :String                       ): Boolean;
function  HasAttr(const FileName:string;
                  Attr          : Word                        ): Boolean;
function  ExtractFilename_no_ext(FileName:String              ): String;
function  CompressedFilename(FileName:String                  ): String;
function  FileExtExists(const FileName,ExtensionList:String   ): String;
function  GetFileSize(AFile:String                            ): Int64;

implementation

uses SysUtils;

//--procedures-----------------------------------------------------------

{$push}{$warn 5092 off}
function CreatePath(DirPath:String): Boolean;
var s: string;
    i: Byte;
begin
DirPath:= ExpandFileName(ExcludeTrailingPathDelimiter(DirPath));
i      := 1;
Result := True;
repeat
  while not ((i=Length(DirPath)) or IsPathDelimiter(DirPath,i)) do Inc(i);
  s:= Copy(DirPath,1,i);
  if not DirectoryExists(s) then Result:= CreateDir(s);
  Inc(i);
until ((i>Length(DirPath)) or (not Result));
end; {createpath}
{$pop}


function HasAttr(const FileName:string; Attr:Word): Boolean;
var FileAttr: Integer;
begin
FileAttr:= FileGetAttr(FileName);
if FileAttr=-1 then FileAttr:= 0;
Result:= (FileAttr and Attr)=Attr;
end; {hasattr}


{$push}{$warn 5092 off}
{20/11/2015}
function ExtractFilename_no_ext(FileName:String): String;
var i: Integer;
begin
FileName:= ExtractFilename(FileName);
i:= Pos('.',FileName)-1;
if i<0 then i:= Length(FileName);
Result:= Copy(FileName,1,i);
end; {extractfilename_no_ext}
{$pop}


function FileExtExists(const FileName,ExtensionList:String): String;
var Stg: String;
    i,j: Integer;
begin
if ExtensionList[1]='.' then i:= 2
else                         i:= 1;
Stg:= '';
repeat
  j:= Pos('.',Copy(ExtensionList,i,Length(ExtensionList)-Pred(i)));
  if (j=0) and (Length(ExtensionList)>=j+i) then
    j:= Length(ExtensionList);
  if j>0 then
    Stg:= ChangeFileExt(FileName,'.'+Copy(ExtensionList,i,j-i+1));
  Inc(i,j);
  if not FileExists(Stg) then
    Stg:= '';
until (Length(Stg)>0) or (j=0);
if Length(Stg)=0 then Result:= FileName
else                  Result:= Stg;
end; {fileextexists}


function CompressedFilename(FileName:String): String;
var i,j: Integer;
begin
Result:= ExtractFilename(FileName);
i     := Length(FileName)-Length(Result)-1;
j     := i;
while (j>0) and (Filename[j]<>PathSeparator) do
  Dec(j);
if j>10 then Result:= ExtractFileDrive(FileName)+PathSeparator+'...'+Copy(FileName,j,i-j)+PathSeparator+Result
else         Result:= FileName;
end;


function GetFileSize(AFile:String): Int64;
var f: file;
begin
if FileExists(AFile) then
  begin
  {$i-}
  AssignFile(f,Afile);
  Reset(f,1);
  Result:= FileSize(f);
  CloseFile(f);
  {$i+}
  end
else
  Result:= -1;
end;

end.
