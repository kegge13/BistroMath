unit FileIter;
{$mode objfpc}{$h+}
{================================================================================
 TFileIterator Component Version 1.00.00  (C) 1996, Glen Why. No rights reserved.
 Improved by Theo van Soest (C) 1999 - 01/03/2011. Also no rights reserved.
 Port to Free Pascal 3.2.0: 16/03/2020-19/09/2020
 Adaptations by Alan Chamberlain for the Fedora platform.
 ================================================================================}

interface

uses
 SysUtils, Classes;

type

  EFileIteratorError     = class(Exception);
  TFileScanOption        = (fiPath, fiRecurseFolders, fiDirectories);
  TFileScanOptions       = Set of TFileScanOption;
  TFileIteratorStatus    = (fsIdle, fsActive, fsTerminated);
  PFileInfo              = ^TFileInfo;
  TFileInfo              = record
                             Size: Integer;
                             Time: Integer;
                             Attr: Integer;
                           end;

  TFileIterAddEvent      = procedure(Sender        :TObject;
                                     const FileName:TFileName;
                                     const FileInfo:TFileInfo) of object;
  TFileIterAddQueryEvent = procedure(Sender        :TObject;
                                     const FileName:TFileName;
                                     const FileInfo:TFileInfo;
                                     var  CanAdd   :Boolean)   of object;
  TFileIterScanQueryEvent= procedure(Sender        :TObject;
                                     const Folder  :TFileName;
                                     var   CanScan :Boolean)   of object;
  TFileIterator = class(TComponent)
  private
    FOptions          : TFileScanOptions;
    FRecurse          : Boolean;
    FStatus           : TFileIteratorStatus;
    FRootFolder       : TFileName;
    FIterationLevel   : Integer;
    FFileScan         : Boolean;
    FFilter           : TStringList;
    FOnAddFile        : TFileIterAddEvent;
    FOnAddFileQuery   : TFileIterAddQueryEvent;
    FOnActivate       : TNotifyEvent;
    FOnScanQuery      : TFileIterScanQueryEvent;
    FOnTerminate      : TNotifyEvent;
    FRelativePath     : Boolean;
    FFoundFiles       : Integer;
    procedure SetOptions   (AOptions     :TFileScanOptions);
    procedure SetRootFolder(const Folder :TFileName       );
    procedure SetFilterText(const AFilter:String          );
    function  GetFilterText                               : String;
    function  GetIsIdle                                   :Boolean;
    procedure ScanFolder   (Folder       :TFileName;
                            Level        :Integer         );
  protected
    procedure AddFileQuery   (const FileName:TFileName;
                              const FileInfo:TFileInfo;
                              var   CanAdd  :Boolean);              virtual;
    procedure ScanFolderQuery(const Folder  :TFileName;
                              var   CanScan :Boolean);              virtual;
  public
    constructor Create(AOwner:TComponent);                                      override;
    procedure   Iterate;
    procedure   Cancel;
    destructor  Destroy;                                                        override;
    property IsIdle           :Boolean                 read GetIsIdle;
    property Status           :TFileIteratorStatus     read FStatus;
  published
    property FileScan         :Boolean                 read FFileScan       write FFileScan
             default True;
    property Filter           :String                  read GetFilterText   write SetFilterText;
    property FoundFiles       :Integer                 read FFoundFiles;
    property IterationLevel   :Integer                                      write FIterationLevel
             default -1;
    property OnAddFile        :TFileIterAddEvent       read FOnAddFile      write FOnAddFile;
    property OnAddFileQuery   :TFileIterAddQueryEvent  read FOnAddFileQuery write FOnAddFileQuery;
    property OnActivate       :TNotifyEvent            read FOnActivate     write FOnActivate;
    property OnScanFolderQuery:TFileIterScanQueryEvent read FOnScanQuery    write FOnScanQuery;
    property OnTerminate      :TNotifyEvent            read FOnTerminate    write FOnTerminate;
    property Options          :TFileScanOptions        read FOptions        write SetOptions
             default [fiPath, fiRecurseFolders, fiDirectories];
    property RootFolder       :TFileName               read FRootFolder     write SetRootFolder;
    property RelativePath     :Boolean                 read FRelativePath   write FRelativePath
             default False;
  end;

  TFileIteratorThread = class(TThread)
  private
    FIterator: TFileIterator;
    function GetStatus: TFileIteratorStatus;
  protected
    procedure   Execute;                                                        override;
  public
    constructor Create(Iterator               :TFileIterator;
                       const FreeOntermination:Boolean=True;
                       const CreateSuspended  :Boolean=False);                  reintroduce;
    procedure   Terminate;
    property Status:TFileIteratorStatus read GetStatus;
  end;


implementation

uses Forms, LazFileUtils;


procedure IteratorError(AMessage:AnsiString);
begin
raise EFileIteratorError.CreateRes(PAnsiString(AMessage));
end;

{ TFileIterator }

constructor TFileIterator.Create(AOwner:TComponent);
begin
inherited Create(AOwner);
FIterationLevel:= -1;
FStatus        := fsIdle;
FOptions       := [fiPath,fiRecurseFolders,fiDirectories];
FRootFolder    := 'C:';
FileScan       := True;
RelativePath   := False;
FFilter        := TStringList.Create;
FFoundFiles    := -1;
end; {~create}


function TFileIterator.GetIsIdle:Boolean;
begin
Result:= (Status=fsIdle);
end; {~getisidle}


procedure TFileIterator.SetOptions(AOptions:TFileScanOptions);
begin
if IsIdle or (csLoading in ComponentState) then
  FOptions:= AOptions;
end;  {~setoptions}


procedure TFileIterator.SetRootFolder(const Folder:TFileName);
begin
if not (IsIdle  or (csLoading in COmponentState)) then
  IteratorError('Iterator options');
FRootFolder:= Folder;
end; {~setrootfolder}


procedure TFileIterator.SetFilterText(const AFilter:String);
begin
FFilter.CommaText:= AFilter;
end; {~setfiltertext}


function  TFileIterator.GetFilterText: String;
begin
Result:= FFilter.CommaText;
end; {~setfiltertext}


procedure TFileIterator.AddFileQuery(const FileName:TFileName;
                                     const FileInfo:TFileInfo;
                                     var   CanAdd  :Boolean);
begin
CanAdd:= True;
if assigned(FOnAddFileQuery) then
  FOnAddFileQuery(Self,FileName,FileInfo,CanAdd);
end; {~addfilequery}


procedure TFileIterator.ScanFolderQuery(const Folder :TFileName;
                                        var   CanScan:Boolean);
begin
CanScan:= True;
if assigned(FOnScanQuery) then
  FOnScanQuery(Self,Folder,CanScan);
end; {~scanfolderquery}


procedure TFileIterator.Cancel;
begin
if Status=fsActive then
  FStatus:= fsTerminated;
end; {~cancel}


procedure TFileIterator.Iterate;
var i: Integer;
begin
if not IsIdle then
  IteratorError('Iterator');
if not (Assigned(FOnAddFile) or Assigned(FOnAddFileQuery)) then
  Exit;
i:= FFilter.Count;
if i=0 then
 {$IFDEF Windows}
  Filter:= '*.*'
 {$ELSE}                                                                        //Alan Chamberlain 16/9/2020: adaptation for Fedora platform
  Filter:= '*'
 {$ENDIF}
else with FFilter do
  while i>0 do
    begin
    Dec(i);
    if Strings[i][1]='.' then
      Strings[i]:= '*'+Strings[i];
   {$IFDEF WINDOWS}                                                             //Alan Chamberlain 16/9/2020: exclude this for Fedora platform
    if Pos('.',Strings[i])=0 then
       Strings[i]:= Strings[i]+'.*';
   {$ENDIF}
    end;
FStatus    := fsActive;
FRecurse   := (fiRecurseFolders in Options);
FFoundFiles:= 0;
try
  if assigned(FOnActivate) then
    FOnActivate(Self);
  ScanFolder(RootFolder,FIterationLevel);
 finally
  if assigned(FOnTerminate) then
    FOnTerminate(Self);
  FStatus:= fsIdle;
 end;
end; {~iterate}


{$WARNINGS OFF}
procedure TFileIterator.ScanFolder(Folder:TFileName;
                                   Level :Integer);
var SearchError: Integer;
    FileInfo   : TFileInfo;
    SearchRec  : TSearchRec;
    PathStg    : String;
    DoFolder   : Boolean;
    i          : Integer;

  procedure AddRecord;
  var Ok: Boolean;
  begin
  with SearchRec do
    begin
    Ok           := True;
    FileInfo.Attr:= Attr;
    FileInfo.Size:= Size;
    FileInfo.Time:= Time;
    if fiPath in Options then
      Name:= PathStg+Name;
    AddFileQuery(Name,FileInfo,Ok);
    if Ok then
      begin
      Inc(FFoundFiles);
      if Assigned(FOnAddFile) then
        FOnAddFile(Self,Name,FileInfo);
      end;
    end; {with searchrec}
  end;

begin
FillChar(SearchRec,SizeOf(SearchRec),0);
DoFolder:= Length(Folder)>0;
if DoFolder then
  begin
  AppendPathDelim(Folder);                                                      //Alan Chamberlain, all platform adaptation 16/9/2020
  ScanFolderQuery(Folder,DoFolder);
  end;
if DoFolder then
  begin
  if FFoundFiles<0 then FFoundFiles:= 0;
  DoFolder:= (fiDirectories in Options) and (Filter<>'*.*') and FileScan;
  try
    i:= FFilter.Count;
    if FileScan then
      while i>0 do
        begin
        Dec(i);
        if fiPath in Options then
          begin
          PathStg:= Folder;
          if RelativePath then Delete(PathStg,1,Length(RootFolder));
          end
        else PathStg:= '';
        if fiDirectories in Options then SearchError:= faAnyFile            -faVolumeID
        else                             SearchError:= faAnyFile-faDirectory-faVolumeID;
        try
          SearchError:= FindFirst(Folder+FFilter.Strings[i],SearchError,SearchRec);
          while (SearchError=0) and (Status<>fsTerminated) do
            begin
            AddRecord;
            SearchError:= FindNext(SearchRec);
            end; {while}
         except
           with SearchRec do
             begin
             Name:= '-';
             Size:= 0;
             Time:= DateTimeToFileDate(Date);
             Attr:= faAnyFile;
             end;
         end; {try}
        end; {filescan}
    if FRecurse or DoFolder then
      begin
      SearchError:= FindFirst(Folder+'*.*',faAnyFile-faVolumeID,SearchRec);
      while (SearchError=0) and (Status<>fsTerminated) do
        begin
        with SearchRec do if ((Attr and faDirectory)>0) and (Name[1]<>'.') then
          begin
          if DoFolder then
            AddRecord;
          if FRecurse and (Level<>0) then
            ScanFolder(Folder+Name,Pred(Level));
          end;
        SearchError:= FindNext(SearchRec);
        end; {while}
      end; {recursefolders}
   finally
    FindClose(SearchRec);  {free resources claimed by findfirst}
   end; {try}
 end; {if b}
end; {~scanfolder}
{$WARNINGS ON}


destructor TFileIterator.Destroy;
begin
FFilter.Free;
inherited;
end; {~destroy}


{------------ TFileIteratorThread --------------------------------------------------- }

{27/04/2020 resume -> start}
constructor TFileIteratorThread.Create(Iterator               :TFileIterator;
                                       const FreeOntermination:Boolean=True;
                                       const CreateSuspended  :Boolean=False);
begin
inherited Create(True);
FIterator      := Iterator;
FreeOnterminate:= FreeOntermination;
if not CreateSuspended then
  Start;        {resume is deprecated}
end; {~create}

procedure TFileIteratorThread.Execute;
begin
if assigned(FIterator) then
  FIterator.Iterate;
end; {~execute}

function TFileIteratorThread.GetStatus: TFileIteratorStatus;
begin
Result:= FIterator.Status;
end; {~getstatus}

procedure TFileIteratorThread.Terminate;
var i: integer;
begin
if assigned(FIterator) then if FIterator.Status=fsActive then FIterator.Cancel;
for i:= 1 to 50 do
  begin
  Application.ProcessMessages;
  Sleep(1);
  end;
inherited;
end; {~terminate}

end.
