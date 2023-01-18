{$WARN SYMBOL_PLATFORM	OFF}
unit TOconfigStrings;   {© Theo van Soest, 29/09/2016-18/01/2023 - Lazarus 2.0.8/FPC 3.0.4: 02/07/2020}
(*
=================================================================================
 This library is original work of Theo van Soest.
 It is published under the GNU Lesser General Public License v3 (LGPL-3.0).
 https://tldrlegal.com/license/gnu-lesser-general-public-license-v3-%28lgpl-3%29
=================================================================================
*)

interface               {28/11/2022}

uses IniFiles, Classes, Controls, StdCtrls, ComCtrls, ExtCtrls, Forms, Buttons, Graphics, SpinEx,
     ActnList, EditBtn, Menus, Dialogs;

type
  TConfigStrings = class(TMemIniFile)
  private
    FMenuWithShortCut: Boolean;
  public
    constructor Create      (AFileName          :String);                 reintroduce;  overload;
    constructor Create;                                                   reintroduce;  overload;
    function    ReadWinBounds(const ASection    :string;                                  //--stored value--
                              var L,T,W,H       :Integer          ): Boolean;  overload;  //integers TopKey,LeftKey,WidthKey,HeightKey, see definitions below
    function    ReadWinBounds(const ASection    :string;
                              var ARect         :TRect            ): Boolean;  overload;  //integers TopKey,LeftKey,WidthKey,HeightKey, see definitions below
    function    ReadControl(const AControl      :TControl;
                            ASection            :string ='';
                            DefaultVisible      :Boolean=True     ): Boolean;  virtual;   //TControl.BoundsRect and TControl.Visible
    procedure   ReadShortCuts(const ASection    :String;
                              const AMenuItem   :TMenuItem        );           overload;  //TMenuItem.ShortCut and TMenuItem.Items[..].ShortCut
    procedure   ReadShortCuts(const ASection    :String;
                              const AMenu       :TMainMenu        );           overload;  //TMenu.ShortCut and TMenu.Items[..].ShortCut
    procedure   ShortRead (const ASection       :String;
                           const AEdit          :TDirectoryEdit   );           overload;  //TDirectoryEdit.RootDir
    procedure   ShortRead (const ASection       :String;
                           const AEdit          :TEdit            );           overload;  //TEdit.Text
    procedure   ShortRead (const ASection       :String;
                           const ALabeledEdit   :TLabeledEdit     );           overload;  //TLabeledEdit.Text
    procedure   ShortRead (const ASection       :String;
                           const AButtonEdit    :TCustomEditButton);           overload;  //TCustomEditButton.Text
    procedure   ShortRead (const ASection       :String;
                           const ACustomEdit    :TCustomEdit      );           overload;  //TCustomEdit.Text
    procedure   ShortRead (const ASection       :String;
                           const ACustomEdit    :TSpinEditEx      );           overload;  //TSpinEditEx.Value
    procedure   ShortRead (const ASection       :String;
                           const ACustomEdit    :TFloatSpinEditEx );           overload;  //TFloatSpinEditEx.Value
    procedure   ShortRead (const ASection       :String;
                           const ATabSheet      :TTabSheet        );           overload;  //TTabSheet.Enabled
    procedure   ShortRead (const ASection       :String;
                           const AUpDown        :TUpDown          );           overload;  //TUpDown.Position
    procedure   ShortRead (const ASection       :String;
                           const ASpeedButton   :TSpeedButton     );           overload;  //TSpeedButton.Down
    procedure   ShortRead (const ASection       :String;
                           const ACheckBox      :TCheckBox        );           overload;  //TCheckBox.Checked
    procedure   ShortRead (const ASection       :String;
                           const ARadioGroup    :TRadioGroup      );           overload;  //TRadioGroup.ItemIndex
    procedure   ShortRead (const ASection       :String;
                           const ARadioButton   :TRadioButton     );           overload;  //TRadioButton.Checked
    procedure   ShortRead (const ASection       :String;
                           const AListBox       :TListBox         );           overload;  //TListBox.ItemIndex
    procedure   ShortRead (const ASection       :String;
                           const AComboBox      :TComboBox        );           overload;  //TComboBox.ItemIndex, stored as text of item
    procedure   ShortRead (const ASection       :String;
                           const AAction        :TAction          );           overload;  //TAction.Checked
    procedure   ShortRead (const ASection       :String;
                           const AMenuItem      :TMenuItem        );           overload;  //TMenuItem.Checked | optionally also TMenuItem.ShortCut
    procedure   ShortRead (const ASection       :String;
                           const APanel         :TPanel           );           overload;  //TPanel.Color, stored as integer
    procedure   ShortRead (const ASection       :String;
                           const AColorButton   :TColorButton     );           overload;  //TColorButton.ButtonColor, stored as integer
    procedure   WriteWinBounds(const ASection   :string;
                               const L,T,W,H    :Integer          );           overload;
    procedure   WriteWinBounds(const ASection   :string;
                               const ARect      :TRect            );           overload;
    procedure   WriteControl(const AControl     :TControl;
                             ASection           :string=''        );           virtual;
    procedure   WriteSectionValues(const Section:string;
                                   List         :TStrings         );           virtual;
    procedure   WriteShortCuts(const ASection   :String;
                               const AMenu      :TMainMenu        );           overload;
    procedure   WriteShortCuts(const ASection   :String;
                               const AMenuItem  :TMenuItem        );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const AEdit          :TDirectoryEdit   );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const AEdit          :TEdit            );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const ALabeledEdit   :TLabeledEdit     );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const AButtonEdit    :TCustomEditButton);           overload;
    procedure   ShortWrite(const ASection       :String;
                           const ACustomEdit    :TCustomEdit      );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const ACustomEdit    :TSpinEditEx      );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const ACustomEdit    :TFloatSpinEditEx );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const ATabSheet      :TTabSheet        );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const ASpeedButton   :TSpeedButton     );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const AUpDown        :TUpDown          );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const ACheckBox      :TCheckBox        );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const ARadioGroup    :TRadioGroup      );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const ARadioButton   :TRadioButton     );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const AListBox       :TListBox         );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const AComboBox      :TComboBox        );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const AAction        :TAction          );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const AMenuItem      :TMenuItem;
                           const WriteShortCut  :Boolean=False    );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const APanel         :TPanel           );           overload;
    procedure   ShortWrite(const ASection       :String;
                           const AColorButton   :TColorButton     );           overload;
    procedure   WriteHex  (const ASection,Ident :string;
                           const Value          :LongInt;
                           const Digits         :Byte             );           virtual;
    property  MenuWithShortCut: Boolean read FMenuWithShortCut write FMenuWithShortCut;
  end;


function  GetConfigStg: String;        {d:\pp\n.CFG when writable else windir\n.cfg}

implementation

uses {$IFDEF Windows}
      Windows,
     {$ENDIF}
      Math, SysUtils;

const
  TopKey     = 'winT';
  LeftKey    = 'winL';
  HeightKey  = 'winH';
  WidthKey   = 'winW';
  ShortCutKey= '_shortcut';

(*
MakeWinBounds
  input : TRect array
  output: individual values Left,Top,Width,Height
*)
procedure MakeWinBounds (const ARect                :TRect;
                         var   Left,Top,Width,Height:Integer);
begin
Left  := ARect.Left;
Top   := Arect.Top;
Width := (ARect.Right -Left);
Height:= (ARect.Bottom-Top );
end; {~makewinbounds}


(*
MakeRect
input : individual values Left,Top,Width,Height
  output: TRect array
*)
function MakeRect(const Left,Top,Width,Height:Integer): TRect;
begin
Result.Left  := Left;
Result.Top   := Top;
Result.Right := Left+Width ;
Result.Bottom:= Top +Height;
end; {makerect}



//--TConfigStrings-----------------------------------------------------------

constructor TConfigStrings.Create(AFileName:String);
begin
if Length(AFileName)=0 then
  AFileName:= GetConfigStg;
FMenuWithShortCut:= False;
inherited Create(AFileName);
end; {~create}


constructor TConfigStrings.Create;
begin
inherited Create('');
end; {~create}


function TConfigStrings.ReadWinBounds(const ASection:string;
                                       var L,T,W,H   :Integer): Boolean;
  function ConditionalRead(const ASectionValue:String;
                           var   AValue       :Integer): Boolean;
  begin
  Result:= ValueExists(ASection,ASectionValue);
  AValue:= ReadInteger(ASection,ASectionValue,AValue);
  end;

begin
Result:= ConditionalRead(LeftKey ,L) and ConditionalRead(TopKey   ,T) and
         ConditionalRead(WidthKey,W) and ConditionalRead(HeightKey,H);
end; {~readwinbounds}


{$push}{$warn 5057 off:Local variable does not seem to be initialized}
function TConfigStrings.ReadWinBounds(const ASection:string;
                                      var   ARect   :TRect): Boolean;
var l,t,w,h: Integer;
begin
MakeWinBounds(ARect,l,t,w,h);
Result:= ReadWinBounds(ASection,l,t,w,h);
ARect := MakeRect(l,t,w,h);
end; {~readwinbounds}
{$pop}

function TConfigStrings.ReadControl(const AControl:TControl;
                                    ASection      :string ='';
                                    DefaultVisible:Boolean=True): Boolean;
var ARect: TRect;
begin
with AControl do
  begin
  if Length(ASection)=0 then ASection:= Name;
  ARect := BoundsRect;
  Result:= SectionExists(ASection) and ReadWinBounds(ASection,ARect);
  if Result then
    begin
    BoundsRect:= ARect;
    ReadBool(ASection,'visible',DefaultVisible);
    end;
  end;
end; {~readcontrol}


procedure TConfigStrings.ReadShortCuts(const ASection :String;
                                       const AMenuItem:TMenuItem);
var i: Integer;
begin
if AMenuItem.Count>0 then
  for i:= 0 to AMenuItem.Count-1 do
    ReadShortCuts(ASection,AMenuItem.Items[i]);
with AMenuItem do
  ShortCut:= ReadInteger(ASection,Name+ShortCutKey,ShortCut);
end; {~readshortcuts}


procedure TConfigStrings.ReadShortCuts(const ASection:String;
                                       const AMenu   :TMainMenu);
var i: Integer;
begin
if AMenu.Items.Count>0 then
  for i:= 0 to AMenu.Items.Count-1 do
    with AMenu.Items[i] do
      ReadShortCuts(ASection,AMenu.Items[i]);
end; {~readshortcuts}


procedure TConfigStrings.ShortRead (const ASection:String;
                                    const AEdit   :TDirectoryEdit);
begin
with AEdit do
  begin
  Directory:= ReadString(ASection,Name,Directory);
  RootDir  := Directory;
  end;
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection:String;
                                   const AEdit   :TEdit);
begin
with AEdit do Text:= ReadString(ASection,Name,Text);
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection    :String;
                                   const ALabeledEdit:TLabeledEdit);
begin
with ALabeledEdit do Text:= ReadString(ASection,Name,Text);
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection   :String;
                                   const AButtonEdit:TCustomEditButton);
begin
with AButtonEdit do Text:= ReadString(ASection,Name,Text);
end;


procedure TConfigStrings.ShortRead(const ASection   :String;
                                   const ACustomEdit:TCustomEdit);
begin
with ACustomEdit do Text:= ReadString(ASection,Name,Text);
end; {~shortread}


procedure TConfigStrings.ShortRead (const ASection   :String;
                                    const ACustomEdit:TSpinEditEx);
begin
with ACustomEdit do Value:= ReadInteger(ASection,Name,Value);
end; {~shortread}


procedure TConfigStrings.ShortRead (const ASection   :String;
                                    const ACustomEdit:TFloatSpinEditEx);
begin
with ACustomEdit do Value:= ReadFloat(ASection,Name,Value);
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection :String;
                                   const ATabSheet:TTabSheet);
begin
with ATabSheet do Enabled:= ReadBool(ASection,Name,Enabled);
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection:String;
                                   const AUpDown :TUpDown);
begin
with AUpDown do Position:= ReadInteger(ASection,Name,Position);
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection    :String;
                                   const ASpeedButton:TSpeedButton);
begin
with ASpeedButton do Down:= ReadBool(ASection,Name,Down);
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection :String;
                                   const ACheckBox:TCheckBox);
begin
with ACheckBox do Checked:= ReadBool(ASection,Name,Checked);
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection   :String;
                                   const ARadioGroup:TRadioGroup);
begin
with ARadioGroup do if Items.Count>0 then
  ItemIndex:= EnsureRange(ReadInteger(ASection,Name,ItemIndex),-1,Items.Count-1);
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection    :String;
                                   const ARadioButton:TRadioButton);
begin
with ARadioButton do Checked:= ReadBool(ASection,Name,Checked);
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection:String;
                                   const AListBox:TListBox);
begin
with AListBox do if Items.Count>0 then
  ItemIndex:= EnsureRange(ReadInteger(ASection,Name,ItemIndex),-1,Items.Count-1);
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection :String;
                                   const AComboBox:TComboBox);
var i: Integer;
    s: String;
begin
with AComboBox do
  begin
  s:= ReadString(ASection,Name,Text);
  i:= Pred(Items.Count);
  while (i>0) and (s<>Items[i]) do Dec(i);
  if (s=Items[i]) then ItemIndex:= i
  else                 ItemIndex:= ReadInteger(ASection,Name+'Index',ItemIndex);
  end;
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection :String;
                                   const AAction  :TAction);
begin
with AAction do Checked:= ReadBool(ASection,Name,Checked);
end; {~shortread}


{16/05/2020 shortcut added}
procedure TConfigStrings.ShortRead(const ASection :String;
                                   const AMenuItem:TMenuItem  );
begin
with AMenuItem do
  begin
  Checked   := ReadBool(ASection,Name,Checked);
  if ValueExists(ASection,Name+ShortCutKey) then
    ShortCut:= ReadInteger(ASection,Name+ShortCutKey,ShortCut);
  end;
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection:String;
                                   const APanel  :TPanel);
begin
with APanel do Color:= ReadInteger(ASection,Name,Color)
end; {~shortread}


procedure TConfigStrings.ShortRead(const ASection    :String;
                                   const AColorButton:TColorButton);
begin
with AColorButton do ButtonColor:= ReadInteger(ASection,Name,ButtonColor)
end; {~shortread}


procedure TConfigStrings.WriteWinBounds(const ASection:string;
                                        const L,T,W,H :Integer);
begin
WriteInteger(ASection,LeftKey  ,L);
WriteInteger(ASection,TopKey   ,T);
WriteInteger(ASection,WidthKey ,W);
WriteInteger(ASection,HeightKey,H);
end; {~writewinbounds}


{$push}{$warn 5057 off:Local variable does not seem to be initialized}
procedure TConfigStrings.WriteWinBounds(const ASection:string;
                                        const ARect   :TRect);
var l,t,w,h: Integer;
begin
MakeWinBounds(ARect,l,t,w,h);
WriteWinBounds(ASection,l,t,w,h);
end; {~writewinbounds}
{$pop}


procedure TConfigStrings.WriteControl(const AControl:TControl;
                                      ASection      :string='');
begin
with AControl do
  begin
  if Length(ASection)=0 then ASection:= Name;
  WriteWinBounds(ASection,Left,Top,Width,Height);
  WriteBool     (ASection,'visible',Visible);
  end;
end; {~writecontrol}


procedure TConfigStrings.WriteShortCuts(const ASection    :String;
                                        const AMenuItem   :TMenuItem);
var i: Integer;
begin
if AMenuItem.Count>0 then
  for i:= 0 to AMenuItem.Count-1 do
    ReadShortCuts(ASection,AMenuItem.Items[i]);
with AMenuItem do
  if ShortCut>0 then
    WriteHex(ASection,Name+ShortCutKey,ShortCut,4);
end; {~writeshortcuts}


procedure TConfigStrings.WriteShortCuts(const ASection    :String;
                                        const AMenu       :TMainMenu);
var i: Integer;
begin
if AMenu.Items.Count>0 then
  for i:= 0 to AMenu.Items.Count-1 do
    with AMenu.Items[i] do
      WriteShortCuts(ASection,AMenu.Items[i]);
end; {~writeshortcuts}


procedure  TConfigStrings.ShortWrite(const ASection:String;
                                     const AEdit   :TDirectoryEdit);
begin
with AEdit do WriteString(ASection,Name,Directory);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection:String;
                                    const AEdit   :TEdit);
begin
with AEdit do WriteString(ASection,Name,Text);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection    :String;
                                    const ALabeledEdit:TLabeledEdit);
begin
with ALabeledEdit do WriteString(ASection,Name,Text);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection   :String;
                                    const AButtonEdit:TCustomEditButton);
begin
with AButtonEdit do WriteString(ASection,Name,Text);
end;


procedure TConfigStrings.ShortWrite(const ASection   :String;
                                    const ACustomEdit:TCustomEdit);
begin
with ACustomEdit do WriteString(ASection,Name,Text);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection   :String;
                                    const ACustomEdit:TSpinEditEx);
begin
with ACustomEdit do WriteInteger(ASection,Name,Value);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection   :String;
                                    const ACustomEdit:TFloatSpinEditEx);
begin
with ACustomEdit do WriteFloat(ASection,Name,Value);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection :String;
                                    const ATabSheet:TTabSheet);
begin
with ATabSheet do WriteBool(ASection,Name,Enabled);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection    :String;
                                    const ASpeedButton:TSpeedButton);
begin
with ASpeedButton do WriteBool(ASection,Name,Down);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection:String;
                                    const AUpDown :TUpDown);
begin
with AUpDown do WriteInteger(ASection,Name,Position);
end; {~shortwrite}

procedure TConfigStrings.ShortWrite(const ASection :String;
                                    const ACheckBox:TCheckBox);
begin
with ACheckBox do WriteBool(ASection,Name,Checked);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection   :String;
                                    const ARadioGroup:TRadioGroup);
begin
with ARadioGroup do WriteInteger(ASection,Name,ItemIndex);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection    :String;
                                    const ARadioButton:TRadioButton);
begin
with ARadioButton do WriteBool(ASection,Name,Checked);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection:String;
                                    const AListBox:TListBox);
begin
with AListBox do WriteInteger(ASection,Name,ItemIndex);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection :String;
                                    const AComboBox:TComboBox);
begin
with AComboBox do
  begin
  WriteString(ASection,Name,Text);
  WriteInteger(ASection,Name+'Index',ItemIndex);
  end;
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection:String;
                                    const AAction :TAction);
begin
with AAction do WriteBool(ASection,Name,Checked);
end; {~shortwrite}


{16/05/2020 shortcut added}
{02/06/2020 deletekey}
procedure TConfigStrings.ShortWrite(const ASection     :String;
                                    const AMenuItem    :TMenuItem;
                                    const WriteShortCut:Boolean=False);
var b: Boolean;
begin
with AMenuItem do
  begin
  WriteBool(ASection,Name,Checked);
  b:= (WriteShortCut or FMenuWithShortCut) and (ShortCut>0);
  if b then
    WriteHex(ASection,Name+ShortCutKey,ShortCut,4)
  else
    DeleteKey(ASection,Name+ShortCutKey);
  end;
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection:String;
                                    const APanel  :TPanel);
begin
with APanel do WriteHex(ASection,Name,Color,8);
end; {~shortwrite}


procedure TConfigStrings.ShortWrite(const ASection    :String;
                                    const AColorButton:TColorButton);
begin
with AColorButton do WriteHex(ASection,Name,ButtonColor,8);
end; {~shortwrite}


procedure TConfigStrings.WriteSectionValues(const Section:string; List:TStrings);
var i: Word;
begin
with List do if Count>0 then
  for i:= 0 to Pred(Count) do WriteString(Section,Names[i],Values[Names[i]]);
end; {~writesectionvalues}


procedure TConfigStrings.WriteHex(const ASection,Ident:string;
                                  const Value:LongInt;
                                  const Digits:Byte);
begin
WriteString(ASection,Ident,'0x'+LowerCase(IntToHex(Value,Digits)));
end; {~writehex}


//--procedures-----------------------------------------------------------


function GetConfigStg: String;           {d:\pp\n.CFG}
var Stg   : String;
    WinCfg: Boolean;
begin
Stg   := ChangeFileExt(ParamStr(0),'.ini');
WinCfg:= (FileGetAttr(ExtractFileDir(Stg)) and faReadOnly)>0;
if not WinCfg and ((FileGetAttr(Stg) and faReadOnly)>0) then
  begin
  FileSetAttr(Stg,0);
  WinCfg:= (FileGetAttr(ExtractFileDir(Stg)) and faReadOnly)>0;
  end;
if WinCfg then
  Stg:= ChangeFileExt(GetAppConfigDir(True)+ExtractFileName(Stg),'.ini');       //existance and accesability not guaranteed
Result:= Stg;
end; {getconfigstg}


end.
