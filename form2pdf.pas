unit form2pdf;

{$mode objfpc}{$H+}

{Copyright Alan Chamberlain 2020

This unit renders the text and image components of a form to a PDF using the
fcl-pdf package. The object is not to provide a pixel by pixel representation of
the form, but to record the text and image information. Multiline controls such
as TStringGrid and TMemo are printed out in their entirety. the unit is
modularised so new components can be added easily. Supported components are:
TForm
TLabel
TStaticText
TEdit
TSpinEdit
TFloatSpinEdit
TSpinEditEx
TFloatSpinEditEx
TDirectoryEdit
TFileNameEdit
TComboBox
TListBox
TStringGrid
TValueListEditor
TMemo (does not support word wrapping)
TCheckBox
TRadioButton
TImage
TChart
TShape (rectangle, rounded rect, ellipse)
TPageControl
TTabSheet
TGroupBox
TPanel
TRadioGroup
TCheckGroup

To use copy form2pdf.pas into your source directory and include Form2PDF in
your uses clause. Any visual control can be passed as a parent eg. TTabControl.

Licence: Apache v2.0

History
26/6/2020  Initial commit.
3/7/2020   Fix bottom margin pagination.
5/07/2020  (TvS):moved initialization of FormToPDF to initalization part of unit
6/7/2020   changed FormToPDF to function to return error code
           added control and filename checks
8/7/2020   add functionality to append pages to document, FDoc now global
13/7/2020  load and use system fonts
15/7/2020  add text alignment for labels
17/7/2020  add text alignment for spinedits
           add Panel caption
5/8/2020   add hide string grid columns
6/8/2020   fix string grid fixed cols bug
           add consistent margin schema
17/12/2020 use rounded rect for smoother appearance
           fix TStringGrid no columns bug
18/12/2020 fix TStringGrid extend beyond end of control}

interface

uses
   Classes, Forms, Graphics, Controls, SysUtils, fppdf;

var FDoc       :TPDFDocument;  {declare FDOC global so it can be accessed by caller}

function FormToPDF:integer;                     {initialise FDoc and check if fonts are available}
function FormToPDF(AControl: TControl):integer; {parse controls and append pages to Fdoc}
function FormToPDF(FileName:string):integer;    {use to save FDoc to PDF and reset FDoc}
function FormToPDF(AControl:TControl; FileName:string):integer; {parse controls and save and close Fdoc}

implementation

uses StdCtrls, ExtCtrls, ComCtrls,TAGraph, Grids, Spin, SpinEx, EditBtn,ValEdit,
     fpparsettf, fpttf, intfgraphics, StrUtils;

type TMargins = record
        H,                     {header margin}
        F,                     {footer margin}
        T,                     {top margin}
        B,                     {bottom margin}
        L,                     {left margin}
        R      :integer;       {right margin}
       end;

{Set Include files in "Project Options, Paths" to include
/usr/share/fpcsrc/packages/fcl-pdf/src for linux and
C:\lazarus\fpc\3.0.4\source\packages\fcl-pdf\src for windows
otherwise the fontmetrics_stdpdf.inc file will not be found}
{$I fontmetrics_stdpdf.inc }

procedure RecurseControls(AControl:TControl; FDoc:TPDFDocument; Page:TPDFPage; ftText:integer; Margins:TMargins); forward;
procedure ParseControls(AControl:TControl; FDoc:TPDFDocument; Page:TPDFPage; ftText:integer; Margins:TMargins); forward;

var FirstPage  :boolean;
    CustomPaper:TPDFPaper;

{------------------------------------------------------------------------------
Page and Document set up
------------------------------------------------------------------------------}

function FontsAvailable: Boolean;
begin
Result:= (gTTFontCache.Count > 0);
end;


function SetupPage(AControl:TControl; FDoc:TPDFDocument):TPDFPage;
var APage      :TPDFPage;

begin
APage := FDoc.Pages.AddPage;
if FDoc.DefaultPaperType = ptCustom then
   APage.Paper := CustomPaper;
APage.PaperType := FDoc.DefaultPaperType;
if APage.Orientation = ppoPortrait then     {work around to trigger adjustmatrix}
   begin
   APage.Orientation := ppoLandscape;
   APage.Orientation := ppoPortrait;
   end
  else
   begin
   APage.Orientation := ppoPortrait;
   APage.Orientation := ppoLandscape;
   end;

FDoc.Sections[0].AddPage(APage);            {only created one section}
Result := APage;
end;


function ColorToPDF(AColor:TColor):TARGBColor;
{Red and blue values appear to be swapped}
var c:         TColor;
begin
c := ColorToRGB(AColor);
Result := Red(c)*$10000 + Green(c)*$100 + Blue(c);
end;


function GetPDFPenStyle(APen:TPen):TPDFPenStyle;
{Unfortunately mapping is not 1:1 therefore this clumsy method}
begin
Result := ppsSolid;
case APen.Style of
   psDash:       Result := ppsDash;
   psDot:        Result := ppsDot;
   psDashDot:    Result := ppsDashDot;
   psDashDotDot: Result := ppsDashDotDot;
   end; {of case}
end;


procedure DrawVarBorder(AControl:TControl; APage:TPDFPage; DX,DY:integer; Margins:TMargins);
{draw rectangle around border}
var DW,DH      :integer;       {height and width to draw item}
begin
DW := AControl.Width;
DH := DY - (Margins.T + AControl.Top);
DX := Margins.L + AControl.Left;
DY := Margins.T + AControl.Top + DH;
APage.DrawRoundedRect(DX,DY,DW,DH,1,1,false,true);
end;


procedure DrawFixedBorder(AControl:TControl; APage:TPDFPage; Margins:TMargins);
var DX,DY,                     {position of item}
    DW,DH      :integer;       {height and width to draw item}
    Isfilled   :boolean;       {is the shape filled}

begin
IsFilled := false;
{draw rectangle around border}
DW := AControl.Width;
DH := AControl.Height;
DX := Margins.L + AControl.Left;
DY := Margins.T + AControl.Top + DH;
if AControl.Color <> clDefault then
   begin
   APage.SetColor(ColorToPDF(AControl.Color),false);
   IsFilled := true;
   end;
APage.DrawRoundedRect(DX,DY,DW,DH,1,1,IsFilled,true);
end;


procedure SetControlFont(AControl:TControl; APage:TPDFPage; var IDX,fSize:integer);
var fFamily,                   {font family}
    fName      :string;        {font name}
    fData      :TFontData;
    lFC        :TFPFontCacheItem;{font item}
    fBold,
    fItalic    :boolean;

begin
fData := GetFontData(AControl.Font.Handle);
fSize := abs(fData.Height);
if fSize = 0 then fSize := 12; {windows returns default control size of 0}
fName := fData.Name;
if Graphics.fsBold in fData.Style then fBold := true else fBold := false;
if Graphics.fsItalic in fData.Style then fItalic := true else fItalic := false;

lFC := gTTFontCache.Find(fName, fBold, fItalic);
if Assigned(lFC) then
   begin                       {use system fonts}
   {we need to further specialise FamilyName otherwise base font is loaded}
   fFamily := lFC.FamilyName;
   if fBold then fFamily := lFC.FamilyName + '-Bold';
   if fItalic then fFamily := lFC.FamilyName + '-Italic';
   if fBold and fItalic then fFamily := lFC.FamilyName + '-BoldItalic';
   IDX := APage.Document.AddFont(lFC.FileName,fFamily);
   end
  else                         {fall back on internal pdf fonts}
   begin
   if Pos('SANS',UpCase(fname)) > 0 then
      begin
      if fBold then IDX := APage.Document.AddFont('Helvetica-Bold');
      if fItalic then IDX := APage.Document.AddFont('Helvetica-Oblique');
      if fBold and fItalic then IDX := APage.Document.AddFont('Helvetica-BoldOblique');
      if not fBold and not fItalic then IDX := APage.Document.AddFont('Helvetica');
      end
     else
      begin
      if fBold then IDX := APage.Document.AddFont('Times-Bold');
      if fItalic then IDX := APage.Document.AddFont('Times-Italic');
      if fBold and fItalic then IDX := APage.Document.AddFont('Times-BoldItalic');
      if not fBold and not fItalic then IDX := APage.Document.AddFont('Times-Roman');
      end;
   end;
APage.SetColor(ColorToPDF(AControl.Font.Color),false);
APage.SetFont(IDX,fSize);
end;


function GetFontTextWidth(AText:string; APage:TPDFPage; IDX,fSize:integer):double;
{string width in points}
var I          :integer;
    lWidth     :double;
    lFC        :TFPFontCacheItem;{font item}
    fName      :string;
    AFont      :TPDFFont;
    FontWArr   :TPDFFontWidthArray;
    fBold,
    fItalic    :boolean;
begin
lWidth := 0;
if IDX >= 0 then
   begin
   AFont := APage.Document.Fonts[IDX];
   FName := AFont.Name;
   if AFont.IsStdFont then         {can't use protected members to get this so have to do it ourselves}
      begin
      case fName of
         'Courier':                 FontWArr := FONT_COURIER_FULL;
         'Courier-Bold':            FontWArr := FONT_COURIER_FULL;
         'Courier-Oblique':         FontWArr := FONT_COURIER_FULL;
         'Courier-BoldOblique':     FontWArr := FONT_COURIER_FULL;
         'Helvetica':               FontWArr := FONT_HELVETICA_ARIAL;
         'Helvetica-Bold':          FontWArr := FONT_HELVETICA_ARIAL_BOLD;
         'Helvetica-Oblique':       FontWArr := FONT_HELVETICA_ARIAL_ITALIC;
         'Helvetica-BoldOblique':   FontWArr := FONT_HELVETICA_ARIAL_BOLD_ITALIC;
         'Times-Roman':             FontWArr := FONT_TIMES;
         'Times-Bold':              FontWArr := FONT_TIMES_BOLD;
         'Times-Italic':            FontWArr := FONT_TIMES_ITALIC;
         'Times-BoldItalic':        FontWArr := FONT_TIMES_BOLD_ITALIC;
         'Symbol':                  FontWArr := FONT_SYMBOL;
         'ZapfDingbats':            FontWArr := FONT_ZAPFDINGBATS;
         end;
      lWidth := 0;
      for I:= 1 to length(AText) do
         lWidth := lWidth + FontWArr[ord(Atext[I])];
      lWidth := lWidth*fSize*72/(96*1540);
      end
     else
      begin
      if Pos('BOLD',upcase(fName)) > 0 then fBold := true else fBold := false;
      if Pos('ITALIC',upcase(fname)) > 0 then fItalic := true else fItalic := false;
      fname := Copy2Symb(fname,'-');
      lFC := gTTFontCache.Find(fName, fBold, fItalic);
      if Assigned(lFC) then
         begin
         lWidth := lFC.TextWidth(AText,fSize);       {TextWidth gives size in screen pixels}
         end
        else
         lWidth := -1;
      end;
   end;
Result := lWidth;
end;

{------------------------------------------------------------------------------
Component Procedures
------------------------------------------------------------------------------}

procedure Header2PDF(cForm:Tform; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DW         :integer;       {height and width to draw item}

begin
SetControlFont(cForm,APage,IDX,fSize);
DW := Round(GetFontTextWidth(cForm.Caption,APage,IDX,fSize));
DX := (APage.Paper.W - DW) div 2;                         {fix this to take into account margins}
DY := Margins.T + (Margins.H - fSize) div 2 + fSize;      {put caption halfway in header}
APage.SetFont(IDX, fSize);
APage.SetColor(cForm.Font.Color, false);
APage.WriteText(DX,DY,cForm.Caption);
end;


{TLabel}
procedure LabelToPDF(cLabel:TLabel; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH         :integer;       {height and width to draw item}
begin
if cLabel.Visible then
   begin
   SetControlFont(cLabel,APage,IDX,fSize);
   DH := cLabel.Height;
   case cLabel.Alignment of
      taLeftJustify : DX := Margins.L + cLabel.Left;
      taCenter      : DX := Margins.L + cLabel.Left + Round((cLabel.Width
                           - GetFontTextWidth(cLabel.Caption,APage,IDX,fSize))/2);
      taRightJustify: DX := Margins.L + cLabel.Left + cLabel.Width
                           - Round(GetFontTextWidth(cLabel.Caption,APage,IDX,fSize));
      end; {of case}
   DY := Margins.T + cLabel.Top + (cLabel.Height + fSize) div 2;
   APage.WriteText(DX,DY,cLabel.Caption);
   end;
end;


{TStaticText}
procedure StaticTextToPDF(cLabel:TStaticText; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH         :integer;       {height and width to draw item}
begin
if cLabel.Visible then
   begin
   SetControlFont(cLabel,APage,IDX,fsize);
   DH := cLabel.Height;
   case cLabel.Alignment of
      taLeftJustify : DX := Margins.L + cLabel.Left;
      taCenter      : DX := Margins.L + cLabel.Left + Round((cLabel.Width
                           - GetFontTextWidth(cLabel.Caption,APage,IDX,fSize))/2);
      taRightJustify: DX := Margins.L + cLabel.Left + cLabel.Width
                           - Round(GetFontTextWidth(cLabel.Caption,APage,IDX,fSize));
      end; {of case}
   DY := Margins.T + cLabel.Top + (cLabel.Height + fSize) div 2;
   APage.WriteText(DX,DY,cLabel.Caption);
   end;
end;


{TImage}
procedure ImageToPDF(cImage:TImage; ADoc:TPDFDocument; APage:TPDFPage; Margins:TMargins);
var IDX,
    DX,DY,                     {x and y pos to draw item}
    DH,DW      :integer;       {height and width to draw item}
    pdfImage   :TPDFImageItem;
    fpBitmap   :TLazIntfImage;
begin
if cImage.Visible then
   begin
   pdfImage := ADoc.Images.AddImageItem;
   IDX := ADoc.Images.Count - 1;
   fpBitMap := TLazIntfImage.Create(0,0);
   fpBitMap.LoadFromBitmap(cImage.Picture.Bitmap.Handle,cImage.Picture.Bitmap.MaskHandle);
   pdfImage.Image := fpBitMap;
   pdfImage.OwnsImage := true;
   DW := cImage.Width;
   DH := cImage.Height;
   DX := Margins.L + cImage.Left;
   DY := Margins.T + cImage.Top + DH;
   APage.DrawImageRawSize(DX,DY,DW,DH,IDX);
   end;
end;


{TChart}
procedure ChartToPDF(cChart:TChart; ADoc:TPDFDocument; APage:TPDFPage; Margins:TMargins);
{for now copy chart to bitmap. use fpVectorial later?}
var IDX,
    DX,DY,                     {x and y pos to draw item}
    DH,DW      :integer;       {height and width to draw item}
    pdfImage   :TPDFImageItem;
    fpBitmap   :TLazIntfImage;
    BitMap     :TBitMap;
begin
if cChart.Visible then
   begin
   pdfImage := ADoc.Images.AddImageItem;
   IDX := ADoc.Images.Count - 1;
   fpBitMap := TLazIntfImage.Create(0,0);
   BitMap := cChart.SaveToImage(TBitMap) as TBitMap;
   fpBitmap.LoadFromBitmap(BitMap.Handle,BitMap.MaskHandle);
   pdfImage.Image := fpBitMap;
   pdfImage.OwnsImage := true;
   DW := cChart.Width;
   DH := cChart.Height;
   DX := Margins.L + cChart.Left;
   DY := Margins.T + cChart.Top + DH;
   APage.DrawImageRawSize(DX,DY,DW,DH,IDX);
   BitMap.Free;
   end;
end;


{TShape}
procedure ShapeToPDF(cShape:TShape; APage:TPDFPage; Margins:TMargins);
{Supported shapes:
Rectangle
Rounded Rect
Ellipse
}
var LW,                        {line width}
    DX,DY,                     {x and y pos to draw item}
    DH,DW      :integer;       {height and width to draw item}
    Isfilled   :boolean;       {is the shape filled}

begin
if cShape.Visible then
   begin
   Isfilled := true;
   APage.SetColor(ColorToPDF(cShape.Pen.Color),true);
   APage.SetPenStyle(GetPDFPenStyle(cShape.Pen));
   APage.SetColor(ColorToPDF(cShape.Brush.Color),false);
   if cShape.Brush.Style = bsClear then Isfilled := false;
   DW := cShape.Width;
   DH := cShape.Height;
   DX := Margins.L + cShape.Left;
   DY := Margins.T + cShape.Top + DH;
   LW := cShape.Pen.Width;
   case cShape.Shape of
      stRectangle: APage.DrawRect(DX,DY,DW,DH,LW,Isfilled,true);
      stRoundRect: APage.DrawRoundedRect(DX,DY,DW,DH,(DW/20 + DH/20),LW,Isfilled,true);
      stEllipse  : APage.DrawEllipse(DX,DY,DW,DH,LW,IsFilled,true);
      end; {case}
   end;
end;


{TGroupBox}
procedure GroupBoxToPDF(cGroupBx:TGroupBox; FDoc:TPDFDocument; APage:TPDFPage; IDX:integer; Margins:TMargins);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
begin
if cGroupBx.Visible then
   begin
   DrawFixedBorder(cGroupBx,APage,Margins);

   {write groupbox caption}
   SetControlFont(cGroupBx,APage,IDX,fsize);
   DX := Margins.L + cGroupBx.Left + 2;
   DY := Margins.T + cGroupBx.Top + fSize + 2;
   APage.WriteText(DX,DY,cGroupBx.Caption);

   {draw components}
   Margins.L := Margins.L + cGroupBx.Left;
   Margins.T := Margins.T + cGroupBx.Top + cGroupBx.Height - cGroupBx.ClientHeight;
   for I:=0 to cGroupBx.ControlCount - 1 do
      ParseControls(cGroupBx.Controls[I],FDoc,APage,IDX,Margins);
   end;
end;


{TPanel}
procedure PanelToPDF(cPanel:TPanel; FDoc:TPDFDocument; APage:TPDFPage; IDX:integer; Margins:TMargins);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
begin
if cPanel.Visible then
   begin
   DrawFixedBorder(cPanel,APage,Margins);

   {write groupbox caption}
   SetControlFont(cPanel,APage,IDX,fsize);
   case cPanel.Alignment of
      taLeftJustify : DX := Margins.L + cPanel.Left;
      taCenter      : DX := Margins.L + cPanel.Left + Round((cPanel.Width
                           - GetFontTextWidth(cPanel.Caption,APage,IDX,fSize))/2);
      taRightJustify: DX := Margins.L + cPanel.Left + cPanel.Width - 2
                           - Round(GetFontTextWidth(cPanel.Caption,APage,IDX,fSize));
      end; {of case}
   DY := Margins.T + cPanel.Top + (cPanel.Height + fSize) div 2;
   APage.WriteText(DX,DY,cPanel.Caption);

   {draw components}
   Margins.L := Margins.L + cPanel.Left;
   Margins.T := Margins.T + cPanel.Top;
   for I:=0 to cPanel.ControlCount - 1 do
      ParseControls(cPanel.Controls[I],FDoc,APage,IDX,Margins);
   end;
end;


{TEdit}
procedure EditToPDF(cEdit:TEdit; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH         :integer;       {height and width to draw item}
begin
if cEdit.Visible then
   begin
   DrawFixedBorder(cEdit,APage,Margins);

   {write edit caption}
   SetControlFont(cEdit,APage,IDX,fsize);
   DH := cEdit.Height;
   case cEdit.Alignment of
      taLeftJustify : DX := Margins.L + cEdit.Left;
      taCenter      : DX := Margins.L + cEdit.Left + Round((cEdit.Width
                           - GetFontTextWidth(cEdit.Caption,APage,IDX,fSize))/2);
      taRightJustify: DX := Margins.L + cEdit.Left + cEdit.Width - 2
                           - Round(GetFontTextWidth(cEdit.Caption,APage,IDX,fSize));
      end; {of case}
   DY := Margins.T + cEdit.Top + (DH + fSize) div 2;
   APage.WriteText(DX + 2,DY,cEdit.Caption);
   end;
end;

{Note: TspinEdit, TFloatSpinEdit, TSpinEditEx and TFloatSpinEditEx are programatically
the same, but if the -CR compiler switch is set we can only typecast to the exact
type. Base class of TSpinEditEx and TFloatSpinEditEx is not specialised so we
cannot typecast to it.}

{TCustomFloatSpinEdit}
procedure CustomFloatSpinEditToPDF(cSpinEd:TCustomFloatSpinEdit; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH,                        {height to draw item}
    X1,X2,
    Y1,Y2      :integer;

begin
if cSpinEd.Visible then
   begin
   DrawFixedBorder(cSpinEd,APage,Margins);

   {write edit caption}
   SetControlFont(cSpinEd,APage,IDX,fsize);
   DH := cSpinEd.Height;
   case cSpinEd.Alignment of
      taLeftJustify : DX := Margins.L + cSpinEd.Left;
      taCenter      : DX := Margins.L + cSpinEd.Left + Round((cSpinEd.Width - DH/2
                         - GetFontTextWidth(cSpinEd.Caption,APage,IDX,fSize))/2);
      taRightJustify: DX := Margins.L + cSpinEd.Left + cSpinEd.Width  - DH div 2 - 4
                          - Round(GetFontTextWidth(cSpinEd.Caption,APage,IDX,fSize));
      end; {of case}
   DY := Margins.T + cSpinEd.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cSpinEd.Caption);

   {draw separator line}
   X1 := Margins.L + cSpinEd.Left + cSpinEd.Width - DH div 2;
   Y1 := Margins.T + cSpinEd.Top;
   Y2 := Y1 + DH;
   APage.DrawLine(X1,Y1,X1,Y2,1,true);

   {draw up tick}
   X1 := Margins.L + cSpinEd.Left + cSpinEd.Width - DH div 2 + 2;
   Y1 := Margins.T + cSpinEd.Top + 2;
   X2 := X1 + DH div 4 - 2;
   Y2 := Y1 + DH div 3 - 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);

   {draw down tick}
   Y1 := Margins.T + cSpinEd.Top + DH - 2;
   Y2 := Y1 - DH div 3 + 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);
   end;
end;


{TSpinEdit}
procedure SpinEditToPDF(cSpinEd:TSpinEdit; APage:TPDFPage; IDX:integer; Margins:TMargins);
begin
CustomFloatSpinEditToPDF(TCustomFloatSpinEdit(cSpinEd),APage,IDX,Margins)
end;


{TFloatSpinEdit}
procedure FloatSpinEditToPDF(cSpinEd:TFloatSpinEdit; APage:TPDFPage; IDX:integer; Margins:TMargins);
begin
CustomFloatSpinEditToPDF(TCustomFloatSpinEdit(cSpinEd),APage,IDX,Margins)
end;


{TSpinEditEx}
procedure SpinEditExToPDF(cSpinEd:TSpinEditEx; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH,                        {height to draw item}
    X1,X2,
    Y1,Y2      :integer;

begin
if cSpinEd.Visible then
   begin
   DrawFixedBorder(cSpinEd,APage,Margins);

   {write edit caption}
   SetControlFont(cSpinEd,APage,IDX,fsize);
   DH := cSpinEd.Height;
   case cSpinEd.Alignment of
      taLeftJustify : DX := Margins.L + cSpinEd.Left;
      taCenter      : DX := Margins.L + cSpinEd.Left + Round((cSpinEd.Width - DH/2
                           - GetFontTextWidth(cSpinEd.Caption,APage,IDX,fSize))/2);
      taRightJustify: DX := Margins.L + cSpinEd.Left + cSpinEd.Width - DH div 2 - 4
                           - Round(GetFontTextWidth(cSpinEd.Caption,APage,IDX,fSize));
      end; {of case}
   DY := Margins.T + cSpinEd.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cSpinEd.Caption);

   {draw separator line}
   X1 := Margins.L + cSpinEd.Left + cSpinEd.Width - DH div 2;
   Y1 := Margins.T + cSpinEd.Top;
   Y2 := Y1 + DH;
   APage.DrawLine(X1,Y1,X1,Y2,1,true);

   {draw up tick}
   X1 := Margins.L + cSpinEd.Left + cSpinEd.Width - DH div 2 + 2;
   Y1 := Margins.T + cSpinEd.Top + 2;
   X2 := X1 + DH div 4 - 2;
   Y2 := Y1 + DH div 3 - 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);

   {draw down tick}
   Y1 := Margins.T + cSpinEd.Top +DH - 2;
   Y2 := Y1 - DH div 3 + 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);
   end;
end;


{TFloatSpinEditEx}
procedure FloatSpinEditExToPDF(cSpinEd:TFloatSpinEditEx; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH,                        {height to draw item}
    X1,X2,
    Y1,Y2      :integer;

begin
if cSpinEd.Visible then
   begin
   DrawFixedBorder(cSpinEd,APage,Margins);

   {write edit caption}
   SetControlFont(cSpinEd,APage,IDX,fsize);
   DH := cSpinEd.Height;
   case cSpinEd.Alignment of
      taLeftJustify : DX := Margins.L + cSpinEd.Left;
      taCenter      : DX := Margins.L + cSpinEd.Left + Round((cSpinEd.Width - DH/2
                           - GetFontTextWidth(cSpinEd.Caption,APage,IDX,fSize))/2);
      taRightJustify: DX := Margins.L + cSpinEd.Left + cSpinEd.Width  - DH div 2 - 4
                           - Round(GetFontTextWidth(cSpinEd.Caption,APage,IDX,fSize));
      end; {of case}
   DY := Margins.T + cSpinEd.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cSpinEd.Caption);

   {draw separator line}
   X1 := Margins.L + cSpinEd.Left + cSpinEd.Width - DH div 2;
   Y1 := Margins.T + cSpinEd.Top;
   Y2 := Y1 + DH;
   APage.DrawLine(X1,Y1,X1,Y2,1,true);

   {draw up tick}
   X1 := Margins.L + cSpinEd.Left + cSpinEd.Width - DH div 2 + 2;
   Y1 := Margins.T + cSpinEd.Top + 2;
   X2 := X1 + DH div 4 - 2;
   Y2 := Y1 + DH div 3 - 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);

   {draw down tick}
   Y1 := Margins.T + cSpinEd.Top +DH - 2;
   Y2 := Y1 - DH div 3 + 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);
   end;
end;


{TDirectoryEdit}
procedure DirEditToPDF(cDirEdit:TDirectoryEdit; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH         :integer;       {height and width to draw item}
begin
if cDirEdit.Visible then
   begin
   DrawFixedBorder(cDirEdit,APage,Margins);

   {write edit caption}
   SetControlFont(cDirEdit,APage,IDX,fsize);
   DH := cDirEdit.Height;
   DX := Margins.L + cDirEdit.Left;
   DY := Margins.T + cDirEdit.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cDirEdit.Caption);
   end;
end;


{TFileNameEdit}
procedure FileEditToPDF(cFileEdit:TFileNameEdit; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH         :integer;       {height and width to draw item}
begin
if cFileEdit.Visible then
   begin
   DrawFixedBorder(cFileEdit,APage,Margins);

   {write edit caption}
   SetControlFont(cFileEdit,APage,IDX,fsize);
   DH := cFileEdit.Height;
   DX := Margins.L + cFileEdit.Left;
   DY := Margins.T + cFileEdit.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cFileEdit.Caption);
   end;
end;


{TComboBox}
procedure ComboBoxToPDF(cCmboBx:TComboBox; APage:TPDFPage; IDX:integer; Margins:TMargins);
{prints selected text only, not the drop down list}
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DW,DH,                     {height and width to draw item}
    X1,X2,
    Y1,Y2      :integer;

begin
if cCmboBx.Visible then
   begin
   DrawFixedBorder(cCmboBx,APage,Margins);

   {write edit caption}
   SetControlFont(cCmboBx,APage,IDX,fsize);
   DH := cCmboBx.Height;
   DW := cCmboBx.Width;
   DX := Margins.L + cCmboBx.Left;
   DY := Margins.T + cCmboBx.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cCmboBx.Caption);

   {draw down tick}
   X1 := DX + DW - DH + 2;
   Y1 := Margins.T + cCmboBx.Top + 2*DH div 3;
   X2 := X1 + DH div 2 - 2;
   Y2 := Y1 - DH div 3 + 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 2 - 2,Y1,X2 + DH div 2 - 2,Y2,1,true);
   end;
end;


{TMemo}
procedure MemoToPDF(cMemo:TMemo; FDoc:TPDFDocument; APage:TPDFPage; IDX:integer; Margins:TMargins);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
    fp         :boolean;       {first page of control}

begin
if cMemo.Visible then
   begin
   {write text, no line wrapping}
   fp := true;
   SetControlFont(cMemo,APage,IDX,fsize);
   DX := Margins.L + cMemo.Left + 2;
   DY := Margins.T + cMemo.Top + fsize + 2;
   for I:=0 to cMemo.Lines.Count - 1 do
      begin
      APage.WriteText(DX,DY,cMemo.Lines[I]);
      DY := DY + fSize;
      if (DY > APage.Paper.Printable.B) or ((DY > Margins.T + cMemo.Top + cMemo.Height) and fp) then
         begin
         DrawVarBorder(cMemo,APage,DX,DY,Margins);
         APage := SetupPage(cMemo,FDoc);
         SetControlFont(cMemo,APage,IDX,fsize);
         DY := Margins.T + fsize + 2;
         fp := false;
         end;
      end;
   DrawVarBorder(cMemo,APage,DX,DY,Margins);
   end;
end;


{TListBox}
procedure ListBoxToPDF(cLstBx:TListBox; FDoc:TPDFDocument; APage:TPDFPage; IDX:integer; Margins:TMargins);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
    fp         :boolean;       {first page of control}

begin
if cLstBx.Visible then
   begin
   fp := true;
   SetControlFont(cLstBx,APage,IDX,fsize);
   DX := Margins.L + cLstBx.Left + 2;
   DY := Margins.T + cLstBx.Top + fsize + 2;
   for I:=0 to cLstBx.Items.Count - 1 do
      begin
      APage.WriteText(DX,DY,cLstBx.Items[I]);
      DY := DY + fSize;
      if (DY > APage.Paper.Printable.B) or ((DY > Margins.T + cLstBx.Top + cLstBx.Height) and fp) then
         begin
         DrawVarBorder(cLstBx,APage,DX,DY,Margins);
         APage := SetupPage(cLstBx,FDoc);
         SetControlFont(cLstBx,APage,IDX,fsize);
         DY := Margins.T + cLstBx.Top + fsize + 2;
         fp := false;
         end;
      end;

   DrawVarBorder(cLstBx,APage,DX,DY,Margins);
   end;
end;


{TStringGrid}
procedure StringGridToPDF(cStrGrd:TStringGrid; FDoc:TPDFDocument; APage:TPDFPage; IDX:integer; Margins:TMargins);
var I,J,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
    fp         :boolean;       {first page of control}

begin
if cStrGrd.Visible then
   begin
   fp := true;
   SetControlFont(cStrGrd,APage,IDX,fsize);
   DX := Margins.L + cStrGrd.Left + 2;
   DY := Margins.T + cStrGrd.Top + fsize + 2;
   for I:=0 to cStrGrd.RowCount - 1 do
      begin
      for J:=0 to cStrGrd.FixedCols - 1 do        {write fixed cols}
         begin
         APage.WriteText(DX,DY,cStrGrd.Cells[J,I]);
         DX := DX + cStrGrd.ColWidths[J];
         end;

      for J:=cStrGrd.FixedCols to cStrGrd.ColCount - 1 do
         if (cStrGrd.Columns.Count > 0) then
            begin
            if (cStrGrd.Columns[J - cStrGrd.FixedCols].Visible) then
               begin
               if I < cStrGrd.FixedRows then
                  APage.WriteText(DX,DY,cStrGrd.Columns[J - cStrGrd.FixedCols].Title.Caption)
                 else
                  APage.WriteText(DX,DY,cStrGrd.Cells[J,I]);
               DX := DX + cStrGrd.ColWidths[J];
               end;
            end
           else
            begin
            APage.WriteText(DX,DY,cStrGrd.Cells[J,I]);
            DX := DX + cStrGrd.ColWidths[J];
            end;

      DX := Margins.L + cStrGrd.Left + 2;
      DY := DY + cStrGrd.RowHeights[I];
      if (DY > APage.Paper.Printable.B) or
         ((DY > Margins.T + cStrGrd.Top + cStrGrd.Height) and fp) then
         begin
         if fp then DY := Margins.T + cStrGrd.Top + cStrGrd.Height;
         DrawVarBorder(cStrGrd,APage,DX,DY,Margins);
         APage := SetupPage(cStrGrd,FDoc);
         SetControlFont(cStrGrd,APage,IDX,fsize);
         DY := Margins.T + cStrGrd.Top + fsize + 2;
         fp := false;
         end;
      end;
   DrawVarBorder(cStrGrd,APage,DX,DY,Margins);
   end;
end;


{TValueListEditor}
procedure ValueListToPDF(cValueList:TValueListEditor; FDoc:TPDFDocument; APage:TPDFPage; IDX:integer; Margins:TMargins);
var I,SI,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
    fp         :boolean;       {first page of control}
    AKey,
    AValue     :string;

begin
if cValueList.Visible then
   begin
   fp := true;
   SI := 0;
   SetControlFont(cValueList,APage,IDX,fsize);
   DX := Margins.L + cValueList.Left + 2;
   DY := Margins.T + cValueList.Top + fsize + 2;

   {write column title if there is one}
   if cValueList.TitleCaptions.Count > 0 then
      begin
      APage.WriteText(DX,DY,cValueList.TitleCaptions[0] + ' = ' + cValueList.TitleCaptions[1]);
      DY := DY + fSize;
      SI := 1;
      end;

   {write key value pairs}
   for I:=SI to cValueList.RowCount - 1 do
      begin
      AKey := cValueList.Cells[0,I];
      AValue := cValueList.Cells[1,I];
      APage.WriteText(DX,DY,Akey + ' = ' + AValue);
      DY := DY + fSize;
      if (DY > APage.Paper.Printable.B) or ((DY > Margins.T + cValueList.Top + cValueList.Height) and fp) then
         begin
         if fp then DY := Margins.T + cValueList.Top + cValueList.Height;
         DrawVarBorder(cValueList,APage,DX,DY,Margins);
         APage := SetupPage(cValueList,FDoc);
         SetControlFont(cValueList,APage,IDX,fsize);
         DY := Margins.T + cValueList.Top + fsize + 2;
         fp := false;
         end;
      end;

   DrawVarBorder(cValueList,APage,DX,DY,Margins);
   end;
end;


{TCheckBox}
procedure CheckBoxToPDF(cCheckBx:TCheckBox; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DW,DH      :integer;       {height and width to draw item}
begin
if cCheckBx.Visible then
   begin
   {write caption}
   SetControlFont(cCheckBx,APage,IDX,fsize);
   DH := cCheckbx.Height;
   DX := Margins.L + cCheckBx.Left + Round(fSize*1.2);       {shift text right to allow for circle}
   DY := Margins.T + cCheckBx.Top + (DH + fsize) div 2;
   APage.WriteText(DX,DY,cCheckbx.Caption);

   {draw tick box}
   DX := Margins.L + cCheckbx.Left;
   DY := DY + 1;
   DW := fSize;                                {want a square box same size as text}
   APage.DrawRect(DX,DY,DW,DW,1,false,true);

   {draw cross}
   if cCheckBx.Checked then
      begin
      DX := DX + 2;
      DY := DY - 2;
      DW := DX + fSize - 4;
      DH := DY - fSize + 4;
      APage.DrawLine(DX,DY,DW,DH,1,true);
      APage.DrawLine(DX,DH,DW,DY,1,true);
      end;
   end;
end;


{TRadioButton}
procedure RadioButToPDF(cRadioBtn:TRadioButton; APage:TPDFPage; IDX:integer; Margins:TMargins);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DW,DH      :integer;       {height and width to draw item}
begin
if cRadioBtn.Visible then
   begin
   {write caption}
   SetControlFont(cRadioBtn,APage,IDX,fsize);
   DH := cRadioBtn.Height;
   DX := Margins.L + cRadioBtn.Left + Round(fSize*1.2);       {shift text right to allow for circle}
   DY := Margins.T + cRadioBtn.Top + (DH + fsize) div 2;
   APage.WriteText(DX,DY,cRadioBtn.Caption);

   {draw outer circle}
   DX := Margins.L + cRadioBtn.Left;
   DY := DY + 1;
   DW := fsize;                                 {same size as text}
   APage.DrawEllipse(DX,DY,DW,DW,1,false,true);

   {draw inner circle}
   if cRadioBtn.Checked then
      begin
      DX := DX + 4;
      DY := DY - 4;
      DW := DW - 8;
      APage.DrawEllipse(DX,DY,DW,DW,1,true,true);
      end;
   end;
end;


{TRadioGroup}
procedure RadioGroupToPDF(cRadioGrp:TRadioGroup; FDoc:TPDFDocument; APage:TPDFPage; IDX:integer; Margins:TMargins);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
begin
if cRadioGrp.Visible then
   begin
   DrawFixedBorder(cRadioGrp,APage,Margins);

   {write caption}
   SetControlFont(cRadioGrp,APage,IDX,fsize);
   DX := Margins.L + cRadioGrp.Left + 2;
   DY := Margins.T + cRadioGrp.Top + fSize + 2;
   APage.WriteText(DX,DY,cRadioGrp.Caption);

   {draw components}
   Margins.L := Margins.L + cRadioGrp.Left;
   Margins.T := Margins.T + cRadioGrp.Top + cRadioGrp.Height - cRadioGrp.ClientHeight;
   for I:=0 to cRadioGrp.ControlCount - 1 do
      ParseControls(cRadioGrp.Controls[I],FDoc,APage,IDX,Margins);
   end;
end;


{TCheckGroup}
procedure CheckGroupToPDF(cCheckGrp:TCheckGroup; FDoc:TPDFDocument; APage:TPDFPage; IDX:integer; Margins:TMargins);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
begin
if cCheckGrp.Visible then
   begin
   DrawFixedBorder(cCheckGrp,APage,Margins);

   {write caption}
   SetControlFont(cCheckGrp,APage,IDX,fsize);
   DX := Margins.L + cCheckGrp.Left + 2;
   DY := Margins.T + cCheckGrp.Top + fSize + 2;
   APage.WriteText(DX,DY,cCheckGrp.Caption);

   {draw components}
   Margins.L := Margins.L + cCheckGrp.Left;
   Margins.T := Margins.T + cCheckGrp.Top + cCheckGrp.Height - cCheckGrp.ClientHeight;
   for I:=0 to cCheckGrp.ControlCount - 1 do
      ParseControls(cCheckGrp.Controls[I],FDoc,APage,IDX,Margins);
   end;
end;


procedure TabSheetToPDF(cTabSht:TTabSheet; FDoc:TPDFDocument; APage:TPDFPage; IDX:integer; Margins:TMargins);
var I          :integer;
begin
if cTabSht.TabVisible then
   begin
   if not FirstPage then APage := SetupPage(cTabSht,FDoc)
      else FirstPage := false;
   for I:=0 to cTabSht.ControlCount - 1 do
      ParseControls(cTabSht.Controls[I],FDoc,APage,IDX,Margins);
   end;
end;


{------------------------------------------------------------------------------
Form parsing functions
-------------------------------------------------------------------------------}

procedure ParseControls(AControl:TControl; FDoc:TPDFDocument; Page:TPDFPage; ftText:integer; Margins:TMargins);
{List of simple controls to take action on}
begin
if AControl is TLabel then                {TLabel}
   LabelToPDF(TLabel(AControl),Page,ftText,Margins);

if AControl is TStaticText then           {TStaticText}
   StaticTextToPDF(TStaticText(AControl),Page,ftText,Margins);

if AControl is TEdit then                 {TEdit}
   EditToPDF(TEdit(AControl),Page,ftText,Margins);

if AControl is TFloatSpinEdit then        {TFloatSpinEdit}
   FloatSpinEditToPDF(TFloatSpinEdit(AControl),Page,ftText,Margins);

if AControl is TSpinEdit then             {TSpinEdit}
   SpinEditToPDF(TSpinEdit(AControl),Page,ftText,Margins);

if AControl is TSpinEditEx then           {TSpinEditEx}
   SpinEditExToPDF(TSpinEditEx(AControl),Page,ftText,Margins);

if AControl is TFloatSpinEditEx then      {TFloatSpinEditEx}
   FloatSpinEditExToPDF(TFloatSpinEditEx(AControl),Page,ftText,Margins);

if AControl is TDirectoryEdit then        {TDirectoryEdit}
   DirEditToPDF(TDirectoryEdit(AControl),Page,ftText,Margins);

if AControl is TFileNameEdit then         {TFileNameEdit}
   FileEditToPDF(TFileNameEdit(AControl),Page,ftText,Margins);

if AControl is TComboBox then             {TComboBox}
   ComboBoxToPDF(TComboBox(AControl),Page,ftText,Margins);

if AControl is TListBox then              {TListBox}
   ListBoxToPDF(TListBox(AControl),FDoc,Page,ftText,Margins);

if AControl is TStringGrid then           {TStringGrid}
   StringGridToPDF(TStringGrid(AControl),FDoc,Page,ftText,Margins);

if AControl is TValueListEditor then      {TValueListEditor}
   ValueListToPDF(TValueListEditor(AControl),FDoc,Page,ftText,Margins);

if AControl is TMemo then                 {TMemo}
   MemoToPDF(TMemo(AControl),FDoc,Page,ftText,Margins);

if AControl is TCheckBox then             {TCheckBox}
   CheckBoxToPDF(TCheckBox(AControl),Page,ftText,Margins);

if AControl is TRadioButton then          {TRadioButton}
   RadioButToPDF(TRadioButton(AControl),Page,ftText,Margins);

if AControl is TImage then                {TImage}
   ImageToPDF(TImage(AControl),FDoc,Page,Margins);

if AControl is TChart then                {TChart}
   ChartToPDF(TChart(AControl),FDoc,Page,Margins);

if AControl is TShape then                {TShape}
   ShapeToPDF(TShape(AControl),Page,Margins);

if AControl is TPageControl then          {TPageControl}
   RecurseControls(AControl,FDoc,Page,ftText,Margins);

if AControl is TTabSheet then             {TTabSheet}
   RecurseControls(AControl,FDoc,Page,ftText,Margins);

if AControl is TGroupBox then             {TGroupBox}
   RecurseControls(AControl,FDoc,Page,ftText,Margins);

if AControl is TPanel then                {TPanel}
   RecurseControls(AControl,FDoc,Page,ftText,Margins);

if AControl is TRadioGroup then           {TRadioGroup}
   RecurseControls(AControl,FDoc,Page,ftText,Margins);

if AControl is TCheckGroup then           {TCheckGroup}
   RecurseControls(AControl,FDoc,Page,ftText,Margins);
end;


procedure RecurseControls(AControl:TControl; FDoc:TPDFDocument; Page:TPDFPage; ftText:integer; Margins:TMargins);
{Iterate through components and print them to PDF, recurse into nested controls.
Terrible programming but use exit to emulate case and increase efficiency}
var cForm      :TForm;
    cPageCtrl  :TPageControl;
    I          :integer;
begin
if AControl is TForm then                 {TForm}
   begin
   cForm := AControl as TForm;
   {write page titel centered}
   Header2PDF(cForm,Page,ftText,Margins);
   Margins.T := Margins.T + Margins.H;
   for I:=0 to cForm.ControlCount - 1 do
      ParseControls(cForm.Controls[I],FDoc,Page,ftText,Margins);
   exit;
   end;
if AControl is TPageControl then          {TPageControl}
   begin
   cPageCtrl := AControl as TPageControl;
   for I:=0 to cPageCtrl.ControlCount - 1 do
      ParseControls(cPageCtrl.Controls[I],FDoc,Page,ftText,Margins);
   exit;
   end;
if AControl is TTabSheet then             {TTabSheet}
   begin
   TabSheetToPDF(TTabSheet(AControl),FDoc,Page,ftText,Margins);
   exit;
   end;
if AControl is TGroupBox then             {TGroupBox}
   begin
   GroupBoxToPDF(TGroupBox(AControl),FDoc,Page,ftText,Margins);
   exit;
   end;
if AControl is TPanel then                {TPanel}
   begin
   PanelToPDF(TPanel(AControl),FDoc,Page,ftText,Margins);
   exit;
   end;
if AControl is TRadioGroup then           {TRadioGroup}
   begin
   RadioGroupToPDF(TRadioGroup(AControl),FDoc,Page,ftText,Margins);
   exit;
   end;
if AControl is TCheckGroup then           {TCheckGroup}
   begin
   CheckGroupToPDF(TCheckGroup(AControl),FDoc,Page,ftText,Margins);
   exit;
   end;
end;


function FormToPDF:integer;
{Use to check if FormToPDF is available. Resets FDoc.}
begin
FreeAndNil(FDoc);
Result := FormToPDF(nil,'');
end;


function FormToPDF(AControl: TControl):integer;
{Use to append pages to FDoc}
begin
Result := FormToPDF(AControl,'');
end;


function FormToPDF(FileName:string):integer;
{Use to save FDoc to PDF and reset FDoc}
begin
Result := FormToPDF(nil,FileName);
end;


function FormToPDF(AControl: TControl; FileName:string):integer;
{Note screen origin is top-left, pdf origin is bottom-left. We map the form to
the page 1:1, but PDF is 72 dpi (Points) and screen is usually 96dpi so the
form will be enlarged. Returns number of objects printed if successful, error
code otherwise. Error codes:
 0 initialisation OK, no objects printed, nil control or empty filename.
-1 no fonts available.
-2 could not create document}

var DW,DH,
    ftTitle,                   {title font index}
    ftText     :integer;       {text font index}
    Page       :TPDFPage;
    Section    :TPDFSection;
    Aspect     :single;        {control aspect ratio}
    Margins    :TMargins;

begin
Result := -1;
FirstPage := false;

{Checks}
if FontsAvailable then Result := 0;
if (Result = 0) then
   begin
   {set margins}
   Margins.T := 72;                       {1 inch}
   Margins.L := 72;                       {1 inch}
   Margins.B := 72;                       {1 inch}
   Margins.R := 72;                       {1 inch}
   Margins.H := 36;                       {1/2 inch}
   Margins.F := 0;                        {nothing}

   if not Assigned(FDoc) then
      begin
      {Set up document}
      try
         FDoc := TPDFDocument.Create(Nil);
      except
         Result := -2;
      end;
      FDoc.Options := [poPageOriginAtTop];
      FDoc.FontDirectory := 'fonts';
      FDoc.DefaultUnitOfMeasure := uomPixels;
      FDoc.StartDocument;
      Section := FDoc.Sections.AddSection;
      end;

   {set up fonts}
   {It is very difficult to get the system fonts. For now the user must copy
   any fonts used in the form to the /fonts directory and specify them explicitly}
   {ftTitle := FDoc.Addfont('Helvetica');
   ftText := FDoc.Addfont('FreeSans.ttf','Regular');}

   {if user has not already set info then set defaults}
   if FDoc.Infos.Title <> '' then FDoc.Infos.Title := Application.Title;
   if FDoc.Infos.Author <> '' then FDoc.Infos.Author := 'Form2PDF';
   if FDoc.Infos.Producer <> '' then FDoc.Infos.Producer := 'fpGUI Toolkit 1.4.1';
   if FDoc.Infos.ApplicationName <> '' then FDoc.Infos.ApplicationName := ApplicationName;
   FDoc.Infos.CreationDate := Now;

   if Assigned(AControl) then
      begin
      {get form aspect ratio}
      DW := AControl.Width;
      DH := AControl.Height;
      Aspect := DW/DH;
      if Aspect > 1 then
         FDoc.DefaultOrientation := ppoLandscape
        else
         FDoc.DefaultOrientation := ppoPortrait;

      {set paper size, smaller than A4 use A4 otherwise use custom as nothing larger}
      CustomPaper.H := DH + Margins.T + Margins.B + Margins.H + Margins.F;
      CustomPaper.W := DW + Margins.L + Margins.R;
      CustomPaper.Printable.T := Margins.T;
      CustomPaper.Printable.L := Margins.L;
      CustomPaper.Printable.R := CustomPaper.W - Margins.R;
      CustomPaper.Printable.B := CustomPaper.H - Margins.B;
      if (DW > 842 - Margins.L - Margins.R) or (DH > 595 - Margins.T - Margins.B) then
         FDoc.DefaultPaperType := ptCustom
        else FDoc.DefaultPaperType := ptA4;

      {Add first page}
      Page := SetupPage(AControl,FDoc);
      FirstPage := true;

      RecurseControls(AControl,FDoc,Page,ftText,Margins);
      end;

   {Save the PDF}
   if FileName <> '' then
      begin
      FDoc.SaveToFile(FileName);
      Result := FDoc.ObjectCount;
      FreeAndNil(FDoc);         {assume document is finished and dispose}
      end;
   end;
end;


initialization
FDoc := nil;
{add any extra fonts}
gTTFontCache.ReadStandardFonts;
{gTTFontCache.SearchPath.Add(ExtractFilePath(Application.ExeName) + 'fonts');
gTTFontCache.BuildFontCache;}
gTTFontCache.DPI := 72;

finalization
FreeAndNil(FDoc);

end.


