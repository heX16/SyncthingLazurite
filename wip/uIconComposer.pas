unit uIconComposer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPImage, FPReadPNG, FPWritePNG, Graphics, Types, ImgList,
  IntfGraphics, GraphType, LCLType;

type
  { Simple RGBA record with 8-bit per channel }
  TRGBA = record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;

{ ComposePng
  Creates a PNG image with alpha channel by drawing layers in the following order:
  1) Background PNG (no alpha considered) – copied as fully opaque base
  2) Solid color fill (RGBA) – alpha-blended over base
  3) Overlay PNG (with alpha) – alpha-blended over current image
  4) Cutout mask PNG (black/white) – black pixels make the result pixel fully transparent

  All images must have the same size (if provided). If sizes mismatch, raises exception.
  Any of the PNG paths may be an empty string to skip that step, but at least one source
  (background/overlay/mask or a non-zero alpha fill) must define the target size.

  Returns a new TPortableNetworkGraphic instance (caller owns the object).
}
function ComposePng(const BackgroundPng, OverlayPng, MaskPng: string; const Fill: TRGBA): TPortableNetworkGraphic;

{ Converts a PNG graphic into an icon suitable for TTrayIcon.
  If DesiredSize > 0, the PNG is scaled to a square icon of DesiredSize.
  Caller owns the returned TIcon instance. }
function PngToIcon(const Png: TPortableNetworkGraphic; DesiredSize: Integer = 0): TIcon;

{ Replaces an image in the given TImageList at Index with contents of the PNG.
  The PNG will be scaled to the list's item size. Supports alpha images. }
procedure ReplaceImageInImageList(ImageList: TImageList; const Png: TPortableNetworkGraphic; Index: Integer);

implementation

function ByteToWord(const B: Byte): Word; inline;
begin
  // Convert 0..255 to 0..65535 (x * 257)
  Result := (Word(B) shl 8) or B;
end;

function BlendChannel(const SrcC, DstC, SrcA, DstA: Cardinal): Word; inline;
var
  // All values are 0..65535. Using 32-bit math to avoid overflow.
  OutC: Cardinal;
begin
  // out = src * srcA + dst * (1 - srcA)
  // Normalize by 65535 using integer math
  OutC := (SrcC * SrcA + DstC * (65535 - SrcA));
  // Divide by 65535 with rounding
  if OutC = 0 then
    Exit(0)
  else
    Exit(Word((OutC + 32767) div 65535));
end;

function BlendPixelOver(const Src, Dst: TFPColor): TFPColor; inline;
var
  OutA: Word;
begin
  // Normal alpha compositing: Src over Dst
  if Src.alpha = 0 then
  begin
    Exit(Dst);
  end
  else if Src.alpha = 65535 then
  begin
    Exit(Src);
  end;

  Result.red   := BlendChannel(Src.red,   Dst.red,   Src.alpha, Dst.alpha);
  Result.green := BlendChannel(Src.green, Dst.green, Src.alpha, Dst.alpha);
  Result.blue  := BlendChannel(Src.blue,  Dst.blue,  Src.alpha, Dst.alpha);

  // Alpha out = srcA + dstA * (1 - srcA)
  OutA := Word((Cardinal(Src.alpha) + Cardinal(Dst.alpha) * (65535 - Cardinal(Src.alpha)) + 32767) div 65535);
  Result.alpha := OutA;
end;

procedure RequireSameSize(const A, B: TFPMemoryImage; const AName, BName: string);
begin
  if (A <> nil) and (B <> nil) then
  begin
    if (A.Width <> B.Width) or (A.Height <> B.Height) then
      raise Exception.CreateFmt('Image sizes mismatch between %s and %s', [AName, BName]);
  end;
end;

function LoadPngToFPMemory(const FileName: string): TFPMemoryImage;
var
  Reader: TFPReaderPNG;
begin
  // Load PNG file to TFPMemoryImage. Returns nil if FileName is empty.
  if FileName = '' then
    Exit(nil);
  if not FileExists(FileName) then
    raise Exception.CreateFmt('File not found: %s', [FileName]);

  Reader := TFPReaderPNG.Create;
  try
    Result := TFPMemoryImage.Create(0, 0);
    try
      Result.LoadFromFile(FileName, Reader);
    except
      Result.Free;
      raise;
    end;
  finally
    Reader.Free;
  end;
end;

function ComposePng(const BackgroundPng, OverlayPng, MaskPng: string; const Fill: TRGBA): TPortableNetworkGraphic;
var
  Bkg, Ovr, Msk: TFPMemoryImage;
  Res: TFPMemoryImage;
  Writer: TFPWriterPNG;
  MS: TMemoryStream;
  HasAnySource: Boolean;
  W, H: Integer;
  x, y: Integer;
  FillColorFP: TFPColor;
  cDst, cBkg, cOvr, cMsk: TFPColor;
  IsBlack: Boolean;
begin
  Bkg := nil; Ovr := nil; Msk := nil; Res := nil; Writer := nil; MS := nil;
  try
    // Load inputs
    Bkg := LoadPngToFPMemory(BackgroundPng);
    Ovr := LoadPngToFPMemory(OverlayPng);
    Msk := LoadPngToFPMemory(MaskPng);

    // Validate sizes (only among provided images)
    RequireSameSize(Bkg, Ovr, 'background', 'overlay');
    RequireSameSize(Bkg, Msk, 'background', 'mask');
    RequireSameSize(Ovr, Msk, 'overlay', 'mask');

    // Determine target size
    if Bkg <> nil then
    begin
      W := Bkg.Width; H := Bkg.Height;
    end
    else if Ovr <> nil then
    begin
      W := Ovr.Width; H := Ovr.Height;
    end
    else if Msk <> nil then
    begin
      W := Msk.Width; H := Msk.Height;
    end
    else
    begin
      // No images; allow pure color fill only if alpha > 0 and a size cannot be inferred → error.
      raise Exception.Create('Target size is undefined. Provide at least one PNG to define size.');
    end;

    // Prepare result image as fully transparent
    Res := TFPMemoryImage.Create(W, H);
    for y := 0 to H - 1 do
      for x := 0 to W - 1 do
        Res.Colors[x, y] := FPColor(0, 0, 0, 0);

    // Step 1: Copy background as fully opaque (ignore any background alpha)
    if Bkg <> nil then
    begin
      for y := 0 to H - 1 do
        for x := 0 to W - 1 do
        begin
          cBkg := Bkg.Colors[x, y];
          cBkg.alpha := 65535; // force opaque
          Res.Colors[x, y] := cBkg;
        end;
    end;

    // Step 2: Solid color fill with given RGBA over current result
    FillColorFP.red   := ByteToWord(Fill.R);
    FillColorFP.green := ByteToWord(Fill.G);
    FillColorFP.blue  := ByteToWord(Fill.B);
    FillColorFP.alpha := ByteToWord(Fill.A);

    if FillColorFP.alpha > 0 then
    begin
      for y := 0 to H - 1 do
        for x := 0 to W - 1 do
        begin
          cDst := Res.Colors[x, y];
          Res.Colors[x, y] := BlendPixelOver(FillColorFP, cDst);
        end;
    end;

    // Step 3: Overlay PNG with its alpha
    if Ovr <> nil then
    begin
      for y := 0 to H - 1 do
        for x := 0 to W - 1 do
        begin
          cDst := Res.Colors[x, y];
          cOvr := Ovr.Colors[x, y];
          Res.Colors[x, y] := BlendPixelOver(cOvr, cDst);
        end;
    end;

    // Step 4: Apply cutout mask – black pixels become fully transparent
    if Msk <> nil then
    begin
      for y := 0 to H - 1 do
        for x := 0 to W - 1 do
        begin
          cMsk := Msk.Colors[x, y];
          // Consider exact black as cut-out; anything else remains as-is
          IsBlack := (cMsk.red = 0) and (cMsk.green = 0) and (cMsk.blue = 0);
          if IsBlack then
          begin
            cDst := Res.Colors[x, y];
            cDst.alpha := 0;
            Res.Colors[x, y] := cDst;
          end;
        end;
    end;

    // Convert TFPMemoryImage to PNG with alpha
    Writer := TFPWriterPNG.Create;
    Writer.UseAlpha := True;
    MS := TMemoryStream.Create;
    Res.SaveToStream(MS, Writer);
    MS.Position := 0;

    Result := TPortableNetworkGraphic.Create;
    try
      Result.LoadFromStream(MS);
    except
      Result.Free;
      raise;
    end;
  finally
    MS.Free;
    Writer.Free;
    Res.Free;
    Msk.Free;
    Ovr.Free;
    Bkg.Free;
  end;
end;

function CreateTransparentBitmap(const W, H: Integer): TBitmap;
var
  Intf: TLazIntfImage;
  x, y: Integer;
  C: TFPColor;
begin
  // Create a 32-bit bitmap initialized with fully transparent pixels
  Intf := TLazIntfImage.Create(W, H);
  try
    C.red := 0; C.green := 0; C.blue := 0; C.alpha := 0;
    for y := 0 to H - 1 do
      for x := 0 to W - 1 do
        Intf.Colors[x, y] := C;

    Result := TBitmap.Create;
    try
      Result.LoadFromIntfImage(Intf);
    except
      Result.Free;
      raise;
    end;
  finally
    Intf.Free;
  end;
end;

function PngToIcon(const Png: TPortableNetworkGraphic; DesiredSize: Integer): TIcon;
var
  SW, SH: Integer;
  B: TBitmap;
  R: TRect;
begin
  if Png = nil then
    raise Exception.Create('PngToIcon: Png is nil');

  if DesiredSize > 0 then
  begin
    SW := DesiredSize;
    SH := DesiredSize;
  end
  else
  begin
    SW := Png.Width;
    SH := Png.Height;
  end;

  B := CreateTransparentBitmap(SW, SH);
  try
    R := Rect(0, 0, SW, SH);
    // Draw PNG onto 32-bit bitmap; LCL should preserve alpha while drawing PNG
    B.Canvas.StretchDraw(R, Png);

    Result := TIcon.Create;
    try
      Result.Assign(B);
    except
      Result.Free;
      raise;
    end;
  finally
    B.Free;
  end;
end;

procedure ReplaceImageInImageList(ImageList: TImageList; const Png: TPortableNetworkGraphic; Index: Integer);
var
  W, H: Integer;
  B: TBitmap;
  R: TRect;
begin
  if (ImageList = nil) then
    raise Exception.Create('ReplaceImageInImageList: ImageList is nil');
  if (Png = nil) then
    raise Exception.Create('ReplaceImageInImageList: Png is nil');
  if (Index < 0) or (Index >= ImageList.Count) then
    raise Exception.CreateFmt('ReplaceImageInImageList: Index %d out of bounds', [Index]);

  W := ImageList.Width;
  H := ImageList.Height;

  B := CreateTransparentBitmap(W, H);
  try
    R := Rect(0, 0, W, H);
    // Stretch to the full cell; caller can pre-scale if aspect preservation is desired
    B.Canvas.StretchDraw(R, Png);

    // Replace the image; for alpha bitmaps Mask can be nil
    ImageList.Replace(Index, B, nil);
  finally
    B.Free;
  end;
end;

end.


