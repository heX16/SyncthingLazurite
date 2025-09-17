program IconComposeDemo;

{$mode ObjFPC}{$H+}

{ Simple demo program for uIconComposer
  - Loads background/overlay/mask PNGs
  - Composes two variants with different RGBA fills
  - Saves resulting PNGs and ICOs
  Notes:
  - All input PNGs must have the same size
  - Mask: black pixels become fully transparent in the result }

uses
  Interfaces, Forms, SysUtils, Graphics, uIconComposer;

var
  Fill1, Fill2: TRGBA;
  Png1, Png2: TPortableNetworkGraphic;
  Ico1, Ico2: TIcon;
begin
  Application.Initialize; // ensure LCL graphics/canvas are initialized
  try
    // Prepare two fills
    Fill1.R := 255; Fill1.G := 0;   Fill1.B := 0;   Fill1.A := 128; // semi-transparent red
    Fill2.R := 0;   Fill2.G := 80;  Fill2.B := 255; Fill2.A := 160; // semi-transparent blue

    // Compose first variant and save
    Png1 := ComposePng('bg.png', 'overlay.png', 'mask.png', Fill1);
    try
      Png1.SaveToFile('out_red.png');
      Ico1 := PngToIcon(Png1, 16);
      try
        Ico1.SaveToFile('out_red_16.ico');
      finally
        Ico1.Free;
      end;
    finally
      Png1.Free;
    end;

    // Compose second variant and save
    Png2 := ComposePng('bg.png', 'overlay.png', 'mask.png', Fill2);
    try
      Png2.SaveToFile('out_blue.png');
      Ico2 := PngToIcon(Png2, 24);
      try
        Ico2.SaveToFile('out_blue_24.ico');
      finally
        Ico2.Free;
      end;
    finally
      Png2.Free;
    end;
  except
    on E: Exception do
    begin
      // In GUI mode console is not visible; this is for debugging runs
      // You can set a breakpoint here to inspect E.Message
    end;
  end;
end.


