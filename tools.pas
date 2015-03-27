unit tools;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics;

type
  //types of tools
  ToolEnum = ( TLPEN, TLERASER, TLLINE, TLRECT, TLELLIPSE,
               TCFILLER );

  //general tool template
  TTool = class
    private
      x1, y1, x2, y2: Integer;
      PenColor, BrushColor: TColor;
      Size: Byte;
    public
      constructor Create( X, Y: Integer;
                          FColor, BColor: TColor; PenSize: Byte );
      procedure Draw( ImgCanvas: TCanvas ); virtual;
      procedure Update( X, Y: Integer );
  end;

  //mouse-moving tools
  TLine      = class(TTool) procedure Draw( ImgCanvas: TCanvas ); override; end;
  TPen       = class(TLine) procedure Draw( ImgCanvas: TCanvas ); override; end;
  TRectangle = class(TTool) procedure Draw( ImgCanvas: TCanvas ); override; end;
  TEllipse   = class(TTool) procedure Draw( ImgCanvas: TCanvas ); override; end;

  //and click tools. so, they are simple procedures and no use TTool
  procedure FillAtXY( ImgCanvas: TCanvas; X, Y: Integer; Color: TColor );

implementation

uses GraphType;

{ TTool - general class }
constructor TTool.Create( X, Y: Integer;
                          FColor, BColor: TColor; PenSize: Byte );
begin
  x1 := X; y1 := Y;
  PenColor := FColor;
  BrushColor := BColor;
  Size := PenSize;
end;

procedure TTool.Draw( ImgCanvas: TCanvas );
begin
  ImgCanvas.Pen.Color := PenColor;
  ImgCanvas.Pen.Width := Size;
  if (BrushColor = clNone) then
    ImgCanvas.Brush.Style := bsClear
  else begin
    ImgCanvas.Brush.Style := bsSolid;
    ImgCanvas.Brush.Color := BrushColor;
  end;
end;

procedure TTool.Update( X, Y: Integer );
begin
  x2 := X;
  y2 := Y;
end;

{ TLine }
procedure TLine.Draw( ImgCanvas: TCanvas );
begin
  inherited;
  ImgCanvas.MoveTo( x1, y1 );
  ImgCanvas.LineTo( x2, y2 );
end;

{ TPen }
procedure TPen.Draw( ImgCanvas: TCanvas );
begin
  inherited;
  x1 := x2;
  y1 := y2;
end;

{ TRectangle }
procedure TRectangle.Draw( ImgCanvas: TCanvas );
begin
  inherited;
  ImgCanvas.Rectangle( x1, y1, x2, y2 );
end;

{ TEllipse }
procedure TEllipse.Draw( ImgCanvas: TCanvas );
begin
  inherited;
  ImgCanvas.Ellipse( x1, y1, x2, y2 );
end;

{ CLICK TOOLS ROUTINES }
procedure FillAtXY( ImgCanvas: TCanvas; X, Y: Integer; Color: TColor );
begin
  ImgCanvas.Brush.Color := Color;
  ImgCanvas.FloodFill( X, Y, ImgCanvas.Pixels[X,Y], fsSurface );
end;

end.

