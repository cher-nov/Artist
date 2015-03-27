unit f_imagesize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Spin, StdCtrls;

type
  
  { TSizeForm }

  TSizeForm = class(TForm)
    LWidth: TLabel;
    WidthSpin: TSpinEdit;
    LHeight: TLabel;
    HeightSpin: TSpinEdit;
    OKBtn: TButton;
    CancelBtn: TButton;

    procedure FormActivate(Sender: TObject);
    procedure BtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SizeForm: TSizeForm;

implementation

uses
  main;

{$R *.lfm}

{ TSizeForm }

procedure TSizeForm.FormActivate(Sender: TObject);
begin
  with MainForm do begin
    WidthSpin.Value := PaintBox.Width;
    HeightSpin.Value := PaintBox.Height;
  end;
end;

procedure TSizeForm.BtnClick(Sender: TObject);
begin
  if (Sender = OKBtn) then
    with MainForm do begin
      ImageBuf.SetSize( WidthSpin.Value, HeightSpin.Value );

      //cleaning black fields around image after enlargement
      ImageBuf.Canvas.Pen.Color := BColorBox.Selected;
      ImageBuf.Canvas.Pen.Width := 1;
      ImageBuf.Canvas.Brush.Color := BColorBox.Selected;
      ImageBuf.Canvas.Brush.Style := bsSolid;
      
      if (PaintBox.Width < ImageBuf.Width) then
        ImageBuf.Canvas.Rectangle( PaintBox.Width, 0,
                                   ImageBuf.Width, PaintBox.Height );
      
      if (PaintBox.Height < ImageBuf.Height) then
        ImageBuf.Canvas.Rectangle( 0, PaintBox.Height,
                                   ImageBuf.Width, ImageBuf.Height );

      PaintBox.Width := ImageBuf.Width;
      PaintBox.Height := ImageBuf.Height;
      PaintBox.Invalidate();
    end;
  Close();
end;

end.

