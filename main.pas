unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ColorBox, StdCtrls, ComCtrls, Menus,
  tools;

type
  
  { TMainForm }
  TMainForm = class(TForm)
    DlgOpenImage: TOpenDialog;
    DlgSaveImage: TSaveDialog;
    ToolBox: TPanel;
      LToolBox: TLabel;
      BtnPen: TSpeedButton;
      BtnLine: TSpeedButton;
      BtnRect: TSpeedButton;
      BtnEllipse: TSpeedButton;
      BtnEraser: TSpeedButton;
      BtnFiller: TSpeedButton;
      FColorBox: TColorBox;
      LFColorBox: TLabel;
      BColorBox: TColorBox;
      LBColorBox: TLabel;
      PenSize: TTrackBar;
      LPenSize: TLabel;
      SolidCB: TCheckBox;
      Doge: TImage; // :3

    ScrollBox: TScrollBox;
      PaintBox: TPaintBox;

    MenuBar: TMainMenu;
      FileMenu: TMenuItem;
        OpenItem: TMenuItem;
        SaveItem: TMenuItem;
        FileSep1: TMenuItem;
        ExitItem: TMenuItem;
      EditMenu: TMenuItem;
        ClearItem: TMenuItem;
        SizeItem: TMenuItem;
      AboutItem: TMenuItem;
    
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure WorkspaceClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; 
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, 
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; 
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);

    procedure BtnEllipseClick(Sender: TObject);
    procedure BtnEraserClick(Sender: TObject);
    procedure BtnLineClick(Sender: TObject);
    procedure BtnPenClick(Sender: TObject);
    procedure BtnRectClick(Sender: TObject);
    procedure BtnFillerClick(Sender: TObject);

    procedure OpenItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure ClearItemClick(Sender: TObject);
    procedure SizeItemClick(Sender: TObject);
    procedure AboutItemClick(Sender: TObject);

    procedure UpdateTool( X, Y: Integer );

  private
    SelTool: ToolEnum;
    CurTool: TTool;
  public
    ImageBuf: TBitMap; //main image buffer
  end;

  { Additional routines }
  procedure ClearImg( image: TBitMap );

var
  MainForm: TMainForm;

implementation

uses
  f_imagesize;

{$R *.lfm}

{ MainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SelTool := TLPEN;
  CurTool := nil;
  ScrollBox.DoubleBuffered := True; //enable double buffering

  ImageBuf := TBitMap.Create();
  ImageBuf.Width := PaintBox.Width;
  ImageBuf.Height := PaintBox.Height;

  //FILL IT WITH WHITE!!1
  ClearImg( ImageBuf );
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ImageBuf.Destroy();
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not ( MessageDlg('Leaving so soon?',
                      mtConfirmation,
                      mbYesNo, 0) = mrYes ) then CloseAction := caNone;
end;

procedure TMainForm.BtnPenClick(Sender: TObject);
    begin SelTool := TLPEN;
      end;

procedure TMainForm.BtnLineClick(Sender: TObject);
    begin SelTool := TLLINE;
      end;

procedure TMainForm.BtnRectClick(Sender: TObject);
    begin SelTool := TLRECT;
      end;

procedure TMainForm.BtnEllipseClick(Sender: TObject);
    begin SelTool := TLELLIPSE;
      end;

procedure TMainForm.BtnEraserClick(Sender: TObject);
    begin SelTool := TLERASER;
      end;

procedure TMainForm.BtnFillerClick(Sender: TObject);
    begin SelTool := TCFILLER;
      end;

procedure TMainForm.OpenItemClick(Sender: TObject);
begin
  if DlgOpenImage.Execute() then begin
    ImageBuf.LoadFromFile( DlgOpenImage.FileName );
    PaintBox.Width := ImageBuf.Width;
    PaintBox.Height := ImageBuf.Height;
    PaintBox.Invalidate();
  end;
end;

procedure TMainForm.SaveItemClick(Sender: TObject);
begin
  if DlgSaveImage.Execute() then
    ImageBuf.SaveToFile( ChangeFileExt( DlgSaveImage.FileName,
                                        DlgSaveImage.DefaultExt ) );
end;

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  Close();
end;

procedure TMainForm.ClearItemClick(Sender: TObject);
begin
  if ( MessageDlg('Are you sure?',
                  mtConfirmation,
                  mbYesNo, 0) = mrYes ) then begin
                                               ClearImg( ImageBuf );
                                               PaintBox.Invalidate();
                                             end;
end;

procedure TMainForm.SizeItemClick(Sender: TObject);
begin
  SizeForm.ShowModal();
end;

procedure TMainForm.AboutItemClick(Sender: TObject);
begin
  ShowMessage( 'DAS IST another paint clone called "Artist"' +#13#10 +
               'but this one with beautiful girl on icon ^^' +#13#10 +
                                                              #13#10 +
               'by Dmitry D. Chernov who made it last night' );
end;

procedure TMainForm.WorkspaceClick(Sender: TObject);
begin
  MainForm.ActiveControl := ScrollBox;
end;

{ PaintBox }
procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  FColor, BColor: TColor;
begin
  if not (CurTool = nil) then Exit;

  case Button of
    mbLeft:  FColor := FColorBox.Selected;
    mbRight: FColor := BColorBox.Selected;
    else Exit;
  end;
  if SolidCB.Checked then BColor := BColorBox.Selected else BColor := clNone;

  case SelTool of
    TLPEN:
      CurTool := TPen.Create( X, Y, FColor, BColor, PenSize.Position );
    TLERASER:
      CurTool := TPen.Create( X, Y, clWhite, clNone, PenSize.Position + 5 );
    TLLINE:
      CurTool := TLine.Create( X, Y, FColor, BColor, PenSize.Position );
    TLRECT:
      CurTool := TRectangle.Create( X, Y, FColor, BColor, PenSize.Position );
    TLELLIPSE:
      CurTool := TEllipse.Create( X, Y, FColor, BColor, PenSize.Position );
    TCFILLER:
      FillAtXY( ImageBuf.Canvas, X, Y, FColor );
  end;

  UpdateTool( X, Y );
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, 
  Y: Integer);
begin
   UpdateTool( X, Y );
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; 
  Shift: TShiftState; X, Y: Integer);
begin
  if not (CurTool = nil) then begin
    CurTool.Draw( ImageBuf.Canvas );
    CurTool.Destroy();
    CurTool := nil;
  end;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  rect: TRect;
begin
  rect := Bounds( 0, 0, PaintBox.Width, PaintBox.Height );
  PaintBox.Canvas.CopyRect( rect, ImageBuf.Canvas, rect );
  if not ( (CurTool = nil) or (CurTool is TPen) ) then
    CurTool.Draw( PaintBox.Canvas );
end;

{ Additional routines }
procedure TMainForm.UpdateTool( X, Y: Integer );
begin
  if not (CurTool = nil) then begin
    CurTool.Update(X, Y);
    if (CurTool is TPen) then CurTool.Draw( ImageBuf.Canvas );
  end;

  PaintBox.Invalidate();
end;

procedure ClearImg( image: TBitMap );
begin
  image.Canvas.Pen.Color := clWhite;
  image.Canvas.Brush.Color := clWhite;
  image.Canvas.Rectangle(0, 0, image.Width, image.Height);
end;

end.

