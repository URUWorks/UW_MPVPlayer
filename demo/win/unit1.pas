unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, MPVPlayer, BGRAOpenGL, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    mpv: TMPVPlayer;
    od: TOpenDialog;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mpvBuffering(ASender: TObject; AParam: Integer);
    procedure mpvDraw(Sender: TObject; ABGLCanvas: TBGLCustomCanvas);
    procedure mpvFileLoaded(Sender: TObject);
    procedure mpvPause(Sender: TObject);
    procedure mpvPlay(Sender: TObject);
    procedure mpvStartFile(Sender: TObject);
    procedure mpvStop(Sender: TObject);
    procedure mpvTimeChanged(ASender: TObject; AParam: Integer);
  private
    fnt: IBGLRenderedFont;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  od.Execute;

  mpv.Play(od.FileName);

  if mpv.Error <> 0 then
    memo1.Lines.add('mpv error: ' + inttostr(mpv.Error));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  mpv.Pause;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  mpv.stop;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  mpv.ShowText(Caption);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  mpv.Play('http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not mpv.IsLibMPVAvailable then
    ShowMessage('Please install libmpv ;)');

  fnt := BGLFont('Arial', 20, CSSLightYellow, CSSBlack, [fsBold]);
end;

procedure TForm1.mpvBuffering(ASender: TObject; AParam: Integer);
begin
  Memo1.Lines.Add('Buffering: ' + IntToStr(AParam) + '%');
end;

procedure TForm1.mpvDraw(Sender: TObject; ABGLCanvas: TBGLCustomCanvas);
begin
  fnt.TextOut(ABGLCanvas.Width div 2, ABGLCanvas.Height div 2, 'BGRABitmap is amazing!', taCenter, tlCenter);
end;

procedure TForm1.mpvFileLoaded(Sender: TObject);
begin
  TrackBar1.Max := mpv.GetMediaLenInMs;
  Label2.Caption := inttostr(TrackBar1.Max) + 'ms';

  Memo1.Lines.Add('File loaded');
  Memo1.Lines.Add('Video Width: ' + IntToStr(mpv.GetVideoWidth));
  Memo1.Lines.Add('Video Height: ' + IntToStr(mpv.GetVideoHeight));
  Memo1.Lines.Add('Video Total Frames: ' + IntToStr(mpv.GetVideoTotalFrames));
  Memo1.Lines.Add('Video FPS: ' + FloatToStr(mpv.GetVideoFPS));
end;

procedure TForm1.mpvPause(Sender: TObject);
begin
  Memo1.Lines.Add('paused');
end;

procedure TForm1.mpvPlay(Sender: TObject);
begin
  Memo1.Lines.Add('playing');
end;

procedure TForm1.mpvStartFile(Sender: TObject);
begin
  Memo1.Lines.Add('reading file');
end;

procedure TForm1.mpvStop(Sender: TObject);
begin
    Memo1.Lines.Add('stopped');
end;

procedure TForm1.mpvTimeChanged(ASender: TObject; AParam: Integer);
begin
  Label1.Caption := inttostr(AParam) + 'ms';
  TrackBar1.Position := AParam;
end;


end.

