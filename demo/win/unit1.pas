unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, MPVPlayer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
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
    procedure mpvFileLoaded(Sender: TObject);
    procedure mpvTimeChanged(ASender: TObject; AParam: Integer);
  private

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

  //mpv.RendererMode := rmWindow;
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

procedure TForm1.mpvFileLoaded(Sender: TObject);
begin
  TrackBar1.Max := mpv.GetMediaLenInMs;
  Label2.Caption := inttostr(TrackBar1.Max) + 'ms';
end;

procedure TForm1.mpvTimeChanged(ASender: TObject; AParam: Integer);
begin
  Label1.Caption := inttostr(AParam) + 'ms';
  TrackBar1.Position := AParam;
end;


end.

