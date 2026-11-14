{*
 * URUWorks MPVPlayer - Scene detection
 *
 * Author  : URUWorks
 * Website : uruworks.net
 *
 * The contents of this file are used with permission, subject to
 * the Mozilla Public License Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/2.0.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * Copyright (C) 2026 URUWorks, uruworks@gmail.com.
 *}

unit MPVPlayer.SceneDetect;

// -----------------------------------------------------------------------------
//
// Shot change detection with a second, private libmpv instance running in its
// own thread: no audio, no window, untimed, the video scaled down to a tiny
// colour image by the decoder's filter chain and read back with the software
// render API, so it doesn't touch the player that is showing the video.
//
// The difference of two frames, D(A, B), is the mean of |dY| + |dCb| + |dCr|
// (luma and both colour differences, so a cut between two shots of the same
// brightness but other colours is seen) over blocks of the image, each block
// compared with the other image moved up to 3 pixels in every direction and
// keeping the shift where the luma matches best (searched every 2 pixels and
// then around the best): a pan, a zoom or a shaking camera move the picture
// but don't change it, a cut does.
//
// Hard cuts. The score of frame k is the smallest D between any of the 4
// frames before k and any of the 4 frames from k on:
//
//   score(k) = min D(F(a), F(b)),  a = k-4..k-1,  b = k..k+3
//
// A cut between k-1 and k makes every pair different (each side is another
// shot), while a flash, an explosion or a few bad frames (up to 3) leave at
// least one pair of the same shot, so the score stays small.
// Frame k is a cut when its score is at least MinScore and at least Ratio
// times the highest score of the other frames within +/- Window seconds. That
// is a proportion against what happens around the frame, not a fixed
// threshold: fast motion gives high scores all over the window and isn't a
// cut, while a cut in a dark or quiet scene still stands out. A single other
// peak in the window (a frame Ratio times over both of its neighbours, e.g.
// another cut two frames later) is not counted as the background (several
// are: an animation changing every 2 frames), and of two cuts closer than
// Window only the stronger is kept.
//
// Gradual transitions (Gradual = True). With s frames (about a third of a
// second), G(k) = D(F(k-s), F(k+s)) (blocks moved up to 1 pixel) and the linearity L(k) = mean of
// |2F(k) - F(k-s) - F(k+s)| / 2. In a dissolve or a fade each frame is a mix
// of the frames before and after, so L is small against G, while motion
// gives a big L. A dissolve is the frame where G is the highest within +/- s,
// at least GradualMinScore and with L <= 0.3 G, away from the hard cuts.
// A fade goes through flat frames (black, white or a single colour): a run of
// them entered and left gradually is one change, in its middle. Fades in at
// the start and out at the end of the video are not changes.
//
// -----------------------------------------------------------------------------

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, dynlibs, ctypes, libMPV.Client, libMPV.Render;

// -----------------------------------------------------------------------------

type

  { TMPVSceneDetectStatus }

  TMPVSceneDetectStatus = (sdsIdle, sdsRunning, sdsDone, sdsCancelled, sdsError);

  { TMPVSceneFrame }

  TMPVSceneFrame = record // what is measured of each frame (see above), the cuts are decided from it
    Time      : Double;   // seconds
    Score     : Single;   // hard cut score
    Gradual   : Single;   // G, 0 near the start and the end
    Linearity : Single;   // L
    Luma      : Single;   // mean luma, 0..255
    Contrast  : Single;   // standard deviation of the luma
  end;
  TMPVSceneFrames = array of TMPVSceneFrame;

  { TMPVSceneDetector }

  TMPVSceneDetector = class
  private
    FThread     : TThread;
    FLock       : TRTLCriticalSection;
    FStatus     : TMPVSceneDetectStatus;
    FErrorText  : String;
    FPosition   : Double;
    FDuration   : Double;
    FCuts       : TDoubleDynArray;
    FFrames     : TMPVSceneFrames;
    FSpan       : Integer;
    FFrameCount : Integer;
    FLibFileName: String;
    FMinScore   : Double;
    FRatio      : Double;
    FWindow     : Double;
    FGradual    : Boolean;
    FGradualMin : Double;
    FWidth      : Integer;
    FHeight     : Integer;
    FHWDec      : String;
    FOnFinished : TNotifyEvent;
    procedure DoFinished;
    procedure SetFrames(const AFrames: TMPVSceneFrames; const ASpan: Integer);
    procedure SetResult(const AStatus: TMPVSceneDetectStatus; const ACuts: TDoubleDynArray; const AError: String);
    procedure SetPosition(const APosition, ADuration: Double; const AFrames: Integer);
    function GetStatus: TMPVSceneDetectStatus;
    function GetErrorText: String;
    function GetFrameCount: Integer;
  public
    constructor Create(const ALibFileName: String = '');
    destructor Destroy; override;
    // starts the detection of a local file in the background (False if one is
    // already running or libmpv can't be loaded)
    function Start(const AFileName: String): Boolean;
    // asks the running detection to stop, it ends as sdsCancelled
    procedure Cancel;
    // waits until the background detection ends
    procedure WaitFor;
    function IsRunning: Boolean;
    // 0..1, by the time of the last frame read and the duration of the video
    function GetProgress: Double;
    // the position (seconds) of the last frame read and the video duration
    procedure GetPosition(out APosition, ADuration: Double);
    // the first frame of each new shot, in seconds (ms), once sdsDone
    function GetCuts: TDoubleDynArray;
    function GetCutsMs: TIntegerDynArray;
    // what was measured of every frame in the last detection (sdsDone): the
    // slow part, the cuts are decided from it. GetScores gives only the
    // times and the hard cut scores
    function GetFrames: TMPVSceneFrames;
    procedure GetScores(out ATimes, AScores: TDoubleDynArray);
    function HasScores: Boolean;
    // the frames of G and L (about a third of a second), from the frame rate
    property GradualSpan: Integer read FSpan;
    // the cuts again with other MinScore/Ratio (and the current Gradual and
    // GradualMinScore) from the last detection, without reading the video
    // again (instant). False if there is nothing measured
    function Recalculate(const AMinScore, ARatio: Double): Boolean;
    // the decision itself: the hard cuts from the times and scores of each frame
    class function FindCuts(const ATimes, AScores: array of Double; const AMinScore, ARatio, AWindow: Double): TDoubleDynArray; overload;
    // hard cuts and, with AGradual, dissolves and fades, sorted
    class function FindCuts(const AFrames: TMPVSceneFrames; const ASpan: Integer; const AMinScore, ARatio, AWindow: Double;
      const AGradual: Boolean; const AGradualMin: Double): TDoubleDynArray; overload;
    // only the dissolves and fades, given the hard cuts (sorted)
    class function FindGradual(const AFrames: TMPVSceneFrames; const ASpan: Integer; const AHardCuts: TDoubleDynArray;
      const AGradualMin: Double): TDoubleDynArray;

    property Status     : TMPVSceneDetectStatus read GetStatus;
    property ErrorText  : String                read GetErrorText;
    property FrameCount : Integer               read GetFrameCount;
    // minimum score (mean difference per pixel) for a cut, 8 by default
    property MinScore   : Double  read FMinScore write FMinScore;
    // how many times the score must be over the next one in the window, 3 by default
    property Ratio      : Double  read FRatio    write FRatio;
    // half the window in seconds, 0.25 by default
    property Window     : Double  read FWindow   write FWindow;
    // also dissolves and fades, True by default
    property Gradual    : Boolean read FGradual  write FGradual;
    // minimum G for a dissolve, 25 by default
    property GradualMinScore: Double read FGradualMin write FGradualMin;
    // size of the analysed image, 48x27 by default (at least 40x8)
    property Width      : Integer read FWidth    write FWidth;
    property Height     : Integer read FHeight   write FHeight;
    // mpv's "hwdec" for the detection ("no" by default, "auto-copy-safe" can
    // be faster with big videos)
    property HWDec      : String  read FHWDec    write FHWDec;
    // called in the main thread when the detection ends (done, cancelled or error)
    property OnFinished : TNotifyEvent read FOnFinished write FOnFinished;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

const
  FlashFrames = 3;   // frames of a flash that is not a cut
  MaxShift    = 3;   // pixels a block may move between the frames compared
  RingSize    = 64;  // frames kept, more than 2 * span + FlashFrames
  MaxSpan     = 24;
  LinearRatio = 0.3; // L <= this * G in a dissolve
  FadeSpan    = 1.0; // seconds around a fade without dissolves

type

  // a frame as luma and two colour differences, a value per pixel
  TPlanes = array[0..2] of array of SmallInt;

  { TFrameDiff }

  // D(A, B): the blocks, and the pixel lookups clamped at the borders
  TFrameDiff = class
  private
    FW, FH, FBX, FBY : Integer;
    FBlockOf         : array of Integer; // block of each pixel
    FBlockCount      : array of Integer; // pixels of each block
    FRect            : array of record X0, Y0, X1, Y1: Integer; end;
    FX               : array of array of Integer; // [dx][x] clamped x + dx
    FY               : array of array of Integer;
    function BlockLuma(const A, B: array of SmallInt; const ABlock, dx, dy: Integer): Integer;
  public
    constructor Create(const AWidth, AHeight: Integer);
    function Diff(const A, B: TPlanes; const AShift: Integer): Double;
    function Linearity(const A, C, B: TPlanes): Double;
  end;

  { TMPVSceneDetectThread }

  TMPVSceneDetectThread = class(TThread)
  private
    FOwner      : TMPVSceneDetector;
    FFileName   : String;
    FMinScore   : Double;
    FRatio      : Double;
    FWindow     : Double;
    FGradual    : Boolean;
    FGradualMin : Double;
    FWidth      : Integer;
    FHeight     : Integer;
    FHWDec      : String;
    FWake       : PRTLEvent;
    FCancel     : Boolean;
    FHandle     : Pmpv_handle;
    FRender     : Pmpv_render_context;
    // own copies: the player's renderer clears the global ones when it's freed
    rc_create   : function(var res: Pmpv_render_context; var mpv: mpv_handle; params: Pmpv_render_param): cint; cdecl;
    rc_callback : procedure(var ctx: mpv_render_context; callback: mpv_render_update_fn; callback_ctx: Pointer); cdecl;
    rc_update   : function(var ctx: mpv_render_context): uint64; cdecl;
    rc_render   : function(var ctx: mpv_render_context; params: Pmpv_render_param): cint; cdecl;
    rc_free     : procedure(var ctx: mpv_render_context); cdecl;
    FFrames     : TMPVSceneFrames;
    FCount      : Integer;
    function Setup(out AError: String): Boolean;
    procedure TearDown;
    procedure AddFrame(const AFrame: TMPVSceneFrame);
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TMPVSceneDetector; const AFileName: String);
    destructor Destroy; override;
    procedure Cancel;
  end;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

procedure WakeUp_CB(d: Pointer); cdecl;
begin
  // from mpv's threads: only signal, no mpv call here
  if d <> NIL then
    RTLEventSetEvent(PRTLEvent(d));
end;

// -----------------------------------------------------------------------------

procedure SortDoubles(var A: array of Double; L, R: Integer);
var
  i, j: Integer;
  p, t: Double;
begin
  while L < R do
  begin
    i := L;
    j := R;
    p := A[(L + R) div 2];
    repeat
      while A[i] < p do Inc(i);
      while A[j] > p do Dec(j);
      if i <= j then
      begin
        t := A[i]; A[i] := A[j]; A[j] := t;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if L < j then SortDoubles(A, L, j);
    L := i;
  end;
end;

// -----------------------------------------------------------------------------

{ TFrameDiff }

// -----------------------------------------------------------------------------

constructor TFrameDiff.Create(const AWidth, AHeight: Integer);
var
  x, y, d: Integer;
begin
  inherited Create;
  FW  := AWidth;
  FH  := AHeight;
  // blocks of about 8x9 pixels (6x3 on 48x27)
  FBX := Max(1, FW div 8);
  FBY := Max(1, FH div 9);
  SetLength(FBlockOf, FW * FH);
  SetLength(FBlockCount, FBX * FBY);
  SetLength(FRect, FBX * FBY);
  for x := 0 to FBX * FBY - 1 do
  begin
    FRect[x].X0 := MaxInt; FRect[x].Y0 := MaxInt;
    FRect[x].X1 := -1;     FRect[x].Y1 := -1;
  end;
  for y := 0 to FH-1 do
    for x := 0 to FW-1 do
    begin
      d := Min(y * FBY div FH, FBY-1) * FBX + Min(x * FBX div FW, FBX-1);
      FBlockOf[y*FW + x] := d;
      Inc(FBlockCount[d]);
      with FRect[d] do
      begin
        X0 := Min(X0, x); Y0 := Min(Y0, y);
        X1 := Max(X1, x); Y1 := Max(Y1, y);
      end;
    end;
  SetLength(FX, 2*MaxShift+1, FW);
  SetLength(FY, 2*MaxShift+1, FH);
  for d := -MaxShift to MaxShift do
  begin
    for x := 0 to FW-1 do FX[d+MaxShift][x] := EnsureRange(x + d, 0, FW-1);
    for y := 0 to FH-1 do FY[d+MaxShift][y] := EnsureRange(y + d, 0, FH-1) * FW;
  end;
end;

// -----------------------------------------------------------------------------

// the luma difference of a block of A against B moved by dx, dy
function TFrameDiff.BlockLuma(const A, B: array of SmallInt; const ABlock, dx, dy: Integer): Integer;
var
  x, y, i, j: Integer;
  xi: array of Integer;
begin
  Result := 0;
  xi := FX[dx+MaxShift];
  with FRect[ABlock] do
    for y := Y0 to Y1 do
    begin
      i := y * FW;
      j := FY[dy+MaxShift][y];
      for x := X0 to X1 do
        Inc(Result, Abs(A[i + x] - B[j + xi[x]]));
    end;
end;

// -----------------------------------------------------------------------------

// the mean of |dY| + |dCb| + |dCr| by blocks, each block at the shift of B
// (up to AShift pixels) where its luma matches best: a coarse search every 2
// pixels and then the 8 around the best one
function TFrameDiff.Diff(const A, B: TPlanes; const AShift: Integer): Double;
var
  dx, dy, bx, by, cx, cy, x, y, i, k, blk, nb, sum, best, step: Integer;
  xi: array of Integer;
begin
  nb := FBX * FBY;
  Result := 0;
  step := Max(1, AShift - 1);
  for blk := 0 to nb-1 do
  begin
    best := MaxInt;
    bx := 0;
    by := 0;
    dy := -AShift + (AShift mod step);
    while dy <= AShift do
    begin
      dx := -AShift + (AShift mod step);
      while dx <= AShift do
      begin
        sum := BlockLuma(A[0], B[0], blk, dx, dy);
        if (sum < best) or ((sum = best) and (Abs(dx) + Abs(dy) < Abs(bx) + Abs(by))) then
        begin
          best := sum;
          bx := dx;
          by := dy;
        end;
        Inc(dx, step);
      end;
      Inc(dy, step);
    end;
    if step > 1 then
    begin
      cx := bx;
      cy := by;
      for dy := Max(-AShift, cy-1) to Min(AShift, cy+1) do
        for dx := Max(-AShift, cx-1) to Min(AShift, cx+1) do
          if (dx <> cx) or (dy <> cy) then
          begin
            sum := BlockLuma(A[0], B[0], blk, dx, dy);
            if sum < best then
            begin
              best := sum;
              bx := dx;
              by := dy;
            end;
          end;
    end;
    // all three planes at that shift
    sum := 0;
    xi := FX[bx+MaxShift];
    with FRect[blk] do
      for y := Y0 to Y1 do
      begin
        i := y * FW;
        k := FY[by+MaxShift][y];
        for x := X0 to X1 do
          Inc(sum, Abs(A[0][i+x] - B[0][k + xi[x]]) + Abs(A[1][i+x] - B[1][k + xi[x]]) +
            Abs(A[2][i+x] - B[2][k + xi[x]]));
      end;
    Result := Result + sum / FBlockCount[blk];
  end;
  Result := Result / nb;
end;

// -----------------------------------------------------------------------------

// the mean of |2C - A - B| / 2 (all three planes): how far C is from the mix
// of A and B
function TFrameDiff.Linearity(const A, C, B: TPlanes): Double;
var
  p, i, n: Integer;
  s: Int64;
begin
  n := FW * FH;
  s := 0;
  for p := 0 to 2 do
    for i := 0 to n-1 do
      s := s + Abs(2*C[p][i] - A[p][i] - B[p][i]);
  Result := s / (2 * n);
end;

// -----------------------------------------------------------------------------

{ TMPVSceneDetectThread }

// -----------------------------------------------------------------------------

constructor TMPVSceneDetectThread.Create(AOwner: TMPVSceneDetector; const AFileName: String);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner      := AOwner;
  FFileName   := AFileName;
  FMinScore   := AOwner.FMinScore;
  FRatio      := AOwner.FRatio;
  FWindow     := AOwner.FWindow;
  FGradual    := AOwner.FGradual;
  FGradualMin := AOwner.FGradualMin;
  FWidth      := AOwner.FWidth;
  FHeight     := AOwner.FHeight;
  FHWDec      := AOwner.FHWDec;
  FWake       := RTLEventCreate;
  FCancel     := False;
  FHandle     := NIL;
  FRender     := NIL;
  FFrames     := NIL;
  FCount      := 0;
end;

// -----------------------------------------------------------------------------

destructor TMPVSceneDetectThread.Destroy;
begin
  RTLEventDestroy(FWake);
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetectThread.Cancel;
begin
  FCancel := True;
  RTLEventSetEvent(FWake);
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetectThread.AddFrame(const AFrame: TMPVSceneFrame);
begin
  if FCount >= Length(FFrames) then
    SetLength(FFrames, Length(FFrames) * 2 + 4096);
  FFrames[FCount] := AFrame;
  Inc(FCount);
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetectThread.Setup(out AError: String): Boolean;

  procedure Opt(const AName, AValue: String);
  begin
    mpv_set_option_string(FHandle^, PChar(AName), PChar(AValue));
  end;

  // two rows: the time and the frame number
  function Rows: String;
  begin
    Result := 'if(eq(Y,0),' +
      '255*mod(floor(floor(max(T,0)*1000000+0.5)/pow(2,X)),2),' +
      '255*mod(floor(N/pow(2,X)),2))';
  end;

var
  params: array[0..1] of mpv_render_param;
begin
  Result := False;
  AError := '';

  Pointer(rc_create)   := GetProcAddress(hLibMPV, 'mpv_render_context_create');
  Pointer(rc_callback) := GetProcAddress(hLibMPV, 'mpv_render_context_set_update_callback');
  Pointer(rc_update)   := GetProcAddress(hLibMPV, 'mpv_render_context_update');
  Pointer(rc_render)   := GetProcAddress(hLibMPV, 'mpv_render_context_render');
  Pointer(rc_free)     := GetProcAddress(hLibMPV, 'mpv_render_context_free');
  if not Assigned(rc_create) or not Assigned(rc_callback) or not Assigned(rc_update) or
    not Assigned(rc_render) or not Assigned(rc_free) then
  begin
    AError := 'libmpv without the render API';
    Exit;
  end;

  FHandle := mpv_create();
  if FHandle = NIL then
  begin
    AError := 'mpv_create failed';
    Exit;
  end;

  // a bare player: nothing from the user's mpv config, no audio, no
  // subtitles, no window, and as fast as it can decode
  Opt('config', 'no');
  Opt('load-scripts', 'no');
  Opt('ytdl', 'no');
  Opt('terminal', 'no');
  Opt('input-default-bindings', 'no');
  Opt('osc', 'no');
  Opt('idle', 'yes');
  Opt('keep-open', 'no');
  Opt('pause', 'no');
  Opt('vo', 'libmpv');
  Opt('ao', 'null');
  Opt('aid', 'no');
  Opt('sid', 'no');
  Opt('sub-auto', 'no');
  Opt('audio-file-auto', 'no');
  Opt('osd-level', '0');
  Opt('untimed', 'yes');
  Opt('video-timing-offset', '0');
  Opt('framedrop', 'no');      // every frame must reach the renderer
  Opt('keepaspect', 'no');     // fill the whole target, no black bars
  Opt('deinterlace', 'no');
  Opt('video-rotate', 'no');   // the rows under the image must stay there
  Opt('hwdec', FHWDec);
  Opt('vd-lavc-threads', '0');
  Opt('vd-lavc-skiploopfilter', 'all'); // faster, enough for this
  Opt('vd-lavc-fast', 'yes');
  Opt('sw-fast', 'yes');
  // the decoder's filter chain makes the tiny image, so the software
  // renderer only has to copy it. Two rows are stacked under it with the
  // frame's own time (us) and number in binary, a pixel per bit: time-pos
  // can't be used, mpv already has the next frame queued when this one is
  // rendered, so it's often one or two frames ahead
  Opt('vf', Format('lavfi=[scale=%d:%d:flags=area,setsar=1,format=gbrp,split[img][r];' +
    '[r]crop=%d:2:0:0,geq=r=''%s'':g=''%s'':b=''%s''[rows];[img][rows]vstack]',
    [FWidth, FHeight, FWidth, Rows, Rows, Rows]));

  if mpv_initialize(FHandle^) < 0 then
  begin
    AError := 'mpv_initialize failed';
    Exit;
  end;

  mpv_set_wakeup_callback(FHandle^, @WakeUp_CB, FWake);

  params[0]._type := MPV_RENDER_PARAM_API_TYPE;
  params[0].data  := PChar(MPV_RENDER_API_TYPE_SW);
  params[1]._type := MPV_RENDER_PARAM_INVALID;
  params[1].data  := NIL;
  if (rc_create(FRender, FHandle^, @params[0]) < 0) or (FRender = NIL) then
  begin
    FRender := NIL;
    AError := 'libmpv without the software renderer';
    Exit;
  end;
  rc_callback(FRender^, @WakeUp_CB, FWake);

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetectThread.TearDown;
begin
  if FRender <> NIL then
  begin
    rc_callback(FRender^, NIL, NIL);
    rc_free(FRender^);
    FRender := NIL;
  end;
  if FHandle <> NIL then
  begin
    mpv_set_wakeup_callback(FHandle^, NIL, NIL);
    mpv_terminate_destroy(FHandle^);
    FHandle := NIL;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetectThread.Execute;
var
  err          : String;
  args         : array[0..2] of PChar;
  ev           : Pmpv_event;
  params       : array[0..5] of mpv_render_param;
  size         : array[0..1] of cint;
  stride       : csize_t;
  block        : cint;
  buf          : TBytes;
  planes       : array[0..RingSize-1] of TPlanes;
  times        : array[0..RingSize-1] of Double;
  lumas        : array[0..RingSize-1] of Double;
  contrasts    : array[0..RingSize-1] of Double;
  // diffs[k mod RingSize][g] = D(F(k), F(k+g))
  diffs        : array[0..RingSize-1, 1..2*FlashFrames+1] of Double;
  fd           : TFrameDiff;
  n, i, k, g, pixels, span : Integer;
  r, gg, b, y  : Integer;
  frameNo, lastNo : Int64;
  back         : Integer;
  t, last, dur, fps, sum, sum2 : Double;
  loaded, ended, failed : Boolean;
  cuts         : TDoubleDynArray;

  // ring buffer: frame k is in slot k mod RingSize
  function Slot(const k: Integer): Integer; inline;
  begin
    Result := k mod RingSize;
  end;

  // a number written by the filter chain in a row under the image
  function ReadRow(const ARow, ABits: Integer): Int64;
  var
    x: Integer;
  begin
    Result := 0;
    for x := Min(ABits, FWidth)-1 downto 0 do
    begin
      Result := Result shl 1;
      if buf[(ARow * FWidth + x) * 4] >= 128 then
        Result := Result or 1;
    end;
  end;

  // the frames of G and L: about a third of a second
  procedure SetSpan;
  begin
    fps := 0;
    if (mpv_get_property(FHandle^, 'container-fps', MPV_FORMAT_DOUBLE, @fps) < 0) or
      IsNan(fps) or (fps <= 1) or (fps > 1000) then
      fps := 25;
    span := EnsureRange(Round(fps / 3), 4, MaxSpan);
  end;

  // what is measured of frame k, once the frames up to k+span are read (or
  // ALast, the last one at the end)
  procedure Emit(const k, ALast: Integer);
  var
    f: TMPVSceneFrame;
    a, bb: Integer;
    m: Double;
  begin
    f.Time     := times[Slot(k)];
    f.Luma     := lumas[Slot(k)];
    f.Contrast := contrasts[Slot(k)];
    // the smallest difference between the frames before k and from k on
    m := 0;
    if k > 0 then
    begin
      m := MaxDouble;
      for a := Max(0, k-1-FlashFrames) to k-1 do
        for bb := k to Min(k+FlashFrames, ALast) do
          if diffs[Slot(a)][bb-a] < m then
            m := diffs[Slot(a)][bb-a];
    end;
    f.Score := m;
    if (k - span >= 0) and (k + span <= ALast) then
    begin
      f.Gradual   := fd.Diff(planes[Slot(k-span)], planes[Slot(k+span)], 1);
      f.Linearity := fd.Linearity(planes[Slot(k-span)], planes[Slot(k)], planes[Slot(k+span)]);
    end
    else
    begin
      f.Gradual   := 0;
      f.Linearity := 0;
    end;
    AddFrame(f);
  end;

begin
  cuts   := NIL;
  failed := False;
  fd     := NIL;
  try
    if not Setup(err) then
    begin
      TearDown;
      FOwner.SetResult(sdsError, NIL, err);
      Exit;
    end;

    pixels := FWidth * FHeight;
    fd := TFrameDiff.Create(FWidth, FHeight);
    SetLength(buf, FWidth * (FHeight + 2) * 4);
    for i := 0 to RingSize-1 do
      for k := 0 to 2 do
        SetLength(planes[i][k], pixels);

    size[0] := FWidth;
    size[1] := FHeight + 2;
    stride  := FWidth * 4;
    block   := 0;
    params[0]._type := MPV_RENDER_PARAM_SW_SIZE;
    params[0].data  := @size[0];
    params[1]._type := MPV_RENDER_PARAM_SW_FORMAT;
    params[1].data  := PChar('rgb0');
    params[2]._type := MPV_RENDER_PARAM_SW_STRIDE;
    params[2].data  := @stride;
    params[3]._type := MPV_RENDER_PARAM_SW_POINTER;
    params[3].data  := @buf[0];
    params[4]._type := MPV_RENDER_PARAM_BLOCK_FOR_TARGET_TIME;
    params[4].data  := @block;
    params[5]._type := MPV_RENDER_PARAM_INVALID;
    params[5].data  := NIL;

    args[0] := 'loadfile';
    args[1] := PChar(FFileName);
    args[2] := NIL;
    if mpv_command(FHandle^, @args[0]) < 0 then
    begin
      FreeAndNil(fd);
      TearDown;
      FOwner.SetResult(sdsError, NIL, 'loadfile failed');
      Exit;
    end;

    n      := 0;
    last   := 0;
    lastNo := -1;
    back   := 0;
    dur    := 0;
    span   := 0;
    loaded := False;
    ended  := False;

    while not ended and not FCancel and not Terminated do
    begin
      RTLEventWaitFor(FWake, 100);

      // the player's events
      repeat
        ev := mpv_wait_event(FHandle^, 0);
        case ev^.event_id of
          MPV_EVENT_FILE_LOADED:
            begin
              loaded := True;
              if mpv_get_property(FHandle^, 'duration', MPV_FORMAT_DOUBLE, @dur) < 0 then
                dur := 0;
              if span = 0 then SetSpan;
            end;
          MPV_EVENT_END_FILE:
            begin
              ended := True;
              if (ev^.data <> NIL) and (Pmpv_event_end_file(ev^.data)^.reason = MPV_END_FILE_REASON_ERROR) then
              begin
                failed := True;
                err := mpv_error_string(Pmpv_event_end_file(ev^.data)^.error);
              end;
            end;
          MPV_EVENT_SHUTDOWN:
            ended := True;
        end;
      until (ev^.event_id = MPV_EVENT_NONE) or ended;

      // the frames, with their time and number from the rows under them
      if (rc_update(FRender^) and MPV_RENDER_UPDATE_FRAME) <> 0 then
      begin
        if rc_render(FRender^, @params[0]) < 0 then
          Continue;

        frameNo := ReadRow(FHeight + 1, 31);
        // a redraw of the same frame, not a new one
        if frameNo = lastNo then
          Continue;
        lastNo := frameNo;
        t := ReadRow(FHeight, 52) / 1000000;
        // the times must go forward: if they don't, the rows aren't there
        // (the filter chain failed and mpv is playing without it)
        if (n > 0) and (t <= last) then
        begin
          Inc(back);
          if (back > 8) and (back * 10 > n) then
          begin
            failed := True;
            err := 'video filter (lavfi geq) not available';
            Break;
          end;
          Continue;
        end;
        last := t;
        if span = 0 then SetSpan;

        // luma and the two colour differences, and the luma's mean and
        // standard deviation
        sum  := 0;
        sum2 := 0;
        k := Slot(n);
        for i := 0 to pixels-1 do
        begin
          r  := buf[i*4];
          gg := buf[i*4+1];
          b  := buf[i*4+2];
          y  := (77*r + 150*gg + 29*b + 128) shr 8;
          planes[k][0][i] := y;
          planes[k][1][i] := (b - y) div 2;
          planes[k][2][i] := (r - y) div 2;
          sum  := sum + y;
          sum2 := sum2 + y*y;
        end;
        times[k]     := t;
        lumas[k]     := sum / pixels;
        contrasts[k] := Sqrt(Max(0, sum2 / pixels - Sqr(sum / pixels)));

        for g := 1 to 2*FlashFrames+1 do
          if n - g >= 0 then
            diffs[Slot(n-g)][g] := fd.Diff(planes[Slot(n-g)], planes[k], MaxShift);

        if n - span >= 0 then
          Emit(n - span, n);

        Inc(n);
        if (n and 7) = 0 then
          FOwner.SetPosition(t, dur, n);
      end;
    end;

    TearDown;

    if FCancel or Terminated then
      FOwner.SetResult(sdsCancelled, NIL, '')
    else if failed or not loaded or (n = 0) then
    begin
      if err = '' then err := 'no video frames';
      FOwner.SetResult(sdsError, NIL, err);
    end
    else
    begin
      // the last frames
      for k := Max(0, n - span) to n-1 do
        Emit(k, n-1);
      FOwner.SetPosition(last, dur, n);
      SetLength(FFrames, FCount);
      cuts := TMPVSceneDetector.FindCuts(FFrames, span, FMinScore, FRatio, FWindow, FGradual, FGradualMin);
      FOwner.SetFrames(FFrames, span);
      FOwner.SetResult(sdsDone, cuts, '');
    end;
    FreeAndNil(fd);
  except
    on E: Exception do
    begin
      FreeAndNil(fd);
      TearDown;
      FOwner.SetResult(sdsError, NIL, E.Message);
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ TMPVSceneDetector }

// -----------------------------------------------------------------------------

constructor TMPVSceneDetector.Create(const ALibFileName: String = '');
begin
  inherited Create;
  InitCriticalSection(FLock);
  FThread      := NIL;
  FStatus      := sdsIdle;
  FErrorText   := '';
  FPosition    := 0;
  FDuration    := 0;
  FCuts        := NIL;
  FFrames      := NIL;
  FSpan        := 0;
  FFrameCount  := 0;
  FLibFileName := ALibFileName;
  FMinScore    := 8;
  FRatio       := 3;
  FWindow      := 0.25;
  FGradual     := True;
  FGradualMin  := 25;
  FWidth       := 48;
  FHeight      := 27;
  FHWDec       := 'no';
  FOnFinished  := NIL;
end;

// -----------------------------------------------------------------------------

destructor TMPVSceneDetector.Destroy;
begin
  FOnFinished := NIL;
  if Assigned(FThread) then
  begin
    TMPVSceneDetectThread(FThread).Cancel;
    FThread.WaitFor;
    FreeAndNil(FThread);
    Free_libMPV;
  end;
  TThread.RemoveQueuedEvents(NIL, @DoFinished);
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.Start(const AFileName: String): Boolean;
begin
  Result := False;
  if IsRunning then Exit;

  // the previous run (already ended)
  if Assigned(FThread) then
  begin
    FThread.WaitFor;
    FreeAndNil(FThread);
    Free_libMPV;
  end;

  // the image must be wide enough for the time row (40 bits, 12 days in us)
  if (FWidth < 40) or (FHeight < 8) or (FRatio < 1) or (FWindow <= 0) then Exit;
  if Load_libMPV(FLibFileName) <> MPV_ERROR_SUCCESS then Exit;

  EnterCriticalSection(FLock);
  try
    FStatus     := sdsRunning;
    FErrorText  := '';
    FPosition   := 0;
    FDuration   := 0;
    FFrameCount := 0;
    FCuts       := NIL;
    FFrames     := NIL;
    FSpan       := 0;
  finally
    LeaveCriticalSection(FLock);
  end;

  FThread := TMPVSceneDetectThread.Create(Self, AFileName);
  FThread.Start;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetector.Cancel;
begin
  if Assigned(FThread) then
    TMPVSceneDetectThread(FThread).Cancel;
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetector.WaitFor;
begin
  if Assigned(FThread) then
    FThread.WaitFor;
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.IsRunning: Boolean;
begin
  Result := GetStatus = sdsRunning;
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetector.SetPosition(const APosition, ADuration: Double; const AFrames: Integer);
begin
  EnterCriticalSection(FLock);
  try
    FPosition   := APosition;
    FDuration   := ADuration;
    FFrameCount := AFrames;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetector.SetFrames(const AFrames: TMPVSceneFrames; const ASpan: Integer);
begin
  EnterCriticalSection(FLock);
  try
    FFrames := Copy(AFrames);
    FSpan   := ASpan;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.GetFrames: TMPVSceneFrames;
begin
  EnterCriticalSection(FLock);
  try
    Result := Copy(FFrames);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetector.GetScores(out ATimes, AScores: TDoubleDynArray);
var
  i: Integer;
begin
  ATimes  := NIL;
  AScores := NIL;
  EnterCriticalSection(FLock);
  try
    SetLength(ATimes, Length(FFrames));
    SetLength(AScores, Length(FFrames));
    for i := 0 to High(FFrames) do
    begin
      ATimes[i]  := FFrames[i].Time;
      AScores[i] := FFrames[i].Score;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.HasScores: Boolean;
begin
  EnterCriticalSection(FLock);
  try
    Result := Length(FFrames) > 0;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.Recalculate(const AMinScore, ARatio: Double): Boolean;
var
  f: TMPVSceneFrames;
  c: TDoubleDynArray;
  span: Integer;
begin
  Result := False;
  if IsRunning or (ARatio < 1) then Exit;
  EnterCriticalSection(FLock);
  try
    f    := FFrames; // not changed while not running
    span := FSpan;
  finally
    LeaveCriticalSection(FLock);
  end;
  if Length(f) = 0 then Exit;

  FMinScore := AMinScore;
  FRatio    := ARatio;
  c := FindCuts(f, span, AMinScore, ARatio, FWindow, FGradual, FGradualMin);
  EnterCriticalSection(FLock);
  try
    FCuts := c;
  finally
    LeaveCriticalSection(FLock);
  end;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetector.SetResult(const AStatus: TMPVSceneDetectStatus; const ACuts: TDoubleDynArray; const AError: String);
begin
  EnterCriticalSection(FLock);
  try
    FCuts      := Copy(ACuts);
    FErrorText := AError;
    FStatus    := AStatus;
  finally
    LeaveCriticalSection(FLock);
  end;
  // queued, not synchronized: the main thread may be inside WaitFor
  TThread.Queue(NIL, @DoFinished);
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetector.DoFinished;
begin
  if Assigned(FOnFinished) then
    FOnFinished(Self);
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.GetStatus: TMPVSceneDetectStatus;
begin
  EnterCriticalSection(FLock);
  try
    Result := FStatus;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.GetErrorText: String;
begin
  EnterCriticalSection(FLock);
  try
    Result := FErrorText;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.GetFrameCount: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result := FFrameCount;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVSceneDetector.GetPosition(out APosition, ADuration: Double);
begin
  EnterCriticalSection(FLock);
  try
    APosition := FPosition;
    ADuration := FDuration;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.GetProgress: Double;
var
  p, d: Double;
begin
  GetPosition(p, d);
  if d > 0 then
  begin
    Result := p / d;
    if Result < 0 then Result := 0 else if Result > 1 then Result := 1;
  end
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.GetCuts: TDoubleDynArray;
begin
  EnterCriticalSection(FLock);
  try
    Result := Copy(FCuts);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVSceneDetector.GetCutsMs: TIntegerDynArray;
var
  c: TDoubleDynArray;
  i: Integer;
begin
  c := GetCuts;
  Result := NIL;
  SetLength(Result, Length(c));
  for i := 0 to High(c) do
    Result[i] := Round(c[i] * 1000);
end;

// -----------------------------------------------------------------------------

class function TMPVSceneDetector.FindCuts(const ATimes, AScores: array of Double;
  const AMinScore, ARatio, AWindow: Double): TDoubleDynArray;
var
  i, j, lo, hi, n, c, pk: Integer;
  s, other, nb: Double;
  peak: array of Boolean;
  strength: TDoubleDynArray;
begin
  Result := NIL;
  n := Min(Length(ATimes), Length(AScores));
  if n = 0 then Exit;

  // peaks: Ratio times over both neighbours
  peak := NIL;
  SetLength(peak, n);
  for i := 0 to n-1 do
  begin
    nb := 0;
    if i > 0 then nb := AScores[i-1];
    if (i < n-1) and (AScores[i+1] > nb) then nb := AScores[i+1];
    peak[i] := (AScores[i] >= AMinScore) and (AScores[i] >= ARatio * nb);
  end;

  strength := NIL;
  SetLength(Result, n);
  SetLength(strength, n);
  c  := 0;
  lo := 0;
  hi := 0;
  for i := 0 to n-1 do
  begin
    // the frames within +/- AWindow of frame i: [lo..hi]
    while ATimes[lo] < ATimes[i] - AWindow do Inc(lo);
    if hi < i then hi := i;
    while (hi+1 < n) and (ATimes[hi+1] <= ATimes[i] + AWindow) do Inc(hi);

    s := AScores[i];
    if not peak[i] or (ATimes[i] <= 0) then Continue;

    // the background: the highest of the window. A single other peak
    // there (another cut a few frames away) doesn't count, but several
    // are a pattern (a zoom or an animation changing every 2 frames)
    other := 0;
    nb    := 0;
    pk    := 0;
    for j := lo to hi do
      if j <> i then
        if peak[j] then
        begin
          Inc(pk);
          if AScores[j] > nb then nb := AScores[j];
        end
        else if AScores[j] > other then
          other := AScores[j];
    if (pk > 1) and (nb > other) then
      other := nb;

    if s < ARatio * other then Continue;

    // too close to the previous cut: only the stronger one
    if (c > 0) and (ATimes[i] - Result[c-1] < AWindow) then
    begin
      if s > strength[c-1] then
      begin
        Result[c-1]   := ATimes[i];
        strength[c-1] := s;
      end;
      Continue;
    end;

    Result[c]   := ATimes[i];
    strength[c] := s;
    Inc(c);
  end;
  SetLength(Result, c);
end;

// -----------------------------------------------------------------------------

class function TMPVSceneDetector.FindCuts(const AFrames: TMPVSceneFrames; const ASpan: Integer;
  const AMinScore, ARatio, AWindow: Double; const AGradual: Boolean; const AGradualMin: Double): TDoubleDynArray;
var
  t, sc, g: TDoubleDynArray;
  i, a, b, c: Integer;
begin
  t  := NIL;
  sc := NIL;
  SetLength(t, Length(AFrames));
  SetLength(sc, Length(AFrames));
  for i := 0 to High(AFrames) do
  begin
    t[i]  := AFrames[i].Time;
    sc[i] := AFrames[i].Score;
  end;
  Result := FindCuts(t, sc, AMinScore, ARatio, AWindow);
  if not AGradual or (ASpan <= 0) then Exit;

  g := FindGradual(AFrames, ASpan, Result, AGradualMin);
  if Length(g) = 0 then Exit;
  // both sorted: merge
  t := Result;
  Result := NIL;
  SetLength(Result, Length(t) + Length(g));
  a := 0;
  b := 0;
  for c := 0 to High(Result) do
    if (b > High(g)) or ((a <= High(t)) and (t[a] <= g[b])) then
    begin
      Result[c] := t[a];
      Inc(a);
    end
    else
    begin
      Result[c] := g[b];
      Inc(b);
    end;
end;

// -----------------------------------------------------------------------------

class function TMPVSceneDetector.FindGradual(const AFrames: TMPVSceneFrames; const ASpan: Integer;
  const AHardCuts: TDoubleDynArray; const AGradualMin: Double): TDoubleDynArray;
var
  n, i, j, k, q, c, nb: Integer;
  fd: Double;
  dt: TDoubleDynArray;
  flat: array of Boolean;
  blocked: array of record a, b: Double; end;
  cand: array of Integer;
  ok, cutIn, cutOut: Boolean;

  // a hard cut within ASpan seconds of ATime
  function NearHard(const ATime, ASpanTime: Double): Boolean;
  var
    lo, hi, m: Integer;
  begin
    // the first cut >= ATime - ASpanTime
    lo := 0;
    hi := Length(AHardCuts);
    while lo < hi do
    begin
      m := (lo + hi) div 2;
      if AHardCuts[m] < ATime - ASpanTime then lo := m + 1 else hi := m;
    end;
    Result := (lo < Length(AHardCuts)) and (AHardCuts[lo] <= ATime + ASpanTime);
  end;

  procedure Block(const A, B: Double);
  begin
    SetLength(blocked, Length(blocked)+1);
    blocked[High(blocked)].a := A;
    blocked[High(blocked)].b := B;
  end;

  procedure Add(const ATime: Double);
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := ATime;
  end;

begin
  Result := NIL;
  n := Length(AFrames);
  if (n < 2*ASpan+1) or (ASpan <= 0) then Exit;

  // the usual frame duration (median)
  dt := NIL;
  SetLength(dt, n-1);
  for i := 1 to n-1 do
    dt[i-1] := AFrames[i].Time - AFrames[i-1].Time;
  SortDoubles(dt, 0, High(dt));
  fd := dt[High(dt) div 2];

  // flat frames: a plain colour, or black or white with a little text
  flat := NIL;
  SetLength(flat, n);
  for i := 0 to n-1 do
    with AFrames[i] do
      flat[i] := (Contrast < 3) or (((Luma < 8) or (Luma > 247)) and (Contrast < 6));

  // the runs of flat frames: fades
  blocked := NIL;
  i := 0;
  while i < n do
  begin
    if not flat[i] then
    begin
      Inc(i);
      Continue;
    end;
    j := i;
    while (j+1 < n) and flat[j+1] do Inc(j);
    // a fade in at the start or out at the end is not a change
    if i = 0 then Block(AFrames[0].Time, AFrames[j].Time + FadeSpan);
    if j = n-1 then Block(AFrames[i].Time - FadeSpan, AFrames[n-1].Time);
    // entered and left with a cut, or gradually
    cutIn  := (i = 0) or NearHard(AFrames[i].Time, 2.5 * fd);
    cutOut := (j = n-1) or NearHard(AFrames[j+1].Time, 2.5 * fd);
    if not cutIn and not cutOut then
    begin
      // out, flat, in: a single change, in the middle
      Add(AFrames[(i + j + 1) div 2].Time);
      Block(AFrames[i].Time - FadeSpan, AFrames[j].Time + FadeSpan);
    end;
    i := j + 1;
  end;

  // dissolves: the highest G within +/- span, linear, away from the cuts
  cand := NIL;
  for k := ASpan to n-1-ASpan do
    with AFrames[k] do
    begin
      if (Gradual < AGradualMin) or (Linearity > LinearRatio * Gradual) or (Time <= 0) then Continue;
      ok := True;
      for q := k-ASpan to k+ASpan do
        if AFrames[q].Gradual > Gradual then
        begin
          ok := False;
          Break;
        end;
      if not ok or NearHard(Time, (ASpan + 1) * fd) then Continue;
      for q := 0 to High(blocked) do
        if (Time >= blocked[q].a) and (Time <= blocked[q].b) then
        begin
          ok := False;
          Break;
        end;
      if not ok then Continue;
      SetLength(cand, Length(cand)+1);
      cand[High(cand)] := k;
    end;

  // the strongest first, none closer than 2 spans to a stronger one
  nb := Length(cand);
  for i := 1 to nb-1 do
  begin
    k := cand[i];
    j := i - 1;
    while (j >= 0) and (AFrames[cand[j]].Gradual < AFrames[k].Gradual) do
    begin
      cand[j+1] := cand[j];
      Dec(j);
    end;
    cand[j+1] := k;
  end;
  c := 0;
  for i := 0 to nb-1 do
  begin
    ok := True;
    for j := 0 to c-1 do
      if Abs(cand[i] - cand[j]) <= 2 * ASpan then
      begin
        ok := False;
        Break;
      end;
    if ok then
    begin
      cand[c] := cand[i];
      Inc(c);
    end;
  end;
  for i := 0 to c-1 do
    Add(AFrames[cand[i]].Time);

  if Length(Result) > 1 then
    SortDoubles(Result, 0, High(Result));
end;

// -----------------------------------------------------------------------------

end.
