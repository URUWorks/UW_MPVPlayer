{*
 *  URUWorks MPVPlayer
 *
 *  Author  : URUWorks
 *  Website : uruworks.net
 *
 *  The contents of this file are used with permission, subject to
 *  the Mozilla Public License Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/2.0.html
 *
 *  Software distributed under the License is distributed on an
 *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing
 *  rights and limitations under the License.
 *
 *  Copyright (C) 2021-2023 URUWorks, uruworks@gmail.com.
 *
 *  Based on the great work of OvoM3U
 *  Copyright (C) 2020 Marco Caselli.
 *}

unit MPVPlayer;

// -----------------------------------------------------------------------------

{$I MPVPlayer.inc}

interface

uses
  Classes, Controls, SysUtils, LazFileUtils, ExtCtrls, Graphics, LCLType,
  LResources, LazarusPackageIntf, libMPV.Client, MPVPlayer.Thread,
  MPVPlayer.RenderGL, OpenGLContext
  {$IFDEF BGLCONTROLS}, BGRAOpenGL{$ENDIF};

// -----------------------------------------------------------------------------

type

  { TMPVPlayer Types }

  TMPVPlayerRenderMode  = (rmEmbedding, rmOpenGL);
  TMPVPlayerSate        = (psStop, psPlay, psPause, psEnd);
  TMPVPlayerTrackType   = (ttVideo, ttAudio, ttSubtitle, ttUnknown);
  TMPVPlayerLogLevel    = (llNo, llFatal, llError, llWarn, llInfo, llStatus, llV, llDebug, llTrace);
  TMPVPlayerNotifyEvent = procedure(ASender: TObject; AParam: Integer) of object;
  TMPVPlayerLogEvent    = procedure(ASender: TObject; APrefix, ALevel, AText: String) of object;

  TMPVPlayerTrackInfo = record
    Kind     : TMPVPlayerTrackType;
    ID       : Integer;
    Codec    : String;
    Title    : String;
    Lang     : String;
    Selected : Boolean;
  end;

  TMPVPlayerTrackList = array of TMPVPlayerTrackInfo;

  { TMPVPlayer }

  TMPVPlayer = class(TCustomPanel)
  private
    FMPV_HANDLE   : Pmpv_handle;
    FError        : mpv_error;
    FVersion      : DWord;
    FGL           : TOpenGLControl;
    FInitialized  : Boolean;
    FStartOptions : TStringList;
    FLogLevel     : TMPVPlayerLogLevel;
    FMPVEvent     : TMPVPlayerThreadEvent;
    FState        : TMPVPlayerSate;
    FTrackList    : TMPVPlayerTrackList;
    FAutoStart    : Boolean;
    FAutoLoadSub  : Boolean;
    FKeepAspect   : Boolean;
    FStartAtPosMs : Integer;
    FFileName     : String;
    {$IFDEF USETIMER}
    FTimer        : TTimer;
    {$ENDIF}
    FRenderMode   : TMPVPlayerRenderMode;
    FRenderGL     : TMPVPlayerRenderGL;

    FOnStartFile: TNotifyEvent;            // Notification before playback start of a file (before the file is loaded).
    FOnEndFile: TMPVPlayerNotifyEvent;     // Notification after playback end (after the file was unloaded), AParam is mpv_end_file_reason.
    FOnFileLoaded: TNotifyEvent;           // Notification when the file has been loaded (headers were read etc.)
    FOnVideoReconfig: TNotifyEvent;        // Happens after video changed in some way.
    FOnAudioReconfig: TNotifyEvent;        // Similar to VIDEO_RECONFIG.
    FOnSeek: TMPVPlayerNotifyEvent;        // Happens when a seek was initiated.
    FOnPlaybackRestart: TNotifyEvent;      // Usually happens on start of playback and after seeking.
    FOnPlay: TNotifyEvent;                 // Play by user
    FOnStop: TNotifyEvent;                 // Stop by user
    FOnPause: TNotifyEvent;                // Pause by user
    FOnTimeChanged: TMPVPlayerNotifyEvent; // Notify playback time, AParam is current position.
    FOnBuffering: TMPVPlayerNotifyEvent;   // Whether playback is paused because of waiting for the cache.
    FOnLogMessage: TMPVPlayerLogEvent;     // Receives messages enabled with mpv_request_log_messages().

    {$IFDEF BGLCONTROLS}
    FOnDrawEvent: TMPVPlayerDrawEvent;
    {$ENDIF}

    function Initialize: Boolean;
    procedure UnInitialize;

    procedure InitializeRenderGL;
    procedure UnInitializeRenderGL;

    procedure PushEvent;
    procedure ReceivedEvent(Sender: TObject);

    procedure SetRenderMode(Value: TMPVPlayerRenderMode);
    function LogLevelToString: String;

    {$IFDEF USETIMER}
    procedure DoTimer(Sender: TObject);
    {$ENDIF}

    procedure DoResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsLibMPVAvailable: Boolean;
    procedure mpv_command_(Args: Array of const);
    procedure mpv_set_option_string_(const AValue: String);
    function mpv_get_property_boolean(const APropertyName: String): Boolean;
    procedure mpv_set_property_boolean(const APropertyName: String; const AValue: Boolean);
    function mpv_get_property_double(const AProperty: String): Double;
    procedure mpv_set_property_double(const AProperty: String; const AValue: Double);
    function mpv_get_property_int64(const AProperty: String): Int64;
    procedure mpv_set_property_int64(const AProperty: String; const AValue: Int64);

    function GetErrorString: String;
    function GetVersionString: String;
    function GetPlayerHandle: Pmpv_handle;

    procedure Play(const AFileName: String; const AStartAtPositionMs: Integer = 0);
    procedure Pause;
    procedure Resume;
    procedure Stop;
    function IsPlaying: Boolean;
    function IsPaused: Boolean;
    function GetMediaLenInMs: Integer;
    function GetMediaPosInMs: Integer;
    procedure SetMediaPosInMs(const AValue: Integer);
    procedure SeekInMs(const MSecs: Integer; const SeekAbsolute: Boolean = True);
    procedure NextFrame;
    procedure PreviousFrame;
    procedure SetPlaybackRate(const AValue: Byte);
    function GetAudioVolume: Integer;
    procedure SetAudioVolume(const AValue: Integer);
    procedure SetTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer); overload;
    procedure SetTrack(const Index: Integer); overload;
    procedure GetTracks;
    procedure LoadTrack(const TrackType: TMPVPlayerTrackType; const AFileName: String);
    procedure RemoveTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
    procedure ShowText(const AText: String; Duration: Integer = 0; FontSize: Integer = 0);
    procedure SetTextColor(const AValue: String);
    procedure SetTextHAlign(const AValue: String);
    procedure SetTextVAlign(const AValue: String);
    procedure SetTextSize(const AValue: Int64);

    function GetVideoWidth: Integer;
    function GetVideoHeight: Integer;
    function GetVideoTotalFrames: Integer;
    function GetVideoFPS: Double;

    property mpv_handle    : Pmpv_handle         read FMPV_HANDLE;
    property Error         : mpv_error           read FError;
    property ErrorString   : String              read GetErrorString;
    property Version       : DWord               read FVersion;
    property VersionString : String              read GetVersionString;
    property Initialized   : Boolean             read FInitialized;
    property StartOptions  : TStringList         read FStartOptions;
    property TrackList     : TMPVPlayerTrackList read FTrackList;
    property FileName      : String              read FFileName;

    {$IFDEF USETIMER}
    property Timer         : TTimer read FTimer;
    {$ENDIF}
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color default clBlack;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property UseDockManager default True;
    property Visible;
    property Width default 320;
    property Height default 240;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    property AutoStartPlayback: Boolean read FAutoStart write FAutoStart;
    property AutoLoadSubtitle: Boolean read FAutoLoadSub write FAutoLoadSub;
    property KeepAspect: Boolean read FKeepAspect write FKeepAspect;
    property RendererMode: TMPVPlayerRenderMode read FRenderMode write SetRenderMode;
    property LogLevel: TMPVPlayerLogLevel read FLogLevel write FLogLevel;

    property OnStartFile: TNotifyEvent read FOnStartFile write FOnStartFile;
    property OnEndFile: TMPVPlayerNotifyEvent read FOnEndFile write FOnEndFile;
    property OnFileLoaded: TNotifyEvent read FOnFileLoaded write FOnFileLoaded;
    property OnVideoReconfig: TNotifyEvent read FOnVideoReconfig write FOnVideoReconfig;
    property OnAudioReconfig: TNotifyEvent read FOnAudioReconfig write FOnAudioReconfig;
    property OnSeek: TMPVPlayerNotifyEvent read FOnSeek write FOnSeek;
    property OnPlaybackRestart: TNotifyEvent read FOnPlaybackRestart write FOnPlaybackRestart;

    property OnPlay : TNotifyEvent read FOnPlay  write FOnPlay;
    property OnStop : TNotifyEvent read FOnStop  write FOnStop;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    property OnTimeChanged: TMPVPlayerNotifyEvent read FOnTimeChanged write FOnTimeChanged;
    property OnBuffering: TMPVPlayerNotifyEvent read FOnBuffering write FOnBuffering;
    property OnLogMessage: TMPVPlayerLogEvent read FOnLogMessage write FOnLogMessage;

    {$IFDEF BGLCONTROLS}
    property OnDraw: TMPVPlayerDrawEvent read FOnDrawEvent write FOnDrawEvent;
    {$ENDIF}
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

const
  LIBMPV_MAX_VOLUME = 100;

// -----------------------------------------------------------------------------

{ libmpv wakeup_events }

// -----------------------------------------------------------------------------

procedure LIBMPV_EVENT(Sender: Pointer); cdecl;
begin
  if (Sender <> NIL) then TMPVPlayer(Sender).PushEvent;
end;

// -----------------------------------------------------------------------------

{ Helpers}

// -----------------------------------------------------------------------------

function TMPVPlayer.IsLibMPVAvailable: Boolean;
begin
  Result := IsLibMPV_Installed;
end;

// -----------------------------------------------------------------------------
procedure TMPVPlayer.mpv_command_(Args: Array of const);
var
  pArgs: array of PChar;
  i: Integer;
begin
  if FInitialized and (High(Args) > 0) then
  begin
    SetLength(pArgs, High(Args)+1);

    for i := 0 to High(Args) do
      pArgs[i] := Args[i].VAnsiString;

    pArgs[High(Args)+1] := NIL;

    FError := mpv_command(FMPV_HANDLE^, PPChar(@pArgs[0]));
    SetLength(pArgs, 0);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_option_string_(const AValue: String);
var
  s1, s2: String;
  i: Integer;
begin
  if AValue.IsEmpty or not Assigned(mpv_set_option_string) then Exit;

  i := Pos('=', AValue);
  if i > 0 then
  begin
    s1 := Copy(AValue, 1, i-1);
    s2 := Copy(AValue, i+1, AValue.Length-i);
  end
  else
  begin
    s1 := AValue;
    s2 := '';
  end;

  FError := mpv_set_option_string(FMPV_HANDLE^, PChar(s1), PChar(s2));
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_boolean(const APropertyName: String): Boolean;
var
  p: Integer;
begin
  Result := False;
  if not FInitialized then Exit;
  FError := mpv_get_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_FLAG, @p);
  Result := Boolean(p);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_boolean(const APropertyName: String; const AValue: Boolean);
var
  p: Integer;
begin
  if not FInitialized then Exit;

  if AValue then
    p := 1
  else
    p := 0;

  FError := mpv_set_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_FLAG, @p);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_double(const AProperty: String): Double;
begin
  if FInitialized then
    FError := mpv_get_property(FMPV_HANDLE^, PChar(AProperty), MPV_FORMAT_DOUBLE, @Result)
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_double(const AProperty: String; const AValue: Double);
begin
  if FInitialized then
    FError := mpv_set_property(FMPV_HANDLE^, PChar(AProperty), MPV_FORMAT_DOUBLE, @AValue);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_int64(const AProperty: String): Int64;
begin
  if FInitialized then
    FError := mpv_get_property(FMPV_HANDLE^, PChar(AProperty), MPV_FORMAT_INT64, @Result)
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_int64(const AProperty: String; const AValue: Int64);
begin
  if FInitialized then
    FError := mpv_set_property(FMPV_HANDLE^, PChar(AProperty), MPV_FORMAT_INT64, @AValue);
end;

// -----------------------------------------------------------------------------

{ TMPVPlayer }

// -----------------------------------------------------------------------------

constructor TMPVPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Our panel settings
  DoubleBuffered   := True;
  Width            := 320;
  Height           := 240;
  TabStop          := True;
  BevelOuter       := bvNone;
  ParentBackground := False;
  ParentColor      := False;
  Color            := clBlack;
  FullRepaint      := False;
  Caption          := '';

  // our control setup
  FGL            := NIL;
  FMPV_HANDLE    := NIL;
  FVersion       := 0;
  FError         := 0;
  FInitialized   := False;
  FMPVEvent      := NIL;
  FLogLevel      := llStatus;
  FState         := psStop;
  FAutoStart     := True;
  FAutoLoadSub   := False;
  FKeepAspect    := True;
  FFileName      := '';
  FRenderMode    := rmOpenGL;
  FStartOptions  := TStringList.Create;
  SetLength(FTrackList, 0);

  FRenderGL   := NIL;

  {$IFDEF USETIMER}
  FTimer          := TTimer.Create(NIL);
  FTimer.Enabled  := False;
  FTimer.Interval := 75;
  FTimer.OnTimer  := @DoTimer;
  {$ENDIF}

  with FStartOptions do
  begin
    Add('hwdec=auto');       // enable best hw decoder
    Add('keep-open=always'); // don't auto close video
    Add('vd-lavc-dr=no');    // fix possibles deadlock issues with OpenGL
  end;
end;

// -----------------------------------------------------------------------------

destructor TMPVPlayer.Destroy;
begin
  UnInitialize;

  {$IFDEF USETIMER}
  FTimer.Free;
  {$ENDIF}

  FStartOptions.Free;
  FStartOptions := NIL;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.Initialize: Boolean;
var
  i: Integer;
  pHwnd: PtrInt;
begin
  FInitialized := False;
  Result := False;

  FError := Load_libMPV;
  if FError <> MPV_ERROR_SUCCESS then Exit;

  FMPV_HANDLE := mpv_create();
  if not Assigned(FMPV_HANDLE) then
  begin
    FError := MPV_ERROR_UNSUPPORTED;
    Exit;
  end;

  // Get version lib
  if Assigned(mpv_client_api_version) then
    FVersion := mpv_client_api_version();

  if not FAutoStart then
    FStartOptions.Add('pause');  // Start the player in paused state.

  if not FAutoLoadSub then
    FStartOptions.Add('sub=no'); // don't load subtitles

  if not FKeepAspect then
    FStartOptions.Add('no-keepaspect'); // always stretch the video to window size

  for i := 0 to FStartOptions.Count-1 do
    mpv_set_option_string_(FStartOptions[i]);

  if FRenderMode = rmEmbedding then
  begin
    // Set our window handle
    {$IFDEF LINUX}
    pHwnd := GDK_WINDOW_XWINDOW(PGtkWidget(Self.Handle)^.window);
    {$ELSE}
    pHwnd := Self.Handle;
    {$ENDIF}
    FError := mpv_set_option(FMPV_HANDLE^, 'wid', MPV_FORMAT_INT64, @pHwnd);

    if FError <> MPV_ERROR_SUCCESS then
      Exit;
  end;

  {$IFNDEF USETIMER}
  mpv_observe_property(FMPV_HANDLE^, 0, 'playback-time', MPV_FORMAT_INT64);
  {$ENDIF}
  //mpv_observe_property(FMPV_HANDLE^, 0, 'paused-for-cache', MPV_FORMAT_INT64);
  mpv_observe_property(FMPV_HANDLE^, 0, 'cache-buffering-state', MPV_FORMAT_INT64);

  FError := mpv_initialize(FMPV_HANDLE^);
  if FError <> MPV_ERROR_SUCCESS then
  begin
    Free_libMPV;
    Exit;
  end;

  FError := mpv_request_log_messages(FMPV_HANDLE^, PChar(LogLevelToString));

  FMPVEvent := TMPVPlayerThreadEvent.Create;
  FMPVEvent.OnEvent := @ReceivedEvent;
  mpv_set_wakeup_callback(FMPV_HANDLE^, @LIBMPV_EVENT, Self);

  if FRenderMode = rmOpenGL then
    InitializeRenderGL;

  FInitialized := True;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.UnInitialize;
begin
  if not FInitialized then Exit;

  if Assigned(mpv_set_wakeup_callback) and Assigned(FMPV_HANDLE) then
    mpv_set_wakeup_callback(FMPV_HANDLE^, NIL, Self);

  if Assigned(FMPVEvent) then
  begin
    FMPVEvent.Free;
    FMPVEvent := NIL;
  end;

  if FRenderMode = rmOpenGL then
    UnInitializeRenderGL;

  if Assigned(mpv_terminate_destroy) and Assigned(FMPV_HANDLE) then
    mpv_terminate_destroy(FMPV_HANDLE^);

  FMPV_HANDLE := NIL;
  SetLength(FTrackList, 0);
  FFileName := '';
  FInitialized := False;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.InitializeRenderGL;
begin
  FGL          := TOpenGLControl.Create(Self);
  FGL.Parent   := Self;
  FGL.Align    := alClient;
  FGL.OnClick  := OnClick;
  FGL.OnResize := @DoResize; // force to draw opengl context when paused

  FRenderGL := TMPVPlayerRenderGL.Create(FGL, FMPV_HANDLE {$IFDEF BGLCONTROLS}, FOnDrawEvent{$ENDIF});
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.UnInitializeRenderGL;
begin
  FRenderGL.Free;
  FRenderGL := NIL;

  FGL.Free;
  FGL := NIL;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetPlayerHandle: Pmpv_handle;
begin
  Result := FMPV_HANDLE;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetErrorString: String;
begin
  if Assigned(mpv_error_string) then
    Result := mpv_error_string(FError)
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetVersionString: String;
begin
  Result := Format('libmpv %d.%d', [FVersion shr 16, FVersion and $FF]);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.LogLevelToString: String;
begin
  case FLogLevel of
    llFatal  : Result := 'fatal';  // fatal messages only
    llError  : Result := 'error';  // error messages
    llWarn   : Result := 'warn';   // warning messages
    llInfo   : Result := 'info';   // informational messages
    llStatus : Result := 'status'; // status messages (default)
    llV      : Result := 'v';      // verbose messages
    llDebug  : Result := 'debug';  // debug messages
    llTrace  : Result := 'trace';  // very noisy debug messages
  else
    Result := 'no'; // complete silence
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Play(const AFileName: String; const AStartAtPositionMs: Integer = 0);
begin
  if not FInitialized then
    Initialize;

  FStartAtPosMs := AStartAtPositionMs;
  FFileName := AFileName;
  mpv_command_(['loadfile', FFileName]);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Pause;
begin
  if IsPlaying then
  begin
    mpv_set_property_boolean('pause', True);
    FState := psPause;
    if Assigned(FOnPause) then FOnPause(Self);
  end
  else
  begin
    mpv_set_property_boolean('pause', False);
    FState := psPlay;
    if Assigned(FOnPlay) then FOnPlay(Self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Resume;
begin
  if IsPaused then
  begin
    mpv_set_property_boolean('pause', False);
    FState := psPlay;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Stop;
begin
  if FState <> psStop then
  begin
    mpv_set_property_boolean('pause', True);
    SetMediaPosInMs(0);
    FState := psStop;
    if Assigned(FOnStop) then FOnStop(Self);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.IsPlaying: Boolean;
begin
  Result := (FState = psPlay);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.IsPaused: Boolean;
begin
  Result := (FState = psPause);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetMediaLenInMs: Integer;
begin
  Result := Trunc(mpv_get_property_double('duration') * 1000);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetMediaPosInMs: Integer;
begin
  Result := Trunc(mpv_get_property_double('time-pos') * 1000);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetMediaPosInMs(const AValue: Integer);
begin
  mpv_set_property_double('time-pos', AValue / 1000);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SeekInMs(const MSecs: Integer; const SeekAbsolute: Boolean = True);
begin
  if SeekAbsolute then
    SetMediaPosInMs(MSecs)
  else
    SetMediaPosInMs(GetMediaPosInMs + MSecs);
end;

// -----------------------------------------------------------------------------
procedure TMPVPlayer.NextFrame;
begin
  mpv_command_(['frame-step']);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.PreviousFrame;
begin
  mpv_command_(['frame-back-step']);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetPlaybackRate(const AValue: Byte);
begin
  mpv_set_property_double('speed', AValue / 100.0);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetAudioVolume: Integer;
begin
  Result := Trunc(mpv_get_property_double('volume') * (255 / LIBMPV_MAX_VOLUME));
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetAudioVolume(const AValue: Integer);
begin
  mpv_set_property_double('volume', AValue * (LIBMPV_MAX_VOLUME / 255));
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer);
var
  s: String;
begin
  case TrackType of
    ttAudio    : s := 'aid';
    ttVideo    : s := 'vid';
    ttSubtitle : s := 'sid';
  else
    Exit;
  end;

  mpv_set_property_int64(s, ID);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTrack(const Index: Integer);
begin
  SetTrack(TrackList[Index].Kind, TrackList[Index].ID);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.GetTracks;
var
  i, j: integer;
  Node, Map, Detail: mpv_node;
  Keys: PPChar;
  Key, Value: String;
begin
  FError := mpv_get_property(FMPV_HANDLE^, 'track-list', MPV_FORMAT_NODE, @Node);
  if FError = MPV_ERROR_SUCCESS then
  begin
    try
      SetLength(FTrackList, Node.u.list^.num);
      for i := 0 to Node.u.list^.num -1 do
      begin
        map  := Node.u.list^.values^[i];
        Keys := map.u.list^.keys;
        FillByte(FTrackList[i], SizeOf(TMPVPlayerTrackInfo), 0);

        for j := 0 to map.u.list^.num -1 do
        begin
          Detail := map.u.list^.values^[j];
          Key    := StrPas(Keys^);

          if Key = 'id' then
            FTrackList[i].Id := Detail.u.int64_
          else if Key = 'type' then
          begin
            Value := StrPas(Detail.u._string);
            if Value = 'audio' then
              FTrackList[i].Kind := ttAudio
            else if Value = 'video' then
              FTrackList[i].Kind := ttVideo
            else if Value = 'sub' then
              FTrackList[i].Kind := ttSubtitle
            else
              FTrackList[i].Kind := ttUnknown;
          end
          else if Key = 'title' then
            FTrackList[i].title := StrPas(Detail.u._string)
          else if Key = 'lang' then
            FTrackList[i].Lang := StrPas(Detail.u._string)
          else if Key = 'codec' then
            FTrackList[i].Codec := StrPas(Detail.u._string)
          else if Key = 'selected' then
            FTrackList[i].Selected := Detail.u.flag = 1;

          Inc(Keys);
        end;
      end;
    except
    end;
    mpv_free_node_contents(Node);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.LoadTrack(const TrackType: TMPVPlayerTrackType; const AFileName: String);
var
  s: String;
begin
  If AFileName.IsEmpty then Exit;

  case TrackType of
    ttAudio    : s := 'audio-add';
    ttVideo    : s := 'video-add';
    ttSubtitle : s := 'sub-add';
  else
    Exit;
  end;

  mpv_command_([s, AFileName]);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.RemoveTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
var
  s: String;
begin
  case TrackType of
    ttAudio    : s := 'audio-remove';
    ttVideo    : s := 'video-remove';
    ttSubtitle : s := 'sub-remove';
  else
    Exit;
  end;

  if ID > -1 then
    mpv_command_([s, ID])
  else
    mpv_command_([s]);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ShowText(const AText: String; Duration: Integer = 0; FontSize: Integer = 0);
begin
  if AText.IsEmpty then Exit;

  if Duration = 0 then
    Duration := mpv_get_property_int64('osd-duration');

  if FontSize = 0 then
    FontSize := mpv_get_property_int64('osd-font-size');

  mpv_command_(['expand-properties', 'show-text', '${osd-ass-cc/0}' + AText]);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTextColor(const AValue: String);
begin
  mpv_set_option_string_('osd-color='+AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTextVAlign(const AValue: String);
begin
  mpv_set_option_string_('osd-align-y='+AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTextHAlign(const AValue: String);
begin
  mpv_set_option_string_('osd-align-x='+AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTextSize(const AValue: Int64);
begin
  mpv_set_property_int64('osd-font-size', AValue);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetVideoWidth: Integer;
begin
  Result := mpv_get_property_int64('width');
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetVideoHeight: Integer;
begin
  Result := mpv_get_property_int64('height');
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetVideoTotalFrames: Integer;
begin
  Result := mpv_get_property_int64('estimated-frame-count');
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetVideoFPS: Double;
begin
  Result := mpv_get_property_double('container-fps');
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.PushEvent;
begin
  if Assigned(FMPVEvent) then FMPVEvent.PushEvent;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ReceivedEvent(Sender: TObject);
var
  Event: Pmpv_event;
begin
  while True do
  begin
    Event := mpv_wait_event(FMPV_HANDLE^, 0);
    if (Event = NIL) or (Event^.event_id = MPV_EVENT_NONE) then Break;

    case (Event^.event_id) of
      MPV_EVENT_SHUTDOWN:
      begin
        Break;
      end;

      MPV_EVENT_LOG_MESSAGE:
        if Assigned(OnLogMessage) then OnLogMessage(Sender,
          Pmpv_event_log_message(Event^.Data)^.prefix,
          Pmpv_event_log_message(Event^.Data)^.level,
          Pmpv_event_log_message(Event^.Data)^.Text);

      MPV_EVENT_START_FILE:
        if Assigned(OnStartFile) then OnStartFile(Sender);

      MPV_EVENT_FILE_LOADED:
      begin
        if FAutoStart then
          FState := psPlay
        else
          FState := psPause;
        {$IFDEF USETIMER}
        FTimer.Enabled := True;
        {$ENDIF}

        if (FStartAtPosMs > 0) then
        begin
          SetMediaPosInMs(FStartAtPosMs);
          FStartAtPosMs := 0;
        end;

        if Assigned(OnFileLoaded) then OnFileLoaded(Sender);
      end;

      MPV_EVENT_SEEK:
        if Assigned(OnSeek) then OnSeek(Sender, GetMediaPosInMs);

      MPV_EVENT_END_FILE:
      begin
        FState := psEnd;
        {$IFDEF USETIMER}
        FTimer.Enabled := False;
        {$ENDIF}
        if Assigned(OnEndFile) then OnEndFile(Sender, Integer(Event^.data^));
      end;

      MPV_EVENT_VIDEO_RECONFIG:
      begin
        GetTracks;
        if Assigned(OnVideoReconfig) then OnVideoReconfig(Sender);
      end;

      MPV_EVENT_AUDIO_RECONFIG:
      begin
        GetTracks;
        if Assigned(OnAudioReconfig) then OnAudioReconfig(Sender);
      end;

      MPV_EVENT_PROPERTY_CHANGE:
      begin
        if (Pmpv_event_property(Event^.Data)^.Name = 'cache-buffering-state') then //if (Pmpv_event_property(Event^.Data)^.Name = 'paused-for-cache') then
        begin
          if Assigned(OnBuffering) then
            OnBuffering(Sender, mpv_get_property_int64('cache-buffering-state'));
        end;

        {$IFNDEF USETIMER}
        if (Pmpv_event_property(Event^.Data)^.Name = 'playback-time') and (Pmpv_event_property(Event^.Data)^.format = MPV_FORMAT_INT64) then
        begin
          if Assigned(OnTimeChanged) then
            OnTimeChanged(Sender, GetMediaPosInMs);
        end;
        {$ENDIF}
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetRenderMode(Value: TMPVPlayerRenderMode);
begin
  if not FInitialized and (FRenderMode <> Value) then
    FRenderMode := Value;
end;

// -----------------------------------------------------------------------------

{$IFDEF USETIMER}
procedure TMPVPlayer.DoTimer(Sender: TObject);
begin
  if Assigned(OnTimeChanged) then OnTimeChanged(Sender, GetMediaPosInMs);
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure TMPVPlayer.DoResize(Sender: TObject);
begin
  if Assigned(FRenderGL) and not IsPlaying then
    FRenderGL.Render(True);
end;

// -----------------------------------------------------------------------------

procedure RegisterUWCompUnit;
begin
  RegisterComponents('URUWorks Multimedia', [TMPVPlayer]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('MPVPlayer', @RegisterUWCompUnit);
end;

// -----------------------------------------------------------------------------

initialization
  {$I MPVPlayer.lrs}

// -----------------------------------------------------------------------------

end.

