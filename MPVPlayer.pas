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
 *  Copyright (C) 2021-2024 URUWorks, uruworks@gmail.com.
 *
 *  Important for Unix/Linux needs:
 *    Place the following units/functions at the beginning
 *    - cthreads
 *    - XInitThreads
 *}

unit MPVPlayer;

// -----------------------------------------------------------------------------

{$I MPVPlayer.inc}

interface

uses
  Classes, Controls, SysUtils, LazFileUtils, ExtCtrls, Graphics, LCLType,
  LResources, LazarusPackageIntf, libMPV.Client, MPVPlayer.Thread,
  MPVPlayer.RenderGL, OpenGLContext, MPVPlayer.Filters
  {$IFDEF LINUX}, gtk2, gdk2x{$ENDIF}
  {$IFDEF BGLCONTROLS}, BGRAOpenGL{$ENDIF}
  {$IFDEF SDL2}, sdl2lib, libMPV.Render, MPVPlayer.RenderSDL{$ENDIF};

// -----------------------------------------------------------------------------

type

  { TMPVPlayer Types }

  TMPVPlayerRenderMode        = (rmEmbedding, rmOpenGL{$IFDEF SDL2}, rmSDL2{$ENDIF});
  TMPVPlayerRendeFailAction   = (rfSwitchToEmbedding, rfNone);
  TMPVPlayerTrackType         = (ttVideo, ttAudio, ttSubtitle, ttUnknown);
  TMPVPlayerVideoAspectRatio  = (arDefault, ar4_3, ar16_9, ar185_1, ar235_1);
  TMPVPlayerLogLevel          = (llNo, llFatal, llError, llWarn, llInfo, llStatus, llV, llDebug, llTrace);
  TMPVPlayerScreenshotMode    = (smSubtitles, smVideo, smWindow);
  TMPVPlayerNotifyEvent       = procedure(ASender: TObject; AParam: Integer) of object;
  TMPVPlayerLogEvent          = procedure(ASender: TObject; APrefix, ALevel, AText: String) of object;
  TMPVPlayerGetReplyEvent     = procedure(ASender: TObject; reply_userdata: Integer; error_code: mpv_error; event_property: Pmpv_event_property) of object;
  TMPVPlayerSetReplyEvent     = procedure(ASender: TObject; reply_userdata: Integer; error_code: mpv_error) of object;
  TMPVPlayerCommandReplyEvent = procedure(ASender: TObject; reply_userdata: Integer; error_code: mpv_error; event_command: Pmpv_event_command) of object;

  TMPVPlayerTrackInfo = record
    Kind     : TMPVPlayerTrackType;
    ID       : Integer;
    Codec    : String;
    Decoder  : String;
    Channels : String;
    Title    : String;
    Lang     : String;
    Selected : Boolean;
  end;

  TMPVPlayerTrackList = array of TMPVPlayerTrackInfo;

  { TMPVPlayer }

  TMPVPlayer = class(TCustomPanel)
  private
    FMPV_HANDLE     : Pmpv_handle;
    FError          : mpv_error;
    FVersion        : DWord;
    FGL             : TOpenGLControl;
    FInitialized    : Boolean;
    FStartOptions   : TStringList;
    FLogLevel       : TMPVPlayerLogLevel;
    FMPVEvent       : TMPVPlayerThreadEvent;
    FTrackList      : TMPVPlayerTrackList;
    FAspectRatio    : TMPVPlayerVideoAspectRatio;
    FAutoStart      : Boolean;
    FAutoLoadSub    : Boolean;
    FKeepAspect     : Boolean;
    FNoAudioDisplay : Boolean;
    FSMPTEMode      : Boolean;
    FRenderFail     : TMPVPlayerRendeFailAction;
    FStartAtPosMs   : Integer;
    FPausePosMs     : Integer;
    FFileName       : String;
    FMPVFileName    : String;
    FYTDLPFileName  : String;
    {$IFDEF USETIMER}
    FTimer          : TTimer;
    FLastPos        : Integer;
    {$ENDIF}

    FRenderMode     : TMPVPlayerRenderMode;
    FRenderGL       : TMPVPlayerRenderGL;

    {$IFDEF SDL2}
    FRenderSDL      : TMPVPlayerRenderSDL;
    {$ENDIF}

    FShowText       : String;
    FText           : String;
    FTextNode       : mpv_node;
    FTextNodeList   : mpv_node_list;
    FTextNodeKeys   : array of PChar;
    FTextNodeValues : array of mpv_node;

    FOnStartFile: TNotifyEvent;                        // Notification before playback start of a file (before the file is loaded).
    FOnEndFile: TMPVPlayerNotifyEvent;                 // Notification after playback end (after the file was unloaded), AParam is mpv_end_file_reason.
    FOnFileLoaded: TNotifyEvent;                       // Notification when the file has been loaded (headers were read etc.)
    FOnVideoReconfig: TNotifyEvent;                    // Happens after video changed in some way.
    FOnAudioReconfig: TNotifyEvent;                    // Similar to VIDEO_RECONFIG.
    FOnSeek: TMPVPlayerNotifyEvent;                    // Happens when a seek was initiated.
    FOnPlaybackRestart: TNotifyEvent;                  // Usually happens on start of playback and after seeking.
    FOnPlay: TNotifyEvent;                             // Play by user
    FOnStop: TNotifyEvent;                             // Stop by user
    FOnPause: TNotifyEvent;                            // Pause by user
    FOnTimeChanged: TMPVPlayerNotifyEvent;             // Notify playback time, AParam is current position.
    FOnBuffering: TMPVPlayerNotifyEvent;               // Whether playback is paused because of waiting for the cache.
    FOnLogMessage: TMPVPlayerLogEvent;                 // Receives messages enabled with mpv_request_log_messages().
    FOnGetReplyEvent: TMPVPlayerGetReplyEvent;         // Result data of mpv_get_property_* async.
    FOnSetReplyEvent: TMPVPlayerSetReplyEvent;         // Result data of mpv_set_property_* async.
    FOnCommandReplyEvent: TMPVPlayerCommandReplyEvent; // Result data of the command async.

    {$IFDEF BGLCONTROLS}
    FOnDrawEvent: TMPVPlayerDrawEvent;
    {$ENDIF}

    function Initialize: Boolean;
    procedure UnInitialize;

    function InitializeRenderGL: Boolean;
    procedure UnInitializeRenderGL;

    procedure PushEvent;
    procedure ReceivedEvent(Sender: TObject);

    {$IFDEF SDL2}
    function InitializeRenderSDL: Boolean;
    procedure UnInitializeRenderSDL;
    {$ENDIF}

    function SetWID: Boolean;
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

    function mpv_command_(args: array of String; const reply_userdata: Integer = 0): mpv_error; // if reply_userdata > 0 commands are executed asynchronously
    function mpv_command_node_(ANode: mpv_node; const reply_userdata: Integer = 0): mpv_error;
    procedure mpv_abort_async_command_(const reply_userdata: Integer);
    function mpv_set_option_string_(const AValue: String): Integer;
    function mpv_get_property_string_(const APropertyName: String; const reply_userdata: Integer = 0): String;
    procedure mpv_set_property_string_(const APropertyName: String; const AValue: String; const reply_userdata: Integer = 0);
    function mpv_get_property_boolean(const APropertyName: String; const reply_userdata: Integer = 0): Boolean;
    procedure mpv_set_property_boolean(const APropertyName: String; const AValue: Boolean; const reply_userdata: Integer = 0);
    function mpv_get_property_double(const APropertyName: String; const reply_userdata: Integer = 0): Double;
    procedure mpv_set_property_double(const APropertyName: String; const AValue: Double; const reply_userdata: Integer = 0);
    function mpv_get_property_int64(const APropertyName: String; const reply_userdata: Integer = 0): Int64;
    procedure mpv_set_property_int64(const APropertyName: String; const AValue: Int64; const reply_userdata: Integer = 0);
    procedure mpv_set_pause(const Value: Boolean);

    function GetErrorString: String;
    function GetVersionString: String;
    function GetPlayerHandle: Pmpv_handle;

    procedure Play(const AFileName: String; const AStartAtPositionMs: Integer = 0); overload;
    procedure Play(const FromMs: Integer); overload;
    procedure Close;
    procedure Loop(const AStartTimeMs, BFinalTimeMs: Integer; const ALoopCount: Integer = -1);
    procedure Pause;
    procedure Resume(const ForcePlay: Boolean = False);
    procedure Stop;
    function IsMediaLoaded: Boolean;
    function IsPlaying: Boolean;
    function IsPaused: Boolean;
    function GetMediaLenInMs: Integer;
    function GetMediaPosInMs: Integer;
    procedure SetMediaPosInMs(const AValue: Integer);
    procedure SeekInMs(const MSecs: Integer; const SeekAbsolute: Boolean = True);
    procedure NextFrame(const AStep: Integer = 1);
    procedure PreviousFrame(const AStep: Integer = 1);
    procedure SetPlaybackRate(const AValue: Byte);
    function GetAudioVolume: Byte;
    procedure SetAudioVolume(const AValue: Byte);
    function GetAudioMute: Boolean;
    procedure SetAudioMute(const AValue: Boolean);
    procedure SetTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer); overload;
    procedure SetTrack(const Index: Integer); overload;
    procedure GetTracks;
    procedure LoadTrack(const TrackType: TMPVPlayerTrackType; const AFileName: String);
    procedure RemoveTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
    procedure ReloadTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
    procedure ShowOverlayText(const AText: String);
    procedure ShowText(const AText: String; const ADuration: Integer = 1000; const ATags: String = '{\an7}');
    procedure SetTextColor(const AValue: String);
    procedure SetTextHAlign(const AValue: String);
    procedure SetTextVAlign(const AValue: String);
    procedure SetTextSize(const AValue: Int64);
    procedure SetTextFont(const AValue: String);
    procedure SetSubtitleColor(const AValue: String);
    procedure SetSubtitleSize(const AValue: Int64);
    procedure SetSubtitleFont(const AValue: String);

    function GetVideoWidth: Integer;
    function GetVideoHeight: Integer;
    function GetVideoTotalFrames: Integer;
    function GetVideoFPS: Double;

    procedure ScreenshotToFile(const AFileName: String; const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo); // name with full path, extension defines the format (file.png)
    procedure ScreenshotToClipboard(const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);

    procedure AddOption(const AValue: String);
    procedure RemoveOption(const AValue: String);

    procedure SetVideoAspectRatio(const AValue: TMPVPlayerVideoAspectRatio);
    procedure CycleVideoAspectRatio;

    procedure SetVideoFilters(const AVideoFilters: TMPVPlayerVideoFilters);
    procedure ClearVideoFilters;

    procedure SetAudioFilters(const AAudioFilters: TMPVPlayerAudioFilters);
    procedure ClearAudioFilters;

    property mpv_handle: Pmpv_handle read FMPV_HANDLE;
    property Error: mpv_error read FError;
    property ErrorString: String read GetErrorString;
    property Version: DWord read FVersion;
    property VersionString: String read GetVersionString;
    property Initialized: Boolean read FInitialized;
    property StartOptions: TStringList read FStartOptions;
    property TrackList: TMPVPlayerTrackList read FTrackList;
    property FileName: String read FFileName;
    property MPVFileName: String read FMPVFileName write FMPVFileName;
    property YTDLPFileName: String read FYTDLPFileName write FYTDLPFileName;
    property SMPTEMode: Boolean read FSMPTEMode write FSMPTEMode;
    {$IFDEF USETIMER}
    property Timer: TTimer read FTimer;
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
    property Caption;
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
    property NoAudioDisplay: Boolean read FNoAudioDisplay write FNoAudioDisplay;
    property RendererMode: TMPVPlayerRenderMode read FRenderMode write SetRenderMode;
    property RenderFailAction : TMPVPlayerRendeFailAction read FRenderFail write FRenderFail;
    property LogLevel: TMPVPlayerLogLevel read FLogLevel write FLogLevel;

    property OnStartFile: TNotifyEvent read FOnStartFile write FOnStartFile;
    property OnEndFile: TMPVPlayerNotifyEvent read FOnEndFile write FOnEndFile;
    property OnFileLoaded: TNotifyEvent read FOnFileLoaded write FOnFileLoaded;
    property OnVideoReconfig: TNotifyEvent read FOnVideoReconfig write FOnVideoReconfig;
    property OnAudioReconfig: TNotifyEvent read FOnAudioReconfig write FOnAudioReconfig;
    property OnSeek: TMPVPlayerNotifyEvent read FOnSeek write FOnSeek;
    property OnPlaybackRestart: TNotifyEvent read FOnPlaybackRestart write FOnPlaybackRestart;

    property OnPlay: TNotifyEvent read FOnPlay  write FOnPlay;
    property OnStop: TNotifyEvent read FOnStop  write FOnStop;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    property OnTimeChanged: TMPVPlayerNotifyEvent read FOnTimeChanged write FOnTimeChanged;
    property OnBuffering: TMPVPlayerNotifyEvent read FOnBuffering write FOnBuffering;
    property OnLogMessage: TMPVPlayerLogEvent read FOnLogMessage write FOnLogMessage;
    property OnGetReplyEvent: TMPVPlayerGetReplyEvent read FOnGetReplyEvent write FOnGetReplyEvent;
    property OnSetReplyEvent: TMPVPlayerSetReplyEvent read FOnSetReplyEvent write FOnSetReplyEvent;
    property OnCommandReplyEvent: TMPVPlayerCommandReplyEvent read FOnCommandReplyEvent write FOnCommandReplyEvent;

    {$IFDEF BGLCONTROLS}
    property OnDraw: TMPVPlayerDrawEvent read FOnDrawEvent write FOnDrawEvent;
    {$ENDIF}
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

uses
  Clipbrd;

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

function MSToTimeStamp(const Time: Integer): String; // 'hh:mm:ss.zzz'
var
  Hour, Min, Secs, MSecs,
  h, m, x: Integer;
begin
  Hour  := Trunc(Time / 3600000);
  h     := Time - (Hour * 3600000);
  Min   := Trunc(h / 60000);
  m     := Min * 60000;
  x     := h - m;
  Secs  := Trunc(x / 1000);
  MSecs := Trunc(x - (Secs*1000));

  Result := Format('%.2d:%.2d:%.2d.%.3d', [Hour, Min, Secs, MSecs]);
end;

// -----------------------------------------------------------------------------

function FramesToMS(const Frames, FPS: Single): Integer;
begin
  if FPS > 0 then
    Result := Round((Frames / FPS) * 1000.0)
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.IsLibMPVAvailable: Boolean;
begin
  FError := IsLibMPV_Installed(FMPVFileName);
  Result := (FError = MPV_ERROR_SUCCESS);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_command_(args: array of String; const reply_userdata: Integer = 0): mpv_error;
var
  pArgs: array of PChar;
  i: Integer;
begin
  Result := MPV_ERROR_INVALID_PARAMETER;

  if High(Args) < 0 then
    Exit
  else if FInitialized and (mpv_command <> NIL) and (FMPV_HANDLE <> NIL) then
  begin
    SetLength(pArgs, (Length(Args)+1));

    for i := 0 to High(Args) do
      pArgs[i] := PChar(Args[i]);

    pArgs[Length(Args)] := NIL;

    if reply_userdata > 0 then
      FError := mpv_command_async(FMPV_HANDLE^, reply_userdata, PPChar(@pArgs[0]))
    else
      FError := mpv_command(FMPV_HANDLE^, PPChar(@pArgs[0]));

    SetLength(pArgs, 0);
  end
  else
    FError := MPV_ERROR_UNINITIALIZED;

  Result := FError;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_command_node_(ANode: mpv_node; const reply_userdata: Integer = 0): mpv_error;
var
  Res: mpv_node;
begin
  FError := MPV_ERROR_UNINITIALIZED;

  if FInitialized and (FMPV_HANDLE <> NIL) then
  begin
    if reply_userdata > 0 then
      FError := mpv_command_node_async(FMPV_HANDLE^, reply_userdata, ANode)
    else
      FError := mpv_command_node(FMPV_HANDLE^, ANode, Res);
  end;

  Result := FError;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_abort_async_command_(const reply_userdata: Integer);
begin
  if FInitialized and (FMPV_HANDLE <> NIL) then
    mpv_abort_async_command(FMPV_HANDLE^, reply_userdata);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_set_option_string_(const AValue: String): Integer;
var
  s1, s2: String;
  i: Integer;
begin
  FError := MPV_ERROR_OPTION_ERROR;
  if not Assigned(mpv_set_option_string) or (FMPV_HANDLE = NIL) or AValue.IsEmpty then Exit(FError);

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
  Result := FError;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_string_(const APropertyName: String; const reply_userdata: Integer = 0): String;
begin
  if FInitialized and (FMPV_HANDLE <> NIL) then
  begin
    if reply_userdata > 0 then
      FError := mpv_get_property_async(FMPV_HANDLE^, reply_userdata, PChar(APropertyName), MPV_FORMAT_STRING)
    else
      FError := mpv_get_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_STRING, @Result);
  end
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_string_(const APropertyName: String; const AValue: String; const reply_userdata: Integer = 0);
begin
  if not FInitialized or (FMPV_HANDLE = NIL) then Exit;

  if reply_userdata > 0 then
    FError := mpv_set_property_async(FMPV_HANDLE^, reply_userdata, PChar(APropertyName), MPV_FORMAT_STRING, PChar(@AValue))
  else
    FError := mpv_set_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_STRING, PChar(@AValue));
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_boolean(const APropertyName: String; const reply_userdata: Integer = 0): Boolean;
var
  p: Integer;
begin
  Result := False;
  if not FInitialized or (FMPV_HANDLE = NIL) then Exit;

  if reply_userdata > 0 then
  begin
    FError := mpv_get_property_async(FMPV_HANDLE^, reply_userdata, PChar(APropertyName), MPV_FORMAT_FLAG);
    Result := True;
  end
  else
  begin
    FError := mpv_get_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_FLAG, @p);
    Result := Boolean(p);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_boolean(const APropertyName: String; const AValue: Boolean; const reply_userdata: Integer = 0);
var
  p: Integer;
begin
  if not FInitialized or (FMPV_HANDLE = NIL) then Exit;

  if AValue then
    p := 1
  else
    p := 0;

  if reply_userdata > 0 then
    FError := mpv_set_property_async(FMPV_HANDLE^, reply_userdata, PChar(APropertyName), MPV_FORMAT_FLAG, @p)
  else
    FError := mpv_set_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_FLAG, @p);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_double(const APropertyName: String; const reply_userdata: Integer = 0): Double;
begin
  if FInitialized and (FMPV_HANDLE <> NIL) then
  begin
    if reply_userdata > 0 then
      FError := mpv_get_property_async(FMPV_HANDLE^, reply_userdata, PChar(APropertyName), MPV_FORMAT_DOUBLE)
    else
      FError := mpv_get_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_DOUBLE, @Result);
  end
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_double(const APropertyName: String; const AValue: Double; const reply_userdata: Integer = 0);
begin
  if FInitialized and (FMPV_HANDLE <> NIL) then
  begin
    if reply_userdata > 0 then
      FError := mpv_set_property_async(FMPV_HANDLE^, reply_userdata, PChar(APropertyName), MPV_FORMAT_DOUBLE, @AValue)
    else
      FError := mpv_set_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_DOUBLE, @AValue);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_int64(const APropertyName: String; const reply_userdata: Integer = 0): Int64;
begin
  if FInitialized and (FMPV_HANDLE <> NIL) then
  begin
    if reply_userdata > 0 then
      FError := mpv_get_property_async(FMPV_HANDLE^, reply_userdata, PChar(APropertyName), MPV_FORMAT_INT64)
    else
      FError := mpv_get_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_INT64, @Result)
  end
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_int64(const APropertyName: String; const AValue: Int64; const reply_userdata: Integer = 0);
begin
  if FInitialized and (FMPV_HANDLE <> NIL) then
  begin
    if reply_userdata > 0 then
      FError := mpv_set_property_async(FMPV_HANDLE^, reply_userdata, PChar(APropertyName), MPV_FORMAT_INT64, @AValue)
    else
      FError := mpv_set_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_INT64, @AValue);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_pause(const Value: Boolean);
begin
  mpv_set_property_boolean('pause', Value);
  case Value of
    True  : if Assigned(FOnPause) then FOnPause(Self);
    False : if Assigned(FOnPlay) then FOnPlay(Self);
  end;
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
  FGL             := NIL;
  FMPV_HANDLE     := NIL;
  FVersion        := 0;
  FError          := 0;
  FInitialized    := False;
  FMPVEvent       := NIL;
  FLogLevel       := llStatus;
  FAutoStart      := True;
  FAutoLoadSub    := False;
  FKeepAspect     := True;
  FNoAudioDisplay := False;
  FSMPTEMode      := False;
  FRenderFail     := rfNone;
  FPausePosMs     := -1;
  FFileName       := '';
  {$IFDEF LINUX}
  FMPVFileName    := '';
  {$ELSE}
  FMPVFileName    := LIBMPV_DLL_NAME;
  {$ENDIF}
  FYTDLPFileName  := '';
  FStartOptions   := TStringList.Create;
  SetLength(FTrackList, 0);

  FAspectRatio    := arDefault;

  {$IFDEF WINDOWS}
  FRenderMode     := rmEmbedding;
  {$ELSE}
  FRenderMode     := rmOpenGL;
  {$ENDIF}
  FRenderGL       := NIL;

  {$IFDEF USETIMER}
  FTimer          := TTimer.Create(NIL);
  FTimer.Enabled  := False;
  FTimer.Interval := 140;
  FTimer.OnTimer  := @DoTimer;
  FLastPos        := -1;
  {$ENDIF}

  with FStartOptions do
  begin
    Sorted     := True;
    Duplicates := dupIgnore;

    Add('osc=no');                  // default: yes.
//    {$IFDEF WINDOWS}
    Add('hwdec=no');                // fix some windows crash
//    {$ELSE}
//    Add('hwdec=auto');              // enable best hw decoder.
//    {$ENDIF}
//    Add('osd-duration=5000');       // default: 1000.
    Add('keep-open=always');        // don't auto close video.
    Add('vd-lavc-dr=no');           // fix possibles deadlock issues with OpenGL.
    Add('hr-seek=yes');             // use precise seeks whenever possible.
    Add('hr-seek-framedrop=no');    // default: yes.
//    Add('osd-scale-by-window=no');  // scale the OSD with the window size. default: yes.
    Add('ytdl=yes');                // use YouTube downloader.
  end;

  {$IFDEF WINDOWS}
  FError := Load_libMPV(FMPVFileName);
  {$ENDIF}
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

  {$IFDEF WINDOWS}
  Free_libMPV;
  {$ENDIF}
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.Initialize: Boolean;
var
  sl : TStringList;
  i  : Integer;
begin
  if FInitialized then Exit(True);

  FInitialized := False;
  Result := False;

  {$IFDEF WINDOWS}
  if not IsLibMPV_Loaded then Exit;
  {$ELSE}
  FError := Load_libMPV(FMPVFileName);
  if FError = MPV_ERROR_UNINITIALIZED then Exit;
  {$ENDIF}

  FMPV_HANDLE := mpv_create();
  if not Assigned(FMPV_HANDLE) then
  begin
    FError := MPV_ERROR_UNSUPPORTED;
    Free_libMPV;
    Exit;
  end;

  // Get version lib
  if Assigned(mpv_client_api_version) then
    FVersion := mpv_client_api_version();

  sl := TStringList.Create;
  try
    sl.Assign(FStartOptions);

    if not FAutoStart then
      sl.Add('pause'); // Start the player in paused state

    if not FAutoLoadSub then
      sl.Add('sub=no'); // don't load subtitles

    if not FKeepAspect then
      sl.Add('keepaspect=no'); // always stretch the video to window size

    if FNoAudioDisplay then
      sl.Add('audio-display=no'); // no display cover art

    for i := 0 to sl.Count-1 do
      mpv_set_option_string_(sl[i]);
  finally
    sl.Free;
  end;

  if not FYTDLPFileName.IsEmpty then
    mpv_set_option_string(FMPV_HANDLE^, PChar('script-opts'), PChar('ytdl_hook-ytdl_path='+FYTDLPFileName));

  SetVideoAspectRatio(FAspectRatio);

  // Set our window handle
  if not SetWID then
  begin
    UnInitialize;
    Exit;
  end;

  {$IFNDEF USETIMER}
  mpv_observe_property(FMPV_HANDLE^, 0, 'playback-time', MPV_FORMAT_INT64);
  {$ENDIF}
  mpv_observe_property(FMPV_HANDLE^, 0, 'eof-reached', MPV_FORMAT_FLAG);
  //mpv_observe_property(FMPV_HANDLE^, 0, 'paused-for-cache', MPV_FORMAT_INT64);
  mpv_observe_property(FMPV_HANDLE^, 0, 'cache-buffering-state', MPV_FORMAT_INT64);

  FError := mpv_initialize(FMPV_HANDLE^);
  if FError <> MPV_ERROR_SUCCESS then
  begin
    UnInitialize;
    Exit;
  end;

  FError := mpv_request_log_messages(FMPV_HANDLE^, PChar(LogLevelToString));

  // Show text string
  FShowText := '';
  // Node text overlay cfg
  FText := '';
  SetLength(FTextNodeKeys, 4);
  SetLength(FTextNodeValues, 4);
  FTextNodeKeys[0]             := 'name';
  FTextNodeValues[0].format    := MPV_FORMAT_STRING;
  FTextNodeValues[0].u._string := 'osd-overlay';
  FTextNodeKeys[1]             := 'id';
  FTextNodeValues[1].format    := MPV_FORMAT_INT64;
  FTextNodeValues[1].u.int64_  := 1;
  FTextNodeKeys[2]             := 'format';
  FTextNodeValues[2].format    := MPV_FORMAT_STRING;
  FTextNodeKeys[3]             := 'data';
  FTextNodeValues[3].format    := MPV_FORMAT_STRING;
  FTextNodeValues[3].u._string := NIL;
  FTextNodeList.num            := 4;
  FTextNodeList.keys           := @FTextNodeKeys[0];
  FTextNodeList.values         := @FTextNodeValues[0];
  FTextNode.format             := MPV_FORMAT_NODE_MAP;
  FTextNode.u.list             := @FTextNodeList;

  FMPVEvent := TMPVPlayerThreadEvent.Create;
  FMPVEvent.OnEvent := @ReceivedEvent;
  mpv_set_wakeup_callback(FMPV_HANDLE^, @LIBMPV_EVENT, Self);

  if FRenderMode = rmOpenGL then
  begin
    if not InitializeRenderGL then
    begin
      UnInitializeRenderGL;

      if FRenderFail = rfNone then
      begin
        FError := MPV_ERROR_VO_INIT_FAILED;
        Exit;
      end
      else
        FRenderMode := rmEmbedding;
    end;
  end
  {$IFDEF SDL2}
  else if FRenderMode = rmSDL2 then
  begin
    if not InitializeRenderSDL then
    begin
      UnInitializeRenderSDL;

      if FRenderFail = rfNone then
      begin
        FError := MPV_ERROR_VO_INIT_FAILED;
        Exit;
      end
      else
        FRenderMode := rmEmbedding;
    end;
  end;
  {$ENDIF};

  FPausePosMs := -1;
  FInitialized := True;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.UnInitialize;
begin
  if not FInitialized then Exit;

  {$IFDEF USETIMER}
  FTimer.Enabled := False;
  FLastPos       := -1;
  {$ENDIF}

  if Assigned(mpv_unobserve_property) and Assigned(FMPV_HANDLE) then
    mpv_unobserve_property(FMPV_HANDLE^, 0);

  if Assigned(mpv_set_wakeup_callback) and Assigned(FMPV_HANDLE) then
    mpv_set_wakeup_callback(FMPV_HANDLE^, NIL, NIL);

  FShowText := '';
  FText := '';
  SetLength(FTextNodeKeys, 0);
  SetLength(FTextNodeValues, 0);

  if Assigned(FMPVEvent) then
  begin
    FMPVEvent.OnEvent := NIL;
    FMPVEvent.Free;
    FMPVEvent := NIL;
  end;

  if FRenderMode = rmOpenGL then
    UnInitializeRenderGL
  {$IFDEF SDL2}
  else if FRenderMode = rmSDL2 then
    UnInitializeRenderSDL
  {$ENDIF};

  if Assigned(mpv_terminate_destroy) and Assigned(FMPV_HANDLE) then
    mpv_terminate_destroy(FMPV_HANDLE^);

  FMPV_HANDLE := NIL;
  SetLength(FTrackList, 0);
  FFileName := '';
  FPausePosMs := -1;
  FInitialized := False;
  {$IFDEF DARWIN}
  Free_libMPV;
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.InitializeRenderGL: Boolean;
begin
  FGL         := TOpenGLControl.Create(Self);
  FGL.Parent  := Self;
  FGL.Align   := alClient;
  FGL.OnClick := OnClick;
  FGL.OnMouseWheelUp   := OnMouseWheelUp;
  FGL.OnMouseWheelDown := OnMouseWheelDown;
  FGL.OnResize := @DoResize; // force to draw opengl context when paused

  FRenderGL := TMPVPlayerRenderGL.Create(FGL, FMPV_HANDLE {$IFDEF BGLCONTROLS}, FOnDrawEvent{$ENDIF});
  Result := FRenderGL.Active;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.UnInitializeRenderGL;
begin
  if Assigned(FRenderGL) then
  begin
    FRenderGL.Free;
    FRenderGL := NIL;
  end;

  if Assigned(FGL) then
  begin
    FGL.Free;
    FGL := NIL;
    Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

{$IFDEF SDL2}
function TMPVPlayer.InitializeRenderSDL: Boolean;
begin
  FRenderSDL := TMPVPlayerRenderSDL.Create(Handle, FMPV_HANDLE);
  Result := FRenderSDL.Active;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.UnInitializeRenderSDL;
begin
  if Assigned(FRenderSDL) then
  begin
    FRenderSDL.Free;
    FRenderSDL := NIL;
  end;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

function TMPVPlayer.SetWID: Boolean;
var
  pHwnd: {$IFDEF WID_AS_STRING}String{$ELSE}PtrInt{$ENDIF};
begin
  if not Assigned(mpv_set_option) or not Assigned(FMPV_HANDLE) then Exit(False);

  {$IFDEF LINUX}
  pHwnd := {$IFDEF WID_AS_STRING}IntToStr({$ENDIF}GDK_WINDOW_XWINDOW(PGtkWidget(Self.Handle)^.window){$IFDEF WID_AS_STRING}){$ENDIF};
  {$ELSE}
  pHwnd := {$IFDEF WID_AS_STRING}IntToStr({$ENDIF}Handle{$IFDEF WID_AS_STRING}){$ENDIF};
  {$ENDIF}

  {$IFDEF WID_AS_STRING}
  FError := mpv_set_option_string_('wid=' + pHwnd);
  {$ELSE}
  FError := mpv_set_option(FMPV_HANDLE^, 'wid', MPV_FORMAT_INT64, @pHwnd);
  {$ENDIF};

  Result := (FError = MPV_ERROR_SUCCESS);
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
  if Initialize then
  begin
    FStartAtPosMs := AStartAtPositionMs;
    FFileName := AFileName;
    mpv_command_(['loadfile', FFileName]);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Play(const FromMs: Integer);
begin
  SeekInMs(FromMs);
  Resume(True);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Close;
begin
  mpv_command_(['quit']);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Loop(const AStartTimeMs, BFinalTimeMs: Integer; const ALoopCount: Integer = -1);
begin
  if (AStartTimeMs = 0) and (BFinalTimeMs = 0) then
  begin
    mpv_set_option_string_('ab-loop-a=no');
    mpv_set_option_string_('ab-loop-b=no');
  end
  else
  begin
    SeekInMs(AStartTimeMs);

    // string format hh:mm:ss.zzz
    mpv_set_option_string_('ab-loop-a=' + MSToTimeStamp(AStartTimeMs));
    mpv_set_option_string_('ab-loop-b=' + MSToTimeStamp(BFinalTimeMs));

    if ALoopCount > 0 then
      mpv_set_option_string_('ab-loop-count=' + ALoopCount.ToString)
    else
      mpv_set_option_string_('ab-loop-count=0');

    if IsPaused then Resume;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Pause;
begin
  Loop(0, 0);

  if IsPlaying then
    mpv_set_pause(True)
  else
    Resume(True);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Resume(const ForcePlay: Boolean = False);
begin
  if ForcePlay or IsPaused then
  begin
    if GetMediaPosInMs = GetMediaLenInMs then
      SetMediaPosInMs(0);

    FPausePosMs := -1;
    mpv_set_pause(False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Stop;
begin
  Loop(0, 0);

  if not IsPaused then
    mpv_set_pause(True);

  FPausePosMs := -1;
  SetMediaPosInMs(0);
  if Assigned(FOnStop) then FOnStop(Self);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.IsMediaLoaded: Boolean;
begin
  Result := GetMediaLenInMs > 0;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.IsPlaying: Boolean;
begin
  Result := not IsPaused;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.IsPaused: Boolean;
begin
  Result := (mpv_get_property_boolean('pause') = True);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetMediaLenInMs: Integer;
begin
  Result := Round(mpv_get_property_double('duration') * 1000.0);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetMediaPosInMs: Integer;
var
  i: Double;
begin
  if FPausePosMs > -1 then
    Exit(FPausePosMs);

  i := mpv_get_property_double('time-pos') * 1000.0;
  if FSMPTEMode then
    Result := Round(i / 1.001)
  else
    Result := Round(i);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetMediaPosInMs(const AValue: Integer);
var
  i: Double;
begin
  if IsPaused and (AValue <= GetMediaLenInMs) then
    FPausePosMs := AValue;

  i := AValue / 1000.0;
  if FSMPTEMode then
    mpv_set_property_double('time-pos', i * 1.001)
  else
    mpv_set_property_double('time-pos', i);
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

procedure TMPVPlayer.NextFrame(const AStep: Integer = 1);
var
  f: Double;
begin
  if AStep > 1 then
  begin
    f := GetVideoFPS;
    if f > 0 then
      SetMediaPosInMs(GetMediaPosInMs + FramesToMS(AStep, f));
  end
  else
  begin
    FPausePosMs := -1;
    if (mpv_command_(['frame-step']) = MPV_ERROR_SUCCESS) and not IsPaused then
      if Assigned(FOnPause) then FOnPause(Self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.PreviousFrame(const AStep: Integer = 1);
var
  f: Double;
begin
  if AStep > 1 then
  begin
    f := GetVideoFPS;
    if f > 0 then
      SetMediaPosInMs(GetMediaPosInMs - FramesToMS(AStep, f));
  end
  else
  begin
    FPausePosMs := -1;
    if (mpv_command_(['frame-back-step']) = MPV_ERROR_SUCCESS) and IsPaused then
      if Assigned(FOnPause) then FOnPause(Self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetPlaybackRate(const AValue: Byte);
begin
  mpv_set_property_double('speed', AValue / 100.0);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetAudioVolume: Byte;
begin
  Result := Trunc(mpv_get_property_int64('volume'));
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetAudioVolume(const AValue: Byte);
begin
  mpv_set_property_int64('volume', AValue);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetAudioMute: Boolean;
begin
  Result := mpv_get_property_boolean('mute');
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetAudioMute(const AValue: Boolean);
begin
  mpv_set_property_boolean('mute', AValue);
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
  Node: mpv_node;
  Values, Detail: Pmpv_node;
  Keys: PPChar;
  Key, Value: String;
begin
  if not Assigned(mpv_get_property) or not Assigned(FMPV_HANDLE) then Exit;

  FError := mpv_get_property(FMPV_HANDLE^, 'track-list', MPV_FORMAT_NODE, @Node);
  if FError = MPV_ERROR_SUCCESS then
  begin
    try
      Values := Node.u.list^.values;
      SetLength(FTrackList, Node.u.list^.num);

      if Values <> NIL then
      begin
        for i := 0 to Node.u.list^.num-1 do
        begin
          Keys := Values^.u.list^.keys;
          FillByte(FTrackList[i], SizeOf(TMPVPlayerTrackInfo), 0);

          if Values <> NIL then
          begin
            Detail := Values^.u.list^.values;

            for j := 0 to Values^.u.list^.num-1 do
              if Keys <> NIL then
              begin
                Key := StrPas(Keys^);

                if Detail <> NIL then
                begin
                  if Key = 'id' then
                    FTrackList[i].Id := Detail^.u.int64_
                  else if Key = 'type' then
                  begin
                    Value := StrPas(Detail^.u._string);
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
                    FTrackList[i].title := StrPas(Detail^.u._string)
                  else if Key = 'lang' then
                    FTrackList[i].Lang := StrPas(Detail^.u._string)
                  else if Key = 'codec' then
                    FTrackList[i].Codec := StrPas(Detail^.u._string)
                  else if Key = 'decoder-desc' then
                    FTrackList[i].Decoder := StrPas(Detail^.u._string)
                  else if Key = 'demux-channels' then
                    FTrackList[i].Channels := StrPas(Detail^.u._string)
                  else if Key = 'selected' then
                    FTrackList[i].Selected := Detail^.u.flag = 1;

                  Inc(Detail);
                end;
                Inc(Keys);
              end;
            Inc(Values);
          end;
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
    mpv_command_([s, ID.ToString])
  else
    mpv_command_([s]);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ReloadTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
var
  s: String;
begin
  case TrackType of
    ttAudio    : s := 'audio-reload';
    ttVideo    : s := 'video-reload';
    ttSubtitle : s := 'sub-reload';
  else
    Exit;
  end;

  if ID > -1 then
    mpv_command_([s, ID.ToString])
  else
    mpv_command_([s]);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ShowOverlayText(const AText: String);
begin
  if not FInitialized then
    Exit
  else if (AText <> FText) then
  begin
    FText := AText;
    if AText.IsEmpty then
      FTextNodeValues[2].u._string := 'none'
    else
      FTextNodeValues[2].u._string := 'ass-events';

    //FTextNodeValues[3].u._string := PChar(AText);
    FTextNodeValues[3].u._string := PChar('{\fscx75\fscy75\shad0}'+AText);
    mpv_command_node_(FTextNode);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ShowText(const AText: String; const ADuration: Integer = 1000; const ATags: String = '{\an7}');
begin
  if (AText <> FShowText) then
  begin
    FShowText := AText;
    mpv_command_(['expand-properties', 'show-text', '${osd-ass-cc/0}' + ATags + AText, ADuration.ToString]);
  end;
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

procedure TMPVPlayer.SetTextFont(const AValue: String);
begin
  mpv_set_option_string_('osd-font='+AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetSubtitleColor(const AValue: String);
begin
  mpv_set_option_string_('sub-color='+AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetSubtitleSize(const AValue: Int64);
begin
  mpv_set_property_int64('sub-font-size', AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetSubtitleFont(const AValue: String);
begin
  mpv_set_option_string_('sub-font='+AValue);
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

procedure TMPVPlayer.ScreenshotToFile(const AFileName: String; const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);
var
  ssm: String;
begin
  case AScreenshotMode of
    smSubtitles : ssm := 'subtitles'; // Video and Subtitles
    smWindow    : ssm := 'window'; // Video and all texts
  else
    ssm := 'video';  // Video only
  end;

  mpv_command_(['screenshot-to-file', AFileName, ssm]);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ScreenshotToClipboard(const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);
var
  s : String;
  p : TPicture;
begin
  s := ChangeFileExt(GetTempFileName, '.png');
  ScreenshotToFile(s, AScreenshotMode);
  if FileExists(s) then
  begin
    p := TPicture.Create;
    try
      p.LoadFromFile(s);
      Clipboard.Assign(p.Bitmap);
    finally
      p.Free;
    end;
    DeleteFile(s);
  end;
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
        UnInitialize;
        Break;
      end;

      MPV_EVENT_LOG_MESSAGE:
        if Assigned(OnLogMessage) then OnLogMessage(Sender,
          Pmpv_event_log_message(Event^.Data)^.prefix,
          Pmpv_event_log_message(Event^.Data)^.level,
          Pmpv_event_log_message(Event^.Data)^.Text);

      MPV_EVENT_START_FILE:
      begin
        if Assigned(OnStartFile) then
          OnStartFile(Sender);
      end;

      MPV_EVENT_FILE_LOADED:
      begin
        {$IFDEF USETIMER}
        FTimer.Enabled := True;
        FLastPos       := -1;
        {$ENDIF}

        if (FStartAtPosMs > 0) then
        begin
          SetMediaPosInMs(FStartAtPosMs);
          FStartAtPosMs := 0;
        end;

        if Assigned(OnFileLoaded) then OnFileLoaded(Sender);
      end;

      MPV_EVENT_SEEK:
      begin
        if Assigned(OnSeek) then
          OnSeek(Sender, GetMediaPosInMs);
      end;

      MPV_EVENT_END_FILE:
      begin
        if Assigned(OnEndFile) then
          OnEndFile(Sender, Pmpv_event_end_file(Event^.data)^.reason);
      end;

      MPV_EVENT_VIDEO_RECONFIG:
      begin
        GetTracks;
        if Assigned(OnVideoReconfig) then
          OnVideoReconfig(Sender);
      end;

      MPV_EVENT_AUDIO_RECONFIG:
      begin
        GetTracks;
        if Assigned(OnAudioReconfig) then
          OnAudioReconfig(Sender);
      end;

      MPV_EVENT_GET_PROPERTY_REPLY:
      begin
        if Assigned(OnGetReplyEvent) then
          OnGetReplyEvent(Sender, Event^.reply_userdata, Event^.error, Pmpv_event_property(Event^.Data));
      end;

      MPV_EVENT_SET_PROPERTY_REPLY:
      begin
        if Assigned(OnSetReplyEvent) then
          OnSetReplyEvent(Sender, Event^.reply_userdata, Event^.error);
      end;

      MPV_EVENT_COMMAND_REPLY:
      begin
        if Assigned(OnCommandReplyEvent) then
          OnCommandReplyEvent(Sender, Event^.reply_userdata, Event^.error, Pmpv_event_command(Event^.Data));
      end;

      MPV_EVENT_PROPERTY_CHANGE:
      begin
        if (Pmpv_event_property(Event^.Data)^.Name = 'eof-reached') then
        begin
          if (Pmpv_event_property(Event^.Data)^.data <> NIL) and (PInteger(Pmpv_event_property(Event^.Data)^.data)^ = 1) then
          begin
            mpv_set_pause(True);
            if Assigned(OnEndFile) then OnEndFile(Sender, MPV_END_FILE_REASON_EOF);
          end;
        end
        else if (Pmpv_event_property(Event^.Data)^.Name = 'cache-buffering-state') then //if (Pmpv_event_property(Event^.Data)^.Name = 'paused-for-cache') then
        begin
          if Assigned(OnBuffering) and (Pmpv_event_property(Event^.Data)^.data <> NIL) then
            OnBuffering(Sender, PInteger(Pmpv_event_property(Event^.Data)^.data)^);
        end;

        {$IFNDEF USETIMER}
        if (Pmpv_event_property(Event^.Data)^.Name = 'playback-time') and (Pmpv_event_property(Event^.Data)^.format = MPV_FORMAT_INT64) then
        begin
          if Assigned(OnTimeChanged) and (Pmpv_event_property(Event^.Data)^.data <> NIL) then
            OnTimeChanged(Sender, PInteger(Pmpv_event_property(Event^.Data)^.data)^);
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
var
  Pos: Integer;
begin
  FTimer.Enabled := False;

  Pos := GetMediaPosInMs;
  if Assigned(OnTimeChanged) and (FLastPos <> Pos) then
  begin
    OnTimeChanged(Sender, Pos);
    FLastPos := Pos;
  end;

  FTimer.Enabled := True;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure TMPVPlayer.DoResize(Sender: TObject);
begin
  if Assigned(FRenderGL) and not IsPlaying then
    FRenderGL.Render(True);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.AddOption(const AValue: String);
begin
  RemoveOption(AValue);
  FStartOptions.Add(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.RemoveOption(const AValue: String);
var
  i: Integer;
begin
  i := FStartOptions.IndexOfName(Copy(AValue, 1, Pos('=', AValue)));
  if i > -1 then
    FStartOptions.Delete(i);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetVideoAspectRatio(const AValue: TMPVPlayerVideoAspectRatio);
var
  s: String;
begin
  FAspectRatio := AValue;
  case FAspectRatio of
    ar4_3   : s := '4:3';
    ar16_9  : s := '16:9';
    ar185_1 : s := '1.85:1';
    ar235_1 : s := '2.35:1';
  else
    s := '-1';
  end;

  mpv_set_option_string_('video-aspect-override=' + s);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.CycleVideoAspectRatio;
var
  i: Integer;
begin
  i := Integer(FAspectRatio) + 1;
  if i > Integer(ar235_1) then i := 0;

  FAspectRatio := TMPVPlayerVideoAspectRatio(i);
  SetVideoAspectRatio(FAspectRatio);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetVideoFilters(const AVideoFilters: TMPVPlayerVideoFilters);
var
  vf : TMPVPlayerVideoFilter;
  fn, fp, s : String;
begin
  s := '';
  for vf in AVideoFilters do
  begin
    fn := TMPVPlayerVideoFiltersInfo[Integer(vf)].Name;
    fp := TMPVPlayerVideoFiltersInfo[Integer(vf)].Params;

    if s.IsEmpty then
      s := fn
    else
      s += ',' + fn;

    if not fp.IsEmpty then
      s += '=' + fp;
  end;

  mpv_set_option_string_('vf=' + s);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ClearVideoFilters;
begin
  SetVideoFilters([]);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetAudioFilters(const AAudioFilters: TMPVPlayerAudioFilters);
var
  af : TMPVPlayerAudioFilter;
  fn, fp, s : String;
begin
  s := '';
  for af in AAudioFilters do
  begin
    fn := TMPVPlayerAudioFiltersInfo[Integer(af)].Name;
    fp := TMPVPlayerAudioFiltersInfo[Integer(af)].Params;

    if s.IsEmpty then
      s := fn
    else
      s += ',' + fn;

    if not fp.IsEmpty then
      s += '=' + fp;
  end;

  mpv_set_option_string_('af=' + s);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ClearAudioFilters;
begin
  SetAudioFilters([]);
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

