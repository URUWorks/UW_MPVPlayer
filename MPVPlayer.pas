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
  MPVPlayer.RenderGL, OpenGLContext
  {$IFDEF LINUX}, gtk2, gdk2x{$ENDIF}
  {$IFDEF BGLCONTROLS}, BGRAOpenGL{$ENDIF}
  {$IFDEF SDL2}, sdl2lib, libMPV.Render, MPVPlayer.RenderSDL{$ENDIF};

// -----------------------------------------------------------------------------

type

  { TMPVPlayer Types }

  TMPVPlayerRenderMode  = (rmEmbedding, rmOpenGL{$IFDEF SDL2}, rmSDL2{$ENDIF});
  TMPVPlayerTrackType   = (ttVideo, ttAudio, ttSubtitle, ttUnknown);
  TMPVPlayerLogLevel    = (llNo, llFatal, llError, llWarn, llInfo, llStatus, llV, llDebug, llTrace);
  TMPVPlayerNotifyEvent = procedure(ASender: TObject; AParam: Integer) of object;
  TMPVPlayerLogEvent    = procedure(ASender: TObject; APrefix, ALevel, AText: String) of object;

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
    FAutoStart      : Boolean;
    FAutoLoadSub    : Boolean;
    FKeepAspect     : Boolean;
    FSMPTEMode      : Boolean;
    FStartAtPosMs   : Integer;
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

    FText           : String;
    FTextNode       : mpv_node;
    FTextNodeList   : mpv_node_list;
    FTextNodeKeys   : array of PChar;
    FTextNodeValues : array of mpv_node;

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

    {$IFDEF SDL2}
    procedure InitializeRenderSDL;
    procedure UnInitializeRenderSDL;
    {$ENDIF}

    procedure SetRenderMode(Value: TMPVPlayerRenderMode);
    function LogLevelToString: String;
    function SetWID: Boolean;

    {$IFDEF USETIMER}
    procedure DoTimer(Sender: TObject);
    {$ENDIF}

    procedure DoResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsLibMPVAvailable: Boolean;

    function mpv_command_(args: array of const): mpv_error;
    function mpv_command_node_(ANode: mpv_node): mpv_error;
    procedure mpv_set_option_string_(const AValue: String);
    function mpv_get_property_boolean(const APropertyName: String): Boolean;
    procedure mpv_set_property_boolean(const APropertyName: String; const AValue: Boolean);
    function mpv_get_property_double(const AProperty: String): Double;
    procedure mpv_set_property_double(const AProperty: String; const AValue: Double);
    function mpv_get_property_int64(const AProperty: String): Int64;
    procedure mpv_set_property_int64(const AProperty: String; const AValue: Int64);
    procedure mpv_set_pause(const Value: Boolean);

    function GetErrorString: String;
    function GetVersionString: String;
    function GetPlayerHandle: Pmpv_handle;

    procedure Play(const AFileName: String; const AStartAtPositionMs: Integer = 0); overload;
    procedure Play(const FromMs: Integer); overload;
    procedure Close;
    procedure Loop(const AStartTimeMs, BFinalTimeMs: Integer; const ALoopCount: Integer = -1); // string format hh:mm:ss.zzz
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
    procedure ShowText(const AText: String; const ATags: String = '{\an7}');
    procedure SetTextColor(const AValue: String);
    procedure SetTextHAlign(const AValue: String);
    procedure SetTextVAlign(const AValue: String);
    procedure SetTextSize(const AValue: Int64);

    function GetVideoWidth: Integer;
    function GetVideoHeight: Integer;
    function GetVideoTotalFrames: Integer;
    function GetVideoFPS: Double;

    procedure ScreenshotToFile(const AFileName: String); // name with full path, extension defines the format (file.png)

    procedure AddOption(const AValue: String);
    procedure RemoveOption(const AValue: String);

    property mpv_handle    : Pmpv_handle         read FMPV_HANDLE;
    property Error         : mpv_error           read FError;
    property ErrorString   : String              read GetErrorString;
    property Version       : DWord               read FVersion;
    property VersionString : String              read GetVersionString;
    property Initialized   : Boolean             read FInitialized;
    property StartOptions  : TStringList         read FStartOptions;
    property TrackList     : TMPVPlayerTrackList read FTrackList;
    property FileName      : String              read FFileName;
    property MPVFileName   : String              read FMPVFileName   write FMPVFileName;
    property YTDLPFileName : String              read FYTDLPFileName write FYTDLPFileName;
    property SMPTEMode     : Boolean             read FSMPTEMode     write FSMPTEMode;
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
  h     := Time - (Hour*3600000);
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

function TMPVPlayer.mpv_command_(args: array of const): mpv_error;
var
  pArgs: array of PChar;
  i: Integer;
  s: String;
begin
  Result := MPV_ERROR_INVALID_PARAMETER;

  if High(Args) < 0 then
    Exit
  else if FInitialized and (mpv_command <> NIL) and (FMPV_HANDLE <> NIL) then
  begin
    SetLength(pArgs, High(Args)+2);

    for i := 0 to High(Args) do
    begin
      case Args[i].VType of
        vtInteger    : s := IntToStr(Args[i].VInteger);
        vtChar       : s := Args[i].VChar;
        vtString     : s := Args[i].VString^;
        vtPChar      : s := Args[i].VPChar;
        vtAnsiString : s := AnsiString(Args[I].VAnsiString);
      end;

      pArgs[i] := PChar(s);
    end;

    pArgs[High(Args)+2] := NIL;

    FError := mpv_command(FMPV_HANDLE^, PPChar(@pArgs[0]));
    SetLength(pArgs, 0);
  end
  else
    FError := MPV_ERROR_UNINITIALIZED;

  Result := FError;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_command_node_(ANode: mpv_node): mpv_error;
var
  Res: mpv_node;
begin
  FError := MPV_ERROR_UNINITIALIZED;

  if FInitialized and (FMPV_HANDLE <> NIL) then
    FError := mpv_command_node(FMPV_HANDLE^, ANode, Res);

  Result := FError;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_option_string_(const AValue: String);
var
  s1, s2: String;
  i: Integer;
begin
  if not Assigned(mpv_set_option_string) or (FMPV_HANDLE = NIL) or AValue.IsEmpty then Exit;

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

procedure TMPVPlayer.mpv_set_pause(const Value: Boolean);
begin
  mpv_set_property_boolean('pause', Value);
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
  FAutoStart     := True;
  FAutoLoadSub   := False;
  FKeepAspect    := True;
  FSMPTEMode     := False;
  FFileName      := '';
  {$IFDEF LINUX}
  FMPVFileName   := '';
  {$ELSE}
  FMPVFileName   := LIBMPV_DLL_NAME;
  {$ENDIF}
  FYTDLPFileName := '';
  FStartOptions  := TStringList.Create;
  SetLength(FTrackList, 0);

  {$IFDEF WINDOWS}
  FRenderMode    := rmEmbedding;
  {$ELSE}
  FRenderMode    := rmOpenGL;
  {$ENDIF}
  FRenderGL      := NIL;

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

//    {$IFDEF WINDOWS}
    Add('hwdec=no');             // fix some windows crash
//    {$ELSE}
//    Add('hwdec=auto');           // enable best hw decoder.
//    {$ENDIF}
    Add('keep-open=always');     // don't auto close video.
    Add('vd-lavc-dr=no');        // fix possibles deadlock issues with OpenGL
    Add('hr-seek=yes');          // use precise seeks whenever possible.
    Add('hr-seek-framedrop=no'); // default: yes.
    Add('seekbarkeyframes=no');  // default: yes.
    Add('ytdl=yes');             // youtube
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
  if FInitialized then Exit;

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
      sl.Add('pause');  // Start the player in paused state

    if not FAutoLoadSub then
      sl.Add('sub=no'); // don't load subtitles

    if not FKeepAspect then
      sl.Add('no-keepaspect'); // always stretch the video to window size

    for i := 0 to sl.Count-1 do
      mpv_set_option_string_(sl[i]);
  finally
    sl.Free;
  end;

  if not FYTDLPFileName.IsEmpty then
    mpv_set_option_string(FMPV_HANDLE^, PChar('script-opts'), PChar('ytdl_hook-ytdl_path='+FYTDLPFileName));

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
    InitializeRenderGL
  {$IFDEF SDL2}
  else if FRenderMode = rmSDL2 then
    InitializeRenderSDL
  {$ENDIF};

  FInitialized := True;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.UnInitialize;
begin
  if not FInitialized then Exit;

  mpv_command_(['stop']); //mpv_command_(['quit']);

  {$IFDEF USETIMER}
  FTimer.Enabled := False;
  FLastPos       := -1;
  {$ENDIF}

  if Assigned(mpv_set_wakeup_callback) and Assigned(FMPV_HANDLE) then
    mpv_set_wakeup_callback(FMPV_HANDLE^, NIL, Self);

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
  FInitialized  := False;
  {$IFDEF DARWIN}
  Free_libMPV;
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.InitializeRenderGL;
begin
  FGL         := TOpenGLControl.Create(Self);
  FGL.Parent  := Self;
  FGL.Align   := alClient;
  FGL.OnClick := OnClick;
  FGL.OnMouseWheelUp   := OnMouseWheelUp;
  FGL.OnMouseWheelDown := OnMouseWheelDown;
  FGL.OnResize := @DoResize; // force to draw opengl context when paused

  FRenderGL := TMPVPlayerRenderGL.Create(MPVFileName, FGL, FMPV_HANDLE {$IFDEF BGLCONTROLS}, FOnDrawEvent{$ENDIF});
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
procedure TMPVPlayer.InitializeRenderSDL;
begin
  FRenderSDL := TMPVPlayerRenderSDL.Create(MPVFileName, Handle, FMPV_HANDLE);
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

function TMPVPlayer.SetWID: Boolean;
var
  pHwnd: PtrInt;
begin
  if FRenderMode = rmEmbedding then
  begin
    Result := False;
    if not Assigned(mpv_set_option) or not Assigned(FMPV_HANDLE) then Exit;

    {$IFDEF LINUX}
    pHwnd := GDK_WINDOW_XWINDOW(PGtkWidget(Self.Handle)^.window);
    {$ELSE}
    pHwnd := Self.Handle;
    {$ENDIF}
    FError := mpv_set_option(FMPV_HANDLE^, 'wid', MPV_FORMAT_INT64, @pHwnd);
    Result := FError = MPV_ERROR_SUCCESS;
  end
  else
    Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Play(const AFileName: String; const AStartAtPositionMs: Integer = 0);
begin
  Initialize;

  if FInitialized then
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
  UnInitialize;
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
  begin
    mpv_set_pause(True);
    if Assigned(FOnPause) then FOnPause(Self);
  end
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

    mpv_set_pause(False);
    if Assigned(FOnPlay) then FOnPlay(Self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Stop;
begin
  Loop(0, 0);

  if not IsPaused then
    mpv_set_pause(True);

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
    mpv_command_([s, ID])
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
    mpv_command_([s, ID])
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

procedure TMPVPlayer.ShowText(const AText: String; const ATags: String = '{\an7}');
begin
  mpv_command_(['expand-properties', 'show-text', '${osd-ass-cc/0}' + ATags + AText]);
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

procedure TMPVPlayer.ScreenshotToFile(const AFileName: String);
begin
  mpv_command_(['screenshot-to-file', AFileName]);
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
        if (FRenderMode = rmOpenGL) and Assigned(FRenderGL) then
          FRenderGL.Active := False;

        {$IFDEF USETIMER}
        FTimer.Enabled := False;
        FLastPos       := -1;
        {$ENDIF}
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
        if Assigned(OnSeek) then OnSeek(Sender, GetMediaPosInMs);

      MPV_EVENT_END_FILE:
      begin
        if Assigned(OnEndFile) then OnEndFile(Sender, Pmpv_event_end_file(Event^.data)^.reason);
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
        if (Pmpv_event_property(Event^.Data)^.Name = 'eof-reached') then
        begin
          if (Pmpv_event_property(Event^.Data)^.data <> NIL) and (PInteger(Pmpv_event_property(Event^.Data)^.data)^ = 1) then
          begin
            Pause;
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

