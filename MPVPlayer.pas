{*
 * URUWorks MPVPlayer
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
 * Copyright (C) 2021-2026 URUWorks, uruworks@gmail.com.
 *}

unit MPVPlayer;

// -----------------------------------------------------------------------------

{$I MPVPlayer.inc}

interface

uses
  Classes, Controls, SysUtils, LazFileUtils, ExtCtrls, Graphics, LCLType,
  LResources, LazarusPackageIntf, libMPV.Client,
  MPVPlayer.RenderGL, OpenGLContext, MPVPlayer.Filters
  {$IFDEF DARWIN}, dynlibs, CocoaAll{$ENDIF}
  {$IFDEF LINUX}, gtk2, gdk2x{$ENDIF}
  {$IFDEF BGLCONTROLS}, BGRAOpenGL{$ENDIF}
  {$IFDEF SDL2}, sdl2lib, libMPV.Render, MPVPlayer.RenderSDL{$ENDIF};

// -----------------------------------------------------------------------------

type

  TMPVPlayerRenderMode = (rmEmbedding, rmOpenGL{$IFDEF SDL2}, rmSDL2{$ENDIF});
  TMPVPlayerRendeFailAction = (rfSwitchToEmbedding, rfNone);
  TMPVPlayerTrackType = (ttVideo, ttAudio, ttSubtitle, ttUnknown);
  TMPVPlayerVideoAspectRatio = (arDefault, ar4_3, ar16_9, ar185_1, ar235_1);
  TMPVPlayerLogLevel = (llNo, llFatal, llError, llWarn, llInfo, llStatus, llV, llDebug, llTrace);
  TMPVPlayerScreenshotMode = (smSubtitles, smVideo, smWindow);

  TMPVPlayerEventReceived = procedure(ASender: TObject; AEvent: Pmpv_event) of object;
  TMPVPlayerNotifyEvent = procedure(ASender: TObject; AParam: Integer) of object;
  TMPVPlayerEndFileEvent = procedure(ASender: TObject; AReason, AError: Integer) of object;
  TMPVPlayerLogEvent = procedure(ASender: TObject; APrefix, ALevel, AText: String) of object;
  TMPVPlayerGetReplyEvent = procedure(ASender: TObject; reply_userdata: Integer; error_code: mpv_error; event_property: Pmpv_event_property) of object;
  TMPVPlayerSetReplyEvent = procedure(ASender: TObject; reply_userdata: Integer; error_code: mpv_error) of object;
  TMPVPlayerCommandReplyEvent = procedure(ASender: TObject; reply_userdata: Integer; error_code: mpv_error; event_command: Pmpv_event_command) of object;

  TMPVPlayerTrackInfo = record
    Kind: TMPVPlayerTrackType;
    ID: Integer;
    Codec: String;
    Decoder: String;
    Channels: String;
    Title: String;
    Lang: String;
    Selected: Boolean;
  end;

  TMPVPlayerTrackList = array of TMPVPlayerTrackInfo;

  TMPVCore = class;
  TMPVPlayer = class;

  { TMPVEventThread }

  TMPVEventThread = class(TThread)
  private
    FHandle: Pmpv_handle;
    FEvent: Pmpv_event;
    FOwner: TMPVCore;
    procedure HandleEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: Pmpv_handle; AOwner: TMPVCore);
  end;

  { TMPVCore }

  TMPVCore = class(TComponent)
  private
    FMPV_HANDLE: Pmpv_handle;
    FError: mpv_error;
    FVersion: DWord;
    FInitialized: Boolean;
    FUnInitCS: TRTLCriticalSection;
    FStartOptions: TStringList;
    FLogLevel: TMPVPlayerLogLevel;
    FMPVEvent: TMPVEventThread;
    FTrackList: TMPVPlayerTrackList;
    FAutoStart: Boolean;
    FAutoLoadSub: Boolean;
    FUseHWDec: Boolean;
    FSMPTEMode: Boolean;
    FStartAtPosMs: Integer;
    FPausePosMs: Integer;
    FFileName: String;
    FMPVFileName: String;
    FYTDLPFileName: String;
    FFormatSettings: TFormatSettings;

    {$IFDEF USETIMER}
    FTimer: TTimer;
    FLastPos: Integer;
    {$ENDIF}

    FShowText: String;
    FText: String;
    FTextNode: mpv_node;
    FTextNodeList: mpv_node_list;
    FTextNodeKeys: array of PChar;
    FTextNodeValues: array of mpv_node;
    FStringBuilder: TStringBuilder;

    // Eventos
    FOnEventReceived: TMPVPlayerEventReceived;
    FOnStartFile: TNotifyEvent;
    FOnEndFile: TMPVPlayerEndFileEvent;
    FOnFileLoaded: TNotifyEvent;
    FOnVideoReconfig: TNotifyEvent;
    FOnAudioReconfig: TNotifyEvent;
    FOnTracksChanged: TNotifyEvent;
    FOnSeek: TMPVPlayerNotifyEvent;
    FOnPlaybackRestart: TNotifyEvent;
    FOnPlay: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FOnPause: TNotifyEvent;
    FOnTimeChanged: TMPVPlayerNotifyEvent;
    FOnBuffering: TMPVPlayerNotifyEvent;
    FOnLogMessage: TMPVPlayerLogEvent;
    FOnGetReplyEvent: TMPVPlayerGetReplyEvent;
    FOnSetReplyEvent: TMPVPlayerSetReplyEvent;
    FOnCommandReplyEvent: TMPVPlayerCommandReplyEvent;

    procedure ReceivedEvent(Sender: TObject; Event: Pmpv_event);
    function LogLevelToString: String;
    procedure SetLogLevel(const AValue: TMPVPlayerLogLevel);
    procedure SetHWDec(const AValue: Boolean);

    {$IFDEF USETIMER}
    procedure DoTimer(Sender: TObject);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Initialize(AWindowID: Int64 = 0): Boolean;
    procedure UnInitialize(const CS: Boolean = True);

    function IsLibMPVAvailable: Boolean;

    // API
    function mpv_command_(args: array of String; const reply_userdata: Integer = 0): mpv_error;
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
    procedure mpv_set_pause(const AValue: Boolean);

    // HELPERS
    function GetErrorString: String;
    function GetVersionString: String;
    function GetPlayerHandle: Pmpv_handle;

    // REPRODUCCION
    procedure Play(const AFileName: String; const AStartAtPositionMs: Integer = 0); overload;
    procedure Play(const AFromMs: Integer); overload;
    procedure Close(const AForce: Boolean = True);
    procedure Loop(const AStartTimeMs, BFinalTimeMs: Integer; const ALoopCount: Integer = -1);
    procedure Pause;
    procedure Resume(const AForcePlay: Boolean = False);
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

    // TRACKS
    procedure SetTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer); overload;
    procedure SetTrack(const Index: Integer); overload;
    procedure GetTracks;
    function HasVideoTrack: Boolean;
    procedure LoadTrack(const TrackType: TMPVPlayerTrackType; const AFileName: String);
    procedure RemoveTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
    procedure ReloadTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);

    // OSD / SUBTITULOS
    procedure ShowOverlayText(const AText: String; const ATags: String = '{\an2}');
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

    function GetScreenshotToBitmap(const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo): TBitmap;
    procedure ScreenshotToFile(const AFileName: String; const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);
    procedure ScreenshotToClipboard(const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);

    procedure AddOption(const AValue: String);
    procedure RemoveOption(const AValue: String);

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
    property TrackList: TMPVPlayerTrackList read FTrackList;
    property FileName: String read FFileName;
  published
    property AutoStartPlayback: Boolean read FAutoStart write FAutoStart;
    property AutoLoadSubtitle: Boolean read FAutoLoadSub write FAutoLoadSub;
    property UseHWDec: Boolean read FUseHWDec write SetHWDec;
    property LogLevel: TMPVPlayerLogLevel read FLogLevel write SetLogLevel;
    property StartOptions: TStringList read FStartOptions;
    property MPVFileName: String read FMPVFileName write FMPVFileName;
    property YTDLPFileName: String read FYTDLPFileName write FYTDLPFileName;
    property SMPTEMode: Boolean read FSMPTEMode write FSMPTEMode;

    property OnEventReceived: TMPVPlayerEventReceived read FOnEventReceived write FOnEventReceived;
    property OnStartFile: TNotifyEvent read FOnStartFile write FOnStartFile;
    property OnEndFile: TMPVPlayerEndFileEvent read FOnEndFile write FOnEndFile;
    property OnFileLoaded: TNotifyEvent read FOnFileLoaded write FOnFileLoaded;
    property OnVideoReconfig: TNotifyEvent read FOnVideoReconfig write FOnVideoReconfig;
    property OnAudioReconfig: TNotifyEvent read FOnAudioReconfig write FOnAudioReconfig;
    property OnTracksChanged: TNotifyEvent read FOnTracksChanged write FOnTracksChanged;
    property OnSeek: TMPVPlayerNotifyEvent read FOnSeek write FOnSeek;
    property OnPlaybackRestart: TNotifyEvent read FOnPlaybackRestart write FOnPlaybackRestart;
    property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    property OnTimeChanged: TMPVPlayerNotifyEvent read FOnTimeChanged write FOnTimeChanged;
    property OnBuffering: TMPVPlayerNotifyEvent read FOnBuffering write FOnBuffering;
    property OnLogMessage: TMPVPlayerLogEvent read FOnLogMessage write FOnLogMessage;
    property OnGetReplyEvent: TMPVPlayerGetReplyEvent read FOnGetReplyEvent write FOnGetReplyEvent;
    property OnSetReplyEvent: TMPVPlayerSetReplyEvent read FOnSetReplyEvent write FOnSetReplyEvent;
    property OnCommandReplyEvent: TMPVPlayerCommandReplyEvent read FOnCommandReplyEvent write FOnCommandReplyEvent;
  end;

  { TMPVPlayer }

  TMPVPlayer = class(TCustomPanel)
  private
    FCore: TMPVCore;
    FAspectRatio: TMPVPlayerVideoAspectRatio;
    FFontSize: Integer;
    FLastFontSize: Integer;
    FLastMarginX: Integer;
    FLastMarginY: Integer;
    FSafeMarginPercent: Byte;
    FSafeZoneEnabled: Boolean;
    FKeepAspect: Boolean;
    FNoAudioDisplay: Boolean;
    FRenderFail: TMPVPlayerRendeFailAction;
    FRenderMode: TMPVPlayerRenderMode;
    FRenderGL: TMPVPlayerRenderGL;
    FGL: TUWOpenGLControl;

    {$IFDEF ENABLE_BACKIMAGE}
    FBackImage: TPicture;
    {$ENDIF}

    {$IFDEF SDL2}
    FRenderSDL: TMPVPlayerRenderSDL;
    {$ENDIF}

    {$IFDEF BGLCONTROLS}
    FOnDrawEvent: TMPVPlayerDrawEvent;
    {$ENDIF}

    // Property Forwarding
    function GetFileName: String;
    function GetTrackList: TMPVPlayerTrackList;
    function GetError: mpv_error;
    function GetInitialized: Boolean;

    function GetAutoStartPlayback: Boolean;
    procedure SetAutoStartPlayback(const AValue: Boolean);
    function GetAutoLoadSubtitle: Boolean;
    procedure SetAutoLoadSubtitle(const AValue: Boolean);
    function GetUseHWDec: Boolean;
    procedure SetUseHWDec(const AValue: Boolean);
    function GetLogLevel: TMPVPlayerLogLevel;
    procedure SetLogLevel(const AValue: TMPVPlayerLogLevel);
    function GetStartOptions: TStringList;
    function GetMPVFileName: String;
    procedure SetMPVFileName(const AValue: String);
    function GetYTDLPFileName: String;
    procedure SetYTDLPFileName(const AValue: String);
    function GetSMPTEMode: Boolean;
    procedure SetSMPTEMode(const AValue: Boolean);

    function GetOnEventReceived: TMPVPlayerEventReceived;
    procedure SetOnEventReceived(const AValue: TMPVPlayerEventReceived);
    function GetOnStartFile: TNotifyEvent;
    procedure SetOnStartFile(const AValue: TNotifyEvent);
    function GetOnEndFile: TMPVPlayerEndFileEvent;
    procedure SetOnEndFile(const AValue: TMPVPlayerEndFileEvent);
    function GetOnFileLoaded: TNotifyEvent;
    procedure SetOnFileLoaded(const AValue: TNotifyEvent);
    function GetOnVideoReconfig: TNotifyEvent;
    procedure SetOnVideoReconfig(const AValue: TNotifyEvent);
    function GetOnAudioReconfig: TNotifyEvent;
    procedure SetOnAudioReconfig(const AValue: TNotifyEvent);
    function GetOnTracksChanged: TNotifyEvent;
    procedure SetOnTracksChanged(const AValue: TNotifyEvent);
    function GetOnSeek: TMPVPlayerNotifyEvent;
    procedure SetOnSeek(const AValue: TMPVPlayerNotifyEvent);
    function GetOnPlaybackRestart: TNotifyEvent;
    procedure SetOnPlaybackRestart(const AValue: TNotifyEvent);
    function GetOnPlay: TNotifyEvent;
    procedure SetOnPlay(const AValue: TNotifyEvent);
    function GetOnStop: TNotifyEvent;
    procedure SetOnStop(const AValue: TNotifyEvent);
    function GetOnPause: TNotifyEvent;
    procedure SetOnPause(const AValue: TNotifyEvent);
    function GetOnTimeChanged: TMPVPlayerNotifyEvent;
    procedure SetOnTimeChanged(const AValue: TMPVPlayerNotifyEvent);
    function GetOnBuffering: TMPVPlayerNotifyEvent;
    procedure SetOnBuffering(const AValue: TMPVPlayerNotifyEvent);
    function GetOnLogMessage: TMPVPlayerLogEvent;
    procedure SetOnLogMessage(const AValue: TMPVPlayerLogEvent);
    function GetOnGetReplyEvent: TMPVPlayerGetReplyEvent;
    procedure SetOnGetReplyEvent(const AValue: TMPVPlayerGetReplyEvent);
    function GetOnSetReplyEvent: TMPVPlayerSetReplyEvent;
    procedure SetOnSetReplyEvent(const AValue: TMPVPlayerSetReplyEvent);
    function GetOnCommandReplyEvent: TMPVPlayerCommandReplyEvent;
    procedure SetOnCommandReplyEvent(const AValue: TMPVPlayerCommandReplyEvent);

    function GetWID: Int64;
    function InitializeRenderGL: Boolean;
    procedure UnInitializeRenderGL;

    {$IFDEF SDL2}
    function InitializeRenderSDL: Boolean;
    procedure UnInitializeRenderSDL;
    {$ENDIF}

    procedure SetRenderMode(const AValue: TMPVPlayerRenderMode);
    procedure SetFontSize(const AValue: Integer);
    procedure SetSafeMarginPercent(const AValue: Byte);
    procedure SetSafeZoneEnabled(const AValue: Boolean);

    procedure DoOnPaint(Sender: TObject);
    procedure DoOnGLResize(Sender: TObject);
  protected
    {$IFDEF DARWIN}
    procedure CreateWnd; override;
    {$ENDIF}
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {$IFDEF ENABLE_BACKIMAGE}
    procedure EraseBackground(DC: HDC); override;
    {$ENDIF}

    function InitializePlayer: Boolean;
    procedure Play(const AFileName: String; const AStartAtPositionMs: Integer = 0); overload;
    procedure Play(const AFromMs: Integer); overload;
    function CycleVideoAspectRatio: TMPVPlayerVideoAspectRatio;
    procedure EnforceSubtitleSafeZone(const AEnable: Boolean);

    // API
    function IsLibMPVAvailable: Boolean;
    function mpv_command_(args: array of String; const reply_userdata: Integer = 0): mpv_error;
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
    procedure mpv_set_pause(const AValue: Boolean);

    // HELPERS
    function GetErrorString: String;
    function GetVersionString: String;
    function GetPlayerHandle: Pmpv_handle;
    procedure AddOption(const AValue: String);
    procedure RemoveOption(const AValue: String);

    // METODOS PUENTE PARA COMPATIBILIDAD
    procedure Close(const AForce: Boolean = True);
    procedure Loop(const AStartTimeMs, BFinalTimeMs: Integer; const ALoopCount: Integer = -1);
    procedure Pause;
    procedure Resume(const AForcePlay: Boolean = False);
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
    function HasVideoTrack: Boolean;
    procedure LoadTrack(const TrackType: TMPVPlayerTrackType; const AFileName: String);
    procedure RemoveTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
    procedure ReloadTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);

    function GetVideoWidth: Integer;
    function GetVideoHeight: Integer;
    function GetVideoTotalFrames: Integer;
    function GetVideoFPS: Double;

    function GetScreenshotToBitmap(const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo): TBitmap;
    procedure ScreenshotToFile(const AFileName: String; const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);
    procedure ScreenshotToClipboard(const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);

    procedure SetVideoAspectRatio(const AValue: TMPVPlayerVideoAspectRatio);

    procedure SetVideoFilters(const AVideoFilters: TMPVPlayerVideoFilters);
    procedure ClearVideoFilters;
    procedure SetAudioFilters(const AAudioFilters: TMPVPlayerAudioFilters);
    procedure ClearAudioFilters;

    // OSD / SUBTITULOS
    procedure ShowOverlayText(const AText: String; const ATags: String = '{\an2}');
    procedure ShowText(const AText: String; const ADuration: Integer = 1000; const ATags: String = '{\an7}');
    procedure SetTextColor(const AValue: String);
    procedure SetTextHAlign(const AValue: String);
    procedure SetTextVAlign(const AValue: String);
    procedure SetTextSize(const AValue: Int64);
    procedure SetTextFont(const AValue: String);
    procedure SetSubtitleColor(const AValue: String);
    procedure SetSubtitleSize(const AValue: Int64);
    procedure SetSubtitleFont(const AValue: String);

    property Core: TMPVCore read FCore;
    property Error: mpv_error read GetError;
    property FileName: String read GetFileName;
    property TrackList: TMPVPlayerTrackList read GetTrackList;
    property Initialized: Boolean read GetInitialized;
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
    property Color default $101010;
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

    property KeepAspect: Boolean read FKeepAspect write FKeepAspect;
    property AspectRatio: TMPVPlayerVideoAspectRatio read FAspectRatio write SetVideoAspectRatio;
    property NoAudioDisplay: Boolean read FNoAudioDisplay write FNoAudioDisplay;
    property RendererMode: TMPVPlayerRenderMode read FRenderMode write SetRenderMode;
    property RenderFailAction: TMPVPlayerRendeFailAction read FRenderFail write FRenderFail;

    property FontSize: Integer read FFontSize write SetFontSize default 55;
    property SafeMarginPercent: Byte read FSafeMarginPercent write SetSafeMarginPercent default 10;
    property SafeZoneEnabled: Boolean read FSafeZoneEnabled write SetSafeZoneEnabled default False;

    {$IFDEF ENABLE_BACKIMAGE}
    property BackImage: TPicture read FBackImage write FBackImage;
    {$ENDIF}

    {$IFDEF BGLCONTROLS}
    property OnDraw: TMPVPlayerDrawEvent read FOnDrawEvent write FOnDrawEvent;
    {$ENDIF}

    // CORE
    property AutoStartPlayback: Boolean read GetAutoStartPlayback write SetAutoStartPlayback;
    property AutoLoadSubtitle: Boolean read GetAutoLoadSubtitle write SetAutoLoadSubtitle;
    property UseHWDec: Boolean read GetUseHWDec write SetUseHWDec;
    property LogLevel: TMPVPlayerLogLevel read GetLogLevel write SetLogLevel;
    property StartOptions: TStringList read GetStartOptions;
    property MPVFileName: String read GetMPVFileName write SetMPVFileName;
    property YTDLPFileName: String read GetYTDLPFileName write SetYTDLPFileName;
    property SMPTEMode: Boolean read GetSMPTEMode write SetSMPTEMode;

    property OnEventReceived: TMPVPlayerEventReceived read GetOnEventReceived write SetOnEventReceived;
    property OnStartFile: TNotifyEvent read GetOnStartFile write SetOnStartFile;
    property OnEndFile: TMPVPlayerEndFileEvent read GetOnEndFile write SetOnEndFile;
    property OnFileLoaded: TNotifyEvent read GetOnFileLoaded write SetOnFileLoaded;
    property OnVideoReconfig: TNotifyEvent read GetOnVideoReconfig write SetOnVideoReconfig;
    property OnAudioReconfig: TNotifyEvent read GetOnAudioReconfig write SetOnAudioReconfig;
    property OnTracksChanged: TNotifyEvent read GetOnTracksChanged write SetOnTracksChanged;
    property OnSeek: TMPVPlayerNotifyEvent read GetOnSeek write SetOnSeek;
    property OnPlaybackRestart: TNotifyEvent read GetOnPlaybackRestart write SetOnPlaybackRestart;
    property OnPlay: TNotifyEvent read GetOnPlay write SetOnPlay;
    property OnStop: TNotifyEvent read GetOnStop write SetOnStop;
    property OnPause: TNotifyEvent read GetOnPause write SetOnPause;
    property OnTimeChanged: TMPVPlayerNotifyEvent read GetOnTimeChanged write SetOnTimeChanged;
    property OnBuffering: TMPVPlayerNotifyEvent read GetOnBuffering write SetOnBuffering;
    property OnLogMessage: TMPVPlayerLogEvent read GetOnLogMessage write SetOnLogMessage;
    property OnGetReplyEvent: TMPVPlayerGetReplyEvent read GetOnGetReplyEvent write SetOnGetReplyEvent;
    property OnSetReplyEvent: TMPVPlayerSetReplyEvent read GetOnSetReplyEvent write SetOnSetReplyEvent;
    property OnCommandReplyEvent: TMPVPlayerCommandReplyEvent read GetOnCommandReplyEvent write SetOnCommandReplyEvent;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

uses
  Clipbrd, Math;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

{$IFDEF DARWIN}
var
  FMetalSupportCache: ShortInt = -1;

function TestMetalSupport: Boolean;
const
  libMetal = '/System/Library/Frameworks/Metal.framework/Metal';
var
  hMetal: TLibHandle;
  MTLCreateSystemDefaultDevice: function: Pointer; cdecl;
begin
  if FMetalSupportCache = -1 then
  begin
    FMetalSupportCache := 0;
    hMetal := LoadLibrary(libMetal);
    if hMetal <> 0 then
    begin
      Pointer(MTLCreateSystemDefaultDevice) := GetProcAddress(hMetal, 'MTLCreateSystemDefaultDevice');
      if Assigned(MTLCreateSystemDefaultDevice) and (MTLCreateSystemDefaultDevice() <> NIL) then
        FMetalSupportCache := 1;

      UnloadLibrary(hMetal);
    end;
  end;

  Result := (FMetalSupportCache = 1);
end;
{$ENDIF}

// -----------------------------------------------------------------------------

function MSToTimeStamp(const Time: Integer): String; // 'hh:mm:ss.zzz'
var
  Hour, Min, Secs, MSecs,
  h, m, x: Integer;
begin
  Hour := Trunc(Time / 3600000);
  h := Time - (Hour * 3600000);
  Min := Trunc(h / 60000);
  m := Min * 60000;
  x := h - m;
  Secs := Trunc(x / 1000);
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

{$IFDEF LINUX}
function IsWaylandSession: Boolean;
begin
  Result := (GetEnvironmentVariable('WAYLAND_DISPLAY') <> '') or
            (LowerCase(GetEnvironmentVariable('XDG_SESSION_TYPE')) = 'wayland');
end;
{$ENDIF}

// -----------------------------------------------------------------------------

{ TMPVEventThread }

// -----------------------------------------------------------------------------

constructor TMPVEventThread.Create(AHandle: Pmpv_handle; AOwner: TMPVCore);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FHandle := AHandle;
  FOwner := AOwner;
end;

// -----------------------------------------------------------------------------

procedure TMPVEventThread.HandleEvent;
begin
  if Assigned(FOwner) and not Terminated then
    FOwner.ReceivedEvent(FOwner, FEvent);
end;

// -----------------------------------------------------------------------------

procedure TMPVEventThread.Execute;
begin
  while not Terminated do
  begin
    FEvent := mpv_wait_event(FHandle^, 0.1);
    if FEvent^.event_id = MPV_EVENT_NONE then
      Continue
    else if FEvent^.event_id = MPV_EVENT_SHUTDOWN then
      Break;

    if Terminated then
      Break;

    Synchronize(@HandleEvent);
  end;
end;

// -----------------------------------------------------------------------------

{ TMPVCore }

// -----------------------------------------------------------------------------

constructor TMPVCore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMPV_HANDLE := NIL;
  FVersion := 0;
  FError := 0;
  FInitialized := False;
  InitCriticalSection(FUnInitCS);
  FMPVEvent := NIL;
  FLogLevel := llStatus;
  FAutoStart := True;
  FAutoLoadSub := False;
  FUseHWDec := False;
  FSMPTEMode := False;
  FPausePosMs := -1;
  FFileName := '';
  FMPVFileName := '';
  FYTDLPFileName := '';
  FStartOptions := TStringList.Create;
  SetLength(FTrackList, 0);

  {$IFDEF USETIMER}
  FTimer := TTimer.Create(NIL);
  FTimer.Enabled := False;
  FTimer.Interval := 33; // 16 = 60fps (Máxima fluidez), 33 = 30fps (estándar), 50 = 20fps (Ahorro de Energía);
  FTimer.OnTimer := @DoTimer;
  FLastPos := -1;
  {$ENDIF}

  FFormatSettings := DefaultFormatSettings;
  with FFormatSettings do
  begin
    DecimalSeparator := '.';
    ThousandSeparator := DecimalSeparator;
  end;

  FStringBuilder := TStringBuilder.Create;

  with FStartOptions do
  begin
    Sorted := True;
    Duplicates := dupIgnore;

    Add('hwdec=no');
    Add('vd-lavc-dr=no');
    Add('osc=no');
    Add('keep-open=always');
    Add('hr-seek=yes');
    Add('hr-seek-framedrop=no');

    Add('osd-scale-by-window=yes');
    Add('osd-align-y=bottom');
    Add('osd-align-x=center');
    Add('sub-scale-with-window=yes');
    Add('sub-use-margins=no');
    Add('sub-ass-override=force');
    Add('sub-align-y=bottom');

    Add('ytdl=yes');
  end;

  FError := Load_libMPV(FMPVFileName);
  if (FError = MPV_ERROR_SUCCESS) and Assigned(mpv_client_api_version) then
    FVersion := mpv_client_api_version();
end;

// -----------------------------------------------------------------------------

destructor TMPVCore.Destroy;
begin
  UnInitialize;
  DoneCriticalSection(FUnInitCS);

  {$IFDEF USETIMER}
  FTimer.Free;
  {$ENDIF}

  FStringBuilder.Free;
  FStartOptions.Free;

  Free_libMPV;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TMPVCore.Initialize(AWindowID: Int64 = 0): Boolean;
var
  sl: TStringList;
  i: Integer;
begin
  EnterCriticalSection(FUnInitCS);
  try
    if FInitialized then Exit(True);

    Result := False;

    if not IsLibMPV_Loaded then Exit;

    FMPV_HANDLE := mpv_create();
    if not Assigned(FMPV_HANDLE) then
    begin
      FError := MPV_ERROR_UNSUPPORTED;
      Exit;
    end;

    if Assigned(mpv_client_api_version) and (FVersion = 0) then
      FVersion := mpv_client_api_version();

    sl := TStringList.Create;
    try
      sl.Assign(FStartOptions);

      if not FAutoStart then sl.Add('pause');
      if not FAutoLoadSub then sl.Add('sub=no');

      if AWindowID = 0 then
      begin
        sl.Values['vo'] := 'null'; // No video
        sl.Values['ao'] := 'null'; // No audio
      end;

      for i := 0 to sl.Count-1 do
        mpv_set_option_string_(sl[i]);
    finally
      sl.Free;
    end;

    if not FYTDLPFileName.IsEmpty then
      mpv_set_option_string(FMPV_HANDLE^, PChar('script-opts'), PChar('ytdl_hook-ytdl_path=' + FYTDLPFileName));

    if AWindowID <> 0 then
    begin
      FError := mpv_set_option(FMPV_HANDLE^, 'wid', MPV_FORMAT_INT64, @AWindowID);
      if FError <> MPV_ERROR_SUCCESS then
      begin
        UnInitialize(False);
        Exit;
      end;
    end;

    {$IFNDEF USETIMER}
    mpv_observe_property(FMPV_HANDLE^, 0, 'playback-time', MPV_FORMAT_INT64);
    {$ENDIF}
    mpv_observe_property(FMPV_HANDLE^, 0, 'eof-reached', MPV_FORMAT_FLAG);
    mpv_observe_property(FMPV_HANDLE^, 0, 'cache-buffering-state', MPV_FORMAT_INT64);

    FError := mpv_initialize(FMPV_HANDLE^);
    if FError <> MPV_ERROR_SUCCESS then
    begin
      UnInitialize(False);
      Exit;
    end;

    FError := mpv_request_log_messages(FMPV_HANDLE^, PChar(LogLevelToString));

    FShowText := '';
    FText := '';
    SetLength(FTextNodeKeys, 4);
    SetLength(FTextNodeValues, 4);
    FTextNodeKeys[0] := 'name';
    FTextNodeValues[0].format := MPV_FORMAT_STRING;
    FTextNodeValues[0].u._string := 'osd-overlay';
    FTextNodeKeys[1] := 'id';
    FTextNodeValues[1].format := MPV_FORMAT_INT64;
    FTextNodeValues[1].u.int64_ := 1;
    FTextNodeKeys[2] := 'format';
    FTextNodeValues[2].format := MPV_FORMAT_STRING;
    FTextNodeValues[2].u._string := NIL;
    FTextNodeKeys[3] := 'data';
    FTextNodeValues[3].format := MPV_FORMAT_STRING;
    FTextNodeValues[3].u._string := NIL;
    FTextNodeList.num := 4;
    FTextNodeList.keys := @FTextNodeKeys[0];
    FTextNodeList.values := @FTextNodeValues[0];
    FTextNode.format := MPV_FORMAT_NODE_MAP;
    FTextNode.u.list := @FTextNodeList;

    FMPVEvent := TMPVEventThread.Create(FMPV_HANDLE, Self);
    FMPVEvent.Start;

    {$IFDEF USETIMER}
    FTimer.Enabled := False;
    FLastPos := -1;
    {$ENDIF}

    FPausePosMs := -1;
    FInitialized := True;
    Result := True;
  finally
    LeaveCriticalSection(FUnInitCS);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.UnInitialize(const CS: Boolean = True);
begin
  if CS then EnterCriticalSection(FUnInitCS);
  try
    if not FInitialized then Exit;
    FInitialized := False;

    {$IFDEF USETIMER}
    FTimer.Enabled := False;
    FLastPos := -1;
    {$ENDIF}

    if Assigned(FMPVEvent) then
    begin
      FMPVEvent.Terminate;
      if Assigned(mpv_wakeup) and Assigned(FMPV_HANDLE) then
        mpv_wakeup(FMPV_HANDLE^);

      FMPVEvent.WaitFor;
      FreeAndNil(FMPVEvent);
    end;

    if Assigned(mpv_unobserve_property) and Assigned(FMPV_HANDLE) then
      mpv_unobserve_property(FMPV_HANDLE^, 0);

    if Assigned(mpv_set_wakeup_callback) and Assigned(FMPV_HANDLE) then
      mpv_set_wakeup_callback(FMPV_HANDLE^, NIL, NIL);

    if Assigned(mpv_terminate_destroy) and Assigned(FMPV_HANDLE) then
    begin
      mpv_terminate_destroy(FMPV_HANDLE^);
      FMPV_HANDLE := NIL;
    end;

    FShowText := '';
    FText := '';
    SetLength(FTextNodeKeys, 0);
    SetLength(FTextNodeValues, 0);
    SetLength(FTrackList, 0);
    FFileName := '';
    FPausePosMs := -1;
  finally
    if CS then LeaveCriticalSection(FUnInitCS);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVCore.IsLibMPVAvailable: Boolean;
begin
  FError := IsLibMPV_Installed(FMPVFileName);
  Result := (FError = MPV_ERROR_SUCCESS);
end;

// -----------------------------------------------------------------------------

function TMPVCore.mpv_command_(args: array of String; const reply_userdata: Integer = 0): mpv_error;
const
  MAX_STACK_ARGS = 10;
var
  StackArgs: array[0..MAX_STACK_ARGS] of PChar;
  DynArgs: array of PChar;
  pArgs: PPChar;
  i, Count: Integer;
begin
  Result := MPV_ERROR_INVALID_PARAMETER;
  Count := Length(Args);

  if Count = 0 then Exit
  else if not (FInitialized and (FMPV_HANDLE <> NIL)) then
  begin
    FError := MPV_ERROR_UNINITIALIZED;
    Exit(FError);
  end;

  if Count <= MAX_STACK_ARGS then
    pArgs := @StackArgs[0]
  else
  begin
    SetLength(DynArgs, Count + 1);
    pArgs := @DynArgs[0];
  end;

  for i := 0 to Count - 1 do
    pArgs[i] := PChar(Args[i]);

  pArgs[Count] := NIL;

  if reply_userdata > 0 then
    FError := mpv_command_async(FMPV_HANDLE^, reply_userdata, pArgs)
  else
    FError := mpv_command(FMPV_HANDLE^, pArgs);

  if Count > MAX_STACK_ARGS then SetLength(DynArgs, 0);
  Result := FError;
end;

// -----------------------------------------------------------------------------

function TMPVCore.mpv_command_node_(ANode: mpv_node; const reply_userdata: Integer = 0): mpv_error;
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

procedure TMPVCore.mpv_abort_async_command_(const reply_userdata: Integer);
begin
  if FInitialized and (FMPV_HANDLE <> NIL) then
    mpv_abort_async_command(FMPV_HANDLE^, reply_userdata);
end;

// -----------------------------------------------------------------------------

function TMPVCore.mpv_set_option_string_(const AValue: String): Integer;
var
  s1, s2: String;
  i: Integer;
begin
  FError := MPV_ERROR_OPTION_ERROR;
  if not Assigned(mpv_set_option_string) or (FMPV_HANDLE = NIL) or AValue.IsEmpty then Exit(FError);

  i := AValue.IndexOf('=');
  if i > -1 then
  begin
    s1 := AValue.Substring(0, i);
    s2 := AValue.Substring(i + 1);
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

function TMPVCore.mpv_get_property_string_(const APropertyName: String; const reply_userdata: Integer = 0): String;
var
  TempPChar: PChar;
begin
  Result := '';
  if FInitialized and (FMPV_HANDLE <> NIL) then
  begin
    if reply_userdata > 0 then
      FError := mpv_get_property_async(FMPV_HANDLE^, reply_userdata, PChar(APropertyName), MPV_FORMAT_STRING)
    else
    begin
      TempPChar := NIL;
      FError := mpv_get_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_STRING, @TempPChar);
      if (FError = MPV_ERROR_SUCCESS) and (TempPChar <> NIL) then
      begin
        Result := StrPas(TempPChar);
        mpv_free(TempPChar);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.mpv_set_property_string_(const APropertyName: String; const AValue: String; const reply_userdata: Integer = 0);
var
  p: PChar;
begin
  if not FInitialized or (FMPV_HANDLE = NIL) then Exit;
  p := PChar(AValue);
  if reply_userdata > 0 then
    FError := mpv_set_property_async(FMPV_HANDLE^, reply_userdata, PChar(APropertyName), MPV_FORMAT_STRING, @p)
  else
    FError := mpv_set_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_STRING, @p);
end;

// -----------------------------------------------------------------------------

function TMPVCore.mpv_get_property_boolean(const APropertyName: String; const reply_userdata: Integer = 0): Boolean;
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

procedure TMPVCore.mpv_set_property_boolean(const APropertyName: String; const AValue: Boolean; const reply_userdata: Integer = 0);
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

function TMPVCore.mpv_get_property_double(const APropertyName: String; const reply_userdata: Integer = 0): Double;
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

procedure TMPVCore.mpv_set_property_double(const APropertyName: String; const AValue: Double; const reply_userdata: Integer = 0);
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

function TMPVCore.mpv_get_property_int64(const APropertyName: String; const reply_userdata: Integer = 0): Int64;
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

procedure TMPVCore.mpv_set_property_int64(const APropertyName: String; const AValue: Int64; const reply_userdata: Integer = 0);
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

procedure TMPVCore.mpv_set_pause(const AValue: Boolean);
begin
  mpv_set_property_boolean('pause', AValue);
  case AValue of
    True  : if Assigned(FOnPause) then FOnPause(Self);
    False : if Assigned(FOnPlay) then FOnPlay(Self);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetErrorString: String;
begin
  if Assigned(mpv_error_string) then
    Result := mpv_error_string(FError)
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetVersionString: String;
begin
  Result := Format('libmpv %d.%d', [FVersion shr 16, FVersion and $FF]);
end;

// -----------------------------------------------------------------------------

function TMPVCore.LogLevelToString: String;
begin
  case FLogLevel of
    llFatal  : Result := 'fatal';
    llError  : Result := 'error';
    llWarn   : Result := 'warn';
    llInfo   : Result := 'info';
    llStatus : Result := 'status';
    llV      : Result := 'v';
    llDebug  : Result := 'debug';
    llTrace  : Result := 'trace';
  else
    Result := 'no';
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetLogLevel(const AValue: TMPVPlayerLogLevel);
begin
  if FLogLevel = AValue then Exit;
  FLogLevel := AValue;
  if FInitialized and (FMPV_HANDLE <> NIL) then
    mpv_request_log_messages(FMPV_HANDLE^, PChar(LogLevelToString));
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetPlayerHandle: Pmpv_handle;
begin
  Result := FMPV_HANDLE;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.Play(const AFileName: String; const AStartAtPositionMs: Integer = 0);
begin
  if Initialize() then
  begin
    FStartAtPosMs := AStartAtPositionMs;
    FFileName := AFileName;
    Loop(0, 0);
    FPausePosMs := -1;
    mpv_command_(['loadfile', FFileName]);
    mpv_set_property_boolean('pause', not FAutoStart);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.Play(const AFromMs: Integer);
begin
  SeekInMs(AFromMs);
  Resume(True);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.Close(const AForce: Boolean = True);
begin
  if AForce then UnInitialize else mpv_command_(['quit']);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.Loop(const AStartTimeMs, BFinalTimeMs: Integer; const ALoopCount: Integer = -1);
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

procedure TMPVCore.Pause;
begin
  if not FInitialized then Exit;

  Loop(0, 0);
  if IsPlaying then
    mpv_set_pause(True)
  else
    Resume(True);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.Resume(const AForcePlay: Boolean = False);
begin
  if not FInitialized then Exit;

  if AForcePlay or IsPaused then
  begin
    if GetMediaPosInMs >= GetMediaLenInMs then
      SetMediaPosInMs(0);

    FPausePosMs := -1;
    mpv_set_pause(False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.Stop;
begin
  if not FInitialized then Exit;
  Loop(0, 0);

  if not IsPaused then
    mpv_set_pause(True);

  FPausePosMs := -1;
  SetMediaPosInMs(0);

  if Assigned(FOnStop) then
    FOnStop(Self);
end;

// -----------------------------------------------------------------------------

function TMPVCore.IsMediaLoaded: Boolean;
begin
  Result := GetMediaLenInMs > 0;
end;

// -----------------------------------------------------------------------------

function TMPVCore.IsPlaying: Boolean;
begin
  Result := not IsPaused;
end;

// -----------------------------------------------------------------------------

function TMPVCore.IsPaused: Boolean;
begin
  Result := (mpv_get_property_boolean('pause') = True);
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetMediaLenInMs: Integer;
begin
  Result := Round(mpv_get_property_double('duration') * 1000.0);
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetMediaPosInMs: Integer;
var
  i: Double;
begin
  if FPausePosMs > -1 then Exit(FPausePosMs);
  i := mpv_get_property_double('time-pos') * 1000.0;

  if FSMPTEMode then
    Result := Round(i / 1.001)
  else
    Result := Round(i);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetMediaPosInMs(const AValue: Integer);
var
  i: Double;
  s: String;
begin
  if IsPaused and (AValue <= GetMediaLenInMs) then
    FPausePosMs := AValue;

  i := AValue / 1000.0;
  if FSMPTEMode then
    i := i * 1.001;

  s := FloatToStr(i, FFormatSettings);
  mpv_command_(['seek', PChar(s), 'absolute+exact']);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SeekInMs(const MSecs: Integer; const SeekAbsolute: Boolean = True);
begin
  if SeekAbsolute then
    SetMediaPosInMs(MSecs)
  else
    SetMediaPosInMs(GetMediaPosInMs + MSecs);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.NextFrame(const AStep: Integer = 1);
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
      if Assigned(FOnPause) then
        FOnPause(Self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.PreviousFrame(const AStep: Integer = 1);
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
      if Assigned(FOnPause) then
        FOnPause(Self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetPlaybackRate(const AValue: Byte);
begin
  mpv_set_property_double('speed', AValue / 100.0);
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetAudioVolume: Byte;
begin
  Result := Trunc(mpv_get_property_int64('volume'));
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetAudioVolume(const AValue: Byte);
begin
  mpv_set_property_int64('volume', AValue);
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetAudioMute: Boolean;
begin
  Result := mpv_get_property_boolean('mute');
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetAudioMute(const AValue: Boolean);
begin
  mpv_set_property_boolean('mute', AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer);
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

procedure TMPVCore.SetTrack(const Index: Integer);
begin
  SetTrack(TrackList[Index].Kind, TrackList[Index].ID);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.GetTracks;
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

function TMPVCore.HasVideoTrack: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(FTrackList)-1 do
    if FTrackList[i].Kind = ttVideo then
      Exit(True);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.LoadTrack(const TrackType: TMPVPlayerTrackType; const AFileName: String);
var
  s: String;
begin
  if AFileName.IsEmpty then Exit;
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

procedure TMPVCore.RemoveTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
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

procedure TMPVCore.ReloadTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
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

procedure TMPVCore.ShowOverlayText(const AText: String; const ATags: String = '{\an2}');
var
  i, StartPos, TextLen, TagPos, CurrentLineStart: Integer;
  GlobalStyle: String;
begin
  if not FInitialized then Exit;

  if AText.IsEmpty then
  begin
    FText := '';
    FTextNodeValues[2].u._string := 'none';
    FTextNodeValues[3].u._string := '';
    mpv_command_node_(FTextNode);
    Exit;
  end;

  FStringBuilder.Clear;
  FStringBuilder.EnsureCapacity(Length(AText) + 32);
  FStringBuilder.Append(ATags);
  FStringBuilder.Append('{\q2}');

  TextLen := Length(AText);
  StartPos := 1;

  while (StartPos <= TextLen) and (AText[StartPos] = '{') and
        (StartPos < TextLen) and (AText[StartPos+1] = '\') do
  begin
    TagPos := Pos('}', AText, StartPos);
    if TagPos = 0 then Break;
    StartPos := TagPos + 1;
  end;

  GlobalStyle := Copy(AText, 1, StartPos - 1);
  FStringBuilder.Append(GlobalStyle);

  CurrentLineStart := StartPos;
  i := StartPos;

  while i <= TextLen do
  begin
    if (AText[i] = #10) or (AText[i] = #13) then
    begin
      if i > CurrentLineStart then
        FStringBuilder.Append(AText, CurrentLineStart - 1, i - CurrentLineStart);

      FStringBuilder.Append('\N');
      if (AText[i] = #13) and (i < TextLen) and (AText[i+1] = #10) then
        i := i + 2
      else
        i := i + 1;

      CurrentLineStart := i;
    end
    else
      Inc(i);
  end;

  if CurrentLineStart <= TextLen then
    FStringBuilder.Append(AText, CurrentLineStart - 1, TextLen - CurrentLineStart + 1);

  FText := FStringBuilder.ToString;
  if FText.EndsWith('\N') then
    Delete(FText, Length(FText)-1, 2);

  FTextNodeValues[2].u._string := 'ass-events';
  FTextNodeValues[3].u._string := PChar(FText);
  mpv_command_node_(FTextNode);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.ShowText(const AText: String; const ADuration: Integer = 1000; const ATags: String = '{\an7}');
begin
  if (AText <> FShowText) then
  begin
    FShowText := AText;
    mpv_command_(['expand-properties', 'show-text', '${osd-ass-cc/0}{\q2}' + ATags + AText.Replace(sLineBreak, '\N'), ADuration.ToString]);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetTextColor(const AValue: String);
begin
  mpv_set_option_string_('osd-color='+AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetTextVAlign(const AValue: String);
begin
  mpv_set_option_string_('osd-align-y='+AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetTextHAlign(const AValue: String);
begin
  mpv_set_option_string_('osd-align-x='+AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetTextSize(const AValue: Int64);
begin
  mpv_set_property_int64('osd-font-size', AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetTextFont(const AValue: String);
begin
  mpv_set_option_string_('osd-font='+AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetSubtitleColor(const AValue: String);
begin
  mpv_set_option_string_('sub-color='+AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetSubtitleSize(const AValue: Int64);
begin
  mpv_set_property_int64('sub-font-size', AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetSubtitleFont(const AValue: String);
begin
  mpv_set_option_string_('sub-font='+AValue);
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetVideoWidth: Integer;
begin
  Result := mpv_get_property_int64('width');
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetVideoHeight: Integer;
begin
  Result := mpv_get_property_int64('height');
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetVideoTotalFrames: Integer;
begin
  Result := mpv_get_property_int64('estimated-frame-count');
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetVideoFPS: Double;
const
  EPSILON = 0.1;
begin
  Result := mpv_get_property_double('container-fps');

  if (Result < EPSILON) then
    Result := mpv_get_property_double('estimated-vf-fps');

  if (Result < EPSILON) then
    Result := 23.976;
end;

// -----------------------------------------------------------------------------

function TMPVCore.GetScreenshotToBitmap(const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo): TBitmap;
var
  cmd, res: mpv_node;
  list: mpv_node_list;
  vals: array[0..1] of mpv_node;
  ssm: String;
  i, w, h, stride: Integer;
  data: PByte;
  fmt: String;
  SrcPtr, DestPtr: PByte;
  pKeys: PPChar;
  pVals: Pmpv_node;
  Key: String;
  DestStride: Integer;
begin
  Result := NIL;
  if not FInitialized or (FMPV_HANDLE = NIL) then Exit;

  case AScreenshotMode of
    smSubtitles : ssm := 'subtitles';
    smWindow    : ssm := 'window';
  else
    ssm := 'video';
  end;

  vals[0].format := MPV_FORMAT_STRING;
  vals[0].u._string := 'screenshot-raw';
  vals[1].format := MPV_FORMAT_STRING;
  vals[1].u._string := PChar(ssm);

  list.num := 2;
  list.values := @vals[0];
  list.keys := NIL;

  cmd.format := MPV_FORMAT_NODE_ARRAY;
  cmd.u.list := @list;

  if mpv_command_node(FMPV_HANDLE^, cmd, res) = MPV_ERROR_SUCCESS then
  begin
    try
      if res.format = MPV_FORMAT_NODE_MAP then
      begin
        w := 0;
        h := 0;
        stride := 0;
        data := NIL;
        fmt := '';
        pKeys := res.u.list^.keys;
        pVals := res.u.list^.values;

        for i := 0 to res.u.list^.num - 1 do
        begin
          if (pKeys <> NIL) and (pVals <> NIL) then
          begin
            Key := StrPas(pKeys^);
            if Key = 'w' then
              w := pVals^.u.int64_
            else if Key = 'h' then
              h := pVals^.u.int64_
            else if Key = 'stride' then
              stride := pVals^.u.int64_
            else if Key = 'format' then
              fmt := StrPas(pVals^.u._string)
            else if Key = 'data' then
            begin
              if pVals^.format = MPV_FORMAT_BYTE_ARRAY then
                data := PByte(pVals^.u.ba^.data);
            end;
          end;
          Inc(pKeys);
          Inc(pVals);
        end;

        if (data <> NIL) and (w > 0) and (h > 0) and (fmt = 'bgr0') then
        begin
          Result := TBitmap.Create;
          Result.PixelFormat := pf32bit;
          Result.Width := w;
          Result.Height := h;

          Result.BeginUpdate(False);
          try
            SrcPtr := data;
            DestPtr := Result.RawImage.Data;
            DestStride := Result.RawImage.Description.BytesPerLine;

            if (stride = w * 4) and (DestStride = w * 4) then
            begin
              Move(SrcPtr^, DestPtr^, w * 4 * h);
            end
            else
            begin
              for i := 0 to h - 1 do
              begin
                Move(SrcPtr^, DestPtr^, w * 4);
                Inc(SrcPtr, stride);
                Inc(DestPtr, DestStride);
              end;
            end;
          finally
            Result.EndUpdate(False);
          end;
        end;
      end;
    finally
      if Assigned(mpv_free_node_contents) then
        mpv_free_node_contents(res);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.ScreenshotToFile(const AFileName: String; const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);
var
  ssm: String;
begin
  case AScreenshotMode of
    smSubtitles : ssm := 'subtitles';
    smWindow    : ssm := 'window';
  else
    ssm := 'video';
  end;
  mpv_command_(['screenshot-to-file', AFileName, ssm]);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.ScreenshotToClipboard(const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);
var
  Bmp: TBitmap;
begin
  Bmp := GetScreenshotToBitmap(AScreenshotMode);

  if Assigned(Bmp) then
  begin
    try
      Clipboard.Assign(Bmp);
    finally
      Bmp.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.AddOption(const AValue: String);
begin
  RemoveOption(AValue);
  FStartOptions.Add(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.RemoveOption(const AValue: String);
var
  i: Integer;
begin
  i := FStartOptions.IndexOfName(Copy(AValue, 1, Pos('=', AValue)));
  if i > -1 then FStartOptions.Delete(i);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetVideoFilters(const AVideoFilters: TMPVPlayerVideoFilters);
var
  vf: TMPVPlayerVideoFilter;
  fn, fp, s: String;
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

procedure TMPVCore.ClearVideoFilters;
begin
  SetVideoFilters([]);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetAudioFilters(const AAudioFilters: TMPVPlayerAudioFilters);
var
  af: TMPVPlayerAudioFilter;
  fn, fp, s: String;
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

procedure TMPVCore.ClearAudioFilters;
begin
  SetAudioFilters([]);
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.SetHWDec(const AValue: Boolean);
var
  s: String;
begin
  if AValue <> FUseHWDec then
  begin
    FUseHWDec := AValue;
    if FUseHWDec then
      s := 'auto-safe'
    else
      s := 'no';

    s := 'hwdec=' + s;
    AddOption(s);
    mpv_set_option_string_(s);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVCore.ReceivedEvent(Sender: TObject; Event: Pmpv_event);
var
  PropName: PChar;
begin
  if Event = NIL then Exit;

  if Assigned(FOnEventReceived) then
    FOnEventReceived(Self, Event);

  case (Event^.event_id) of
    MPV_EVENT_LOG_MESSAGE:
      if (Event^.Data <> NIL) and Assigned(OnLogMessage) then
        OnLogMessage(Sender, Pmpv_event_log_message(Event^.Data)^.prefix, Pmpv_event_log_message(Event^.Data)^.level, Pmpv_event_log_message(Event^.Data)^.Text);

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
      if (Event^.Data <> NIL) and Assigned(OnEndFile) then
        with Pmpv_event_end_file(Event^.data)^ do OnEndFile(Sender, reason, error);

    MPV_EVENT_VIDEO_RECONFIG, MPV_EVENT_AUDIO_RECONFIG:
    begin
      GetTracks;
      if (Event^.event_id = MPV_EVENT_VIDEO_RECONFIG) and Assigned(OnVideoReconfig) then
        OnVideoReconfig(Sender)
      else if (Event^.event_id = MPV_EVENT_AUDIO_RECONFIG) and Assigned(OnAudioReconfig) then
        OnAudioReconfig(Sender);

      if Assigned(FOnTracksChanged) then FOnTracksChanged(Sender);
    end;

    MPV_EVENT_GET_PROPERTY_REPLY:
      if (Event^.Data <> NIL) and Assigned(OnGetReplyEvent) then
        OnGetReplyEvent(Sender, Event^.reply_userdata, Event^.error, Pmpv_event_property(Event^.Data));

    MPV_EVENT_SET_PROPERTY_REPLY:
      if Assigned(OnSetReplyEvent) then OnSetReplyEvent(Sender, Event^.reply_userdata, Event^.error);

    MPV_EVENT_COMMAND_REPLY:
      if (Event^.Data <> NIL) and Assigned(OnCommandReplyEvent) then
        OnCommandReplyEvent(Sender, Event^.reply_userdata, Event^.error, Pmpv_event_command(Event^.Data));

    MPV_EVENT_PROPERTY_CHANGE:
    begin
      if (Event^.Data = NIL) then Exit;
      PropName := Pmpv_event_property(Event^.Data)^.Name;
      if StrComp(PropName, 'eof-reached') = 0 then
      begin
        if (Pmpv_event_property(Event^.Data)^.data <> NIL) and (PInteger(Pmpv_event_property(Event^.Data)^.data)^ = 1) then
        begin
          mpv_set_pause(True);
          if Assigned(OnEndFile) then OnEndFile(Sender, MPV_END_FILE_REASON_EOF, 0);
        end;
      end
      else if StrComp(PropName, 'cache-buffering-state') = 0 then
      begin
        if Assigned(OnBuffering) and (Pmpv_event_property(Event^.Data)^.data <> NIL) then
          OnBuffering(Sender, PInteger(Pmpv_event_property(Event^.Data)^.data)^);
      end
      {$IFNDEF USETIMER}
      else if (StrComp(PropName, 'playback-time') = 0) and (Pmpv_event_property(Event^.Data)^.format = MPV_FORMAT_INT64) then
      begin
        if Assigned(OnTimeChanged) and (Pmpv_event_property(Event^.Data)^.data <> NIL) then
          OnTimeChanged(Sender, PInteger(Pmpv_event_property(Event^.Data)^.data)^);
      end;
      {$ENDIF}
    end;
  end;
end;

// -----------------------------------------------------------------------------

{$IFDEF USETIMER}
procedure TMPVCore.DoTimer(Sender: TObject);
var
  Pos: Integer;
begin
  FTimer.Enabled := False;

  if FInitialized and IsMediaLoaded then
  begin
    Pos := GetMediaPosInMs;
    if Assigned(FOnTimeChanged) and (FLastPos <> Pos) then
    begin
      FOnTimeChanged(Self, Pos);
      FLastPos := Pos;
    end;
  end;

  if FInitialized then
    FTimer.Enabled := True;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

{ TMPVPlayer }

// -----------------------------------------------------------------------------

constructor TMPVPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DoubleBuffered := True;
  Width := 320;
  Height := 240;
  TabStop := True;
  BevelOuter := bvNone;
  ParentBackground := False;
  ParentColor := False;
  Color := $101010;
  FullRepaint := False;
  Caption := '';

  FCore := TMPVCore.Create(Self);
  FAspectRatio := arDefault;
  FFontSize := 55;
  FLastFontSize := -1;
  FLastMarginX := -1;
  FLastMarginY := -1;
  FSafeMarginPercent := 10;
  FSafeZoneEnabled := False;
  FKeepAspect := True;
  FNoAudioDisplay := False;
  FRenderFail := rfNone;

  {$IFDEF WINDOWS}
  FRenderMode := rmEmbedding;
  {$ELSE}
  FRenderMode := rmOpenGL;
  {$ENDIF}

  FGL := NIL;
  FRenderGL := NIL;

  {$IFDEF ENABLE_BACKIMAGE}
  FBackImage := TPicture.Create;
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

destructor TMPVPlayer.Destroy;
begin
  {$IFDEF ENABLE_BACKIMAGE}
  FBackImage.Free;
  {$ENDIF}

  if FCore.Initialized and FCore.IsPlaying then
    FCore.mpv_command_(['stop']);

  if FRenderMode = rmOpenGL then
    UnInitializeRenderGL
  {$IFDEF SDL2}
  else if FRenderMode = rmSDL2 then
    UnInitializeRenderSDL
  {$ENDIF};

  FCore.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

{$IFDEF DARWIN}
procedure TMPVPlayer.CreateWnd;
begin
  inherited CreateWnd;
  NSView(Self.Handle).setWantsLayer(True);
end;
{$ENDIF}

// -----------------------------------------------------------------------------

function TMPVPlayer.GetWID: Int64;
{$IFDEF LINUX}
var
  Widget: PGtkWidget;
{$ENDIF}
begin
  {$IFDEF LINUX}
  Widget := PGtkWidget(Self.Handle);

  //if not gtk_widget_get_realized(Widget) then
  if (gtk_widget_get_flags(Widget) and GTK_REALIZED) = 0 then
    gtk_widget_realize(Widget);

  if Assigned(Widget) and Assigned(Widget^.window) then
    Result := Int64(GDK_WINDOW_XWINDOW(Widget^.window))
  else
    Result := 0;
  {$ELSE}
  Result := Int64(PtrUInt(Self.Handle)); // Windows(HWND) y macOS(NSView)
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.InitializePlayer: Boolean;
begin
  if not FKeepAspect then
    FCore.AddOption('keepaspect=no');

  if FNoAudioDisplay then
    FCore.AddOption('audio-display=no');

  {$IFDEF DARWIN}
  if (FRenderMode = rmEmbedding) and TestMetalSupport then
  begin
    FCore.AddOption('vo=gpu-next');
    FCore.AddOption('gpu-api=vulkan');
    FCore.AddOption('hwdec=auto');
  end
  else if FRenderMode = rmOpenGL then
    FCore.AddOption('vo=libmpv');
  {$ELSE}
  if (FRenderMode = rmOpenGL) then
    FCore.AddOption('vo=libmpv');
  {$ENDIF}
  FCore.AddOption('gpu-hwdec-interop=auto');

  {$IFDEF LINUX}
  if (FRenderMode = rmEmbedding) and IsWaylandSession then
    FRenderMode := rmOpenGL;
  {$ENDIF}

  SetVideoAspectRatio(FAspectRatio);
  Result := FCore.Initialize(GetWID);

  if Result then
  begin
    if FRenderMode = rmOpenGL then
    begin
      if not InitializeRenderGL then
      begin
        UnInitializeRenderGL;
        if FRenderFail = rfSwitchToEmbedding then
          FRenderMode := rmEmbedding;
      end;
    end
    {$IFDEF SDL2}
    else if FRenderMode = rmSDL2 then
    begin
      if not InitializeRenderSDL then
      begin
        UnInitializeRenderSDL;
        if FRenderFail = rfSwitchToEmbedding then
          FRenderMode := rmEmbedding;
      end;
    end
    {$ENDIF};
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Resize;
var
  VideoW, VideoH: Integer;
  EffectiveW, EffectiveH: Double;
  BlackBarX, BlackBarY: Double;
  TargetMarginXPx, TargetMarginYPx: Double;
  MarginMultiplier: Double;
  NewFontSize, NewMarginX, NewMarginY: Int64;
begin
  inherited Resize;

  if Assigned(FCore) and FCore.Initialized then
  begin
    VideoW := FCore.GetVideoWidth;
    VideoH := FCore.GetVideoHeight;

    if (VideoW > 0) and (VideoH > 0) and (ClientHeight > 0) then
    begin
      if (ClientWidth / ClientHeight) < (VideoW / VideoH) then
      begin
        EffectiveW := ClientWidth;
        EffectiveH := ClientWidth * (VideoH / VideoW);
        BlackBarX := 0;
        BlackBarY := (ClientHeight - EffectiveH) / 2.0;
      end
      else
      begin
        EffectiveW := ClientHeight * (VideoW / VideoH);
        EffectiveH := ClientHeight;
        BlackBarX := (ClientWidth - EffectiveW) / 2.0;
        BlackBarY := 0;
      end;

      MarginMultiplier := FSafeMarginPercent / 100.0;
      TargetMarginXPx := BlackBarX + (EffectiveW * MarginMultiplier);
      TargetMarginYPx := BlackBarY + (EffectiveH * MarginMultiplier);

      NewMarginX := Round(TargetMarginXPx * (720.0 / ClientHeight));
      NewMarginY := Round(TargetMarginYPx * (720.0 / ClientHeight));
      NewFontSize := Round(FFontSize * (EffectiveH / ClientHeight));

      if (NewFontSize <> FLastFontSize) or (NewMarginX <> FLastMarginX) or (NewMarginY <> FLastMarginY) then
      begin
        FCore.mpv_set_property_int64('osd-font-size', NewFontSize);
        FCore.mpv_set_property_int64('osd-margin-x', NewMarginX);
        FCore.mpv_set_property_int64('osd-margin-y', NewMarginY);
        FCore.mpv_set_property_int64('sub-font-size', NewFontSize);
        FCore.mpv_set_property_int64('sub-margin-x', NewMarginX);
        FCore.mpv_set_property_int64('sub-margin-y', NewMarginY);

        FLastFontSize := NewFontSize;
        FLastMarginX := NewMarginX;
        FLastMarginY := NewMarginY;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetRenderMode(const AValue: TMPVPlayerRenderMode);
begin
  if not FCore.Initialized and (FRenderMode <> AValue) then
    FRenderMode := AValue;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetFontSize(const AValue: Integer);
begin
  if FFontSize = AValue then Exit;
  if AValue < 10 then
    FFontSize := 10
  else if AValue > 200 then
    FFontSize := 200
  else
    FFontSize := AValue;

  if Assigned(FCore) and FCore.Initialized then Resize;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetSafeMarginPercent(const AValue: Byte);
begin
  if FSafeMarginPercent = AValue then Exit;

  if AValue > 50 then
    FSafeMarginPercent := 50
  else
    FSafeMarginPercent := AValue;

  if Assigned(FCore) and FCore.Initialized and FSafeZoneEnabled then Resize;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetSafeZoneEnabled(const AValue: Boolean);
begin
  if FSafeZoneEnabled = AValue then Exit;
  EnforceSubtitleSafeZone(AValue);
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

  if Assigned(FCore) then
    FCore.mpv_set_option_string_('video-aspect-override=' + s);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.CycleVideoAspectRatio: TMPVPlayerVideoAspectRatio;
var
  i: Integer;
begin
  i := Integer(FAspectRatio) + 1;
  if i > Integer(ar235_1) then
    i := 0;

  FAspectRatio := TMPVPlayerVideoAspectRatio(i);
  SetVideoAspectRatio(FAspectRatio);
  Result := FAspectRatio;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.EnforceSubtitleSafeZone(const AEnable: Boolean);
var
  LavfiFilter: String;
  TitleMargin, TitleBox: Double;
begin
  FSafeZoneEnabled := AEnable;
  if not Assigned(FCore) or not FCore.Initialized then Exit;

  if AEnable then
  begin
    TitleMargin := FSafeMarginPercent / 100.0;
    TitleBox := 1.0 - (TitleMargin * 2.0);
    FCore.mpv_command_(['vf', 'add', '@actionsafe:' + LavfiActionSafe]);
    LavfiFilter := Format(LavfiTitleSafe, [TitleMargin, TitleMargin, TitleBox, TitleBox], FCore.FFormatSettings);
    FCore.mpv_command_(['vf', 'add', '@titlesafe:' + LavfiFilter]);
    Resize;
  end
  else
  begin
    FCore.mpv_command_(['vf', 'remove', '@actionsafe']);
    FCore.mpv_command_(['vf', 'remove', '@titlesafe']);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.InitializeRenderGL: Boolean;
begin
  if Assigned(FRenderGL) or Assigned(FGL) then
      UnInitializeRenderGL;

  FGL := TUWOpenGLControl.Create(Self);
  FGL.Parent := Self;
  FGL.Align := alClient;
  FGL.OnClick := OnClick;
  FGL.OnMouseWheelUp := OnMouseWheelUp;
  FGL.OnMouseWheelDown := OnMouseWheelDown;
  FGL.OnPaint := @DoOnPaint;
  FGL.OnResize := @DoOnGLResize;
  FRenderGL := TMPVPlayerRenderGL.Create(FGL, FCore.mpv_handle {$IFDEF BGLCONTROLS}, FOnDrawEvent{$ENDIF});
  Result := FRenderGL.Active;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.UnInitializeRenderGL;
begin
  if Assigned(FRenderGL) then
    FreeAndNil(FRenderGL);

  if Assigned(FGL) then
  begin
    FreeAndNil(FGL);
    Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

{$IFDEF SDL2}
function TMPVPlayer.InitializeRenderSDL: Boolean;
begin
  if Assigned(FRenderSDL) then
     UnInitializeRenderSDL;

  FRenderSDL := TMPVPlayerRenderSDL.Create(Handle, FCore.mpv_handle);
  Result := FRenderSDL.Active;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.UnInitializeRenderSDL;
begin
  if Assigned(FRenderSDL) then FreeAndNil(FRenderSDL);
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure TMPVPlayer.DoOnPaint(Sender: TObject);
begin
  if Assigned(FRenderGL) and FCore.IsMediaLoaded and not FCore.IsPlaying then
    FRenderGL.Render(True);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.DoOnGLResize(Sender: TObject);
begin
  if Assigned(FRenderGL) then
    FRenderGL.UpdateThreadSize(FGL.ClientWidth, FGL.ClientHeight);
end;

// -----------------------------------------------------------------------------

{$IFDEF ENABLE_BACKIMAGE}
procedure TMPVPlayer.EraseBackground(DC: HDC);
const
  MAX_UI_SIZE = 300;
var
  FCanvas: TCanvas;
  R, aRect: TRect;
  scaledWidth, scaledHeight : Integer;
  ScaleFactor: Double;
  OldAntialias: TAntialiasingMode;
begin
  if Assigned(FCore) and FCore.Initialized then
  begin
    inherited;
    Exit;
  end;

  FCanvas := (Self as TCustomControl).Canvas;
  aRect := Rect(0, 0, Self.Width, Self.Height);

  if (FCanvas <> NIL) then
  begin
    if DC <> 0 then FCanvas.Handle := DC;
    FCanvas.Brush.Color := Color;
    FCanvas.FillRect(aRect);

    if not (csDesigning in ComponentState) then
    begin
      if Assigned(FBackImage) and (FBackImage.Width > 0) and (FBackImage.Height > 0) then
      begin
        ScaleFactor := Min(aRect.Width / FBackImage.Width, aRect.Height / FBackImage.Height);
        if ScaleFactor > 1 then ScaleFactor := 1;
        scaledWidth  := Round(FBackImage.Width * ScaleFactor);
        scaledHeight := Round(FBackImage.Height * ScaleFactor);

        if scaledWidth > MAX_UI_SIZE then
        begin
          ScaleFactor  := MAX_UI_SIZE / scaledWidth;
          scaledWidth  := Round(scaledWidth * ScaleFactor);
          scaledHeight := Round(scaledHeight * ScaleFactor);
        end;

        if scaledHeight > MAX_UI_SIZE then
        begin
          ScaleFactor  := MAX_UI_SIZE / scaledHeight;
          scaledWidth  := Round(scaledWidth * ScaleFactor);
          scaledHeight := Round(scaledHeight * ScaleFactor);
        end;

        R.Left := (aRect.Width - scaledWidth) div 2;
        R.Top := (aRect.Height - scaledHeight) div 2;
        R.Right := R.Left + scaledWidth;
        R.Bottom := R.Top + scaledHeight;

        OldAntialias := FCanvas.AntialiasingMode;
        FCanvas.AntialiasingMode := amOn;
        FCanvas.StretchDraw(R, FBackImage.Graphic);
        FCanvas.AntialiasingMode := OldAntialias;
      end;
    end
    else
      FCanvas.DrawFocusRect(aRect);

    if DC <> 0 then FCanvas.Handle := 0;
  end;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

function TMPVPlayer.GetAutoStartPlayback: Boolean;
begin
  Result := FCore.AutoStartPlayback;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetAutoStartPlayback(const AValue: Boolean);
begin
  FCore.AutoStartPlayback := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetAutoLoadSubtitle: Boolean;
begin
  Result := FCore.AutoLoadSubtitle;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetAutoLoadSubtitle(const AValue: Boolean);
begin
  FCore.AutoLoadSubtitle := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetUseHWDec: Boolean;
begin
  Result := FCore.UseHWDec;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetUseHWDec(const AValue: Boolean);
begin
  FCore.UseHWDec := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetLogLevel: TMPVPlayerLogLevel;
begin
  Result := FCore.LogLevel;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetLogLevel(const AValue: TMPVPlayerLogLevel);
begin
  FCore.LogLevel := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetStartOptions: TStringList;
begin
  Result := FCore.StartOptions;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetMPVFileName: String;
begin
  Result := FCore.MPVFileName;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetMPVFileName(const AValue: String);
begin
  FCore.MPVFileName := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetYTDLPFileName: String;
begin
  Result := FCore.YTDLPFileName;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetYTDLPFileName(const AValue: String);
begin
  FCore.YTDLPFileName := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetSMPTEMode: Boolean;
begin
  Result := FCore.SMPTEMode;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetSMPTEMode(const AValue: Boolean);
begin
  FCore.SMPTEMode := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnEventReceived: TMPVPlayerEventReceived;
begin
  Result := FCore.OnEventReceived;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnEventReceived(const AValue: TMPVPlayerEventReceived);
begin
  FCore.OnEventReceived := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnStartFile: TNotifyEvent;
begin
  Result := FCore.OnStartFile;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnStartFile(const AValue: TNotifyEvent);
begin
  FCore.OnStartFile := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnEndFile: TMPVPlayerEndFileEvent;
begin
  Result := FCore.OnEndFile;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnEndFile(const AValue: TMPVPlayerEndFileEvent);
begin
  FCore.OnEndFile := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnFileLoaded: TNotifyEvent;
begin
  Result := FCore.OnFileLoaded;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnFileLoaded(const AValue: TNotifyEvent);
begin
  FCore.OnFileLoaded := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnVideoReconfig: TNotifyEvent;
begin
  Result := FCore.OnVideoReconfig;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnVideoReconfig(const AValue: TNotifyEvent);
begin
  FCore.OnVideoReconfig := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnAudioReconfig: TNotifyEvent;
begin
  Result := FCore.OnAudioReconfig;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnAudioReconfig(const AValue: TNotifyEvent);
begin
  FCore.OnAudioReconfig := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnTracksChanged: TNotifyEvent;
begin
  Result := FCore.OnTracksChanged;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnTracksChanged(const AValue: TNotifyEvent);
begin
  FCore.OnTracksChanged := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnSeek: TMPVPlayerNotifyEvent;
begin
  Result := FCore.OnSeek;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnSeek(const AValue: TMPVPlayerNotifyEvent);
begin
  FCore.OnSeek := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnPlaybackRestart: TNotifyEvent;
begin
  Result := FCore.OnPlaybackRestart;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnPlaybackRestart(const AValue: TNotifyEvent);
begin
  FCore.OnPlaybackRestart := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnPlay: TNotifyEvent;
begin
  Result := FCore.OnPlay;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnPlay(const AValue: TNotifyEvent);
begin
  FCore.OnPlay := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnStop: TNotifyEvent;
begin
  Result := FCore.OnStop;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnStop(const AValue: TNotifyEvent);
begin
  FCore.OnStop := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnPause: TNotifyEvent;
begin
  Result := FCore.OnPause;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnPause(const AValue: TNotifyEvent);
begin
  FCore.OnPause := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnTimeChanged: TMPVPlayerNotifyEvent;
begin
  Result := FCore.OnTimeChanged;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnTimeChanged(const AValue: TMPVPlayerNotifyEvent);
begin
  FCore.OnTimeChanged := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnBuffering: TMPVPlayerNotifyEvent;
begin
  Result := FCore.OnBuffering;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnBuffering(const AValue: TMPVPlayerNotifyEvent);
begin
  FCore.OnBuffering := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnLogMessage: TMPVPlayerLogEvent;
begin
  Result := FCore.OnLogMessage;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnLogMessage(const AValue: TMPVPlayerLogEvent);
begin
  FCore.OnLogMessage := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnGetReplyEvent: TMPVPlayerGetReplyEvent;
begin
  Result := FCore.OnGetReplyEvent;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnGetReplyEvent(const AValue: TMPVPlayerGetReplyEvent);
begin
  FCore.OnGetReplyEvent := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnSetReplyEvent: TMPVPlayerSetReplyEvent;
begin
  Result := FCore.OnSetReplyEvent;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnSetReplyEvent(const AValue: TMPVPlayerSetReplyEvent);
begin
  FCore.OnSetReplyEvent := AValue;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetOnCommandReplyEvent: TMPVPlayerCommandReplyEvent;
begin
  Result := FCore.OnCommandReplyEvent;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetOnCommandReplyEvent(const AValue: TMPVPlayerCommandReplyEvent);
begin
  FCore.OnCommandReplyEvent := AValue;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

function TMPVPlayer.IsLibMPVAvailable: Boolean;
begin
  Result := FCore.IsLibMPVAvailable;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_command_(args: array of String; const reply_userdata: Integer = 0): mpv_error;
begin
  Result := FCore.mpv_command_(args, reply_userdata);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_command_node_(ANode: mpv_node; const reply_userdata: Integer = 0): mpv_error;
begin
  Result := FCore.mpv_command_node_(ANode, reply_userdata);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_abort_async_command_(const reply_userdata: Integer);
begin
  FCore.mpv_abort_async_command_(reply_userdata);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_set_option_string_(const AValue: String): Integer;
begin
  Result := FCore.mpv_set_option_string_(AValue);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_string_(const APropertyName: String; const reply_userdata: Integer = 0): String;
begin
  Result := FCore.mpv_get_property_string_(APropertyName, reply_userdata);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_string_(const APropertyName: String; const AValue: String; const reply_userdata: Integer = 0);
begin
  FCore.mpv_set_property_string_(APropertyName, AValue, reply_userdata);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_boolean(const APropertyName: String; const reply_userdata: Integer = 0): Boolean;
begin
  Result := FCore.mpv_get_property_boolean(APropertyName, reply_userdata);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_boolean(const APropertyName: String; const AValue: Boolean; const reply_userdata: Integer = 0);
begin
  FCore.mpv_set_property_boolean(APropertyName, AValue, reply_userdata);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_double(const APropertyName: String; const reply_userdata: Integer = 0): Double;
begin
  Result := FCore.mpv_get_property_double(APropertyName, reply_userdata);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_double(const APropertyName: String; const AValue: Double; const reply_userdata: Integer = 0);
begin
  FCore.mpv_set_property_double(APropertyName, AValue, reply_userdata);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_int64(const APropertyName: String; const reply_userdata: Integer = 0): Int64;
begin
  Result := FCore.mpv_get_property_int64(APropertyName, reply_userdata);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_int64(const APropertyName: String; const AValue: Int64; const reply_userdata: Integer = 0);
begin
  FCore.mpv_set_property_int64(APropertyName, AValue, reply_userdata);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_pause(const AValue: Boolean);
begin
  FCore.mpv_set_pause(AValue);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetErrorString: String;
begin
  Result := FCore.GetErrorString;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetVersionString: String;
begin
  Result := FCore.GetVersionString;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetPlayerHandle: Pmpv_handle;
begin
  Result := FCore.GetPlayerHandle;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Play(const AFileName: String; const AStartAtPositionMs: Integer = 0);
begin
  if not FCore.Initialized then InitializePlayer;
  FCore.Play(AFileName, AStartAtPositionMs);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Play(const AFromMs: Integer);
begin
  FCore.Play(AFromMs);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.AddOption(const AValue: String);
begin
  FCore.AddOption(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.RemoveOption(const AValue: String);
begin
  FCore.RemoveOption(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ShowOverlayText(const AText: String; const ATags: String = '{\an2}');
begin
  FCore.ShowOverlayText(AText, ATags);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ShowText(const AText: String; const ADuration: Integer = 1000; const ATags: String = '{\an7}');
begin
  FCore.ShowText(AText, ADuration, ATags);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTextColor(const AValue: String);
begin
  FCore.SetTextColor(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTextHAlign(const AValue: String);
begin
  FCore.SetTextHAlign(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTextVAlign(const AValue: String);
begin
  FCore.SetTextVAlign(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTextSize(const AValue: Int64);
begin
  FCore.SetTextSize(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTextFont(const AValue: String);
begin
  FCore.SetTextFont(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetSubtitleColor(const AValue: String);
begin
  FCore.SetSubtitleColor(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetSubtitleSize(const AValue: Int64);
begin
  FCore.SetSubtitleSize(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetSubtitleFont(const AValue: String);
begin
  FCore.SetSubtitleFont(AValue);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetFileName: String;
begin
  Result := FCore.FileName;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetTrackList: TMPVPlayerTrackList;
begin
  Result := FCore.FTrackList;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetError: mpv_error;
begin
  Result := FCore.FError;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetInitialized: Boolean;
begin
  Result := FCore.FInitialized;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Close(const AForce: Boolean = True);
begin
  if AForce then
  begin
    if FCore.Initialized and FCore.IsPlaying then
      FCore.mpv_command_(['stop']);

    if FRenderMode = rmOpenGL then
      UnInitializeRenderGL
    {$IFDEF SDL2}
    else if FRenderMode = rmSDL2 then
      UnInitializeRenderSDL
    {$ENDIF};
  end;

  FCore.Close(AForce);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Loop(const AStartTimeMs, BFinalTimeMs: Integer; const ALoopCount: Integer = -1);
begin
  FCore.Loop(AStartTimeMs, BFinalTimeMs, ALoopCount);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Pause;
begin
  FCore.Pause;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Resume(const AForcePlay: Boolean = False);
begin
  FCore.Resume(AForcePlay);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Stop;
begin
  FCore.Stop;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.IsMediaLoaded: Boolean;
begin
  Result := FCore.IsMediaLoaded;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.IsPlaying: Boolean;
begin
  Result := FCore.IsPlaying;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.IsPaused: Boolean;
begin
  Result := FCore.IsPaused;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetMediaLenInMs: Integer;
begin
  Result := FCore.GetMediaLenInMs;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetMediaPosInMs: Integer;
begin
  Result := FCore.GetMediaPosInMs;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetMediaPosInMs(const AValue: Integer);
begin
  FCore.SetMediaPosInMs(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SeekInMs(const MSecs: Integer; const SeekAbsolute: Boolean = True);
begin
  FCore.SeekInMs(MSecs, SeekAbsolute);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.NextFrame(const AStep: Integer = 1);
begin
  FCore.NextFrame(AStep);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.PreviousFrame(const AStep: Integer = 1);
begin
  FCore.PreviousFrame(AStep);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetPlaybackRate(const AValue: Byte);
begin
  FCore.SetPlaybackRate(AValue);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetAudioVolume: Byte;
begin
  Result := FCore.GetAudioVolume;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetAudioVolume(const AValue: Byte);
begin
  FCore.SetAudioVolume(AValue);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetAudioMute: Boolean;
begin
  Result := FCore.GetAudioMute;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetAudioMute(const AValue: Boolean);
begin
  FCore.SetAudioMute(AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer);
begin
  FCore.SetTrack(TrackType, ID);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetTrack(const Index: Integer);
begin
  FCore.SetTrack(Index);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.GetTracks;
begin
  FCore.GetTracks;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.HasVideoTrack: Boolean;
begin
  Result := FCore.HasVideoTrack;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.LoadTrack(const TrackType: TMPVPlayerTrackType; const AFileName: String);
begin
  FCore.LoadTrack(TrackType, AFileName);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.RemoveTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
begin
  FCore.RemoveTrack(TrackType, ID);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ReloadTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer = -1);
begin
  FCore.ReloadTrack(TrackType, ID);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetVideoWidth: Integer;
begin
  Result := FCore.GetVideoWidth;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetVideoHeight: Integer;
begin
  Result := FCore.GetVideoHeight;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetVideoTotalFrames: Integer;
begin
  Result := FCore.GetVideoTotalFrames;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetVideoFPS: Double;
begin
  Result := FCore.GetVideoFPS;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.GetScreenshotToBitmap(const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo): TBitmap;
begin
  Result := FCore.GetScreenshotToBitmap(AScreenshotMode);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ScreenshotToFile(const AFileName: String; const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);
begin
  FCore.ScreenshotToFile(AFileName, AScreenshotMode);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ScreenshotToClipboard(const AScreenshotMode: TMPVPlayerScreenshotMode = smVideo);
begin
  FCore.ScreenshotToClipboard(AScreenshotMode);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetVideoFilters(const AVideoFilters: TMPVPlayerVideoFilters);
begin
  FCore.SetVideoFilters(AVideoFilters);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ClearVideoFilters;
begin
  FCore.ClearVideoFilters;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.SetAudioFilters(const AAudioFilters: TMPVPlayerAudioFilters);
begin
  FCore.SetAudioFilters(AAudioFilters);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ClearAudioFilters;
begin
  FCore.ClearAudioFilters;
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('URUWorks Multimedia', [TMPVCore, TMPVPlayer]);
end;

// -----------------------------------------------------------------------------

initialization
  {$I MPVPlayer.lrs}

// -----------------------------------------------------------------------------

end.
