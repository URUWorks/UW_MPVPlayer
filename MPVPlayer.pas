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
  LMessages, LResources, LazarusPackageIntf, libMPV.Client, MPVPlayer.Thread,
  BGLVirtualScreen, libMPV.Render, libMPV.Render_gl, gl, glext, BGRAOpenGL,
  BGRABitmapTypes;

// -----------------------------------------------------------------------------

type

  { TMPVControl Types }

  TMPVPlayerSate        = (ssStop, ssPlay, ssPause, ssEnd);
  TMPVPlayerTrackType   = (ttVideo, ttAudio, ttSubtitle, ttUnknown);
  TMPVPlayerNotifyEvent = procedure(ASender: TObject; AParam: Integer) of object;

  TMPVPlayerTrackInfo = record
    Kind     : TMPVPlayerTrackType;
    ID       : Integer;
    Codec    : String;
    Lang     : String;
    Selected : Boolean;
  end;

  TMPVPlayerTrackList = array of TMPVPlayerTrackInfo;

  { TMPVPlayer }

  TMPVPlayer = class(TCustomBGLVirtualScreen)
  private
    FMPV_HANDLE   : Pmpv_handle;
    FError        : mpv_error;
    FVersion      : DWord;
    FInitialized  : Boolean;
    FStartOptions : TStringList;
    FMPVEvent     : TMPVPlayerThreadEvent;
    FState        : TMPVPlayerSate;
    FTrackList    : TMPVPlayerTrackList;
    FAutoStart    : Boolean;
    FAutoLoadSub  : Boolean;
    {$IFDEF USETIMER}
    FTimer        : TTimer;
    {$ENDIF}

    mpvRenderParams  : array of mpv_render_param;
    mpvOpenGLParams  : mpv_opengl_init_params;
    mpvRenderContext : pmpv_render_context;
    mpvfbo           : mpv_opengl_fbo;
    FGlEvent         : TMPVPlayerThreadEvent;
    FGlInitialized   : Boolean;

    FOnStartFile: TNotifyEvent;            // Notification before playback start of a file (before the file is loaded).
    FOnEndFile: TMPVPlayerNotifyEvent;     // Notification after playback end (after the file was unloaded), AParam is mpv_end_file_reason.
    FOnFileLoaded: TNotifyEvent;           // Notification when the file has been loaded (headers were read etc.)
    FOnVideoReconfig: TNotifyEvent;        // Happens after video changed in some way.
    FOnAudioReconfig: TNotifyEvent;        // Similar to VIDEO_RECONFIG.
    FOnSeek: TMPVPlayerNotifyEvent;        // Happens when a seek was initiated.
    FOnPlaybackRestart: TNotifyEvent;      // Usually happens on start of playback and after seeking.
    FOnTimeChanged: TMPVPlayerNotifyEvent; // Notify playback time, AParam is current position.

    FOnGlDraw : TBGLRedrawEvent;

    procedure PushEvent;
    procedure ReceivedEvent(Sender: TObject);

    procedure PushRenderEvent;
    procedure ReceivedRenderEvent(Sender: TObject);

    {$IFDEF USETIMER}
    procedure DoTimer(Sender: TObject);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Initialize: Boolean;
    procedure UnInitialize;

    function InitializeGl: Boolean;
    function UnInitializeGl: Boolean;

    procedure Update_mpvfbo;
    procedure DoOnPaint; override;

    procedure mpv_command_(Args: Array of const);
    procedure mpv_set_option_string_(const AValue: String);
    function mpv_get_property_boolean(const APropertyName: String): Boolean;
    procedure mpv_set_property_boolean(const APropertyName: String; const AValue: Boolean);
    function mpv_get_property_double(const AProperty: String): Double;
    procedure mpv_set_property_double(const AProperty: String; const AValue: Double);
    procedure mpv_set_property_int64(const AProperty: String; const AValue: Double);

    function GetErrorString: String;
    function GetVersionString: String;
    function GetPlayerHandle: Pmpv_handle;

    procedure Play(const AFileName: String);
    procedure Pause;
    procedure Resume;
    procedure Stop;
    function GetMediaLenInMs: Integer;
    function GetMediaPosInMs: Integer;
    procedure SetMediaPosInMs(const AValue: Integer);
    procedure SeekInMs(const MSecs: Integer; const SeekAbsolute: Boolean = True);
    procedure NextFrame;
    procedure PreviousFrame;
    procedure SetPlaybackRate(const AValue: Byte);
    function GetAudioVolume: Integer;
    procedure SetAudioVolume(const AValue: Integer);
    procedure SetMediaTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer); overload;
    procedure SetMediaTrack(const Index: Integer); overload;
    procedure GetMediaTracks;

    property mpv_handle    : Pmpv_handle         read FMPV_HANDLE;
    property Error         : mpv_error           read FError;
    property ErrorString   : String              read GetErrorString;
    property Version       : DWord               read FVersion;
    property VersionString : String              read GetVersionString;
    property Initialized   : Boolean             read FInitialized;
    property StartOptions  : TStringList         read FStartOptions;
    property TrackList     : TMPVPlayerTrackList read FTrackList;

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
    property OnElapse;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFramesPerSecond;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnLoadTextures;
    property OnUnloadTextures;
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
    property SmoothedElapse;

    property OnDraw: TBGLRedrawEvent Read FOnGlDraw Write FOnGlDraw;

    property AutoStartPlayback: Boolean read FAutoStart write FAutoStart;
    property AutoLoadSubtitle: Boolean read FAutoLoadSub write FAutoLoadSub;

    property OnStartFile: TNotifyEvent read FOnStartFile write FOnStartFile;
    property OnEndFile: TMPVPlayerNotifyEvent read FOnEndFile write FOnEndFile;
    property OnFileLoaded: TNotifyEvent read FOnFileLoaded write FOnFileLoaded;
    property OnVideoReconfig: TNotifyEvent read FOnVideoReconfig write FOnVideoReconfig;
    property OnAudioReconfig: TNotifyEvent read FOnAudioReconfig write FOnAudioReconfig;
    property OnSeek: TMPVPlayerNotifyEvent read FOnSeek write FOnSeek;
    property OnPlaybackRestart: TNotifyEvent read FOnPlaybackRestart write FOnPlaybackRestart;
    property OnTimeChanged: TMPVPlayerNotifyEvent read FOnTimeChanged write FOnTimeChanged;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

const
  LIBMPV_MAX_VOLUME = 100;
  glFlip: Longint   = 1;

// -----------------------------------------------------------------------------

{ libmpv wakeup_events }

// -----------------------------------------------------------------------------

procedure LIBMPV_EVENT(Sender: Pointer); cdecl;
begin
  if (Sender <> NIL) then TMPVPlayer(Sender).PushEvent;
end;

// -----------------------------------------------------------------------------

procedure LIBMPV_RENDER_EVENT(Sender: Pointer); cdecl;
begin
  if (Sender <> NIL) then TMPVPlayer(Sender).PushRenderEvent;
end;

// -----------------------------------------------------------------------------

function get_proc_address_mpv(ctx: Pointer; Name: PChar): Pointer; cdecl;
begin
  Result := GetProcAddress(LibGL, Name);

  if Result = NIL then
    Result := wglGetProcAddress(Name);
end;

// -----------------------------------------------------------------------------

{ Helpers}

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_command_(Args: Array of const);
var
  pArgs: array of PChar;
  i: Integer;
begin
  if Initialized and (High(Args) > 0) then
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
  if not Initialized then Exit;
  FError := mpv_get_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_FLAG, @p);
  Result := Boolean(p);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_boolean(const APropertyName: String; const AValue: Boolean);
var
  p: Integer;
begin
  if not Initialized then Exit;

  if AValue then
    p := 1
  else
    p := 0;

  FError := mpv_set_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_FLAG, @p);
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.mpv_get_property_double(const AProperty: String): Double;
begin
  if Initialized then
    FError := mpv_get_property(FMPV_HANDLE^, PChar(AProperty), MPV_FORMAT_DOUBLE, @Result)
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_double(const AProperty: String; const AValue: Double);
begin
  if Initialized then
    FError := mpv_set_property(FMPV_HANDLE^, PChar(AProperty), MPV_FORMAT_DOUBLE, @AValue);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.mpv_set_property_int64(const AProperty: String; const AValue: Double);
begin
  if Initialized then
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
  Caption          := '';

  // our control setup
  FMPV_HANDLE    := NIL;
  FVersion       := 0;
  FError         := 0;
  FInitialized   := False;
  FGlInitialized := False;
  FMPVEvent      := NIL;
  FOnGlDraw      := NIL;
  FState         := ssStop;
  FAutoStart     := True;
  FAutoLoadSub   := False;
  FStartOptions  := TStringList.Create;
  SetLength(FTrackList, 0);

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
    Add('vd-lavc-dr=no');    // fix possibles deadlock issues
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
  pHwnd: PtrUInt;
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

  for i := 0 to FStartOptions.Count-1 do
    mpv_set_option_string_(FStartOptions[i]);

  // Set our window handle (not necessary for OpenGl)
  {$IFDEF LINUX}
  pHwnd := GDK_WINDOW_XWINDOW(PGtkWidget(Self.Handle)^.window);
  {$ELSE}
  pHwnd := Self.Handle;
  {$ENDIF}
  FError := mpv_set_option(FMPV_HANDLE^, 'wid', MPV_FORMAT_INT64, @pHwnd);

  {$IFNDEF USETIMER}
  mpv_observe_property(FMPV_HANDLE^, 0, 'playback-time', MPV_FORMAT_INT64);
  {$ENDIF}

  FError := mpv_initialize(FMPV_HANDLE^);
  FError := mpv_request_log_messages(FMPV_HANDLE^, 'no');

  FMPVEvent := TMPVPlayerThreadEvent.Create;
  FMPVEvent.OnEvent := @ReceivedEvent;
  mpv_set_wakeup_callback(FMPV_HANDLE^, @LIBMPV_EVENT, Self);

  InitializeGl;

  FInitialized := True;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.UnInitialize;
begin
  if not Initialized then Exit;

  if Assigned(mpv_set_wakeup_callback) and Assigned(FMPV_HANDLE) then
    mpv_set_wakeup_callback(FMPV_HANDLE^, NIL, Self);

  if Assigned(FMPVEvent) then
  begin
    FMPVEvent.Free;
    FMPVEvent := NIL;
  end;

  UnInitializeGl;

  if Assigned(mpv_terminate_destroy) and Assigned(FMPV_HANDLE) then
    mpv_terminate_destroy(FMPV_HANDLE^);

  FMPV_HANDLE := NIL;
  SetLength(FTrackList, 0);
  FInitialized := False;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.InitializeGl: Boolean;
begin
  Result := False;
  FGlInitialized := False;

  mpvOpenGLParams.get_proc_address := @get_proc_address_mpv;
  mpvOpenGLParams.get_proc_address_ctx := NIL;

  // Initialize params
  SetLength(mpvRenderParams, 4);
  mpvRenderParams[0]._type := MPV_RENDER_PARAM_API_TYPE;
  mpvRenderParams[0].Data  := PChar(MPV_RENDER_API_TYPE_OPENGL);
  mpvRenderParams[1]._type := MPV_RENDER_PARAM_OPENGL_INIT_PARAMS;
  mpvRenderParams[1].Data  := @mpvOpenGLParams;
  mpvRenderParams[2]._type := MPV_RENDER_PARAM_ADVANCED_CONTROL;
  mpvRenderParams[2].Data  := @glFlip;
  mpvRenderParams[3]._type := MPV_RENDER_PARAM_INVALID;
  mpvRenderParams[3].Data  := NIL;

  if not Load_libMPV_Render then
  begin
    FError := -1;
    Exit;
  end;

  FError := mpv_render_context_create(mpvRenderContext, FMPV_HANDLE^, Pmpv_render_param(@mpvRenderParams[0]));
  if FError <> 0 then Exit;

  FGlEvent := TMPVPlayerThreadEvent.Create;
  FGlEvent.OnEvent := @ReceivedRenderEvent;

  mpv_render_context_set_update_callback(mpvRenderContext^, @LIBMPV_RENDER_EVENT, Self);

  // Update params
  Update_mpvfbo;
  SetLength(mpvRenderParams, 3);
  mpvRenderParams[0]._type := MPV_RENDER_PARAM_OPENGL_FBO;
  mpvRenderParams[0].Data  := @mpvfbo;
  mpvRenderParams[1]._type := MPV_RENDER_PARAM_FLIP_Y;
  mpvRenderParams[1].Data  := @glFlip;
  mpvRenderParams[2]._type := MPV_RENDER_PARAM_INVALID;
  mpvRenderParams[2].Data  := NIL;

  FGlInitialized := True;
  Result := FGlInitialized;
end;

// -----------------------------------------------------------------------------

function TMPVPlayer.UnInitializeGl: Boolean;
begin
  FGlInitialized := False;
  Result := False;

  mpv_render_context_set_update_callback(mpvRenderContext^, NIL, Self);

  if Assigned(FGlEvent) then
  begin
    FGlEvent.Free;
    FGlEvent := NIL;
  end;

  if Assigned(mpv_render_context_free) and  Assigned(mpvRenderContext) then
    mpv_render_context_free(mpvRenderContext^);

  SetLength(mpvRenderParams, 0);
  Free_libMPV_Render;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Update_mpvfbo;
begin
  mpvfbo.fbo := 0;
  mpvfbo.w   := ClientWidth;
  mpvfbo.h   := ClientHeight;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.DoOnPaint;
var
  ctx: TBGLContext;
begin
  if FGlInitialized and (FState = ssPlay) then Exit;

  ctx := PrepareBGLContext;

  if Color = clNone then
    BGLViewPort(ClientWidth, ClientHeight)
  else
  if Color = clDefault then
    BGLViewPort(ClientWidth, ClientHeight, ColorToBGRA(clWindow))
  else
    BGLViewPort(ClientWidth, ClientHeight, ColorToBGRA(Color));

  MakeCurrent;
  Update_mpvfbo;

  if (FState <> ssPlay) and FGlInitialized then
    mpv_render_context_render(mpvRenderContext^, Pmpv_render_param(@mpvRenderParams[0]));

  if Assigned(FOnGlDraw) then FOnGlDraw(Self, ctx);

  SwapBuffers;

  if FGlInitialized then
    mpv_render_context_report_swap(mpvRenderContext^);

  ReleaseBGLContext(ctx);
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

procedure TMPVPlayer.Play(const AFileName: String);
begin
  if not Initialized then
    Initialize;

  mpv_command_(['loadfile', AFileName]);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Pause;
begin
  if FState = ssPlay then
  begin
    mpv_set_property_boolean('pause', True);
    FState := ssPause;
  end
  else
  begin
    mpv_set_property_boolean('pause', False);
    FState := ssPlay;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Resume;
begin
  if FState = ssPause then
  begin
    mpv_set_property_boolean('pause', False);
    FState := ssPlay;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Stop;
begin
  if FState <> ssStop then
  begin
    mpv_set_property_boolean('pause', True);
    SetMediaPosInMs(0);
    FState := ssStop;
  end;
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

procedure TMPVPlayer.SetMediaTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer);
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

procedure TMPVPlayer.SetMediaTrack(const Index: Integer);
begin
  SetMediaTrack(TrackList[Index].Kind, TrackList[Index].ID);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.GetMediaTracks;
var
  Node, Map, Detail: mpv_node;
  i, j: integer;
  pc: PPChar;
  Value, Value2: String;
begin
  FError := mpv_get_property(FMPV_HANDLE^, 'track-list', MPV_FORMAT_NODE, @Node);
  if FError = MPV_ERROR_SUCCESS then
  begin
    try
      SetLength(FTrackList, Node.u.list^.num);
      for i := 0 to Node.u.list^.num -1 do
      begin
        map := Node.u.list^.values^[i];
        pc  := map.u.list^.keys;
        FillByte(FTrackList[i], SizeOf(TMPVPlayerTrackInfo), 0);

        for j := 0 to map.u.list^.num -1 do
        begin
          Detail := map.u.list^.values^[j];
          Value  := StrPas(pc^);
          if Value = 'id' then
            FTrackList[i].Id := Detail.u.int64_;

          if Value = 'type' then
          begin
            Value2 := Detail.u._string;
            if Value2 = 'audio' then
              FTrackList[i].Kind := ttAudio
            else if Value2 = 'video' then
              FTrackList[i].Kind := ttVideo
            else if Value2 = 'sub' then
              FTrackList[i].Kind := ttSubtitle
            else
              FTrackList[i].Kind := ttUnknown;
          end;

          if Value = 'lang' then
            FTrackList[i].Lang := StrPas(Detail.u._string);

          if Value = 'codec' then
            FTrackList[i].Codec := StrPas(Detail.u._string);
          if Value = 'selected' then
            FTrackList[i].Selected := Detail.u.flag = 1;

          Inc(pc);
        end;
      end;
    except
    end;
    mpv_free_node_contents(Node);
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
        Break;
      end;

      MPV_EVENT_START_FILE:
        if Assigned(OnStartFile) then OnStartFile(Sender);

      MPV_EVENT_FILE_LOADED:
      begin
        if FAutoStart then
          FState := ssPlay
        else
          FState := ssPause;
        {$IFDEF USETIMER}
        FTimer.Enabled := True;
        {$ENDIF}
        if Assigned(OnFileLoaded) then OnFileLoaded(Sender);
      end;

      MPV_EVENT_SEEK:
        if Assigned(OnSeek) then OnSeek(Sender, GetMediaPosInMs);

      MPV_EVENT_END_FILE:
      begin
        FState := ssEnd;
        {$IFDEF USETIMER}
        FTimer.Enabled := False;
        {$ENDIF}
        if Assigned(OnEndFile) then OnEndFile(Sender, Integer(Event^.data^));
      end;

      MPV_EVENT_AUDIO_RECONFIG:
      begin
        GetMediaTracks;
        if Assigned(OnAudioReconfig) then OnAudioReconfig(Sender);
      end;

      {$IFNDEF USETIMER}
      MPV_EVENT_PROPERTY_CHANGE:
        if (Pmpv_event_property(Event^.Data)^.Name = 'playback-time') and (Pmpv_event_property(Event^.Data)^.format = MPV_FORMAT_INT64) then
        begin
          if Assigned(OnTimeChanged) then
            OnTimeChanged(Sender, GetMediaPosInMs);
        end;
      {$ENDIF}
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.PushRenderEvent;
begin
  if Assigned(FGlEvent) then FGlEvent.PushEvent;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.ReceivedRenderEvent(Sender: TObject);
var
  ctx : TBGLContext;
begin
  ctx := PrepareBGLContext;
  BGLViewPort(ClientWidth, ClientHeight);

  while ((mpv_render_context_update(mpvRenderContext^) and MPV_RENDER_UPDATE_FRAME) <> 0) do
  begin
    MakeCurrent;
    Update_mpvfbo;

    mpv_render_context_render(mpvRenderContext^, Pmpv_render_param(@mpvRenderParams[0]));
    if Assigned(FOnGlDraw) then FOnGlDraw(Self, ctx);
    SwapBuffers;
    mpv_render_context_report_swap(mpvRenderContext^);
  end;

  ReleaseBGLContext(ctx);
end;

// -----------------------------------------------------------------------------

{$IFDEF USETIMER}
procedure TMPVPlayer.DoTimer(Sender: TObject);
begin
  if Assigned(OnTimeChanged) then OnTimeChanged(Sender, GetMediaPosInMs);
end;
{$ENDIF}

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

