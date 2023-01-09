{*
 *  URUWorks libMPV
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

unit UWlibMPV;

// -----------------------------------------------------------------------------

{$I UWlibMPV.inc}

interface

uses
  Classes, Controls, SysUtils, LazFileUtils, ExtCtrls, Graphics, LCLType,
  LMessages, LResources, LazarusPackageIntf, libMPV.Client, UWlibMPV.Thread
  {$IFDEF SDL2}, sdl2, libMPV.Render, libMPV.Render_gl, ctypes{$ENDIF};

// -----------------------------------------------------------------------------

type

  { TMPVControl Types }

  TUWLibMPVSate        = (ssStop, ssPlay, ssPause, ssEnd);
  TUWLibMPVTrackType   = (ttVideo, ttAudio, ttSubtitle, ttUnknown);
  TUWLibMPVNotifyEvent = procedure(ASender: TObject; AParam: Integer) of object;

  TUWLibMPVTrackInfo = record
    Kind     : TUWLibMPVTrackType;
    ID       : Integer;
    Codec    : String;
    Lang     : String;
    Selected : Boolean;
  end;

  TUWLibMPVTrackList = array of TUWLibMPVTrackInfo;

  { TUWlibMPV }

  TUWLibMPV = class(TCustomPanel)
  private
    FMPV_HANDLE   : Pmpv_handle;
    FError        : mpv_error;
    FVersion      : DWord;
    FInitialized  : Boolean;
    FStartOptions : TStringList;
    FMPVEvent     : TUWlibMPVThreadEvent;
    FState        : TUWLibMPVSate;
    FTrackList    : TUWLibMPVTrackList;
    FAutoStart    : Boolean;
    FAutoLoadSub  : Boolean;
    {$IFDEF USETIMER}
    FTimer        : TTimer;
    {$ENDIF}

    {$IFDEF SDL2}
    sdlError         : Integer;
    sdlWindow        : PSDL_Window;
    sdlGLContext     : TSDL_GLContext;
    mpvRenderParams  : array of mpv_render_param;
    mpvOpenGLParams  : mpv_opengl_init_params;
    mpvRenderContext : pmpv_render_context;
    sdlEvent         : TUWlibMPVThreadEvent;
    sdlInitialized   : Boolean;
    {$ENDIF}

    FOnStartFile: TNotifyEvent;           // Notification before playback start of a file (before the file is loaded).
    FOnEndFile: TUWLibMPVNotifyEvent;     // Notification after playback end (after the file was unloaded), AParam is mpv_end_file_reason.
    FOnFileLoaded: TNotifyEvent;          // Notification when the file has been loaded (headers were read etc.)
    FOnVideoReconfig: TNotifyEvent;       // Happens after video changed in some way.
    FOnAudioReconfig: TNotifyEvent;       // Similar to VIDEO_RECONFIG.
    FOnSeek: TUWLibMPVNotifyEvent;        // Happens when a seek was initiated.
    FOnPlaybackRestart: TNotifyEvent;     // Usually happens on start of playback and after seeking.
    FOnTimeChanged: TUWLibMPVNotifyEvent; // Notify playback time, AParam is current position.

    procedure PushEvent;
    procedure ReceivedEvent(Sender: TObject);
    {$IFDEF SDL2}
    procedure PushSdlEvent;
    procedure ReceivedSdlEvent(Sender: TObject);
    {$ENDIF}

    {$IFDEF USETIMER}
    procedure DoTimer(Sender: TObject);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Initialize: Boolean;
    procedure UnInitialize;

    {$IFDEF SDL2}
    function InitializeSDL2: Boolean;
    function UnInitializeSDL2: Boolean;
    {$ENDIF}

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
    procedure SetMediaTrack(const TrackType: TUWLibMPVTrackType; const ID: Integer); overload;
    procedure SetMediaTrack(const Index: Integer); overload;
    procedure GetMediaTracks;

    property mpv_handle    : Pmpv_handle        read FMPV_HANDLE;
    property Error         : mpv_error          read FError;
    property ErrorString   : String             read GetErrorString;
    property Version       : DWord              read FVersion;
    property VersionString : String             read GetVersionString;
    property Initialized   : Boolean            read FInitialized;
    property StartOptions  : TStringList        read FStartOptions;
    property TrackList     : TUWLibMPVTrackList read FTrackList;
    {$IFDEF SDL2}
    property ErrorSDL      : Integer            read sdlError;
    {$ENDIF}

    {$IFDEF USETIMER}
    property Timer         : TTimer read FTimer;
    {$ENDIF}
  published
    property Align;
    property Anchors;
    property Color default clBlack;
    property Width default 320;
    property Height default 240;
    property Constraints;
    property DragKind;
    property DragMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
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
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property TabOrder;
    property TabStop default True;

    property AutoStartPlayback: Boolean read FAutoStart write FAutoStart;
    property AutoLoadSubtitle: Boolean read FAutoLoadSub write FAutoLoadSub;

    property OnStartFile: TNotifyEvent read FOnStartFile write FOnStartFile;
    property OnEndFile: TUWLibMPVNotifyEvent read FOnEndFile write FOnEndFile;
    property OnFileLoaded: TNotifyEvent read FOnFileLoaded write FOnFileLoaded;
    property OnVideoReconfig: TNotifyEvent read FOnVideoReconfig write FOnVideoReconfig;
    property OnAudioReconfig: TNotifyEvent read FOnAudioReconfig write FOnAudioReconfig;
    property OnSeek: TUWLibMPVNotifyEvent read FOnSeek write FOnSeek;
    property OnPlaybackRestart: TNotifyEvent read FOnPlaybackRestart write FOnPlaybackRestart;
    property OnTimeChanged: TUWLibMPVNotifyEvent read FOnTimeChanged write FOnTimeChanged;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

const
   LIBMPV_MAX_VOLUME = 100;
{$IFDEF SDL2}
  glFlip: Longint = 1;

var
  sdlRenderEventId: cuint32;
{$ENDIF}

// -----------------------------------------------------------------------------

{ libmpv wakeup_events }

// -----------------------------------------------------------------------------

procedure LIBMPV_EVENT(Sender: Pointer); cdecl;
begin
  if (Sender <> NIL) then TUWlibMPV(Sender).PushEvent;
end;

// -----------------------------------------------------------------------------

{$IFDEF SDL2}
procedure LIBMPV_RENDER_EVENT(Sender: Pointer); cdecl;
var
  Event: PSDL_Event;
begin
  if (Sender <> NIL) then
  begin
    New(Event);
    Event^.type_:= sdlRenderEventId;
    SDL_PushEvent(Event);
    TUWlibMPV(Sender).PushSdlEvent;
  end;
end;

// -----------------------------------------------------------------------------

function get_proc_address_mpv(ctx: Pointer; Name: PChar): Pointer; cdecl;
begin
  Result := SDL_GL_GetProcAddress(Name);
end;
{$ENDIF}

// -----------------------------------------------------------------------------

{ Helpers}

// -----------------------------------------------------------------------------

procedure TUWlibMPV.mpv_command_(Args: Array of const);
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

procedure TUWlibMPV.mpv_set_option_string_(const AValue: String);
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

function TUWLibMPV.mpv_get_property_boolean(const APropertyName: String): Boolean;
var
  p: Integer;
begin
  Result := False;
  if not Initialized then Exit;
  FError := mpv_get_property(FMPV_HANDLE^, PChar(APropertyName), MPV_FORMAT_FLAG, @p);
  Result := Boolean(p);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.mpv_set_property_boolean(const APropertyName: String; const AValue: Boolean);
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

function TUWlibMPV.mpv_get_property_double(const AProperty: String): Double;
begin
  if Initialized then
    FError := mpv_get_property(FMPV_HANDLE^, PChar(AProperty), MPV_FORMAT_DOUBLE, @Result)
  else
    Result := 0;
end;

// -----------------------------------------------------------------------------

procedure TUWlibMPV.mpv_set_property_double(const AProperty: String; const AValue: Double);
begin
  if Initialized then
    FError := mpv_set_property(FMPV_HANDLE^, PChar(AProperty), MPV_FORMAT_DOUBLE, @AValue);
end;

// -----------------------------------------------------------------------------

procedure TUWlibMPV.mpv_set_property_int64(const AProperty: String; const AValue: Double);
begin
  if Initialized then
    FError := mpv_set_property(FMPV_HANDLE^, PChar(AProperty), MPV_FORMAT_INT64, @AValue);
end;

// -----------------------------------------------------------------------------

{ TUWlibMPV }

// -----------------------------------------------------------------------------

constructor TUWlibMPV.Create(AOwner: TComponent);
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
  FMPV_HANDLE   := NIL;
  FVersion      := 0;
  FError        := 0;
  FInitialized  := False;
  FMPVEvent     := NIL;
  FState        := ssStop;
  FAutoStart    := True;
  FAutoLoadSub  := False;
  FStartOptions := TStringList.Create;
  SetLength(FTrackList, 0);

  {$IFDEF USETIMER}
  FTimer          := TTimer.Create(NIL);
  FTimer.Enabled  := False;
  FTimer.Interval := 75;
  FTimer.OnTimer  := @DoTimer;
  {$ENDIF}

  {$IFDEF SDL2}
  sdlRenderEventId := -1;
  sdlInitialized   := False;
  {$ENDIF}

  with FStartOptions do
  begin
    Add('hwdec=auto');       // enable best hw decoder
    Add('keep-open=always'); // don't auto close video
  end;
end;

// -----------------------------------------------------------------------------

destructor TUWlibMPV.Destroy;
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

function TUWlibMPV.Initialize: Boolean;
var
  i: Integer;
  pHwnd: PtrUInt;
begin
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

  // Set our window handle
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

  FMPVEvent := TUWlibMPVThreadEvent.Create;
  FMPVEvent.OnEvent := @ReceivedEvent;
  mpv_set_wakeup_callback(FMPV_HANDLE^, @LIBMPV_EVENT, Self);

  {$IFDEF SDL2}
  InitializeSDL2;
  {$ENDIF}

  FInitialized := True;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TUWlibMPV.UnInitialize;
begin
  if not Initialized then Exit;

  if Assigned(mpv_set_wakeup_callback) and Assigned(FMPV_HANDLE) then
    mpv_set_wakeup_callback(FMPV_HANDLE^, NIL, Self);

  {$IFDEF SDL2}
  UnInitializeSDL2;
  {$ENDIF}

  if Assigned(FMPVEvent) then
  begin
    FMPVEvent.Free;
    FMPVEvent := NIL;
  end;

  if Assigned(mpv_terminate_destroy) and Assigned(FMPV_HANDLE) then
    mpv_terminate_destroy(FMPV_HANDLE^);

  FMPV_HANDLE := NIL;
  SetLength(FTrackList, 0);
  FInitialized := False;
end;

// -----------------------------------------------------------------------------

{$IFDEF SDL2}
function TUWlibMPV.InitializeSDL2: Boolean;
begin
  Result   := False;
  sdlError := 0;
  sdlEvent := NIL;

  SDL_SetHint(SDL_HINT_NO_SIGNAL_HANDLERS, 'no');

  // initilization of video subsystem
  sdlError := SDL_Init(SDL_INIT_VIDEO);
  if sdlError < 0 then // 0 on success and a negative error code on failure.
    Exit;

  SDL_SetHint(SDL_HINT_VIDEO_FOREIGN_WINDOW_OPENGL, '1'); // let SDL know that a foreign window will be used with OpenGL
  sdlWindow := SDL_CreateWindowFrom(Pointer(Self.Handle));
  if sdlWindow = NIL then // failed to create SDL window
  begin
    sdlError := -1;
    Exit;
  end;

  sdlGLContext := SDL_GL_CreateContext(sdlWindow);
  if @sdlGLContext = NIL then
  begin
    sdlError := -3;
    Exit;
  end;

  mpvOpenGLParams.get_proc_address := @get_proc_address_mpv;
  mpvOpenGLParams.get_proc_address_ctx := NIL;

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
    sdlError := -4;
    Exit;
  end;

  sdlError := mpv_render_context_create(mpvRenderContext, FMPV_HANDLE^, Pmpv_render_param(@mpvRenderParams[0]));
  if sdlError <> 0 then Exit;

  sdlEvent := TUWlibMPVThreadEvent.Create;
  sdlEvent.OnEvent := @ReceivedSdlEvent;

  sdlRenderEventId := SDL_RegisterEvents(1);
  mpv_render_context_set_update_callback(mpvRenderContext^, @LIBMPV_RENDER_EVENT, Self);

  sdlInitialized := True;
  Result := sdlInitialized;
end;

// -----------------------------------------------------------------------------

function TUWlibMPV.UnInitializeSDL2: Boolean;
begin
  sdlInitialized := False;
  Result := False;

  if Assigned(mpv_render_context_free) and  Assigned(mpvRenderContext) then
    mpv_render_context_free(mpvRenderContext^);

  SetLength(mpvRenderParams, 0);
  Free_libMPV_Render;

  if Assigned(sdlEvent) then
  begin
    sdlEvent.Free;
    sdlEvent := NIL;
  end;

  SDL_GL_DeleteContext(sdlGLContext);
  SDL_DestroyWindow(sdlWindow);

  //closing SDL2
  SDL_Quit;

  sdlRenderEventId := -1;
  Result := True;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

function TUWlibMPV.GetPlayerHandle: Pmpv_handle;
begin
  Result := FMPV_HANDLE;
end;

// -----------------------------------------------------------------------------

function TUWlibMPV.GetErrorString: String;
begin
  if Assigned(mpv_error_string) then
    Result := mpv_error_string(FError)
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function TUWlibMPV.GetVersionString: String;
begin
  Result := Format('libmpv %d.%d', [FVersion shr 16, FVersion and $FF]);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.Play(const AFileName: String);
begin
  if not Initialized then
    Initialize;

  mpv_command_(['loadfile', AFileName]);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.Pause;
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

procedure TUWLibMPV.Resume;
begin
  if FState = ssPause then
  begin
    mpv_set_property_boolean('pause', False);
    FState := ssPlay;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.Stop;
begin
  if FState <> ssStop then
  begin
    mpv_set_property_boolean('pause', True);
    SetMediaPosInMs(0);
    FState := ssStop;
  end;
end;

// -----------------------------------------------------------------------------

function TUWLibMPV.GetMediaLenInMs: Integer;
begin
  Result := Trunc(mpv_get_property_double('duration') * 1000);
end;

// -----------------------------------------------------------------------------

function TUWLibMPV.GetMediaPosInMs: Integer;
begin
  Result := Trunc(mpv_get_property_double('time-pos') * 1000);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetMediaPosInMs(const AValue: Integer);
begin
  mpv_set_property_double('time-pos', AValue / 1000);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SeekInMs(const MSecs: Integer; const SeekAbsolute: Boolean = True);
begin
  if SeekAbsolute then
    SetMediaPosInMs(MSecs)
  else
    SetMediaPosInMs(GetMediaPosInMs + MSecs);
end;

// -----------------------------------------------------------------------------
procedure TUWLibMPV.NextFrame;
begin
  mpv_command_(['frame-step']);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.PreviousFrame;
begin
  mpv_command_(['frame-back-step']);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetPlaybackRate(const AValue: Byte);
begin
  mpv_set_property_double('speed', AValue / 100.0);
end;

// -----------------------------------------------------------------------------

function TUWLibMPV.GetAudioVolume: Integer;
begin
  Result := Trunc(mpv_get_property_double('volume') * (255 / LIBMPV_MAX_VOLUME));
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetAudioVolume(const AValue: Integer);
begin
  mpv_set_property_double('volume', AValue * (LIBMPV_MAX_VOLUME / 255));
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.SetMediaTrack(const TrackType: TUWLibMPVTrackType; const ID: Integer);
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

procedure TUWLibMPV.SetMediaTrack(const Index: Integer);
begin
  SetMediaTrack(TrackList[Index].Kind, TrackList[Index].ID);
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.GetMediaTracks;
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
        FillByte(FTrackList[i], SizeOf(TUWLibMPVTrackInfo), 0);

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

procedure TUWLibMPV.PushEvent;
begin
  if Assigned(FMPVEvent) then FMPVEvent.PushEvent;
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.ReceivedEvent(Sender: TObject);
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

{$IFDEF SDL2}
procedure TUWLibMPV.PushSdlEvent;
begin
  if Assigned(sdlEvent) then sdlEvent.PushEvent;
end;

// -----------------------------------------------------------------------------

procedure TUWLibMPV.ReceivedSdlEvent(Sender: TObject);
var
  Event  : PSDL_Event;
  mpvfbo : mpv_opengl_fbo;
  redraw : Boolean;
  flags  : uint64;
  params : array of mpv_render_param;
begin
  SetLength(params, 3);
  redraw := False;
  New(Event);
  try
    while SDL_WaitEvent(Event) = 1 do
    begin
      case Event^.type_ of
        SDL_QUITEV:
        begin
          UnInitialize;
          Break;
        end;

        SDL_WINDOWEVENT:
        begin
          if Event^.type_ = SDL_WINDOWEVENT_EXPOSED then // Window has been exposed and should be redrawn
            redraw := True
          else
            Break;
        end;
      else
        if Event^.type_ = sdlRenderEventId then // Happens when there is new work for the render thread (such as rendering a new video frame or redrawing it).
        begin
          flags := mpv_render_context_update(mpvRenderContext^);
          if (flags and MPV_RENDER_UPDATE_FRAME) <> 0 then
            redraw := True;
        end;
      end;

      if redraw then // redraw sdl window
      begin
        mpvfbo.fbo := 0;
        mpvfbo.w   := Self.Width;
        mpvfbo.h   := Self.Height;

        params[0]._type := MPV_RENDER_PARAM_OPENGL_FBO;
        params[0].Data  := @mpvfbo;
        params[1]._type := MPV_RENDER_PARAM_FLIP_Y;
        params[1].Data  := @glFlip;
        params[2]._type := MPV_RENDER_PARAM_INVALID;
        params[2].Data  := NIL;

        mpv_render_context_render(mpvRenderContext^, Pmpv_render_param(@params[0]));
        SDL_GL_SwapWindow(sdlWindow);

        Break;
      end;
    end;
  finally
    Dispose(Event);
    SetLength(params, 0);
  end;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

{$IFDEF USETIMER}
procedure TUWLibMPV.DoTimer(Sender: TObject);
begin
  if Assigned(OnTimeChanged) then OnTimeChanged(Sender, GetMediaPosInMs);
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure RegisterUWCompUnit;
begin
  RegisterComponents('URUWorks Multimedia', [TUWLibMPV]);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterUnit('UWlibMPV', @RegisterUWCompUnit);
end;

// -----------------------------------------------------------------------------

initialization
  {$I UWlibMPV.lrs}

// -----------------------------------------------------------------------------

end.

