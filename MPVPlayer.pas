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
  LResources, LazarusPackageIntf, OpenGLContext, libMPV.Client, MPVPlayer.Thread,
  MPVPlayer.RenderGL;

// -----------------------------------------------------------------------------

type

  { TMPVPlayer Types }

  TMPVPlayerRenderMode  = (rmWindow, rmEmbedding, rmOpenGL);
  TMPVPlayerSate        = (psStop, psPlay, psPause, psEnd);
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

  TMPVPlayer = class(TCustomPanel)
  private
    FMPV_HANDLE   : Pmpv_handle;
    FError        : mpv_error;
    FVersion      : DWord;
    FGL           : TOpenGLControl;
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
    FRenderMode   : TMPVPlayerRenderMode;
    FRenderGL     : TMPVPlayerRenderGL;

    FOnStartFile: TNotifyEvent;            // Notification before playback start of a file (before the file is loaded).
    FOnEndFile: TMPVPlayerNotifyEvent;     // Notification after playback end (after the file was unloaded), AParam is mpv_end_file_reason.
    FOnFileLoaded: TNotifyEvent;           // Notification when the file has been loaded (headers were read etc.)
    FOnVideoReconfig: TNotifyEvent;        // Happens after video changed in some way.
    FOnAudioReconfig: TNotifyEvent;        // Similar to VIDEO_RECONFIG.
    FOnSeek: TMPVPlayerNotifyEvent;        // Happens when a seek was initiated.
    FOnPlaybackRestart: TNotifyEvent;      // Usually happens on start of playback and after seeking.
    FOnTimeChanged: TMPVPlayerNotifyEvent; // Notify playback time, AParam is current position.
    FOnPlay: TNotifyEvent;                 // Play by user
    FOnStop: TNotifyEvent;                 // Stop by user
    FOnPause: TNotifyEvent;                // Pause by user

    function Initialize: Boolean;
    procedure UnInitialize;

    procedure InitializeRenderGL;
    procedure UnInitializeRenderGL;

    procedure PushEvent;
    procedure ReceivedEvent(Sender: TObject);

    procedure SetRenderMode(Value: TMPVPlayerRenderMode);

    {$IFDEF USETIMER}
    procedure DoTimer(Sender: TObject);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

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

    procedure Play(const AFileName: String);
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
    procedure SetMediaTrack(const TrackType: TMPVPlayerTrackType; const ID: Integer); overload;
    procedure SetMediaTrack(const Index: Integer); overload;
    procedure GetMediaTracks;
    procedure ShowText(const AText: String; Duration: Integer = 0; FontSize: Integer = 0);
    procedure SetTextColor(const AValue: String);
    procedure SetTextHAlign(const AValue: String);
    procedure SetTextVAlign(const AValue: String);
    procedure SetTextSize(const AValue: Int64);

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
    property RendererMode: TMPVPlayerRenderMode read FRenderMode write SetRenderMode;

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
  FState         := psStop;
  FAutoStart     := True;
  FAutoLoadSub   := False;
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

  if FRenderMode = rmEmbedding then
  begin
    // Set our window handle (not necessary for OpenGl)
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

  FError := mpv_initialize(FMPV_HANDLE^);
  FError := mpv_request_log_messages(FMPV_HANDLE^, 'no');

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
  FInitialized := False;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.InitializeRenderGL;
begin
  FGL := TOpenGLControl.Create(Self);
  FGL.Parent := Self;
  FGL.Align := alClient;
  //FGL.AutoResizeViewport := True;
  //FGL.MultiSampling := 4;

  FRenderGL := TMPVPlayerRenderGL.Create(FGL, FMPV_HANDLE);
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

procedure TMPVPlayer.Play(const AFileName: String);
begin
  if not FInitialized then
    Initialize;

  mpv_command_(['loadfile', AFileName]);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayer.Pause;
begin
  if IsPlaying then
  begin
    mpv_set_property_boolean('pause', True);
    FState := psPause;
    if Assigned(FOnPlay) then FOnPlay(Self);
  end
  else
  begin
    mpv_set_property_boolean('pause', False);
    FState := psPlay;
    if Assigned(FOnPause) then FOnPause(Self);
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

procedure TMPVPlayer.ShowText(const AText: String; Duration: Integer = 0; FontSize: Integer = 0);
begin
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
          FState := psPlay
        else
          FState := psPause;
        {$IFDEF USETIMER}
        FTimer.Enabled := True;
        {$ENDIF}
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

