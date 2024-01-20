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
 *}

unit MPVPlayer.RenderGL;

// -----------------------------------------------------------------------------

{$I MPVPlayer.inc}

interface

uses
  Classes, SysUtils, libMPV.Client, libMPV.Render, libMPV.Render_gl, gl, glext,
  OpenGLContext
  {$IFDEF BGLCONTROLS}, BGRAOpenGL{$ENDIF};

// -----------------------------------------------------------------------------

type

  { TUWOpenGLControl }

  TUWOpenGLControl = class(TOpenGLControl)
  public
    procedure Paint; override;
  end;

  {$IFDEF BGLCONTROLS}
  TMPVPlayerDrawEvent = procedure (Sender: TObject; ABGLCanvas: TBGLCustomCanvas) of object;
  {$ENDIF}

  TMPVPlayerRenderGL = class;

  { TMPVPlayerRenderThread }

  TMPVPlayerRenderThread = class(TThread)
  private
    FGL              : TUWOpenGLControl;
    FError           : mpv_error;
    FCS              : TRTLCriticalSection;
    mpvHandle        : Pmpv_handle;
    mpvRenderParams  : array of mpv_render_param;
    mpvUpdateParams  : array of mpv_render_param;
    mpvOpenGLParams  : mpv_opengl_init_params;
    mpvRenderContext : pmpv_render_context;
    mpvfbo           : mpv_opengl_fbo;
    function InitializeRenderContext: Boolean;
    procedure UnInitializeRenderContext;
    function IsDestroyingGL: Boolean;
    procedure Update_mpvfbo;
  protected
    procedure TerminatedSet; override;
  public
    Owner : TMPVPlayerRenderGL;
    Event : PRTLEvent;
    IsRenderActive : Boolean;
    ForceInvalidateContext : Boolean;
    Ready : Boolean;
    {$IFDEF BGLCONTROLS}
    FDrawCallback: TMPVPlayerDrawEvent;
    {$ENDIF}
    constructor Create(AControl: TUWOpenGLControl; AHandle: Pmpv_handle; AOwner: TMPVPlayerRenderGL {$IFDEF BGLCONTROLS}; ADrawCallback: TMPVPlayerDrawEvent = NIL{$ENDIF});
    destructor Destroy; override;
    procedure Execute; override;
    procedure InvalidateContext;
  end;

  { TMPVPlayerRenderGL }

  TMPVPlayerRenderGL = class
  private
    FThread : TMPVPlayerRenderThread;
    function GetRenderActive: Boolean;
  public
    constructor Create(AControl: TUWOpenGLControl; AHandle: Pmpv_handle {$IFDEF BGLCONTROLS}; ADrawCallback: TMPVPlayerDrawEvent = NIL{$ENDIF});
    destructor Destroy; override;
    procedure Terminate;
    procedure Render(const ForceInvalidate: Boolean = False);

    property Active : Boolean read GetRenderActive;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TUWOpenGLControl }

// -----------------------------------------------------------------------------

procedure TUWOpenGLControl.Paint;
begin
  if Assigned(OnPaint) then
    OnPaint(Self);
end;

// -----------------------------------------------------------------------------

{ libmpv render wakeup_events }

// -----------------------------------------------------------------------------

function get_proc_address_mpv(ctx: Pointer; Name: PChar): Pointer; cdecl;
begin
  Result := GetProcAddress(LibGL, Name);

  if Result = NIL then
    Result := wglGetProcAddress(Name);
end;

// -----------------------------------------------------------------------------

procedure LIBMPV_RENDER_EVENT(Sender: Pointer); cdecl;
begin
  if (Sender <> NIL) then TMPVPlayerRenderGL(Sender).Render;
end;

// -----------------------------------------------------------------------------

{ TMPVPlayerRenderThread }

// -----------------------------------------------------------------------------

constructor TMPVPlayerRenderThread.Create(AControl: TUWOpenGLControl; AHandle: Pmpv_handle; AOwner: TMPVPlayerRenderGL {$IFDEF BGLCONTROLS}; ADrawCallback: TMPVPlayerDrawEvent = NIL{$ENDIF});
begin
  inherited Create(True);

  FreeOnTerminate := True;
  Event     := RTLEventCreate;
  Owner     := AOwner;
  mpvHandle := AHandle;
  {$IFDEF BGLCONTROLS}
  FDrawCallback := ADrawCallback;
  {$ENDIF}
  FGL := AControl;
  FGL.ReleaseContext;

  IsRenderActive         := False;
  ForceInvalidateContext := False;
  Ready                  := False;
  mpvRenderContext       := NIL;
end;

// -----------------------------------------------------------------------------

destructor TMPVPlayerRenderThread.Destroy;
begin
  UnInitializeRenderContext;
  RTLEventDestroy(Event);
  Owner := NIL;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.TerminatedSet;
begin
  IsRenderActive := False;
  ForceInvalidateContext := False;
  if Assigned(Event) then RTLEventSetEvent(Event);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.Execute;
begin
  if not InitializeRenderContext then
    Exit;

  while not Terminated do
  begin
    RTLEventWaitFor(Event);

    if ForceInvalidateContext then
    begin
      ForceInvalidateContext := False;
      InvalidateContext;
    end
    else
      while IsRenderActive and ((mpv_render_context_update(mpvRenderContext^) and MPV_RENDER_UPDATE_FRAME) <> 0) do
      begin
        InvalidateContext;
        mpv_render_context_report_swap(mpvRenderContext^);
      end;

    RTLEventResetEvent(Event);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVPlayerRenderThread.InitializeRenderContext: Boolean;
begin
  Result := False;
  try
    mpvOpenGLParams.get_proc_address := @get_proc_address_mpv;
    mpvOpenGLParams.get_proc_address_ctx := NIL;

    // Initialize params
    SetLength(mpvRenderParams, 4);
    mpvRenderParams[0]._type := MPV_RENDER_PARAM_API_TYPE;
    mpvRenderParams[0].Data  := PChar(MPV_RENDER_API_TYPE_OPENGL);
    mpvRenderParams[1]._type := MPV_RENDER_PARAM_OPENGL_INIT_PARAMS;
    mpvRenderParams[1].Data  := @mpvOpenGLParams;
    mpvRenderParams[2]._type := MPV_RENDER_PARAM_ADVANCED_CONTROL;
    mpvRenderParams[2].Data  := @MPV_RENDER_PARAM_ADVANCED_CONTROL_ENABLED;
    mpvRenderParams[3]._type := MPV_RENDER_PARAM_INVALID;
    mpvRenderParams[3].Data  := NIL;

    FGL.MakeCurrent();
    FError := mpv_render_context_create(mpvRenderContext, mpvHandle^, Pmpv_render_param(@mpvRenderParams[0]));
    if FError <> MPV_ERROR_SUCCESS then Exit;

    mpv_render_context_set_update_callback(mpvRenderContext^, @LIBMPV_RENDER_EVENT, Owner);

    // Update params
    Update_mpvfbo;
    SetLength(mpvUpdateParams, 3);
    mpvUpdateParams[0]._type := MPV_RENDER_PARAM_OPENGL_FBO;
    mpvUpdateParams[0].Data  := @mpvfbo;
    mpvUpdateParams[1]._type := MPV_RENDER_PARAM_FLIP_Y;
    mpvUpdateParams[1].Data  := @MPV_RENDER_PARAM_ADVANCED_CONTROL_ENABLED;
    mpvUpdateParams[2]._type := MPV_RENDER_PARAM_INVALID;
    mpvUpdateParams[2].Data  := NIL;

    InitCriticalSection(FCS);

    IsRenderActive := True;
    Result := True;
  finally
    Ready := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.UnInitializeRenderContext;
begin
  if Assigned(mpvRenderContext) then
  begin
    mpv_render_context_set_update_callback(mpvRenderContext^, NIL, NIL);
    mpv_render_context_free(mpvRenderContext^);
    mpvRenderContext := NIL;
  end;

  DoneCriticalSection(FCS);

  SetLength(mpvRenderParams, 0);
  SetLength(mpvUpdateParams, 0);
  UnInitialize_libMPV_Render;
end;

// -----------------------------------------------------------------------------

function TMPVPlayerRenderThread.IsDestroyingGL: Boolean;
begin
  Result := (csDestroying in FGL.ComponentState);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.Update_mpvfbo;
begin
  mpvfbo.internal_format := 0;
  mpvfbo.fbo := 0;
  mpvfbo.w   := FGL.ClientWidth;
  mpvfbo.h   := FGL.ClientHeight;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.InvalidateContext;
begin
  Update_mpvfbo;
  if not Terminated and IsRenderActive then
  begin
    EnterCriticalSection(FCS);
    try
      if not IsDestroyingGL then FGL.MakeCurrent();
      mpv_render_context_render(mpvRenderContext^, Pmpv_render_param(@mpvUpdateParams[0]));
      {$IFDEF BGLCONTROLS}
      if Assigned(FDrawCallback) then
      begin
        BGLViewPort(FGL.ClientWidth, FGL.ClientHeight);
        FDrawCallback(Self, BGLCanvas);
      end;
      {$ENDIF}
      if IsRenderActive and not IsDestroyingGL then FGL.SwapBuffers;
    finally
      LeaveCriticalSection(FCS);
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ TMPVPlayerRenderGL }

// -----------------------------------------------------------------------------

constructor TMPVPlayerRenderGL.Create(AControl: TUWOpenGLControl; AHandle: Pmpv_handle {$IFDEF BGLCONTROLS}; ADrawCallback: TMPVPlayerDrawEvent = NIL{$ENDIF});
begin
  if Initialize_libMPV_Render(hLibMPV) then
  begin
    FThread := TMPVPlayerRenderThread.Create(AControl, AHandle, Self {$IFDEF BGLCONTROLS}, ADrawCallback{$ENDIF});
    FThread.Start;
    while not FThread.Ready do Sleep(100);
  end;
end;

// -----------------------------------------------------------------------------

destructor TMPVPlayerRenderGL.Destroy;
begin
  Terminate;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.Terminate;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    {$IFDEF WINDOWS}
    FThread.WaitFor;
    {$ENDIF}
    FThread := NIL;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.Render(const ForceInvalidate: Boolean = False);
begin
  if Assigned(FThread) and FThread.IsRenderActive then
  begin
    FThread.ForceInvalidateContext := ForceInvalidate;
    RTLEventSetEvent(FThread.Event);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVPlayerRenderGL.GetRenderActive: Boolean;
begin
  if Assigned(FThread) then
    Result := FThread.IsRenderActive
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

end.

