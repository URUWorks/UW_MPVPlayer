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
 *}

unit MPVPlayer.RenderSDL;

// -----------------------------------------------------------------------------

{$I MPVPlayer.inc}

interface

uses
  Classes, SysUtils, LCLType, sdl2lib, libMPV.Client, libMPV.Render,
  libMPV.Render_gl, ctypes, MPVPlayer.Thread;

// -----------------------------------------------------------------------------

type

  { TMPVPlayerRenderSDL }

  TMPVPlayerRenderSDL = class;

  { TMPVPlayerRenderThread }

  TMPVPlayerRenderThread = class(TThread)
  private
    FHandle          : HWND;
    FError           : mpv_error;
    mpvHandle        : Pmpv_handle;
    mpvRenderParams  : array of mpv_render_param;
    mpvUpdateParams  : array of mpv_render_param;
    mpvOpenGLParams  : mpv_opengl_init_params;
    mpvRenderContext : pmpv_render_context;
    mpvfbo           : mpv_opengl_fbo;
    sdlWindow        : PSDL_Window;
    sdlGLContext     : TSDL_GLContext;
    procedure Update_mpvfbo;
  protected
    procedure TerminatedSet; override;
  public
    Owner : TMPVPlayerRenderSDL;
    Event : PRTLEvent;
    IsRenderActive : Boolean;
    ForceInvalidateContext : Boolean;
    constructor Create(ACtrlHandle: HWND; AMPVHandle: Pmpv_handle; AOwner: TMPVPlayerRenderSDL);
    destructor Destroy; override;
    procedure Execute; override;
    procedure InvalidateContext;

    function InitializeRenderContext: Boolean;
    procedure UnInitializeRenderContext;
  end;

  { TMPVPlayerRenderSDL }

  TMPVPlayerRenderSDL = class
  private
    FThread : TMPVPlayerRenderThread;
    function GetRenderActive: Boolean;
  public
    constructor Create(AMPVFileName: String; ACtrlHandle: HWND; AMPVHandle: Pmpv_handle);
    destructor Destroy; override;
    procedure Render(const ForceInvalidate: Boolean = False);

    property Active : Boolean read GetRenderActive;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ libmpv render wakeup_events }

// -----------------------------------------------------------------------------

function get_proc_address_mpv(ctx: Pointer; Name: PChar): Pointer; cdecl;
begin
  Result := SDL_GL_GetProcAddress(Name);
end;

// -----------------------------------------------------------------------------

procedure LIBMPV_RENDER_EVENT(Sender: Pointer); cdecl;
begin
  if (Sender <> NIL) then TMPVPlayerRenderSDL(Sender).Render;
end;

// -----------------------------------------------------------------------------

{ TMPVPlayerRenderThread }

// -----------------------------------------------------------------------------

constructor TMPVPlayerRenderThread.Create(ACtrlHandle: HWND; AMPVHandle: Pmpv_handle; AOwner: TMPVPlayerRenderSDL);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  Event     := RTLEventCreate;
  FHandle   := ACtrlHandle;
  mpvHandle := AMPVHandle;
  Owner     := AOwner;

  IsRenderActive         := False;
  ForceInvalidateContext := False;
  mpvRenderParams        := NIL;
  mpvUpdateParams        := NIL;
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
  inherited TerminatedSet;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.Execute;
begin
  while not Terminated and IsRenderActive do
  begin
    RTLEventWaitFor(Event);

    if ForceInvalidateContext then
    begin
      ForceInvalidateContext := False;
      InvalidateContext;
    end
    else
      while ((mpv_render_context_update(mpvRenderContext^) and MPV_RENDER_UPDATE_FRAME) <> 0) do
        Synchronize(@InvalidateContext); //InvalidateContext;

    RTLEventResetEvent(Event);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVPlayerRenderThread.InitializeRenderContext: Boolean;
const
  SDL_HINT_VIDEO_FOREIGN_WINDOW_OPENGL = 'SDL_VIDEO_FOREIGN_WINDOW_OPENGL';

begin
  Result := False;

  SDL2LIB_Initialize;
  SDL_SetHint(SDL_HINT_NO_SIGNAL_HANDLERS, 'no');

  // Initilization of video subsystem
  FError := SDL_Init(SDL_INIT_VIDEO);
  if FError < 0 then // 0 on success and a negative error code on failure.
    Exit;

  SDL_SetHint(SDL_HINT_VIDEO_FOREIGN_WINDOW_OPENGL, '1'); // Let SDL know that a foreign window will be used with OpenGL
  sdlWindow := SDL_CreateWindowFrom(Pointer(FHandle));
  if sdlWindow = NIL then // Failed to create SDL window
  begin
    FError := -1;
    Exit;
  end;

  sdlGLContext := SDL_GL_CreateContext(sdlWindow);
  if @sdlGLContext = NIL then
  begin
    FError := -3;
    Exit;
  end;

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

  //SDL_GL_MakeCurrent(sdlWindow, sdlGLContext);
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

  IsRenderActive := True;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.UnInitializeRenderContext;
begin
  IsRenderActive := False;

  if Assigned(mpvRenderContext) then
    mpv_render_context_set_update_callback(mpvRenderContext^, NIL, NIL);

  if Assigned(mpv_render_context_free) and Assigned(mpvRenderContext) then
    mpv_render_context_free(mpvRenderContext^);

  SDL_GL_DeleteContext(sdlGLContext);
  SDL_DestroyWindow(sdlWindow);

  SDL_Quit;
  SDL2LIB_Finalize;

  SetLength(mpvRenderParams, 0);
  SetLength(mpvUpdateParams, 0);
  Free_libMPV_Render;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.Update_mpvfbo;
var
  w, h : Integer;
begin
  SDL_GetWindowSize(sdlWindow, @w, @h);
  mpvfbo.internal_format := 0;
  mpvfbo.fbo := 0;
  mpvfbo.w   := w;
  mpvfbo.h   := h;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.InvalidateContext;
begin
  Update_mpvfbo;
  if not Terminated and IsRenderActive then
  begin
    //SDL_GL_MakeCurrent(sdlWindow, sdlGLContext);
    mpv_render_context_render(mpvRenderContext^, Pmpv_render_param(@mpvUpdateParams[0]));
    SDL_GL_SwapWindow(sdlWindow);
    mpv_render_context_report_swap(mpvRenderContext^);
  end;
end;

// -----------------------------------------------------------------------------

{ TMPVPlayerRenderSDL }

// -----------------------------------------------------------------------------

constructor TMPVPlayerRenderSDL.Create(AMPVFileName: String; ACtrlHandle: HWND; AMPVHandle: Pmpv_handle);
begin
  FThread := TMPVPlayerRenderThread.Create(ACtrlHandle, AMPVHandle, Self);
  if Load_libMPV_Render(AMPVFileName) and FThread.InitializeRenderContext then
    FThread.Start;
end;

// -----------------------------------------------------------------------------

destructor TMPVPlayerRenderSDL.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.IsRenderActive := False;
    FThread.Terminate;
    FThread := NIL;
  end;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderSDL.Render(const ForceInvalidate: Boolean = False);
begin
  if Assigned(FThread) and (FThread.IsRenderActive) then
  begin
    FThread.ForceInvalidateContext := ForceInvalidate;
    RTLEventSetEvent(FThread.Event);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVPlayerRenderSDL.GetRenderActive: Boolean;
begin
  if Assigned(FThread) then
    Result := FThread.IsRenderActive
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

end.

