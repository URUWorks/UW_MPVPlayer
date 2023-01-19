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

unit MPVPlayer.RenderGL;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, libMPV.Client, libMPV.Render, libMPV.Render_gl, gl, glext,
  OpenGLContext;

// -----------------------------------------------------------------------------

type

  TMPVPlayerRenderGL = class;

  { TMPVPlayerRenderThread }

  TMPVPlayerRenderThread = class(TThread)
  private
    FGL              : TOpenGLControl;
    FError           : mpv_error;
    mpvHandle        : Pmpv_handle;
    mpvRenderParams  : array of mpv_render_param;
    mpvUpdateParams  : array of mpv_render_param;
    mpvOpenGLParams  : mpv_opengl_init_params;
    mpvRenderContext : pmpv_render_context;
    mpvfbo           : mpv_opengl_fbo;
    function InitializeRenderContext: Boolean;
    procedure UnInitializeRenderContext;
    procedure Update_mpvfbo;
  protected
    procedure TerminatedSet; override;
  public
    Owner: TMPVPlayerRenderGL;
    Event: PRtlEvent;
    IsRenderActive: Boolean;
    constructor Create(AControl: TOpenGLControl; AHandle: Pmpv_handle; AOwner: TMPVPlayerRenderGL);
    destructor Destroy; override;
    procedure Execute; override;
    procedure RefreshContext;
  end;

  { TMPVPlayerRenderGL }

  TMPVPlayerRenderGL = class
  private
    FThread: TMPVPlayerRenderThread;
  public
    constructor Create(AControl: TOpenGLControl; AHandle: Pmpv_handle);
    destructor Destroy; override;
    procedure Render;
  end;

// -----------------------------------------------------------------------------

implementation

const
  glEnabled  : Longint = 1;
  glDisabled : Longint = 0;

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

constructor TMPVPlayerRenderThread.Create(AControl: TOpenGLControl; AHandle: Pmpv_handle; AOwner: TMPVPlayerRenderGL);
begin
  inherited Create(True);

  FreeOnTerminate := True;
  Event     := RTLEventCreate;
  Owner     := AOwner;
  mpvHandle := AHandle;
  FGL       := AControl;
  FGL.ReleaseContext;

  IsRenderActive  := False;
  mpvRenderParams := NIL;
  mpvUpdateParams := NIL;
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
  if Assigned(Event) then RTLEventSetEvent(Event);
  inherited TerminatedSet;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.Execute;
begin
  if not InitializeRenderContext then
  begin
    Owner.FThread := NIL;
    Exit;
  end;

  while not Terminated do
  begin
    RTLEventWaitFor(Event);

    while ((mpv_render_context_update(mpvRenderContext^) and MPV_RENDER_UPDATE_FRAME) <> 0) and not Terminated do
    begin
      mpv_render_context_report_swap(mpvRenderContext^);
      RefreshContext;
    end;

    RTLEventResetEvent(Event);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVPlayerRenderThread.InitializeRenderContext: Boolean;
begin
  Result := False;

  mpvOpenGLParams.get_proc_address := @get_proc_address_mpv;
  mpvOpenGLParams.get_proc_address_ctx := NIL;

  // Initialize params
  SetLength(mpvRenderParams, 4);
  mpvRenderParams[0]._type := MPV_RENDER_PARAM_API_TYPE;
  mpvRenderParams[0].Data  := PChar(MPV_RENDER_API_TYPE_OPENGL);
  mpvRenderParams[1]._type := MPV_RENDER_PARAM_OPENGL_INIT_PARAMS;
  mpvRenderParams[1].Data  := @mpvOpenGLParams;
  mpvRenderParams[2]._type := MPV_RENDER_PARAM_ADVANCED_CONTROL;
  mpvRenderParams[2].Data  := @glEnabled;
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
  mpvUpdateParams[1].Data  := @glEnabled;
  mpvUpdateParams[2]._type := MPV_RENDER_PARAM_INVALID;
  mpvUpdateParams[2].Data  := NIL;

  IsRenderActive := True;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.UnInitializeRenderContext;
begin
  if Assigned(mpvRenderContext) then
    mpv_render_context_set_update_callback(mpvRenderContext^, NIL, NIL);

  if Assigned(mpv_render_context_free) and Assigned(mpvRenderContext) then
    mpv_render_context_free(mpvRenderContext^);

  SetLength(mpvRenderParams, 0);
  SetLength(mpvUpdateParams, 0);
  Free_libMPV_Render;
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

procedure TMPVPlayerRenderThread.RefreshContext;
begin
  Update_mpvfbo;
  mpv_render_context_render(mpvRenderContext^, Pmpv_render_param(@mpvUpdateParams[0]));
  if IsRenderActive then FGL.SwapBuffers;
end;

// -----------------------------------------------------------------------------

{ TMPVPlayerRenderGL }

// -----------------------------------------------------------------------------

constructor TMPVPlayerRenderGL.Create(AControl: TOpenGLControl; AHandle: Pmpv_handle);
begin
  FThread := TMPVPlayerRenderThread.Create(AControl, AHandle, Self);
  if Load_libMPV_Render then FThread.Start;
end;

// -----------------------------------------------------------------------------

destructor TMPVPlayerRenderGL.Destroy;
begin
  if Assigned(FThread) then FThread.Terminate;
  FThread := NIL;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.Render;
begin
  if Assigned(FThread) and (FThread.IsRenderActive) then
    RTLEventSetEvent(FThread.Event);
end;

// -----------------------------------------------------------------------------

end.

