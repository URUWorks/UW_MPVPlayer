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
  Classes, SysUtils, LCLType, sdl2lib, libMPV.Client, libMPV.Render;

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
    mpvRenderContext : pmpv_render_context;
    sdlWindow        : PSDL_Window;
    sdlRenderer      : PSDL_Renderer;
    sdlTexture       : PSDL_Texture;
    sdlTexSize       : array[0..1] of Integer;
    procedure InvalidateContext;
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
  Event := RTLEventCreate;
  FHandle := ACtrlHandle;
  mpvHandle := AMPVHandle;
  Owner := AOwner;
  mpvRenderContext := NIL;
  IsRenderActive := False;
  ForceInvalidateContext := False;
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
        InvalidateContext;

    RTLEventResetEvent(Event);
  end;
end;

// -----------------------------------------------------------------------------

function TMPVPlayerRenderThread.InitializeRenderContext: Boolean;
begin
  Result := False;

  SDL2LIB_Initialize;
  SDL_SetHint(SDL_HINT_NO_SIGNAL_HANDLERS, 'no');

  // Initilization of video subsystem
  FError := SDL_Init(SDL_INIT_VIDEO);
  if FError < 0 then // 0 on success and a negative error code on failure.
    Exit;

  sdlWindow := SDL_CreateWindowFrom(Pointer(FHandle));
  if sdlWindow = NIL then // Failed to create SDL window
  begin
    FError := -1;
    Exit;
  end;

  sdlRenderer := SDL_CreateRenderer(sdlWindow, -1, 0);
  if @sdlRenderer = NIL then
  begin
    FError := -2;
    Exit;
  end;

  // Initialize params
  SetLength(mpvRenderParams, 3);
  mpvRenderParams[0]._type := MPV_RENDER_PARAM_API_TYPE;
  mpvRenderParams[0].Data  := PChar(MPV_RENDER_API_TYPE_SW);
  mpvRenderParams[1]._type := MPV_RENDER_PARAM_ADVANCED_CONTROL;
  mpvRenderParams[1].Data  := @MPV_RENDER_PARAM_ADVANCED_CONTROL_ENABLED;
  mpvRenderParams[2]._type := MPV_RENDER_PARAM_INVALID;
  mpvRenderParams[2].Data  := NIL;

  FError := mpv_render_context_create(mpvRenderContext, mpvHandle^, Pmpv_render_param(@mpvRenderParams[0]));
  if FError <> MPV_ERROR_SUCCESS then Exit;

  SetLength(mpvUpdateParams, 5);
  mpvUpdateParams[0]._type := MPV_RENDER_PARAM_SW_SIZE;
  mpvUpdateParams[1]._type := MPV_RENDER_PARAM_SW_FORMAT;
  mpvUpdateParams[1].Data  := PChar('0bgr');
  mpvUpdateParams[2]._type := MPV_RENDER_PARAM_SW_STRIDE;
  mpvUpdateParams[3]._type := MPV_RENDER_PARAM_SW_POINTER;
  mpvUpdateParams[4]._type := MPV_RENDER_PARAM_INVALID;
  mpvUpdateParams[4].Data  := NIL;

  mpv_render_context_set_update_callback(mpvRenderContext^, @LIBMPV_RENDER_EVENT, Owner);

  sdlTexture := NIL;
  sdlTexSize[0] := 0;
  sdlTexSize[1] := 0;

  IsRenderActive := True;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.UnInitializeRenderContext;
begin
  IsRenderActive := False;

  if Assigned(mpvRenderContext) then
  begin
    mpv_render_context_set_update_callback(mpvRenderContext^, NIL, NIL);
    mpv_render_context_free(mpvRenderContext^);
  end;

  SDL_DestroyTexture(sdlTexture);
  SDL_DestroyRenderer(sdlRenderer);
  SDL_DestroyWindow(sdlWindow);

  SDL_Quit;
  SDL2LIB_Finalize;

  SetLength(mpvRenderParams, 0);
  SetLength(mpvUpdateParams, 0);
  Free_libMPV_Render;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.InvalidateContext;
var
  w, h : Integer;
  pixels : Pointer;
  pitch : size_t;
begin
  if not Terminated and IsRenderActive then
  begin
    SDL_GetWindowSize(sdlWindow, @w, @h);
    if not Assigned(sdlTexture) or (sdlTexSize[0] <> w) or (sdlTexSize[1] <> h) then
    begin
      SDL_DestroyTexture(sdlTexture);
      sdlTexture := SDL_CreateTexture(sdlRenderer, SDL_PIXELFORMAT_RGBX8888, SDL_TEXTUREACCESS_STREAMING, w, h);
      sdlTexSize[0] := w;
      sdlTexSize[1] := h;
    end;

    SDL_LockTexture(sdlTexture, NIL, @pixels, @pitch);

    mpvUpdateParams[0].Data := @sdlTexSize[0];
    mpvUpdateParams[2].Data := @pitch;
    mpvUpdateParams[3].Data := pixels;

    FError := mpv_render_context_render(mpvRenderContext^, Pmpv_render_param(@mpvUpdateParams[0]));

    SDL_UnlockTexture(sdlTexture);
    SDL_RenderCopy(sdlRenderer, sdlTexture, NIL, NIL);
    SDL_RenderPresent(sdlRenderer);
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

