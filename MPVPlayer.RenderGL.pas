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
 *  Copyright (C) 2021-2026 URUWorks, uruworks@gmail.com.
 *}

unit MPVPlayer.RenderGL;

// -----------------------------------------------------------------------------

{$I MPVPlayer.inc}

interface

uses
  Classes, SysUtils, libMPV.Client, libMPV.Render, libMPV.Render_gl, gl, glext,
  OpenGLContext
  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}, dynlibs, xlib, glx{$ENDIF}
  {$IFDEF LCLGTK3}, LazGLib2, LazGObject2, LazGtk3, gtk3widgets{$ENDIF}
  {$IFDEF BGLCONTROLS}, BGRAOpenGL{$ENDIF}
  {$IFDEF DARWIN}, CocoaAll{$ENDIF};

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
    FGL: TUWOpenGLControl;
    FWidth: Integer;
    FHeight: Integer;
    FError: mpv_error;
    mpvHandle: Pmpv_handle;
    mpvRenderParams: array of mpv_render_param;
    mpvUpdateParams: array of mpv_render_param;
    mpvOpenGLParams: mpv_opengl_init_params;
    mpvRenderContext: pmpv_render_context;
    mpvfbo: mpv_opengl_fbo;
    ReadyEvent: PRTLEvent;
    function InitializeRenderContext: Boolean;
    procedure UnInitializeRenderContext;
    function IsDestroyingGL: Boolean;
    procedure Update_mpvfbo;
  protected
    procedure TerminatedSet; override;
  public
    Owner: TMPVPlayerRenderGL;
    Event: PRTLEvent;
    IsRenderActive : Boolean;
    ForceInvalidateContext: Boolean;
    Ready: Boolean;
    {$IFDEF BGLCONTROLS}
    FDrawCallback: TMPVPlayerDrawEvent;
    {$ENDIF}
    constructor Create(AControl: TUWOpenGLControl; AHandle: Pmpv_handle; AOwner: TMPVPlayerRenderGL {$IFDEF BGLCONTROLS}; ADrawCallback: TMPVPlayerDrawEvent = NIL{$ENDIF});
    destructor Destroy; override;
    procedure Execute; override;
    procedure UpdateSize(const AWidth, AHeight: Integer);
    procedure InvalidateContext(const AReportSwap: Boolean = True);
  end;

  { TMPVPlayerRenderGL }

  TMPVPlayerRenderGL = class
  private
    FThread: TMPVPlayerRenderThread;
    {$IFDEF MPVGL_MAINTHREAD}
    // Renderer for the GUI thread. mpv's update callback runs on mpv's own
    // threads and only touches FActive/FQueued, always under FLock. Everything
    // else is used from the GUI thread only.
    FLock: TRTLCriticalSection;
    FActive: Boolean;
    FQueued: Boolean;
    FLibLoaded: Boolean;
    FRecreate: Boolean;
    FGL: TUWOpenGLControl;
    FHandle: Pmpv_handle;
    FFlipY: Integer;
    {$IFDEF LCLGTK3}
    // GtkGLArea we follow; LCL gtk3 replaces it when the control's handle is
    // recreated
    FGLArea: PGtkWidget;
    FSignalIDs: array[0..2] of gulong;
    {$ENDIF}
    mpvRenderContext: pmpv_render_context;
    mpvOpenGLParams: mpv_opengl_init_params;
    mpvRenderParams: array[0..2] of mpv_render_param;
    mpvUpdateParams: array[0..3] of mpv_render_param;
    mpvfbo: mpv_opengl_fbo;
    FBlockForTargetTime: Integer;
    {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN) AND NOT DEFINED(LCLGTK3)}
    FSwapDrawable: PtrUInt;
    procedure DisableVSync;
    {$ENDIF}
    function CreateRenderContext: Boolean;
    procedure FreeRenderContext(const AMakeCurrent: Boolean);
    procedure RecreateRenderContext;
    {$IFDEF LCLGTK3}
    procedure ConnectGLArea;
    procedure DisconnectGLArea;
    {$ENDIF}
    procedure DoQueuedUpdate;
    {$ENDIF}
    function GetRenderActive: Boolean;
  public
    constructor Create(AControl: TUWOpenGLControl; AHandle: Pmpv_handle{$IFDEF BGLCONTROLS}; ADrawCallback: TMPVPlayerDrawEvent = NIL{$ENDIF});
    destructor Destroy; override;
    procedure Terminate;
    procedure Render(const ForceInvalidate: Boolean = False);
    procedure UpdateThreadSize(const w, h: Integer);
    {$IFDEF MPVGL_MAINTHREAD}
    procedure Paint;
    procedure PrepareRender;
    {$ENDIF}

    property Active: Boolean read GetRenderActive;
  end;

// -----------------------------------------------------------------------------

implementation

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
var
  hLibEGL: TLibHandle = NilHandle;
  egl_GetCurrentContext: function: Pointer; cdecl = NIL;
  egl_GetProcAddress: function(procname: PChar): Pointer; cdecl = NIL;
{$ENDIF}

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

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
procedure LoadEGL;
begin
  if hLibEGL <> NilHandle then Exit;
  hLibEGL := LoadLibrary('libEGL.so.1');
  if hLibEGL <> NilHandle then
  begin
    Pointer(egl_GetCurrentContext) := GetProcAddress(hLibEGL, 'eglGetCurrentContext');
    Pointer(egl_GetProcAddress) := GetProcAddress(hLibEGL, 'eglGetProcAddress');
  end;
end;

// -----------------------------------------------------------------------------

// On Linux, wglGetProcAddress is only an alias of dlsym on libGL. Resolve the
// functions with the loader of the context that is current: EGL (gtk3/qt on
// Wayland, or GDK_GL=gles) or GLX.
function get_proc_address_mpv(ctx: Pointer; Name: PChar): Pointer; cdecl;
begin
  Result := NIL;
  if Assigned(egl_GetCurrentContext) and Assigned(egl_GetProcAddress) and
    (egl_GetCurrentContext() <> NIL) then
    Result := egl_GetProcAddress(Name)
  else if Assigned(glXGetProcAddressARB) then
    Result := glXGetProcAddressARB(Name)
  else if Assigned(glXGetProcAddress) then
    Result := glXGetProcAddress(Name);

  if Result = NIL then
    Result := GetProcAddress(LibGL, Name);
end;
{$ELSE}
function get_proc_address_mpv(ctx: Pointer; Name: PChar): Pointer; cdecl;
begin
  Result := GetProcAddress(LibGL, Name);

  if Result = NIL then
    Result := wglGetProcAddress(Name);
end;
{$ENDIF}

// -----------------------------------------------------------------------------

{$IF DEFINED(MPVGL_MAINTHREAD) AND DEFINED(LCLGTK3)}
// GtkGLArea destroys its GL context when it is unrealized (e.g. when the video
// panel is moved to another window). Free mpv's resources while it still
// exists and create them again as soon as there is a new context: vo=libmpv
// can't start a file while there is no render context.
procedure gtkglarea_unrealizeCB(Widget: PGtkWidget; Data: gpointer); cdecl;
begin
  gtk_gl_area_make_current(PGtkGLArea(Widget));
  TMPVPlayerRenderGL(Data).FreeRenderContext(False);
  TMPVPlayerRenderGL(Data).FRecreate := True;
end;

// -----------------------------------------------------------------------------

procedure gtkglarea_realizeCB(Widget: PGtkWidget; Data: gpointer); cdecl;
begin
  // connected after GtkGLArea's own handler, so its new context exists
  TMPVPlayerRenderGL(Data).RecreateRenderContext;
end;

// -----------------------------------------------------------------------------

procedure gtkglarea_destroyCB(Widget: PGtkWidget; Data: gpointer); cdecl;
begin
  // the widget goes away together with its handlers
  TMPVPlayerRenderGL(Data).FGLArea := NIL;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

// mpv sets the state it uses but doesn't reset the rest. GtkGLArea enables
// GL_DEPTH_TEST before the render signal when it has a depth buffer
// (TOpenGLControl asks for one), and mpv then draws nothing but black.
// The others are cleared too in case something else (BGL overlay, toolkit)
// left them on in the context. Call it with the context current.
procedure ResetGLState;
begin
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_SCISSOR_TEST);
  glDisable(GL_CULL_FACE);
end;

// -----------------------------------------------------------------------------

procedure LIBMPV_RENDER_EVENT(Sender: Pointer); cdecl;
begin
  if (Sender <> NIL) then
    TMPVPlayerRenderGL(Sender).Render;
end;

// -----------------------------------------------------------------------------

{ TMPVPlayerRenderThread }

// -----------------------------------------------------------------------------

constructor TMPVPlayerRenderThread.Create(AControl: TUWOpenGLControl; AHandle: Pmpv_handle; AOwner: TMPVPlayerRenderGL {$IFDEF BGLCONTROLS}; ADrawCallback: TMPVPlayerDrawEvent = NIL{$ENDIF});
begin
  inherited Create(True);

  FreeOnTerminate := False;
  Event := RTLEventCreate;
  ReadyEvent := RTLEventCreate;
  Owner := AOwner;
  mpvHandle := AHandle;
  {$IFDEF BGLCONTROLS}
  FDrawCallback := ADrawCallback;
  {$ENDIF}
  FGL := AControl;
  FWidth := FGL.ClientWidth;
  FHeight := FGL.ClientHeight;
  FGL.ReleaseContext;

  IsRenderActive := False;
  ForceInvalidateContext := False;
  Ready := False;
  mpvRenderContext := NIL;
end;

// -----------------------------------------------------------------------------

destructor TMPVPlayerRenderThread.Destroy;
begin
  RTLEventDestroy(Event);
  RTLEventDestroy(ReadyEvent);
  Owner := NIL;
  mpvHandle := NIL;

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
{$IFDEF DARWIN}
var
  OuterPool, LoopPool: NSAutoreleasePool;
{$ENDIF}
begin
  {$IFDEF DARWIN}
  // On macOS, Cocoa/OpenGL autoreleased objects can trigger exceptions
  // when the thread exits, causing an LCL modal alert and deadlock.
  OuterPool := NSAutoreleasePool.alloc.init;
  {$ENDIF}
  try
    try
      if InitializeRenderContext then
      begin
        while not Terminated do
        begin
          RTLEventWaitFor(Event);
          {$IFDEF DARWIN}
          LoopPool := NSAutoreleasePool.alloc.init;
          {$ENDIF}
          try
            if ForceInvalidateContext then
            begin
              ForceInvalidateContext := False;
              InvalidateContext(False);
            end
            else if IsRenderActive and ((mpv_render_context_update(mpvRenderContext^) and MPV_RENDER_UPDATE_FRAME) > 0) then
              InvalidateContext;
          except
            //on E: Exception do
            //  DebugLog('RenderGL Thread EXCEPTION: ' + E.ClassName + ': ' + E.Message);
          end;
          {$IFDEF DARWIN}
          try
            LoopPool.release;
          except
          end;
          {$ENDIF}

          RTLEventResetEvent(Event);
        end;
      end;
    except
    end;
  finally
    try
      UnInitializeRenderContext;
    except
    end;
    {$IFDEF DARWIN}
    try
      OuterPool.release;
    except
    end;
    {$ENDIF}
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

    IsRenderActive := True;
    Result := True;
  finally
    Ready := True;
    RTLEventSetEvent(ReadyEvent);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.UnInitializeRenderContext;
begin
  if Assigned(mpvRenderContext) then
  begin
    if not IsDestroyingGL then
      FGL.MakeCurrent();

    mpv_render_context_set_update_callback(mpvRenderContext^, NIL, NIL);
    mpv_render_context_free(mpvRenderContext^);
    mpvRenderContext := NIL;

    if not IsDestroyingGL then
      FGL.ReleaseContext;
  end;

  SetLength(mpvRenderParams, 0);
  SetLength(mpvUpdateParams, 0);
  UnInitialize_libMPV_Render;
end;

// -----------------------------------------------------------------------------

function TMPVPlayerRenderThread.IsDestroyingGL: Boolean;
begin
  Result := not Assigned(FGL) or (csDestroying in FGL.ComponentState) or not FGL.HandleAllocated;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.Update_mpvfbo;
begin
  mpvfbo.internal_format := 0;
  mpvfbo.fbo := 0;
  mpvfbo.w := FWidth;
  mpvfbo.h := FHeight;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.UpdateSize(const AWidth, AHeight: Integer);
begin
  FWidth  := AWidth;
  FHeight := AHeight;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderThread.InvalidateContext(const AReportSwap: Boolean = True);
begin
  Update_mpvfbo;
  if not Terminated and IsRenderActive then
  begin
    if not IsDestroyingGL then
      FGL.MakeCurrent();

    ResetGLState;
    mpv_render_context_render(mpvRenderContext^, Pmpv_render_param(@mpvUpdateParams[0]));
    {$IFDEF BGLCONTROLS}
    if Assigned(FDrawCallback) then
    begin
      BGLViewPort(FGL.ClientWidth, FGL.ClientHeight);
      FDrawCallback(Self, BGLCanvas);
    end;
    {$ENDIF}

    if IsRenderActive and not IsDestroyingGL then
    begin
      FGL.SwapBuffers;

      if AReportSwap  then
        mpv_render_context_report_swap(mpvRenderContext^);
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ TMPVPlayerRenderGL }

// -----------------------------------------------------------------------------

constructor TMPVPlayerRenderGL.Create(AControl: TUWOpenGLControl; AHandle: Pmpv_handle{$IFDEF BGLCONTROLS}; ADrawCallback: TMPVPlayerDrawEvent = NIL{$ENDIF});
begin
  {$IFDEF MPVGL_MAINTHREAD}
  InitCriticalSection(FLock);
  FGL := AControl;
  FHandle := AHandle;
  FFlipY := 1; // both draw bottom-up: GtkGLArea's FBO and the GLX window
  mpvRenderContext := NIL;

  FLibLoaded := Initialize_libMPV_Render(hLibMPV);
  if not FLibLoaded then Exit;

  FActive := True;
  if not CreateRenderContext then
  begin
    // leave a valid, inactive object: Active = False makes TMPVPlayer fall
    // back to rmEmbedding (rfSwitchToEmbedding)
    FActive := False;
    UnInitialize_libMPV_Render;
    FLibLoaded := False;
    Exit;
  end;

  {$IFDEF LCLGTK3}
  ConnectGLArea;
  {$ENDIF}
  Exit;
  {$ENDIF}

  if Initialize_libMPV_Render(hLibMPV) then
  begin
    FThread := TMPVPlayerRenderThread.Create(AControl, AHandle, Self{$IFDEF BGLCONTROLS}, ADrawCallback{$ENDIF});
    FThread.Start;
    RTLEventWaitFor(FThread.ReadyEvent, 5000);
  end;
end;

// -----------------------------------------------------------------------------

destructor TMPVPlayerRenderGL.Destroy;
begin
  Terminate;
  {$IFDEF MPVGL_MAINTHREAD}
  DoneCriticalSection(FLock);
  {$ENDIF}
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.Terminate;
var
  Deadline: QWord;
begin
  {$IFDEF MPVGL_MAINTHREAD}
  EnterCriticalSection(FLock);
  FActive := False;
  LeaveCriticalSection(FLock);

  {$IFDEF LCLGTK3}
  DisconnectGLArea;
  {$ENDIF}

  FreeRenderContext(True);

  if FLibLoaded then
  begin
    UnInitialize_libMPV_Render;
    FLibLoaded := False;
  end;
  Exit;
  {$ENDIF}

  if Assigned(FThread) then
  begin
    if Assigned(FThread.mpvRenderContext) and Assigned(FThread.mpvRenderContext^) then
      mpv_render_context_set_update_callback(FThread.mpvRenderContext^, NIL, NIL);

    FThread.Terminate;
    RTLEventSetEvent(FThread.Event);

    // Use a bounded wait to prevent application hangs during thread shutdown.
    // If the thread gets stuck, the thread object is leaked.
    Deadline := GetTickCount64 + 3000;
    while not FThread.Finished and (GetTickCount64 < Deadline) do
    begin
      RTLEventSetEvent(FThread.Event);
      Sleep(10);
    end;

    if FThread.Finished then
      FreeAndNil(FThread)
    else
      FThread := NIL;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.Render(const ForceInvalidate: Boolean = False);
begin
  {$IFDEF MPVGL_MAINTHREAD}
  // Any thread (mpv's update callback). At most one update is pending; it is
  // always queued, even from the GUI thread, so it never runs inside a mpv
  // call or a paint event.
  EnterCriticalSection(FLock);
  try
    if FActive and not FQueued then
    begin
      FQueued := True;
      TThread.ForceQueue(NIL, @DoQueuedUpdate);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
  Exit;
  {$ENDIF}

  if Assigned(FThread) then
  begin
    FThread.ForceInvalidateContext := ForceInvalidate;
    RTLEventSetEvent(FThread.Event);
  end;
end;

// -----------------------------------------------------------------------------

{$IFDEF MPVGL_MAINTHREAD}
function TMPVPlayerRenderGL.CreateRenderContext: Boolean;
begin
  Result := False;
  // gtk3: GtkGLArea's own context (the same one current in its render signal)
  // qt5/qt6: the control's GLX context
  if not FGL.MakeCurrent() then Exit;
  {$IFDEF LCLGTK3}
  ConnectGLArea;
  {$ENDIF}

  mpvOpenGLParams.get_proc_address := @get_proc_address_mpv;
  mpvOpenGLParams.get_proc_address_ctx := NIL;

  // No MPV_RENDER_PARAM_ADVANCED_CONTROL: the GUI thread also sends
  // synchronous commands to mpv, which could then wait on this renderer.
  mpvRenderParams[0]._type := MPV_RENDER_PARAM_API_TYPE;
  mpvRenderParams[0].Data  := PChar(MPV_RENDER_API_TYPE_OPENGL);
  mpvRenderParams[1]._type := MPV_RENDER_PARAM_OPENGL_INIT_PARAMS;
  mpvRenderParams[1].Data  := @mpvOpenGLParams;
  mpvRenderParams[2]._type := MPV_RENDER_PARAM_INVALID;
  mpvRenderParams[2].Data  := NIL;

  if mpv_render_context_create(mpvRenderContext, FHandle^, Pmpv_render_param(@mpvRenderParams[0])) <> MPV_ERROR_SUCCESS then
  begin
    mpvRenderContext := NIL;
    Exit;
  end;

  mpvUpdateParams[0]._type := MPV_RENDER_PARAM_OPENGL_FBO;
  mpvUpdateParams[0].Data  := @mpvfbo;
  mpvUpdateParams[1]._type := MPV_RENDER_PARAM_FLIP_Y;
  mpvUpdateParams[1].Data  := @FFlipY;
  // Rendering runs on the GUI thread: don't let mpv wait there until the
  // frame's display time, it would freeze the interface on every frame.
  FBlockForTargetTime := 0;
  mpvUpdateParams[2]._type := MPV_RENDER_PARAM_BLOCK_FOR_TARGET_TIME;
  mpvUpdateParams[2].Data  := @FBlockForTargetTime;
  mpvUpdateParams[3]._type := MPV_RENDER_PARAM_INVALID;
  mpvUpdateParams[3].Data  := NIL;

  mpv_render_context_set_update_callback(mpvRenderContext^, @LIBMPV_RENDER_EVENT, Self);
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.FreeRenderContext(const AMakeCurrent: Boolean);
begin
  if not Assigned(mpvRenderContext) then Exit;

  // Once this returns mpv does not run the callback anymore, so no new
  // update can be queued; then drop the one that may be pending.
  mpv_render_context_set_update_callback(mpvRenderContext^, NIL, NIL);
  TThread.RemoveQueuedEvents(NIL, @DoQueuedUpdate);
  EnterCriticalSection(FLock);
  FQueued := False;
  LeaveCriticalSection(FLock);

  if AMakeCurrent and FGL.HandleAllocated and not (csDestroying in FGL.ComponentState) then
    FGL.MakeCurrent();

  mpv_render_context_free(mpvRenderContext^);
  mpvRenderContext := NIL;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.DoQueuedUpdate;
begin
  EnterCriticalSection(FLock);
  FQueued := False;
  LeaveCriticalSection(FLock);

  if not Assigned(mpvRenderContext) then Exit;

  // Only repaint when there is a new frame; the widget set merges repeated
  // invalidations into one paint event.
  if (mpv_render_context_update(mpvRenderContext^) and MPV_RENDER_UPDATE_FRAME) <> 0 then
    if FGL.HandleAllocated and FGL.IsVisible then
      FGL.Invalidate;
end;

// -----------------------------------------------------------------------------

{$IFDEF LCLGTK3}
procedure TMPVPlayerRenderGL.ConnectGLArea;
var
  Widget: PGtkWidget;
begin
  if not FGL.HandleAllocated then Exit;
  Widget := TGtk3Widget(FGL.Handle).Widget;
  if Widget = FGLArea then Exit;

  DisconnectGLArea;
  FGLArea := Widget;
  FSignalIDs[0] := g_signal_connect_data(PGObject(FGLArea), 'unrealize',
    TGCallback(@gtkglarea_unrealizeCB), Self, NIL, G_CONNECT_DEFAULT);
  FSignalIDs[1] := g_signal_connect_data(PGObject(FGLArea), 'realize',
    TGCallback(@gtkglarea_realizeCB), Self, NIL, [G_CONNECT_AFTER]);
  FSignalIDs[2] := g_signal_connect_data(PGObject(FGLArea), 'destroy',
    TGCallback(@gtkglarea_destroyCB), Self, NIL, G_CONNECT_DEFAULT);
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.DisconnectGLArea;
var
  i: Integer;
begin
  if Assigned(FGLArea) then
    for i := Low(FSignalIDs) to High(FSignalIDs) do
      if FSignalIDs[i] <> 0 then
        g_signal_handler_disconnect(PGObject(FGLArea), FSignalIDs[i]);

  FGLArea := NIL;
  FillChar(FSignalIDs, SizeOf(FSignalIDs), 0);
end;
{$ENDIF}

// -----------------------------------------------------------------------------

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN) AND NOT DEFINED(LCLGTK3)}
procedure TMPVPlayerRenderGL.DisableVSync;
type
  TglXSwapIntervalEXT = function(dpy: PDisplay; drawable: GLXDrawable; interval: Integer): Integer; cdecl;
  TglXSwapIntervalMESA = function(interval: Cardinal): Integer; cdecl;
var
  Drawable: GLXDrawable;
  SwapEXT: TglXSwapIntervalEXT;
  SwapMESA: TglXSwapIntervalMESA;
begin
  // qt5/qt6: SwapBuffers runs on the GUI thread, and with vsync it waits for
  // the next refresh. mpv already paces the frames, so swap right away. The
  // interval belongs to the window, which Qt may recreate: set it once for
  // each new one.
  if not Assigned(glXGetCurrentDrawable) then Exit;
  Drawable := glXGetCurrentDrawable();
  if (Drawable = 0) or (PtrUInt(Drawable) = FSwapDrawable) then Exit;
  FSwapDrawable := PtrUInt(Drawable);

  Pointer(SwapEXT) := get_proc_address_mpv(NIL, 'glXSwapIntervalEXT');
  if Assigned(SwapEXT) and Assigned(glXGetCurrentDisplay) then
    SwapEXT(glXGetCurrentDisplay(), Drawable, 0)
  else
  begin
    Pointer(SwapMESA) := get_proc_address_mpv(NIL, 'glXSwapIntervalMESA');
    if Assigned(SwapMESA) then
      SwapMESA(0);
  end;
end;
{$ENDIF}

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.PrepareRender;
begin
  // TMPVPlayer calls this before loading a file. gtk3 may have replaced the
  // GtkGLArea (and its context) since the last paint, while the form that now
  // holds it is still hidden: create the render context in the new one right
  // away, CreateRenderContext realizes the widget if needed.
  RecreateRenderContext;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.RecreateRenderContext;
var
  args: array[0..1] of PChar;
begin
  // gtk3: GtkGLArea was realized again with a new context, recreate mpv's
  // resources in it and let mpv bring the video output back (if a file was
  // already playing; otherwise mpv just answers with an error)
  if not FRecreate or Assigned(mpvRenderContext) then Exit;
  EnterCriticalSection(FLock);
  try
    if not FActive then Exit;
  finally
    LeaveCriticalSection(FLock);
  end;

  FRecreate := False;
  if CreateRenderContext then
  begin
    args[0] := 'video-reload';
    args[1] := NIL;
    mpv_command_async(FHandle^, 0, PPChar(@args[0]));
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.Paint;
var
  fbo: GLint;
  w, h: Integer;
  args: array[0..1] of PChar;
begin
  // GUI thread, inside the control's paint event
  EnterCriticalSection(FLock);
  try
    if not FActive then Exit;
  finally
    LeaveCriticalSection(FLock);
  end;

  // gtk3: in case the realize handler couldn't do it
  RecreateRenderContext;

  if not Assigned(mpvRenderContext) then Exit;

  {$IFNDEF LCLGTK3}
  // qt5/qt6: the control draws with its own GLX context on its native window
  // and TUWOpenGLControl.Paint skips the MakeCurrent of TOpenGLControl.
  // gtk3: GtkGLArea already made its context current and bound its FBO.
  if not FGL.MakeCurrent() then Exit;
  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN) AND NOT DEFINED(LCLGTK3)}
  DisableVSync;
  {$ENDIF}
  {$ENDIF}

  w := Round(FGL.ClientWidth * FGL.GetCanvasScaleFactor);
  h := Round(FGL.ClientHeight * FGL.GetCanvasScaleFactor);
  // gtk3 can paint with an empty allocation while the layout settles
  if (w <= 0) or (h <= 0) then Exit;

  fbo := 0;
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, @fbo);
  mpvfbo.fbo := fbo;
  mpvfbo.w := w;
  mpvfbo.h := h;
  mpvfbo.internal_format := 0;

  ResetGLState;

  // nothing was drawn (bad parameters or context): don't present garbage
  if mpv_render_context_render(mpvRenderContext^, Pmpv_render_param(@mpvUpdateParams[0])) < 0 then
    Exit;

  {$IFNDEF LCLGTK3}
  // qt5/qt6: double buffered GLX window that nobody else presents
  FGL.SwapBuffers;
  mpv_render_context_report_swap(mpvRenderContext^);
  {$ENDIF}
  // gtk3: GtkGLArea presents its FBO itself after the render signal, at a
  // moment we don't see, so there is no swap to report (optional for mpv)
end;
{$ENDIF}

// -----------------------------------------------------------------------------

function TMPVPlayerRenderGL.GetRenderActive: Boolean;
begin
  {$IFDEF MPVGL_MAINTHREAD}
  EnterCriticalSection(FLock);
  Result := FActive;
  LeaveCriticalSection(FLock);
  Exit;
  {$ENDIF}
  if Assigned(FThread) then
    Result := FThread.IsRenderActive
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerRenderGL.UpdateThreadSize(const w, h: Integer);
begin
  if Assigned(FThread) then
    FThread.UpdateSize(w, h);
end;

// -----------------------------------------------------------------------------

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
initialization
  LoadEGL;

// -----------------------------------------------------------------------------

finalization
  if hLibEGL <> NilHandle then
    FreeLibrary(hLibEGL);
{$ENDIF}

// -----------------------------------------------------------------------------

end.


