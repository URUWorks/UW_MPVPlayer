{*
 *  URUWorks libMPV (render_gl.h)
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

{$PACKRECORDS C}

unit libMPV.Render_gl;

// -----------------------------------------------------------------------------

interface

// -----------------------------------------------------------------------------

  { Copyright (C) 2018 the mpv developers
   *
   * Permission to use, copy, modify, and/or distribute this software for any
   * purpose with or without fee is hereby granted, provided that the above
   * copyright notice and this permission notice appear in all copies.
   *
   * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
    }

  {*
   * OpenGL backend
   * --------------
   *
   * This header contains definitions for using OpenGL with the render.h API.
   *
   * OpenGL interop
   * --------------
   *
   * The OpenGL backend has some special rules, because OpenGL itself uses
   * implicit per-thread contexts, which causes additional API problems.
   *
   * This assumes the OpenGL context lives on a certain thread controlled by the
   * API user. All mpv_render_* APIs have to be assumed to implicitly use the
   * OpenGL context if you pass a mpv_render_context using the OpenGL backend,
   * unless specified otherwise.
   *
   * The OpenGL context is indirectly accessed through the OpenGL function
   * pointers returned by the get_proc_address callback in mpv_opengl_init_params.
   * Generally, mpv will not load the system OpenGL library when using this API.
   *
   * OpenGL state
   * ------------
   *
   * OpenGL has a large amount of implicit state. All the mpv functions mentioned
   * above expect that the OpenGL state is reasonably set to OpenGL standard
   * defaults. Likewise, mpv will attempt to leave the OpenGL context with
   * standard defaults. The following state is excluded from this:
   *
   *      - the glViewport state
   *      - the glScissor state (but GL_SCISSOR_TEST is in its default value)
   *      - glBlendFuncSeparate() state (but GL_BLEND is in its default value)
   *      - glClearColor() state
   *      - mpv may overwrite the callback set with glDebugMessageCallback()
   *      - mpv always disables GL_DITHER at init
   *
   * Messing with the state could be avoided by creating shared OpenGL contexts,
   * but this is avoided for the sake of compatibility and interoperability.
   *
   * On OpenGL 2.1, mpv will strictly call functions like glGenTextures() to
   * create OpenGL objects. You will have to do the same. This ensures that
   * objects created by mpv and the API users don't clash. Also, legacy state
   * must be either in its defaults, or not interfere with core state.
   *
   * API use
   * -------
   *
   * The mpv_render_* API is used. That API supports multiple backends, and this
   * section documents specifics for the OpenGL backend.
   *
   * Use mpv_render_context_create() with MPV_RENDER_PARAM_API_TYPE set to
   * MPV_RENDER_API_TYPE_OPENGL, and MPV_RENDER_PARAM_OPENGL_INIT_PARAMS provided.
   *
   * Call mpv_render_context_render() with MPV_RENDER_PARAM_OPENGL_FBO to render
   * the video frame to an FBO.
   *
   * Hardware decoding
   * -----------------
   *
   * Hardware decoding via this API is fully supported, but requires some
   * additional setup. (At least if direct hardware decoding modes are wanted,
   * instead of copying back surface data from GPU to CPU RAM.)
   *
   * There may be certain requirements on the OpenGL implementation:
   *
   * - Windows: ANGLE is required (although in theory GL/DX interop could be used)
   * - Intel/Linux: EGL is required, and also the native display resource needs
   *                to be provided (e.g. MPV_RENDER_PARAM_X11_DISPLAY for X11 and
   *                MPV_RENDER_PARAM_WL_DISPLAY for Wayland)
   * - nVidia/Linux: Both GLX and EGL should work (GLX is required if vdpau is
   *                 used, e.g. due to old drivers.)
   * - OSX: CGL is required (CGLGetCurrentContext() returning non-NULL)
   * - iOS: EAGL is required (EAGLContext.currentContext returning non-nil)
   *
   * Once these things are setup, hardware decoding can be enabled/disabled at
   * any time by setting the "hwdec" property.
    }

type
  _drmModeAtomicReq  = record end;
  P_drmModeAtomicReq  = ^_drmModeAtomicReq;

  {*
   * For initializing the mpv OpenGL state via MPV_RENDER_PARAM_OPENGL_INIT_PARAMS.
    }
  mpv_opengl_init_params = record
  {*
       * This retrieves OpenGL function pointers, and will use them in subsequent
       * operation.
       * Usually, you can simply call the GL context APIs from this callback (e.g.
       * glXGetProcAddressARB or wglGetProcAddress), but some APIs do not always
       * return pointers for all standard functions (even if present); in this
       * case you have to compensate by looking up these functions yourself when
       * libmpv wants to resolve them through this callback.
       * libmpv will not normally attempt to resolve GL functions on its own, nor
       * does it link to GL libraries directly.
        }
    get_proc_address : function (ctx:pointer; name:pchar):pointer;cdecl;
  {*
       * Value passed as ctx parameter to get_proc_address().
        }
    get_proc_address_ctx : pointer;
  end;

  {*
   * For MPV_RENDER_PARAM_OPENGL_FBO.
    }
  mpv_opengl_fbo = record
  {*
       * Framebuffer object name. This must be either a valid FBO generated by
       * glGenFramebuffers() that is complete and color-renderable, or 0. If the
       * value is 0, this refers to the OpenGL default framebuffer.
        }
    fbo : longint;
  {*
       * Valid dimensions. This must refer to the size of the framebuffer. This
       * must always be set.
        }
    w, h : longint;
  {*
       * Underlying texture internal format (e.g. GL_RGBA8), or 0 if unknown. If
       * this is the default framebuffer, this can be an equivalent.
        }
    internal_format : longint;
  end;

  {*
   * Deprecated. For MPV_RENDER_PARAM_DRM_DISPLAY.
    }
  mpv_opengl_drm_params = record
    fd : longint;
    crtc_id : longint;
    connector_id : longint;
    atomic_request_ptr : ^P_drmModeAtomicReq;
    render_fd : longint;
  end;

  {*
   * For MPV_RENDER_PARAM_DRM_DRAW_SURFACE_SIZE.
    }
  mpv_opengl_drm_draw_surface_size = record
  {*
       * size of the draw plane surface in pixels.
        }
    width  : longint;
    height : longint;
  end;

  {*
   * For MPV_RENDER_PARAM_DRM_DISPLAY_V2.
    }
  mpv_opengl_drm_params_v2 = record
  {*
       * DRM fd (int). Set to -1 if invalid.
        }
    fd : longint;
  {*
       * Currently used crtc id
        }
    crtc_id : longint;
  {*
       * Currently used connector id
        }
    connector_id : longint;
  {*
       * Pointer to a drmModeAtomicReq pointer that is being used for the renderloop.
       * This pointer should hold a pointer to the atomic request pointer
       * The atomic request pointer is usually changed at every renderloop.
        }
    atomic_request_ptr : ^P_drmModeAtomicReq;
  {*
       * DRM render node. Used for VAAPI interop.
       * Set to -1 if invalid.
        }
    render_fd : longint;
  end;

  {*
   * For backwards compatibility with the old naming of mpv_opengl_drm_draw_surface_size
    }
  mpv_opengl_drm_osd_size = mpv_opengl_drm_draw_surface_size;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

end.
