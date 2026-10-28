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
 *  Copyright (C) 2021-2026 URUWorks, uruworks@gmail.com.
 *}

{$mode ObjFPC}{$H+}
{$IFDEF DARWIN}
  {$modeswitch Objectivec1}
{$ENDIF}
{$DEFINE USETIMER}
{$DEFINE ENABLE_BACKIMAGE}
{$DEFINE ENABLE_SMPTE}
{.$DEFINE ENABLE_EXT_EVENTS}
{.$DEFINE ENABLE_EXT_STATUS}
{.$DEFINE ENABLE_EXT_EQUALIZER}
{.$DEFINE BGLCONTROLS} // requires BGLControls package.
{.$DEFINE SDL2} // requires pl_SDL2 package.

// gtk3 (GtkGLArea) and qt5/qt6 (QWidget winId) can't drive the GL context
// from a worker thread: render from the GUI thread, inside the paint event.
{$IF DEFINED(LCLGTK3) OR DEFINED(LCLQt5) OR DEFINED(LCLQt6)}
  {$DEFINE MPVGL_MAINTHREAD}
{$ENDIF}



