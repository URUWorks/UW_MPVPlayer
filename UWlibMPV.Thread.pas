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

unit UWlibMPV.Thread;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils;

// -----------------------------------------------------------------------------

type

  // used to handle mpv envents

  { TUWCustomThreadEvent }

  TUWlibMPVThreadEvent = class;

  TUWCustomThreadEvent = class(TThread)
  private
    procedure HandleEvent;
  public
    FOwner : TUWlibMPVThreadEvent;
    Event  : PRtlEvent;
    constructor Create(AOwner: TUWlibMPVThreadEvent);
    procedure Execute; override;
    destructor Destroy; override;
  end;

  { TUWlibMPVThreadEvent }

  TUWlibMPVThreadEvent = class
  private
    FThread  : TUWCustomThreadEvent;
    FOnEvent : TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PushEvent;
    property OnEvent: TNotifyEvent read FOnEvent write FOnEvent;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TUWCustomThreadEvent }

// -----------------------------------------------------------------------------

constructor TUWCustomThreadEvent.Create(AOwner: TUWlibMPVThreadEvent);
begin
  inherited Create(True);
  FOwner   := AOwner;
  Event    := RTLEventCreate;
  //Priority := tpHigher;
end;

// -----------------------------------------------------------------------------

procedure TUWCustomThreadEvent.Execute;
begin
  while not Terminated do
  begin
    RTLEventWaitFor(Event);
    Queue(@HandleEvent); //Synchronize(@HandleEvent);
    RTLEventResetEvent(Event);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUWCustomThreadEvent.HandleEvent;
begin
  if Assigned(FOwner.OnEvent) then FOwner.OnEvent(FOwner);
end;

// -----------------------------------------------------------------------------

destructor TUWCustomThreadEvent.Destroy;
begin
  RTLEventDestroy(Event);
  FOwner := NIL;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

{ TUWlibMPVThreadEvent }

// -----------------------------------------------------------------------------

constructor TUWlibMPVThreadEvent.Create;
begin
  FOnEvent := NIL;
  FThread  := TUWCustomThreadEvent.Create(Self);
  FThread.Start;
end;

// -----------------------------------------------------------------------------

destructor TUWlibMPVThreadEvent.Destroy;
begin
  FThread.Terminate;
  RTLEventSetEvent(FThread.Event);
  FThread.WaitFor;
  FThread.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUWlibMPVThreadEvent.PushEvent;
begin
  RTLEventSetEvent(FThread.Event);
end;

// -----------------------------------------------------------------------------

end.

