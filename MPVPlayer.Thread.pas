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

unit MPVPlayer.Thread;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils;

// -----------------------------------------------------------------------------

type

  // used to handle mpv envents

  { TUWCustomThreadEvent }

  TMPVPlayerThreadEvent = class;

  TMPVPlayerCustomThreadEvent = class(TThread)
  private
    procedure HandleEvent;
  protected
    procedure TerminatedSet; override;
  public
    FOwner : TMPVPlayerThreadEvent;
    Event  : PRtlEvent;
    constructor Create(AOwner: TMPVPlayerThreadEvent);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TMPVPlayerThreadEvent }

  TMPVPlayerThreadEvent = class
  private
    FThread  : TMPVPlayerCustomThreadEvent;
    FOnEvent : TNotifyEvent;
  public
    constructor Create(CreateSuspended: Boolean = False);
    destructor Destroy; override;
    procedure Start;
    procedure PushEvent;
    property OnEvent: TNotifyEvent read FOnEvent write FOnEvent;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TMPVPlayerCustomThreadEvent }

// -----------------------------------------------------------------------------

constructor TMPVPlayerCustomThreadEvent.Create(AOwner: TMPVPlayerThreadEvent);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FOwner := AOwner;
  Event := RTLEventCreate;
end;

// -----------------------------------------------------------------------------

destructor TMPVPlayerCustomThreadEvent.Destroy;
begin
  RTLEventDestroy(Event);
  FOwner := NIL;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerCustomThreadEvent.TerminatedSet;
begin
  if Assigned(Event) then RTLEventSetEvent(Event);
  inherited TerminatedSet;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerCustomThreadEvent.Execute;
begin
  while not Terminated do
  begin
    RTLEventWaitFor(Event);
    Queue(@HandleEvent); //Synchronize(@HandleEvent);
    RTLEventResetEvent(Event);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerCustomThreadEvent.HandleEvent;
begin
  if Assigned(FOwner) and Assigned(FOwner.OnEvent) then
    FOwner.OnEvent(FOwner);
end;

// -----------------------------------------------------------------------------

{ TMPVPlayerThreadEvent }

// -----------------------------------------------------------------------------

constructor TMPVPlayerThreadEvent.Create(CreateSuspended: Boolean = False);
begin
  FOnEvent := NIL;
  FThread := TMPVPlayerCustomThreadEvent.Create(Self);
  if not CreateSuspended then FThread.Start;
end;

// -----------------------------------------------------------------------------

destructor TMPVPlayerThreadEvent.Destroy;
begin
  FThread.Terminate;
  FThread := NIL;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerThreadEvent.Start;
begin
  if Assigned(FThread) and FThread.Suspended then FThread.Start;
end;

// -----------------------------------------------------------------------------

procedure TMPVPlayerThreadEvent.PushEvent;
begin
  if Assigned(FThread) then RTLEventSetEvent(FThread.Event);
end;

// -----------------------------------------------------------------------------

end.

