{*
 * URUWorks MPVPlayer - Extended
 *
 * Author  : URUWorks
 * Website : uruworks.net
 *
 * The contents of this file are used with permission, subject to
 * the Mozilla Public License Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/2.0.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * Copyright (C) 2026 URUWorks, uruworks@gmail.com.
 *}

unit MPVPlayer.Extended;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, Math, Graphics, MPVPlayer, libMPV.Client;

// -----------------------------------------------------------------------------

type

  { TMPVVideoInfo }

  TMPVVideoInfo = record
    Width: Integer;
    Height: Integer;
    FPS: Double;
    Bitrate: Int64;
    Codec: String;
    HWDec: String;
    Format: String;
  end;

  { TMPVAudioInfo }

  TMPVAudioInfo = record
    Channels: Integer;
    SampleRate: Integer;
    Bitrate: Int64;
    Codec: String;
    Format: String;
  end;

  { Deinterlace }

  TMPVDeinterlaceMode = (dimOff, dimOn, dimAuto);

const

  { Cache por tipo de fuente }

  CACHE_TYPE_DEFAULT = 0;
  CACHE_TYPE_DVD = 1;
  CACHE_TYPE_INTERNET = 2;
  CACHE_TYPE_NETWORK = 3;

  { Frecuencias del equalizador de 10 bandas en Hz }

  MPV_EQ_FREQUENCIES: array[0..9] of Integer =
    (31, 62, 125, 250, 500, 1000, 2000, 4000, 8000, 16000);

{ Subtítulos }

procedure MPVSetSubBold(const ACore: TMPVCore; const AValue: Boolean);
procedure MPVSetSubItalic(const ACore: TMPVCore; const AValue: Boolean);
procedure MPVSetSubOutlineColor(const ACore: TMPVCore; const AValue: String); // '#RRGGBB' o '&HBBGGRR&' según convención mpv
procedure MPVSetSubOutlineSize(const ACore: TMPVCore; const AValue: Double);
procedure MPVSetSubPosition(const ACore: TMPVCore; const AValue: Integer); // 0=arriba, 100=abajo
procedure MPVSetSubEncoding(const ACore: TMPVCore; const AValue: String);
procedure MPVSetSubAutoLoad(const ACore: TMPVCore; const AValue: Boolean);
procedure MPVSetSubDelay(const ACore: TMPVCore; const AValueMs: Double);
procedure MPVSetSubVisible(const ACore: TMPVCore; const AValue: Boolean);
procedure MPVSetSubScale(const ACore: TMPVCore; const AValue: Double);

function TColorToMPVColor(const AColor: TColor): String; // TColor -> '#RRGGBB'

{ Propiedades de video / color }

procedure MPVSetBrightness(const ACore: TMPVCore; const AValue: Integer); // -100..100
procedure MPVSetContrast(const ACore: TMPVCore; const AValue: Integer);   // -100..100
procedure MPVSetSaturation(const ACore: TMPVCore; const AValue: Integer); // -100..100
procedure MPVSetHue(const ACore: TMPVCore; const AValue: Integer);        // -100..100
procedure MPVSetGamma(const ACore: TMPVCore; const AValue: Integer);      // -100..100

{ Deinterlace }

procedure MPVSetDeinterlace(const ACore: TMPVCore; const AMode: TMPVDeinterlaceMode);
procedure MPVSetDeinterlaceAlg(const ACore: TMPVCore; const AAlgorithm: String); // 'yadif', 'bwdif', etc.

{ Cache por tipo de fuente }

function GetCacheForSource(const AURL: String): Integer;
procedure MPVSetCacheSize(const ACore: TMPVCore; const ASourceType: Integer; const ASeconds: Integer);

{ Info de stream }

function MPVGetVideoInfo(const ACore: TMPVCore): TMPVVideoInfo;
function MPVGetAudioInfo(const ACore: TMPVCore): TMPVAudioInfo;

{ Dispositivos de audio }

procedure MPVSetAudioOutput(const ACore: TMPVCore; const ADriver: String);
procedure MPVSetAudioDevice(const ACore: TMPVCore; const ADevice: String);
function MPVGetAudioDeviceList(const ACore: TMPVCore): TStringList;
procedure MPVSetAudioNormalize(const ACore: TMPVCore; const AEnable: Boolean);
procedure MPVSetAudioChannels(const ACore: TMPVCore; const AChannels: String); // 'mono','stereo','5.1','7.1'

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

function TColorToMPVColor(const AColor: TColor): String;
var
  RGB: LongInt;
begin
  RGB := ColorToRGB(AColor);
  Result := Format('#%.2x%.2x%.2x', [Byte(RGB), Byte(RGB shr 8), Byte(RGB shr 16)]);
end;

// -----------------------------------------------------------------------------

{ Subtítulos }

// -----------------------------------------------------------------------------

procedure MPVSetSubBold(const ACore: TMPVCore; const AValue: Boolean);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_boolean('sub-bold', AValue);
end;

// -----------------------------------------------------------------------------

procedure MPVSetSubItalic(const ACore: TMPVCore; const AValue: Boolean);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_boolean('sub-italic', AValue);
end;

// -----------------------------------------------------------------------------

procedure MPVSetSubOutlineColor(const ACore: TMPVCore; const AValue: String);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_option_string_('sub-border-color=' + AValue);
end;

// -----------------------------------------------------------------------------

procedure MPVSetSubOutlineSize(const ACore: TMPVCore; const AValue: Double);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_double('sub-border-size', AValue);
end;

// -----------------------------------------------------------------------------

procedure MPVSetSubPosition(const ACore: TMPVCore; const AValue: Integer);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_int64('sub-pos', AValue);
end;

// -----------------------------------------------------------------------------

procedure MPVSetSubEncoding(const ACore: TMPVCore; const AValue: String);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_option_string_('sub-codepage=' + AValue);
end;

// -----------------------------------------------------------------------------

procedure MPVSetSubAutoLoad(const ACore: TMPVCore; const AValue: Boolean);
begin
  if not Assigned(ACore) then Exit;
  if AValue then
    ACore.mpv_set_option_string_('sub-auto=exact')
  else
    ACore.mpv_set_option_string_('sub-auto=no');
end;

// -----------------------------------------------------------------------------

procedure MPVSetSubDelay(const ACore: TMPVCore; const AValueMs: Double);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_double('sub-delay', AValueMs / 1000.0);
end;

// -----------------------------------------------------------------------------

procedure MPVSetSubVisible(const ACore: TMPVCore; const AValue: Boolean);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_boolean('sub-visibility', AValue);
end;

// -----------------------------------------------------------------------------

procedure MPVSetSubScale(const ACore: TMPVCore; const AValue: Double);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_double('sub-scale', AValue);
end;

// -----------------------------------------------------------------------------

{ Propiedades de video / color }

// -----------------------------------------------------------------------------

procedure MPVSetBrightness(const ACore: TMPVCore; const AValue: Integer);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_int64('brightness', EnsureRange(AValue, -100, 100));
end;

// -----------------------------------------------------------------------------

procedure MPVSetContrast(const ACore: TMPVCore; const AValue: Integer);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_int64('contrast', EnsureRange(AValue, -100, 100));
end;

// -----------------------------------------------------------------------------

procedure MPVSetSaturation(const ACore: TMPVCore; const AValue: Integer);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_int64('saturation', EnsureRange(AValue, -100, 100));
end;

// -----------------------------------------------------------------------------

procedure MPVSetHue(const ACore: TMPVCore; const AValue: Integer);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_int64('hue', EnsureRange(AValue, -100, 100));
end;

// -----------------------------------------------------------------------------

procedure MPVSetGamma(const ACore: TMPVCore; const AValue: Integer);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_property_int64('gamma', EnsureRange(AValue, -100, 100));
end;

// -----------------------------------------------------------------------------

{ Deinterlace }

// -----------------------------------------------------------------------------

procedure MPVSetDeinterlace(const ACore: TMPVCore; const AMode: TMPVDeinterlaceMode);
begin
  if not Assigned(ACore) then Exit;
  case AMode of
    dimOff:  ACore.mpv_set_option_string_('deinterlace=no');
    dimOn:   ACore.mpv_set_option_string_('deinterlace=yes');
    dimAuto: ACore.mpv_set_option_string_('deinterlace=auto');
  end;
end;

// -----------------------------------------------------------------------------

procedure MPVSetDeinterlaceAlg(const ACore: TMPVCore; const AAlgorithm: String);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_command_(['vf-add', AAlgorithm]); // p.ej. 'yadif' o 'bwdif'
end;

// -----------------------------------------------------------------------------

{ Cache por tipo de fuente }

// -----------------------------------------------------------------------------

function GetCacheForSource(const AURL: String): Integer;
begin
  if AURL.StartsWith('dvd://', True) or AURL.StartsWith('bd://', True) or
     AURL.StartsWith('bluray://', True) then
    Result := CACHE_TYPE_DVD
  else if AURL.StartsWith('http://', True) or AURL.StartsWith('https://', True) or
          AURL.StartsWith('rtsp://', True) or AURL.StartsWith('rtmp://', True) then
    Result := CACHE_TYPE_INTERNET
  else if AURL.StartsWith('smb://', True) or AURL.StartsWith('nfs://', True) then
    Result := CACHE_TYPE_NETWORK
  else
    Result := CACHE_TYPE_DEFAULT;
end;

// -----------------------------------------------------------------------------

procedure MPVSetCacheSize(const ACore: TMPVCore; const ASourceType: Integer; const ASeconds: Integer);
begin
  if not Assigned(ACore) then Exit;
  case ASourceType of
    CACHE_TYPE_DVD:
      ACore.mpv_set_option_string_('cache-secs=10');
    CACHE_TYPE_INTERNET,
    CACHE_TYPE_NETWORK:
      ACore.mpv_set_option_string_('cache-secs=' + IntToStr(ASeconds));
  else
    ACore.mpv_set_option_string_('cache-secs=5');
  end;
end;

// -----------------------------------------------------------------------------

{ Info de stream }

// -----------------------------------------------------------------------------

function MPVGetVideoInfo(const ACore: TMPVCore): TMPVVideoInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  if not Assigned(ACore) or not ACore.Initialized then Exit;

  Result.Width   := ACore.GetVideoWidth;
  Result.Height  := ACore.GetVideoHeight;
  Result.FPS     := ACore.GetVideoFPS;
  Result.Bitrate := Round(ACore.mpv_get_property_double('video-bitrate'));
  Result.Codec   := ACore.mpv_get_property_string_('video-codec');
  Result.HWDec   := ACore.mpv_get_property_string_('hwdec-current');
  Result.Format  := ACore.mpv_get_property_string_('video-format');
end;

// -----------------------------------------------------------------------------

function MPVGetAudioInfo(const ACore: TMPVCore): TMPVAudioInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  if not Assigned(ACore) or not ACore.Initialized then Exit;

  Result.Channels   := Round(ACore.mpv_get_property_double('audio-params/channel-count'));
  Result.SampleRate := Round(ACore.mpv_get_property_double('audio-params/samplerate'));
  Result.Bitrate    := Round(ACore.mpv_get_property_double('audio-bitrate'));
  Result.Codec      := ACore.mpv_get_property_string_('audio-codec');
  Result.Format     := ACore.mpv_get_property_string_('audio-params/format');
end;

// -----------------------------------------------------------------------------

{ Dispositivos de audio }

// -----------------------------------------------------------------------------

procedure MPVSetAudioOutput(const ACore: TMPVCore; const ADriver: String);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_option_string_('ao=' + ADriver);
end;

// -----------------------------------------------------------------------------

procedure MPVSetAudioDevice(const ACore: TMPVCore; const ADevice: String);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_option_string_('audio-device=' + ADevice);
end;

// -----------------------------------------------------------------------------

function MPVGetAudioDeviceList(const ACore: TMPVCore): TStringList;
var
  H: Pmpv_handle;
  Node: mpv_node;
  i: Integer;
  DeviceName, DeviceDesc: String;
  j: Integer;
  MapKeys: PPChar;
  MapValues: Pmpv_node;
begin
  Result := TStringList.Create;

  if not Assigned(ACore) or not ACore.Initialized then Exit;

  H := ACore.mpv_handle;
  if not Assigned(H) then Exit;

  FillChar(Node, SizeOf(Node), 0);
  if not Assigned(mpv_get_property) then Exit;
  if mpv_get_property(H^, 'audio-device-list', MPV_FORMAT_NODE, @Node) <> MPV_ERROR_SUCCESS then
    Exit;

  try
    if (Node.format = MPV_FORMAT_NODE_ARRAY) and Assigned(Node.u.list) then
    begin
      for i := 0 to Node.u.list^.num - 1 do
      begin
        DeviceName := '';
        DeviceDesc := '';

        if (Node.u.list^.values[i].format = MPV_FORMAT_NODE_MAP) and
           Assigned(Node.u.list^.values[i].u.list) then
        begin
          MapKeys   := Node.u.list^.values[i].u.list^.keys;
          MapValues := Node.u.list^.values[i].u.list^.values;

          for j := 0 to Node.u.list^.values[i].u.list^.num - 1 do
          begin
            if (String(MapKeys[j]) = 'name') and (MapValues[j].format = MPV_FORMAT_STRING) then
              DeviceName := String(MapValues[j].u._string)
            else if (String(MapKeys[j]) = 'description') and (MapValues[j].format = MPV_FORMAT_STRING) then
              DeviceDesc := String(MapValues[j].u._string);
          end;
        end;

        if DeviceName <> '' then
          Result.Add(DeviceName + '=' + DeviceDesc); // Result.Names[i]/Result.ValueFromIndex[i]
      end;
    end;
  except
    on E: Exception do
      {$IFDEF DEBUG}
      Writeln('MPVGetAudioDeviceList EXCEPTION: ', E.ClassName, ': ', E.Message);
      {$ELSE}
      ;
      {$ENDIF}
  end;

  if Assigned(mpv_free_node_contents) then
    mpv_free_node_contents(Node);
end;

// -----------------------------------------------------------------------------

procedure MPVSetAudioNormalize(const ACore: TMPVCore; const AEnable: Boolean);
begin
  if not Assigned(ACore) then Exit;
  if AEnable then
    ACore.mpv_set_option_string_('af=loudnorm')
  else
    ACore.mpv_set_option_string_('af=');
end;

// -----------------------------------------------------------------------------

procedure MPVSetAudioChannels(const ACore: TMPVCore; const AChannels: String);
begin
  if not Assigned(ACore) then Exit;
  ACore.mpv_set_option_string_('audio-channels=' + AChannels);
end;

// -----------------------------------------------------------------------------

end.
