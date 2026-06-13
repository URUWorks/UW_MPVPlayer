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

unit MPVPlayer.Filters;

// -----------------------------------------------------------------------------

interface

// -----------------------------------------------------------------------------

type

  { Basic Video Filters }

  TMPVPlayerVideoFilter =
  (
    vfHFlip,
    vfVFlip,
    vfSharpen,
    vfBlur,
    vfEdgeEnhance,
    vfEmboss,
    vfNegative,
    vfVintage,
    vfDeinterlace,
    vfHistogram,
    vfOscilloscope
  );

  TMPVPlayerVideoFilters = set of TMPVPlayerVideoFilter;

  { Basic Audio Filters }

  TMPVPlayerAudioFilter =
  (
    afDialoguEnhance,
    afFlanger,
    afSurround,
    afSpeechNormalizer,
    afTremolo,
    afVibrato
  );

  TMPVPlayerAudioFilters = set of TMPVPlayerAudioFilter;

  { Basic Info Filter }

  TMPVPlayerFilterInfo = record
    Name   : String;
    Params : String;
  end;

const

  TMPVPlayerVideoFiltersInfo : array[0..10] of TMPVPlayerFilterInfo =
  (
    // vfHFlip
    (Name: 'hflip'; Params: ''),
    // vfVFlip
    (Name: 'vflip'; Params: ''),
    // vfSharpen
    (Name: 'convolution'; Params: '0 -1 0 -1 5 -1 0 -1 0:0 -1 0 -1 5 -1 0 -1 0:0 -1 0 -1 5 -1 0 -1 0:0 -1 0 -1 5 -1 0 -1 0'),
    // vfBlur
    (Name: 'convolution'; Params: '1 1 1 1 1 1 1 1 1:1 1 1 1 1 1 1 1 1:1 1 1 1 1 1 1 1 1:1 1 1 1 1 1 1 1 1:1/9:1/9:1/9:1/9'),
    // vfEdgeEnhance
    (Name: 'convolution'; Params: '1 1 1 1 -8 1 1 1 1:1 1 1 1 -8 1 1 1 1:1 1 1 1 -8 1 1 1 1:1 1 1 1 -8 1 1 1 1:5:5:5:1:0:128:128:0'),
    // vfEmboss
    (Name: 'convolution'; Params: '-2 -1 0 -1 1 1 0 1 2:-2 -1 0 -1 1 1 0 1 2:-2 -1 0 -1 1 1 0 1 2:-2 -1 0 -1 1 1 0 1 2'),
    // vfNegative
    (Name: 'curves'; Params: 'preset=color_negative'),
    // vfVintage
    (Name: 'curves'; Params: 'preset=vintage'),
    // vfDeinterlace
    (Name: 'bwdif'; Params: ''),
    // vfHistogram
    (Name: 'histogram'; Params: ''),
    // vfOscilloscope
    (Name: 'oscilloscope'; Params: 'x=0.5:y=0:s=1')
  );

  TMPVPlayerAudioFiltersInfo : array[0..5] of TMPVPlayerFilterInfo =
  (
    // afDialoguenhance
    (Name: 'dialoguenhance'; Params: ''),
    // afFlanger
    (Name: 'flanger'; Params: ''),
    // afSurround
    (Name: 'surround'; Params: ''),
    // afSpeechNormalizer
    (Name: 'speechnorm'; Params: ''),
    // afTremolo
    (Name: 'tremolo'; Params: ''),
    // afVibrato
    (Name: 'vibrato'; Params: '')
  );

const
  // Safe Guides: Action Safe (5%) + Title Safe (dinámico)
  LavfiActionSafe = 'drawbox=x=iw*0.05:y=ih*0.05:w=iw*0.90:h=ih*0.90:t=3:color=Red';
  LavfiTitleSafe  = 'drawbox=x=iw*%.2f:y=ih*%.2f:w=iw*%.2f:h=ih*%.2f:t=3:color=Green';

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

end.

