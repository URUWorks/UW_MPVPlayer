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
    vfHistogram,
    vfOscilloscope,
    vfSafeAreaLines
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
    (Name: 'hflip'; Params: ''), // vfHFlip
    (Name: 'vflip'; Params: ''), // vfVFlip
    (Name: 'convolution'; Params: '0 -1 0 -1 5 -1 0 -1 0:0 -1 0 -1 5 -1 0 -1 0:0 -1 0 -1 5 -1 0 -1 0:0 -1 0 -1 5 -1 0 -1 0'), // vfSharpen
    (Name: 'convolution'; Params: '1 1 1 1 1 1 1 1 1:1 1 1 1 1 1 1 1 1:1 1 1 1 1 1 1 1 1:1 1 1 1 1 1 1 1 1:1/9:1/9:1/9:1/9'), // vfBlur
    (Name: 'convolution'; Params: '1 1 1 1 -8 1 1 1 1:1 1 1 1 -8 1 1 1 1:1 1 1 1 -8 1 1 1 1:1 1 1 1 -8 1 1 1 1:5:5:5:1:0:128:128:0'), // vfEdgeEnhance
    (Name: 'convolution'; Params: '-2 -1 0 -1 1 1 0 1 2:-2 -1 0 -1 1 1 0 1 2:-2 -1 0 -1 1 1 0 1 2:-2 -1 0 -1 1 1 0 1 2'), // vfEmboss
    (Name: 'curves'; Params: 'preset=color_negative'), // vfNegative
    (Name: 'curves'; Params: 'preset=vintage'), // vfVintage
    (Name: 'histogram'; Params: ''), // vfHistogram
    (Name: 'oscilloscope'; Params: 'x=0.5:y=0:s=1'), // vfOscilloscope
    (Name: 'drawbox'; Params: 'x=(iw-iw*0.95)/2:y=(ih-ih*0.95)/2:w=iw*0.95:h=ih*0.95:t=1:color=Red@0.4,drawbox=x=(iw-iw*0.9)/2:y=(ih-ih*0.9)/2:w=iw*0.9:h=ih*0.9:t=1:color=Green@0.6') // vfSafeAreaLines
  );

  TMPVPlayerAudioFiltersInfo : array[0..5] of TMPVPlayerFilterInfo =
  (
    (Name: 'dialoguenhance'; Params: ''), // afDialoguenhance
    (Name: 'flanger'; Params: ''), // afFlanger
    (Name: 'surround'; Params: ''), // afSurround
    (Name: 'speechnorm'; Params: ''), // afSpeechNormalizer
    (Name: 'tremolo'; Params: ''), // afTremolo
    (Name: 'vibrato'; Params: '') // afVibrato
  );

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

end.

