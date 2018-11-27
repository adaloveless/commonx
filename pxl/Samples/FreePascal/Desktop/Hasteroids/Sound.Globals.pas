unit Sound.Globals;
{
  This file is part of Asphyre Framework, also known as Pascal eXtended Library (PXL).
  Copyright (c) 2000 - 2015  Yuriy Kotsarenko

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.
}
interface

{$INCLUDE PXL.Config.inc}

{ Special note: this code was ported multiple times from earliest framework releases predating Asphyre. }

uses
  bass;

var
  MusicModule: HMusic = 0;
  EffectSamples: array[0..3] of HSample;

procedure PlaySample(const Sample: HSample; const Volume: Integer);

implementation

procedure PlaySample(const Sample: HSample; const Volume: Integer);
var
  Channel: HChannel;
begin
  Channel := BASS_SampleGetChannel(Sample, False);
  if Channel <> 0 then
  begin
    BASS_ChannelSetAttribute(Channel, BASS_ATTRIB_VOL, Volume / 100.0);
    BASS_ChannelPlay(Channel, True);
  end;
end;

end.
