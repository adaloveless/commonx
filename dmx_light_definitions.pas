unit dmx_light_definitions;
{$DEFINE DIMMERS}
{$DEFINE ENABLE_ARTNET_LIGHTS}
{$DEFINE SMALL_WASHES}
{$DEFINE HOME_LIGHTS}
{x$DEFINE REMOTE_ARTNET}
{x$DEFINE TWASH}

interface
uses
  dmx_objects, types, classes, applicationparams;


{x$DEFINE SMALL_WASHES}

procedure CreateLights;

implementation



procedure CreateLights;
var
  cb: TDMXColorBar;
  t,u: integer;
  X: integer;
  l,l2: TDMXPixellicious;
  l3,l4: TDMXPixellicious2;
  uv: TDMxUniverse;
begin

// --big washes
{$IFDEF HOME_LIGHTS}
  FirstUniverse.Add(TDMXPro38B, 54, 4, 3, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXPro38B, 51, -4, 2, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
{$ENDIF}


//  Multiverse.Add(TDMXchauvetPar56, 91, -3, 0, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(Multiverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(Multiverse.NewestLight);
//
//  Multiverse.Add(TDMXchauvetPar56, 81, -3, 2, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(Multiverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(Multiverse.NewestLight);
//
//  Multiverse.Add(TDMXchauvetPar56, 1, -3, 4, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(Multiverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(Multiverse.NewestLight);
//
//  Multiverse.Add(TDMXchauvetPar56, 91 + 256, +3, 0, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(Multiverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(Multiverse.NewestLight);
//
//  Multiverse.Add(TDMXchauvetPar56, 61 + 256, +3, 2, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(Multiverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(Multiverse.NewestLight);
//
//  Multiverse.Add(TDMXchauvetPar56, 51 + 256, +3, 4, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(Multiverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(Multiverse.NewestLight);



  // optitris
{
  for t:= 0 to 5 do begin
    if t >= 3 then
      x := -3
    else
      x := 3;

    Multiverse.Add(TDMXPro38B, 275+t*3, x, 2+t, GROUP_BLINDER);
    Multiverse.Groups['LiveWash'].Add(Multiverse.NewestLight);
    Multiverse.Groups['LiveCore'].Add(Multiverse.NewestLight);
  end;
}


{$IFDEF DIMMERS}
  FirstUniverse.Add(TDMXDimmer, 509, 0, 0, 0);
  Multiverse.Groups['Dimmer'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXDimmer, 510, 0, 0, 0);
  Multiverse.Groups['Dimmer'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXDimmer, 511, 0, 0, 0);
  Multiverse.Groups['Dimmer'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXDimmer, 512, 0, 0, 0);
  Multiverse.Groups['Dimmer'].Add(FirstUniverse.NewestLight);
{$ENDIF}

  FirstUniverse.Add(TDMXCaliente, 191, 0, 0, 0);
  Multiverse.Groups['Caliente'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMX4Banger, 1, 0, 0, 0);
  Multiverse.Groups['4Banger'].Add(FirstUniverse.NewestLight);


//  Multiverse.Add(TDMXCaliente, 191, 0, 0, 0);
//  Multiverse.Groups['Caliente'].Add(Multiverse.NewestLight);



{$IFDEF BARRYS}
  FirstUniverse.Add(TDMXRGBCluster, 51, 1, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWashR'].Add(FirstUniverse.NewestLight);


  FirstUniverse.Add(TDMXRGBCluster, 57, 2, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWashR'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 63, 3, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWashR'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 111, 4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWashR'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 400, -1, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWashL'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 410, -2, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWashL'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 420, -3, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWashL'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 426, -4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWashL'].Add(FirstUniverse.NewestLight);
{$ENDIF}

{$IFDEF HOME_LIGHTS}
  FirstUniverse.Add(TDMXColorDash, 111, -4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);


  FirstUniverse.Add(TDMXColorDash, 115, +4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
{$ENDIF}

  // big spots
  FirstUniverse.Add(TDMXQSpot14, 200, -3, -2, GROUP_SPOT);
//  Multiverse.Groups['LiveSpotColor'].Add(TDMXQSpot14(FirstUniverse.NewestLight).Color);
//  Multiverse.Groups['LiveSpotInt'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['Spot'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['qspot'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['spotL'].Add(TDMXQSpot14(FirstUniverse.NewestLight));

  FirstUniverse.Add(TDMXQSpot14, 220, 0, -2, GROUP_SPOT);
//  Multiverse.Groups['LiveSpotColor'].Add(TDMXQSpot14(FirstUniverse.NewestLight).Color);
//  Multiverse.Groups['LiveSpotInt'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['Spot'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['qspot'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['spotC'].Add(TDMXQSpot14(FirstUniverse.NewestLight));

  FirstUniverse.Add(TDMXQSpot14, 120, 3, -2, GROUP_SPOT);
//  Multiverse.Groups['LiveSpotColor'].Add(TDMXQSpot14(FirstUniverse.NewestLight).Color);
//  Multiverse.Groups['LiveSpotInt'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['Spot'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['qspot'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['spotR'].Add(TDMXQSpot14(FirstUniverse.NewestLight));


//  FirstUniverse.Add(TDMXFlurryQ, 305, -4,-2, GROUP_WASH);
//  Multiverse.Groups['MovingWash'].Add(TDMXFlurryQ(FirstUniverse.NewestLight));
//  fspots.add(TDMXFlurryQ(FirstUniverse.NewestLight).PanTilt);
//  Multiverse.Groups['GalaWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['Movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));


  FirstUniverse.Add(TDMXFlurryQ, 329, +4,-2, GROUP_WASH);
  Multiverse.Groups['MovingWash'].Add(TDMXFlurryQ(FirstUniverse.NewestLight));
  Multiverse.Groups['GalaWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));

  FirstUniverse.Add(TDMXFlurryQ, 317, +6,-2, GROUP_WASH);
  Multiverse.Groups['MovingWash'].Add(TDMXFlurryQ(FirstUniverse.NewestLight));
  Multiverse.Groups['GalaWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));



  cb := FirstUniverse.Add(TDMXColorBar, 134, -4, 5, GROUP_BLINDER) as TDMXColorBar;
  Multiverse.Groups['BarGroups'].Add(FirstUniverse.NewestLight);
  for t := 0 to 7 do begin
//    Multiverse.Groups['LiveBlinders'].Add(cb.sections[t]);
//    Multiverse.Groups['LiveCore'].Add(cb.sections[t]);
//    Multiverse.Groups['Wall'].Add(cb.sections[t]);
    Multiverse.Groups['Bars'].Add(cb.sections[t]);
  end;


  cb := FirstUniverse.Add(TDMXColorBar, 162, 4, 5, GROUP_BLINDER) as TDMXColorBar;
  Multiverse.Groups['BarGroups'].Add(FirstUniverse.NewestLight);
  for t := 0 to 7 do begin
//    Multiverse.Groups['LiveBlinders'].Add(cb.sections[t]);
//    Multiverse.Groups['LiveCore'].Add(cb.sections[t]);
//    Multiverse.Groups['Wall'].Add(cb.sections[t]);
    Multiverse.Groups['Bars'].Add(cb.sections[t]);
  end;

  cb := FirstUniverse.Add(TDMXColorBar, 257, 2, 0, GROUP_BLINDER) as TDMXColorBar;
  Multiverse.Groups['BarGroups'].Add(FirstUniverse.NewestLight);
  for t := 0 to 7 do begin
//    Multiverse.Groups['LiveBlinders'].Add(cb.sections[t]);
//    Multiverse.Groups['LiveCore'].Add(cb.sections[t]);
//    Multiverse.Groups['Wall'].Add(cb.sections[t]);
    Multiverse.Groups['Bars'].Add(cb.sections[t]);
  end;

  cb := FirstUniverse.Add(TDMXColorBar, 281, -2, 0, GROUP_BLINDER) as TDMXColorBar;
  Multiverse.Groups['BarGroups'].Add(FirstUniverse.NewestLight);
  for t := 0 to 7 do begin
//    Multiverse.Groups['LiveBlinders'].Add(cb.sections[t]);
//    Multiverse.Groups['LiveCore'].Add(cb.sections[t]);
//    Multiverse.Groups['Wall'].Add(cb.sections[t]);
    Multiverse.Groups['Bars'].Add(cb.sections[t]);
  end;


// small washes
{$IFDEF SMALL_WASHES}
  FirstUniverse.Add(TDMXColorDashAccent, 61, 2, 2, GROUP_WASH);
//  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXColorDashAccent, 57, 3, 0, GROUP_WASH);
//  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
//  Multiverse.Add(TDMXColorDashAccent, 257 + (0 * 4), 3 - 5, 2, GROUP_WASH);
//  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXColorDashAccent, 65,  2.5,0, GROUP_WASH);
//  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXColorDashAccent, 69,  -1, 2, GROUP_WASH);
//  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXColorDashAccent, 73,  -1.5, 0, GROUP_WASH);
//  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXColorDashAccent, 95,  -2.5, 0, GROUP_WASH);
//  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);



//  FirstUniverse.add(TDMXColorDashAccent, 257 + (3 * 4), 9 - 5, 2, GROUP_WASH);
//  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
{$ENDIF}

  FirstUniverse.add(TDMXScorpion, 80, 0, 0, GROUP_LASER);
  Multiverse.Groups['laser'].Add(FirstUniverse.NewestLight);
  FirstUniverse.add(TDMXVue, 87, -2, 0, GROUP_BEAM);
  Multiverse.Groups['vue'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['beamfx'].Add(FirstUniverse.NewestLight);
  FirstUniverse.add(TDMXElan, 33, 2, 0, GROUP_BEAM);
  Multiverse.Groups['elan'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['beamfx'].Add(FirstUniverse.NewestLight);
  FirstUniverse.add(TDMXnucleus, 353, 0, 2, GROUP_BEAM);
  Multiverse.Groups['nuke'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['beamfx'].Add(FirstUniverse.NewestLight);

  FirstUniverse.add(TDMXStrobe, 37, 0, 0, GROUP_STROBE);
// trackme := strobe;
// trackme.DebugFlags := 666;
  Multiverse.Groups['strobe'].Add(FirstUniverse.NewestLight);


  FirstUniverse.add(TDMXSKyLaser, 45, 0, 0, 0);

  //bluelaser := TDMXBlueGalaxy(FirstUniverse.add(TDMXBlueGalaxy, 366, 0,0,0));

//  FirstUniverse.add(TDMXLedGrid, 257, 0, 0, 0);
//  Multiverse.Groups['Grid'].Add(FirstUniverse.NewestLight);
//  for t:= 0 to 15 do begin
//    for u := 0 to 15 do begin
//      Multiverse.Groups['GridPoint'].Add(TDMXLEDGrid(FirstUniverse.NewestLight).Grid[t,u]);
//    end;
//  end;


  uv := TDMXUniverse.Create;
{$IFDEF REMOTE_ARTNET}
  uv.Host := APGEt('DMXHost', '');
{$ENDIF}
  uv.ArtNetIP := '2.0.0.1';


{$IFDEF ENABLE_ARTNET_LIGHTS}
  //add a light at channel 1 of the artnet
  l := uv.Add(TDMXPixellicious, {ch=} 1 , {x=} 10, {y=}-5 , {flags=} 0 , '' {artnet ip}) as TDMXPixellicious;
  Multiverse.Groups['Artnet'].Add(l);
  Multiverse.Groups['ArtnetBar'].Add(l);
  l.orientation := lo90Left;
  l.pixeloffset := point(4,12);


  uv := TDMXUniverse.Create;
{$IFDEF REMOTE_ARTNET}
  uv.Host := APGEt('DMXHost', '');
{$ENDIF}
  uv.ArtNetIP := '2.0.0.2';


  //add a light at channel 1 of the artnet
  l2 := uv.Add(TDMXPixellicious, {ch=} 1 , {x=} -10, {y=}-5 , {flags=} 0 , '' {artnet ip}) as TDMXPixellicious;
  Multiverse.Groups['Artnet'].Add(l2);
  Multiverse.Groups['ArtnetBar'].Add(l2);
  l2.orientation := lo90Left;
  l2.pixeloffset := point(16,12);


  uv := TDMXUniverse.Create;
{$IFDEF REMOTE_ARTNET}
  uv.Host := APGEt('DMXHost', '');
{$ENDIF}
  uv.ArtNetIP := '2.0.0.4';


  l3 := uv.Add(TDMXPixellicious2, {ch=} 1 , {x=} -10, {y=}5 , {flags=} 0 , '' {artnet ip}) as TDMXPixellicious2;
  //l3.orientation := TLightOrientation.lo180;
  Multiverse.Groups['Artnet'].Add(l3);
  Multiverse.Groups['ArtnetSquare'].Add(l3);
  l3.pixeloffset := point(0,0);


  uv := TDMXUniverse.Create;
{$IFDEF REMOTE_ARTNET}
  uv.Host := APGEt('DMXHost', '');
{$ENDIF}
  uv.ArtNetIP := '2.0.0.3';


  l4 := uv.Add(TDMXPixellicious2, {ch=} 1 , {x=} 10, {y=}5 , {flags=} 0 , '' {artnet ip}) as TDMXPixellicious2;
  //l4.orientation := TLightOrientation.lo180;
  Multiverse.Groups['Artnet'].Add(l4);
  Multiverse.Groups['ArtnetSquare'].Add(l4);
  l4.pixeloffset := point(12,0);
{$ENDIF}

end;


end.
