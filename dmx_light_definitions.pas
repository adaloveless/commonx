unit dmx_light_definitions;

interface
uses
  dmx_objects;

{$DEFINE TWASH}

{x$DEFINE SMALL_WASHES}

procedure CreateLights;

implementation



procedure CreateLights;
var
  cb: TDMXColorBar;
  t: nativeint;
  fusion: TDMXFusionBar;
begin
// --big washes
{$IFDEF PRO38}
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


//  FirstUniverse.Add(TDMXchauvetPar56, 91, -3, 0, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
//
//  FirstUniverse.Add(TDMXchauvetPar56, 81, -3, 2, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
//
//  FirstUniverse.Add(TDMXchauvetPar56, 1, -3, 4, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
//
//  FirstUniverse.Add(TDMXchauvetPar56, 91 + 256, +3, 0, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
//
//  FirstUniverse.Add(TDMXchauvetPar56, 61 + 256, +3, 2, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
//
//  FirstUniverse.Add(TDMXchauvetPar56, 51 + 256, +3, 4, GROUP_WASH);
//  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);



  // optitris
{
  for t:= 0 to 5 do begin
    if t >= 3 then
      x := -3
    else
      x := 3;

    FirstUniverse.Add(TDMXPro38B, 275+t*3, x, 2+t, GROUP_BLINDER);
    Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
    Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  end;
}

{$IFDEF TWASH}
  FirstUniverse.Add(TDMXRGBCluster, 51, -4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 57, -4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 63, -4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 111, 4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXRGBCluster, 400, +4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 410, +4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 420, +4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);

  FirstUniverse.Add(TDMXRGBCluster, 426, +4, 0, GROUP_WASH);
  Multiverse.Groups['LiveWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['BigWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Blinders'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['TWash'].Add(FirstUniverse.NewestLight);
{$ENDIF}



  // big spots
  // big spots
  FirstUniverse.Add(TDMXQSpot14, 200, -3, 3, GROUP_SPOT);
  Multiverse.Groups['LiveSpotColor'].Add(TDMXQSpot14(FirstUniverse.NewestLight).Color);
  Multiverse.Groups['LiveSpotInt'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['Spot'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['Movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));


  FirstUniverse.Add(TDMXQSpot14, 220, 3, 3, GROUP_SPOT);
  Multiverse.Groups['LiveSpotColor'].Add(TDMXQSpot14(FirstUniverse.NewestLight).Color);
  Multiverse.Groups['LiveSpotInt'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['Spot'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['Movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));

  FirstUniverse.Add(TDMXQSpot14, 120, 4, 4, GROUP_SPOT);
  Multiverse.Groups['LiveSpotColor'].Add(TDMXQSpot14(FirstUniverse.NewestLight).Color);
  Multiverse.Groups['LiveSpotInt'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['Spot'].Add(TDMXQSpot14(FirstUniverse.NewestLight));
  Multiverse.Groups['Movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));


  FirstUniverse.Add(TDMXFlurryQ, 305, -4,-2, GROUP_WASH);
  Multiverse.Groups['MovingWash'].Add(TDMXFlurryQ(FirstUniverse.NewestLight));
  Multiverse.Groups['Movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));

  FirstUniverse.Add(TDMXFlurryQ, 317, +4,-2, GROUP_WASH);
  Multiverse.Groups['MovingWash'].Add(TDMXFlurryQ(FirstUniverse.NewestLight));
  Multiverse.Groups['Movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));

  FirstUniverse.Add(TDMXFlurryQ, 329, +6,-2, GROUP_WASH);
  Multiverse.Groups['MovingWash'].Add(TDMXFlurryQ(FirstUniverse.NewestLight));
  Multiverse.Groups['Movers'].Add(TDMXQSpot14(FirstUniverse.NewestLight));

  cb := FirstUniverse.Add(TDMXColorBar, 134, -4, 5, GROUP_BLINDER) as TDMXColorBar;
  for t := 0 to 7 do begin
    Multiverse.Groups['LiveBlinders'].Add(cb.sections[t]);
    Multiverse.Groups['LiveCore'].Add(cb.sections[t]);
    Multiverse.Groups['Wall'].Add(cb.sections[t]);
    Multiverse.Groups['Bars'].Add(cb.sections[t]);
  end;


  cb := FirstUniverse.Add(TDMXColorBar, 162, -6, 5, GROUP_BLINDER) as TDMXColorBar;
  for t := 0 to 7 do begin
    Multiverse.Groups['LiveBlinders'].Add(cb.sections[t]);
    Multiverse.Groups['LiveCore'].Add(cb.sections[t]);
    Multiverse.Groups['Wall'].Add(cb.sections[t]);
    Multiverse.Groups['Bars'].Add(cb.sections[t]);
  end;

  cb := FirstUniverse.Add(TDMXColorBar, 257, 4, 0, GROUP_BLINDER) as TDMXColorBar;
  for t := 0 to 7 do begin
    Multiverse.Groups['LiveBlinders'].Add(cb.sections[t]);
    Multiverse.Groups['LiveCore'].Add(cb.sections[t]);
    Multiverse.Groups['Wall'].Add(cb.sections[t]);
    Multiverse.Groups['Bars'].Add(cb.sections[t]);
  end;

  cb := FirstUniverse.Add(TDMXColorBar, 281, 6, 0, GROUP_BLINDER) as TDMXColorBar;
  for t := 0 to 7 do begin
    Multiverse.Groups['LiveBlinders'].Add(cb.sections[t]);
    Multiverse.Groups['LiveCore'].Add(cb.sections[t]);
    Multiverse.Groups['Wall'].Add(cb.sections[t]);
    Multiverse.Groups['Bars'].Add(cb.sections[t]);
  end;


// small washes
{$IFDEF SMALL_WASHES}
  FirstUniverse.Add(TDMXColorDashAccent, 61, 2, 2, GROUP_WASH);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXColorDashAccent, 57, 3, 0, GROUP_WASH);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
//  FirstUniverse.Add(TDMXColorDashAccent, 257 + (0 * 4), 3 - 5, 2, GROUP_WASH);
//  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXColorDashAccent, 65,  2.5,0, GROUP_WASH);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXColorDashAccent, 69,  -1, 2, GROUP_WASH);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXColorDashAccent, 73,  -1.5, 0, GROUP_WASH);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXColorDashAccent, 95,  -2.5, 0, GROUP_WASH);
  Multiverse.Groups['Wall'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['Wash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['SmallWash'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);



//  FirstUniverse.Add(TDMXColorDashAccent, 257 + (3 * 4), 9 - 5, 2, GROUP_WASH);
//  Multiverse.Groups['LiveDrum'].Add(FirstUniverse.NewestLight);
//  Multiverse.Groups['LiveCore'].Add(FirstUniverse.NewestLight);
{$ENDIF}
  FirstUniverse.Add(TDMXScorpion, 80, 0, 0, GROUP_LASER);
  Multiverse.Groups['laser'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXVue, 87, -2, 0, GROUP_BEAM);
  Multiverse.Groups['vue'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['beamfx'].Add(FirstUniverse.NewestLight);
  FirstUniverse.Add(TDMXElan, 33, 2, 0, GROUP_BEAM);
  Multiverse.Groups['elan'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['beamfx'].Add(FirstUniverse.NewestLight);
{$IFDEF NUKE}
  FirstUniverse.Add(TDMXnucleus, 1, 0, 2, GROUP_BEAM);
  Multiverse.Groups['nuke'].Add(FirstUniverse.NewestLight);
  Multiverse.Groups['beamfx'].Add(FirstUniverse.NewestLight);
{$ENDIF}

  FirstUniverse.Add(TDMXStrobe, 37, 0, 0, GROUP_STROBE);
// trackme := strobe;
// trackme.DebugFlags := 666;
  Multiverse.Groups['strobe'].Add(FirstUniverse.NewestLight);

  fusion := TDMXFusionBar(FirstUniverse.Add(TDMXFusionBar, 190, 0, 1,
      GROUP_BLINDER + GROUP_SPOT));
  Multiverse.Groups['LiveWash'].Add(fusion.wash);//don't copy this line
  Multiverse.Groups['LiveCore'].Add(fusion.wash);//don't copy this line
  Multiverse.Groups['Wash'].Add(fusion.wash);//don't copy this line
  Multiverse.Groups['BigWash'].Add(fusion.wash);//don't copy this line
  Multiverse.Groups['Wall'].Add(fusion.wash);//don't copy this line
  Multiverse.Groups['Blinder'].Add(fusion.wash);//don't copy this line


//  FirstUniverse.Add(TDMXLedGrid, 257, 0, 0, 0);
//  Multiverse.Groups['Grid'].Add(FirstUniverse.NewestLight);
//  for t:= 0 to 15 do begin
//    for u := 0 to 15 do begin
//      Multiverse.Groups['GridPoint'].Add(TDMXLEDGrid(FirstUniverse.NewestLight).Grid[t,u]);
//    end;
//  end;

end;

end.
