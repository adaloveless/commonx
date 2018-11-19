unit BrainScanUltra;
{$D-}
interface

{$IFDEF LINUX}
type
  TChart = TObject;
{$ELSE}
uses VCLTee.Chart, graphics, sysutils;
{$ENDIF}



procedure MemChart(chart1: TChart);
procedure MemBlockChart(chart1: TChart);
implementation

{$IFNDEF NOSUPERMEM}
uses BigBrainUltra;
{$ENDIF}

{$IFNDEF LINUX}
function ColorBlend(cBackGround, cForeGround: Tcolor; alpha: real): TColor;
var
  r1,g1,b1,r2,g2,b2: integer;
  r,g,b: integer;
begin
  r1 := (cbackGround shr 0) and 255;
  g1 := (cbackGround shr 8) and 255;
  b1 := (cbackGround shr 16) and 255;

  r2 := (cForeGround shr 0) and 255;
  g2 := (cForeGround shr 8) and 255;
  b2 := (cForeGround shr 16) and 255;


  r := round(((r2-r1)*alpha)+r1);
  g := round(((g2-g1)*alpha)+g1);
  b := round(((b2-b1)*alpha)+b1);

  result := (b shl 16)+(g shl 8)+r;

end;





function GetChartColor(x,y: integer; isFree: boolean; bEmptying: boolean): TColor;
type
  Tcolors = array [0..12] of TColor;
const
  colors: Tcolors = ($0000FF, $00FF00,$FF0000, $FF00FF, $FFFF00, $00FFFF, $AAAAAA, $FF7F00, $7FFF00, $00FF7F, $007FFF, $7F00FF, $FF007F);
var
  r,g,b: integer;
begin
  x := x mod 12;

  result := colors[x];

  case y of
    0: begin
      b := (result shr 16) and 255;
      g := (result shr 8) and 255;
      r := (result shr 0) and 255;

      r := r+((255-r) div 2);
      g := g+((255-g) div 2);
      b := b+((255-b) div 2);

      result := (b shl 16)+(g shl 8)+r;
    end;
    1: begin
    end;
    2: begin
      result := ((((result shr 16) and 255) div 2) shl 16)
             or ((((result shr 8) and 255) div 2) shl 8)
             or ((((result shr 0) and 255) div 2) shl 0)
    end;
    3: begin
      result := ((((result shr 16) and 255) div 3) shl 16)
             or ((((result shr 8) and 255) div 3) shl 8)
             or ((((result shr 0) and 255) div 3) shl 0)

    end;

  end;


  if bEmptying then
    result := ColorBlend(result, clWhite, 0.65)
  else if IsFree then
    result := ColorBlend(result, clBlack, 0.65);

end;
{$ENDIF}


procedure MemChart(chart1: TChart);
{$IFNDEF NOSUPERMEM}
var
  t: integer;
  b: boolean;
  b2: boolean;
{$ENDIF}
begin
{$IFNDEF NOSUPERMEM}
{$IFNDEF LINUX}

  ManMan.Lock;
//  if ManMan.TryLock then
  try

    Chart1.SeriesList[0].Clear;
    for t:= 0 to ManMan.ManagerCount-1 do begin
      Chart1.Title.text.text := 'Memory distribution across '+inttostr(ManMan.ManagerCount)+' managers '+' ('+inttostr(ManMan.DeadCount)+' dead managers not shown)';
      ManMan.Managers[t].Lock;
//      if ManMan.Managers[t].TryLock then
      try
        b := ManMan.Managers[t].IsFree;
        b2 := ManMan.Managers[t].Emptying;
        Chart1.SeriesList[0].Add(ManMan.Managers[t].BlockHeaderOverhead,inttostr(TThreadBlockManager(ManMAn.Managers[t]).LastOwnedThreadID)+':Overhead', GetChartColor(t, 0, b, b2));
        Chart1.SeriesList[0].Add(ManMan.Managers[t].UsedBytes,inttostr(TThreadBlockManager(ManMAn.Managers[t]).LastOwnedThreadID)+':Used', GetChartColor(t, 1, b, b2));
        Chart1.SeriesList[0].Add(ManMan.Managers[t].WasteBytes,inttostr(TThreadBlockManager(ManMAn.Managers[t]).LastOwnedThreadID)+':Waste', GetChartColor(t, 2,b, b2));
        Chart1.SeriesList[0].Add(ManMan.Managers[t].FreeBytes,inttostr(TThreadBlockManager(ManMAn.Managers[t]).LastOwnedThreadID)+':Free', GetChartColor(t, 3,b, b2));
//        TPieSeries(Chart1.Serieslist[0]).RotationAngle := TPieSeries(Chart1.Serieslist[0]).RotationAngle+1
        //ManMan.Managers[t].checkMem;
      finally
        ManMan.Managers[t].Unlock;
      end;
    end;

  finally
    ManMan.Unlock;
  end;
{$ENDIF}
{$ENDIF}
end;

procedure MemBlockChart(chart1: TChart);
{$IFNDEF NOSUPERMEM}
var
  t: integer;
{$ENDIF}
begin
{$IFNDEF NOSUPERMEM}
  Chart1.SeriesList[0].Clear;
  ManMan.Lock;
  try
    for t:= 0 to ManMan.ManagerCount-1 do begin
      Chart1.Title.text.text := 'Memory BLOCK distribution across '+inttostr(ManMan.ManagerCount)+' managers '+' ('+inttostr(ManMan.DeadCount)+' dead managers not shown)';
      ManMan.Managers[t].Lock;
      try
        Chart1.SeriesList[0].Add(ManMan.Managers[t].UsedBlockCount,'('+inttostr(t)+')', GetChartColor(t, 1,false,false));
        Chart1.SeriesList[0].Add(ManMan.Managers[t].FreeBlockCount,'('+inttostr(t)+')', GetChartColor(t, 1,true,false));

//        TPieSeries(Chart1.Serieslist[0]).RotationAngle := TPieSeries(Chart1.Serieslist[0]).RotationAngle+1
        //ManMan.Managers[t].checkMem;
      finally
        ManMan.Managers[t].Unlock;
      end;
    end;

    ManMan.Clean;

  finally
    ManMan.Unlock;
  end;
{$ENDIF}
end;

end.

