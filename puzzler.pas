unit puzzler;

interface

uses
  easyimage, typex, fastbitmap, commandprocessor,
  graphics, colorblending, colorconversion,
  math, generics.collections, debug, sysutils;

type
  TPuzzleArgs = record
    cx,cy,radiusx,radiusy: nativeint;
  end;

  TpuzzleResult = record
    x,y: nativeint;
    rRating: nativefloat;
  end;


  Tcmd_GetPuzzleError= class(TCommand)
  public
    a: TFastBitmap;
    b: TFastBitmap;
    xo,yo: nativeint;
    rError: nativefloat;
    compare_radius: nativeint;
    maxerror: nativefloat;
    OnlyNegative: boolean;
    procedure Init;override;
    procedure DoExecute;override;
  end;

  Tcmd_PuzzleTogether=class(TCommand)
  public
    puzzle: TFastBitmap;
    piece: TFastBitmap;
    args: TPuzzleArgs;
    result: TPuzzleResult;
    procedure InitExpense;override;
    procedure DoExecute;override;
  end;


function PuzzleTogether(bmPuzzle, bmPiece: TFastBitmap; args: TPuzzleArgs; startx,endx,starty,endy: nativeint; rMaxError: nativefloat): TPuzzleResult;overload;
function PuzzleTogether(bmPuzzle, bmPiece: TFastBitmap; args: TPuzzleArgs): TPuzzleResult;overload;
function PuzzleTogether(bmPuzzle, bmPiece: TFastBitmap; args: TPuzzleArgs; rMaxError: nativefloat): TPuzzleResult;overload;
procedure ProcessMap(bm: TFastBitmap);

implementation


procedure ProcessMap(bm: TFastBitmap);
const
  MAP_COLOR: Tcolor = 88 + (74 shl 8) + (54 shl 16);
var
  x,y: nativeint;
  c: TColor;
  h,hDif,hCompare: THSLNativeFloatColor;
  h1,h2: THSLnativefloatColor;
  rgb,rgb2: TRGBNativefloatColor;
begin
  hCompare.FromColor(MAP_COLOR);

  for y:= 0 to bm.height-1 do begin
    if y mod 2 = 0 then continue;
    for x := 0 to bm.width-1 do begin
      if x mod 2 = 0 then continue;
      h1.FromColor(bm.Canvas.Pixels[x,y]);
      h2.FromColor(bm.Canvas.Pixels[x+1,y+0]);
      if h2.l > h1.l then h1 := h2;
      h2.FromColor(bm.Canvas.Pixels[x+1,y+1]);
      if h2.l > h1.l then h1 := h2;
      h2.FromColor(bm.Canvas.Pixels[x+0,y+1]);
      if h2.l > h1.l then h1 := h2;

      bm.Canvas.Pixels[x+0,y+0] := h1.ToColor;
      bm.Canvas.Pixels[x+1,y+0] := h1.ToColor;
      bm.Canvas.Pixels[x+0,y+1] := h1.ToColor;
      bm.Canvas.Pixels[x+1,y+1] := h1.ToColor;

    end;
  end;
  for y:= 0 to bm.height-1 do begin
    for x := 0 to bm.width-1 do begin
      c := bm.canvas.pixels[x,y];
      h.FromColor(c);
      hDif := h.Compare(hCompare);
      if h.l < (hCompare.l * 0.55) then
        h.l := 0;
      h.l := h.l * power(1-hDif.h,4);
      h.l := h.l * power(1-hDif.s,10);
      h.l := h.l * power(1-hDif.l,1);

      bm.Canvas.Pixels[x,y] := h.ToColor;



    end;
  end;


end;
function PuzzleTogether(bmPuzzle, bmPiece: TFastBitmap; args: TPuzzleArgs; startx,endx,starty,endy: nativeint; rMaxError: nativefloat): TPuzzleResult;
var
  x,y: nativeint;
  e, eMax, eMin: nativefloat;
  c: Tcmd_GetPuzzleError;
  cs: TList<Tcmd_GetPuzzleError>;
  scan_radius: nativeint;
begin
  eMin := 99999 * (64*64);
  eMax := 0;
  result.x := 0;
  result.y := 0;
  bmpuzzle.Canvas.ClearTranslation;

  cs := TList<Tcmd_GetPuzzleError>.Create;
  try
    for y:= 0-args.radiusy to 0 + args.radiusy do begin
//      Debug(inttostr(y));
      for x:= 0-args.radiusx to 0 + args.radiusx do begin
        if (((abs(x) >= startx) and (abs(x) <= endx)) AND ((abs(y) <= endy)))
        OR (((abs(y) >= starty) and (abs(y) <=endy)) AND ((abs(x) <= endx))) then begin
          c := Tcmd_GetPuzzleError.Create;
          c.a := bmPuzzle;
          c.b := bmPiece;
          c.xo := x;
          c.yo := y;
          c.Start;
          c.MaxError := rMAxError;
          c.CPUExpense := 0.1;
          c.onlynegative := false;
          cs.Add(c);
        end;
      end;
    end;
    for x := 0 to cs.Count-1 do begin
      cs[x].WaitFor;
      try
        e := cs[x].rError;
        if e < eMin then begin
          result.x := cs[x].xo;
          result.y := cs[x].yo;
          eMin := e;
        end;
        if e > eMax then begin
          eMax := e;
        end;
      finally
        cs[x].Free;
      end;
    end;
    Debug.Log(nil,'Min-Max: '+floattostr(eMin)+'-'+floattostr(emax));

  finally
    cs.Free;
  end;

  result.rRating := emin;


end;
function PuzzleTogether(bmPuzzle, bmPiece: TFastBitmap; args: TPuzzleArgs; rMaxError: nativefloat): TPuzzleResult;
var
  rx,ry: nativeint;
  res: TPuzzleResult;
begin

  result.rRating := 99999999;
  rx := 0;
  ry := 0;
  repeat
    res := PuzzleTogether(bmPuzzle, bmPiece, args,rx,rx,ry,ry, result.rRating);
    if res.rRating < result.rRating then
      result := res;
    if rx < args.radiusx then
    inc(rx);
    if ry < args.radiusy then
      inc(ry);


  until (res.rRating < rMaxError) or (rx >= args.radiusx) and (ry >=args.radiusy);

  bmpuzzle.Canvas.ClearTranslation;
  bmPuzzle.Canvas.Paste_AutoExpand(bmPiece, 0-result.x, 0-result.y);

end;
function PuzzleTogether(bmPuzzle, bmPiece: TFastBitmap; args: TPuzzleArgs): TPuzzleResult;
var
  x,y: nativeint;
  e, eMax, eMin: nativefloat;
  c: Tcmd_GetPuzzleError;
  cs: TList<Tcmd_GetPuzzleError>;
  scan_radius: nativeint;
begin
  eMin := 99999 * (64*64);
  eMax := 0;
  result.x := 0;
  result.y := 0;
  bmpuzzle.Canvas.ClearTranslation;

  cs := TList<Tcmd_GetPuzzleError>.Create;
  try
    for y:= 0-args.radiusy to 0 + args.radiusy do begin
//      Debug(inttostr(y));
      for x:= 0-args.radiusx to 0 + args.radiusx do begin
        c := Tcmd_GetPuzzleError.Create;
        c.a := bmPuzzle;
        c.b := bmPiece;
        c.xo := x;
        c.yo := y;
        c.Start;
        cs.Add(c);
      end;
    end;
    for x := 0 to cs.Count-1 do begin
      try
        cs[x].WaitFor;
        e := cs[x].rError;
        if e < eMin then begin
          result.x := cs[x].xo;
          result.y := cs[x].yo;
          eMin := e;
        end;
        if e > eMax then begin
          eMax := e;
        end;
      finally
        cs[x].Free;
      end;
    end;
    Debug.Log(nil,'Min-Max: '+floattostr(eMin)+'-'+floattostr(emax));

  finally
    cs.Free;
  end;

  bmpuzzle.Canvas.ClearTranslation;
  bmPuzzle.Canvas.Paste_AutoExpand(bmPiece, 0-result.x, 0-result.y);

end;



procedure Tcmd_GetPuzzleError.DoExecute;
var
  c1,c2: THSLNativefloatColor;
  cx,cy,x,y: nativeint;
begin
  inherited;

  rError := 0;
  cx := a.width div 2;
  cy := a.height div 2;

  for y:= cy-COMPARE_RADIUS to cy+COMPARE_RADIUS do begin
    for x:= cx-COMPARE_RADIUS to cx+COMPARE_RADIUS do begin
      c1.FromColor(a.canvas.pixels[x,y]);
      c2.FromColor(b.canvas.pixels[x+xo,y+yo]);
      if (not onlynegative) or (c2.l < c1.l) then
        rError := rError + power(abs(c1.l-c2.l),2);
      if rError > Maxerror then
        break;

    end;
  end;
end;

procedure Tcmd_GetPuzzleError.Init;
begin
  inherited;
  compare_radius := 90;
  maxerror := 9999999;
end;

{ Tcmd_PuzzleTogether }

procedure Tcmd_PuzzleTogether.DoExecute;
begin
  inherited;
  result := PuzzleTogether(puzzle, piece, args, 0.3);
end;

procedure Tcmd_PuzzleTogether.InitExpense;
begin
  inherited;
  CPUExpense := 0.0;
end;



end.
