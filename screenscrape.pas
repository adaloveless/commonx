unit screenscrape;
{$INLINE AUTO}

interface

uses
  systemx, sysutils, classes,forms, graphicsx,
  mouse, windows, graphics, betterobject,
  generics.collections.fixed, easyimage, dir, dirfile, stringx, fastbitmap;

const
  DEFAULT_TOLERANCE = 32;
type


  TSearchMethod = (smStandard, smSomewhere, smLimit, smPadwalls);
  TBitmapSearch = class(TBetterObject)
    // class(TBetterObject)
    // bm to the bitmap you're searching for
    // Set a region you'll be searching
  private
    FSearchMethod: TSearchMethod;
  public
    Maps: TList<TFastBitmap>;
    DefaultSearchRegion: TRect;
    SearchRegion: TRect;
    FoundAtX, FoundAtY: integer;
    Found: boolean;
    FoundIndex: integer;
    LastFound: boolean;
    SearchRegionBitmap: TFastBitmap;
    ExternalMarker: PPoint;
    AnchorSet: boolean;
    constructor Create;override;
    destructor Destroy;override;
    procedure SearchInThis(bmIn: TFastBitmap);
    property SearchMethod: TSearchMethod read FSearchMethod write FSearchMethod;
    procedure ConfigureLike(like:TBitmapSearch);
    procedure FreeupResources;
  end;

  TBitmapCrop = class(TBetterObject)
    // This is so that you can setup a persistent REPEATED crop from various bitmaps
  private

  public
    bm: TFastBitmap;
    X1: integer;
    X2: integer;
    Y1: integer;
    Y2: integer;
    ExternalMarker: PPoint;
    destructor Destroy; override;
    procedure CropFrom(bmIn: TFastBitmap);
    procedure FreeupResources;
  end;

  TBitmapClassificationList = class(TBetterObject)
  private
    function GetClassNames(idx: nativeint): string;
  protected
    FList: TStringList;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Add(sClass: string; fb: TFastBitmap; bAsCopy: boolean);
    procedure Clear;

    function IndexOfBitmap(fb: TFastBitmap): nativeint;
    function HasBitmap(fb: TFastBitmap): boolean;
    property ClassNames[idx: nativeint]: string read GetClassNames;
    function GetClassOfBitmap(fb: TFastBitmap): string;
    procedure SaveToDisk(sDir: string);
    procedure LoadFromdisk(sDir: string);
  end;

function  FindImageOnScreen(sFile: ansistring): boolean; overload;
function FindImageOnScreen(bm: TBitmap; out x, y: integer): boolean; overload;
function FindImageOnScreen(bmScreen, bm: TBitmap; var x, y: integer): boolean;
  overload;
function FindImageOnScreen(bmScreen, bm: TFastBitmap; var x, y: integer): boolean;
  overload;

function FindImageOnScreen(bmScreen: TBitmap; const sFile: ansistring;
  var x, y: integer): boolean; overload;
function FindImageOnScreen(bmScreen: TBitmap; const bm: TBitmap; var lastKnownPointAndNewPoint: TPoint): boolean;overload;
function FindImageOnScreen(bmScreen, bm: TBitmap; var x, y: integer;
  padwalls: integer; bx1: integer; bx2: integer; by1: integer;
  by2: integer): boolean; overload;
function FindImageOnScreen(bmScreen, bm: TBitmap; X1, X2, Y1, Y2: integer;
  var x, y: integer): boolean; overload;
function FindImageOnScreen(bmScreen, bm: TFastBitmap; X1, X2, Y1, Y2: integer;
  var x, y: integer; tolerance: integer = DEFAULT_TOLERANCE): boolean; overload;
function IsSubImageHere(bmScreen, bm: TBitmap; X1, Y1: integer;
  tolerance: integer = DEFAULT_TOLERANCE): boolean; overload;
function IsSubImageHere(bmScreen, bm: TFastBitmap; X1, Y1: integer;
  tolerance: integer = DEFAULT_TOLERANCE): boolean; overload;

function IsSubImageHereBorderExempt(bmScreen, bm: TBitmap; X1, Y1: integer;
  tolerance: integer = DEFAULT_TOLERANCE; comp: TBitmap = nil): boolean;
function IsSubImageHereLimit(bmScreen, bm: TBitmap; X1, Y1: integer;
  tolerance: integer = DEFAULT_TOLERANCE; comp: TBitmap = nil): boolean;
function IsSubImageHereSomewhere(bmScreen, bm: TBitmap; X1, Y1: integer;
  tolerance: integer = DEFAULT_TOLERANCE): boolean;
function ImageCount(bmScreen, bm: TBitmap): integer; overload;
function ClickOnImage(sFile: ansistring): boolean;
function WaitForThenClickOnImage(sFile: ansistring): boolean;
procedure CropScreenShot(bmScreen: TBitmap);
function AreBitmapsIdentical(bm1,bm2: TBitmap): boolean;overload;
function AreBitmapsIdentical(bm1,bm2: TFastBitmap): boolean;overload;

type
  format = (pf24, int);

  TP4 = packed record
    case format of
      pf24:
        (a, r, g, b: byte);
      int:
        (val: integer);
  end;

  TP4Array = array of TP4;

  PP4Array = ^TP4Array;

var
  GScrapeOnlyMainMonitor: boolean;

implementation

uses screenshot, ColorConversion;

function FindImageOnScreen(bm: TBitmap; out x, y: integer): boolean; overload;
var
  bmScreen: TBitmap;
begin
  bmScreen := TBitmap.create;
  try
    TakeScreenShot(bmScreen);
    CropScreenShot(bmScreen);
    result := FindImageOnScreen(bmScreen, bm, x, y);
  finally
    bmScreen.free;
  end;
end;

function IsRegionBlack(bm: TBitmap; X1, Y1, X2, Y2: integer;
  tolerance: integer = 2): boolean;
var
  t, u: integer;
  p: PAnsiChar;
begin
  result := true;
  bm.pixelformat := pf32bit;
  for u := Y1 to Y2 do
  begin
    p := bm.scanline[u];
    for t := X1 to X2 do
    begin
      if ord(p[t]) > tolerance then
      begin
        result := false;
        exit;
      end;
    end;
  end;
end;

function IsSubImageHere(bmScreen, bm: TFastBitmap; X1, Y1: integer;
  tolerance: integer = DEFAULT_TOLERANCE): boolean; overload;
var
  t, u: integer;
  ps, p: Pbyte;
  val: TP4;
  Y2, X2: integer;
  a, r, g, b: integer;
begin
  if bm = nil then
  begin
    result := false;
    exit;
  end;
  result := true;
  bmScreen.pixelformat := xpf32bit;
  bm.pixelformat := xpf32bit;
  Y2 := Y1 + bm.height - 1;
  X2 := X1 + bm.width - 1;

  if (X1 < 0) or (X1 > bmScreen.width - 1) then
  begin
    result := false;
    exit;
  end;
  if (Y1 < 0) or (Y1 > bmScreen.height - 1) then
  begin
    result := false;
    exit;
  end;
  if (X2 < 0) or (X2 > bmScreen.width - 1) then
  begin
    result := false;
    exit;
  end;
  if (Y2 < 0) or (Y2 > bmScreen.height - 1) then
  begin
    result := false;
    exit;
  end;

  for u := Y1 to Y2 do
  begin
    ps := bmScreen.scanline[u];
    p := bm.scanline[u - Y1];
    for t := X1 to X2 do
    begin
      r := ord(ps[((t) * 4) + 0]) - ord(p[((t - X1) * 4) + 0]);
      g := ord(ps[((t) * 4) + 1]) - ord(p[((t - X1) * 4) + 1]);
      b := ord(ps[((t) * 4) + 2]) - ord(p[((t - X1) * 4) + 2]);
      a := ord(ps[((t) * 4) + 3]) - ord(p[((t - X1) * 4) + 3]);

      if (abs(r) > tolerance) or (abs(g) > tolerance) or (abs(b) > tolerance)
        then
      begin
        result := false;
        exit;
      end;
    end;
  end;
end;

function IsSubImageHere(bmScreen, bm: TBitmap; X1, Y1: integer;
  tolerance: integer = DEFAULT_TOLERANCE): boolean; overload;
var
  t, u: integer;
  ps, p: PAnsiChar;
  val: TP4;
  Y2, X2: integer;
  a, r, g, b: integer;
begin
  if bm = nil then
  begin
    result := false;
    exit;
  end;
  result := true;
  bmScreen.pixelformat := pf32bit;
  bm.pixelformat := pf32bit;
  Y2 := Y1 + bm.height - 1;
  X2 := X1 + bm.width - 1;

  if (X1 < 0) or (X1 > bmScreen.width - 1) then
  begin
    result := false;
    exit;
  end;
  if (Y1 < 0) or (Y1 > bmScreen.height - 1) then
  begin
    result := false;
    exit;
  end;
  if (X2 < 0) or (X2 > bmScreen.width - 1) then
  begin
    result := false;
    exit;
  end;
  if (Y2 < 0) or (Y2 > bmScreen.height - 1) then
  begin
    result := false;
    exit;
  end;

  for u := Y1 to Y2 do
  begin
    ps := bmScreen.scanline[u];
    p := bm.scanline[u - Y1];
    for t := X1 to X2 do
    begin
      r := ord(ps[((t) * 4) + 0]) - ord(p[((t - X1) * 4) + 0]);
      g := ord(ps[((t) * 4) + 1]) - ord(p[((t - X1) * 4) + 1]);
      b := ord(ps[((t) * 4) + 2]) - ord(p[((t - X1) * 4) + 2]);
      a := ord(ps[((t) * 4) + 3]) - ord(p[((t - X1) * 4) + 3]);

      if (abs(r) > tolerance) or (abs(g) > tolerance) or (abs(b) > tolerance)
        then
      begin
        result := false;
        exit;
      end;
    end;
  end;
end;

function IsSubImageHereBorderExempt(bmScreen, bm: TBitmap; X1, Y1: integer;
  tolerance: integer = DEFAULT_TOLERANCE; comp: TBitmap = nil): boolean;
var
  t, u: integer;
  pc, ps, p: PAnsiChar;
  val: TP4;
  Y2, X2: integer;
  a, r, g, b: integer;
begin
  result := true;
  bmScreen.pixelformat := pf32bit;
  bm.pixelformat := pf32bit;
  (* if assigned(comp) then begin
    comp.assign(bmScreen);
    comp.canvas.pixels[0,0] := comp.canvas.pixels[0,0];
    end; *)

  Y2 := Y1 + bm.height - 1;
  X2 := X1 + bm.width - 1;
  if (X1 < 0) or (X1 > bmScreen.width - 2) then
  begin
    result := false;
    exit;
  end;
  if (Y1 < 0) or (Y1 > bmScreen.height - 2) then
  begin
    result := false;
    exit;
  end;
  if (X2 < 0) or (X2 > bmScreen.width - 2) then
  begin
    result := false;
    exit;
  end;
  if (Y2 < 0) or (Y2 > bmScreen.height - 2) then
  begin
    result := false;
    exit;
  end;
  // inc(y1, 3);
  // inc(x1, 3);
  // dec(x2, 3);
  // dec(y2, 3);

  for u := Y1 + 3 to Y2 - 3 do
  begin
    ps := bmScreen.scanline[u];
    if assigned(comp) then
      pc := comp.scanline[u];
    p := bm.scanline[u - Y1];

    for t := X1 + 3 to X2 - 3 do
    begin
      r := ord(ps[(t) * 4 + 0]) - ord(p[((t - X1) + 3) * 4 + 0]);
      g := ord(ps[(t) * 4 + 1]) - ord(p[((t - X1) + 3) * 4 + 1]);
      b := ord(ps[(t) * 4 + 2]) - ord(p[((t - X1) + 3) * 4 + 2]);
      a := ord(ps[(t) * 4 + 3]) - ord(p[((t - X1) + 3) * 4 + 3]);

      (* if assigned(comp) then begin
        pc[(t*4)+0] := AnsiChar(r);
        pc[(t*4)+1] := AnsiChar(g);
        pc[(t*4)+2] := AnsiChar(b);
        pc[(t*4)+3] := AnsiChar(a);
        end; *)

      if (abs(r) > tolerance) or (abs(g) > tolerance) or (abs(b) > tolerance)
        then
      begin
        result := false;
        exit;
      end;
    end;
  end;

  // application.mainform.Refresh;

end;

function FindImageOnScreen(bmScreen, bm: TBitmap; var x, y: integer): boolean;
  overload;
var
  t, u: integer;
begin
  try
    result := false;
    if IsSubImageHere(bmScreen, bm, x, y) then
    begin
      result := true;
      exit;
    end;

    for u := 0 to (bmScreen.height - bm.height) - 2 do
    begin
      for t := 0 to bmScreen.width - bm.width do
      begin
        if IsSubImageHere(bmScreen, bm, t, u) then
        begin
          result := true;
          x := t;
          y := u;
          exit;
        end;
      end;
    end;
  finally
  end;
end;

function FindImageOnScreen(bmScreen, bm: TFastBitmap; var x, y: integer): boolean;
  overload;
var
  t, u: integer;
begin
  try
    result := false;
    if IsSubImageHere(bmScreen, bm, x, y) then
    begin
      result := true;
      exit;
    end;

    for u := 0 to (bmScreen.height - bm.height) - 2 do
    begin
      for t := 0 to bmScreen.width - bm.width do
      begin
        if IsSubImageHere(bmScreen, bm, t, u) then
        begin
          result := true;
          x := t;
          y := u;
          exit;
        end;
      end;
    end;
  finally
  end;
end;


function ImageCount(bmScreen, bm: TBitmap): integer; overload;
var
  t, u: integer;
begin
  try
    result := 0;
    for u := 0 to (bmScreen.height - bm.height) - 2 do
    begin
      for t := 0 to bmScreen.width - bm.width do
      begin
        if IsSubImageHere(bmScreen, bm, t, u, 8) then
        begin
          inc(result);
        end;
      end;
    end;
  finally
  end;
end;

function FindImageOnScreen(bmScreen, bm: TFastBitmap; X1, X2, Y1, Y2: integer;
  var x, y: integer; tolerance: integer = DEFAULT_TOLERANCE): boolean; overload;
var
  t, u: integer;
begin
  try
    result := false;
    for u := Y1 to Y2 do
    begin
      for t := X1 to X2 do
      begin
        if IsSubImageHere(bmScreen, bm, t, u, tolerance) then
        begin
          result := true;
          x := t;
          y := u;
          exit;
        end;
      end;
    end;
  finally
  end;
end;

function FindImageOnScreen(bmScreen, bm: TBitmap; X1, X2, Y1, Y2: integer;
  var x, y: integer): boolean; overload;
var
  t, u: integer;
begin
  try
    result := false;
    for u := Y1 to Y2 do
    begin
      for t := X1 to X2 do
      begin
        if IsSubImageHere(bmScreen, bm, t, u) then
        begin
          result := true;
          x := t;
          y := u;
          exit;
        end;
      end;
    end;
  finally
  end;
end;

function IsSubImageHereLimit(bmScreen, bm: TBitmap; X1, Y1: integer;
  tolerance: integer = DEFAULT_TOLERANCE; comp: TBitmap = nil): boolean;
var
  t, u, tt, uu: integer;
  pc, ps, p: PAnsiChar;
  val: TP4;
  Y2, X2: integer;
  a, r, g, b: integer;
  iW: integer;
  iH: integer;
  cScreen, cOp: PPackedColorRGBX;
  bTolerable: boolean;
const
  RADIUS = 0;
begin
  // return false if the requested area is off screen
  if (X1 < 0) or (X1 > bmScreen.width - 2) then
  begin
    result := false;
    exit;
  end;
  if (Y1 < 0) or (Y1 > bmScreen.height - 2) then
  begin
    result := false;
    exit;
  end;

  // default the result to true - we'll return false at first mismatch
  result := true;

  bmScreen.pixelformat := pf32bit;
  bm.pixelformat := pf32bit;

  // comp is for debug purposes apparently
  if assigned(comp) then
  begin
    comp.assign(bmScreen);
    comp.canvas.pixels[0, 0] := comp.canvas.pixels[0, 0];
  end;

  // setup end Y
  iH := bm.height - 1;
  if iH > 25 then
    iH := 25;
  Y2 := Y1 + iH - 1;

  // setup end X
  iW := bm.width - 1;
  if iW > 10 then
    iW := 10;

  X2 := X1 + iW - 1;

  // output some debug stuff
  if assigned(comp) then
    comp.canvas.Rectangle(X1, Y1, X2, Y2);

  if X2 > bmScreen.width - 2 then
  begin
    result := false;
    exit;
  end;
  if Y2 > bmScreen.height - 2 then
  begin
    result := false;
    exit;
  end;
  // inc(y1, 3);
  // inc(x1, 3);
  // dec(x2, 3);
  // dec(y2, 3);

  for u := Y1 + 3 to Y2 - 3 do
  begin
    ps := bmScreen.scanline[u];
    p := bm.scanline[u - Y1];

    for t := X1 + 3 to X2 - 3 do
    begin
      // fill up blur array
      ps := bmScreen.scanline[u];
      p := bm.scanline[u - Y1];

      bTolerable := false;
      for uu := 0 to RADIUS * 2 do
      begin
        ps := bmScreen.scanline[u + (uu - RADIUS)];
        for tt := 0 to RADIUS * 2 do
        begin
          cScreen := PPackedColorRGBX(@ps[(t + (tt - RADIUS)) * 4]);
          cOp := PPackedColorRGBX(@p[((t - X1) + 3) * 4]);
          if cScreen.AbsoluteDifference(cOp^) < tolerance then
          begin
            bTolerable := true;
            break;
          end;
        end;
        if bTolerable then
          break;
      end;

      if not bTolerable then
      begin
        result := false;
        exit;
      end;
    end;
  end;

  // application.mainform.Refresh;

end;

function IsSubImageHereSomewhere(bmScreen, bm: TBitmap; X1, Y1: integer;
  tolerance: integer = DEFAULT_TOLERANCE): boolean;
var
  x, y: integer;
begin
  result := FindImageOnScreen(bmScreen, bm, X1 - 16, X1 + 16, Y1 - 16, Y1 + 16,
    x, y);
end;

function FindImageOnScreen(bmScreen, bm: TBitmap; var x, y: integer;
  padwalls: integer; bx1: integer; bx2: integer; by1: integer;
  by2: integer): boolean; overload;
var
  t, u: integer;
begin
  try
    result := false;
    if IsSubImageHereLimit(bmScreen, bm, x, y) then
    begin
      result := true;
      exit;
    end;
    if bx1 < 0 then
      bx1 := 0;
    if by1 < 0 then
      by1 := 0;

    if by2 = 0 then
      by2 := (bmScreen.height - bm.height) - 2;
    if bx2 = 0 then
      bx2 := bmScreen.width - bm.width;

    for u := by1 to by2 do
    begin
      for t := bx1 to bx2 do
      begin
        if IsSubImageHereLimit(bmScreen, bm, t, u, 32) then
        begin
          result := true;
          x := t;
          y := u;
          exit;
        end;
      end;
    end;
  finally
  end;
end;

(* function IsSubImageHere(bmScreen, bm:TBitMap; x1,y1: integer; tolerance: integer = 16; comp: TBitmap = nil): boolean;
  var
  t,u: integer;
  pc, ps, p: PAnsiChar;
  val: TP4;
  y2, x2: integer;
  a,r,g,b: integer;
  begin
  result := true;
  bmScreen.pixelformat := pf32bit;
  bm.pixelformat := pf32bit;
  if assigned(comp) then begin
  comp.assign(bmScreen);
  comp.canvas.pixels[0,0] := comp.canvas.pixels[0,0];
  end;

  y2 := y1+bm.height-1;
  x2 := x1+bm.width-1;
  if x2 > bmScreen.width-2 then begin
  result := false;
  exit;
  end;
  if y2 > bmScreen.height-2 then begin
  result := false;
  exit;
  end;
  //  inc(y1, 3);
  //  inc(x1, 3);
  //  dec(x2, 3);
  //  dec(y2, 3);

  for u:= y1 to y2 do begin
  ps := bmScreen.scanline[u];
  if assigned(comp) then
  pc := comp.scanline[u];
  p := bm.scanline[u-y1];

  for t:= x1 to x2 do begin
  r := ord(ps[(t)*4+0]) - ord(p[((t-x1)+3)*4+0]);
  g := ord(ps[(t)*4+1]) - ord(p[((t-x1)+3)*4+1]);
  b := ord(ps[(t)*4+2]) - ord(p[((t-x1)+3)*4+2]);
  a := ord(ps[(t)*4+3]) - ord(p[((t-x1)+3)*4+3]);

  if assigned(comp) then begin
  pc[(t*4)+0] := AnsiChar(r);
  pc[(t*4)+1] := AnsiChar(g);
  pc[(t*4)+2] := AnsiChar(b);
  pc[(t*4)+3] := AnsiChar(a);
  end;


  if (abs(r) > tolerance) or (abs(g) > tolerance) or (abs(b) > tolerance)then begin
  result := false;
  exit;
  end;
  end;
  end;

  //application.mainform.Refresh;

  end; *)
function FindImageOnScreen(bmScreen: TBitmap; const sFile: ansistring;
  var x, y: integer): boolean; overload;
var
  bmFind: TBitmap;
begin
  bmFind := TBitmap.create;
  try
    bmFind.loadfromfile(sFile);
    RemoveColorTotalWhite(bmFind);
    result := FindImageOnScreen(bmScreen, bmFind, x, y);
  finally
    bmFind.free;
  end;

end;

function WaitForThenClickOnImage(sFile: ansistring): boolean;
begin
  while not ClickOnImage(sFile) do
    sleep(2000);

  result := true;
end;

function FindImageOnScreen(sFile: ansistring): boolean;
var
  x, y: integer;
  bm: TBitmap;
begin
  bm := TBitmap.create;
  try
    bm.loadfromfile(sFile);
    if FindImageOnScreen(bm, x, y) then
    begin
      result := true;
    end
    else
    begin
      result := false;
    end;
  finally
    bm.free;
  end;
end;

function ClickOnImage(sFile: ansistring): boolean;
var
  x, y: integer;
  bm: TBitmap;
begin

  bm := TBitmap.create;
  try
    bm.loadfromfile(sFile);
    if FindImageOnScreen(bm, x, y) then
    begin
      mouse.clickmouse(x, y);
      result := true;
    end
    else
    begin
      result := false;
    end;
  finally
    bm.free;
  end;

end;

procedure CropScreenShot(bmScreen: TBitmap);
var
  m: TMonitor;
begin
  if GScrapeOnlyMainMonitor then
    exit;

  m := screen.monitors[0];

  EasyImage.NewCrop(bmScreen, m.Left, m.Top, m.Left + m.width - 1,
    m.Top + m.height - 1);

end;

{ TBitmapSearch }

procedure TBitmapSearch.ConfigureLike(like: TBitmapSearch);
begin
  if like = nil then exit;
  FoundAtX := like.FoundAtX;
  FoundAtY := like.FoundAtY;
  LastFound := like.found;
  FoundIndex := like.foundindex;
  ExternalMarker := like.ExternalMarker;
  AnchorSet := like.AnchorSet;

end;

constructor TBitmapSearch.Create;
begin
  inherited;
  Maps := TLIst<TFastBitmap>.create;
end;

destructor TBitmapSearch.Destroy;
begin
  SearchRegionBitmap.free;
  Maps.free;
  inherited;
end;

procedure TBitmapSearch.FreeupResources;
begin
  SearchRegionBitmap.free;
  SearchRegionBitmap := nil;
end;

procedure TBitmapSearch.SearchInThis(bmIn: TFastBitmap);
var
  bfound: boolean;
  t: integer;
  x,y: integer;
  mx,my: integer;
begin
  bFound := false;
  if assigned(SearchRegionBitmap) then begin
    SearchRegionBitmap.free;
    SearchRegionBitmap := nil;
  end;

  if ExternalMarker = nil then begin
    mx := 0;
    my := 0;
  end else begin
    mx := ExternalMarker.x;
    my := ExternalMarker.y;
  end;

  SearchRegionbitmap := NewCrop(bmIn, SearchRegion.Left+MX,SearchRegion.Top+My,SearchRegion.Right+MX,SearchRegion.Bottom+My);

  LastFound := found;
  for t:= 0 to Maps.count-1 do begin
    if not AnchorSet then begin
      case SearchMethod of
        smStandard: bFound := FindImageOnScreen(bmIn, maps[t], SearchRegion.Left+mx,SearchRegion.Right+mx,SearchRegion.Top+my,SearchRegion.Bottom+my, x,y, DEFAULT_TOLERANCE);
//        smSomewhere: bFound := IsSubImageHereSomewhere(bmIn, maps[t], SearchRegion.Left+mx,SearchRegion.Top+my);
//        smLimit: bFound := IsSubImageHereLimit(bmIn, maps[t], SearchRegion.Left+mx,SearchRegion.Top+my);
//        smPadWalls: bFound := FindImageOnScreen(bmIn, maps[t], x, y, 3, SearchRegion.Left+mx, SearchRegion.Right+mx,  SearchRegion.Top+my, SearchRegion.Bottom+my);
      end;
    end else begin
      case SearchMethod of
        smStandard: bFound := FindImageOnScreen(bmIn, maps[t], FoundAtX+mx,FoundAtX+mx,FoundAtY+my,FoundAtY+my, x,y, DEFAULT_TOLERANCE);
//        smSomewhere: bFound := IsSubImageHereSomewhere(bmIn, maps[t], FoundAtX+mx,FoundAtY+my);
//        smLimit: bFound := IsSubImageHereLimit(bmIn, maps[t], FoundAtX+mx,FoundAtY+my);
//        smPadWalls: bFound := FindImageOnScreen(bmIn, maps[t], x, y, 3, FoundAtX+mx, FoundAtX+mx, FoundAtY+mx, FoundAtY+my);
      end;
    end;

    Found := bFound;
    if Found then begin
      FoundIndex := t;
      FoundAtX := x-MX;
      FoundAtY := y-MY;
      AnchorSet := true;
      break;
    end else
      AnchorSet := false;
  end;
end;

{ TBitmapCrop }


procedure TBitmapCrop.CropFrom(bmIn: TFastBitmap);
var
  mx,my: integer;
begin
  if assigned(bm) then begin
    bm.free;
    bm := nil;
  end;
  if ExternalMarker = nil then begin
    mx := 0;
    my := 0;
  end else begin
    mx := ExternalMarker.x;
    my := ExternalMarker.y;
  end;
  bm := EasyImage.NewCrop(bmIn, X1+MX, Y1+MY, X2+MX, Y2+MY);
end;


destructor TBitmapCrop.Destroy;
begin
  bm.free;
  bm := nil;
  inherited;
end;

procedure TBitmapCrop.FreeupResources;
begin
  bm.free;
  bm := nil;
end;

function AreBitmapsIdentical(bm1,bm2: TFastBitmap): boolean;overload;
begin
  if bm1 = bm2 then begin
    result := true;
    exit;
  end;

  if (bm1 = nil) or (bm2 = nil) then begin
    result := false;
    exit;
  end;

  result := IsSubImageHere(bm1, bm2, 0,0);
end;


function AreBitmapsIdentical(bm1,bm2: TBitmap): boolean;
begin
  if bm1 = bm2 then begin
    result := true;
    exit;
  end;

  if (bm1 = nil) or (bm2 = nil) then begin
    result := false;
    exit;
  end;

  result := IsSubImageHere(bm1, bm2, 0,0);
end;

{ TBitmapClassificationList }


procedure TBitmapClassificationList.Add(sClass: string; fb: TFastBitmap;
  bAsCopy: boolean);
begin
  if bAsCopy then
    fb := TFastBitmap.CopyCreate(fb);

  FList.AddObject(sClass, fb);
end;

procedure TBitmapClassificationList.Clear;
begin
  while FList.count > 0 do begin
    TFastBitmap(FList.Objects[FList.Count-1]).free;
    FList.Delete(flist.Count-1);
  end;
end;

constructor TBitmapClassificationList.Create;
begin
  inherited;
  flist := TStringlist.Create;
end;

destructor TBitmapClassificationList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TBitmapClassificationList.GetClassNames(idx: nativeint): string;
begin
  result := FList[idx];
end;

function TBitmapClassificationList.GetClassOfBitmap(fb: TFastBitmap): string;
var
  i: nativeint;
begin
  i := IndexOfBitMap(fb);
  result := '';

  if i>=0 then
    result := ClassNames[i];


end;

function TBitmapClassificationList.HasBitmap(fb: TFastBitmap): boolean;
begin
  result := IndexOfBitmap(fb) >= 0;

end;

function TBitmapClassificationList.IndexOfBitmap(fb: TFastBitmap): nativeint;
var
  t: integer;
  f: TFastBitmap;
begin
  result := -1;
  for t := 0 to FList.count-1 do begin
    f := TFastBitmap(FList.objects[t]);
    if f.IsEqualTo(fb) then begin
      result := t;
      break;
    end;
  end;

end;

procedure TBitmapClassificationList.LoadFromdisk(sDir: string);
var
  dir: TDirectory;
  t: integer;
  fb: TFastBitmap;
  s1,s2,s3: string;
begin
  sDir := slash(sDir);
  dir := TDirectory.Create(sDir, '*.bmp', 0,0);
  try
    for t:= 0 to dir.Filecount-1 do begin
      fb := TFastBitmap.Create;
      fb.LoadFromFile(dir.Files[t].FullName);
      SplitString(dir.Files[t].Namepart, '_', s1,s2);
      self.Add(s1, fb, false);
    end;
  finally
    dir.Free;
  end;

end;

procedure TBitmapClassificationList.SaveToDisk(sDir: string);
var
  dir: TDirectory;
  t: integer;
  fb: TFastBitmap;
  s1,s2,s3: string;
begin
  sDir := slash(sDir);
  dir := TDirectory.Create(sDir, '*.bmp', 0,0);
  try
    for t:= 0 to dir.Filecount-1 do begin
      DeleteFile(pchar(dir.Files[t].fullname));
    end;

    for t:= 0 to FList.count-1 do begin
      fb := TFastBitmap(FList.Objects[t]);
      fb.SaveToFile(sdir+FList[t]+'_'+inttostr(t)+'.bmp');
    end;


  finally
    dir.Free;
  end;
end;

function FindImageOnScreen(bmScreen: TBitmap; const bm: TBitmap; var lastKnownPointAndNewPoint: TPoint): boolean;overload;
begin
  result := FindImageOnSCreen(bmScreen, bm, lastKnownPointAndNewPoint.x, lastKnownPointAndNewPoint.y);
end;

initialization

GScrapeOnlyMainMonitor := false;

end.
