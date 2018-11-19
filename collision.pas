unit collision;

interface

uses
  typex, numbers, types;

type
  TCollision1D = (cTangentLeft, cTangentRight, cOverlapLeft, cOverlapRight, cThru, cWithin, cNoCollision);

function TestCollision1D(opLeft, opWidth, testLeft, testWidth: ni): TCollision1D;
function RectCollision(r1,r2: TRect): boolean;overload;
function RectCollision(r1,r2: TRectF): boolean;overload;
function IsRectEdge(p: TPOintF; r: TRectF; thickness: nativefloat = 1): boolean;



implementation

function TestCollision1D(opLeft, opWidth, testLeft, testWidth: ni): TCollision1D;
var
  opBadRight, testBadRight: ni;
begin
  result := cNoCollision;
  opBadRight := opLeft + opWidth;
  testBadRight := testLeft + testWidth;

  //quick miss left
  // tttt oooo
  if testBadRight < opLeft then
    exit;

  //quick miss right
  //      oooo tttt
  if opBadRight < testLeft then
    exit;

  //tan left
  //     ttttoooo
  if testBadRight = opLeft then begin
    result := cTangentLEft;
    exit;
  end;
  //tan right
  // ooootttt
  if testLeft = opBadRight then begin
    result := cTangentRight;
    exit;
  end;

  if testLeft > opLeft then begin
    if testBadRight <= opBadRight then begin
      //within
      //   tttt
      //  oooooo
      result := cWithin;
      exit;
    end else begin
      //overlap right
      //   tttttt
      // oooo
      result := cOverLapRight;
    end;
  end else begin
    if testBadRight <= opBadRight then begin
      //overlap left
      // tttttt
      //     oooo
      result := cOverLapLeft;
      exit;
    end else begin
      //thru
      // tttttttttt
      //     oooo
      result := cThru;
    end;

  end;
end;

function RectCollision(r1,r2: TRect): boolean;
begin
  order(r1);
  order(r2);
  result := true;
  if r1.Right < r2.Left then
    result := false
  else
  if r2.Right < r1.Left then
    result := false
  else
  if r1.Bottom < r2.Top then
    result := false
  else
  if r2.Bottom < r1.Top then
    result := false;

end;

function RectCollision(r1,r2: TRectF): boolean;overload;
begin
  order(r1);
  order(r2);
  result := true;
  if r1.Right < r2.Left then
    result := false
  else
  if r2.Right < r1.Left then
    result := false
  else
  if r1.Bottom < r2.Top then
    result := false
  else
  if r2.Bottom < r1.Top then
    result := false;

end;

function IsRectEdge(p: TPOintF; r: TRectF; thickness: nativefloat = 1): boolean;
var
  dif: TPOintF;
begin
  order(r);
  result := true;
  if p.x < r.left then
    exit(false);
  if p.x > r.Left+r.Width then
    exit(false);

  if p.y < r.top then
    exit(false);
  if p.y > r.top+r.height then
    exit(false);

  result := false;

  dif := p-r.TopLeft;
  if dif.X < thickness then
    exit(true);
  if dif.y < thickness then
    exit(true);

  dif := (r.BottomRight)-(p);
  if (dif.X) < thickness then
    exit(true);
  if (dif.y) < thickness then
    exit(true);




end;


end.
