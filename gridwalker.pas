unit gridwalker;

interface

uses
  typex, types, numbers;

type
  TGridWalker = record
    w,h: ni;//number of tiles w/h
    tileW, tileH: ni;//size of tiles
    fullW, fullH: ni;//full size of input
    procedure Configure(fullW, fullH, tileW, tileH: ni);
    function Steps: ni;
    function LeftToRight(step: ni): TPoint;
    function RightToLeft(step: ni): TPoint;
    function TopToBottom(step: ni): TPoint;
    function BottomToTop(step: ni): TPoint;
    function VerticalSnake(step: ni): TPoint;
    function ToTile(p: TPoint): TPixelRect;

  end;



implementation

{ TGridWalker }

function TGridWalker.BottomToTop(step: ni): TPoint;
begin
  result.X := (step div h);
  result.y := (h-1)-(step mod h);
end;

procedure TGridWalker.Configure(fullW, fullH, tileW, tileH: ni);
begin
  //5 2 2.5 (3) === 5-1=4 div 2 =2 + 1 = 3
  //4 2 2 (2) == 4-1=3 div 2 =1+ 1 = 2
  self.fullW := fullW;
  self.fullH := fullH;
  self.tileW := tileW;
  self.tileH := tileH;
  w := ((fullW-1) div tileW)+1;
  h := ((fullH-1) div tileH)+1;

end;

function TGridWalker.RightToLeft(step: ni): TPoint;
begin
  result.X := (w-1)-(step mod w);
  result.y := step div w;

end;

function TGridWalker.LeftToRight(step: ni): TPoint;
begin
  result.X := step mod w;
  result.y := step div w;
end;

function TGridWalker.Steps: ni;
begin
  result := w * h;
end;

function TGridWalker.TopToBottom(step: ni): TPoint;
begin
  result.X := step div h;
  result.y := step mod h;

end;

function TGridWalker.ToTile(p: TPoint): TPixelRect;
begin
  result.Top := p.y*tileH;
  result.Left := p.x*tileW;
  result.Right := lesserof(fullW, ((p.X+1)*tileW)-1);
  result.Bottom := lesserof(fullH, ((p.Y+1)*tileH)-1);

end;

function TGridWalker.VerticalSnake(step: ni): TPoint;
begin
  result.X := step div h;
  if (result.x and 1)=0 then
    result.y := step mod h
  else
    result.y := (h-1)-(step mod h);

end;

end.
