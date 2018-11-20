unit gridwalker;

interface

uses
  typex, types;

type
  TGridWalker = record
    w,h: ni;
    function Steps: ni;
    function GetStandard(step: ni): TPoint;
    function GetRightToLeft(step: ni): TPoint;
    function TopToBottom(step: ni): TPoint;
    function BottomToTop(step: ni): TPoint;
    function VerticalSnake(step: ni): TPoint;


  end;



implementation

{ TGridWalker }

function TGridWalker.BottomToTop(step: ni): TPoint;
begin
  result.X := (step div h);
  result.y := (h-1)-(step mod h);
end;

function TGridWalker.GetRightToLeft(step: ni): TPoint;
begin
  result.X := (w-1)-(step mod w);
  result.y := step div w;

end;

function TGridWalker.GetStandard(step: ni): TPoint;
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

function TGridWalker.VerticalSnake(step: ni): TPoint;
begin
  result.X := step div h;
  if (result.x and 1)=0 then
    result.y := step mod h
  else
    result.y := (h-1)-(step mod h);

end;

end.
