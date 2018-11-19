unit better_colors;

interface

uses
  GRAPHICS, typex;


const
  clOrange = $007FFF;
  clCyan = $FFFF00;
  clMagenta = $FF00FF;
  clbrown = $003040;

  aclWhite =   $FFFFFFFF;
  aclSilver =  $FFaFaFaF;
  aclGrey =    $FF3F3F3F;
  aclBlack =   $FF000000;
  aclYellow =  $FFFFFF00;
  aclGold =    $FF7f7f00;
  aclRed =     $FFFF0000;
  aclGreen   = $FF007f00;
  aclOrange  = $FFFF7F00;
  aclBrown   = $FFAF3f00;
  aclMagenta = $FFFF00FF;
  aclTeal  =   $FF007FFF;
  aclCyan  =   $FF00FFFF;
  aclLime =    $FF00FF00;
  aclBlue =    $FF0000FF;
  aclNavy =    $FF00007F;
  aclPurple =  $FF7F00FF;
  aclViolet =  aclPurple;
  aclGray =    aclGrey;

function ColorToAColor(c: TColor): TColor;

const
  Crainbow : array of TColor = [clRed, clOrange, clYellow, clGreen, clBlue, clPurple];

function RainBow(idx: ni): TColor;

implementation

function RainBow(idx: ni): TColor;
begin
  result := Crainbow[idx mod length(Crainbow)];
end;
function ColorToAColor(c: TColor): TColor;
begin
  result := (c and $FF00FF00) + ((c and $00FF0000) shr 16) + ((c and $000000FF) shl 16)


end;

end.
