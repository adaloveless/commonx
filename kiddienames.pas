unit kiddienames;
//Contains a single function for auto-generating group names.  Used for Digital TundraLINK.

interface

function GetAutoGroupName(i: integer): ansistring;

implementation

function GetAutoGroupName(i: integer): ansistring;
//Given an integer, returns a kid-friendly group name.
//Supports only 31 combinations. Used for Digital TundraLINK account setup only.
//Examples of names include "blue", "green", "red", "Plum", "Bear", "Apple"...etc.
begin
  result := '';
  case i of
    0: result := 'Blue';
    1: result := 'Green';
    2: result := 'Red';
    3: result := 'Purple';
    4: result := 'Yellow';
    5: result := 'Orange';
    6: result := 'Brown';
    7: result := 'Pink';
    8: result := 'Grape';
    9: result := 'Lime';
   10: result := 'Apple';
   11: result := 'Lemon';
   12: result := 'Melon';
   13: result := 'Pear';
   14: result := 'Plum';
   15: result := 'Bear';
   16: result := 'Dog';
   17: result := 'Cat';
   18: result := 'Bat';
   19: result := 'Rabbit';
   20: result := 'Fish';
   21: result := 'Fox';
   22: result := 'Horse';
   23: result := 'Tiger';
   24: result := 'Frog';
   25: result := 'Bug';
   26: result := 'Robin';
   27: result := 'Bird';
   28: result := 'Duck';
   29: result := 'Car';
   30: result := 'Star';
  end;
end;



end.
