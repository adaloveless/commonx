unit Mouse;

interface

uses
  Windows, forms, graphics, tickcount;

procedure MoveMouse(x,y: integer);
procedure ClickMouse(x,y: integer);
procedure MouseDown(x,y: integer; bRight: boolean = false);
procedure MouseUp(x,y: integer; bRight: boolean = false);

var
  lastmousex: integer;
  lastmousey: integer;

implementation

procedure MoveMouse(x,y: integer);
//var
//  a: array[0..3] of cardinal;
var
  mppx, mppy: real;
  tm1, tm2: cardinal;
  r: real;
  tx,ty: integer;
const
  movetime=0;
begin
//  SystemParametersInfo(SPI_GETMOUSE	, 0, @a,0);
  mppx := 65536/screen.width;
  mppy := 65536/screen.height;


  tm1 := GetTicker;
  tm2 := tm1;
//  while (tm2 >= tm1) and (tm2 < tm1+movetime) do begin
//    tm2 := GetTicker;
//    r := (tm2-tm1)/movetime;
//    if r> 1 then r := 1;
//
//    tx := round(((x-lastmousex)*r)+lastmousex);
//    ty := round(((y-lastmousey)*r)+lastmousey);
//
//    mouse_event(mouseeventF_absolute+mouseeventf_move, round(tx*mppx),round(ty*mppy), 0,0);
//
//  end;

  mouse_event(mouseeventF_absolute+mouseeventf_move, round(x*mppx),round(y*mppy), 0,0);
  lastmousex := x;
  lastmousey := y;





end;

procedure ClickMouse(x,y: integer);
//var
//  a: array[0..3] of cardinal;
var
  mppx, mppy: real;
begin
//  SystemParametersInfo(SPI_GETMOUSE	, 0, @a,0);
  mppx := 65536/screen.width;
  mppy := 65536/screen.height;

  MoveMouse(x,y);
  mouse_event(mouseeventF_absolute+mouseeventf_leftdown, round(x*mppx),round(y*mppy), 0,0);
  mouse_event(mouseeventF_absolute+mouseeventf_leftup, round(x*mppx),round(y*mppy), 0,0);



end;

procedure MouseDown(x,y: integer; bRight: boolean = false);
//var
//  a: array[0..3] of cardinal;
var
  mppx, mppy: real;
begin
//  SystemParametersInfo(SPI_GETMOUSE	, 0, @a,0);
  mppx := 65536/screen.width;
  mppy := 65536/screen.height;

  MoveMouse(x,y);
  if bRight then
    mouse_event(mouseeventF_absolute+mouseeventf_rightdown, round(x*mppx),round(y*mppy), 0,0)
  else
    mouse_event(mouseeventF_absolute+mouseeventf_leftdown, round(x*mppx),round(y*mppy), 0,0);
//  mouse_event(mouseeventF_absolute+mouseeventf_leftup, round(x*mppx),round(y*mppy), 0,0);



end;


procedure MouseUp(x,y: integer; bRight: boolean = false);
//var
//  a: array[0..3] of cardinal;
var
  mppx, mppy: real;
begin
//  SystemParametersInfo(SPI_GETMOUSE	, 0, @a,0);
  mppx := 65536/screen.width;
  mppy := 65536/screen.height;

  MoveMouse(x,y);
//  mouse_event(mouseeventF_absolute+mouseeventf_leftdown, round(x*mppx),round(y*mppy), 0,0);
  if bRight then
    mouse_event(mouseeventF_absolute+mouseeventf_rightup, round(x*mppx),round(y*mppy), 0,0)
  else
    mouse_event(mouseeventF_absolute+mouseeventf_leftup, round(x*mppx),round(y*mppy), 0,0);



end;



end.
