unit RDTPRemoteControlServerImplib;
{$INLINE AUTO}
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPRemoteControlRQs.txt}
{END}
interface
uses
  rdtpprocessor, sysutils, classes, screenshot, graphics, consolelock;
{INTERFACE_START}
    function RQ_MouseClick(x:integer; y:integer; button:integer):boolean;overload;override;
    function RQ_MouseDown(x:integer; y:integer; button:integer):boolean;overload;override;
    function RQ_MouseMove(x:integer; y:integer; button:integer):boolean;overload;override;
    function RQ_MouseUp(x:integer; y:integer; button:integer):boolean;overload;override;
    function RQ_ScreenShot():TStream;overload;override;

{INTERFACE_END}
implementation

uses
  mouse;

//------------------------------------------------------------------------------
function RQ_MouseClick(proc: TRDTPProcessor; x:integer; y:integer; button:integer): boolean;overload;
begin
  Mouse.ClickMouse(x,y);
end;
//------------------------------------------------------------------------------
function RQ_MouseDown(proc: TRDTPProcessor; x:integer; y:integer; button:integer): boolean;overload;
begin
  Mouse.MouseDown(x,y);
end;
//------------------------------------------------------------------------------
function RQ_MouseMove(proc: TRDTPProcessor; x:integer; y:integer; button:integer): boolean;overload;
begin
  Mouse.MoveMouse(x,y);
end;
//------------------------------------------------------------------------------
function RQ_MouseUp(proc: TRDTPProcessor; x:integer; y:integer; button:integer): boolean;overload;
begin
  Mouse.MouseUp(x,y);
end;
//------------------------------------------------------------------------------
function RQ_ScreenShot(proc: TRDTPProcessor):TStream;overload;
var
  bm: TBitmap;
  bRepeat: boolean;
begin
  LockConsole;
  try
    bm := TBitmap.create;
    bm.canvas.lock;
    try
      repeat
        bRepeat := false;
        screenshot.takeScreenshot(bm);
        result := TMemoryStream.create;
        result.Seek(0, soBeginning);
        bm.SaveToStream(result);
        if result.Size = 0 then begin
          bRepeat := true;
          result.free;
        end;
      until bRepeat = false;
    finally
      bm.canvas.unlock;
      bm.free;
    end;
  finally
    UnlockConsole;
  end;
end;
end.
