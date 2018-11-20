unit KinectDLL;

interface

{$DEFINE DISABLE_KINECT}

const
  Kinect_DLL = 'Kinect_Simple_DLL.dll';


type HKINECT = smallint;



{$IFDEF DISABLE_KINECT}

function Kinect_DLL_Init(): integer; Cdecl;
function Kinect_Init(): HKINECT; Cdecl;
procedure Kinect_Close(iHandle: HKINECT); Cdecl;
procedure Kinect_DLL_Close(); Cdecl;
procedure Kinect_GetDepthBuffer(h: HKINECT; p: pointer); Cdecl;
procedure Kinect_SetTilt(h: HKINECT; d: double); Cdecl;

{$ELSE}

function Kinect_DLL_Init(): integer; Cdecl; external Kinect_DLL;
function Kinect_Init(): HKINECT; Cdecl; external Kinect_DLL;
procedure Kinect_Close(iHandle: HKINECT); Cdecl; external Kinect_DLL;
procedure Kinect_DLL_Close(); Cdecl; external Kinect_DLL;
procedure Kinect_GetDepthBuffer(h: HKINECT; p: pointer); Cdecl; external Kinect_DLL;
procedure Kinect_SetTilt(h: HKINECT; d: double); Cdecl; external Kinect_DLL;

{$ENDIF}


implementation

{$IFDEF DISABLE_KINECT}
function Kinect_DLL_Init(): integer; Cdecl;
begin
  result := 0;
end;
function Kinect_Init(): HKINECT; Cdecl;
begin
  result := 0;
end;

procedure Kinect_Close(iHandle: HKINECT); Cdecl;
begin
  //
end;

procedure Kinect_DLL_Close(); Cdecl;
begin
  //
end;

procedure Kinect_GetDepthBuffer(h: HKINECT; p: pointer); Cdecl;
begin
  //
end;

procedure Kinect_SetTilt(h: HKINECT; d: double); Cdecl;
begin
  //
end;
{$ENDIF}


end.
