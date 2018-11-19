unit ScreenShot;
{$INLINE AUTO}
interface

uses
  types, windows, forms, graphics, easyimage, typex, fastbitmap;

procedure TakeScreenShot(Bild: TBitMap; hScreen: THandle = $FFFFFFFF);
function CaptureCurrentMousePointer: TFastBitmap;

implementation

uses screenscrape, advancedgraphics;

function CaptureCurrentMousePointer: TFastBitmap;
var
  bm: tagBITMAP;
  cur: tagCURSORINFO;
  sh: tagCURSORSHAPE;
  ii: _ICONINFO;
  fi: TFastBitmap;
  t,u: ni;
  a: array[0..255] of array[0..63] of byte;
  p: Pbyte;
begin
  //int width, height;
  //    BITMAP bitmap;
  //    ICONINFO ii;
  //
  //    GetIconInfo((HICON)GetCursor(), &ii);
  //    GetObject(ii.hbmMask, sizeof(BITMAP), &bitmap);
  //
  //    width = bitmap.bmWidth;
  //    height = bitmap.bmHeight;
  result := nil;

  cur.cbSize := sizeof(cur);
  GetCursorInfo(cur);
//  GetIconInfo(GetCursor(), ii);

  if not GetIconInfo(cur.hCursor,ii) then begin
     exit;
  end;

  GetObject(ii.hbmMask, sizeof(bm), @bm);
  GetObject(ii.hbmColor, sizeof(bm), @bm);
  GetBitmapBits(ii.hbmColor, bm.bmWidthBytes*bm.bmHeight, @a[0][0]);
  p := @a[0][0];


  fi := TFastBitmap.create;
  try
    fi.Width := bm.bmWidth;
    fi.Height := bm.bmHeight;
    fi.New;
    for u := 0 to fi.Height-1 do begin
      for t:= 0 to fi.Width-1 do begin
        fi.Canvas.pixels[t,u] := pointerToColor(t,u,bm.bmWidth, bm.bmHeight, p, bm.bmBitsPixel);
      end;
    end;
  finally
    result := fi;
  end;

  DeleteObject(ii.hbmMask);
  DeleteObject(ii.hbmColor);

end;

procedure TakeScreenShot(Bild: TBitMap; hScreen: THandle = $FFFFFFFF);
var
  c: TCanvas;
  r: TRect;
begin
  c := TCanvas.Create;
  c.Lock;
  try
    if (hScreen = $FFFFFFFF) then
      c.Handle := GetWindowDC(GetDesktopWindow)
    else
      c.Handle := hScreen;
    try
      r := Rect(0, 0, Screen.Width, Screen.Height);
      Bild.Width := Screen.Width;
      Bild.Height := Screen.Height;
      Bild.Canvas.CopyRect(r, c, r);
    finally
      ReleaseDC(0, c.Handle);
    end;
  finally
    c.Unlock;
    c.free;
  end;
end;

end.
