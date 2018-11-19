unit gdiplus;


interface

uses
  GDIPOBJ, typex,systemx, betterobject, forms, graphics, easyimage, colorconversion;

type
  TGPGraphicEvent = procedure(g: TGPGraphics; sender: TObject) of object;

  TCanvasPlus = class(TBetterObject)
  private
    FGraphics: TGPGRaphics;
    FBitmap: TGPBitmap;
    FHandle: THandle;
    FManager: TForm;
    function GetGRaphics: TGPGRaphics;
    function GetPixels(x, y: integer): TSmallColor;
    procedure SetHandle(const Value: THandle);
    procedure SetPixels(x, y: integer; const Value: TSmallColor);
    function GetBitMap: TGPBitmap;
  public
    constructor Create(handle: THandle = 0);reintroduce;virtual;
    destructor Destroy;override;

    property Pixels[x: integer; y: integer]: TSmallColor read GetPixels write SetPixels;
    property Handle: THandle read FHandle write SetHandle;
    property Graphics: TGPGRaphics read GetGRaphics write FGraphics;
    property Bitmap: TGPBitmap read GetBitMap write FBitMap;
  end;

implementation


{ TCanvasPlus }

constructor TCanvasPlus.Create(handle: THandle);
begin
  inherited Create;


end;

destructor TCanvasPlus.Destroy;
begin

  inherited;
end;


function TCanvasPlus.GetBitMap: TGPBitmap;
begin
  Result := FBitMap;
end;

function TCanvasPlus.GetGRaphics: TGPGRaphics;
begin
  if FGraphics = nil then begin
    FGraphics := GDIPOBJ.TGPGraphics.Create(FHandle);
  end;

  Result := FGraphics;
end;





function TCanvasPlus.GetPixels(x, y: integer): TSmallColor;
var
  bm: TGPBitMap;
begin
  bm := self.Bitmap;

//  bm.GetPixel(x,y, result);

end;

procedure TCanvasPlus.SetHandle(const Value: THandle);
begin
  FHandle := Value;
end;


procedure TCanvasPlus.SetPixels(x, y: integer; const Value: TSmallColor);
begin
//  self.Bitmap.SetPixel(x,y, value);
end;




end.
