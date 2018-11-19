unit d3dabstraction;
{$INLINE AUTO}
interface

uses
  betterobject, direct3d9_jedi,miscroutines;

type
  INativeDevice = IDirect3dDevice9;
  TCullMode = (cmBad, cmNone, cmCW, cmCCW);
  TRenderStateManager = class(TBetterObject)
  private
    procedure SetEnableAlpha(const Value: boolean);
    procedure SetNAtive(const Value: INativeDevice);
    procedure SetCull(const Value: TCullMode);
  protected
    FEnableALpha: boolean;
    FCull: TCullMode;
    FNative: INativeDevice;
  published
  public
    constructor create;override;
    property Native: INativeDevice read FNative write SetNAtive;
    procedure Init;

    property EnableAlpha: boolean read FEnableAlpha write SetEnableAlpha;
    property Cull: TCullMode read FCull write SetCull;

  end;


implementation

{ TRenderStateManager }

constructor TRenderStateManager.create;
begin
  inherited create;


end;

procedure TRenderStateManager.Init;
begin
  FEnableAlpha := false;
  FCull := cmCW;
end;

procedure TRenderStateManager.SetCull(const Value: TCullMode);
begin
  if value <> FCull then begin
    FCull := Value;
    if native <> nil then
      native.SetRenderState(D3DRS_CULLMODE, ord(FCull));
  end;

end;

procedure TRenderStateManager.SetEnableAlpha(const Value: boolean);
begin
  if value <> FEnableAlpha then begin
    FEnableAlpha := Value;
    if native <> nil then
      native.SetRenderState(D3DRS_ALPHABLENDENABLE, booltoint(FEnableAlpha));
  end;
end;

procedure TRenderStateManager.SetNAtive(const Value: INativeDevice);
begin
  if value <> FNative then begin
    FNative := Value;
    Init;
  end;
end;

end.
