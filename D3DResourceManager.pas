unit D3DResourceManager;


interface
uses
  direct3d9_jedi, sharedobject, miscroutines, interfacelist,d3dvertexbufferlist, d3dindexbufferlist, d3dtexturelist;


type
  TD3DResourceManager = class (TSharedObject)
  protected
    FVertexBuffers: Td3dVertexBufferListEx;
    FIndexBuffers: Td3dIndexBufferListEx;
    FTextures: Td3dTextureListEx;
  public
    constructor Create;override;
    destructor Destroy;override;

    property TExtures: Td3dtextureListex read FTextures;
    property IndexBuffers: Td3dindexbufferlistex read FIndexBuffers;
    property VErtexBuffers: Td3dVErtexBufferlistex read FVErtexBuffers;

  end;

implementation



{ TD3DResourceManager }

constructor TD3DResourceManager.Create;
begin
  inherited;
  FVertexBuffers := Td3dVertexBufferListEx.create;
  FIndexBuffers := Td3dIndexBufferListEx.create;
  FTextures := Td3dTextureListEx.create;

end;

destructor TD3DResourceManager.Destroy;
begin
  FIndexBuffers.free;
  FVErtexBuffers.free;
  FTExtures.free;

  inherited;
end;

end.
