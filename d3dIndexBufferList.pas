unit d3dIndexBufferList;


interface
uses
  direct3d9_jedi, sharedobject, miscroutines, interfacelist, betterobject;


type
  _COLLECTION_ITEM_ = IDirect3DResource9;
  {$INCLUDE Template3DResourceCollectionInterface.pas}

  TD3DIndexBufferListEx = class (_COLLECTION_)
  private
  published
  public
  end;

implementation


{$INCLUDE Template3DResourceCollectionImplementation.pas}

end.

