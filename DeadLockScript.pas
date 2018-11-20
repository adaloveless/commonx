unit DeadLockScript;

interface

uses
  typex, systemx, better_collections, betterobject;


type
  TScriptLock = class(TBetterObject)
  public
    LockCount: ni;


  end;


  TLockStack = class(TBetterStack<TScriptLock>)
  public
  end;

  TDeadLockScriptObjets = class(TList<TDeadLockScriptObject>)
  public
  end;



  TDeadLockScriptObject = class(TBetterObject)
  private
    FIdentifier: string;
  public
    constructor create;override;
    destructor destroy;override;
    property ScopeObjects: TDeadLockScriptObjects read FScopeObjects;
    property Identifier: string read FIdentifier write FIdentifier;
  end;


  TDeadLockScript = class(TDeadLockScriptObject)
  public


  end;










implementation

end.
