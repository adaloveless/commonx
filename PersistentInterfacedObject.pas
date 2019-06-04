unit PersistentInterfacedObject;
//This unit contains the TPersistentInterfacedObject class.

interface

uses
{$IFDEF WINDOWS}
  ComObj,
  Windows,
{$ENDIF}
  systemx,typex,
  CommonInterfaces,
  SharedObject, betterobject;

{$D+}

type
//------------------------------------------------------------------------------
  TPersistentInterfacedObject = class(TSharedObject, IUnknownEnhanced)
    //TPersistentInterfacedObject is like a hybird of the VCL TInterfacedObject
    //and a standard TObject funtionality.  TPersistentInterfaced object supports
    //interface reference counting, but unlike TInterfacedObject, a class
    //descending from TPersistentInterfacedObject will NOT de destroyed
    //automatically when the reference count goes to zero.  The object, just
    //normal objects, will not go away until DESTROY/FREE is called. If DESTROY
    //is called while there are still references to the object, the object will
    //wait until the references go away to be completely destroyed.

    protected
      FCOMReferences: integer;
        //number of interfaces currently opened to the object
      bInternalRef: boolean;
        //signifies whether the object holds a reference to itself
      bFreeAtWill : boolean;
      sectCOM : TCLXCriticalSection;
    public
      procedure BeforeDestruction; override;
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; override;stdcall;
      function _Release: Integer; override;stdcall;
      function AddRefBy(const sBy: string): Integer;
      function ReleaseBy(const sBy: string): Integer;

      constructor Create; reintroduce; virtual;
      procedure ReleaseInternalRef;
      destructor Destroy; override;
      procedure FreeAtWill; virtual;
      procedure FreeWithReferences; virtual;
      procedure Free; reintroduce; virtual;
      property COMReferences: integer read FComReferences;
  end;
//------------------------------------------------------------------------------

implementation

uses
  SysUtils, debug;

//------------------------------------------------------------------------------
constructor TPersistentInterfacedObject.Create;
begin
  inherited;
  InitializeCriticalSection(sectCOM);

  //upon initialization, the object holds a reference to itself
  bInternalRef := true;
  //FComReferences := 1;
end;
//------------------------------------------------------------------------------
procedure TPersistentInterfacedObject.Free;
begin
  inherited;
end;
//------------------------------------------------------------------------------
destructor TPersistentInterfacedObject.Destroy;
begin
//  LeaveCriticalSection(sectCOM);
  DeleteCriticalSection(sectCOM);
  inherited;
end;
//------------------------------------------------------------------------------
function TPersistentInterfacedObject.AddRefBy(const sBy: string): Integer;
begin
  Debug.Log(self, 'AddRefby '+sBy);
  result := _AddRef;
end;

procedure TPersistentInterfacedObject.BeforeDestruction;
var
  iREf: integer;
begin
  if not Detached then
    Detach;
  //FreeAtWill cannot be set when destroy comes around... this may occur if
  //FreeAtWill is set, but Free is still called.
  bFreeAtWill := false;

  iRef := _RefCount;
  //Destroy is allowed if:
    //The reference count is 1 and it is an internal reference
    //-or-
    //The reference count is 0 and the free was previously called
    //(occurs when an interface reference is still pointing to the object when
    //free is called.
  if ((iREf = 1) and bInternalRef) or ((iRef = 0) and (not bInternalRef)) then begin
    exit;
  end else begin
    if bInternalRef then begin
      dec(FComReferences);
      bInternalRef := false;
    end;
    if FComReferences > 0 then begin
      raise EAbort.create('Aborting destruction of '+classname+' due to '+
                      'opened COM references. Object has '+inttostr(FComReferences)+' references');
    end;
  end;
  inherited BeforeDestruction;
end;
//------------------------------------------------------------------------------
function TPersistentInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
//IUnknown method for returning an interface supported by an object.  If the
//interface is not supported the E_NOINTERFACE constant is returned as the result.
//otherwise the result is 0.
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;


function TPersistentInterfacedObject.ReleaseBy(const sBy: string): Integer;
begin
  Debug.Log(self, 'ReleaseBy '+sBy);
  result := _Release;
end;

procedure TPersistentInterfacedObject.ReleaseInternalRef;
begin
  bInternalRef := false;
end;

function TPersistentInterfacedObject._AddRef: Integer;
begin
  ECS(sectCom);
  inc(FComReferences);
//  if self.classname = 'TReliableUDPClientEndpoint' then
//    Debug.Log(self.classname+' Reference PIO refs='+inttostr(FComReferences));
  result := FComReferences;
  LCS(sectCom);
end;

function TPersistentInterfacedObject._Release: Integer;
begin
  ECS(sectCom);
  dec(FComReferences);
  result := FComReferences;
//  if self.classname = 'TReliableUDPClientEndpoint' then
//    Debug.Log(self.classname+' Release PIO refs='+inttostr(FComReferences));

  LCS(sectCom);
  if result = 0 then
      free;
end;

procedure TPersistentInterfacedObject.FreeAtWill;
//Causes the object to be destroyed when all references are released, but does
//not put the object in a disonnected state. (before destruction code is not called)
//in essence, when this procedure is called, the PersistentInterfaced object
//acts like a standard InterfacedObject (without the persistent part)
begin
  bFreeAtWill := true;

end;

procedure TPersistentInterfacedObject.FreeWithReferences;
//IMPORTANT NOTE:  There is a difference between FreeAtWill and FreeWithReferences
//while they both are in the same general spirit. FreeAtWill will NOT free with
//1 reference active upon the time of the initial call
//-- (as needed for Data field objects).
//FreeWithReferences will act just like FREE when there is ONE reference left
//--(as needed by TDataObjectCache);
begin
  if _RefCount = 1 then begin
    if bInternalRef then begin
      bInternalRef := false;
      _Release
    end;

  end else
    bFreeAtWill := true;

end;


end.

