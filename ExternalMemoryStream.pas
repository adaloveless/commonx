unit ExternalMemoryStream;

interface

uses
  classes;

type
  TExternalMemoryStream = class(TMemoryStream)
  public
    procedure SetExternalPointer(ptr: pointer; size: nativeint);
  end;

implementation

{ TExternalMemoryStream }

procedure TExternalMemoryStream.SetExternalPointer(ptr: pointer;
  size: nativeint);
begin
  SetPointer(ptr, size);
end;

end.
