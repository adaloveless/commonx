unit HashAlg_U;
// Description: Hash Algorithm Wrapper Base Class
// By Sarah Dean
// Email: sdean12@softhome.net
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes;


type
//  THashArray = array [0..19] of byte;
//  THashArray = array [0..39] of byte;
  THashArray = array [0..63] of byte;

type
  THashAlg = class(TComponent)
  private
    { Private declarations }
  protected
    fDigestTitle: ansistring;
    fDigestSize: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    function  HashString(theString: ansistring): THashArray; virtual; abstract;
    function  HashFile(filename: ansistring; var digest: THashArray): boolean; virtual; abstract;

    // Generate a prettyprinted version of the hash
    function  HashToDisplay(theHash: THashArray): ansistring; virtual;

    // Convert the hash (an array) into a data ansistring
    function  HashToDataString(theHash: THashArray): ansistring; virtual;

    procedure ClearHash(var theHash: THashArray);
  published
    property Title: ansistring read fDigestTitle;
    property DigestSize: integer read fDigestSize;
  end;


implementation

uses SysUtils; // needed for inttohex

constructor THashAlg.Create(AOwner: TComponent);
begin
  inherited;
  fDigestTitle := '<<UNDEFINED>>';
  fDigestSize := 0;

end;

destructor THashAlg.Destroy();
begin
  inherited;

end;

function THashAlg.HashToDisplay(theHash: THashArray): ansistring;
var
  i: integer;
  retVal: ansistring;
begin
  retVal := '';
  for i:=0 to ((DigestSize div 8)-1) do
    begin
    retVal := retVal + ansistring(inttohex(theHash[i], 2));
    end;

  Result := retVal;

end;

procedure THashAlg.ClearHash(var theHash: THashArray);
var
  i: integer;
begin
  for i:=0 to (sizeof(theHash)-1) do
    begin
    theHash[i] := 0;
    end;

end;

function THashAlg.HashToDataString(theHash: THashArray): ansistring;
var
  i: integer;
  retval: ansistring;
begin
  retval := '';
  for i:=0 to ((DigestSize div 8)-1) do
    begin
    retval := retval + AnsiChar(theHash[i]);
    end;

  Result := retval;
end;

END.

