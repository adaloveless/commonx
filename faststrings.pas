unit faststrings;
{$MESSAGE '*******************COMPILING FastStrings.pas'}
{$DEFINE USE_FAST_STRING}
{$DEFINE USE_INTERFACE}
{x$INLINE AUTO}
{$DEFINE OPTIMAL_LOCKING}
// when assigned from string
// . create data
// . copy data
// . return result
{$Q-}
{$R-}
{$O+}

interface

uses
  betterobject, sharedobject, systemx, sysutils, numbers, debug, typex;

const
  MAX_REF_COUNT = 1;

type
  TFastChar = char;
  PFastChar = PChar;

  tfastStringData = class; // forward

  IFastStringData = interface(IUnknown)
    function GEtRealObject: TFastStringData;
    function GetRawData: PChar;
    procedure SetRawData(const Value: PChar);
    function GetData(idx: integer): TFastChar;
    procedure SetData(idx: integer; const Value: TFastChar);
    function GetLength: integer;
    procedure SetLength(const Value: integer);
    function GetDataLength: integer;
    function GetCharAddr(idx: nativeint): PFastChar;


    property Data[idx: integer]: TFastChar read GetData write SetData; default;

    property DataLength: integer read GetDataLength;
    property Length: integer read GetLength write SetLength;
    property rawData: PChar read GetRawData write SetRawData;
    function GetRefcount: integer;
    property RefCount: integer read GetRefcount;
    function CopyCreate: tfastStringData;
    procedure Allocate(iSizeInChars: nativeint);
    procedure CopyCharsFrom(a: TFastStringData; iSourceIndex: nativeint; iDestIndex: nativeint; iCount: nativeint);
    property realobject: TFastStringData read GEtRealObject;
    property CharAddr[idx: nativeint]: PFastChar read GetCharAddr;
  end;

  TfastStringData = class(TBetterObject, IFastStringData)
  private
    FCopies: integer;
//    function _AddRef: integer;
//    function _Release: integer;
//    function QueryInterface(const IID: TGUID; out Obj): HResult;
    function GetData(idx: integer): TFastChar;inline;
    procedure SetData(idx: integer; const Value: TFastChar);inline;
    function GetLength: integer;inline;
    procedure SetLength(const Value: integer);inline;
    function GetRawData: PChar;inline;
    procedure SetRawData(const Value: PChar);inline;
    function CopyCreate: tfastStringData;
    function GetRefcount: integer;inline;
    function GetDataLength: integer;inline;
    procedure CleanupDAta;
    function GEtRealObject: TFastStringData;inline;
    function GetCharAddr(idx: nativeint): PFastChar;inline;


  protected
    FStringRefCount: integer;
    FData: PChar;
    FDataLength: integer;
    FLength: integer;
  public
    function _AddRef: Integer;override;stdcall;
    function _Release: Integer;override;stdcall;

    constructor Create; reintroduce;
      virtual;

    procedure FromExternalPointer(pNew: PChar; len_in_chars: nativeint);inline;

    destructor Destroy;override;
    property Data[idx: integer]: char read GetData write SetData; default;
    property Length: integer read GetLength write SetLength;
    property DataLength: integer read GetDataLength;
    property rawData: PChar read GetRawData write SetRawData;
    property RefCount: integer read GetRefcount;
    procedure Allocate(iSizeInChars: nativeint);inline;
    procedure CopyCharsFrom(a: TFastStringData; iSourceIndex: nativeint; iDestIndex: nativeint; iCount: nativeint);inline;
    property Copies: integer read FCopies;
    procedure AddCopyReference;inline;
    procedure ReleaseCopyReference;inline;
    function QuickCopy: TfastStringData;inline;
    function SlowCopy: TFastStringData;inline;
    property realobject: TFastStringData read GEtRealObject;
    property CharAddr[idx: nativeint]: PFastChar read GetCharAddr;
  end;

  TNewString = record
  end;

  TFastString = record
  strict private

  private
    procedure BeforeChangeData;inline;
    function GetChars(idx: nativeint): char;inline;
    procedure SetChars(idx: nativeint; const Value: char);inline;
    function GetLength: nativeint;inline;
    function GetAddr(idx: nativeint): PChar;inline;

  public
{$IFDEF USE_INTERFACE}
    fsdata: IFastStringData;
{$ENDIF}
    ffsdata: TFastSTringData;
    procedure Init();

    procedure Allocate(iSize: nativeint);
    procedure FromString(s: string);

    class operator Implicit(a: TFastString): string;
    class operator Implicit(a: string): TFastString;
    class operator Implicit(a: TNewString): TFastString;
    class operator Add(a,b: TFastString): TFastString;
    class operator Equal(a,b: TFastSTring): boolean;
    class operator Equal(a:TFastSTring; b: string): boolean;
    class operator Equal(a:string; b: TFastString): boolean;


    function IsEqualTo(b: TFastSTring): boolean;overload;
    function IsEqualTo(b: string): boolean;overload;
    function CompareTo(b: TFastString): nativeint;overload;
    function CompareTo(b: string): nativeint;overload;
    property chars[idx: nativeint]: char read GetChars write SetChars;default;
    property charaddr[idx: nativeint]: PChar read GetAddr;
    procedure SetLength(const i: nativeint);
    property Length: nativeint read GetLength write SetLength;
  end;
{$IFDEF USE_FAST_STRING}
  faststring = TFastString;
{$ELSE}
  faststring = string;
{$ENDIF}

const
  NewString: TNewString = ();

function FS(const s: string): TFastString;

function fs_copy(fs: faststring; iStartat, iCount: nativeint): faststring;
procedure fs_SetLength(var fs: TFastString; iLength: nativeint);
function fs_length(var fs: TFastString): nativeint;
function fs_Pos(const SubStr, Str: faststring): nativeint;

implementation

uses
  stringx.fast;
{ TFastString }

function fs_length(var fs: TFastString): nativeint;
begin
  result := fs.Length;
end;


function fs_Pos(const SubStr, Str: faststring): nativeint;
var
  SubLen, SrcLen, Len, I, J: nativeint;
  C1: WideChar;
begin
  Result := 0;
//  if (Pointer(SubStr) = nil) or (Pointer(Str) = nil) then Exit;
  SrcLen := Str.Length;
  SubLen := SubStr.Length;
  if (SubLen <= 0) or (SrcLen <= 0) or (SrcLen < SubLen) then Exit;
  // find SubStr[1] in S[1 .. SrcLen - SubLen + 1]
  Len := SrcLen - SubLen + 1;
  C1 := SubStr[1];
  for I := 1 to Len do
  begin
    if Str.ffsdata[I] = C1 then
    begin
      Result := I + 1;
      for J := 1 to SubLen-1 do
      begin
        if Str.ffsdata[I+J] <> SubStr.ffsdata[J] then
        begin
          Result := 0;
          break;
        end;
      end;
      if Result <> 0 then
        break;
    end;
  end;
end;



function fs_copy(fs: faststring; iStartat, iCount: nativeint): faststring;
begin
  if fs.ffsdata.FData = nil then
    result := ''
  else begin
    if ((iStartAt + iCount) -1) > fs.Length then begin
      iCount := fs.Length - iStartAt; 
    end;
  
    result.Allocate(iCount);
    result.ffsdata.CopyCharsFrom(fs.ffsdata, iStartat, 0, iCount);
  end;
end;

procedure fs_SetLength(var fs: TFastString; iLength: nativeint);
//todo 4: optimize, low frequency usage
var
  s: string;
begin
  s := fs;
  system.setlength(s, iLength);
  fs.FromString(s);
end;

function FS(const s: string): TFastString;
begin
  result.FromString(s);
end;

class operator TFastString.Implicit(a: TFastString): string;
begin
  if a.ffsdata.FData = nil then
    result := ''
  else begin

    system.SetLength(result, a.ffsdata.Length);
    movemem32(@result[1], @a.ffsdata.rawData[0], a.ffsdata.Length * sizeof
        (TFastChar));


  end;

end;

class operator TFastString.Implicit(a: string): TFastString;
begin
  result.FromString(a);

end;

class operator TFastString.Add(a, b: TFastString): TFastString;
begin
  result.Allocate(a.ffsdata.Length+b.ffsdata.Length);
  result.ffsdata.CopyCharsFrom(a.ffsdata, 0, 0, a.ffsdata.Length);
  result.ffsdata.CopyCharsFrom(b.ffsdata, 0, a.ffsdata.Length, b.ffsdata.Length);

end;

procedure TFastString.Allocate(iSize: nativeint);
var
  pNew: PByte;
  l: integer;
begin
//  Debug.Log('Allocate '+inttostr(iSize));
  //Init;
  l := iSize;
  if l = 0 then
    pNew := nil
  else
    Getmem(pNew, l* sizeof(char));


  ffsdata := tfastStringData.Create;
  ffsdata.FromExternalPointer(pchar(pnew),l);
{$IFDEF USE_INTERFACE}
  fsdata := ffsdata;
{$ENDIF}

end;
procedure TFastString.BeforeChangeData;
begin
{$IFDEF USE_INTERFACE}
  if fsdata.RefCount > MAX_REF_COUNT then
    fsdata := fsdata.CopyCreate;
{$ENDIF}

end;


function TFastString.CompareTo(b: string): nativeint;
var
  t: nativeint;
begin
  result := 0;

  for t:= 0+STRZ to (greaterof(self.Length, system.Length(b))-1)+STRZ do begin
    if (t > self.Length) then begin
      result := -1;
      exit;
    end;
    if (t > system.Length(b)) then begin
      result := 1;
      exit;
    end;
    if self[t] < b[t] then begin
      result := -1;
      exit;
    end;
    if self[t] > b[t] then begin
      result := 1;
      exit;
    end;
  end;
end;

class operator TFastString.Equal(a: TFastSTring; b: string): boolean;
begin
  result := a.IsEqualTo(b);
end;

class operator TFastString.Equal(a: string; b: TFastString): boolean;
begin
  result := b.IsEqualTo(a);
end;

function TFastString.CompareTo(b: TFastString): nativeint;
var
  t: nativeint;
begin
  result := 0;
  
  for t:= 1 to greaterof(self.Length, b.Length) do begin
    if (t > self.Length) then begin
      result := -1;
      exit;
    end;
    if (t > b.Length) then begin
      result := 1;
      exit;
    end;
    if self[t] < b[t] then begin
      result := -1;
      exit;
    end;
    if self[t] > b[t] then begin
      result := 1;
      exit;
    end;
  end;


end;

class operator TFastString.Equal(a, b: TFastSTring): boolean;
begin
  result := a.IsEqualTo(b);
end;

procedure TFastString.FromString(s: string);
var
  l: nativeint;
begin

  l := system.length(s);
  self.Allocate(l);
  movemem32(Self.ffsdata.rawData, @s[1], l*sizeof(char));
end;

function TFastString.GetAddr(idx: nativeint): PChar;
begin
  result := ffsdata.CharAddr[idx-1];
end;

function TFastString.GetChars(idx: nativeint): char;
begin
  result := ffsdata.Data[idx-1];
end;

function TFastString.GetLength: nativeint;
begin
  if ffsdata = nil then
    result := 0
  else
    result := ffsdata.length;    
end;

//destructor TFastString.Destroy;
//begin
//  fsdata := nil;
//end;

class operator TFastString.Implicit(a: TNewString): TFastString;
begin
  result.Init;

end;

procedure TFastString.Init;
begin
  fsdata := nil;
  ffsdata := nil;
end;

function TFastString.IsEqualTo(b: string): boolean;
begin
  result := CompareTo(b) = 0;
end;

function TFastString.IsEqualTo(b: TFastSTring): boolean;
begin
  if self.Length <> b.Length then begin
    result := false;
   exit;
  end;

  if (self.Length = 0) and (b.Length = 0) then begin
    result := true;
    exit;
  end else begin
    result := CompareMem(self.ffsdata.FData, b.ffsdata.FData, self.Length * sizeof(char));
  end;
end;

procedure TFastString.SetChars(idx: nativeint; const Value: char);
begin
  BeforeChangeData;
  ffsdata.Data[idx-1] := value;
end;

procedure TFastString.SetLength(const i: nativeint);
begin
  self.Allocate(i);
end;

{ TFastStringData }

procedure TfastStringData.AddCopyReference;
begin
  interlockedincrement(FCopies);
//  Lock;
//  inc(FCopies);
//  Unlock;
end;

procedure TfastStringData.Allocate(iSizeInChars: nativeint);
begin
  if iSizeInChars <> FLength then begin
    CleanupData;
    FData := GetMemory(iSizeinChars * sizeof(char));
  end;
  FLength := iSizeInChars;
  FDataLength := iSizeinChars * sizeof(char);

end;

procedure TfastStringData.CopyCharsFrom(a: TFastStringData; iSourceIndex,
  iDestIndex, iCount: nativeint);
begin
  movemem32(@FData[iDestIndex], @a.FData[iSourceIndex], iCount*sizeof(char));

end;

function TFastStringData.CopyCreate: tfastStringData;
var
  pNew: PChar;
begin
{$IFNDEF OPTIMAL_LOCKING}
  Lock;
  try
{$ENDIF}
    Getmem(pNew, Length);
    movemem32(pNew, FData, Length);

    result := tfastStringData.Create;
    result.FromExternalPointer(pNew, Length);
{$IFNDEF OPTIMAL_LOCKING}
  finally
    Unlock;
  end;
{$ENDIF}
end;

constructor tfastStringData.Create;
begin
  FRefCount := 0;
  inherited Create;
  FreeWithReferences := true;
//  FLength := len_in_chars;
end;

procedure TFastSTringDAta.CleanupDAta;
begin
  if FData <> nil then
    FreeMem(FData);

end;
destructor tfastStringData.Destroy;
begin
  CleanupData;

  inherited;
end;

procedure TfastStringData.FromExternalPointer(pNew: PChar; len_in_chars: nativeint);
begin
  rawData := pNew;
  FDataLength := len_in_chars * sizeof(TFastChar);
  FLength := len_in_chars;
end;

function TfastStringData.GetCharAddr(idx: nativeint): PFastChar;
begin
  result := @Self.FData[idx];
end;

function tfastStringData.GetData(idx: integer): TFastChar;
begin
  result := FData[idx];
end;

function tfastStringData.GetDataLength: integer;
begin
{$IFNDEF OPTIMAL_LOCKING}
  Lock;
  try
{$ENDIF}
    result := FDataLength;
{$IFNDEF OPTIMAL_LOCKING}
  finally
    Unlock;
  end;
{$ENDIF}
end;

function tfastStringData.GetLength: integer;
begin
{$IFNDEF OPTIMAL_LOCKING}
  Lock;
  try
{$ENDIF}
    result := FLength;
{$IFNDEF OPTIMAL_LOCKING}
  finally
    Unlock;
  end;
{$ENDIF}
end;

function tfastStringData.GetRawData: PChar;
begin
  result := @FData[0];
end;

function TfastStringData.GEtRealObject: TFastStringData;
begin
  result := self;
end;

function tfastStringData.GetRefcount: integer;
begin
  result := FStringRefCount;
end;

function TfastStringData.QuickCopy: TfastStringData;
begin
  result := self;
end;

procedure TfastStringData.ReleaseCopyReference;
begin
  interlockeddecrement(FCopies);
//  Lock;
//  dec(FCopies);
//  Unlock;
end;

//function tfastStringData.QueryInterface(const IID: TGUID; out Obj): HResult;
//begin
//  if GetInterface(IID, Obj) then
//    result := 0
//  else
//    result := E_NOINTERFACE;
//end;


procedure tfastStringData.SetData(idx: integer; const Value: TFastChar);
begin
  if FStringRefCount > 1 then
    raise Exception.Create(
      'Cannot change string data when reference count is > 1');

  FData[idx] := Value;
end;


procedure tfastStringData.SetLength(const Value: integer);
begin
  if Value <> FDataLength then
    Reallocmem(FData, Value * sizeof(TFastString));

  FDataLength := Value;
  FLength := FDataLength div sizeof(TFastChar);
end;

procedure tfastStringData.SetRawData(const Value: PChar);
begin
  FData := PChar(Value);
end;

function TfastStringData.SlowCopy: TFastStringData;
begin
  result := Self.CopyCreate;
end;

function TfastStringData._AddRef: Integer;
begin
  result := inherited;
//  Debug(classname+' +1 = '+inttostr(FRefCount));
end;

function TfastStringData._Release: Integer;
begin
//  Debug(classname+' -1 = '+inttostr(FRefCount-1));
  result := inherited;


end;

//function tfastStringData._AddRef: integer;
//begin
//  result := InterlockedIncrement(FRefCount);
//end;
//
//function tfastStringData._Release: integer;
//begin
//  result := InterlockedDecrement(FRefCount);
//  if result = 0 then
//    Destroy;
//end;

end.
