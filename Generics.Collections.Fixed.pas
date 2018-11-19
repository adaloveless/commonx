{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2014 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Generics.Collections.Fixed;

{$R-,T-,X+,H+,B-}
{$I DelphiDefs.inc}

interface

uses
  System.Types, System.SysUtils,

  System.Generics.Defaults;



{$IFDEF LT_XE8}
resourcestring
  sSameArrays = 'Source and Destination arrays must not be the same';
{$ENDIF}


type
{$IFDEF LT_XE8}
  TWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError, wrIOCompletion);
{$ENDIF}
  TArray = class
  private
    class procedure QuickSort<T>(var Values: array of T; Comparer: IComparer<T>;
      L, R: Integer); static;
    class procedure CheckArrays(Source, Destination: Pointer; SourceIndex, SourceLength, DestIndex, DestLength, Count: NativeInt); static;
  public
    class procedure Sort<T>(var Values: array of T); overload; static;
    class procedure Sort<T>(var Values: array of T; Comparer: IComparer<T>); overload; static;
    class procedure Sort<T>(var Values: array of T;
      Comparer: IComparer<T>; Index, Count: Integer); overload; static;

    class function BinarySearch<T>(Values: array of T; Item: T;
      out FoundIndex: Integer; Comparer: IComparer<T>;
      Index, Count: Integer): Boolean; overload; static;
    class function BinarySearch<T>(Values: array of T; Item: T;
      out FoundIndex: Integer; Comparer: IComparer<T>): Boolean; overload; static;
    class function BinarySearch<T>(Values: array of T; Item: T;
      out FoundIndex: Integer): Boolean; overload; static; static;

    class procedure Copy<T>(Source, Destination: array of T; SourceIndex, DestIndex, Count: NativeInt); overload; static;
    class procedure Copy<T>(Source, Destination: array of T; Count: NativeInt); overload; static;
  end;
  
  TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  TCollectionNotifyEvent<T> = procedure(Sender: TObject; Item: T;
    Action: TCollectionNotification) of object;

  TEnumerator<T> = class abstract
  protected
    function DoGetCurrent: T; virtual; abstract;
    function DoMoveNext: Boolean; virtual; abstract;
  public
    property Current: T read DoGetCurrent;
    function MoveNext: Boolean;
  end;

  TEnumerable<T> = class abstract
  private
  {$HINTS OFF}
    function ToArrayImpl(Count: Integer): TArray<T>; // used by descendants
  {$HINTS ON}
  protected
    function DoGetEnumerator: TEnumerator<T>; virtual; abstract;
  public
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<T>;
    function ToArray: TArray<T>; virtual;
  end;

  TArrayManager<T> = class abstract
  public
    procedure Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer); overload; virtual; abstract;
    procedure Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer); overload; virtual; abstract;
    procedure Finalize(var AArray: array of T; Index, Count: Integer); virtual; abstract;
  end;

  TMoveArrayManager<T> = class(TArrayManager<T>)
  public
    procedure Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer); overload; override;
    procedure Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer); overload; override;
    procedure Finalize(var AArray: array of T; Index, Count: Integer); override;
  end;

{$IF Defined(WEAKREF)}
  TManualArrayManager<T> = class(TArrayManager<T>)
  public
    procedure Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer); overload; override;
    procedure Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer); overload; override;
    procedure Finalize(var AArray: array of T; Index, Count: Integer); override;
  end;
{$ENDIF}

  TList<T> = class(TEnumerable<T>)
  private
  type
    arrayofT = array of T;
  var
    FItems: arrayofT;
    FCount: Integer;
    FComparer: IComparer<T>;
    FOnNotify: TCollectionNotifyEvent<T>;
    FArrayManager: TArrayManager<T>;

    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; Value: T);
    procedure Grow(ACount: Integer);
    procedure GrowCheck(ACount: Integer); inline;
    procedure DoDelete(Index: Integer; Notification: TCollectionNotification);
  protected
    function ItemValue(Item: T): NativeInt;
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure Notify(Item: T; Action: TCollectionNotification); virtual;
  public
  type
    TDirection = System.Types.TDirection;
    TEmptyFunc = reference to function (L, R: T): Boolean;
    TListCompareFunc = reference to function (L, R: T): Integer;

    constructor Create; overload;
    constructor Create(AComparer: IComparer<T>); overload;
    constructor Create(Collection: TEnumerable<T>); overload;
    destructor Destroy; override;

    class procedure Error(Msg: string; Data: NativeInt); overload; virtual;
{$IFNDEF NEXTGEN}
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
{$ENDIF  NEXTGEN}

    function Add(Value: T): Integer;

    procedure AddRange(Values: array of T); overload;
    procedure AddRange(Collection: IEnumerable<T>); overload;
    procedure AddRange(Collection: TEnumerable<T>); overload;

    procedure Insert(Index: Integer; Value: T);

    procedure InsertRange(Index: Integer; Values: array of T); overload;
    procedure InsertRange(Index: Integer; Collection: IEnumerable<T>); overload;
    procedure InsertRange(Index: Integer; Collection: TEnumerable<T>); overload;

    procedure Pack; overload;
    procedure Pack(IsEmpty: TEmptyFunc); overload;

    function Remove(Value: T): Integer;
    function RemoveItem(Value: T; Direction: TDirection): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function Extract(Value: T): T;
    function ExtractItem(Value: T; Direction: TDirection): T;

    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);

    function First: T;
    function Last: T;

    procedure Clear;

    function Expand: TList<T>;

    function Contains(Value: T): Boolean;
    function IndexOf(Value: T): Integer;
    function IndexOfItem(Value: T; Direction: TDirection): Integer;
    function LastIndexOf(Value: T): Integer;
    
    procedure Reverse;
    
    procedure Sort; overload;
    procedure Sort(AComparer: IComparer<T>); overload;
    function BinarySearch(Item: T; out Index: Integer): Boolean; overload;
    function BinarySearch(Item: T; out Index: Integer; AComparer: IComparer<T>): Boolean; overload;
    
    procedure TrimExcess;

    function ToArray: TArray<T>; override; final;
    
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property List: arrayofT read FItems;

    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;

    type
      TEnumerator = class(TEnumerator<T>)
      private
        FList: TList<T>;
        FIndex: Integer;
        function GetCurrent: T;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(AList: TList<T>);
        property Current: T read GetCurrent;
        function MoveNext: Boolean;
      end;
    
    function GetEnumerator: TEnumerator; reintroduce;
  end;

  TThreadList<T> = class
  private
    FList: TList<T>;
    FLock: TObject;
    FDuplicates: TDuplicates;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: T);
    procedure Clear;
    function LockList: TList<T>;
    procedure Remove(Item: T); inline;
    procedure RemoveItem(Item: T; Direction: TDirection);
    procedure UnlockList; inline;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;
  
  // Queue implemented over array, using wrapping.
  TQueue<T> = class(TEnumerable<T>)
  private
    FHead: Integer;
    FTail: Integer;
    FCount: Integer;
    FItems: array of T;
    FOnNotify: TCollectionNotifyEvent<T>;
    FArrayManager: TArrayManager<T>;
    procedure Grow;
    procedure SetCapacity(Value: Integer);
    function DoDequeue(Notification: TCollectionNotification): T;
    procedure DoSetCapacity(Value: Integer);
    function GetCapacity: Integer;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure Notify(Item: T; Action: TCollectionNotification); virtual;
  public
    constructor Create; overload;
    constructor Create(Collection: TEnumerable<T>); overload;
    destructor Destroy; override;
    procedure Enqueue(Value: T);
    function Dequeue: T;
    function Extract: T;
    function Peek: T;
    procedure Clear;
    procedure TrimExcess;
    property Count: Integer read FCount;
    property Capacity: Integer read GetCapacity write DoSetCapacity;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;
    function ToArray: TArray<T>; override; final;
    
    type
      TEnumerator = class(TEnumerator<T>)
      private
        FQueue: TQueue<T>;
        FIndex: Integer;
        function GetCurrent: T;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(AQueue: TQueue<T>);
        property Current: T read GetCurrent;
        function MoveNext: Boolean;
      end;
    
    function GetEnumerator: TEnumerator; reintroduce;
  end;
  
  TStack<T> = class(TEnumerable<T>)
  private
    FCount: Integer;
    FItems: array of T;
    FOnNotify: TCollectionNotifyEvent<T>;
    procedure Grow;
    function DoPop(Notification: TCollectionNotification): T;
    procedure DoSetCapacity(Value: Integer);
    function GetCapacity: Integer;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure Notify(Item: T; Action: TCollectionNotification); virtual;
  public
    constructor Create(Collection: TEnumerable<T>); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure Push(Value: T);
    function Pop: T;
    function Peek: T;
    function Extract: T;
    procedure TrimExcess;
    function ToArray: TArray<T>; override; final;
    property Count: Integer read FCount;
    property Capacity: Integer read GetCapacity write DoSetCapacity;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;
    
    type
      TEnumerator = class(TEnumerator<T>)
      private
        FStack: TStack<T>;
        FIndex: Integer;
        function GetCurrent: T;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(AStack: TStack<T>);
        property Current: T read GetCurrent;
        function MoveNext: Boolean;
      end;

    function GetEnumerator: TEnumerator; reintroduce;
  end;

  TPair<TKey,TValue> = record
    Key: TKey;
    Value: TValue;
    constructor Create(AKey: TKey; AValue: TValue);
  end;

  // Hash table using linear probing
  TDictionary<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
  private
    type
      TItem = record
        HashCode: Integer;
        Key: TKey;
        Value: TValue;
      end;
      TItemArray = array of TItem;
  private
    FItems: TItemArray;
    FCount: Integer;
    FComparer: IEqualityComparer<TKey>;
    FGrowThreshold: Integer;
    
    procedure SetCapacity(ACapacity: Integer);
    procedure Rehash(NewCapPow2: Integer);
    procedure Grow;
    function GetBucketIndex(Key: TKey; HashCode: Integer): Integer;
    function Hash(Key: TKey): Integer;
    function GetItem(Key: TKey): TValue;
    procedure SetItem(Key: TKey; Value: TValue);
    procedure RehashAdd(HashCode: Integer; Key: TKey; Value: TValue);
    procedure DoAdd(HashCode, Index: Integer; Key: TKey; Value: TValue);
    procedure DoSetValue(Index: Integer; Value: TValue);
    function DoRemove(Key: TKey; HashCode: Integer; Notification: TCollectionNotification): TValue;
  protected
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    procedure KeyNotify(Key: TKey; Action: TCollectionNotification); virtual;
    procedure ValueNotify(Value: TValue; Action: TCollectionNotification); virtual;
  public
    constructor Create(ACapacity: Integer = 0); overload;
    constructor Create(AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(ACapacity: Integer; AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(Collection: TEnumerable<TPair<TKey,TValue>>); overload;
    constructor Create(Collection: TEnumerable<TPair<TKey,TValue>>; AComparer: IEqualityComparer<TKey>); overload;
    destructor Destroy; override;
    
    procedure Add(Key: TKey; Value: TValue);
    procedure Remove(Key: TKey);
    function ExtractPair(Key: TKey): TPair<TKey,TValue>;
    procedure Clear;
    procedure TrimExcess;
    function TryGetValue(Key: TKey; out Value: TValue): Boolean;
    procedure AddOrSetValue(Key: TKey; Value: TValue);
    function ContainsKey(Key: TKey): Boolean;
    function ContainsValue(Value: TValue): Boolean;
    function ToArray: TArray<TPair<TKey,TValue>>; override; final;

    function GetItemKey(i: nativeint): TKey;
//    property KeysByIndex[i: nativeint]: TKey read GetItemKey;
    property Items[Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read FCount;

    type
      TPairEnumerator = class(TEnumerator<TPair<TKey,TValue>>)
      private
        FDictionary: TDictionary<TKey,TValue>;
        FIndex: Integer;
        function GetCurrent: TPair<TKey,TValue>;
      protected
        function DoGetCurrent: TPair<TKey,TValue>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(ADictionary: TDictionary<TKey,TValue>);
        property Current: TPair<TKey,TValue> read GetCurrent;
        function MoveNext: Boolean;
      end;

      TKeyEnumerator = class(TEnumerator<TKey>)
      private
        FDictionary: TDictionary<TKey,TValue>;
        FIndex: Integer;
        function GetCurrent: TKey;
      protected
        function DoGetCurrent: TKey; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(ADictionary: TDictionary<TKey,TValue>);
        property Current: TKey read GetCurrent;
        function MoveNext: Boolean;
      end;

      TValueEnumerator = class(TEnumerator<TValue>)
      private
        FDictionary: TDictionary<TKey,TValue>;
        FIndex: Integer;
        function GetCurrent: TValue;
      protected
        function DoGetCurrent: TValue; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(ADictionary: TDictionary<TKey,TValue>);
        property Current: TValue read GetCurrent;
        function MoveNext: Boolean;
      end;

      TValueCollection = class(TEnumerable<TValue>)
      private
        [Weak] FDictionary: TDictionary<TKey,TValue>;
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<TValue>; override;
      public
        constructor Create(ADictionary: TDictionary<TKey,TValue>);
        function GetEnumerator: TValueEnumerator; reintroduce;
        function ToArray: TArray<TValue>; override; final;
        property Count: Integer read GetCount;
      end;

      TKeyCollection = class(TEnumerable<TKey>)
      private
        [Weak] FDictionary: TDictionary<TKey,TValue>;
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<TKey>; override;
      public
        constructor Create(ADictionary: TDictionary<TKey,TValue>);
        function GetEnumerator: TKeyEnumerator; reintroduce;
        function ToArray: TArray<TKey>; override; final;
        property Count: Integer read GetCount;
      end;
      
  private
    FOnKeyNotify: TCollectionNotifyEvent<TKey>;
    FOnValueNotify: TCollectionNotifyEvent<TValue>;
    FKeyCollection: TKeyCollection;
    FValueCollection: TValueCollection;
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
  public
    function GetEnumerator: TPairEnumerator; reintroduce;
    property Keys: TKeyCollection read GetKeys;
    property  Values: TValueCollection read GetValues;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write FOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write FOnValueNotify;
  end;
  
  TObjectList<T: class> = class(TList<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(Value: T; Action: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(AComparer: IComparer<T>; AOwnsObjects: Boolean = True); overload;
    constructor Create(Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TObjectQueue<T: class> = class(TQueue<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(Value: T; Action: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Dequeue;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TObjectStack<T: class> = class(TStack<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(Value: T; Action: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(Collection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Pop;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TDictionaryOwnerships = set of (doOwnsKeys, doOwnsValues);

  TObjectDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>)
  private
    FOwnerships: TDictionaryOwnerships;
  protected
    procedure KeyNotify(Key: TKey; Action: TCollectionNotification); override;
    procedure ValueNotify(Value: TValue; Action: TCollectionNotification); override;
  public
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer = 0); overload;
    constructor Create(Ownerships: TDictionaryOwnerships;
      AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(Ownerships: TDictionaryOwnerships; ACapacity: Integer; 
      AComparer: IEqualityComparer<TKey>); overload;
  end;
  
  TThreadedQueue<T> = class
  private
    FQueue: array of T;
    FQueueSize, FQueueOffset: Integer;
    FQueueNotEmpty,
    FQueueNotFull: TObject;
    FQueueLock: TObject;
    FShutDown: Boolean;
    FPushTimeout, FPopTimeout: LongWord;
    FTotalItemsPushed, FTotalItemsPopped: LongWord;
  public
    constructor Create(AQueueDepth: Integer = 10; PushTimeout: LongWord = INFINITE; PopTimeout: LongWord = INFINITE);
    destructor Destroy; override;

    procedure Grow(ADelta: Integer);
    function PushItem(AItem: T): TWaitResult; overload;
    function PushItem(AItem: T; var AQueueSize: Integer): TWaitResult; overload;
    function PopItem: T; overload;
    function PopItem(var AQueueSize: Integer): T; overload;
    function PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult; overload;
    function PopItem(var AItem: T): TWaitResult; overload;
    procedure DoShutDown;

    property QueueSize: Integer read FQueueSize;
    property ShutDown: Boolean read FShutDown;
    property TotalItemsPushed: LongWord read FTotalItemsPushed;
    property TotalItemsPopped: LongWord read FTotalItemsPopped;
  end;

  PObject = ^TObject;

function InCircularRange(Bottom, Item, TopInc: Integer): Boolean; inline;

implementation

uses System.TypInfo, System.SysConst, System.RTLConsts;

{ TArray }

class function TArray.BinarySearch<T>(Values: array of T; Item: T;
  out FoundIndex: Integer; Comparer: IComparer<T>; Index,
  Count: Integer): Boolean;
var
  L, H: Integer;
  mid, cmp: Integer;
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) or (Count < 0)
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Count = 0 then
  begin
    FoundIndex := Index;
    Exit(False);
  end;
  
  Result := False;
  L := Index;
  H := Index + Count - 1;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := Comparer.Compare(Values[mid], Item);
    if cmp < 0 then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if cmp = 0 then
        Result := True;
    end;
  end;
  FoundIndex := L;
end;

class function TArray.BinarySearch<T>(Values: array of T; Item: T;
  out FoundIndex: Integer; Comparer: IComparer<T>): Boolean;
begin
  Result := BinarySearch<T>(Values, Item, FoundIndex, Comparer,
    Low(Values), Length(Values));
end;

class function TArray.BinarySearch<T>(Values: array of T; Item: T;
  out FoundIndex: Integer): Boolean;
begin
  Result := BinarySearch<T>(Values, Item, FoundIndex, TComparer<T>.Default, 
    Low(Values), Length(Values));
end;

class procedure TArray.Copy<T>(Source, Destination: array of T; SourceIndex, DestIndex, Count: NativeInt);
begin
  CheckArrays(PPointer(@Source)^, PPointer(@Destination)^, SourceIndex, Length(Source), DestIndex, Length(Destination), Count);
  System.CopyArray(Pointer(@Destination[SourceIndex]), Pointer(@Source[SourceIndex]), TypeInfo(T), Count);
end;

class procedure TArray.CheckArrays(Source, Destination: Pointer; SourceIndex, SourceLength, DestIndex, DestLength, Count: NativeInt);
begin
  if (SourceIndex < 0) or (DestIndex < 0) or (SourceIndex >= SourceLength) or (DestIndex >= DestLength) or
     (SourceIndex + Count >= SourceLength) or (DestIndex + Count >= DestLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Source = Destination then
    raise EArgumentException.CreateRes(@sSameArrays);
end;

class procedure TArray.Copy<T>(Source, Destination: array of T; Count: NativeInt);
begin
  Copy<T>(Source, Destination, 0, 0, Count);
end;

class procedure TArray.QuickSort<T>(var Values: array of T; Comparer: IComparer<T>;
  L, R: Integer);
var
  I, J: Integer;
  pivot, temp: T;
begin
  if (Length(Values) = 0) or ((R - L) <= 0) then
    Exit;
  repeat
    I := L;
    J := R;
    pivot := Values[L + (R - L) shr 1];
    repeat
      while Comparer.Compare(Values[I], pivot) < 0 do
        Inc(I);
      while Comparer.Compare(Values[J], pivot) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          temp := Values[I];
          Values[I] := Values[J];
          Values[J] := temp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort<T>(Values, Comparer, L, J);
    L := I;
  until I >= R;
end;

class procedure TArray.Sort<T>(var Values: array of T);
begin
  QuickSort<T>(Values, TComparer<T>.Default, Low(Values), High(Values));
end;

class procedure TArray.Sort<T>(var Values: array of T; Comparer: IComparer<T>);
begin
  QuickSort<T>(Values, Comparer, Low(Values), High(Values));
end;

class procedure TArray.Sort<T>(var Values: array of T; Comparer: IComparer<T>;
  Index, Count: Integer);
begin
  if (Index < Low(Values)) or ((Index > High(Values)) and (Count > 0))
    or (Index + Count - 1 > High(Values)) or (Count < 0)
    or (Index + Count < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Count <= 1 then
    Exit;
  QuickSort<T>(Values, Comparer, Index, Index + Count - 1);
end;

{ TEnumerator<T> }

function TEnumerator<T>.MoveNext: Boolean;
begin
  Result := DoMoveNext;
end;

{ TEnumerable<T> }

// The overridden destructor that simply invokes 'inherited' is
// required to instantiate the destructor for C++ code
destructor TEnumerable<T>.Destroy;
begin
  inherited;
end;

function TEnumerable<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := DoGetEnumerator;
end;

function TEnumerable<T>.ToArray: TArray<T>;
var
  buf: TList<T>;
  x: T;
begin
  buf := TList<T>.Create;
  try
    for x in Self do
      buf.Add(x);
    Result := buf.ToArray; // relies on TList<T>.ToArray override
  finally
    buf.Free;
  end;
end;

function TEnumerable<T>.ToArrayImpl(Count: Integer): TArray<T>;
var
  x: T;
begin
  // We assume our caller has passed correct Count
  SetLength(Result, Count);
  Count := 0;
  for x in Self do
  begin
    Result[Count] := x;
    Inc(Count);
  end;
end;

{ TList<T> }

function TList<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

procedure TList<T>.SetCapacity(Value: Integer);
begin
  if Value < Count then
    Count := Value;
  SetLength(FItems, Value);
end;

procedure TList<T>.SetCount(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > Capacity then
    SetCapacity(Value);
  if Value < Count then
    DeleteRange(Value, Count - Value);
  FCount := Value;
end;

function TList<T>.GetItem(Index: Integer): T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := FItems[Index];
end;

procedure TList<T>.SetItem(Index: Integer; Value: T);
var
  oldItem: T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  oldItem := FItems[Index];
  FItems[Index] := Value;

  Notify(oldItem, cnRemoved);
  Notify(Value, cnAdded);
end;

procedure TList<T>.Grow(ACount: Integer);
var
  newCount: Integer;
begin
  newCount := Length(FItems);
  if newCount = 0 then
    newCount := ACount
  else
    repeat
      newCount := newCount * 2;
      if newCount < 0 then
        OutOfMemoryError;
    until newCount >= ACount;
  Capacity := newCount;
end;

procedure TList<T>.GrowCheck(ACount: Integer);
begin
  if ACount > Length(FItems) then
    Grow(ACount)
  else if ACount < 0 then
    OutOfMemoryError;
end;

procedure TList<T>.Notify(Item: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

procedure TList<T>.Pack;
begin
  Pack(function (Left, Right: T): Boolean
    begin
      Result := FComparer.Compare(Left, Right) = 0;
    end);
end;

procedure TList<T>.Pack(IsEmpty: TEmptyFunc);
var
  PackedCount : Integer;
  StartIndex : Integer;
  EndIndex : Integer;
begin
  if FCount = 0 then
    Exit;

  PackedCount := 0;
  StartIndex := 0;
  repeat
    // Locate the first/next non-nil element in the list
//    while (StartIndex < FCount) and (FComparer.Compare(FItems[StartIndex], Default(T)) = 0) do
    while (StartIndex < FCount) and (IsEmpty(FItems[StartIndex], Default(T))) do
      Inc(StartIndex);

    if StartIndex < FCount then // There is nothing more to do
    begin
      // Locate the next nil pointer
      EndIndex := StartIndex;
//      while (EndIndex < FCount) and (FComparer.Compare(FItems[EndIndex], Default(T)) <> 0) do
      while (EndIndex < FCount) and not IsEmpty(FItems[EndIndex], Default(T)) do
        Inc(EndIndex);
      Dec(EndIndex);

      // Move this block of non-null items to the index recorded in PackedToCount:
      // If this is a contiguous non-nil block at the start of the list then
      // StartIndex and PackedToCount will be equal (and 0) so don't bother with the move.
      if StartIndex > PackedCount then
        FArrayManager.Move(FItems, StartIndex, PackedCount, EndIndex - StartIndex + 1);

      // Set the PackedToCount to reflect the number of items in the list
      // that have now been packed.
      Inc(PackedCount, EndIndex - StartIndex + 1);

      // Reset StartIndex to the element following EndIndex
      StartIndex := EndIndex + 1;
    end;
  until StartIndex >= FCount;

  // Set Count so that the 'free' item
  FCount := PackedCount;
end;

constructor TList<T>.Create;
begin
  Create(TComparer<T>.Default);
end;

constructor TList<T>.Create(AComparer: IComparer<T>);
begin
  inherited Create;
{$IF Defined(WEAKREF)}
  if System.HasWeakRef(T) then
    FArrayManager := TManualArrayManager<T>.Create
  else
{$ENDIF}
    FArrayManager := TMoveArrayManager<T>.Create;
  FComparer := AComparer;
  if FComparer = nil then
    FComparer := TComparer<T>.Default;
end;

constructor TList<T>.Create(Collection: TEnumerable<T>);
begin
  inherited Create;
{$IF Defined(WEAKREF)}
  if System.HasWeakRef(T) then
    FArrayManager := TManualArrayManager<T>.Create
  else
{$ENDIF}
    FArrayManager := TMoveArrayManager<T>.Create;
  FComparer := TComparer<T>.Default;
  InsertRange(0, Collection);
end;

destructor TList<T>.Destroy;
begin
  Capacity := 0;
  FArrayManager.Free;
  inherited;
end;

class procedure TList<T>.Error(Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddress;
end;

{$IFNDEF NEXTGEN}
class procedure TList<T>.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) at ReturnAddress;
end;
{$ENDIF  NEXTGEN}

function TList<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TList<T>.Add(Value: T): Integer;
begin
  GrowCheck(Count + 1);
  Result := Count;
  FItems[Count] := Value;
  Inc(FCount);
  Notify(Value, cnAdded);
end;
    
procedure TList<T>.AddRange(Values: array of T);
begin
  InsertRange(Count, Values);
end;

procedure TList<T>.AddRange(Collection: IEnumerable<T>);
begin
  InsertRange(Count, Collection);
end;

procedure TList<T>.AddRange(Collection: TEnumerable<T>);
begin
  InsertRange(Count, Collection);
end;

function TList<T>.BinarySearch(Item: T; out Index: Integer): Boolean;
begin
  Result := TArray.BinarySearch<T>(FItems, Item, Index, FComparer, 0, Count);
end;

function TList<T>.BinarySearch(Item: T; out Index: Integer;
  AComparer: IComparer<T>): Boolean;
begin
  Result := TArray.BinarySearch<T>(FItems, Item, Index, AComparer, 0, Count);
end;

procedure TList<T>.Insert(Index: Integer; Value: T);
begin
  if (Index < 0) or (Index > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  GrowCheck(Count + 1);
  if Index <> Count then
  begin
    FArrayManager.Move(FItems, Index, Index + 1, Count - Index);
    FArrayManager.Finalize(FItems, Index, 1);
  end;
  FItems[Index] := Value;
  Inc(FCount);
  Notify(Value, cnAdded);
end;

procedure TList<T>.InsertRange(Index: Integer; Values: array of T);
var
  I: Integer;
begin
  if (Index < 0) or (Index > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  GrowCheck(Count + Length(Values));
  if Index <> Count then
  begin
    FArrayManager.Move(FItems, Index, Index + Length(Values), Count - Index);
    FArrayManager.Finalize(FItems, Index, Length(Values));
  end;

  for I := 0 to Length(Values) - 1 do
    FItems[Index + I] := Values[I];

  Inc(FCount, Length(Values));

  for I := 0 to Length(Values) - 1 do
    Notify(Values[I], cnAdded);
end;

procedure TList<T>.InsertRange(Index: Integer; Collection: IEnumerable<T>);
var
  item: T;
begin
  for item in Collection do
  begin
    Insert(Index, item);
    Inc(Index);
  end;
end;

procedure TList<T>.InsertRange(Index: Integer; Collection: TEnumerable<T>);
var
  item: T;
begin
  for item in Collection do
  begin
    Insert(Index, item);
    Inc(Index);
  end;
end;

function TList<T>.ItemValue(Item: T): NativeInt;
begin
  case SizeOf(Item) of
    1: Result := PByte(@Item)[0] shl 0;
    2: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8;
    3: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16;
{$IF SizeOf(IntPtr) <= 4}
  else
    Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24;
{$ELSE}
    4: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24;
    5: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24 +
                 IntPtr(PByte(@Item)[4]) shl 32;
    6: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24 +
                 IntPtr(PByte(@Item)[4]) shl 32 + IntPtr(PByte(@Item)[5]) shl 40;
    7: Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24 +
                 IntPtr(PByte(@Item)[4]) shl 32 + IntPtr(PByte(@Item)[5]) shl 40 + IntPtr(PByte(@Item)[6]) shl 48;
 else
    Result := PByte(@Item)[0] shl 0 + PByte(@Item)[1] shl 8 + PByte(@Item)[2] shl 16 + PByte(@Item)[3] shl 24 +
              IntPtr(PByte(@Item)[4]) shl 32 + IntPtr(PByte(@Item)[5]) shl 40 + IntPtr(PByte(@Item)[6]) shl 48 + IntPtr(PByte(@Item)[7]) shl 56;
{$ENDIF}
  end;
end;

procedure TList<T>.Exchange(Index1, Index2: Integer);
var
  temp: T;
begin
  temp := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := temp;
end;

function TList<T>.Extract(Value: T): T;
begin
  Result := ExtractItem(Value, TDirection.FromBeginning);
end;

function TList<T>.ExtractItem(Value: T; Direction: TDirection): T;
var
  index: Integer;
begin
  index := IndexOfItem(Value, Direction);
  if index < 0 then
    Result := Default(T)
  else
  begin
    Result := FItems[index];
    DoDelete(index, cnExtracted);
  end;
end;

function TList<T>.First: T;
begin
  Result := Items[0];
end;

function TList<T>.Remove(Value: T): Integer;
begin
  Result := IndexOf(Value);
  if Result >= 0 then
    Delete(Result);
end;

function TList<T>.RemoveItem(Value: T; Direction: TDirection): Integer;
begin
  Result := IndexOfItem(Value, Direction);
  if Result >= 0 then
    Delete(Result);
end;

procedure TList<T>.DoDelete(Index: Integer; Notification: TCollectionNotification);
var
  oldItem: T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  oldItem := FItems[Index];
  FItems[Index] := Default(T);
  Dec(FCount);
  if Index <> Count then
  begin
    FArrayManager.Move(FItems, Index + 1, Index, Count - Index);
    FArrayManager.Finalize(FItems, Count, 1);
  end;
  Notify(oldItem, Notification);
end;

procedure TList<T>.Delete(Index: Integer);
begin
  DoDelete(Index, cnRemoved);
end;

procedure TList<T>.DeleteRange(AIndex, ACount: Integer);
var
  oldItems: array of T;
  tailCount, I: Integer;
begin
  if (AIndex < 0) or (ACount < 0) or (AIndex + ACount > Count)
    or (AIndex + ACount < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if ACount = 0 then
    Exit;
  
  SetLength(oldItems, ACount);
  FArrayManager.Move(FItems, oldItems, AIndex, 0, ACount);

  tailCount := Count - (AIndex + ACount);
  if tailCount > 0 then
  begin
    FArrayManager.Move(FItems, AIndex + ACount, AIndex, tailCount);
    FArrayManager.Finalize(FItems, Count - ACount, ACount);
  end else
    FArrayManager.Finalize(FItems, AIndex, ACount);

  Dec(FCount, ACount);

  for I := 0 to Length(oldItems) - 1 do
    Notify(oldItems[I], cnRemoved);
end;

procedure TList<T>.Clear;
begin
  Count := 0;
  Capacity := 0;
end;

function TList<T>.Expand: TList<T>;
begin
  if FCount = Length(FItems) then
    GrowCheck(FCount + 1);
  Result := Self;
end;

function TList<T>.Contains(Value: T): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

function TList<T>.IndexOf(Value: T): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if FComparer.Compare(FItems[i], Value) = 0 then
      Exit(i);
  Result := -1;
end;

function TList<T>.IndexOfItem(Value: T; Direction: TDirection): Integer;
var
  P: T;
  i: Integer;
begin
  if Direction = TDirection.FromBeginning then
    Result := IndexOf(Value)
  else
  begin
    if Count > 0 then
    begin
      for i := Count - 1 downto 0 do
        if FComparer.Compare(FItems[i], Value) = 0 then
          Exit(i);
    end;
    Result := -1;
  end;
end;

function TList<T>.Last: T;
begin
  Result := Items[Count - 1];
end;

function TList<T>.LastIndexOf(Value: T): Integer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if FComparer.Compare(FItems[i], Value) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TList<T>.Move(CurIndex, NewIndex: Integer);
var
  temp: T;
begin
  if CurIndex = NewIndex then
    Exit;
  if (NewIndex < 0) or (NewIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  temp := FItems[CurIndex];
  FItems[CurIndex] := Default(T);
  if CurIndex < NewIndex then
    FArrayManager.Move(FItems, CurIndex + 1, CurIndex, NewIndex - CurIndex)
  else
    FArrayManager.Move(FItems, NewIndex, NewIndex + 1, CurIndex - NewIndex);

  FArrayManager.Finalize(FItems, NewIndex, 1);
  FItems[NewIndex] := temp;
end;

procedure TList<T>.Reverse;
var
  tmp: T;
  b, e: Integer;
begin
  b := 0;
  e := Count - 1;
  while b < e do
  begin
    tmp := FItems[b];
    FItems[b] := FItems[e];
    FItems[e] := tmp;
    Inc(b);
    Dec(e);
  end;
end;

procedure TList<T>.Sort;
begin
  TArray.Sort<T>(FItems, FComparer, 0, Count);
end;

procedure TList<T>.Sort(AComparer: IComparer<T>);
begin
  TArray.Sort<T>(FItems, AComparer, 0, Count);
end;

function TList<T>.ToArray: TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Items[i];
end;

procedure TList<T>.TrimExcess;
begin
  Capacity := Count;
end;

function TList<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{ TList<T>.TEnumerator }

constructor TList<T>.TEnumerator.Create(AList: TList<T>);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TList<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TList<T>.TEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TList<T>.TEnumerator.GetCurrent: T;
begin
  Result := FList[FIndex];
end;

function TList<T>.TEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;
    
{ TQueue<T> }

procedure TQueue<T>.Notify(Item: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

constructor TQueue<T>.Create;
begin
  inherited Create;
{$IF Defined(WEAKREF)}
  if System.HasWeakRef(T) then
    FArrayManager := TManualArrayManager<T>.Create
  else
{$ENDIF}
    FArrayManager := TMoveArrayManager<T>.Create;
end;

function TQueue<T>.Dequeue: T;
begin
  Result := DoDequeue(cnRemoved);
end;

destructor TQueue<T>.Destroy;
begin
  Clear;
  FArrayManager.Free;
  inherited;
end;

function TQueue<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

procedure TQueue<T>.DoSetCapacity(Value: Integer);
begin
  if Value < Count then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  SetCapacity(Value);
end;

procedure TQueue<T>.Enqueue(Value: T);
begin
  if Count = Length(FItems) then
    Grow;
  FItems[FHead] := Value;
  FHead := (FHead + 1) mod Length(FItems);
  Inc(FCount);
  Notify(Value, cnAdded);
end;

function TQueue<T>.Extract: T;
begin
  Result := DoDequeue(cnExtracted);
end;

constructor TQueue<T>.Create(Collection: TEnumerable<T>);
var
  item: T;
begin
  inherited Create;
{$IF Defined(WEAKREF)}
  if System.HasWeakRef(T) then
    FArrayManager := TManualArrayManager<T>.Create
  else
{$ENDIF}
    FArrayManager := TMoveArrayManager<T>.Create;
  for item in Collection do
    Enqueue(item);
end;

function TQueue<T>.DoDequeue(Notification: TCollectionNotification): T;
begin
  if Count = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);
  Result := FItems[FTail];
  FItems[FTail] := Default(T);
  FTail := (FTail + 1) mod Length(FItems);
  Dec(FCount);
  Notify(Result, Notification);
end;

function TQueue<T>.Peek: T;
begin
  if Count = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);
  Result := FItems[FTail];
end;

procedure TQueue<T>.Clear;
begin
  while Count > 0 do
    Dequeue;
  FHead := 0;
  FTail := 0;
  FCount := 0;
end;

function TQueue<T>.ToArray: TArray<T>;
begin
  Result := ToArrayImpl(Count);
end;

procedure TQueue<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

procedure TQueue<T>.SetCapacity(Value: Integer);
var
  tailCount, offset: Integer;
begin
  offset := Value - Length(FItems);
  if offset = 0 then
    Exit;
  
  // If head <= tail, then part of the queue wraps around
  // the end of the array; don't introduce a gap in the queue.
  if (FHead < FTail) or ((FHead = FTail) and (Count > 0)) then
    tailCount := Length(FItems) - FTail
  else
    tailCount := 0;
  
  if offset > 0 then
    SetLength(FItems, Value);
  if tailCount > 0 then
  begin
    FArrayManager.Move(FItems, FTail, FTail + offset, tailCount);
    if offset > 0 then
      FArrayManager.Finalize(FItems, FTail, offset)
    else if offset < 0 then
      FArrayManager.Finalize(FItems, Count, (- offset));
    Inc(FTail, offset);
  end
  else if FTail > 0 then
  begin
    if Count > 0 then
    begin
      FArrayManager.Move(FItems, FTail, 0, Count);
      FArrayManager.Finalize(FItems, FCount, FTail);
    end;
    Dec(FHead, FTail);
    FTail := 0;
  end;
  if offset < 0 then
  begin
    SetLength(FItems, Value);
    if Value = 0 then
      FHead := 0
    else
      FHead := FHead mod Length(FItems);
  end;
end;

procedure TQueue<T>.Grow;
var
  newCap: Integer;
begin
  newCap := Length(FItems) * 2;
  if newCap = 0 then
    newCap := 4
  else if newCap < 0 then
    OutOfMemoryError;
  SetCapacity(newCap);
end;

function TQueue<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

function TQueue<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{ TQueue<T>.TEnumerator }

constructor TQueue<T>.TEnumerator.Create(AQueue: TQueue<T>);
begin
  inherited Create;
  FQueue := AQueue;
  FIndex := -1;
end;

function TQueue<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TQueue<T>.TEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TQueue<T>.TEnumerator.GetCurrent: T;
begin
  Result := FQueue.FItems[(FQueue.FTail + FIndex) mod Length(FQueue.FItems)];
end;

function TQueue<T>.TEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FQueue.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FQueue.Count;
end;

{ TStack<T> }

procedure TStack<T>.Notify(Item: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, Item, Action);
end;

constructor TStack<T>.Create(Collection: TEnumerable<T>);
var
  item: T;
begin
  inherited Create;
  for item in Collection do
    Push(item);
end;

destructor TStack<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TStack<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

procedure TStack<T>.Grow;
var
  newCap: Integer;
begin
  newCap := Length(FItems) * 2;
  if newCap = 0 then
    newCap := 4
  else if newCap < 0 then
    OutOfMemoryError;
  SetLength(FItems, newCap);
end;

procedure TStack<T>.Push(Value: T);
begin
  if Count = Length(FItems) then
    Grow;
  FItems[Count] := Value;
  Inc(FCount);
  Notify(Value, cnAdded);
end;

function TStack<T>.DoPop(Notification: TCollectionNotification): T;
begin
  if Count = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);
  Dec(FCount);
  Result := FItems[Count];
  FItems[Count] := Default(T);
  Notify(Result, Notification);
end;

procedure TStack<T>.DoSetCapacity(Value: Integer);
begin
  if Value < Count then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  SetLength(FItems, Value);
end;

function TStack<T>.Extract: T;
begin
  Result := DoPop(cnExtracted);
end;

function TStack<T>.Peek: T;
begin
  if Count = 0 then
    raise EListError.CreateRes(@SUnbalancedOperation);
  Result := FItems[Count - 1];
end;

function TStack<T>.Pop: T;
begin
  Result := DoPop(cnRemoved);
end;

procedure TStack<T>.Clear; 
begin
  while Count > 0 do
    Pop;
  SetLength(FItems, 0);
end;

function TStack<T>.ToArray: TArray<T>;
begin
  Result := ToArrayImpl(Count);
end;

procedure TStack<T>.TrimExcess;
begin
  SetLength(FItems, Count);
end;

function TStack<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

function TStack<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

constructor TStack<T>.TEnumerator.Create(AStack: TStack<T>);
begin
  inherited Create;
  FStack := AStack;
  FIndex := -1;
end;

function TStack<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TStack<T>.TEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TStack<T>.TEnumerator.GetCurrent: T;
begin
  Result := FStack.FItems[FIndex];
end;

function TStack<T>.TEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FStack.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FStack.Count;
end;

{ TPair<TKey,TValue> }

constructor TPair<TKey,TValue>.Create(AKey: TKey; AValue: TValue);
begin
  Key := AKey;
  Value := AValue;
end;

{ TDictionary<TKey,TValue> }
const
  EMPTY_HASH = -1;

procedure TDictionary<TKey,TValue>.Rehash(NewCapPow2: Integer);
var
  oldItems, newItems: TItemArray;
  i: Integer;
begin
  if NewCapPow2 = Length(FItems) then
    Exit
  else if NewCapPow2 < 0 then
    OutOfMemoryError;
  
  oldItems := FItems;
  SetLength(newItems, NewCapPow2);
  for i := 0 to Length(newItems) - 1 do
    newItems[i].HashCode := EMPTY_HASH;
  FItems := newItems;
  FGrowThreshold := NewCapPow2 shr 1 + NewCapPow2 shr 2; // 75%
  
  for i := 0 to Length(oldItems) - 1 do
    if oldItems[i].HashCode <> EMPTY_HASH then
      RehashAdd(oldItems[i].HashCode, oldItems[i].Key, oldItems[i].Value);
end;

procedure TDictionary<TKey,TValue>.SetCapacity(ACapacity: Integer);
var
  newCap: Integer;
begin
  if ACapacity < Count then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  
  if ACapacity = 0 then
    Rehash(0)
  else
  begin
    newCap := 4;
    while newCap < ACapacity do
      newCap := newCap shl 1;
    Rehash(newCap);
  end
end;

procedure TDictionary<TKey,TValue>.Grow;
var
  newCap: Integer;
begin
  newCap := Length(FItems) * 2;
  if newCap = 0 then
    newCap := 4;
  Rehash(newCap);
end;

function TDictionary<TKey,TValue>.GetBucketIndex(Key: TKey; HashCode: Integer): Integer;
var
  start, hc: Integer;
begin
  if Length(FItems) = 0 then
    Exit(not High(Integer));
  
  start := HashCode and (Length(FItems) - 1);
  Result := start;
  while True do
  begin
    hc := FItems[Result].HashCode;
    
    // Not found: return complement of insertion point.
    if hc = EMPTY_HASH then
      Exit(not Result);
    
    // Found: return location.
    if (hc = HashCode) and FComparer.Equals(FItems[Result].Key, Key) then
      Exit(Result);
    
    Inc(Result);
    if Result >= Length(FItems) then
      Result := 0;
  end;
end;

function TDictionary<TKey,TValue>.Hash(Key: TKey): Integer;
const
  PositiveMask = not Integer($80000000);
begin
  // Double-Abs to avoid -MaxInt and MinInt problems.
  // Not using compiler-Abs because we *must* get a positive integer;
  // for compiler, Abs(Low(Integer)) is a null op.
  Result := PositiveMask and ((PositiveMask and FComparer.GetHashCode(Key)) + 1);
end;

function TDictionary<TKey,TValue>.GetItem(Key: TKey): TValue;
var
  index: Integer;
begin
  index := GetBucketIndex(Key, Hash(Key));
  if index < 0 then
    raise EListError.CreateRes(@SGenericItemNotFound);
  Result := FItems[index].Value;
end;

procedure TDictionary<TKey,TValue>.SetItem(Key: TKey; Value: TValue);
var
  index: Integer;
  oldValue: TValue;
begin
  index := GetBucketIndex(Key, Hash(Key));
  if index < 0 then
    raise EListError.CreateRes(@SGenericItemNotFound);
  
  oldValue := FItems[index].Value;
  FItems[index].Value := Value;
  
  ValueNotify(oldValue, cnRemoved);
  ValueNotify(Value, cnAdded);
end;

procedure TDictionary<TKey,TValue>.RehashAdd(HashCode: Integer; Key: TKey; Value: TValue);
var
  index: Integer;
begin
  index := not GetBucketIndex(Key, HashCode);
  FItems[index].HashCode := HashCode;
  FItems[index].Key := Key;
  FItems[index].Value := Value;
end;

procedure TDictionary<TKey,TValue>.KeyNotify(Key: TKey; Action: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self, Key, Action);
end;

procedure TDictionary<TKey,TValue>.ValueNotify(Value: TValue; Action: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, Value, Action);
end;

constructor TDictionary<TKey,TValue>.Create(ACapacity: Integer = 0);
begin
  Create(ACapacity, nil);
end;

constructor TDictionary<TKey,TValue>.Create(AComparer: IEqualityComparer<TKey>);
begin
  Create(0, AComparer);
end;

constructor TDictionary<TKey,TValue>.Create(ACapacity: Integer; AComparer: IEqualityComparer<TKey>);
var
  cap: Integer;
begin
  inherited Create;
  if ACapacity < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  FComparer := AComparer;
  if FComparer = nil then
    FComparer := TEqualityComparer<TKey>.Default;
  SetCapacity(ACapacity);
end;

constructor TDictionary<TKey, TValue>.Create(Collection: TEnumerable<TPair<TKey, TValue>>);
var
  item: TPair<TKey,TValue>;
begin
  Create(0, nil);
  for item in Collection do
    AddOrSetValue(item.Key, item.Value);
end;

constructor TDictionary<TKey, TValue>.Create(Collection: TEnumerable<TPair<TKey, TValue>>;
  AComparer: IEqualityComparer<TKey>);
var
  item: TPair<TKey,TValue>;
begin
  Create(0, AComparer);
  for item in Collection do
    AddOrSetValue(item.Key, item.Value);
end;

destructor TDictionary<TKey,TValue>.Destroy;
begin
  Clear;
  FKeyCollection.Free;
  FValueCollection.Free;
  inherited;
end;

procedure TDictionary<TKey,TValue>.Add(Key: TKey; Value: TValue);
var
  index, hc: Integer;
begin
  if Count >= FGrowThreshold then
    Grow;
  
  hc := Hash(Key);
  index := GetBucketIndex(Key, hc);
  if index >= 0 then
    raise EListError.CreateRes(@SGenericDuplicateItem);
  
  DoAdd(hc, not index, Key, Value);
end;

function InCircularRange(Bottom, Item, TopInc: Integer): Boolean;
begin
  Result := (Bottom < Item) and (Item <= TopInc) // normal
    or (TopInc < Bottom) and (Item > Bottom) // top wrapped
    or (TopInc < Bottom) and (Item <= TopInc) // top and item wrapped
end;

function TDictionary<TKey,TValue>.DoRemove(Key: TKey; HashCode: Integer;
  Notification: TCollectionNotification): TValue;
var
  gap, index, hc, bucket: Integer;
begin
  index := GetBucketIndex(Key, HashCode);
  if index < 0 then
    Exit(Default(TValue));
  
  // Removing item from linear probe hash table is moderately
  // tricky. We need to fill in gaps, which will involve moving items
  // which may not even hash to the same location.
  // Knuth covers it well enough in Vol III. 6.4.; but beware, Algorithm R
  // (2nd ed) has a bug: step R4 should go to step R1, not R2 (already errata'd).
  // My version does linear probing forward, not backward, however.
  
  // gap refers to the hole that needs filling-in by shifting items down.
  // index searches for items that have been probed out of their slot,
  // but being careful not to move items if their bucket is between
  // our gap and our index (so that they'd be moved before their bucket).
  // We move the item at index into the gap, whereupon the new gap is
  // at the index. If the index hits a hole, then we're done.
  
  // If our load factor was exactly 1, we'll need to hit this hole
  // in order to terminate. Shouldn't normally be necessary, though.
  FItems[index].HashCode := EMPTY_HASH;
  Result := FItems[index].Value;
  
  gap := index;
  while True do
  begin
    Inc(index);
    if index = Length(FItems) then
      index := 0;
    
    hc := FItems[index].HashCode;
    if hc = EMPTY_HASH then
      Break;
    
    bucket := hc and (Length(FItems) - 1);
    if not InCircularRange(gap, bucket, index) then
    begin
      FItems[gap] := FItems[index];
      gap := index;
      // The gap moved, but we still need to find it to terminate.
      FItems[gap].HashCode := EMPTY_HASH;
    end;
  end;
  
  FItems[gap].HashCode := EMPTY_HASH;
  FItems[gap].Key := Default(TKey);
  FItems[gap].Value := Default(TValue);
  Dec(FCount);
  
  KeyNotify(Key, Notification);
  ValueNotify(Result, Notification);
end;

procedure TDictionary<TKey,TValue>.Remove(Key: TKey);
begin
  DoRemove(Key, Hash(Key), cnRemoved);
end;

function TDictionary<TKey,TValue>.ExtractPair(Key: TKey): TPair<TKey,TValue>;
var
  hc, index: Integer;
begin
  hc := Hash(Key);
  index := GetBucketIndex(Key, hc);
  if index < 0 then
    Exit(TPair<TKey,TValue>.Create(Key, Default(TValue)));
  
  Result := TPair<TKey,TValue>.Create(Key, DoRemove(Key, hc, cnExtracted));
end;

procedure TDictionary<TKey,TValue>.Clear;
var
  i: Integer;
  oldItems: TItemArray;
begin
  oldItems := FItems;
  FCount := 0;
  SetLength(FItems, 0);
  SetCapacity(0);
  FGrowThreshold := 0;
  
  for i := 0 to Length(oldItems) - 1 do
  begin
    if oldItems[i].HashCode = EMPTY_HASH then
      Continue;
    KeyNotify(oldItems[i].Key, cnRemoved);
    ValueNotify(oldItems[i].Value, cnRemoved);
  end;
end;

function TDictionary<TKey, TValue>.ToArray: TArray<TPair<TKey,TValue>>;
begin
  Result := ToArrayImpl(Count);
end;

procedure TDictionary<TKey,TValue>.TrimExcess;
begin
  // Ensure at least one empty slot for GetBucketIndex to terminate.
  SetCapacity(Count + 1);
end;

function TDictionary<TKey,TValue>.TryGetValue(Key: TKey; out Value: TValue): Boolean;
var
  index: Integer;
begin
  index := GetBucketIndex(Key, Hash(Key));
  Result := index >= 0;
  if Result then
    Value := FItems[index].Value
  else
    Value := Default(TValue);
end;

procedure TDictionary<TKey,TValue>.DoAdd(HashCode, Index: Integer; Key: TKey; Value: TValue);
begin
  FItems[Index].HashCode := HashCode;
  FItems[Index].Key := Key;
  FItems[Index].Value := Value;
  Inc(FCount);
  
  KeyNotify(Key, cnAdded);
  ValueNotify(Value, cnAdded);
end;

function TDictionary<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  Result := GetEnumerator;
end;

procedure TDictionary<TKey,TValue>.DoSetValue(Index: Integer; Value: TValue);
var
  oldValue: TValue;
begin
  oldValue := FItems[Index].Value;
  FItems[Index].Value := Value;
  
  ValueNotify(oldValue, cnRemoved);
  ValueNotify(Value, cnAdded);
end;


procedure TDictionary<TKey,TValue>.AddOrSetValue(Key: TKey; Value: TValue);
var
  hc: Integer;
  index: Integer;
begin
  hc := Hash(Key);
  index := GetBucketIndex(Key, hc);
  if index >= 0 then
    DoSetValue(index, Value)
  else
  begin
    // We only grow if we are inserting a new value.
    if Count >= FGrowThreshold then
    begin
      Grow;
      // We need a new Bucket Index because the array has grown.
      index := GetBucketIndex(Key, hc);
    end;
    DoAdd(hc, not index, Key, Value);
  end;
end;

function TDictionary<TKey,TValue>.ContainsKey(Key: TKey): Boolean;
begin
  Result := GetBucketIndex(Key, Hash(Key)) >= 0;
end;

function TDictionary<TKey,TValue>.ContainsValue(Value: TValue): Boolean;
var
  i: Integer;
  c: IEqualityComparer<TValue>;
begin
  c := TEqualityComparer<TValue>.Default;

  for i := 0 to Length(FItems) - 1 do
    if (FItems[i].HashCode <> EMPTY_HASH) and c.Equals(FItems[i].Value, Value) then
      Exit(True);
  Result := False;
end;

function TDictionary<TKey,TValue>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TDictionary<TKey, TValue>.GetItemKey(i: nativeint): TKey;
begin
  result := FItems[i].Key;
end;

function TDictionary<TKey,TValue>.GetKeys: TKeyCollection;
begin
  if FKeyCollection = nil then
    FKeyCollection := TKeyCollection.Create(Self);
  Result := FKeyCollection;
end;

function TDictionary<TKey,TValue>.GetValues: TValueCollection;
begin
  if FValueCollection = nil then
    FValueCollection := TValueCollection.Create(Self);
  Result := FValueCollection;
end;

// Pairs

constructor TDictionary<TKey,TValue>.TPairEnumerator.Create(ADictionary: TDictionary<TKey,TValue>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

function TDictionary<TKey, TValue>.TPairEnumerator.DoGetCurrent: TPair<TKey, TValue>;
begin
  Result := GetCurrent;
end;

function TDictionary<TKey, TValue>.TPairEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TDictionary<TKey,TValue>.TPairEnumerator.GetCurrent: TPair<TKey,TValue>;
begin
  Result.Key := FDictionary.FItems[FIndex].Key;
  Result.Value := FDictionary.FItems[FIndex].Value;
end;

function TDictionary<TKey,TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FDictionary.FItems) - 1 do
  begin
    Inc(FIndex);
    if FDictionary.FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

// Keys

constructor TDictionary<TKey,TValue>.TKeyEnumerator.Create(ADictionary: TDictionary<TKey,TValue>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.DoGetCurrent: TKey;
begin
  Result := GetCurrent;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TDictionary<TKey,TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := FDictionary.FItems[FIndex].Key;
end;

function TDictionary<TKey,TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FDictionary.FItems) - 1 do
  begin
    Inc(FIndex);
    if FDictionary.FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

// Values

constructor TDictionary<TKey,TValue>.TValueEnumerator.Create(ADictionary: TDictionary<TKey,TValue>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.DoGetCurrent: TValue;
begin
  Result := GetCurrent;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TDictionary<TKey,TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := FDictionary.FItems[FIndex].Value;
end;

function TDictionary<TKey,TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FDictionary.FItems) - 1 do
  begin
    Inc(FIndex);
    if FDictionary.FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

{ TObjectList<T> }

constructor TObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectList<T>.Create(AComparer: IComparer<T>; AOwnsObjects: Boolean);
begin
  inherited Create(AComparer);
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectList<T>.Create(Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(Collection);
  FOwnsObjects := AOwnsObjects;
end;

procedure TObjectList<T>.Notify(Value: T; Action: TCollectionNotification);
begin
  inherited;
  if OwnsObjects and (Action = cnRemoved) then
    Value.DisposeOf;
end;

{ TObjectQueue<T> }

constructor TObjectQueue<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectQueue<T>.Create(Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(Collection);
  FOwnsObjects := AOwnsObjects;
end;

procedure TObjectQueue<T>.Dequeue;
begin
  inherited Dequeue;
end;

procedure TObjectQueue<T>.Notify(Value: T; Action: TCollectionNotification);
begin
  inherited;
  if OwnsObjects and (Action = cnRemoved) then
    Value.DisposeOf;
end;

{ TObjectStack<T> }

constructor TObjectStack<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

constructor TObjectStack<T>.Create(Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(Collection);
  FOwnsObjects := AOwnsObjects;
end;

procedure TObjectStack<T>.Notify(Value: T; Action: TCollectionNotification);
begin
  inherited;
  if OwnsObjects and (Action = cnRemoved) then
    Value.DisposeOf;
end;

procedure TObjectStack<T>.Pop;
begin
  inherited Pop;
end;

{ TObjectDictionary<TKey,TValue> }

procedure TObjectDictionary<TKey,TValue>.KeyNotify(Key: TKey; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsKeys in FOwnerships) then
    PObject(@Key)^.DisposeOf;
end;

procedure TObjectDictionary<TKey,TValue>.ValueNotify(Value: TValue; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsValues in FOwnerships) then
    PObject(@Value)^.DisposeOf;
end;

constructor TObjectDictionary<TKey,TValue>.Create(Ownerships: TDictionaryOwnerships; 
  ACapacity: Integer = 0);
begin
  Create(Ownerships, ACapacity, nil);
end;

constructor TObjectDictionary<TKey,TValue>.Create(Ownerships: TDictionaryOwnerships; 
  AComparer: IEqualityComparer<TKey>);
begin
  Create(Ownerships, 0, AComparer);
end;

constructor TObjectDictionary<TKey,TValue>.Create(Ownerships: TDictionaryOwnerships; 
  ACapacity: Integer; AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(ACapacity, AComparer);
  if doOwnsKeys in Ownerships then
  begin
    if (TypeInfo(TKey) = nil) or (PTypeInfo(TypeInfo(TKey))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;

  if doOwnsValues in Ownerships then
  begin
    if (TypeInfo(TValue) = nil) or (PTypeInfo(TypeInfo(TValue))^.Kind <> tkClass) then
      raise EInvalidCast.CreateRes(@SInvalidCast);
  end;
  FOwnerships := Ownerships;
end;

{ TDictionary<TKey, TValue>.TValueCollection }

constructor TDictionary<TKey, TValue>.TValueCollection.Create(ADictionary: TDictionary<TKey, TValue>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

function TDictionary<TKey, TValue>.TValueCollection.DoGetEnumerator: TEnumerator<TValue>;
begin
  Result := GetEnumerator;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetEnumerator: TValueEnumerator;
begin
  Result := TValueEnumerator.Create(FDictionary);
end;

function TDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
begin
  Result := ToArrayImpl(FDictionary.Count);
end;

{ TDictionary<TKey, TValue>.TKeyCollection }

constructor TDictionary<TKey, TValue>.TKeyCollection.Create(
  ADictionary: TDictionary<TKey, TValue>);
begin
  inherited Create;
  FDictionary := ADictionary;
end;

function TDictionary<TKey, TValue>.TKeyCollection.DoGetEnumerator: TEnumerator<TKey>;
begin
  Result := GetEnumerator;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  Result := TKeyEnumerator.Create(FDictionary);
end;

function TDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
begin
  Result := ToArrayImpl(FDictionary.Count);
end;

{ TThreadedQueue<T> }

constructor TThreadedQueue<T>.Create(AQueueDepth: Integer = 10; PushTimeout: LongWord = INFINITE; PopTimeout: LongWord = INFINITE);
begin
  inherited Create;
  SetLength(FQueue, AQueueDepth);
  FQueueLock := TObject.Create;
  FQueueNotEmpty := TObject.Create;
  FQueueNotFull := TObject.Create;
  FPushTimeout := PushTimeout;
  FPopTimeout := PopTimeout;
end;

destructor TThreadedQueue<T>.Destroy;
begin
  DoShutDown;
  FQueueNotFull.Free;
  FQueueNotEmpty.Free;
  FQueueLock.Free;
  inherited;
end;

procedure TThreadedQueue<T>.Grow(ADelta: Integer);
begin
  TMonitor.Enter(FQueueLock);
  try
    SetLength(FQueue, Length(FQueue) + ADelta);
  finally
    TMonitor.Exit(FQueueLock);
  end;
  TMonitor.PulseAll(FQueueNotFull);
end;

function TThreadedQueue<T>.PopItem: T;
var
  LQueueSize: Integer;
begin
  PopItem(LQueueSize, Result);
end;

function TThreadedQueue<T>.PopItem(var AQueueSize: Integer; var AItem: T): TWaitResult;
begin
  AItem := Default(T);
  TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = 0) and not FShutDown do
      if not TMonitor.Wait(FQueueNotEmpty, FQueueLock, FPopTimeout) then
        Result := wrTimeout;

    if (FShutDown and (FQueueSize = 0)) or (Result <> wrSignaled) then
      Exit;

    AItem := FQueue[FQueueOffset];

    FQueue[FQueueOffset] := Default(T);

    Dec(FQueueSize);
    Inc(FQueueOffset);
    Inc(FTotalItemsPopped);

    if FQueueOffset = Length(FQueue) then
      FQueueOffset := 0;

  finally
    AQueueSize := FQueueSize;
    TMonitor.Exit(FQueueLock);
  end;

  TMonitor.Pulse(FQueueNotFull);
end;

function TThreadedQueue<T>.PopItem(var AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PopItem(LQueueSize, AItem);
end;

function TThreadedQueue<T>.PopItem(var AQueueSize: Integer): T;
begin
  PopItem(AQueueSize, Result);
end;

function TThreadedQueue<T>.PushItem(AItem: T): TWaitResult;
var
  LQueueSize: Integer;
begin
  Result := PushItem(AItem, LQueueSize);
end;

function TThreadedQueue<T>.PushItem(AItem: T; var AQueueSize: Integer): TWaitResult;
begin
  TMonitor.Enter(FQueueLock);
  try
    Result := wrSignaled;
    while (Result = wrSignaled) and (FQueueSize = Length(FQueue)) and not FShutDown do
      if not TMonitor.Wait(FQueueNotFull, FQueueLock, FPushTimeout) then
        Result := wrTimeout;

    if FShutDown or (Result <> wrSignaled) then
      Exit;

    FQueue[(FQueueOffset + FQueueSize) mod Length(FQueue)] := AItem;
    Inc(FQueueSize);
    Inc(FTotalItemsPushed);

  finally
    AQueueSize := FQueueSize;
    TMonitor.Exit(FQueueLock);
  end;

  TMonitor.Pulse(FQueueNotEmpty);
end;

procedure TThreadedQueue<T>.DoShutDown;
begin
  TMonitor.Enter(FQueueLock);
  try
    FShutDown := True;
  finally
    TMonitor.Exit(FQueueLock);
  end;
  TMonitor.PulseAll(FQueueNotFull);
  TMonitor.PulseAll(FQueueNotEmpty);
end;

{ TThreadList<T> }

procedure TThreadList<T>.Add(Item: T);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or
       (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
      raise EListError.CreateFmt(SDuplicateItem, [FList.ItemValue(Item)]);
  finally
    UnlockList;
  end;
end;

procedure TThreadList<T>.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

constructor TThreadList<T>.Create;
begin
  inherited Create;
  FLock := TObject.Create;
  FList := TList<T>.Create;
  FDuplicates := dupIgnore;
end;

destructor TThreadList<T>.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
end;

function TThreadList<T>.LockList: TList<T>;
begin
  TMonitor.Enter(FLock);
  Result := FList;
end;

procedure TThreadList<T>.Remove(Item: T);
begin
  RemoveItem(Item, TDirection.FromBeginning);
end;

procedure TThreadList<T>.RemoveItem(Item: T; Direction: TDirection);
begin
  LockList;
  try
    FList.RemoveItem(Item, Direction);
  finally
    UnlockList;
  end;
end;

procedure TThreadList<T>.UnlockList;
begin
  TMonitor.Exit(FLock);
end;

{ TArrayMoveManager<T> }

procedure TMoveArrayManager<T>.Finalize(var AArray: array of T; Index, Count: Integer);
begin
  System.FillChar(AArray[Index], Count * SizeOf(T), 0);
end;

procedure TMoveArrayManager<T>.Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer);
begin
  System.Move(AArray[FromIndex], AArray[ToIndex], Count * SizeOf(T));
end;

procedure TMoveArrayManager<T>.Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer);
begin
  System.Move(FromArray[FromIndex], ToArray[ToIndex], Count * SizeOf(T));
end;

{$IF Defined(WEAKREF)}
procedure TManualArrayManager<T>.Finalize(var AArray: array of T; Index, Count: Integer);
begin
  System.Finalize(AArray[Index], Count);
  System.FillChar(AArray[Index], Count * SizeOf(T), 0);
end;

procedure TManualArrayManager<T>.Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer);
var
  I: Integer;
begin
  if Count > 0 then
    if FromIndex < ToIndex then
      for I := Count - 1 downto 0 do
        AArray[ToIndex + I] := AArray[FromIndex + I]
    else if FromIndex > ToIndex then
      for I := 0 to Count - 1 do
        AArray[ToIndex + I] := AArray[FromIndex + I];
end;

procedure TManualArrayManager<T>.Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer);
var
  I: Integer;
begin
  if Count > 0 then
    if FromIndex < ToIndex then
      for I := Count - 1 downto 0 do
        ToArray[ToIndex + I] := FromArray[FromIndex + I]
    else if FromIndex > ToIndex then
      for I := 0 to Count - 1 do
        ToArray[ToIndex + I] := FromArray[FromIndex + I];
end;
{$ENDIF}

end.


