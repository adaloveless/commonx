{
 *  Copyright (c) 2013 Maciej Izak (aka. HNB or mrmizak)
 *
 *  This software is provided 'as-is', without any express or
 *  implied warranty. In no event will the authors be held
 *  liable for any damages arising from the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute
 *  it freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented;
 *     you must not claim that you wrote the original software.
 *     If you use this software in a product, an acknowledgment
 *     in the product documentation would be appreciated but
 *     is not required.
 *
 *  2. Altered source versions must be plainly marked as such,
 *     and must not be misrepresented as being the original software.
 *
 *  3. This notice may not be removed or altered from any
 *     source distribution.
 *
 *  Homepage: https://code.google.com/p/fpc-generics-collections/
 *  SVN: http://fpc-generics-collections.googlecode.com/svn/trunk/
 *  ZIP: https://code.google.com/p/fpc-generics-collections/downloads/list
 *
 *  *LICENSE* to choose from:
 *  -zlib
 *  -beerware mixed with zlib
 *  -postcardware mixed with zlib
 *
 * If you select beerware (preferred) or postcardware, please send me email
 * (hnb.code[at]gmail.com) to get my home address.
}

unit Generics.Collections;

{$mode delphi}{$H+}
{$macro on}
{$define CUSTOM_DICTIONARY_CONSTRAINTS := TKey, TValue, TIndex, THash, THashFactory}
{$define DICTIONARY_LIST_CONSTRAINTS := TKey, TValue, TIndex, THash, THashFactory}
{$define DICTIONARY_CONSTRAINTS := TKey, TValue, TProbeSequence, THash, THashFactory}
{.$define FAST_COMPARISON}

interface

uses
    Classes, SysUtils, Generics.Defaults, Generics.Helpers;

  {  BUGS!

      http://bugs.freepascal.org/view.php?id=24283 (CRITICAL! Very Important!)
      http://bugs.freepascal.org/view.php?id=24282 (CRITICAL! Very Important!)
      http://bugs.freepascal.org/view.php?id=24254 (CRITICAL! for TArray.Sort<T>)
      http://bugs.freepascal.org/view.php?id=24287 (Very Important!)
      http://bugs.freepascal.org/view.php?id=24072 (Very Important!)
      http://bugs.freepascal.org/view.php?id=24097 (Important!)
      http://bugs.freepascal.org/view.php?id=24064 (Important!)
      http://bugs.freepascal.org/view.php?id=24071 (Important!)
      http://bugs.freepascal.org/view.php?id=24285 (Important!)
      http://bugs.freepascal.org/view.php?id=24286 similar to 24285
      http://bugs.freepascal.org/view.php?id=24458
      http://bugs.freepascal.org/view.php?id=24098 (Frustrating)
      http://bugs.freepascal.org/view.php?id=24073
      http://bugs.freepascal.org/view.php?id=24463
  }

type
  TArray<T> = array of T; // for name TArray<T> conflict with current TArray<T> record implementation

  // bug. workaround (http://bugs.freepascal.org/view.php?id=24254)
  // should be
  // TArray = record
  //   class procedure Sort<T>(...) etc.
  TCustomArrayHelper<T> = class abstract
  private
    type
      // bug (http://bugs.freepascal.org/view.php?id=24282)
      TComparerBugHack = TDefaultComparer<T>;
  protected
    // modified QuickSort from classes\lists.inc
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: Integer; const AComparer: IComparer<T>); virtual; abstract;
  public
    class procedure Sort(var AValues: array of T); overload;
    class procedure Sort(var AValues: array of T;
      const AComparer: IComparer<T>);   overload;
    class procedure Sort(var AValues: array of T;
      const AComparer: IComparer<T>; AIndex, ACount: Integer); overload;

    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: Integer; const AComparer: IComparer<T>;
      AIndex, ACount: Integer): Boolean; virtual; abstract; overload;
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: Integer; const AComparer: IComparer<T>): Boolean; overload;
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: Integer): Boolean; overload;
  end;

  TArrayHelper<T> = class(TCustomArrayHelper<T>)
  protected
    // modified QuickSort from classes\lists.inc
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: Integer; const AComparer: IComparer<T>); override;
  public
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: Integer; const AComparer: IComparer<T>;
      AIndex, ACount: Integer): Boolean; override; overload;
  end;

  TFastArrayHelper<T> = class(TCustomArrayHelper<T>)
  protected
    // modified QuickSort from classes\lists.inc
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: Integer; const AComparer: IComparer<T>); override;
  public
    class function BinarySearch(const AValues: array of T; const AItem: T;
      out AFoundIndex: Integer; const AComparer: IComparer<T>;
      AIndex, ACount: Integer): Boolean; override; overload;
  end;

  IEnumerator = interface(IInterface)
    function GetCurrent: TObject;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TObject read GetCurrent;
  end;

  IEnumerable = interface(IInterface)
    function GetEnumerator: IEnumerator;
  end;

  IEnumerator<T> = interface(IEnumerator)
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  IEnumerable<T> = interface(IEnumerable)
    function GetEnumerator: IEnumerator<T>;
  end;

  TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  TCollectionNotifyEvent<T> = procedure(ASender: TObject; const AItem: T; AAction: TCollectionNotification) of object;

  { TEnumerator }

  TEnumerator<T> = class abstract
  protected
    function DoGetCurrent: T; virtual; abstract;
    function DoMoveNext: boolean; virtual; abstract;
  public
    property Current: T read DoGetCurrent;
    function MoveNext: boolean;
  end;

  { TEnumerable }

  TEnumerable<T> = class abstract
  strict protected
  {$HINTS OFF}
    function ToArrayImpl(ACount: Integer): TArray<T>; overload; // used by descendants
  {$HINTS ON}
  protected
    function DoGetEnumerator: TEnumerator<T>; virtual; abstract;
  public
    function GetEnumerator: TEnumerator<T>; inline;
    function ToArray: TArray<T>; virtual; overload;
  end;

  // http://stackoverflow.com/questions/5232198/about-vectors-growth
  {$DEFINE CUSTOM_LIST_CAPACITY_INC := Result + Result div 2} // ~approximation to golden ratio: n = n * 1.5 }
  // {$DEFINE CUSTOM_LIST_CAPACITY_INC := Result * 2} // standard inc
  TCustomList<T> = class abstract(TEnumerable<T>)
  protected
    type // bug (http://bugs.freepascal.org/view.php?id=24282)
      TArrayHelperBugHack = TArrayHelper<T>;
  private
    FOnNotify: TCollectionNotifyEvent<T>;
    function GetCapacity: Integer; inline;
  protected
    FItemsLength: Integer;
    FItems: array of T;

    function PrepareAddingItem: integer; virtual;
    function PrepareAddingRange(ACount: integer): integer; virtual;
    procedure Notify(const AValue: T; ACollectionNotification: TCollectionNotification); virtual;
    function DoRemove(AIndex: Integer; ACollectionNotification: TCollectionNotification): T; virtual;
    procedure SetCapacity(AValue: Integer); virtual; abstract;
    function GetCount: Integer; virtual;
  public
    function ToArray: TArray<T>; override; final;

    property Count: Integer read GetCount;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;
  end;

  TCustomListEnumerator<T> = class abstract(TEnumerator< T >)
  private
    FList: TCustomList<T>;
    FIndex: Integer;
  protected
    function DoMoveNext: boolean; override;
    function DoGetCurrent: T; override;
    function GetCurrent: T; virtual;
  public
    constructor Create(AList: TCustomList<T>);
  end;

  TList<T> = class(TCustomList<T>)
  private
    type
      // bug (http://bugs.freepascal.org/view.php?id=24282)
      TValueComparerBugHack = TDefaultComparer<T>;
    var
      FComparer: IComparer<T>;
  public
    // with this type declaration i found this bugs:
    // http://bugs.freepascal.org/view.php?id=24285 (Important!)
    // http://bugs.freepascal.org/view.php?id=24286 similar to 24285
    type
      TEnumerator = class(TCustomListEnumerator<T>);

    function GetEnumerator: TEnumerator; reintroduce;
  protected
    // bug - workaround for generics type name conflict (Identifier not found)
    // (http://bugs.freepascal.org/view.php?id=24287)
    function DoGetEnumerator: Generics.Collections.fixed.TEnumerator<T>; override;

    procedure SetCapacity(AValue: Integer); override;
    procedure SetCount(AValue: Integer);
  private
    function GetItem(AIndex: Integer): T;
    procedure SetItem(AIndex: Integer; const AValue: T);
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(ACollection: TEnumerable<T>); overload;
    destructor Destroy; override;

    function Add(const AValue: T): Integer;
    procedure AddRange(const AValues: array of T); overload; inline;
    procedure AddRange(const AEnumerable: IEnumerable<T>); overload;
    procedure AddRange(AEnumerable: TEnumerable<T>); overload;

    procedure Insert(AIndex: Integer; const AValue: T);
    procedure InsertRange(AIndex: Integer; const AValues: array of T); overload;
    procedure InsertRange(AIndex: Integer; const AEnumerable: IEnumerable<T>); overload;
    procedure InsertRange(AIndex: Integer; const AEnumerable: TEnumerable<T>); overload;

    function Remove(const AValue: T): Integer;
    procedure Delete(AIndex: Integer); inline;
    procedure DeleteRange(AIndex, ACount: Integer);
    function Extract(const AIndex: Integer): T; overload;
    function Extract(const AValue: T): T; overload;

    procedure Exchange(AIndex1, AIndex2: Integer);
    procedure Move(AIndex, ANewIndex: Integer);

    function First: T; inline;
    function Last: T; inline;

    procedure Clear;

    function Contains(const AValue: T): Boolean; inline;
    function IndexOf(const AValue: T): Integer; virtual;
    function LastIndexOf(const AValue: T): Integer; virtual;

    procedure Reverse;

    procedure TrimExcess;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(const AItem: T; out AIndex: Integer): Boolean; overload;
    function BinarySearch(const AItem: T; out AIndex: Integer; const AComparer: IComparer<T>): Boolean; overload;

    property Count: Integer read FItemsLength write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

  TFastList<T> = class(TList<T>)
  public
    constructor Create;

    function IndexOf(const AValue: T): Integer; override;
    function LastIndexOf(const AValue: T): Integer; override;
  end experimental;

  TQueue<T> = class(TCustomList<T>)
  public
    type
      TEnumerator = class(TCustomListEnumerator<T>)
      public
        constructor Create(AQueue: TQueue<T>);
      end;

    function GetEnumerator: TEnumerator; reintroduce;
  protected
    // bug - workaround for generics type name conflict (Identifier not found)
    // (http://bugs.freepascal.org/view.php?id=24287)
    function DoGetEnumerator: Generics.Collections.fixed.TEnumerator<T>; override;
  private
    FLow: Integer;
  protected
    procedure SetCapacity(AValue: Integer); override;
    function DoRemove(AIndex: Integer; ACollectionNotification: TCollectionNotification): T; override;
    function GetCount: Integer; override;
  public
    constructor Create(ACollection: TEnumerable<T>); overload;
    destructor Destroy; override;
    procedure Enqueue(const AValue: T);
    function Dequeue: T;
    function Extract: T;
    function Peek: T;
    procedure Clear;
    procedure TrimExcess;
  end;

  TStack<T> = class(TCustomList<T>)
  public
    type
      TEnumerator = class(TCustomListEnumerator<T>);

    function GetEnumerator: TEnumerator; reintroduce;
  protected
    // bug - workaround for generics type name conflict (Identifier not found)
    // (http://bugs.freepascal.org/view.php?id=24287)
    function DoGetEnumerator: Generics.Collections.fixed.TEnumerator<T>; override;
    function DoRemove(AIndex: Integer; ACollectionNotification: TCollectionNotification): T; override;
    procedure SetCapacity(AValue: Integer); override;
  public
    constructor Create(ACollection: TEnumerable<T>); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure Push(const AValue: T);
    function Pop: T; inline;
    function Peek: T;
    function Extract: T; inline;
    procedure TrimExcess;
  end;

  { TPair }
  TPair<TKey, TValue> = record
  public
    Key: TKey;
    Value: TValue;
    class function Create(AKey: TKey; AValue: TValue): TPair<TKey, TValue>; static;
  end;

  // bug - should be: (http://bugs.freepascal.org/view.php?id=24283)
  // TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS> = class(TEnumerable<TPair<TKey, TValue> >);
  // and workaround bug of generic forward declaration http://bugs.freepascal.org/view.php?id=24097
  TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS> = class abstract
  public
    type
      //  workaround... for no generics types in generics types
      TDictionaryPair = TPair<TKey, TValue>;
      PDictionaryPair = ^TDictionaryPair;
      PKey = ^TKey;
      PValue = ^TValue;
      TEqualityKeyComparerBugHack = TDelphiEqualityComparer<TKey>; // bug (http://bugs.freepascal.org/view.php?id=24282)
      TEqualityValueComparerBugHack = TDelphiEqualityComparer<TValue>; // bug (http://bugs.freepascal.org/view.php?id=24282)
      // bug (http://bugs.freepascal.org/view.php?id=24282)
      //TKeyArrayHelperBugHack = TArrayHelper<TKey>;

  private
    FItemsLength: TIndex;
    FEqualityComparer: IEqualityComparer<TKey>;
    FKeys: TEnumerable<TKey>;
    FValues: TEnumerable<TValue>;
  protected
    procedure SetCapacity(ACapacity: Integer); virtual; abstract;
    // bug - can't descadent from TEnumerable (http://bugs.freepascal.org/view.php?id=24283)
    function DoGetEnumerator: TEnumerator<TDictionaryPair>; virtual; abstract; {override;}
  public
    property Count: TIndex read FItemsLength;

    procedure Clear; virtual; abstract;
    procedure Add(const APair: TPair<TKey, TValue>); virtual; abstract;
  strict private // workaround for this class because can't descadent from TEnumerable
    function ToArray(ACount: Integer): TArray<TDictionaryPair>; overload;
    // bug - can't descadent from TEnumerable (http://bugs.freepascal.org/view.php?id=24283)
    // and same bug for this version of method: (http://bugs.freepascal.org/view.php?id=24283)
    // function ToArray: TArray<TPair<TKey,TValue>>; {override; final;}
  public
    function ToArray: TArray<TDictionaryPair>; virtual; final; {override; final;} overload;

    // bug - related to TEqualityComparerBugHack (http://bugs.freepascal.org/view.php?id=24282)
    constructor Create(ACapacity: Integer = 0); virtual; overload;
    constructor Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(ACollection: TEnumerable<TDictionaryPair>); overload;
    constructor Create(ACollection: TEnumerable<TDictionaryPair>; const AComparer: IEqualityComparer<TKey>); overload;

    destructor Destroy; override;
  private
    FOnKeyNotify: TCollectionNotifyEvent<TKey>;
    FOnValueNotify: TCollectionNotifyEvent<TValue>;
  protected
    // internal error for (not reported to bugtracker):
    //pass ? procedure PairNotify(const APair: TPair; ACollectionNotification: TCollectionNotification); inline;
    //procedure PairNotify(const APair: TDictionaryPair; ACollectionNotification: TCollectionNotification); inline;
    procedure KeyNotify(const AKey: TKey; ACollectionNotification: TCollectionNotification); virtual;
    procedure ValueNotify(const AValue: TValue; ACollectionNotification: TCollectionNotification); virtual;
    procedure PairNotify(const APair: TPair<TKey, TValue>; ACollectionNotification: TCollectionNotification); inline;
    procedure SetValue(var AValue: TValue; const ANewValue: TValue);
  public
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write FOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write FOnValueNotify;
  end;

  { TCustomDictionaryEnumerator }

  TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS> = class abstract(TEnumerator< T >)
  private
    FDictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>;
    FIndex: Integer;
  protected
    function DoGetCurrent: T; override;
    function GetCurrent: T; virtual; abstract;
  public
    constructor Create(ADictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>);
  end;

  { TDictionaryEnumerable }

  TDictionaryEnumerable<TDictionaryEnumerator: TObject; // ... descadent from TDictionaryEnumerator. workaround... Should be TE:
    T, CUSTOM_DICTIONARY_CONSTRAINTS> = class(TEnumerable<T>)
  private
    FDictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>;
    function GetCount: Integer;
  public
    constructor Create(ADictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>);
    function DoGetEnumerator: TDictionaryEnumerator; override;
    function ToArray: TArray<T>; override; final;
    property Count: Integer read GetCount;
  end;

  // http://en.wikipedia.org/wiki/Open_addressing

  { TLinearProbing }

  // bug - for partial specialization TLinearProbing<TKey, TEqualityComparer, THash> = record
  // (http://bugs.freepascal.org/view.php?id=24283)
  TLinearProbing = record// class(TProbeSequence) //
  private
    FHash: UInt32;
  public
    // ? should be record constructor Create(const AKey: TKey): TLinearProbing;
    // bug - rewrited because I can't use (http://bugs.freepascal.org/view.php?id=24254)
    //    TX = class
    //        procedure Foo<T>; // Fatal: Syntax error, ";" expected but "<" found
    class function Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TLinearProbing; static;
    class function Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TLinearProbing; static;  inline;
    function Probe(I, M: Int32): Int32; inline;

    property Hash: UInt32 read FHash;
  end;

  { TQuadraticProbing }

  TQuadraticProbing = record
  private
    FHash: UInt32;
  public
    class var C1: Integer;
    class var C2: Integer;

    class constructor Create;

    class function Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TQuadraticProbing; static;
    class function Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TQuadraticProbing; static;  inline;
    function Probe(I, M: Int32): Int32; inline;

    property Hash: UInt32 read FHash;
  end;

  { TDoubleHashing }

  TDoubleHashing = record
  private
    FHash, FHash2: UInt32;
  public
    class function Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TDoubleHashing; static;
    class function Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TDoubleHashing; static;  inline;
    function Probe(I, M: Int32): Int32; inline;

    property Hash: UInt32 read FHash;
    property Hash2: UInt32 read FHash2;
  end;

  { TFastLinearProbing }

  // bug - for partial specialization TFastLinearProbing<TKey, TEqualityComparer, THash> = record
  // (http://bugs.freepascal.org/view.php?id=24283)
  TFastLinearProbing = record// class(TProbeSequence) //
  private
    FHash: UInt32;
  public
    // ? should be record constructor Create(const AKey: TKey): TLinearProbing;
    // bug - rewrited because I can't use (http://bugs.freepascal.org/view.php?id=24254)
    //    TX = class
    //        procedure Foo<T>; // Fatal: Syntax error, ";" expected but "<" found
    class function Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TFastLinearProbing; static;
    class function Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TFastLinearProbing; static;  inline;
    function Probe(I, M: Int32): Int32; inline;

    property Hash: UInt32 read FHash;
  end;

  { TFastQuadraticProbing }

  TFastQuadraticProbing = record
  private
    FHash: UInt32;
  public
    class var C1: Integer;
    class var C2: Integer;

    class constructor Create;

    class function Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TFastQuadraticProbing; static;
    class function Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TFastQuadraticProbing; static;  inline;
    function Probe(I, M: Int32): Int32; inline;

    property Hash: UInt32 read FHash;
  end;

  { TFastDoubleHashing }

  TFastDoubleHashing = record
  private
    FHash, FHash2: UInt32;
  public
    class function Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TFastDoubleHashing; static;
    class function Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TFastDoubleHashing; static;  inline;
    function Probe(I, M: Int32): Int32; inline;

    property Hash: UInt32 read FHash;
    property Hash2: UInt32 read FHash2;
  end;


  TDictionaryEnumerator<T, DICTIONARY_CONSTRAINTS> = class abstract(TCustomDictionaryEnumerator<T, TKey, TValue, Int32, THash, THashFactory>)
  protected
    function DoMoveNext: Boolean; override;
  end;

  TDictionary<DICTIONARY_CONSTRAINTS> = class(TCustomDictionary<TKey, TValue, Int32, THash, THashFactory>)
  private
    type
      PItem = ^TItem;
      TItem = record
        Hash: THash;
        // bug - when I use in this place TDictionaryPair, compiler raise fatal error (http://bugs.freepascal.org/view.php?id=24072)
        Pair: TPair<TKey, TValue>;
      end;
  var
    FItemsThreshold: TIndex;
    FItems: array of TItem;

    procedure UpdateItemsThreshold; inline;
    procedure Resize(ANewSize: Integer);
    procedure Rehash(ASizePow2: Integer);
    function PrepareAddingItem: integer;
  protected
    function FindBucketIndex(const AKey: TKey): Integer; overload; inline;
    function FindBucketIndex(const AItems: TArray<TItem>; const AKey: TKey; out AHash: THash): Integer; virtual; overload;
  public
    type
      // bug - detected during implementation following classes (http://bugs.freepascal.org/view.php?id=24071)
      // Enumerators
      TPairEnumerator = class(TDictionaryEnumerator<TDictionaryPair, DICTIONARY_CONSTRAINTS>)
      protected
        function GetCurrent: TPair<TKey,TValue>; override;
      end;

      TValueEnumerator = class(TDictionaryEnumerator<TValue, DICTIONARY_CONSTRAINTS>)
      protected
        function GetCurrent: TValue; override;
      end;

      TKeyEnumerator = class(TDictionaryEnumerator<TKey, DICTIONARY_CONSTRAINTS>)
      protected
        function GetCurrent: TKey; override;
      end;

      // Collections
      TValueCollection = class(TDictionaryEnumerable<TValueEnumerator, TValue, CUSTOM_DICTIONARY_CONSTRAINTS>);

      TKeyCollection = class(TDictionaryEnumerable<TKeyEnumerator, TKey, CUSTOM_DICTIONARY_CONSTRAINTS>);

    // bug - workaround related to lack of DoGetEnumerator (http://bugs.freepascal.org/view.php?id=24283)
    function GetEnumerator: TPairEnumerator; reintroduce;
  private
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
  private
    function GetItem(const AKey: TKey): TValue;
    procedure SetItem(const AKey: TKey; const AValue: TValue);
    procedure AddItem(var AItem: TItem; const AKey: TKey; const AValue: TValue; const AHash: THash); inline;
    function DoRemove(AIndex: Integer; ACollectionNotification: TCollectionNotification): TValue;
  protected
    procedure SetCapacity(ACapacity: Integer); override;
    // bug - can't descadent from TEnumerable (http://bugs.freepascal.org/view.php?id=24283)
    function DoGetEnumerator: TEnumerator<TDictionaryPair>; override;
  public
    procedure Add(const APair: TPair<TKey, TValue>); override; overload;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;
    procedure Remove(const AKey: TKey);
    function ExtractPair(const AKey: TKey): TPair<TKey, TValue>;
    procedure Clear; override;
    procedure TrimExcess;
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    procedure AddOrSetValue(const AKey: TKey; const AValue: TValue);
    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsValue(const AValue: TValue): Boolean; overload;
    function ContainsValue(const AValue: TValue; const AEqualityComparer: IEqualityComparer<TValue>): Boolean; virtual; overload;

    property Items[Index: TKey]: TValue read GetItem write SetItem; default;
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
  end;

  TFastDictionary<DICTIONARY_CONSTRAINTS> = class(TDictionary<DICTIONARY_CONSTRAINTS>)
  private type // bug workaround (http://bugs.freepascal.org/view.php?id=24463)
    TItem = TDictionary<DICTIONARY_CONSTRAINTS>.TItem;
  protected
    function FindBucketIndex(const AItems: TArray<TItem>; const AKey: TKey; out AHash: THash): Integer; override; overload;
  public
    constructor Create(ACapacity: Integer = 0); override;
    function ContainsValue(const AValue: TValue; const AEqualityComparer: IEqualityComparer<TValue>): Boolean; override; overload;
  end experimental;

  TDictionaryListEnumerator<T, DICTIONARY_LIST_CONSTRAINTS> = class abstract(TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS>)
  protected
    function DoMoveNext: Boolean; override;
  end;

  // http://stackoverflow.com/questions/5232198/about-vectors-growth
  {$DEFINE DICTIONARYLIST_CAPACITY_INC := Result + Result div 2} // ~approximation to golden ratio: n = n * 1.5 }
  // {$DEFINE DICTIONARYLIST_CAPACITY_INC := Result * 2} // standard inc
  TDictionaryList<DICTIONARY_LIST_CONSTRAINTS> = class(TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>)
  private
    type
      PItemIndex = ^TItemIndex;
      TItemIndex = record
        Hash: THash;
        Index: TIndex;
      end;

    var
      // FItems: array of TItem;
      FPairItems: array of TPair<TKey, TValue>;
      FIndexItems: array of TItemIndex;

    function GetItemIndex(const AKey: TKey): Integer; overload; inline;
    function GetItemIndex(const AKey: TKey; out AHash: THash): Integer; virtual; overload;

    function GetItem(const AKey: TKey): TValue;
    procedure SetItem(const AKey: TKey; const AValue: TValue);
    function GetPairItem(const AIndex: TIndex): TPair<TKey, TValue>;
    procedure SetPairItem(const AIndex: TIndex; const APair: TPair<TKey, TValue>);
    function GetKeyItem(const AIndex: TIndex): TKey;
    procedure SetKeyItem(const AIndex: TIndex; const AKey: TKey);
    function GetValueItem(const AIndex: TIndex): TValue;
    procedure SetValueItem(const AIndex: TIndex; const AValue: TValue);

    function PrepareAddingItem: integer;
    procedure InsertItem(AIndex: integer);
  public
    type
      // bug - detected during implementation following classes (http://bugs.freepascal.org/view.php?id=24071)
      // Enumerators
      TPairEnumerator = class(TDictionaryListEnumerator<TDictionaryPair, DICTIONARY_LIST_CONSTRAINTS>)
      protected
        function GetCurrent: TPair<TKey,TValue>; override;
      end;

      TValueEnumerator = class(TDictionaryListEnumerator<TValue, DICTIONARY_LIST_CONSTRAINTS>)
      protected
        function GetCurrent: TValue; override;
      end;

      TKeyEnumerator = class(TDictionaryListEnumerator<TKey, DICTIONARY_LIST_CONSTRAINTS>)
      protected
        function GetCurrent: TKey; override;
      end;

      // Collections
      TValueCollection = class(TDictionaryEnumerable<TValueEnumerator, TValue, CUSTOM_DICTIONARY_CONSTRAINTS>);

      TKeyCollection = class(TDictionaryEnumerable<TKeyEnumerator, TKey, CUSTOM_DICTIONARY_CONSTRAINTS>);

    // bug - workaround related to lack of DoGetEnumerator (http://bugs.freepascal.org/view.php?id=24283)
    function GetEnumerator: TPairEnumerator; reintroduce;
  private
    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
    function DoRemove(AIndex: Integer; ACollectionNotification: TCollectionNotification): TPair<TKey, TValue>;
  protected
    procedure SetCapacity(ACapacity: Integer); override;
    // bug - can't descadent from TEnumerable (http://bugs.freepascal.org/view.php?id=24283)
    function DoGetEnumerator: TEnumerator<TDictionaryPair>; override;
    function GetHash(const AKey: TKey): THash; virtual;
    function AddIndex(AHash: THash): Integer;
    function Add(const AKey: TKey; const AValue: TValue; AHash: THash): TIndex; overload;
  public
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    property Items[Index: TKey]: TValue read GetItem write SetItem; default;
    property PairItems[Index: TIndex]: TPair<TKey, TValue> read GetPairItem write SetPairItem;
    property KeyItems[Index: TIndex]: TKey read GetKeyItem write SetKeyItem;
    property ValueItems[Index: TIndex]: TValue read GetValueItem write SetValueItem;

    procedure Add(const APair: TPair<TKey, TValue>); override; overload;
    function Add(const AKey: TKey; const AValue: TValue): TIndex; overload;
    function ContainsKey(const AKey: TKey): Boolean; overload;
    //function ContainsKey(const AKey: TKey; out AIndex: TIndex): Boolean;
    //function ContainsKey(const AValue: TValue; out APtrToKey: PKey): Boolean;
    //function ContainsKey(const AValue: TValue; out APtrToPair: PDictionaryPair): Boolean;

    function ContainsValue(const AValue: TValue): Boolean; overload;
    function ContainsValue(const AValue: TValue; const AEqualityComparer: IEqualityComparer<TValue>): Boolean; virtual; overload;
    //function ContainsValue(const AValue: TValue; out AIndex: TIndex): Boolean;
    //function ContainsValue(const AValue: TValue; out APtrToValue: TIndex): Boolean;
    //function ContainsValueList(const AValue: TValue; out AValueList: TArray<TIndex>): Boolean;
    //function ContainsValueList(const AValue: TValue; out AValueList: TArray<PValue>): Boolean;
    //function ContainsValueList(const AValue: TValue; out APairList: TArray<PDictionaryPair>): Boolean;

    procedure Remove(const AKey: TKey);
    function ExtractPair(const AKey: TKey): TPair<TKey,TValue>;
    procedure Clear; override;
    procedure TrimExcess;
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    procedure AddOrSetValue(const AKey: TKey; const AValue: TValue);
    //function ToArray: TArray<TPair<TKey,TValue>>; override; final;

    // from TList
    procedure Insert(AIndex: Integer; const AKey: TKey; const AValue: TValue);
    procedure Exchange(AIndex1, AIndex2: Integer);
    // procedure Move(AIndex, ANewIndex: Integer); - cant be implemented in Dictionary (needed unique key)

    function First: TPair<TKey, TValue>; inline;
    function Last: TPair<TKey, TValue>; inline;
    function IndexOfValue(const AValue: TValue; const AEqualityComparer: IEqualityComparer<TValue> = nil): Integer;
    function IndexOfKey(const AKey: TKey): Integer;
    function IndexOfPair(const APair: TPair<TKey, TValue>; const AEqualityComparer: IEqualityComparer<TValue> = nil): Integer;
    function LastIndexOfValue(const AValue: TValue; const AEqualityComparer: IEqualityComparer<TValue> = nil): Integer;

    procedure Reverse;
    //procedure SortByKey; overload; // special version of equality comparer
    //procedure SortByKey(const AComparer: IComparer<TKey>); overload;
    //procedure SortByValue; overload;
    //procedure SortByValue(const AComparer: IComparer<TValue>); overload;

    //function BinarySearch(const AItem: T; out AIndex: Integer): Boolean; overload;
    //function BinarySearch(const AItem: T; out AIndex: Integer; const AComparer: IComparer<T>): Boolean; overload; }
  end;

  TFastDictionaryList<DICTIONARY_LIST_CONSTRAINTS> = class(TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>)
  private
    function GetItemIndex(const AKey: TKey; out AHash: THash): Integer; override;
  protected
    function GetHash(const AKey: TKey): THash; override;
  public
    function ContainsValue(const AValue: TValue; const AEqualityComparer: IEqualityComparer<TValue>): Boolean; override; overload;

    function IndexOfValue(const AValue: TValue; const AEqualityComparer: IEqualityComparer<TValue> = nil): Integer; reintroduce;
    function IndexOfKey(const AKey: TKey): Integer; reintroduce;
    function IndexOfPair(const APair: TPair<TKey, TValue>; const AEqualityComparer: IEqualityComparer<TValue> = nil): Integer; reintroduce;
    function LastIndexOfValue(const AValue: TValue; const AEqualityComparer: IEqualityComparer<TValue> = nil): Integer; reintroduce;
  end experimental;

  // http://bugs.freepascal.org/view.php?id=24458
  // TObjectList<T: class> = class(TList<T>)
  TObjectList<T: TObject> = class(TList<T>)
  private
    FObjectsOwner: Boolean;
  protected
    procedure Notify(const AValue: T; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean = True); overload;
    constructor Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    property OwnsObjects: Boolean read FObjectsOwner write FObjectsOwner;
  end;

  // http://bugs.freepascal.org/view.php?id=24458
  // TObjectQueue<T: class> = class(TQueue<T>)
  TObjectQueue<T: TObject> = class(TQueue<T>)
  private
    FObjectsOwner: Boolean;
  protected
    procedure Notify(const AValue: T; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Dequeue;
    property OwnsObjects: Boolean read FObjectsOwner write FObjectsOwner;
  end;

  // http://bugs.freepascal.org/view.php?id=24458
  // TObjectStack<T: class> = class(TStack<T>)
  TObjectStack<T: TObject> = class(TStack<T>)
  private
    FObjectsOwner: Boolean;
  protected
    procedure Notify(const AValue: T; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Pop;
    property OwnsObjects: Boolean read FObjectsOwner write FObjectsOwner;
  end;

  TDictionaryOwnerships = set of (doOwnsKeys, doOwnsValues);

  TObjectDictionary<DICTIONARY_CONSTRAINTS> = class(TDictionary<DICTIONARY_CONSTRAINTS>)
  private
    FOwnerships: TDictionaryOwnerships;
  protected
    procedure KeyNotify(const AKey: TKey; ACollectionNotification: TCollectionNotification); override;
    procedure ValueNotify(const AValue: TValue; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(AOwnerships: TDictionaryOwnerships; ACapacity: Integer = 0); overload;
    constructor Create(AOwnerships: TDictionaryOwnerships;
      const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(AOwnerships: TDictionaryOwnerships; ACapacity: Integer;
      const AComparer: IEqualityComparer<TKey>); overload;
  end;

  TDictionaryList<TKey, TValue> = class(TDictionary<TKey, TValue, Int32, UInt32, TDelphiHashFactory>);

  TDictionary<TKey, TValue> = class(TDictionary<TKey, TValue, TFastLinearProbing, UInt32, TDelphiHashFactory>);
  TObjectDictionary<TKey, TValue> = class(TObjectDictionary<TKey, TValue, TFastLinearProbing, UInt32, TDelphiHashFactory>);

resourcestring
  SArgumentOutOfRange = 'Argument out of range';
  SDuplicatesNotAllowed = 'Duplicates not allowed in dictionary';
  SDictionaryKeyDoesNotExist = 'Dictionary key does not exist';
  SItemNotFound = 'Item not found';

implementation

class function TCustomArrayHelper<T>.BinarySearch(const AValues: array of T; const AItem: T;
  out AFoundIndex: Integer; const AComparer: IComparer<T>): Boolean;
begin
  Result := BinarySearch(AValues, AItem, AFoundIndex, AComparer, Low(AValues), Length(AValues));
end;

class function TCustomArrayHelper<T>.BinarySearch(const AValues: array of T; const AItem: T;
  out AFoundIndex: Integer): Boolean;
begin
  Result := BinarySearch(AValues, AItem, AFoundIndex, TComparerBugHack.Default, Low(AValues), Length(AValues));
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T);
begin
  QuickSort(AValues, Low(AValues), High(AValues), TComparerBugHack.Default);
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T;
  const AComparer: IComparer<T>);
begin
  QuickSort(AValues, Low(AValues), High(AValues), AComparer);
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T;
  const AComparer: IComparer<T>; AIndex, ACount: Integer);
begin
  if ACount <= 1 then
    Exit;
  QuickSort(AValues, AIndex, Pred(AIndex + ACount), AComparer);
end;

{ TArrayHelper<T> }

class procedure TArrayHelper<T>.QuickSort(var AValues: array of T; ALeft, ARight: Integer; const AComparer: IComparer<T>);
var
  I, J: Integer;
  P, Q: T;
begin
  if ((ARight - ALeft) <= 0) or (Length(AValues) = 0) then
    Exit;
  repeat
    I := ALeft;
    J := ARight;
    P := AValues[ALeft + (ARight - ALeft) shr 1];
    repeat
{$IFDEF FAST_COMPARISON}
        if AComparer = nil then
          while AValues[I] < P do
            I += 1
        else
{$ENDIF}
        while AComparer.Compare(AValues[I].GetReferenceToValue, P.GetReferenceToValue, AValues[I].GetValueSize, P.GetValueSize) < 0 do
          I += 1;
{$IFDEF FAST_COMPARISON}
        if AComparer = nil then
          while AValues[J] > P do
            J -= 1
        else
{$ENDIF}
        while AComparer.Compare(AValues[J].GetReferenceToValue, P.GetReferenceToValue, AValues[J].GetValueSize, P.GetValueSize) > 0 do
          J -= 1;
      if I <= J then
      begin
        if I <> J then
        begin
          Q := AValues[I];
          AValues[I] := AValues[J];
          AValues[J] := Q;
        end;
        I += 1;
        J -= 1;
      end;
    until I > J;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - ALeft < ARight - I then
    begin
      if ALeft < J then
        QuickSort(AValues, ALeft, J, AComparer);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        QuickSort(AValues, I, ARight, AComparer);
      ARight := J;
    end;
   until ALeft >= ARight;
end;

class function TArrayHelper<T>.BinarySearch(const AValues: array of T; const AItem: T;
  out AFoundIndex: Integer; const AComparer: IComparer<T>;
  AIndex, ACount: Integer): Boolean;
var
  imin, imax, imid: Int32;
{$IFDEF FAST_COMPARISON}
  LCompare: Boolean;
{$ELSE}
  LCompare: Integer;
{$ENDIF}
begin
    // continually narrow search until just one element remains
  imin := AIndex;
  imax := Pred(AIndex + ACount);

  // http://en.wikipedia.org/wiki/Binary_search_algorithm
  while (imin < imax) do
  begin
        imid := imin + ((imax - imin) shr 1);

        // code must guarantee the interval is reduced at each iteration
        // assert(imid < imax);
        // note: 0 <= imin < imax implies imid will always be less than imax

{$IFDEF FAST_COMPARISON}
        if AComparer = nil then
           LCompare := AValues[imid] < AItem
        else
          LCompare := AComparer.Compare(AValues[imid].GetReferenceToValue, AItem.GetReferenceToValue,
            AValues[imid].GetValueSize, AItem.GetValueSize) < 0;
{$ELSE}
        LCompare := AComparer.Compare(AValues[imid].GetReferenceToValue, AItem.GetReferenceToValue,
          AValues[imid].GetValueSize, AItem.GetValueSize);
{$ENDIF}
        // reduce the search
{$IFDEF FAST_COMPARISON}
        if LCompare then
{$ELSE}
        if (LCompare < 0) then
{$ENDIF}
          imin := imid + 1
        else
        begin
          imax := imid;
{$IFNDEF FAST_COMPARISON}
          if LCompare = 0 then
          begin
            AFoundIndex := imid;
            Exit(True);
          end;
{$ENDIF}
        end;
  end;
    // At exit of while:
    //   if A[] is empty, then imax < imin
    //   otherwise imax == imin

    // deferred test for equality

{$IFDEF FAST_COMPARISON}
  if AComparer = nil then
     LCompare := AValues[imin] = AItem
  else
    LCompare := AComparer.Compare(AValues[imin].GetReferenceToValue, AItem.GetReferenceToValue,
      AValues[imin].GetValueSize, AItem.GetValueSize) = 0;
{$ELSE}
  LCompare := AComparer.Compare(AValues[imin].GetReferenceToValue, AItem.GetReferenceToValue,
    AValues[imin].GetValueSize, AItem.GetValueSize);
{$ENDIF}

{$IFDEF FAST_COMPARISON}
  if (imax = imin) and LCompare then
{$ELSE}
  if (imax = imin) and (LCompare = 0) then
{$ENDIF}
  begin
    AFoundIndex := imin;
    Exit(True);
  end
  else
  begin
    AFoundIndex := -1;
    Exit(False);
  end;
end;

{ TFastArrayHelper<T> }

class procedure TFastArrayHelper<T>.QuickSort(var AValues: array of T; ALeft, ARight: Integer; const AComparer: IComparer<T>);
var
  I, J: Integer;
  P, Q: T;
begin
  if ((ARight - ALeft) <= 0) or (Length(AValues) = 0) then
    Exit;
  repeat
    I := ALeft;
    J := ARight;
    P := AValues[ALeft + (ARight - ALeft) shr 1];
    repeat
        if AComparer = nil then
          while AValues[I] < P do
            I += 1
        else
        while AComparer.Compare(AValues[I].GetReferenceToValue, P.GetReferenceToValue, AValues[I].GetValueSize, P.GetValueSize) < 0 do
          I += 1;
        if AComparer = nil then
          while AValues[J] > P do
            J -= 1
        else
        while AComparer.Compare(AValues[J].GetReferenceToValue, P.GetReferenceToValue, AValues[J].GetValueSize, P.GetValueSize) > 0 do
          J -= 1;
      if I <= J then
      begin
        if I <> J then
        begin
          Q := AValues[I];
          AValues[I] := AValues[J];
          AValues[J] := Q;
        end;
        I += 1;
        J -= 1;
      end;
    until I > J;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - ALeft < ARight - I then
    begin
      if ALeft < J then
        QuickSort(AValues, ALeft, J, AComparer);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        QuickSort(AValues, I, ARight, AComparer);
      ARight := J;
    end;
   until ALeft >= ARight;
end;

class function TFastArrayHelper<T>.BinarySearch(const AValues: array of T; const AItem: T;
  out AFoundIndex: Integer; const AComparer: IComparer<T>;
  AIndex, ACount: Integer): Boolean;
var
  imin, imax, imid: Int32;
  i: Int32;
  LCompare: Boolean;
begin
    // continually narrow search until just one element remains
  imin := AIndex;
  imax := Pred(AIndex + ACount);

  // http://en.wikipedia.org/wiki/Binary_search_algorithm
  while (imin < imax) do
  begin
        imid := imin + ((imax - imin) shr 1);

        // code must guarantee the interval is reduced at each iteration
        // assert(imid < imax);
        // note: 0 <= imin < imax implies imid will always be less than imax

        if AComparer = nil then
           LCompare := AValues[imid] < AItem
        else
          LCompare := AComparer.Compare(AValues[imid].GetReferenceToValue, AItem.GetReferenceToValue,
            AValues[imid].GetValueSize, AItem.GetValueSize) < 0;
        // reduce the search
        if LCompare then
          imin := imid + 1
        else
        begin
          imax := imid;
        end;
  end;
    // At exit of while:
    //   if A[] is empty, then imax < imin
    //   otherwise imax == imin

    // deferred test for equality

  if AComparer = nil then
     LCompare := AValues[imin] = AItem
  else
    LCompare := AComparer.Compare(AValues[imin].GetReferenceToValue, AItem.GetReferenceToValue,
      AValues[imin].GetValueSize, AItem.GetValueSize) = 0;

  if (imax = imin) and LCompare then
  begin
    AFoundIndex := imin;
    Exit(True);
  end
  else
  begin
    AFoundIndex := -1;
    Exit(False);
  end;
end;

{ TEnumerableCollection<T> }

function TCustomList<T>.PrepareAddingItem: integer;
begin
  Result := Length(FItems);

  if (FItemsLength < 4) and (Result < 4) then
    SetLength(FItems, 4)
  else if FItemsLength = High(FItemsLength) then
    OutOfMemoryError
  else if FItemsLength = Result then
    SetLength(FItems, CUSTOM_LIST_CAPACITY_INC);

  Result := FItemsLength;
  Inc(FItemsLength);
end;

function TCustomList<T>.PrepareAddingRange(ACount: integer): integer;
begin
  if ACount < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if ACount = 0 then
    Exit(FItemsLength - 1);

  Result := Length(FItems);

  if (FItemsLength = 0) and (Result = 0) then
    SetLength(FItems, 4)
  else if FItemsLength = High(FItemsLength) then
    OutOfMemoryError;

  while Pred(FItemsLength + ACount) >= Result do
  begin
    SetLength(FItems, CUSTOM_LIST_CAPACITY_INC);
    Result := Length(FItems);
  end;

  Result := FItemsLength;
  Inc(FItemsLength, ACount);
end;

function TCustomList<T>.ToArray: TArray<T>;
begin
  Result := ToArrayImpl(Count);
end;

function TCustomList<T>.GetCount: Integer;
begin
  Result := FItemsLength;
end;

procedure TCustomList<T>.Notify(const AValue: T; ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, AValue, ACollectionNotification);
end;

function TCustomList<T>.DoRemove(AIndex: Integer; ACollectionNotification: TCollectionNotification): T;
begin
  if (AIndex < 0) or (AIndex >= FItemsLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[AIndex];
  Dec(FItemsLength);

  FItems[AIndex] := Default(T);
  if AIndex <> FItemsLength then
  begin
    System.Move(FItems[AIndex + 1], FItems[AIndex], Pred(FItemsLength - AIndex) * SizeOf(T));
    FillChar(FItems[FItemsLength], SizeOf(T), 0);
  end;

  Notify(Result, ACollectionNotification);
end;

function TCustomList<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

{ TCustomListEnumerator<T> }

function TCustomListEnumerator<T>.DoMoveNext: boolean;
begin
  Inc(FIndex);
  Result := (FList.FItemsLength <> 0) and (FIndex < FList.FItemsLength)
end;

function TCustomListEnumerator<T>.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TCustomListEnumerator<T>.GetCurrent: T;
begin
  Result := FList.FItems[FIndex];
end;

constructor TCustomListEnumerator<T>.Create(AList: TCustomList<T>);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

{ TList<T> }

constructor TList<T>.Create;
begin
{$IFNDEF FAST_COMPARISON}
  FComparer := TComparer<T>.Default;
{$ENDIF}
end;

constructor TList<T>.Create(const AComparer: IComparer<T>);
begin
  FComparer := AComparer;
end;

constructor TList<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  Create;
  for LItem in ACollection do
    Add(LItem);
end;

destructor TList<T>.Destroy;
begin
  SetCapacity(0);
end;

procedure TList<T>.SetCapacity(AValue: Integer);
begin
  if AValue < Count then
    Count := AValue;

  SetLength(FItems, AValue);
end;

procedure TList<T>.SetCount(AValue: Integer);
begin
  if AValue < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if AValue > Capacity then
    Capacity := AValue;
  if AValue < Count then
    DeleteRange(AValue, Count - AValue);

  FItemsLength := AValue;
end;

function TList<T>.GetItem(AIndex: Integer): T;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[AIndex];
end;

procedure TList<T>.SetItem(AIndex: Integer; const AValue: T);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  FItems[AIndex] := AValue;
end;

function TList<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TList<T>.DoGetEnumerator: Generics.Collections.fixed.TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TList<T>.Add(const AValue: T): Integer;
begin
  Result := PrepareAddingItem;
  FItems[Result] := AValue;
  Notify(AValue, cnAdded);
end;

procedure TList<T>.AddRange(const AValues: array of T);
begin
  InsertRange(Count, AValues);
end;

procedure TList<T>.AddRange(const AEnumerable: IEnumerable<T>);
var
  LValue: T;
begin
  for LValue in AEnumerable do
    Add(LValue);
end;

procedure TList<T>.AddRange(AEnumerable: TEnumerable<T>);
var
  LValue: T;
begin
  for LValue in AEnumerable do
    Add(LValue);
end;

procedure TList<T>.Insert(AIndex: Integer; const AValue: T);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if AIndex <> PrepareAddingItem then
  begin
    System.Move(FItems[AIndex], FItems[AIndex + 1], ((Count - AIndex) - 1) * SizeOf(T));
    FillChar(FItems[AIndex], SizeOf(T), 0);
  end;

  FItems[AIndex] := AValue;
  Notify(AValue, cnAdded);
end;

procedure TList<T>.InsertRange(AIndex: Integer; const AValues: array of T);
var
  i: integer;
  LLength: Integer;
  LValue: ^T;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LLength := Length(AValues);
  if LLength = 0 then
    Exit;

  if AIndex <> PrepareAddingRange(LLength) then
  begin
    System.Move(FItems[AIndex], FItems[AIndex + LLength], ((Count - AIndex) - LLength) * SizeOf(T));
    FillChar(FItems[AIndex], SizeOf(T) * LLength, 0);
  end;

  LValue := @AValues[0];
  for i := AIndex to Pred(AIndex + LLength) do
  begin
    FItems[i] := LValue^;
    Notify(LValue^, cnAdded);
    Inc(LValue);
  end;
end;

procedure TList<T>.InsertRange(AIndex: Integer; const AEnumerable: IEnumerable<T>);
var
  LValue: T;
  i:  integer;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable do
  begin
    Insert(Aindex + i, LValue);
    Inc(i);
  end;
end;

procedure TList<T>.InsertRange(AIndex: Integer; const AEnumerable: TEnumerable<T>);
var
  LValue: T;
  i:  integer;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable do
  begin
    Insert(Aindex + i, LValue);
    Inc(i);
  end;
end;

function TList<T>.Remove(const AValue: T): Integer;
begin
  Result := IndexOf(AValue);
  if Result >= 0 then
    DoRemove(Result, cnRemoved);
end;

procedure TList<T>.Delete(AIndex: Integer);
begin
  DoRemove(AIndex, cnRemoved);
end;

procedure TList<T>.DeleteRange(AIndex, ACount: Integer);
var
  LDeleted: array of T;
  i: Integer;
  LMoveDelta: Integer;
begin
  if ACount = 0 then
    Exit;

  if (ACount < 0) or (AIndex < 0) or (AIndex + ACount > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  SetLength(LDeleted, Count);
  System.Move(FItems[AIndex], LDeleted[0], ACount * SizeOf(T));

  LMoveDelta := Count - (AIndex + ACount);

  if LMoveDelta = 0 then
    FillChar(FItems[AIndex], ACount * SizeOf(T), #0)
  else
  begin
    System.Move(FItems[AIndex + ACount], FItems[AIndex], LMoveDelta * SizeOf(T));
    FillChar(FItems[Count - ACount], ACount * SizeOf(T), #0);
  end;

  FItemsLength -= ACount;

  for i := 0 to High(LDeleted) do
    Notify(LDeleted[i], cnRemoved);
end;

function TList<T>.Extract(const AIndex: Integer): T;
begin
  Result := DoRemove(AIndex, cnExtracted);
end;

function TList<T>.Extract(const AValue: T): T;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AValue);
  if LIndex < 0 then
    Exit(Default(T));

  Result := DoRemove(LIndex, cnExtracted);
end;

procedure TList<T>.Exchange(AIndex1, AIndex2: Integer);
var
  LTemp: T;
begin
  LTemp := FItems[AIndex1];
  FItems[AIndex1] := FItems[AIndex2];
  FItems[AIndex2] := LTemp;
end;

procedure TList<T>.Move(AIndex, ANewIndex: Integer);
var
  LTemp: T;
begin
  if ANewIndex = AIndex then
    Exit;

  if (ANewIndex < 0) or (ANewIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LTemp := FItems[AIndex];
  FItems[AIndex] := Default(T);

  if AIndex < ANewIndex then
    System.Move(FItems[Succ(AIndex)], FItems[AIndex], (ANewIndex - AIndex) * SizeOf(T))
  else
    System.Move(FItems[ANewIndex], FItems[Succ(ANewIndex)], (AIndex - ANewIndex) * SizeOf(T));

  FillChar(FItems[ANewIndex], SizeOf(T), #0);
  FItems[ANewIndex] := LTemp;
end;

function TList<T>.First: T;
begin
  Result := Items[0];
end;

function TList<T>.Last: T;
begin
  Result := Items[Pred(Count)];
end;

procedure TList<T>.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TList<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TList<T>.Contains(const AValue: T): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

function TList<T>.IndexOf(const AValue: T): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
{$IFDEF FAST_COMPARISON}
    if FComparer = nil then
    begin
      if AValue = FItems[i] then
        Exit(i);
    end
    else
{$ENDIF}
      if FComparer.Compare(AValue.GetReferenceToValue, FItems[i].GetReferenceToValue,
        AValue.GetValueSize, FItems[i].GetValueSize) = 0 then
        Exit(i);
  Result := -1;
end;

function TList<T>.LastIndexOf(const AValue: T): Integer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
{$IFDEF FAST_COMPARISON}
    if FComparer = nil then
    begin
      if AValue = FItems[i] then
        Exit(i);
    end
    else
{$ENDIF}
      if FComparer.Compare(AValue.GetReferenceToValue, FItems[i].GetReferenceToValue,
        AValue.GetValueSize, FItems[i].GetValueSize) = 0 then
        Exit(i);
  Result := -1;
end;

procedure TList<T>.Reverse;
var
  a, b: Integer;
  LTemp: T;
begin
  a := 0;
  b := Count - 1;
  while a < b do
  begin
    LTemp := FItems[a];
    FItems[a] := FItems[b];
    FItems[b] := LTemp;
    Inc(a);
    Dec(b);
  end;
end;

procedure TList<T>.Sort;
begin
  TArrayHelperBugHack.Sort(FItems, FComparer, 0, Count);
end;

procedure TList<T>.Sort(const AComparer: IComparer<T>);
begin
  TArrayHelperBugHack.Sort(FItems, FComparer, 0, Count);
end;

function TList<T>.BinarySearch(const AItem: T; out AIndex: Integer): Boolean;
begin
  Result := TArrayHelperBugHack.BinarySearch(FItems, AItem, AIndex);
end;

function TList<T>.BinarySearch(const AItem: T; out AIndex: Integer; const AComparer: IComparer<T>): Boolean;
begin
  Result := TArrayHelperBugHack.BinarySearch(FItems, AItem, AIndex, AComparer);
end;

{ TFastList<T> }

constructor TFastList<T>.Create;
begin
  FComparer := nil;
end;

function TFastList<T>.IndexOf(const AValue: T): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if FComparer = nil then
    begin
      if AValue = FItems[i] then
        Exit(i);
    end
    else
      if FComparer.Compare(AValue.GetReferenceToValue, FItems[i].GetReferenceToValue,
        AValue.GetValueSize, FItems[i].GetValueSize) = 0 then
        Exit(i);
  Result := -1;
end;

function TFastList<T>.LastIndexOf(const AValue: T): Integer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if FComparer = nil then
    begin
      if AValue = FItems[i] then
        Exit(i);
    end
    else
      if FComparer.Compare(AValue.GetReferenceToValue, FItems[i].GetReferenceToValue,
        AValue.GetValueSize, FItems[i].GetValueSize) = 0 then
        Exit(i);
  Result := -1;
end;

{ TQueue<T>.TEnumerator }

constructor TQueue<T>.TEnumerator.Create(AQueue: TQueue<T>);
begin
  inherited Create(AQueue);

  FIndex := Pred(AQueue.FLow);
end;

{ TQueue<T> }

function TQueue<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TQueue<T>.DoGetEnumerator: Generics.Collections.fixed.TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TQueue<T>.DoRemove(AIndex: Integer; ACollectionNotification: TCollectionNotification): T;
begin
  Result := FItems[AIndex];
  FItems[AIndex] := Default(T);
  Notify(Result, ACollectionNotification);
  FLow += 1;
  if FLow = FItemsLength then
  begin
    FLow := 0;
    FItemsLength := 0;
  end;
end;

procedure TQueue<T>.SetCapacity(AValue: Integer);
begin
  if AValue < Count then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if AValue = FItemsLength then
    Exit;

  if (Count > 0) and (FLow > 0) then
  begin
    Move(FItems[FLow], FItems[0], Count * SizeOf(T));
    FillChar(FItems[Count], (FItemsLength - Count) * SizeOf(T), #0);
  end;

  SetLength(FItems, AValue);
  FItemsLength := Count;
  FLow := 0;
end;

function TQueue<T>.GetCount: Integer;
begin
  Result := FItemsLength - FLow;
end;

constructor TQueue<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  for LItem in ACollection do
    Enqueue(LItem);
end;

destructor TQueue<T>.Destroy;
begin
  Clear;
end;

procedure TQueue<T>.Enqueue(const AValue: T);
var
  LIndex: Integer;
begin
  LIndex := PrepareAddingItem;
  FItems[LIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TQueue<T>.Dequeue: T;
begin
  Result := DoRemove(FLow, cnRemoved);
end;

function TQueue<T>.Extract: T;
begin
  Result := DoRemove(FLow, cnExtracted);
end;

function TQueue<T>.Peek: T;
begin
  if (Count = 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[FLow];
end;

procedure TQueue<T>.Clear;
begin
  while Count <> 0 do
    Dequeue;
  FLow := 0;
  FItemsLength := 0;
end;

procedure TQueue<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

{ TStack<T> }

function TStack<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TStack<T>.DoGetEnumerator: Generics.Collections.fixed.TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

constructor TStack<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  for LItem in ACollection do
    Push(LItem);
end;

function TStack<T>.DoRemove(AIndex: Integer; ACollectionNotification: TCollectionNotification): T;
begin
  if AIndex < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[AIndex];
  FItems[AIndex] := Default(T);
  FItemsLength -= 1;
  Notify(Result, ACollectionNotification);
end;

destructor TStack<T>.Destroy;
begin
  Clear;
end;

procedure TStack<T>.Clear;
begin
  while Count <> 0 do
    Pop;
end;

procedure TStack<T>.SetCapacity(AValue: Integer);
begin
  if AValue < Count then
    AValue := Count;

  SetLength(FItems, AValue);
end;

procedure TStack<T>.Push(const AValue: T);
var
  LIndex: Integer;
begin
  LIndex := PrepareAddingItem;
  FItems[LIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TStack<T>.Pop: T;
begin
  Result := DoRemove(FItemsLength - 1, cnRemoved);
end;

function TStack<T>.Peek: T;
begin
  if (Count = 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[FItemsLength - 1];
end;

function TStack<T>.Extract: T;
begin
  Result := DoRemove(FItemsLength - 1, cnExtracted);
end;

procedure TStack<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

{ TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS> }

procedure TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.PairNotify(const APair: TPair<TKey, TValue>; ACollectionNotification: TCollectionNotification);
begin
  KeyNotify(APair.Key, ACollectionNotification);
  ValueNotify(APair.Value, ACollectionNotification);
end;

procedure TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.KeyNotify(const AKey: TKey; ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self, AKey, ACollectionNotification);
end;

procedure TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.SetValue(var AValue: TValue; const ANewValue: TValue);
var
  LOldValue: TValue;
begin
  LOldValue := AValue;
  AValue := ANewValue;

  ValueNotify(LOldValue, cnRemoved);
  ValueNotify(ANewValue, cnAdded);
end;

procedure TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.ValueNotify(const AValue: TValue; ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, AValue, ACollectionNotification);
end;

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(ACapacity: Integer); overload;
begin
{$IFNDEF FAST_COMPARISON}
  FEqualityComparer := TEqualityKeyComparerBugHack.Create; //() as IEqualityComparer;
{$ENDIF}
  SetCapacity(ACapacity);
end;

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(ACapacity: Integer; const AComparer: IEqualityComparer<TKey>);
begin
  FEqualityComparer := AComparer;
  SetCapacity(ACapacity);
end;

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(const AComparer: IEqualityComparer<TKey>);
begin
  Create(0, AComparer);
end;

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(ACollection: TEnumerable<TDictionaryPair>);
var
  LItem: TPair<TKey, TValue>;
begin
  Create;
  for LItem in ACollection do
    Add(LItem);
end;

constructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Create(ACollection: TEnumerable<TDictionaryPair>; const AComparer: IEqualityComparer<TKey>); overload;
var
  LItem: TPair<TKey, TValue>;
begin
  Create(AComparer);
  for LItem in ACollection do
    Add(LItem);
end;

destructor TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.Destroy;
begin
  Clear;
  FKeys.Free;
  FValues.Free;
  inherited;
end;

function TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.ToArray(ACount: Integer): TArray<TDictionaryPair>;
var
  i: Integer;
  LEnumerator: TEnumerator<TDictionaryPair>;
begin
  SetLength(Result, ACount);
  LEnumerator := DoGetEnumerator;

  i := 0;
  while LEnumerator.MoveNext do
  begin
    Result[i] := LEnumerator.Current;
    Inc(i);
  end;
  LEnumerator.Free;
end;

function TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>.ToArray: TArray<TDictionaryPair>;
begin
  Result := ToArray(Count);
end;

{$REGION 'Enumeratory'}

{$ENDREGION}

{ TPair<TKey,TValue> }

class function TPair<TKey, TValue>.Create(AKey: TKey;
  AValue: TValue): TPair<TKey, TValue>;
begin
  Result.Key := AKey;
  Result.Value := AValue;
end;

{ TEnumerator<T> }

function TEnumerator<T>.MoveNext: boolean;
begin
  Exit(DoMoveNext);
end;

{ TEnumerable<T> }

//function TEnumerable<T>.ToArrayImpl(Count: Integer): TArray;
//begin

//end;

function TEnumerable<T>.ToArrayImpl(ACount: Integer): TArray<T>;
var
  i: Integer;
  LEnumerator: TEnumerator<T>;
begin
  SetLength(Result, ACount);
  LEnumerator := GetEnumerator;

  i := 0;
  while LEnumerator.MoveNext do
  begin
    Result[i] := LEnumerator.Current;
    Inc(i);
  end;
  LEnumerator.Free;
end;

function TEnumerable<T>.GetEnumerator: TEnumerator;
begin
  Exit(DoGetEnumerator);
end;

function TEnumerable<T>.ToArray: TArray<T>;
begin
  Result := nil;
end;

//function TEnumerable<T>.ToArray: TArray;
//begin
//
//end;

{ TDictionaryEnumerator<THash,TIndex,TValue,TKey,T> }

constructor TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS>.Create(
  ADictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>);
begin
  inherited Create;
  FIndex := -1;
  FDictionary := ADictionary;
end;

function TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS>.DoGetCurrent: TPair<TKey, TValue>;
begin
  Result := GetCurrent;
end;

{ TDictionaryEnumerator }

function TDictionaryListEnumerator<T, DICTIONARY_LIST_CONSTRAINTS>.DoMoveNext: Boolean;
begin
  Inc(FIndex);
  Result := (FDictionary.FItemsLength <> 0)
    and (FIndex < FDictionary.FItemsLength)
end;

{ TDictionary.TKeyEnumerator<TKey,TValue> }

function TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>.TKeyEnumerator.GetCurrent: TKey;
begin
  //Result := TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair.Key;
  Result := TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>(FDictionary).FPairItems[FIndex].Key;
end;

{ TDictionary.TPairEnumerator<TKey,TValue> }

function TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>.TPairEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  //Result := TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair;
  Result := TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>(FDictionary).FPairItems[FIndex];
end;

{ TDictionary.TValueEnumerator<TKey,TValue> }

function TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>.TValueEnumerator.GetCurrent: TValue;
begin
  //Result := TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair.Value;
  Result := TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>(FDictionary).FPairItems[FIndex].Value;
end;

{ TDictionaryEnumerable<THash,TIndex,TValue,TKey,T,TE> }

constructor TDictionaryEnumerable<TDictionaryEnumerator, T, CUSTOM_DICTIONARY_CONSTRAINTS>.Create(
  ADictionary: TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>);
begin
  FDictionary := ADictionary;
end;

function TDictionaryEnumerable<TDictionaryEnumerator, T, CUSTOM_DICTIONARY_CONSTRAINTS>.DoGetEnumerator: TDictionaryEnumerator;
begin
  Result := TDictionaryEnumerator(TDictionaryEnumerator.NewInstance);
  TCustomDictionaryEnumerator<T, CUSTOM_DICTIONARY_CONSTRAINTS>(Result).Create(FDictionary);

  //if TE.ClassType = (TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>).TKeyEnumerator.ClassType then
  //  Result := TE(TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>.TKeyEnumerator.Create(FDictionary))
  //else if TE.ClassType = (TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>).TValueEnumerator.ClassType then
  //  Result := TE(TDictionaryList<CUSTOM_DICTIONARY_CONSTRAINTS>.TValueEnumerator.Create(FDictionary))
  //else
  //  Assert(False);
end;

function TDictionaryEnumerable<TDictionaryEnumerator, T, CUSTOM_DICTIONARY_CONSTRAINTS>.GetCount: Integer;
begin
  Result := TCustomDictionary<CUSTOM_DICTIONARY_CONSTRAINTS>(FDictionary).Count;
end;

function TDictionaryEnumerable<TDictionaryEnumerator, T, CUSTOM_DICTIONARY_CONSTRAINTS>.ToArray: TArray;
begin
  Result := inherited ToArray;
end;

{ TDictionary<TProbeSequence,THash,TIndex,TEqualityComparer,TValue,TKey> }

function TDictionary<DICTIONARY_CONSTRAINTS>.GetKeys: TKeyCollection;
begin
  if not Assigned(FKeys) then
    FKeys := TKeyCollection.Create(Self);
  Result := TKeyCollection(FKeys);
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.GetValues: TValueCollection;
begin
  if not Assigned(FValues) then
    FValues := TValueCollection.Create(Self);
  Result := TValueCollection(FValues);
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.FindBucketIndex(const AKey: TKey): Integer;
var
  LHash: THash;
begin
  Result := FindBucketIndex(FItems, AKey, LHash);
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.FindBucketIndex(const AItems: TArray<TItem>; const AKey: TKey;
  out AHash: THash): Integer;
var
  LItem: TItem;
  LLengthMask: Integer;
  i, m: integer;
  LProbeSequence: TProbeSequence;
begin
  LLengthMask := Length(AItems) - 1;

{$IFDEF FAST_COMPARISON}
  if FEqualityComparer = nil then
    // bug - should be (http://bugs.freepascal.org/view.php?id=24282)
    // LProbeSequence := TProbeSequence.Create<TKey, THashFactory>(AKey)
    LProbeSequence := TProbeSequence.Create(THashFactory, AKey.GetReferenceToValue, AKey.GetValueSize)
  else
{$ENDIF}
    LProbeSequence := TProbeSequence.Create(FEqualityComparer, AKey.GetReferenceToValue, AKey.GetValueSize);

  i := 0;
  m := Length(AItems);
  Result := LProbeSequence.Probe(i, m) and LLengthMask;

  AHash := LProbeSequence.Hash or THash.GetSignMask;

  repeat
    LItem := AItems[Result];
    // Empty position
    if (LItem.Hash and THash.GetSignMask) = 0 then
      Exit(not Result); // insert!

    // Same position?
    if LItem.Hash = AHash then
{$IFDEF FAST_COMPARISON}
      if FEqualityComparer = nil then
      begin
        if AKey = LItem.Pair.Key then
          Exit;
      end
      else
{$ENDIF}
        if FEqualityComparer.Equals(AKey.GetReferenceToValue, LItem.Pair.Key.GetReferenceToValue, AKey.GetValueSize, LItem.Pair.Key.GetValueSize) then
          Exit;
        //if (AKey.GetValueSize = LItem.Pair.Key.GetValueSize) then
        //  if CompareMem(AKey.GetReferenceToValue, LItem.Pair.Key.GetReferenceToValue, AKey.GetValueSize) then
        //    Exit;

    Inc(i);
    Result := LProbeSequence.Probe(i, m) and LLengthMask;
  until false;
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.PrepareAddingItem: integer;
begin
  if FItemsLength > FItemsThreshold then
    Rehash(Length(FItems) shl 1)
  else if FItemsLength = 0 then
  begin
    SetLength(FItems, 4);
    UpdateItemsThreshold;
  end
  else if FItemsLength = $40000001 then // High(TIndex) ... System.Generics.Collections.fixed.pas(292,26) Error: Type mismatch
    OutOfMemoryError;

  Result := FItemsLength;
  Inc(FItemsLength);
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.UpdateItemsThreshold;
var
  LLength: Integer;
begin
  LLength := Length(FItems);
  if LLength = $40000000 then
    FItemsThreshold := $40000001
  else
    FItemsThreshold := (LLength shr 1) or (LLength shr 2); // threshold for table expansion is 0.75
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.AddItem(var AItem: TItem; const AKey: TKey; const AValue: TValue; const AHash: THash);
begin
  AItem.Hash := AHash;
  AItem.Pair.Key := AKey;
  AItem.Pair.Value := AValue;

  PairNotify(AItem.Pair, cnAdded);
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.Add(const AKey: TKey; const AValue: TValue);
var
  LHash: THash;
  LIndex: Integer;
begin
  PrepareAddingItem;

  LIndex := FindBucketIndex(FItems, AKey, LHash);
  if LIndex >= 0 then
    raise EListError.CreateRes(@SDuplicatesNotAllowed);

  AddItem(FItems[not LIndex], AKey, AValue, LHash);
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.Add(const APair: TPair<TKey, TValue>);
begin
  Add(APair.Key, APair.Value);
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.DoRemove(AIndex: Integer;
  ACollectionNotification: TCollectionNotification): TValue;
var
  LItem: PItem;
begin
  LItem := @FItems[AIndex];
  LItem.Hash := 0;
  Result := LItem.Pair.Value;
  PairNotify(LItem.Pair, ACollectionNotification);
  LItem.Pair := Default(TPair<TKey, TValue>);

  Dec(FItemsLength);
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.Remove(const AKey: TKey);
var
  LIndex: Integer;
begin
  LIndex := FindBucketIndex(AKey);
  if LIndex  < 0 then
    Exit;

  DoRemove(LIndex, cnRemoved);
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.ExtractPair(const AKey: TKey): TPair<TKey, TValue>;
var
  LIndex: Integer;
begin
  LIndex := FindBucketIndex(AKey);
  if LIndex  < 0 then
    Exit(Default(TPair<TKey, TValue>));

  Result.Key := AKey;
  Result.Value := DoRemove(LIndex, cnExtracted);
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.Clear;
var
  LItem: PItem;
  i: Integer;
  LOldItems: array of TItem;
begin
  FItemsLength := 0;
  FItemsThreshold := 0;
  LOldItems := FItems;
  FItems := nil;

  for i := 0 to High(LOldItems) do
  begin
    LItem := @LOldItems[i];
    if (LItem.Hash and THash.GetSignMask = 0) then
      Continue;

    PairNotify(LItem.Pair, cnRemoved);
  end;
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.Rehash(ASizePow2: Integer);
var
  LNewItems: TArray<TItem>;
  LHash: THash;
  LIndex: Integer;
  i: Integer;
  LItem, LNewItem: PItem;
begin
  if ASizePow2 = Length(FItems) then
    Exit;
  if ASizePow2 < 0 then
    OutOfMemoryError;

  SetLength(LNewItems, ASizePow2);
  for i := 0 to High(FItems) do
  begin
    LItem := @FItems[i];
    if (LItem.Hash and THash.GetSignMask) <> 0 then
    begin
      LIndex := not FindBucketIndex(LNewItems, LItem.Pair.Key, LHash);

      LNewItem := @LNewItems[LIndex];
      LNewItem.Hash := LHash;
      LNewItem.Pair := LItem.Pair;
    end;
  end;

  FItems := LNewItems;
  UpdateItemsThreshold;
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.DoGetEnumerator: TEnumerator<TDictionaryPair>;
begin
  Result := GetEnumerator;
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.SetCapacity(ACapacity: Integer);
begin
  if ACapacity < FItemsLength then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Resize(ACapacity);
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.Resize(ANewSize: Integer);
var
  LNewSize: Integer;
begin
  if ANewSize < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LNewSize := 0;
  if ANewSize > 0 then
  begin
    LNewSize := 4;
    while LNewSize <= ANewSize do
      LNewSize := LNewSize shl 1;
  end;

  Rehash(LNewSize);
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.GetItem(const AKey: TKey): TValue;
var
  LIndex: Integer;
begin
  LIndex := FindBucketIndex(AKey);
  if LIndex < 0 then
    raise EListError.CreateRes(@SDictionaryKeyDoesNotExist);
  Result := FItems[LIndex].Pair.Value;
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.TrimExcess;
begin
  SetCapacity(Succ(FItemsLength));
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.SetItem(const AKey: TKey; const AValue: TValue);
var
  LIndex: Integer;
begin
  LIndex := FindBucketIndex(AKey);
  if LIndex < 0 then
    raise EListError.CreateRes(@SItemNotFound);

  SetValue(FItems[LIndex].Pair.Value, AValue);
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
var
  LIndex: Integer;
begin
  LIndex := FindBucketIndex(AKey);
  Result := LIndex >= 0;

  if Result then
    AValue := FItems[LIndex].Pair.Value
  else
    AValue := Default(TValue);
end;

procedure TDictionary<DICTIONARY_CONSTRAINTS>.AddOrSetValue(const AKey: TKey; const AValue: TValue);
var
  LIndex: Integer;
  LHash: THash;
begin
  LIndex := FindBucketIndex(FItems, AKey, LHash);

  if LIndex < 0 then
    AddItem(FItems[not LIndex], AKey, AValue, LHash)
  else
    SetValue(FItems[LIndex].Pair.Value, AValue);
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.ContainsKey(const AKey: TKey): Boolean;
var
  LIndex: Integer;
begin
  LIndex := FindBucketIndex(AKey);
  Result := LIndex >= 0;
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.ContainsValue(const AValue: TValue): Boolean;
begin
  Result := ContainsValue(AValue, TEqualityValueComparerBugHack.Create);
end;

function TDictionary<DICTIONARY_CONSTRAINTS>.ContainsValue(const AValue: TValue;
  const AEqualityComparer: IEqualityComparer<TValue>): Boolean;
var
  i: Integer;
  LItem: PItem;
begin
  if Length(FItems) = 0 then
    Exit(False);

  for i := 0 to High(FItems) do
  begin
    LItem := @FItems[i];
    if (LItem.Hash and THash.GetSignMask) = 0 then
      Continue;

{$IFDEF FAST_COMPARISON}
    // if AKey == LItem.Pair.Key then or if TryExecuteEqualOperator(AKey == LItem.Pair.Key) then
    if AEqualityComparer = nil then
    begin
      if AValue = LItem.Pair.Value then
        Exit(True);
    end
    else
{$ENDIF}
      if AEqualityComparer.Equals(AValue.GetReferenceToValue, LItem.Pair.Value.GetReferenceToValue, AValue.GetValueSize, LItem.Pair.Value.GetValueSize) then
        Exit;
    //if AValue = LItem.Pair.Value then
    //  Exit(True);
    //if (AValue.GetValueSize = LItem.Pair.Value.GetValueSize) then
    //  if CompareMem(AValue.GetReferenceToValue, LItem.Pair.Value.GetReferenceToValue, AValue.GetValueSize) then
    //    Exit(True);
  end;
  Result := False;
end;

{ TDictionaryEnumerator<T, DICTIONARY_CONSTRAINTS> }

function TDictionaryEnumerator<T, DICTIONARY_CONSTRAINTS>.DoMoveNext: Boolean;
var
  LLength: Integer;
begin
  Inc(FIndex);

  LLength := Length(TDictionary<DICTIONARY_CONSTRAINTS>(FDictionary).FItems);

  if FIndex >= LLength then
    Exit(False);

  // is this bug related to (http://bugs.freepascal.org/view.php?id=24098) ?
  // compiler error for (TDictionary<DICTIONARY_CONSTRAINTS>(FDictionary).FItems[FIndex].Hash and THash.GetSignMask) = 0
  while ((TDictionary<DICTIONARY_CONSTRAINTS>(FDictionary).FItems[FIndex].Hash) and THash.GetSignMask) = 0 do
  begin
    Inc(FIndex);
    if FIndex = LLength then
      Exit(False);
  end;

  Result := True;
end;

{ TDictionary<DICTIONARY_CONSTRAINTS>.TPairEnumerator }

function TDictionary<DICTIONARY_CONSTRAINTS>.TPairEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  Result := TDictionary<DICTIONARY_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair;
end;

{ TDictionary<DICTIONARY_CONSTRAINTS>.TValueEnumerator }

function TDictionary<DICTIONARY_CONSTRAINTS>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := TDictionary<DICTIONARY_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair.Value;
end;

{ TDictionary<DICTIONARY_CONSTRAINTS>.TKeyEnumerator }

function TDictionary<DICTIONARY_CONSTRAINTS>.TKeyEnumerator.GetCurrent: TKey;
begin
  Result := TDictionary<DICTIONARY_CONSTRAINTS>(FDictionary).FItems[FIndex].Pair.Key;
end;

{ TFastDictionary<DICTIONARY_CONSTRAINTS> }

constructor TFastDictionary<DICTIONARY_CONSTRAINTS>.Create(ACapacity: Integer);
begin
  FEqualityComparer := nil;
  SetCapacity(ACapacity);
end;

function TFastDictionary<DICTIONARY_CONSTRAINTS>.FindBucketIndex(const AItems: TArray<TItem>; const AKey: TKey;
  out AHash: THash): Integer;
var
  LItem: TItem;
  LLengthMask: Integer;
  i, m: integer;
  LProbeSequence: TProbeSequence;
begin
  LLengthMask := Length(AItems) - 1;

  if FEqualityComparer = nil then
    // bug - should be (http://bugs.freepascal.org/view.php?id=24282)
    // LProbeSequence := TProbeSequence.Create<TKey, THashFactory>(AKey)
    LProbeSequence := TProbeSequence.Create(THashFactory, AKey.GetReferenceToValue, AKey.GetValueSize)
  else
    LProbeSequence := TProbeSequence.Create(FEqualityComparer, AKey.GetReferenceToValue, AKey.GetValueSize);

  i := 0;
  m := Length(AItems);
  Result := LProbeSequence.Probe(i, m) and LLengthMask;

  AHash := LProbeSequence.Hash or THash.GetSignMask;

  repeat
    LItem := AItems[Result];
    // Empty position
    if (LItem.Hash and THash.GetSignMask) = 0 then
      Exit(not Result); // insert!

    // Same position?
    if LItem.Hash = AHash then
      if FEqualityComparer = nil then
      begin
        if AKey = LItem.Pair.Key then
          Exit;
      end
      else
        if FEqualityComparer.Equals(AKey.GetReferenceToValue, LItem.Pair.Key.GetReferenceToValue, AKey.GetValueSize, LItem.Pair.Key.GetValueSize) then
          Exit;
        //if (AKey.GetValueSize = LItem.Pair.Key.GetValueSize) then
        //  if CompareMem(AKey.GetReferenceToValue, LItem.Pair.Key.GetReferenceToValue, AKey.GetValueSize) then
        //    Exit;

    Inc(i);
    Result := LProbeSequence.Probe(i, m) and LLengthMask;
  until false;
end;

function TFastDictionary<DICTIONARY_CONSTRAINTS>.ContainsValue(const AValue: TValue;
  const AEqualityComparer: IEqualityComparer<TValue>): Boolean;
var
  i: Integer;
  LItem: PItem;
begin
  if Length(FItems) = 0 then
    Exit(False);

  for i := 0 to High(FItems) do
  begin
    LItem := @FItems[i];
    if (LItem.Hash and THash.GetSignMask) = 0 then
      Continue;

    // if AKey == LItem.Pair.Key then or if TryExecuteEqualOperator(AKey == LItem.Pair.Key) then
    if AEqualityComparer = nil then
    begin
      if AValue = LItem.Pair.Value then
        Exit(True);
    end
    else
      if AEqualityComparer.Equals(AValue.GetReferenceToValue, LItem.Pair.Value.GetReferenceToValue, AValue.GetValueSize, LItem.Pair.Value.GetValueSize) then
        Exit;
    //if AValue = LItem.Pair.Value then
    //  Exit(True);
    //if (AValue.GetValueSize = LItem.Pair.Value.GetValueSize) then
    //  if CompareMem(AValue.GetReferenceToValue, LItem.Pair.Value.GetReferenceToValue, AValue.GetValueSize) then
    //    Exit(True);
  end;
  Result := False;
end;

{ TDictionaryList<TValue,TKey> }

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetPairItem(const AIndex: TIndex): TPair<TKey, TValue>;
begin
  if (AIndex < 0) or  (AIndex >= FItemsLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FPairItems[AIndex];
end;

// need optimalization... this funcion don't really need DoRemove and Insert (can be more efficient)
procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.SetPairItem(const AIndex: TIndex; const APair: TPair<TKey, TValue>);
var
  LIndex: Integer;
begin
  if (AIndex < 0) or  (AIndex >= FItemsLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LIndex := GetItemIndex(APair.Key);
  if LIndex <> AIndex then
  begin
    if (LIndex >= 0) then
      raise EListError.CreateRes(@SDuplicatesNotAllowed);

    // new key and value...
    DoRemove(AIndex, cnRemoved);
    Insert(AIndex, APair.Key, APair.Value);
  end
  else
    SetValue(FPairItems[AIndex].Value, APair.Value);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetKeyItem(const AIndex: TIndex): TKey;
begin
  if (AIndex < 0) or  (AIndex >= FItemsLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FPairItems[AIndex].Key;
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.SetKeyItem(const AIndex: TIndex; const AKey: TKey);
var
  LIndex: Integer;
  LValue: TValue;
begin
  if (AIndex < 0) or  (AIndex >= FItemsLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LIndex := GetItemIndex(AKey);
  if LIndex <> AIndex then
  begin
    if (LIndex >= 0) then
      raise EListError.CreateRes(@SDuplicatesNotAllowed);

    LValue := FPairItems[AIndex].Value;
    DoRemove(AIndex, cnRemoved);
    Insert(AIndex, AKey, LValue);
  end;
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetValueItem(const AIndex: TIndex): TValue;
begin
  if (AIndex < 0) or  (AIndex >= FItemsLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FPairItems[AIndex].Value;
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.SetValueItem(const AIndex: TIndex; const AValue: TValue);
begin
  if (AIndex < 0) or  (AIndex >= FItemsLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  SetValue(FPairItems[AIndex].Value, AValue);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetItem(const AKey: TKey): TValue;
var
  LIndex: Integer;
begin
  LIndex := GetItemIndex(AKey);
  if LIndex < 0 then
    raise EListError.CreateRes(@SDictionaryKeyDoesNotExist);

  Result := FPairItems[FIndexItems[LIndex].Index].Value;
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.SetItem(const AKey: TKey;
  const AValue: TValue);
var
  LIndex: Integer;
begin
  LIndex := GetItemIndex(AKey);
  if LIndex < 0 then
    raise EListError.CreateRes(@SItemNotFound);

  SetValue(FPairItems[FIndexItems[LIndex].Index].Value, AValue);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetItemIndex(const AKey: TKey): Integer;
var
  LHash: THash;
begin
  Result := GetItemIndex(AKey, LHash);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetItemIndex(const AKey: TKey; out AHash: THash): Integer;
var
  imin, imax, imid: Int32;
  i: Int32;
  LKey: TKey;
begin
    // continually narrow search until just one element remains
  imin := 0;
  imax := FItemsLength - 1;
{$IFDEF FAST_COMPARISON}
  if FEqualityComparer = nil then
    AHash := THashFactory.Hash(AKey.GetReferenceToValue, AKey.GetValueSize)
  else
{$ENDIF}
    AHash := FEqualityComparer.GetHashCode(AKey.GetReferenceToValue, AKey.GetValueSize);
  // http://en.wikipedia.org/wiki/Binary_search_algorithm
  while (imin < imax) do
  begin
        imid := imin + ((imax - imin) div 2);

        // code must guarantee the interval is reduced at each iteration
        // assert(imid < imax);
        // note: 0 <= imin < imax implies imid will always be less than imax

        // reduce the search
        if (FIndexItems[imid].Hash < AHash) then
          imin := imid + 1
        else
          imax := imid;
  end;
    // At exit of while:
    //   if A[] is empty, then imax < imin
    //   otherwise imax == imin

    // deferred test for equality

    if (imax = imin) and (FIndexItems[imin].Hash = AHash) then
    begin
      LKey := FPairItems[FIndexItems[imin].Index].Key;
{$IFDEF FAST_COMPARISON}
      if FEqualityComparer = nil then
      begin
        if AKey = LKey then
          Exit(imin);
      end
      else
{$ENDIF}
        if FEqualityComparer.Equals(AKey.GetReferenceToValue, LKey.GetReferenceToValue, AKey.GetValueSize, LKey.GetValueSize) then
          Exit(imin);
      //if (AKey.GetValueSize = LKey.GetValueSize) then
      //  if CompareMem(AKey.GetReferenceToValue, LKey.GetReferenceToValue, LKey.GetValueSize) then
      //    Exit(imin);

      // sprawdź lewą stronę hashy
      for i := imin - 1 downto 0 do
        if (FIndexItems[i].Hash <> AHash) then
          Break
        else
        begin
          LKey := FPairItems[FIndexItems[i].Index].Key;
{$IFDEF FAST_COMPARISON}
          if FEqualityComparer = nil then
          begin
            if AKey = LKey then
              Exit(i);
          end
          else
{$ENDIF}
            if FEqualityComparer.Equals(AKey.GetReferenceToValue, LKey.GetReferenceToValue, AKey.GetValueSize, LKey.GetValueSize) then
              Exit(i);
          //if (AKey.GetValueSize = LKey.GetValueSize) then
          //  if CompareMem(AKey.GetReferenceToValue, LKey.GetReferenceToValue, LKey.GetValueSize) then
          //    Exit(i);
        end;
      // sprawdź prawą stronę hashy
      for i := imin + 1 to FItemsLength - 1 do
        if (FIndexItems[i].Hash <> AHash) then
          Exit(-1)
        else
        begin
          LKey := FPairItems[FIndexItems[i].Index].Key;
{$IFDEF FAST_COMPARISON}
          if FEqualityComparer = nil then
          begin
            if AKey = LKey then
              Exit(i);
          end
          else
{$ENDIF}
            if FEqualityComparer.Equals(AKey.GetReferenceToValue, LKey.GetReferenceToValue, AKey.GetValueSize, LKey.GetValueSize) then
              Exit(i);
          //if (AKey.GetValueSize = LKey.GetValueSize) then
          //  if CompareMem(AKey.GetReferenceToValue, LKey.GetReferenceToValue, LKey.GetValueSize) then
          //    Exit(i);
        end;
    end
    else
      Exit(-1);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.PrepareAddingItem: integer;
begin
  Result := Length(FPairItems);

  if (FItemsLength < 4) and (Result < 4) then
  begin
    SetLength(FPairItems, 4);
    SetLength(FIndexItems, 4);
  end
  else if FItemsLength = TIndex.High then // High ... System.Generics.Collections.fixed.pas(292,26) Error: Type mismatch
    OutOfMemoryError
  else if FItemsLength = Result then
  begin
    SetLength(FPairItems, DICTIONARYLIST_CAPACITY_INC);
    SetLength(FIndexItems, DICTIONARYLIST_CAPACITY_INC);
  end;

  Result := FItemsLength;
  Inc(FItemsLength);
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.InsertItem(AIndex: integer);
begin
  System.Move(FIndexItems[AIndex], FIndexItems[AIndex + 1], Pred(FItemsLength - AIndex) * SizeOf(TItemIndex));
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetKeys: TKeyCollection;
begin
  if not Assigned(FKeys) then
    FKeys := TKeyCollection.Create(Self);
  Exit(TKeyCollection(FKeys));
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetValues: TValueCollection;
begin
  if not Assigned(FValues) then
    FValues := TValueCollection.Create(Self);
  Exit(TValueCollection(FValues));
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := GetItemIndex(AKey) >= 0;
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.ContainsValue(const AValue: TValue): Boolean;
begin
  Result := ContainsValue(AValue, TEqualityValueComparerBugHack.Create);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.ContainsValue(const AValue: TValue;
  const AEqualityComparer: IEqualityComparer<TValue>): Boolean;
var
  i: Integer;
  LItem: ^TPair<TKey, TValue>;
begin
  if FItemsLength = 0 then
    Exit(False);

  for i := 0 to FItemsLength - 1 do
  begin
    LItem := @FPairItems[i];

{$IFDEF FAST_COMPARISON}
    if AEqualityComparer = nil then
    begin
      if AValue = LItem.Value then
        Exit(True);
    end
    else
{$ENDIF}
      if AEqualityComparer.Equals(AValue.GetReferenceToValue, LItem.Value.GetReferenceToValue,
        AValue.GetValueSize, LItem.Value.GetValueSize) then
        Exit;
  end;
  Result := False;
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.DoGetEnumerator: TEnumerator<TDictionaryPair>;
begin
  Result := GetEnumerator;
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.SetCapacity(ACapacity: Integer);
begin
  if ACapacity < FItemsLength then
    ACapacity := FItemsLength;

  SetLength(FPairItems, ACapacity);
  SetLength(FIndexItems, ACapacity);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.DoRemove(AIndex: Integer;
  ACollectionNotification: TCollectionNotification): TPair<TKey, TValue>;
var
  i: Integer;
  LItem: PItemIndex;
  LIndex: Integer;
begin
  // change bigger indexes ... :(
  LItem := @FIndexItems[0];
  for i := 0 to FItemsLength - 1 do
  begin
    if LItem.Index > AIndex then
      Dec(LItem.Index)
    else
    if LItem.Index = AIndex then
      LIndex := i;
    Inc(LItem);
  end;

  Result := FPairItems[AIndex];
  FPairItems[AIndex] := Default(TPair<TKey, TValue>);
  System.Move(FPairItems[AIndex + 1], FPairItems[AIndex], Pred(FItemsLength - AIndex) * SizeOf(TPair<TKey, TValue>));
  System.Move(FIndexItems[LIndex + 1], FIndexItems[LIndex], Pred(FItemsLength - LIndex) * SizeOf(TItemIndex));
  Dec(FItemsLength);
  PairNotify(Result, ACollectionNotification);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.Add(const AKey: TKey; const AValue: TValue): Integer;
var
  LHash: THash;
begin
  if GetItemIndex(AKey, LHash) >= 0 then
    raise EListError.CreateRes(@SDuplicatesNotAllowed);

  Result := Add(AKey, AValue, LHash);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetHash(const AKey: TKey): THash;
begin
{$IFDEF FAST_COMPARISON}
  if FEqualityComparer = nil then
    Result := THashFactory.Hash(AKey.GetReferenceToValue, AKey.GetValueSize)
  else
{$ENDIF}
    Result := FEqualityComparer.GetHashCode(AKey.GetReferenceToValue, AKey.GetValueSize);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.AddIndex(AHash: THash): Integer;
var
  LInsertIdx: TIndex;
  i: Int32;
  LNewIdx: Boolean;
  LNewIndexRec: TItemIndex;
begin
  LNewIdx := True;

  for i := 0 to FItemsLength - 1 do
  begin
    if AHash < FIndexItems[i].Hash then
    begin
      LInsertIdx := i;
      LNewIdx := False;
      Break;
    end;
  end;

  Result := PrepareAddingItem;

  LNewIndexRec.Hash := AHash;
  LNewIndexRec.Index := Result;

  // candidate to insert, or new index...
  // greater ... - remember about golden ratio
  if LNewIdx then
    FIndexItems[Result] := LNewIndexRec
  else
  begin
    InsertItem(LInsertIdx);
    FIndexItems[LInsertIdx] := LNewIndexRec
  end;
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.Add(const AKey: TKey; const AValue: TValue; AHash: THash): Integer;
var
  LNewPairRec: TPair<TKey, TValue>;
begin
  Result := AddIndex(AHash);

  LNewPairRec.Key := AKey;
  LNewPairRec.Value := AValue;
  FPairItems[Result] := LNewPairRec;

  PairNotify(LNewPairRec, cnAdded);
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.Add(const APair: TPair<TKey, TValue>);
begin
  Add(APair.Key, APair.Value);
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.Remove(const AKey: TKey);
var
  LIndex: Integer;
begin
  LIndex := GetItemIndex(AKey);
  if LIndex < 0 then
    Exit;

  DoRemove(LIndex, cnRemoved);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.ExtractPair(const AKey: TKey): TPair<TKey,TValue>;
var
  LIndex: Integer;
begin
  LIndex := GetItemIndex(AKey);
  if LIndex < 0 then
    Exit(Default(TPair<TKey, TValue>));

  Result := DoRemove(LIndex, cnExtracted);
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.Clear;
var
  i: Integer;
  LOldPairItems: array of TPair<TKey, TValue>;
begin
  LOldPairItems := FPairItems;
  FIndexItems := nil;
  FPairItems := nil;
  FItemsLength := 0;

  for i := 0 to High(LOldPairItems) do
    PairNotify(LOldPairItems[i], cnRemoved);
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
var
  LIndex: Integer;
begin
  LIndex := GetItemIndex(AKey);
  Result := LIndex >= 0;

  if Result then
    AValue := FPairItems[FIndexItems[LIndex].Index].Value
  else
    AValue := Default(TValue);
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.AddOrSetValue(const AKey: TKey; const AValue: TValue);
var
  LIndex: Integer;
  LHash: THash;
begin
  LIndex := GetItemIndex(AKey, LHash);

  if LIndex < 0 then
    Add(AKey, AValue, LHash)
  else
    SetValue(FPairItems[FIndexItems[LIndex].Index].Value, AValue);
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.Insert(AIndex: Integer; const AKey: TKey; const AValue: TValue);
var
  LHash: THash;
  LIndex: Integer;
  i: Integer;
begin
  if AIndex = Count then
  begin
    Add(AKey, AValue);
    Exit;
  end;

  if AIndex < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if GetItemIndex(AKey, LHash) >= 0 then
    raise EListError.CreateRes(@SDuplicatesNotAllowed);

  LIndex := AddIndex(LHash);

  // very slow :<
  for i := AIndex to FItemsLength - 2 do // without new Pair
    Inc(FIndexItems[GetItemIndex(FPairItems[i].Key)].Index);
  System.Move(FPairItems[AIndex], FPairItems[AIndex + 1], Pred(FItemsLength - AIndex) * SizeOf(TPair<TKey, TValue>));
  FillChar(FPairItems[AIndex], SizeOf(TPair<TKey, TValue>), #0);

  FIndexItems[LIndex].Index := AIndex;
  FPairItems[AIndex].Key := AKey;
  FPairItems[AIndex].Value := AValue;

  PairNotify(FPairItems[AIndex], cnAdded);
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.Exchange(AIndex1, AIndex2: Integer);
var
  LTemp: TPair<TKey, TValue>;
  LIndex1, LIndex2: Integer;
begin
  if (AIndex1 < 0) or (AIndex2 < 0) or (AIndex1 >= FItemsLength) or (AIndex2 >= FItemsLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LTemp := FPairItems[AIndex1];

  LIndex1 := GetItemIndex(LTemp.Key);
  LIndex2 := GetItemIndex(FPairItems[AIndex2].Key);

  FIndexItems[LIndex1].Index := AIndex2;
  FIndexItems[LIndex2].Index := AIndex1;

  FPairItems[AIndex1] := FPairItems[AIndex2];
  FPairItems[AIndex2] := LTemp;
end;

//procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.Move(AIndex, ANewIndex: Integer);
//var
//  LTemp: T;
//begin
//  if ANewIndex = AIndex then
//    Exit;
//
//  if (ANewIndex < 0) or (ANewIndex >= Count) then
//    raise Exception.Create('Argument out of range');
//
//  LTemp := FItems[AIndex];
//  FItems[AIndex] := Default(T);
//
//  if AIndex < ANewIndex then
//    System.Move(FItems[Succ(AIndex)], FItems[AIndex], (ANewIndex - AIndex) * SizeOf(T))
//  else
//    System.Move(FItems[ANewIndex], FItems[Succ(ANewIndex)], (AIndex - ANewIndex) * SizeOf(T));
//
//  FillChar(FItems[ANewIndex], SizeOf(T), #0);
//  FItems[ANewIndex] := LTemp;
//end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.First: TPair<TKey, TValue>;
begin
  Result := PairItems[0];
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.Last: TPair<TKey, TValue>;
begin
  Result := PairItems[Pred(Count)];
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.IndexOfValue(const AValue: TValue;
  const AEqualityComparer: IEqualityComparer<TValue>): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
{$IFDEF FAST_COMPARISON}
    if AEqualityComparer = nil then
    begin
      if AValue = FPairItems[i].Value then
        Exit(i);
    end
    else
{$ENDIF}
      if AEqualityComparer.Equals(AValue.GetReferenceToValue, FPairItems[i].Value.GetReferenceToValue,
        AValue.GetValueSize, FPairItems[i].Value.GetValueSize) then
        Exit(i);
  Result := -1;
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.IndexOfKey(const AKey: TKey): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
{$IFDEF FAST_COMPARISON}
    if FEqualityComparer = nil then
    begin
      if AKey = FPairItems[i].Key then
        Exit(i);
    end
    else
{$ENDIF}
      if FEqualityComparer.Equals(AKey.GetReferenceToValue, FPairItems[i].Key.GetReferenceToValue,
        AKey.GetValueSize, FPairItems[i].Key.GetValueSize) then
        Exit(i);
  Result := -1;
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.IndexOfPair(const APair: TPair<TKey, TValue>;
  const AEqualityComparer: IEqualityComparer<TValue>): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
{$IFDEF FAST_COMPARISON}
    if AEqualityComparer = nil then
    begin
      if (APair.Key = FPairItems[i].Key) and (APair.Value = FPairItems[i].Value) then
        Exit(i);
    end
    else
{$ENDIF}
      if AEqualityComparer.Equals(APair.Key.GetReferenceToValue, FPairItems[i].Key.GetReferenceToValue,
        APair.Key.GetValueSize, FPairItems[i].Key.GetValueSize)
      and AEqualityComparer.Equals(APair.Value.GetReferenceToValue, FPairItems[i].Value.GetReferenceToValue,
        APair.Value.GetValueSize, FPairItems[i].Value.GetValueSize)
      then
        Exit(i);
  Result := -1;
end;

function TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.LastIndexOfValue(const AValue: TValue;
  const AEqualityComparer: IEqualityComparer<TValue>): Integer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
{$IFDEF FAST_COMPARISON}
    if AEqualityComparer = nil then
    begin
      if AValue = FPairItems[i].Value then
        Exit(i);
    end
    else
{$ENDIF}
      if AEqualityComparer.Equals(AValue.GetReferenceToValue, FPairItems[i].Value.GetReferenceToValue,
        AValue.GetValueSize, FPairItems[i].Value.GetValueSize) then
        Exit(i);
  Result := -1;
end;

procedure TDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.Reverse;
var
  a, b: Integer;
  LTempPair: TPair<TKey, TValue>;
begin
  a := 0;
  b := Count - 1;
  while a < b do
  begin
    LTempPair := FPairItems[a];
    FIndexItems[GetItemIndex(LTempPair.Key)].Index := b;
    FIndexItems[GetItemIndex(FPairItems[b].Key)].Index := a;
    FPairItems[a] := FPairItems[b];
    FPairItems[b] := LTempPair;
    Inc(a);
    Dec(b);
  end;
end;

//procedure SortByKey; overload; // special version of equality comparer
//procedure SortByKey(const AComparer: IComparer<T>); overload;
//procedure SortByValue; overload;
//procedure SortByValue(const AComparer: IComparer<T>); overload;

{ TFastDictionaryList<DICTIONARY_LIST_CONSTRAINTS> }

function TFastDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetItemIndex(const AKey: TKey; out AHash: THash): Integer;
var
  imin, imax, imid: Int32;
  i: Int32;
  LKey: TKey;
begin
    // continually narrow search until just one element remains
  imin := 0;
  imax := FItemsLength - 1;
  if FEqualityComparer = nil then
    AHash := THashFactory.Hash(AKey.GetReferenceToValue, AKey.GetValueSize)
  else
    AHash := FEqualityComparer.GetHashCode(AKey.GetReferenceToValue, AKey.GetValueSize);
  // http://en.wikipedia.org/wiki/Binary_search_algorithm
  while (imin < imax) do
  begin
        imid := imin + ((imax - imin) div 2);

        // code must guarantee the interval is reduced at each iteration
        // assert(imid < imax);
        // note: 0 <= imin < imax implies imid will always be less than imax

        // reduce the search
        if (FIndexItems[imid].Hash < AHash) then
          imin := imid + 1
        else
          imax := imid;
  end;
    // At exit of while:
    //   if A[] is empty, then imax < imin
    //   otherwise imax == imin

    // deferred test for equality

    if (imax = imin) and (FIndexItems[imin].Hash = AHash) then
    begin
      LKey := FPairItems[FIndexItems[imin].Index].Key;
      if FEqualityComparer = nil then
      begin
        if AKey = LKey then
          Exit(imin);
      end
      else
        if FEqualityComparer.Equals(AKey.GetReferenceToValue, LKey.GetReferenceToValue, AKey.GetValueSize, LKey.GetValueSize) then
          Exit(imin);
      //if (AKey.GetValueSize = LKey.GetValueSize) then
      //  if CompareMem(AKey.GetReferenceToValue, LKey.GetReferenceToValue, LKey.GetValueSize) then
      //    Exit(imin);

      // sprawdź lewą stronę hashy
      for i := imin - 1 downto 0 do
        if (FIndexItems[i].Hash <> AHash) then
          Break
        else
        begin
          LKey := FPairItems[FIndexItems[i].Index].Key;
          if FEqualityComparer = nil then
          begin
            if AKey = LKey then
              Exit(i);
          end
          else
            if FEqualityComparer.Equals(AKey.GetReferenceToValue, LKey.GetReferenceToValue, AKey.GetValueSize, LKey.GetValueSize) then
              Exit(i);
          //if (AKey.GetValueSize = LKey.GetValueSize) then
          //  if CompareMem(AKey.GetReferenceToValue, LKey.GetReferenceToValue, LKey.GetValueSize) then
          //    Exit(i);
        end;
      // sprawdź prawą stronę hashy
      for i := imin + 1 to FItemsLength - 1 do
        if (FIndexItems[i].Hash <> AHash) then
          Exit(-1)
        else
        begin
          LKey := FPairItems[FIndexItems[i].Index].Key;
          if FEqualityComparer = nil then
          begin
            if AKey = LKey then
              Exit(i);
          end
          else
            if FEqualityComparer.Equals(AKey.GetReferenceToValue, LKey.GetReferenceToValue, AKey.GetValueSize, LKey.GetValueSize) then
              Exit(i);
          //if (AKey.GetValueSize = LKey.GetValueSize) then
          //  if CompareMem(AKey.GetReferenceToValue, LKey.GetReferenceToValue, LKey.GetValueSize) then
          //    Exit(i);
        end;
    end
    else
      Exit(-1);
end;

function TFastDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.GetHash(const AKey: TKey): THash;
begin
  if FEqualityComparer = nil then
    Result := THashFactory.Hash(AKey.GetReferenceToValue, AKey.GetValueSize)
  else
    Result := FEqualityComparer.GetHashCode(AKey.GetReferenceToValue, AKey.GetValueSize);
end;

function TFastDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.ContainsValue(const AValue: TValue;
  const AEqualityComparer: IEqualityComparer<TValue>): Boolean;
var
  i: Integer;
  LItem: ^TPair<TKey, TValue>;
begin
  if FItemsLength = 0 then
    Exit(False);

  for i := 0 to FItemsLength - 1 do
  begin
    LItem := @FPairItems[i];

    if AEqualityComparer = nil then
    begin
      if AValue = LItem.Value then
        Exit(True);
    end
    else
      if AEqualityComparer.Equals(AValue.GetReferenceToValue, LItem.Value.GetReferenceToValue,
        AValue.GetValueSize, LItem.Value.GetValueSize) then
        Exit;
  end;
  Result := False;
end;

function TFastDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.IndexOfValue(const AValue: TValue;
  const AEqualityComparer: IEqualityComparer<TValue>): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if AEqualityComparer = nil then
    begin
      if AValue = FPairItems[i].Value then
        Exit(i);
    end
    else
      if AEqualityComparer.Equals(AValue.GetReferenceToValue, FPairItems[i].Value.GetReferenceToValue,
        AValue.GetValueSize, FPairItems[i].Value.GetValueSize) then
        Exit(i);
  Result := -1;
end;

function TFastDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.IndexOfKey(const AKey: TKey): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if FEqualityComparer = nil then
    begin
      if AKey = FPairItems[i].Key then
        Exit(i);
    end
    else
      if FEqualityComparer.Equals(AKey.GetReferenceToValue, FPairItems[i].Key.GetReferenceToValue,
        AKey.GetValueSize, FPairItems[i].Key.GetValueSize) then
        Exit(i);
  Result := -1;
end;

function TFastDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.IndexOfPair(const APair: TPair<TKey, TValue>;
  const AEqualityComparer: IEqualityComparer<TValue>): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if AEqualityComparer = nil then
    begin
      if (APair.Key = FPairItems[i].Key) and (APair.Value = FPairItems[i].Value) then
        Exit(i);
    end
    else
      if AEqualityComparer.Equals(APair.Key.GetReferenceToValue, FPairItems[i].Key.GetReferenceToValue,
        APair.Key.GetValueSize, FPairItems[i].Key.GetValueSize)
      and AEqualityComparer.Equals(APair.Value.GetReferenceToValue, FPairItems[i].Value.GetReferenceToValue,
        APair.Value.GetValueSize, FPairItems[i].Value.GetValueSize)
      then
        Exit(i);
  Result := -1;
end;

function TFastDictionaryList<DICTIONARY_LIST_CONSTRAINTS>.LastIndexOfValue(const AValue: TValue;
  const AEqualityComparer: IEqualityComparer<TValue>): Integer;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if AEqualityComparer = nil then
    begin
      if AValue = FPairItems[i].Value then
        Exit(i);
    end
    else
      if AEqualityComparer.Equals(AValue.GetReferenceToValue, FPairItems[i].Value.GetReferenceToValue,
        AValue.GetValueSize, FPairItems[i].Value.GetValueSize) then
        Exit(i);
  Result := -1;
end;


{ TLinearProbing }

class function TLinearProbing.Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TLinearProbing;
begin
  Result.FHash := AEqualityComparer.GetHashCode(AKey, AKeySize);
end;

class function TLinearProbing.Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TLinearProbing;
begin
  Result.FHash := AHashFactory.Hash(AKey, AKeySize)
end;

function TLinearProbing.Probe(I, M: Int32): Int32;
begin
  Result := (FHash + I) mod M;
end;

{ TQuadraticProbing }

class constructor TQuadraticProbing.Create;
begin
  C1 := 1;
  C2 := 1;
end;

class function TQuadraticProbing.Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TQuadraticProbing;
begin
  Result.FHash := AEqualityComparer.GetHashCode(AKey, AKeySize);
end;

class function TQuadraticProbing.Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TQuadraticProbing;
begin
  Result.FHash := AHashFactory.Hash(AKey, AKeySize)
end;

function TQuadraticProbing.Probe(I, M: Int32): Int32;
begin
  Result := (FHash + C1 * I + C2 * Sqr(I)) mod M;
end;

{ TDoubleHashing }

class function TDoubleHashing.Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TDoubleHashing;
begin
  Result.FHash := AEqualityComparer.GetHashCode(AKey, AKeySize, Result.FHash2);
end;

class function TDoubleHashing.Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TDoubleHashing;
begin
  Result.FHash := AHashFactory.Hash(AKey, AKeySize, Result.FHash2)
end;

function TDoubleHashing.Probe(I, M: Int32): Int32;
begin
  Result := (FHash + I * FHash2) mod M;
end;

{ TFastLinearProbing }

class function TFastLinearProbing.Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TFastLinearProbing;
begin
  Result.FHash := AEqualityComparer.GetHashCode(AKey, AKeySize);
end;

class function TFastLinearProbing.Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TFastLinearProbing;
begin
  Result.FHash := AHashFactory.Hash(AKey, AKeySize)
end;

function TFastLinearProbing.Probe(I, M: Int32): Int32;
begin
  Result := FHash + I
end;

{ TFastQuadraticProbing }

class constructor TFastQuadraticProbing.Create;
begin
  C1 := 1;
  C2 := 1;
end;

class function TFastQuadraticProbing.Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TFastQuadraticProbing;
begin
  Result.FHash := AEqualityComparer.GetHashCode(AKey, AKeySize);
end;

class function TFastQuadraticProbing.Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TFastQuadraticProbing;
begin
  Result.FHash := AHashFactory.Hash(AKey, AKeySize)
end;

function TFastQuadraticProbing.Probe(I, M: Int32): Int32;
begin
  Result := FHash + C1 * I + C2 * Sqr(I);
end;

{ TFastDoubleHashing }

class function TFastDoubleHashing.Create(const AEqualityComparer: IEqualityComparer; AKey: Pointer; AKeySize: Integer): TFastDoubleHashing;
begin
  Result.FHash := AEqualityComparer.GetHashCode(AKey, AKeySize, Result.FHash2);
end;

class function TFastDoubleHashing.Create(const AHashFactory: THashFactoryClass; AKey: Pointer; AKeySize: Integer): TFastDoubleHashing;
begin
  Result.FHash := AHashFactory.Hash(AKey, AKeySize, Result.FHash2)
end;

function TFastDoubleHashing.Probe(I, M: Int32): Int32;
begin
  Result := FHash + I * FHash2;
end;

{ TObjectList<T> }

procedure TObjectList<T>.Notify(const AValue: T; ACollectionNotification: TCollectionNotification);
begin
  inherited Notify(AValue, ACollectionNotification);

  if FObjectsOwner and (ACollectionNotification = cnRemoved) then
    AValue.Free;
end;

constructor TObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectList<T>.Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean);
begin
  inherited Create(AComparer);

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectList<T>.Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;

{ TObjectQueue<T> }

procedure TObjectQueue<T>.Notify(const AValue: T; ACollectionNotification: TCollectionNotification);
begin
  inherited Notify(AValue, ACollectionNotification);
  if FObjectsOwner and (ACollectionNotification = cnRemoved) then
    AValue.Free;
end;

constructor TObjectQueue<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectQueue<T>.Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;

procedure TObjectQueue<T>.Dequeue;
begin
  inherited Dequeue;
end;

{ TObjectStack<T> }

procedure TObjectStack<T>.Notify(const AValue: T; ACollectionNotification: TCollectionNotification);
begin
  inherited Notify(AValue, ACollectionNotification);
  if FObjectsOwner and (ACollectionNotification = cnRemoved) then
    AValue.Free;
end;

constructor TObjectStack<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectStack<T>.Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;

procedure TObjectStack<T>.Pop;
begin
  inherited Pop;
end;

{ TObjectDictionary<DICTIONARY_CONSTRAINTS> }

procedure TObjectDictionary<DICTIONARY_CONSTRAINTS>.KeyNotify(const AKey: TKey; ACollectionNotification: TCollectionNotification);
begin
  inherited;

  if (doOwnsKeys in FOwnerships) and (ACollectionNotification = cnRemoved) then
    AKey.Free;
end;

procedure TObjectDictionary<DICTIONARY_CONSTRAINTS>.ValueNotify(const AValue: TValue; ACollectionNotification: TCollectionNotification);
begin
  inherited;

  if (doOwnsValues in FOwnerships) and (ACollectionNotification = cnRemoved) then
    AValue.Free;
end;

constructor TObjectDictionary<DICTIONARY_CONSTRAINTS>.Create(AOwnerships: TDictionaryOwnerships; ACapacity: Integer);
begin
  inherited Create(ACapacity);

  FOwnerships := AOwnerships;
end;

constructor TObjectDictionary<DICTIONARY_CONSTRAINTS>.Create(AOwnerships: TDictionaryOwnerships;
  const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(AComparer);

  FOwnerships := AOwnerships;
end;

constructor TObjectDictionary<DICTIONARY_CONSTRAINTS>.Create(AOwnerships: TDictionaryOwnerships; ACapacity: Integer;
  const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(ACapacity, AComparer);

  FOwnerships := AOwnerships;
end;

end.
