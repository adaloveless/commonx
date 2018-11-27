unit PXL.Lists;
{
  This file is part of Asphyre Framework, also known as Pascal eXtended Library (PXL).
  Copyright (c) 2000 - 2015  Yuriy Kotsarenko

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.
}
interface

{$INCLUDE PXL.Config.inc}

uses
  Classes, PXL.TypeDef, PXL.Types;

{ Enable the following option to make TIntegerList and TIntProbList use VectorInt instead of Integer data type.
  This can be useful, for instance, when VectorInt is used as 64-bit integer. }
{$DEFINE TINTEGERLIST_VECTORINT}

type
  PListInt = ^TListInt;
{$IFDEF TINTEGERLIST_VECTORINT}
  TListInt = VectorInt;
{$ELSE}
  TListInt = Integer;
{$ENDIF}

  TIntegerList = class
  public type
    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FList: TIntegerList;
      FIndex: Integer;
      function GetCurrent: TListInt;
    public
      constructor Create(const AList: TIntegerList);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: TListInt read GetCurrent;
    end;
  private const
    ListGrowIncrement = 8;
    ListGrowFraction = 4;
  private
    FData: array of TListInt;
    FDataCount: Integer;

    function GetItem(const Index: Integer): TListInt;
    procedure SetItem(const Index: Integer; const Value: TListInt);
    procedure Request(const NeedCapacity: Integer);
    function GetMemAddr: Pointer;
    function GetValuesSum: TListInt;
    function GetValuesAvg: TListInt;
    function GetValuesMax: TListInt;
    function GetValuesMin: TListInt;
    function GetRandomValue: TListInt;
    procedure ListSwap(const Index1, Index2: Integer); inline;
    function ListCompare(const Value1, Value2: TListInt): Integer;
    function ListSplit(const Start, Stop: Integer): Integer;
    procedure ListSort(const Start, Stop: Integer);
    procedure SetCount(const NewCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(const Value: TListInt): Integer;
    function Add(const Value: TListInt): Integer; overload;

    procedure InsertFirst(const Value: TListInt);

    procedure Remove(const Index: Integer);
    procedure Clear;

    procedure Sort;
    procedure Swap(const Index1, Index2: Integer);

    procedure CopyFrom(const Source: TIntegerList);
    procedure AddFrom(const Source: TIntegerList);
    procedure AddFromPtr(const Source: PListInt; const ElementCount: Integer);

    procedure Include(const Value: TListInt);
    procedure Exclude(const Value: TListInt);

    function Exists(const Value: TListInt): Boolean;

    procedure Series(const NumCount: Integer);

    procedure InsertRepeatValue(const Value: TListInt; const Count: Integer);

    procedure Shuffle;
    procedure BestShuffle;
    procedure RemoveDuplicates;

    // Returns text representation of the string e.g. "21, 7, 14, 10, 20".
    function ChainToString: StdString;

    procedure DefineValueAtIndex(const Index: Integer; const Value: TListInt);
    procedure IncrementValueAtIndex(const Index: Integer);
    function GetValueAtIndex(const Index: Integer): TListInt;

    function SameAs(const OtherList: TIntegerList): Boolean;

    function GetEnumerator: TEnumerator;

    procedure SaveToStream(const Stream: TStream);
    procedure LoadFromStream(const Stream: TStream);

    property MemAddr: Pointer read GetMemAddr;

    property Count: Integer read FDataCount write SetCount;
    property Items[const Index: Integer]: TListInt read GetItem write SetItem; default;

    property ValuesSum: TListInt read GetValuesSum;
    property ValuesAvg: TListInt read GetValuesAvg;
    property ValuesMax: TListInt read GetValuesMax;
    property ValuesMin: TListInt read GetValuesMin;

    property RandomValue: TListInt read GetRandomValue;
  end;

  TPointList = class
  public type
    PItem = ^TItem;
    TItem = record
      Point: TPoint2px;
      Data: Pointer;
    end;

    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FList: TPointList;
      FIndex: Integer;
      function GetCurrent: PItem;
    public
      constructor Create(const AList: TPointList);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: PItem read GetCurrent;
    end;
  private const
    ListGrowIncrement = 8;
    ListGrowFraction = 4;
  private
    FData: array of TItem;
    FDataCount: Integer;

    function GetMemAddr: Pointer;
    procedure Request(const NeedCapacity: Integer);
    function GetItem(const Index: Integer): PItem;
    function GetPoint(const Index: Integer): PPoint2px;
    procedure SetCount(const NewCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Point: TPoint2px; const Data: Pointer = nil): Integer;

    procedure Remove(const Index: Integer);
    procedure Clear;

    function IndexOf(const Point: TPoint2px): Integer;
    function IndexOfData(const Data: Pointer): Integer;

    procedure Include(const Point: TPoint2px; const Data: Pointer = nil);
    procedure Exclude(const Point: TPoint2px);

    procedure CopyFrom(const Source: TPointList);
    procedure AddFrom(const Source: TPointList);

    function GetEnumerator: TEnumerator;

    property MemAddr: Pointer read GetMemAddr;

    property Count: Integer read FDataCount write SetCount;
    property Items[const Index: Integer]: PItem read GetItem; default;
    property Point[const Index: Integer]: PPoint2px read GetPoint;
  end;

  TIntRectList = class
  public type
    PItem = ^TItem;
    TItem = record
      Rect: TIntRect;
      Data: Pointer;
    end;

    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FList: TIntRectList;
      FIndex: Integer;
      function GetCurrent: PItem;
    public
      constructor Create(const AList: TIntRectList);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: PItem read GetCurrent;
    end;
  private const
    ListGrowIncrement = 8;
    ListGrowFraction = 4;
  private
    FData: array of TItem;
    FDataCount: Integer;

    function GetMemAddr: Pointer;
    function GetItem(const Index: Integer): PItem;
    procedure Request(const NeedCapacity: Integer);
    procedure SetCount(const NewCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Rect: TIntRect; const Data: Pointer = nil): Integer; overload;
    function Add(const Left, Top, Width, Height: VectorInt; const Data: Pointer = nil): Integer; overload;

    procedure Remove(const Index: Integer);
    procedure Clear;

    function IndexOf(const Rect: TIntRect): Integer;
    function IndexOfData(const Data: Pointer): Integer;

    procedure CopyFrom(const Source: TIntRectList);
    procedure AddFrom(const Source: TIntRectList);

    function GetEnumerator: TEnumerator;

    property MemAddr: Pointer read GetMemAddr;

    property Count: Integer read FDataCount write SetCount;
    property Items[const Index: Integer]: PItem read GetItem; default;
  end;

  TFloatRectList = class
  public type
    PItem = ^TItem;
    TItem = record
      Rect: TFloatRect;
      Data: Pointer;
    end;

    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FList: TFloatRectList;
      FIndex: Integer;
      function GetCurrent: PItem;
    public
      constructor Create(const AList: TFloatRectList);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: PItem read GetCurrent;
    end;
  private const
    ListGrowIncrement = 8;
    ListGrowFraction = 4;
  private
    FData: array of TItem;
    FDataCount: Integer;

    function GetMemAddr: Pointer;
    function GetItem(const Index: Integer): PItem;
    procedure Request(const NeedCapacity: Integer);
    procedure SetCount(const NewCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Rect: TFloatRect; const Data: Pointer = nil): Integer; overload;
    function Add(const Left, Top, Width, Height: VectorFloat; const Data: Pointer = nil): Integer; overload;

    procedure Remove(const Index: Integer);
    procedure Clear;

    function IndexOf(const Rect: TFloatRect): Integer;
    function IndexOfData(const Data: Pointer): Integer;

    procedure CopyFrom(const Source: TFloatRectList);
    procedure AddFrom(const Source: TFloatRectList);

    function GetEnumerator: TEnumerator;

    property MemAddr: Pointer read GetMemAddr;

    property Count: Integer read FDataCount write SetCount;
    property Items[const Index: Integer]: PItem read GetItem; default;
  end;

  TIntegerProbabilityList = class
  public type
    PDataSample = ^TDataSample;
    TDataSample = record
      Value: TListInt;
      Probability: VectorFloat;
    end;

    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FList: TIntegerProbabilityList;
      FIndex: Integer;
      function GetCurrent: PDataSample;
    public
      constructor Create(const AList: TIntegerProbabilityList);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: PDataSample read GetCurrent;
    end;
  private const
    ListGrowIncrement = 8;
    ListGrowFraction = 4;
  private
    FData: array of TDataSample;
    FDataCount: Integer;

    function GetMemAddr: Pointer;
    function GetItem(const Index: Integer): PDataSample;
    procedure Request(const NeedCapacity: Integer);
    function GetValue(const Index: Integer): Integer;
    function GetProbability(const DataValue: TListInt): VectorFloat;
    procedure SetProbability(const DataValue: TListInt; const Probability: VectorFloat);
    function GetRandomValue: TListInt;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const DataValue: TListInt; const Probability: VectorFloat = 1): Integer;

    procedure Remove(const Index: Integer);
    procedure Clear;

    function IndexOf(const DataValue: TListInt): Integer;
    function Exists(const DataValue: TListInt): Boolean;

    procedure Include(const DataValue: TListInt; const Probability: VectorFloat = 1);
    procedure Exclude(const DataValue: TListInt);

    procedure Series(const MaxValue: TListInt; const Probability: VectorFloat = 1.0);

    procedure ScaleProbability(const DataValue: TListInt; const Scale: VectorFloat);
    procedure ScaleProbabilityExcept(const DataValue: TListInt; const Scale: VectorFloat);

    procedure CopyFrom(const Source: TIntegerProbabilityList);
    procedure AddFrom(const Source: TIntegerProbabilityList);

    function ExtractRandomValue: TListInt;
    procedure NormalizeProbabilities;

    procedure SaveToStream(const Stream: TStream);
    procedure LoadFromStream(const Stream: TStream);

    function GetEnumerator: TEnumerator;

    property MemAddr: Pointer read GetMemAddr;

    property Count: Integer read FDataCount;
    property Items[const Index: Integer]: PDataSample read GetItem;

    property Value[const Index: Integer]: Integer read GetValue;
    property Probability[const DataValue: TListInt]: VectorFloat read GetProbability write SetProbability; default;

    property RandomValue: TListInt read GetRandomValue;
  end;

  TPoints2px = class
  public type
    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FList: TPoints2px;
      FIndex: Integer;
      function GetCurrent: PPoint2px;
    public
      constructor Create(const AList: TPoints2px);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: PPoint2px read GetCurrent;
    end;
  private const
    ListGrowIncrement = 60;
    ListGrowFraction = 4;
  private
    FData: array of TPoint2px;
    FDataCount: Integer;

    function GetItem(const Index: Integer): PPoint2px;
    procedure Request(const NeedCapacity: Integer);
    procedure SetCount(const NewCount: Integer);
    function GetMemAddr: Pointer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Point: TPoint2px): Integer;

    procedure Remove(const Index: Integer);
    procedure Clear;

    function IndexOf(const Point: TPoint2px): Integer;

    procedure Include(const Point: TPoint2px);
    procedure Exclude(const Point: TPoint2px);

    procedure CopyFrom(const Source: TPoints2px);
    procedure AddFrom(const Source: TPoints2px);
    procedure AddFromPtr(const Source: PPoint2px; const ElementCount: Integer);

    function GetEnumerator: TEnumerator;

    property MemAddr: Pointer read GetMemAddr;

    property Count: Integer read FDataCount write SetCount;
    property Items[const Index: Integer]: PPoint2px read GetItem; default;
  end;

  TPoints2 = class
  public type
    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FList: TPoints2;
      FIndex: Integer;
      function GetCurrent: PPoint2;
    public
      constructor Create(const AList: TPoints2);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: PPoint2 read GetCurrent;
    end;
  private const
    ListGrowIncrement = 60;
    ListGrowFraction = 4;
  private
    FData: array of TPoint2;
    FDataCount: Integer;

    procedure SetCount(const NewCount: Integer);
    function GetItem(const Index: Integer): PPoint2;
    procedure Request(const NeedCapacity: Integer);
    function GetMemAddr: Pointer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Point: TPoint2): Integer;

    procedure Remove(const Index: Integer);
    procedure Clear;

    function IndexOf(const Point: TPoint2): Integer;

    procedure Include(const Point: TPoint2);
    procedure Exclude(const Point: TPoint2);

    procedure CopyFrom(const Source: TPoints2);
    procedure AddFrom(const Source: TPoints2);
    procedure AddFromPtr(const Source: PPoint2; const ElementCount: Integer);

    function GetEnumerator: TEnumerator;

    property MemAddr: Pointer read GetMemAddr;

    property Count: Integer read FDataCount write SetCount;
    property Items[const Index: Integer]: PPoint2 read GetItem; default;
  end;

  TVectors3 = class
  public type
    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FList: TVectors3;
      FIndex: Integer;
      function GetCurrent: PVector3;
    public
      constructor Create(const AList: TVectors3);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: PVector3 read GetCurrent;
    end;
  private const
    ListGrowIncrement = 80;
    ListGrowFraction = 4;
  private
    FData: array of TVector3;
    FDataCount: Integer;

    procedure SetCount(const NewCount: Integer);
    function GetItem(const Index: Integer): PVector3;
    procedure Request(const NeedCapacity: Integer);
    function GetMemAddr: Pointer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Vector: TVector3): Integer;
    function IndexOf(const Vector: TVector3): Integer;

    procedure Remove(const Index: Integer);
    procedure Clear;

    procedure Include(const Vector: TVector3);
    procedure Exclude(const Vector: TVector3);

    procedure CopyFrom(const Source: TVectors3);
    procedure AddFrom(const Source: TVectors3);
    procedure AddFromPtr(const Source: PVector3; const ElementCount: Integer);

    function GetEnumerator: TEnumerator;

    property MemAddr: Pointer read GetMemAddr;

    property Count: Integer read FDataCount write SetCount;
    property Items[const Index: Integer]: PVector3 read GetItem; default;
  end;

  TVectors4 = class
  public type
    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FList: TVectors4;
      FIndex: Integer;
      function GetCurrent: PVector4;
    public
      constructor Create(const AList: TVectors4);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: PVector4 read GetCurrent;
    end;
  private const
    ListGrowIncrement = 120;
    ListGrowFraction = 4;
  private
    FData: array of TVector4;
    FDataCount: Integer;

    function GetMemAddr: Pointer;
    procedure SetCount(const NewCount: Integer);
    procedure Request(const NeedCapacity: Integer);
    function GetItem(const Index: Integer): PVector4;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Vector: TVector4): Integer;
    function IndexOf(const Vector: TVector4): Integer;

    procedure Remove(const Index: Integer);
    procedure Clear;

    procedure Include(const Vector: TVector4);
    procedure Exclude(const Vector: TVector4);

    procedure CopyFrom(const Source: TVectors4);
    procedure AddFrom(const Source: TVectors4);

    function GetEnumerator: TEnumerator;

    property MemAddr: Pointer read GetMemAddr;

    property Count: Integer read FDataCount write SetCount;
    property Items[const Index: Integer]: PVector4 read GetItem; default;
  end;

implementation

uses
  SysUtils, Math, PXL.Classes;

{$REGION 'Global Functions'}

function RoundBy16(const Value: Integer): Integer; inline;
const
  RoundBlockSize = 16;
begin
  Result := (Value + RoundBlockSize - 1) and (not (RoundBlockSize - 1));
end;

{$ENDREGION}
{$REGION 'TIntegerList'}

constructor TIntegerList.TEnumerator.Create(const AList: TIntegerList);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FList := AList;
  FIndex := -1;
end;

destructor TIntegerList.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TIntegerList.TEnumerator.GetCurrent: TListInt;
begin
  Result := FList[FIndex];
end;

function TIntegerList.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;

  if Result then
    Inc(FIndex);
end;

constructor TIntegerList.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TIntegerList.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TIntegerList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TIntegerList.GetMemAddr: Pointer;
begin
  if FDataCount > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

function TIntegerList.GetItem(const Index: Integer): TListInt;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := FData[Index]
  else
    Result := Low(Integer);
end;

procedure TIntegerList.SetItem(const Index: Integer; const Value: TListInt);
begin
  if (Index >= 0) and (Index < FDataCount) then
    FData[Index] := Value;
end;

procedure TIntegerList.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FData);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(FData, RoundBy16(NewCapacity));
  end;
end;

procedure TIntegerList.SetCount(const NewCount: Integer);
begin
  if NewCount > 0 then
  begin
    Request(NewCount);
    FDataCount := NewCount;
  end
  else
    Clear;
end;

function TIntegerList.Add(const Value: TListInt): Integer;
var
  Index: Integer;
begin
  Index := FDataCount;
  Request(FDataCount + 1);

  FData[Index] := Value;
  Inc(FDataCount);

  Result := Index;
end;

procedure TIntegerList.InsertFirst(const Value: TListInt);
var
  I: Integer;
begin
  Request(FDataCount + 1);

  for I := FDataCount - 1 downto 1 do
    FData[I] := FData[I - 1];

  FData[0] := Value;
end;

procedure TIntegerList.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FDataCount) then
    Exit;

  for I := Index to FDataCount - 2 do
    FData[I] := FData[I + 1];

  Dec(FDataCount);
end;

procedure TIntegerList.Clear;
begin
  FDataCount := 0;
end;

procedure TIntegerList.CopyFrom(const Source: TIntegerList);
var
  I: Integer;
begin
  Request(Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I] := Source.FData[I];

  FDataCount := Source.FDataCount;
end;

procedure TIntegerList.AddFrom(const Source: TIntegerList);
var
  I: Integer;
begin
  Request(FDataCount + Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I + FDataCount] := Source.FData[I];

  Inc(FDataCount, Source.FDataCount);
end;

procedure TIntegerList.AddFromPtr(const Source: PListInt; const ElementCount: Integer);
var
  I: Integer;
  InpValue: PListInt;
begin
  Request(FDataCount + ElementCount);

  InpValue := Source;

  for I := 0 to ElementCount - 1 do
  begin
    FData[I + FDataCount] := InpValue^;
    Inc(InpValue);
  end;

  Inc(FDataCount, ElementCount);
end;

function TIntegerList.IndexOf(const Value: TListInt): Integer;
var
  I: Integer;
begin
  for I := 0 to FDataCount - 1 do
    if FData[I] = Value then
      Exit(I);

  Result := -1;
end;

procedure TIntegerList.Include(const Value: TListInt);
begin
  if IndexOf(Value) = -1 then
    Add(Value);
end;

procedure TIntegerList.Exclude(const Value: TListInt);
var
  Index: Integer;
begin
  Index := IndexOf(Value);
  if Index <> -1 then
    Remove(Index);
end;

function TIntegerList.Exists(const Value: TListInt): Boolean;
begin
  Result := IndexOf(Value) <> -1;
end;

procedure TIntegerList.BestShuffle;
var
  TempList: TIntegerList;
  Index: Integer;
begin
  TempList := TIntegerList.Create;
  try
    TempList.CopyFrom(Self);
    Clear;

    while TempList.Count > 0 do
    begin
      Index := Random(TempList.Count);
      Add(TempList[Index]);
      TempList.Remove(Index);
    end;
  finally
    TempList.Free;
  end;
end;

procedure TIntegerList.Shuffle;
var
  I: Integer;
begin
  for I := FDataCount - 1 downto 1 do
    ListSwap(I, Random(I + 1));
end;

procedure TIntegerList.Series(const NumCount: Integer);
var
  I: Integer;
begin
  Request(NumCount);
  FDataCount := NumCount;

  for I := 0 to FDataCount - 1 do
    FData[I] := I;
end;

procedure TIntegerList.InsertRepeatValue(const Value: TListInt; const Count: Integer);
var
  I: Integer;
begin
  Request(FDataCount + Count);

  for I := 0 to Count - 1 do
    FData[FDataCount + I] := Value;

  Inc(FDataCount, Count);
end;

function TIntegerList.GetValuesSum: TListInt;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to FDataCount - 1 do
    Inc(Result, FData[I]);
end;

function TIntegerList.GetValuesAvg: TListInt;
begin
  if FDataCount > 0 then
    Result := GetValuesSum div FDataCount
  else
    Result := 0;
end;

function TIntegerList.GetValuesMax: TListInt;
var
  I: Integer;
begin
  if FDataCount < 1 then
    Exit(0);

  Result := FData[0];

  for I := 1 to FDataCount - 1 do
    Result := Max(Result, FData[I]);
end;

function TIntegerList.GetValuesMin: TListInt;
var
  I: Integer;
begin
  if FDataCount < 1 then
    Exit(0);

  Result := FData[0];

  for I := 1 to Length(FData) - 1 do
    Result := Min(Result, FData[I]);
end;

function TIntegerList.GetRandomValue: TListInt;
begin
  if FDataCount > 0 then
    Result := FData[Random(FDataCount)]
  else
    Result := 0;
end;

procedure TIntegerList.ListSwap(const Index1, Index2: Integer);
var
  TempValue: TListInt;
begin
  TempValue := FData[Index1];
  FData[Index1] := FData[Index2];
  FData[Index2] := TempValue;
end;

function TIntegerList.ListCompare(const Value1, Value2: TListInt): Integer;
begin
  Result := 0;

  if Value1 < Value2 then
    Result := -1
  else if Value1 > Value2 then
    Result := 1;
end;

function TIntegerList.ListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right: Integer;
  Pivot: TListInt;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FData[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (ListCompare(FData[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (ListCompare(FData[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      ListSwap(Left, Right);
  end;

  ListSwap(Start, Right);
  Result := Right;
end;

procedure TIntegerList.ListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := ListSplit(Start, Stop);

    ListSort(Start, SplitPt - 1);
    ListSort(SplitPt + 1, Stop);
  end;
end;

procedure TIntegerList.Sort;
begin
  if FDataCount > 1 then
    ListSort(0, FDataCount - 1);
end;

procedure TIntegerList.Swap(const Index1, Index2: Integer);
begin
  if (Index1 >= 0) and (Index1 < FDataCount) and (Index2 >= 0) and (Index2 < FDataCount) then
    ListSwap(Index1, Index2);
end;

procedure TIntegerList.SaveToStream(const Stream: TStream);
var
  I: Integer;
begin
  Stream.PutLongInt(FDataCount);

  for I := 0 to FDataCount - 1 do
    Stream.PutLongInt(FData[I]);
end;

procedure TIntegerList.LoadFromStream(const Stream: TStream);
var
  Amount, I: Integer;
begin
  Amount := Stream.GetLongInt;
  Request(Amount);

  for I := 0 to Amount - 1 do
    FData[I] := Stream.GetLongInt;

  FDataCount := Amount;
end;

function TIntegerList.SameAs(const OtherList: TIntegerList): Boolean;
var
  I: Integer;
begin
  // (1) Check if the list points to itself or if both lists are empty.
  if (Self = OtherList) or ((FDataCount < 1) and (OtherList.FDataCount < 1)) then
    Exit(True);

  // (2) If the lists have different number of elements, they are not equals.
  if FDataCount <> OtherList.FDataCount then
    Exit(False);

  // (3) Test element one by one.
  for I := 0 to FDataCount - 1 do
    if FData[I] <> OtherList.FData[I] then
      Exit(False);

  Result := True;
end;

procedure TIntegerList.RemoveDuplicates;
var
  I, J: Integer;
begin
  for J := FDataCount - 1 downto 0 do
    for I := 0 to J - 1 do
      if FData[J] = FData[I] then
      begin
        Remove(J);
        Break;
      end;
end;

function TIntegerList.ChainToString: StdString;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to FDataCount - 1 do
  begin
    Result := Result + IntToStr(FData[I]);

    if I < FDataCount - 1 then
      Result := Result + ', ';
  end;
end;

procedure TIntegerList.DefineValueAtIndex(const Index: Integer; const Value: TListInt);
var
  StartAt, I: Integer;
begin
  if Index < 0 then
    Exit;

  if Index >= FDataCount then
  begin
    StartAt := FDataCount;
    Request(Index + 1);

    for I := StartAt to Index - 1 do
      FData[I] := 0;

    FDataCount := Index + 1;
  end;

  FData[Index] := Value;
end;

procedure TIntegerList.IncrementValueAtIndex(const Index: Integer);
var
  StartAt, I: Integer;
begin
  if Index < 0 then
    Exit;

  if Index >= FDataCount then
  begin
    StartAt := FDataCount;

    Request(Index + 1);

    for I := StartAt to Index do
      FData[I] := 0;

    FDataCount := Index + 1;
  end;

  Inc(FData[Index]);
end;

function TIntegerList.GetValueAtIndex(const Index: Integer): TListInt;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := FData[Index]
  else
    Result := 0;
end;

{$ENDREGION}
{$REGION 'TPointList'}

constructor TPointList.TEnumerator.Create(const AList: TPointList);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FList := AList;
  FIndex := -1;
end;

destructor TPointList.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TPointList.TEnumerator.GetCurrent: PItem;
begin
  Result := FList[FIndex];
end;

function TPointList.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;

  if Result then
    Inc(FIndex);
end;

constructor TPointList.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TPointList.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TPointList.GetMemAddr: Pointer;
begin
  if FDataCount > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

function TPointList.GetItem(const Index: Integer): PItem;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := @FData[Index]
  else
    Result := nil;
end;

function TPointList.GetPoint(const Index: Integer): PPoint2px;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := @FData[Index].Point
  else
    Result := nil;
end;

procedure TPointList.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FData);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(FData, RoundBy16(NewCapacity));
  end;
end;

procedure TPointList.SetCount(const NewCount: Integer);
begin
  if NewCount > 0 then
  begin
    Request(NewCount);
    FDataCount := NewCount;
  end
  else
    Clear;
end;

function TPointList.Add(const Point: TPoint2px; const Data: Pointer): Integer;
var
  Index: Integer;
begin
  Index := FDataCount;
  Request(FDataCount + 1);

  Self.FData[Index].Point := Point;
  Self.FData[Index].Data := Data;

  Inc(FDataCount);

  Result := Index;
end;

procedure TPointList.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FDataCount) then
    Exit;

  for I := Index to FDataCount - 2 do
    FData[I] := FData[I + 1];

  Dec(FDataCount);
end;

procedure TPointList.Clear;
begin
  FDataCount := 0;
end;

function TPointList.IndexOf(const Point: TPoint2px): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if FData[I].Point = Point then
    begin
      Result := I;
      Break;
    end;
end;

function TPointList.IndexOfData(const Data: Pointer): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if Self.FData[I].Data = Data then
    begin
      Result := I;
      Break;
    end;
end;

procedure TPointList.Include(const Point: TPoint2px; const Data: Pointer);
begin
  if IndexOf(Point) = -1 then
    Add(Point, Data);
end;

procedure TPointList.Exclude(const Point: TPoint2px);
begin
  Remove(IndexOf(Point));
end;

procedure TPointList.CopyFrom(const Source: TPointList);
var
  I: Integer;
begin
  Request(Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I] := Source.FData[I];

  FDataCount := Source.FDataCount;
end;

procedure TPointList.AddFrom(const Source: TPointList);
var
  I: Integer;
begin
  Request(FDataCount + Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I + FDataCount] := Source.FData[I];

  Inc(FDataCount, Source.FDataCount);
end;

function TPointList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}
{$REGION 'TIntRectList'}

constructor TIntRectList.TEnumerator.Create(const AList: TIntRectList);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FList := AList;
  FIndex := -1;
end;

destructor TIntRectList.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TIntRectList.TEnumerator.GetCurrent: PItem;
begin
  Result := FList[FIndex];
end;

function TIntRectList.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;

  if Result then
    Inc(FIndex);
end;

constructor TIntRectList.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TIntRectList.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TIntRectList.GetMemAddr: Pointer;
begin
  if FDataCount > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

function TIntRectList.GetItem(const Index: Integer): PItem;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := @FData[Index]
  else
    Result := nil;
end;

procedure TIntRectList.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FData);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(FData, RoundBy16(NewCapacity));
  end;
end;

procedure TIntRectList.SetCount(const NewCount: Integer);
begin
  if NewCount > 0 then
  begin
    Request(NewCount);
    FDataCount := NewCount;
  end
  else
    Clear;
end;

function TIntRectList.Add(const Rect: TIntRect; const Data: Pointer): Integer;
var
  Index: Integer;
begin
  Index := FDataCount;
  Request(FDataCount + 1);

  Self.FData[Index].Rect := Rect;
  Self.FData[Index].Data := Data;

  Inc(FDataCount);
  Result := Index;
end;

function TIntRectList.Add(const Left, Top, Width, Height: VectorInt; const Data: Pointer): Integer;
begin
  Result := Add(IntRect(Left, Top, Width, Height), Data);
end;

procedure TIntRectList.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FDataCount) then
    Exit;

  for I := Index to FDataCount - 2 do
    FData[I] := FData[I + 1];

  Dec(FDataCount);
end;

procedure TIntRectList.Clear;
begin
  FDataCount := 0;
end;

function TIntRectList.IndexOf(const Rect: TIntRect): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if FData[I].Rect = Rect then
    begin
      Result := I;
      Break;
    end;
end;

function TIntRectList.IndexOfData(const Data: Pointer): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if Self.FData[I].Data = Data then
    begin
      Result := I;
      Break;
    end;
end;

procedure TIntRectList.CopyFrom(const Source: TIntRectList);
var
  I: Integer;
begin
  Request(Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I] := Source.FData[I];

  FDataCount := Source.FDataCount;
end;

procedure TIntRectList.AddFrom(const Source: TIntRectList);
var
  I: Integer;
begin
  Request(FDataCount + Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I + FDataCount] := Source.FData[I];

  Inc(FDataCount, Source.FDataCount);
end;

function TIntRectList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}
{$REGION 'TFloatRectList'}

constructor TFloatRectList.TEnumerator.Create(const AList: TFloatRectList);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FList := AList;
  FIndex := -1;
end;

destructor TFloatRectList.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TFloatRectList.TEnumerator.GetCurrent: PItem;
begin
  Result := FList[FIndex];
end;

function TFloatRectList.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;

  if Result then
    Inc(FIndex);
end;

constructor TFloatRectList.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TFloatRectList.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TFloatRectList.GetMemAddr: Pointer;
begin
  if FDataCount > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

function TFloatRectList.GetItem(const Index: Integer): PItem;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := @FData[Index]
  else
    Result := nil;
end;

procedure TFloatRectList.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FData);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(FData, RoundBy16(NewCapacity));
  end;
end;

procedure TFloatRectList.SetCount(const NewCount: Integer);
begin
  if NewCount > 0 then
  begin
    Request(NewCount);
    FDataCount := NewCount;
  end
  else
    Clear;
end;

function TFloatRectList.Add(const Rect: TFloatRect; const Data: Pointer): Integer;
var
  Index: Integer;
begin
  Index := FDataCount;
  Request(FDataCount + 1);

  Self.FData[Index].Rect := Rect;
  Self.FData[Index].Data := Data;

  Inc(FDataCount);

  Result := Index;
end;

function TFloatRectList.Add(const Left, Top, Width, Height: VectorFloat; const Data: Pointer): Integer;
begin
  Result := Add(FloatRect(Left, Top, Width, Height), Data);
end;

procedure TFloatRectList.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FDataCount) then
    Exit;

  for I := Index to FDataCount - 2 do
    FData[I] := FData[I + 1];

  Dec(FDataCount);
end;

procedure TFloatRectList.Clear;
begin
  FDataCount := 0;
end;

function TFloatRectList.IndexOf(const Rect: TFloatRect): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if FData[I].Rect = Rect then
    begin
      Result := I;
      Break;
    end;
end;

function TFloatRectList.IndexOfData(const Data: Pointer): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if Self.FData[I].Data = Data then
    begin
      Result := I;
      Break;
    end;
end;

procedure TFloatRectList.CopyFrom(const Source: TFloatRectList);
var
  I: Integer;
begin
  Request(Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I] := Source.FData[I];

  FDataCount := Source.FDataCount;
end;

procedure TFloatRectList.AddFrom(const Source: TFloatRectList);
var
  I: Integer;
begin
  Request(FDataCount + Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I + FDataCount] := Source.FData[I];

  Inc(FDataCount, Source.FDataCount);
end;

function TFloatRectList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}
{$REGION 'TIntegerProbabilityList'}

constructor TIntegerProbabilityList.TEnumerator.Create(const AList: TIntegerProbabilityList);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FList := AList;
  FIndex := -1;
end;

destructor TIntegerProbabilityList.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TIntegerProbabilityList.TEnumerator.GetCurrent: PDataSample;
begin
  Result := FList.Items[FIndex];
end;

function TIntegerProbabilityList.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;

  if Result then
    Inc(FIndex);
end;

constructor TIntegerProbabilityList.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TIntegerProbabilityList.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TIntegerProbabilityList.GetMemAddr: Pointer;
begin
  Result := @FData[0];
end;

function TIntegerProbabilityList.GetItem(const Index: Integer): PDataSample;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := @FData[Index]
  else
    Result := nil;
end;

function TIntegerProbabilityList.GetValue(const Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := FData[Index].Value
  else
    Result := 0;
end;

procedure TIntegerProbabilityList.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FData);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(FData, RoundBy16(NewCapacity));
  end;
end;

procedure TIntegerProbabilityList.Clear;
begin
  FDataCount := 0;
end;

function TIntegerProbabilityList.IndexOf(const DataValue: TListInt): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if FData[I].Value = DataValue then
    begin
      Result := I;
      Break;
    end;
end;

function TIntegerProbabilityList.Add(const DataValue: TListInt; const Probability: VectorFloat): Integer;
var
  Index: Integer;
begin
  Index := FDataCount;
  Request(FDataCount + 1);

  FData[Index].Value := DataValue;
  FData[Index].Probability := Probability;
  Inc(FDataCount);

  Result := Index;
end;

procedure TIntegerProbabilityList.Include(const DataValue: TListInt; const Probability: VectorFloat);
var
  Index: Integer;
begin
  Index := IndexOf(DataValue);

  if Index <> -1 then
    FData[Index].Probability := Probability
  else
    Add(DataValue, Probability);
end;

procedure TIntegerProbabilityList.Exclude(const DataValue: TListInt);
begin
  Remove(IndexOf(DataValue));
end;

function TIntegerProbabilityList.Exists(const DataValue: TListInt): Boolean;
begin
  Result := IndexOf(DataValue) <> -1;
end;

procedure TIntegerProbabilityList.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FDataCount) then
    Exit;

  for I := Index to FDataCount - 2 do
    FData[I] := FData[I + 1];

  Dec(FDataCount);
end;

procedure TIntegerProbabilityList.CopyFrom(const Source: TIntegerProbabilityList);
var
  I: Integer;
begin
  Request(Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I] := Source.FData[I];

  FDataCount := Source.FDataCount;
end;

procedure TIntegerProbabilityList.AddFrom(const Source: TIntegerProbabilityList);
var
  I: Integer;
begin
  Request(FDataCount + Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I + FDataCount] := Source.FData[I];

  Inc(FDataCount, Source.FDataCount);
end;

function TIntegerProbabilityList.GetProbability(const DataValue: TListInt): VectorFloat;
var
  Index: Integer;
begin
  Index := IndexOf(DataValue);

  if Index <> -1 then
    Result := FData[Index].Probability
  else
    Result := 0;
end;

procedure TIntegerProbabilityList.SetProbability(const DataValue: TListInt; const Probability: VectorFloat);
var
  Index: Integer;
begin
  Index := IndexOf(DataValue);

  if Index <> -1 then
    FData[Index].Probability := Probability
  else
    Add(DataValue, Probability);
end;

procedure TIntegerProbabilityList.Series(const MaxValue: TListInt; const Probability: VectorFloat);
var
  CurValue: TListInt;
begin
  Clear;
  CurValue := 0;

  while CurValue < MaxValue do
  begin
    Add(CurValue, Probability);
    Inc(CurValue);
  end;
end;

function TIntegerProbabilityList.GetRandomValue: TListInt;
var
  Sample, SampleMax, SampleIn: VectorFloat;
  I: Integer;
begin
  Result := 0;
  if FDataCount < 1 then
    Exit;

  SampleMax := 0;

  for I := 0 to FDataCount - 1 do
    SampleMax := SampleMax + FData[I].Probability;

  Sample := Random * SampleMax;

  SampleIn := 0;
  for I := 0 to FDataCount - 1 do
  begin
    if (Sample >= SampleIn) and (Sample < SampleIn + FData[I].Probability) then
      Exit(FData[I].Value);

    SampleIn := SampleIn + FData[I].Probability;
  end;
end;

function TIntegerProbabilityList.ExtractRandomValue: TListInt;
var
  Sample, SampleMax, SampleIn: VectorFloat;
  I, SampleNo: Integer;
begin
  Result := 0;
  if FDataCount < 1 then
    Exit;

  SampleMax := 0.0;

  for I := 0 to FDataCount - 1 do
    SampleMax := SampleMax + FData[I].Probability;

  Sample := Random * SampleMax;

  SampleIn := 0.0;
  SampleNo := -1;

  for I := 0 to FDataCount - 1 do
  begin
    if (Sample >= SampleIn) and (Sample < SampleIn + FData[I].Probability) then
    begin
      Result := FData[I].Value;
      SampleNo := I;

      Break;
    end;

    SampleIn := SampleIn + FData[I].Probability;
  end;

  if SampleNo <> -1 then
    Remove(SampleNo);
end;

procedure TIntegerProbabilityList.ScaleProbability(const DataValue: TListInt; const Scale: VectorFloat);
var
  Index: Integer;
begin
  Index := IndexOf(DataValue);

  if Index <> -1 then
    FData[Index].Probability := FData[Index].Probability * Scale;
end;

procedure TIntegerProbabilityList.ScaleProbabilityExcept(const DataValue: TListInt; const Scale: VectorFloat);
var
  I: Integer;
begin
  for I := 0 to FDataCount - 1 do
    if FData[I].Value <> DataValue then
      FData[I].Probability := FData[I].Probability * Scale;
end;

procedure TIntegerProbabilityList.NormalizeProbabilities;
var
  I: Integer;
  Total: VectorFloat;
begin
  Total := 0;

  for I := 0 to FDataCount - 1 do
    Total := Total + FData[I].Probability;

  if Total <= 0 then
    Exit;

  for I := 0 to FDataCount - 1 do
    FData[I].Probability := FData[I].Probability / Total;
end;

procedure TIntegerProbabilityList.SaveToStream(const Stream: TStream);
var
  I: Integer;
begin
  Stream.PutLongInt(FDataCount);

  for I := 0 to FDataCount - 1 do
  begin
    Stream.PutLongInt(FData[I].Value);
    Stream.PutSingle(FData[I].Probability);
  end;
end;

procedure TIntegerProbabilityList.LoadFromStream(const Stream: TStream);
var
  I, Total: Integer;
begin
  Total := Stream.GetLongInt;

  Request(Total);

  for I := 0 to Total - 1 do
  begin
    FData[I].Value := Stream.GetLongInt;
    FData[I].Probability := Stream.GetSingle;
  end;

  FDataCount := Total;
end;

function TIntegerProbabilityList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}
{$REGION 'TPoints2px'}

constructor TPoints2px.TEnumerator.Create(const AList: TPoints2px);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FList := AList;
  FIndex := -1;
end;

destructor TPoints2px.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TPoints2px.TEnumerator.GetCurrent: PPoint2px;
begin
  Result := FList[FIndex];
end;

function TPoints2px.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;

  if Result then
    Inc(FIndex);
end;

constructor TPoints2px.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TPoints2px.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TPoints2px.GetMemAddr: Pointer;
begin
  if FDataCount > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

function TPoints2px.GetItem(const Index: Integer): PPoint2px;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := @FData[Index]
  else
    Result := nil;
end;

procedure TPoints2px.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FData);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(FData, RoundBy16(NewCapacity));
  end;
end;

procedure TPoints2px.SetCount(const NewCount: Integer);
begin
  if NewCount > 0 then
  begin
    Request(NewCount);
    FDataCount := NewCount;
  end
  else
    Clear;
end;

function TPoints2px.Add(const Point: TPoint2px): Integer;
var
  Index: Integer;
begin
  Index := FDataCount;
  Request(FDataCount + 1);

  FData[Index] := Point;
  Inc(FDataCount);

  Result := Index;
end;

function TPoints2px.IndexOf(const Point: TPoint2px): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if FData[I] = Point then
    begin
      Result := I;
      Break;
    end;
end;

procedure TPoints2px.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FDataCount) then
    Exit;

  for I := Index to FDataCount - 2 do
    FData[I] := FData[I + 1];

  Dec(FDataCount);
end;

procedure TPoints2px.Clear;
begin
  FDataCount := 0;
end;

procedure TPoints2px.Include(const Point: TPoint2px);
begin
  if IndexOf(Point) = -1 then
    Add(Point);
end;

procedure TPoints2px.Exclude(const Point: TPoint2px);
begin
  Remove(IndexOf(Point));
end;

procedure TPoints2px.CopyFrom(const Source: TPoints2px);
var
  I: Integer;
begin
  Request(Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I] := Source.FData[I];

  FDataCount := Source.FDataCount;
end;

procedure TPoints2px.AddFrom(const Source: TPoints2px);
var
  I: Integer;
begin
  Request(FDataCount + Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I + FDataCount] := Source.FData[I];

  Inc(FDataCount, Source.FDataCount);
end;

procedure TPoints2px.AddFromPtr(const Source: PPoint2px; const ElementCount: Integer);
var
  I: Integer;
  SourceValue: PPoint2px;
begin
  Request(FDataCount + ElementCount);

  SourceValue := Source;

  for I := 0 to ElementCount - 1 do
  begin
    FData[I + FDataCount] := SourceValue^;
    Inc(SourceValue);
  end;

  Inc(FDataCount, ElementCount);
end;

function TPoints2px.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}
{$REGION 'TPoints2'}

constructor TPoints2.TEnumerator.Create(const AList: TPoints2);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FList := AList;
  FIndex := -1;
end;

destructor TPoints2.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TPoints2.TEnumerator.GetCurrent: PPoint2;
begin
  Result := FList[FIndex];
end;

function TPoints2.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;

  if Result then
    Inc(FIndex);
end;

constructor TPoints2.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TPoints2.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TPoints2.GetMemAddr: Pointer;
begin
  if FDataCount > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

procedure TPoints2.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FData);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(FData, RoundBy16(NewCapacity));
  end;
end;

function TPoints2.GetItem(const Index: Integer): PPoint2;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := @FData[Index]
  else
    Result := nil;
end;

procedure TPoints2.SetCount(const NewCount: Integer);
begin
  if NewCount > 0 then
  begin
    Request(NewCount);
    FDataCount := NewCount;
  end
  else
    Clear;
end;

function TPoints2.Add(const Point: TPoint2): Integer;
var
  Index: Integer;
begin
  Index := FDataCount;
  Request(FDataCount + 1);

  FData[Index] := Point;
  Inc(FDataCount);

  Result := Index;
end;

procedure TPoints2.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FDataCount) then
    Exit;

  for I := Index to FDataCount - 2 do
    FData[I] := FData[I + 1];

  Dec(FDataCount);
end;

procedure TPoints2.Clear;
begin
  FDataCount := 0;
end;

function TPoints2.IndexOf(const Point: TPoint2): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if FData[I] = Point then
    begin
      Result := I;
      Break;
    end;
end;

procedure TPoints2.Include(const Point: TPoint2);
begin
  if IndexOf(Point) = -1 then
    Add(Point);
end;

procedure TPoints2.Exclude(const Point: TPoint2);
begin
  Remove(IndexOf(Point));
end;

procedure TPoints2.CopyFrom(const Source: TPoints2);
var
  I: Integer;
begin
  Request(Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I] := Source.FData[I];

  FDataCount := Source.FDataCount;
end;

procedure TPoints2.AddFrom(const Source: TPoints2);
var
  I: Integer;
begin
  Request(FDataCount + Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I + FDataCount] := Source.FData[I];

  Inc(FDataCount, Source.FDataCount);
end;

procedure TPoints2.AddFromPtr(const Source: PPoint2; const ElementCount: Integer);
var
  I: Integer;
  SourceValue: PPoint2;
begin
  Request(FDataCount + ElementCount);

  SourceValue := Source;

  for I := 0 to ElementCount - 1 do
  begin
    FData[I + FDataCount] := SourceValue^;
    Inc(SourceValue);
  end;

  Inc(FDataCount, ElementCount);
end;

function TPoints2.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}
{$REGION 'TVectors3'}

constructor TVectors3.TEnumerator.Create(const AList: TVectors3);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FList := AList;
  FIndex := -1;
end;

destructor TVectors3.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TVectors3.TEnumerator.GetCurrent: PVector3;
begin
  Result := FList[FIndex];
end;

function TVectors3.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;

  if Result then
    Inc(FIndex);
end;

constructor TVectors3.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TVectors3.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TVectors3.GetMemAddr: Pointer;
begin
  if FDataCount > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

procedure TVectors3.SetCount(const NewCount: Integer);
begin
  if NewCount > 0 then
  begin
    Request(NewCount);
    FDataCount := NewCount;
  end
  else
    Clear;
end;

function TVectors3.GetItem(const Index: Integer): PVector3;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := @FData[Index]
  else
    Result := nil;
end;

procedure TVectors3.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FData);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(FData, RoundBy16(NewCapacity));
  end;
end;

function TVectors3.Add(const Vector: TVector3): Integer;
var
  Index: Integer;
begin
  Index := FDataCount;
  Request(FDataCount + 1);

  FData[Index] := Vector;
  Inc(FDataCount);

  Result := Index;
end;

procedure TVectors3.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FDataCount) then
    Exit;

  for I := Index to FDataCount - 2 do
    FData[I] := FData[I + 1];

  Dec(FDataCount);
end;

procedure TVectors3.Clear;
begin
  FDataCount := 0;
end;

function TVectors3.IndexOf(const Vector: TVector3): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if FData[I] = Vector then
    begin
      Result := I;
      Break;
    end;
end;

procedure TVectors3.Include(const Vector: TVector3);
begin
  if IndexOf(Vector) = -1 then
    Add(Vector);
end;

procedure TVectors3.Exclude(const Vector: TVector3);
begin
  Remove(IndexOf(Vector));
end;

procedure TVectors3.CopyFrom(const Source: TVectors3);
var
  I: Integer;
begin
  Request(Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I] := Source.FData[I];

  FDataCount := Source.FDataCount;
end;

procedure TVectors3.AddFrom(const Source: TVectors3);
var
  I: Integer;
begin
  Request(FDataCount + Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I + FDataCount] := Source.FData[I];

  Inc(FDataCount, Source.FDataCount);
end;

procedure TVectors3.AddFromPtr(const Source: PVector3; const ElementCount: Integer);
var
  I: Integer;
  SourceValue: PVector3;
begin
  Request(FDataCount + ElementCount);

  SourceValue := Source;

  for I := 0 to ElementCount - 1 do
  begin
    FData[I + FDataCount] := SourceValue^;
    Inc(SourceValue);
  end;

  Inc(FDataCount, ElementCount);
end;

function TVectors3.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}
{$REGION 'TVectors4'}

constructor TVectors4.TEnumerator.Create(const AList: TVectors4);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FList := AList;
  FIndex := -1;
end;

destructor TVectors4.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TVectors4.TEnumerator.GetCurrent: PVector4;
begin
  Result := FList[FIndex];
end;

function TVectors4.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

constructor TVectors4.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
end;

destructor TVectors4.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TVectors4.GetMemAddr: Pointer;
begin
  if FDataCount > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

procedure TVectors4.Request(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FData);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ListGrowIncrement + Capacity + (Capacity div ListGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ListGrowIncrement + NeedCapacity + (NeedCapacity div ListGrowFraction);

    SetLength(FData, RoundBy16(NewCapacity));
  end;
end;

function TVectors4.GetItem(const Index: Integer): PVector4;
begin
  if (Index >= 0) and (Index < FDataCount) then
    Result := @FData[Index]
  else
    Result := nil;
end;

procedure TVectors4.SetCount(const NewCount: Integer);
begin
  if NewCount > 0 then
  begin
    Request(NewCount);
    FDataCount := NewCount;
  end
  else
    Clear;
end;

function TVectors4.Add(const Vector: TVector4): Integer;
var
  Index: Integer;
begin
  Index := FDataCount;
  Request(FDataCount + 1);

  FData[Index] := Vector;
  Inc(FDataCount);

  Result := Index;
end;

procedure TVectors4.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FDataCount) then
    Exit;

  for I := Index to FDataCount - 2 do
    FData[I] := FData[I + 1];

  Dec(FDataCount);
end;

procedure TVectors4.Clear;
begin
  FDataCount := 0;
end;

function TVectors4.IndexOf(const Vector: TVector4): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to FDataCount - 1 do
    if FData[I] = Vector then
    begin
      Result := I;
      Break;
    end;
end;

procedure TVectors4.Include(const Vector: TVector4);
begin
  if IndexOf(Vector) = -1 then
    Add(Vector);
end;

procedure TVectors4.Exclude(const Vector: TVector4);
begin
  Remove(IndexOf(Vector));
end;

procedure TVectors4.CopyFrom(const Source: TVectors4);
var
  I: Integer;
begin
  Request(Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I] := Source.FData[I];

  FDataCount := Source.FDataCount;
end;

procedure TVectors4.AddFrom(const Source: TVectors4);
var
  I: Integer;
begin
  Request(FDataCount + Source.FDataCount);

  for I := 0 to Source.FDataCount - 1 do
    FData[I + FDataCount] := Source.FData[I];

  Inc(FDataCount, Source.FDataCount);
end;

function TVectors4.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}

end.
