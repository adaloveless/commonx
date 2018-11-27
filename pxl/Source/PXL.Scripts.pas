unit PXL.Scripts;
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

type
  TScriptObject = class
  public const
    UnknownID = High(Cardinal);
  public type
    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FOwner: TScriptObject;
      FCurrent: TScriptObject;

      function GetCurrent: TScriptObject;
    public
      constructor Create(const Owner: TScriptObject);
      destructor Destroy; override;

      function MoveNext: Boolean;

      property Current: TScriptObject read GetCurrent;
    end;

    TScriptID = Cardinal;
  private const
    ListGrowIncrement = 8;
    ListGrowFraction = 4;
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FOwner: TScriptObject;

    FNext: TScriptObject;
    FPrev: TScriptObject;

    FID: TScriptID;

    CurrentID: TScriptID;
    FObjDisposed: Boolean;

    FFirstNode: TScriptObject;
    FLastNode: TScriptObject;

    SearchList: array of Pointer;
    DrawList: array of Pointer;

    SearchCount: Integer;
    SearchDirty: Boolean;

    DrawCount: Integer;
    DrawDirty: Boolean;
    FDrawOrder: Integer;

    function GenerateID: TScriptID;
    procedure Include(const Node: TScriptObject);
    procedure Exclude(const Node: TScriptObject);
    procedure SetOwner(const Value: TScriptObject);

    procedure SetDrawOrder(const Value: Integer);

    procedure GrowSearchList;
    procedure InitSearchList;
    procedure SearchListSwap(const Index1, Index2: Integer);
    function SearchListCompare(const Node1, Node2: Pointer): Integer;
    function SearchListSplit(const Start, Stop: Integer): Integer;
    procedure SearchListSort(const Start, Stop: Integer);
    procedure UpdateSearchList;

    procedure GrowDrawList;
    procedure InitDrawList;
    procedure DrawListSwap(const Index1, Index2: Integer);
    function DrawListCompare(const Node1, Node2: Pointer): Integer;
    function DrawListSplit(const Start, Stop: Integer): Integer;
    procedure DrawListSort(const Start, Stop: Integer);
    procedure UpdateDrawList;

    function GetNode(const ID: TScriptID): TScriptObject;
    function GetNodeCount: Integer;
    function GetDrawItem(const Index: Integer): TScriptObject;
    function GetDrawNodeCount: Integer;
  protected
    procedure DoUpdate; virtual;
    procedure DoDraw; virtual;
  public
    constructor Create(const AOwner: TScriptObject = nil);
    destructor Destroy; override;

    function DrawSorted: Integer;
    function DrawUnsorted: Integer;

    function Update: Integer;

    procedure ObjDispose;
    procedure Clear;

    function ComputeTotalNodeCount: Integer;

    function GetEnumerator: TEnumerator;

    property Owner: TScriptObject read FOwner write SetOwner;

    property Prev: TScriptObject read FPrev write FPrev;
    property Next: TScriptObject read FNext write FNext;

    property ID: TScriptID read FID;

    property FirstNode: TScriptObject read FFirstNode;
    property LastNode: TScriptObject read FLastNode;

    property ObjDisposed: Boolean read FObjDisposed;
    property DrawOrder: Integer read FDrawOrder write SetDrawOrder;

    property NodeCount: Integer read GetNodeCount;
    property Node[const ID: TScriptID]: TScriptObject read GetNode; default;

    property DrawItemCount: Integer read GetDrawNodeCount;
    property DrawItem[const Index: Integer]: TScriptObject read GetDrawItem;
  private
    class var FInstanceCount: Integer;
  public
    class property InstanceCount: Integer read FInstanceCount;
  end;

implementation

uses
  SysUtils;

constructor TScriptObject.TEnumerator.Create(const Owner: TScriptObject);
begin
  inherited Create;

  Inc(FInstanceCount);

  FOwner := Owner;
  FCurrent := nil;
end;

destructor TScriptObject.TEnumerator.Destroy;
begin
  Dec(FInstanceCount);

  inherited;
end;

function TScriptObject.TEnumerator.GetCurrent: TScriptObject;
begin
  Result := FCurrent;
end;

function TScriptObject.TEnumerator.MoveNext: Boolean;
begin
  if FCurrent = nil then
    FCurrent := FOwner.FirstNode
  else
    FCurrent := FCurrent.Next;

  Result := FCurrent <> nil;
end;

constructor TScriptObject.Create(const AOwner: TScriptObject);
begin
  inherited Create;

  Inc(FInstanceCount);

  FPrev := nil;
  FNext := nil;
  FOwner := AOwner;

  FFirstNode := nil;
  FLastNode := nil;
  FObjDisposed := False;

  SearchDirty := False;
  SearchCount := 0;

  DrawDirty := False;
  DrawCount := 0;

  CurrentID := 0;

  if FOwner <> nil then
  begin
    FID := FOwner.GenerateID;
    FOwner.Include(Self);
  end
  else
    FID := UnknownID;
end;

destructor TScriptObject.Destroy;
begin
  Clear;
  Dec(FInstanceCount);

  inherited;
end;

function TScriptObject.GenerateID: TScriptID;
begin
  if CurrentID = High(Cardinal) then
    CurrentID := 0;

  Result := CurrentID;
  Inc(CurrentID);
end;

procedure TScriptObject.Include(const Node: TScriptObject);
begin
  SearchDirty := True;
  DrawDirty := True;

  if FFirstNode = nil then
  begin
    FFirstNode := Node;
    FLastNode := Node;

    Node.Prev := nil;
    Node.Next := nil;
  end
  else
  begin
    Node.Next := FFirstNode;
    Node.Prev := nil;

    FFirstNode.Prev := Node;
    FFirstNode := Node;
  end;
end;

procedure TScriptObject.Exclude(const Node: TScriptObject);
begin
  SearchDirty := True;
  DrawDirty := True;

  if Node.Prev = nil then
    FFirstNode := Node.Next
  else
    Node.Prev.Next := Node.Next;

  if Node.Next = nil then
    FLastNode := Node.Prev
  else
    Node.Next.Prev := Node.Prev;

  Node.Prev := nil;
  Node.Next := nil;
end;

procedure TScriptObject.SetOwner(const Value: TScriptObject);
begin
  if FOwner <> Value then
  begin
    if FOwner <> nil then
      FOwner.Exclude(Self);

    FOwner := Value;

    if FOwner <> nil then
    begin
      FID := FOwner.GenerateID;
      FOwner.Include(Self);
    end
    else
      FID := UnknownID;
  end;
end;

procedure TScriptObject.ObjDispose;
begin
  FObjDisposed := True;
end;

procedure TScriptObject.Clear;
var
  Node, Temp: TScriptObject;
begin
  Node := FFirstNode;
  while Node <> nil do
  begin
    Temp := Node;
    Node := Node.Next;

    Exclude(Temp);
    Temp.FOwner := nil;

    FreeAndNil(Temp);
  end;

  FFirstNode := nil;
  FLastNode := nil;

  SearchDirty := False;
  SearchCount := 0;
  DrawDirty := False;
  DrawCount := 0;
end;

procedure TScriptObject.SetDrawOrder(const Value: Integer);
begin
  FDrawOrder := Value;

  if FOwner <> nil then
    FOwner.DrawDirty := True;
end;

procedure TScriptObject.GrowSearchList;
begin
  SetLength(SearchList, Length(SearchList) + ListGrowIncrement + (Length(SearchList) div ListGrowFraction));
end;

procedure TScriptObject.InitSearchList;
var
  Index: Integer;
  TempNode: Pointer;
begin
  Index := 0;
  TempNode := FFirstNode;

  while TempNode <> nil do
  begin
    if Length(SearchList) <= Index then
      GrowSearchList;

    SearchList[Index] := TempNode;
    Inc(Index);

    TempNode := TScriptObject(TempNode).Next;
  end;

  SearchCount := Index;
end;

procedure TScriptObject.SearchListSwap(const Index1, Index2: Integer);
var
  TempValue: Pointer;
begin
  TempValue := SearchList[Index1];
  SearchList[Index1] := SearchList[Index2];
  SearchList[Index2] := TempValue;
end;

function TScriptObject.SearchListCompare(const Node1, Node2: Pointer): Integer;
begin
  if TScriptObject(Node1).ID < TScriptObject(Node2).ID then
    Result := -1
  else if TScriptObject(Node1).ID > TScriptObject(Node2).ID then
    Result := 1
  else
    Result := 0;
end;

function TScriptObject.SearchListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right: Integer;
  Pivot: Pointer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := SearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (SearchListCompare(SearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (SearchListCompare(SearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      SearchListSwap(Left, Right);
  end;

  SearchListSwap(Start, Right);
  Result := Right;
end;

procedure TScriptObject.SearchListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := SearchListSplit(Start, Stop);

    SearchListSort(Start, SplitPt - 1);
    SearchListSort(SplitPt + 1, Stop);
  end;
end;

procedure TScriptObject.UpdateSearchList;
begin
  InitSearchList;

  if SearchCount > 1 then
    SearchListSort(0, SearchCount - 1);

  SearchDirty := False;
end;

function TScriptObject.GetNode(const ID: TScriptID): TScriptObject;
var
  Left, Right, Pivot: Integer;
begin
  if SearchDirty then
    UpdateSearchList;

  Left := 0;
  Right := SearchCount - 1;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;

    if TScriptObject(SearchList[Pivot]).ID = ID then
      Exit(TScriptObject(SearchList[Pivot]));

    if TScriptObject(SearchList[Pivot]).ID > ID then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := nil;
end;

procedure TScriptObject.GrowDrawList;
begin
  SetLength(DrawList, Length(DrawList) + ListGrowIncrement + (Length(DrawList) div ListGrowFraction));
end;

procedure TScriptObject.InitDrawList;
var
  Index: Integer;
  TempNode: Pointer;
begin
  Index := 0;
  TempNode := FFirstNode;

  while TempNode <> nil do
  begin
    if Length(DrawList) <= Index then
      GrowDrawList;

    DrawList[Index] := TempNode;
    Inc(Index);

    TempNode := TScriptObject(TempNode).Next;
  end;

  DrawCount := Index;
end;

procedure TScriptObject.DrawListSwap(const Index1, Index2: Integer);
var
  TempValue: Pointer;
begin
  TempValue := DrawList[Index1];
  DrawList[Index1] := DrawList[Index2];
  DrawList[Index2] := TempValue;
end;

function TScriptObject.DrawListCompare(const Node1, Node2: Pointer): Integer;
begin
  if TScriptObject(Node1).DrawOrder < TScriptObject(Node2).DrawOrder then
    Result := -1
  else if TScriptObject(Node1).DrawOrder > TScriptObject(Node2).DrawOrder then
    Result := 1
  else if TScriptObject(Node1).ID < TScriptObject(Node2).ID then
    Result := -1
  else if TScriptObject(Node1).ID > TScriptObject(Node2).ID then
    Result := 1
  else
    Result := 0;
end;

function TScriptObject.DrawListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right: Integer;
  Pivot: Pointer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := DrawList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (DrawListCompare(DrawList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (DrawListCompare(DrawList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      DrawListSwap(Left, Right);
  end;

  DrawListSwap(Start, Right);
  Result := Right;
end;

procedure TScriptObject.DrawListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := DrawListSplit(Start, Stop);

    DrawListSort(Start, SplitPt - 1);
    DrawListSort(SplitPt + 1, Stop);
  end;
end;

procedure TScriptObject.UpdateDrawList;
begin
  InitDrawList;

  if DrawCount > 1 then
    DrawListSort(0, DrawCount - 1);

  DrawDirty := False;
end;

function TScriptObject.GetNodeCount: Integer;
begin
  if SearchDirty then
    UpdateSearchList;

  Result := SearchCount;
end;

procedure TScriptObject.DoUpdate;
begin
end;

procedure TScriptObject.DoDraw;
begin
end;

function TScriptObject.DrawSorted: Integer;
var
  I: Integer;
begin
  Result := 1;

  DoDraw;

  if DrawDirty then
    UpdateDrawList;

  for I := 0 to DrawCount - 1 do
    Inc(Result, TScriptObject(DrawList[I]).DrawSorted);
end;

function TScriptObject.DrawUnsorted: Integer;
var
  TempNode: Pointer;
begin
  Result := 1;

  DoDraw;

  TempNode := FFirstNode;

  while TempNode <> nil do
  begin
    Inc(Result, TScriptObject(TempNode).DrawUnsorted);
    TempNode := TScriptObject(TempNode).Next;
  end;
end;

function TScriptObject.Update: Integer;
var
  TempNode: Pointer;
  NodeToDiscard: TScriptObject;
begin
  Result := 1;

  DoUpdate;

  // Update non-discarded nodes.
  TempNode := FFirstNode;

  while TempNode <> nil do
  begin
    if not TScriptObject(TempNode).FObjDisposed then
      Inc(Result, TScriptObject(TempNode).Update);

    TempNode := TScriptObject(TempNode).Next;
  end;

  // Release discarded nodes.
  TempNode := FFirstNode;

  while TempNode <> nil do
  begin
    if TScriptObject(TempNode).FObjDisposed then
    begin
      NodeToDiscard := TempNode;
      TempNode := TScriptObject(TempNode).Next;

      Exclude(NodeToDiscard);
      NodeToDiscard.FOwner := nil;

      FreeAndNil(NodeToDiscard);
    end
    else
      TempNode := TScriptObject(TempNode).Next;
  end;
end;

function TScriptObject.ComputeTotalNodeCount: Integer;
var
  TempNode: Pointer;
begin
  Result := 1;
  TempNode := FFirstNode;

  while TempNode <> nil do
  begin
    Inc(Result, TScriptObject(TempNode).ComputeTotalNodeCount);
    TempNode := TScriptObject(TempNode).Next;
  end;
end;

function TScriptObject.GetDrawNodeCount: Integer;
begin
  if DrawDirty then
    UpdateDrawList;

  Result := DrawCount;
end;

function TScriptObject.GetDrawItem(const Index: Integer): TScriptObject;
begin
  if DrawDirty then
    UpdateDrawList;

  if (Index >= 0) and (Index < DrawCount) then
    Result := TScriptObject(DrawList[Index])
  else
    Result := nil;
end;

function TScriptObject.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

initialization
  TScriptObject.FInstanceCount := 0;

end.
