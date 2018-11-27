unit PXL.XML;
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
  SysUtils, Classes, PXL.Classes, PXL.TypeDef;

type
  TXMLNode = class
  public type
    PFieldItem = ^TFieldItem;
    TFieldItem = record
      Name : StdString;
      Value: StdString;
    end;

    TEnumerator = class
    private
      {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FNode: TXMLNode;
      Index: Integer;
      function GetCurrent: TXMLNode;
    public
      constructor Create(const ANode: TXMLNode);
      destructor Destroy; override;
      function MoveNext: Boolean;
      property Current: TXMLNode read GetCurrent;
    end;
  private const
    FieldGrowIncrement = 8;
    FieldGrowFraction = 4;
    ChildGrowIncrement = 5;
    ChildGrowFraction = 5;
    GeneratedXMLTabWidth = 2;
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FParent: TXMLNode;
    FName: StdString;

    FieldItems: array of TFieldItem;
    FieldItemCount: Integer;

    FieldSearchList: array of Integer;
    FieldSearchListDirty: Boolean;

    ChildItems: array of TXMLNode;
    ChildItemCount: Integer;

    ChildSearchList: array of Integer;
    ChildSearchListDirty: Boolean;

    function GetFieldCount: Integer;
    function GetField(const Index: Integer): PFieldItem;
    procedure RequestFields(const NeedCapacity: Integer);
    procedure InitFieldSearchList;
    procedure FieldSearchListSwap(const Index1, Index2: Integer);
    function FieldSearchListCompare(const Value1, Value2: Integer): Integer;
    function FieldSearchListSplit(const Start, Stop: Integer): Integer;
    procedure FieldSearchListSort(const Start, Stop: Integer);
    procedure UpdateFieldSearchList;
    function GetFieldValue(const FieldName: StdString): StdString;
    procedure SetFieldValue(const FieldName, Value: StdString);

    function GetChildCount: Integer;
    function GetChild(const Index: Integer): TXMLNode;
    procedure RequestChildren(const NeedCapacity: Integer);
    procedure InitChildSearchList;
    procedure ChildSearchListSwap(const Index1, Index2: Integer);
    function ChildSearchListCompare(const Value1, Value2: Integer): Integer;
    function ChildSearchListSplit(const Start, Stop: Integer): Integer;
    procedure ChildSearchListSort(const Start, Stop: Integer);
    procedure UpdateChildSearchList;
    function GetChildNode(const ChildName: StdString): TXMLNode;

    function GenerateSourceSubCode(const Spacing: Integer = 0): StdString;
  public
    constructor Create(const AName: StdString = ''; const AParent: TXMLNode = nil);
    destructor Destroy; override;

    function AddField(const FieldName, FieldValue: StdString): Integer;
    function IndexOfField(const FieldName: StdString): Integer;

    procedure RemoveField(const Index: Integer);
    procedure ClearFields;

    function AddChildNode(const ChildName: StdString): TXMLNode;
    function AddChild(const ChildName: StdString): Integer;

    function IndexOfChild(const ChildName: StdString): Integer;

    procedure RemoveChild(const Index: Integer);
    procedure ClearChildren;

    function GenerateSourceCode: StdString;

    function SaveToFile(const FileName: StdString): Boolean;
    function SaveToStream(const Stream: TStream): Boolean;

    function GetEnumerator: TEnumerator;

    property Parent: TXMLNode read FParent;
    property Name: StdString read FName;

    property FieldCount: Integer read GetFieldCount;
    property Field[const Index: Integer]: PFieldItem read GetField;
    property FieldValue[const FieldName: StdString]: StdString read GetFieldValue write SetFieldValue;

    property ChildCount: Integer read GetChildCount;
    property Child[const Index: Integer]: TXMLNode read GetChild;
    property ChildNode[const ChildName: StdString]: TXMLNode read GetChildNode;
  end;

function LoadXMLFromText(const Text: StdString): TXMLNode;
function LoadXMLFromStream(const Stream: TStream): TXMLNode;
function LoadXMLFromFile(const FileName: StdString): TXMLNode;
function LoadXMLFromAsset(const AssetName: StdString): TXMLNode;

implementation

{$REGION 'TXMLNode'}

constructor TXMLNode.TEnumerator.Create(const ANode: TXMLNode);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FNode := ANode;
  Index := -1;
end;

destructor TXMLNode.TEnumerator.Destroy;
begin
  Decrement_PXL_ClassInstances;

  inherited;
end;

function TXMLNode.TEnumerator.GetCurrent: TXMLNode;
begin
  Result := FNode.Child[Index];
end;

function TXMLNode.TEnumerator.MoveNext: Boolean;
begin
  Result := Index < FNode.ChildCount - 1;

  if Result then
    Inc(Index);
end;

constructor TXMLNode.Create(const AName: StdString; const AParent: TXMLNode);
begin
  inherited Create;

  Increment_PXL_ClassInstances;

  FName := AName;
  FParent := AParent;
end;

destructor TXMLNode.Destroy;
begin
  try
    ClearChildren;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TXMLNode.GetFieldCount: Integer;
begin
  Result := FieldItemCount;
end;

function TXMLNode.GetField(const Index: Integer): PFieldItem;
begin
  if (Index >= 0) and (Index < FieldItemCount) then
    Result := @FieldItems[Index]
  else
    Result := nil;
end;

procedure TXMLNode.RequestFields(const NeedCapacity: Integer);
var
  NewCapacity, Capacity: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(FieldItems);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := FieldGrowIncrement + Capacity + (Capacity div FieldGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := FieldGrowIncrement + NeedCapacity + (NeedCapacity div FieldGrowFraction);

    SetLength(FieldItems, NewCapacity);
  end;
end;

function TXMLNode.AddField(const FieldName, FieldValue: StdString): Integer;
var
  Index: Integer;
begin
  Index := FieldItemCount;
  RequestFields(FieldItemCount + 1);

  FieldItems[Index].Name := FieldName;
  FieldItems[Index].Value := FieldValue;

  Inc(FieldItemCount);
  FieldSearchListDirty := True;

  Result := Index;
end;

procedure TXMLNode.InitFieldSearchList;
var
  I: Integer;
begin
  if Length(FieldSearchList) <> FieldItemCount then
    SetLength(FieldSearchList, FieldItemCount);

  for I := 0 to FieldItemCount - 1 do
    FieldSearchList[I] := I;
end;

procedure TXMLNode.FieldSearchListSwap(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := FieldSearchList[Index1];
  FieldSearchList[Index1] := FieldSearchList[Index2];
  FieldSearchList[Index2] := TempValue;
end;

function TXMLNode.FieldSearchListCompare(const Value1, Value2: Integer): Integer;
begin
  Result := CompareText(FieldItems[Value1].Name, FieldItems[Value2].Name);
end;

function TXMLNode.FieldSearchListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := FieldSearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (FieldSearchListCompare(FieldSearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (FieldSearchListCompare(FieldSearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      FieldSearchListSwap(Left, Right);
  end;

  FieldSearchListSwap(Start, Right);

  Result := Right;
end;

procedure TXMLNode.FieldSearchListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := FieldSearchListSplit(Start, Stop);

    FieldSearchListSort(Start, SplitPt - 1);
    FieldSearchListSort(SplitPt + 1, Stop);
  end;
end;

procedure TXMLNode.UpdateFieldSearchList;
begin
  InitFieldSearchList;

  if FieldItemCount > 1 then
    FieldSearchListSort(0, FieldItemCount - 1);

  FieldSearchListDirty := False;
end;

function TXMLNode.IndexOfField(const FieldName: StdString): Integer;
var
  Lo, Hi, Mid, Res: Integer;
begin
  if FieldSearchListDirty then
    UpdateFieldSearchList;

  Result := -1;

  Lo := 0;
  Hi := Length(FieldSearchList) - 1;

  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    Res := CompareText(FieldItems[FieldSearchList[Mid]].Name, FieldName);

    if Res = 0 then
    begin
      Result := FieldSearchList[Mid];
      Break;
    end;

    if Res > 0 then
      Hi := Mid - 1
    else
      Lo := Mid + 1;
  end;
end;

function TXMLNode.GetFieldValue(const FieldName: StdString): StdString;
var
  Index: Integer;
begin
  Index := IndexOfField(FieldName);

  if Index <> -1 then
    Result := FieldItems[Index].Value
  else
    Result := '';
end;

procedure TXMLNode.SetFieldValue(const FieldName, Value: StdString);
var
  Index: Integer;
begin
  Index := IndexOfField(FieldName);

  if Index <> -1 then
    FieldItems[Index].Value := Value
  else
    AddField(FieldName, Value);
end;

procedure TXMLNode.RemoveField(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= FieldItemCount) then
    Exit;

  for I := Index to FieldItemCount - 2 do
    FieldItems[I] := FieldItems[I + 1];

  Dec(FieldItemCount);
  FieldSearchListDirty := True;
end;

procedure TXMLNode.ClearFields;
begin
  if FieldItemCount > 0 then
  begin
    FieldItemCount := 0;
    FieldSearchListDirty := True;
  end;
end;

function TXMLNode.GetChildCount: Integer;
begin
  Result := ChildItemCount;
end;

function TXMLNode.GetChild(const Index: Integer): TXMLNode;
begin
  if (Index >= 0) and (Index < ChildItemCount) then
    Result := ChildItems[Index]
  else
    Result := nil;
end;

procedure TXMLNode.RequestChildren(const NeedCapacity: Integer);
var
  NewCapacity, Capacity, I: Integer;
begin
  if NeedCapacity < 1 then
    Exit;

  Capacity := Length(ChildItems);

  if Capacity < NeedCapacity then
  begin
    NewCapacity := ChildGrowIncrement + Capacity + (Capacity div ChildGrowFraction);

    if NewCapacity < NeedCapacity then
      NewCapacity := ChildGrowIncrement + NeedCapacity + (NeedCapacity div ChildGrowFraction);

    SetLength(ChildItems, NewCapacity);

    for I := Capacity to NewCapacity - 1 do
      ChildItems[I] := nil;
  end;
end;

function TXMLNode.AddChild(const ChildName: StdString): Integer;
var
  Index: Integer;
begin
  Index := ChildItemCount;
  RequestChildren(ChildItemCount + 1);

  ChildItems[Index] := TXMLNode.Create(ChildName, Self);

  Inc(ChildItemCount);
  ChildSearchListDirty := True;

  Result := Index;
end;

function TXMLNode.AddChildNode(const ChildName: StdString): TXMLNode;
var
  Index: Integer;
begin
  Index := AddChild(ChildName);

  if Index <> -1 then
    Result := ChildItems[Index]
  else
    Result := nil;
end;

procedure TXMLNode.InitChildSearchList;
var
  I: Integer;
begin
  if Length(ChildSearchList) <> ChildItemCount then
    SetLength(ChildSearchList, ChildItemCount);

  for I := 0 to ChildItemCount - 1 do
    ChildSearchList[I] := I;
end;

procedure TXMLNode.ChildSearchListSwap(const Index1, Index2: Integer);
var
  TempValue: Integer;
begin
  TempValue := ChildSearchList[Index1];
  ChildSearchList[Index1] := ChildSearchList[Index2];
  ChildSearchList[Index2] := TempValue;
end;

function TXMLNode.ChildSearchListCompare(const Value1, Value2: Integer): Integer;
begin
  Result := CompareText(ChildItems[Value1].Name, ChildItems[Value2].Name);
end;

function TXMLNode.ChildSearchListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := ChildSearchList[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (ChildSearchListCompare(ChildSearchList[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (ChildSearchListCompare(ChildSearchList[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      ChildSearchListSwap(Left, Right);
  end;

  ChildSearchListSwap(Start, Right);

  Result := Right;
end;

procedure TXMLNode.ChildSearchListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := ChildSearchListSplit(Start, Stop);

    ChildSearchListSort(Start, SplitPt - 1);
    ChildSearchListSort(SplitPt + 1, Stop);
  end;
end;

procedure TXMLNode.UpdateChildSearchList;
begin
  InitChildSearchList;

  if ChildItemCount > 1 then
    ChildSearchListSort(0, ChildItemCount - 1);

  ChildSearchListDirty := False;
end;

function TXMLNode.IndexOfChild(const ChildName: StdString): Integer;
var
  Lo, Hi, Mid, Res: Integer;
begin
  if ChildSearchListDirty then
    UpdateChildSearchList;

  Result := -1;

  Lo := 0;
  Hi := Length(ChildSearchList) - 1;

  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    Res := CompareText(ChildItems[ChildSearchList[Mid]].Name, ChildName);

    if Res = 0 then
    begin
      Result := ChildSearchList[Mid];
      Break;
    end;

    if Res > 0 then
      Hi := Mid - 1
    else
      Lo := Mid + 1;
  end;
end;

function TXMLNode.GetChildNode(const ChildName: StdString): TXMLNode;
var
  Index: Integer;
begin
  Index := IndexOfChild(ChildName);

  if Index <> -1 then
    Result := ChildItems[Index]
  else
    Result := nil;
end;

procedure TXMLNode.RemoveChild(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= ChildItemCount) then
    Exit;

  ChildItems[Index].Free;

  for I := Index to ChildItemCount - 2 do
    ChildItems[I] := ChildItems[I + 1];

  Dec(ChildItemCount);
  ChildSearchListDirty := True;
end;

procedure TXMLNode.ClearChildren;
var
  I: Integer;
begin
  if ChildItemCount > 0 then
  begin
    for I := ChildItemCount - 1 downto 0 do
      ChildItems[I].Free;

    ChildItemCount := 0;
    ChildSearchListDirty := True;
  end;
end;

function TXMLNode.GenerateSourceSubCode(const Spacing: Integer): StdString;

  function AddSpaces(const Count: Integer): StdString;
  var
    I: Integer;
  begin
    SetLength(Result, Count);

    for I := 0 to Count - 1 do
      Result[1 + I] := ' ';
  end;

var
  I: Integer;
begin
  Result := AddSpaces(Spacing) + '<' + FName;

  if FieldItemCount > 0 then
  begin
    Result := Result + ' ';

    for I := 0 to FieldItemCount - 1 do
    begin
      Result := Result + FieldItems[I].Name + '="' + FieldItems[I].Value + '"';

      if I < FieldItemCount - 1 then
        Result := Result + ' ';
    end;
  end;

  if ChildItemCount > 0 then
  begin
    Result := Result + '>'#13#10;

    for I := 0 to ChildItemCount - 1 do
      Result := Result + ChildItems[I].GenerateSourceSubCode(Spacing + GeneratedXMLTabWidth);

    Result := Result + AddSpaces(Spacing) + '</' + FName + '>'#13#10;
  end
  else
    Result := Result + ' />'#13#10;
end;

function TXMLNode.GenerateSourceCode: StdString;
begin
  Result := GenerateSourceSubCode;
end;

function TXMLNode.SaveToFile(const FileName: StdString): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
    try
      Result := SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Exit(False);
  end;
end;

function TXMLNode.SaveToStream(const Stream: TStream): Boolean;
var
  StStream: TStringStream;
begin
  try
    StStream := TStringStream.Create(GenerateSourceCode);
    try
      Stream.CopyFrom(StStream, 0);
      Result := True;
    finally
      StStream.Free;
    end;
  except
    Exit(False);
  end;
end;

function TXMLNode.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}
{$REGION 'XML Parser'}

const
  XWhiteSpace = [#0, #9, #10, #13, #32];
  XQuotes = ['"', ''''];
  XSeparators = ['+', '-', '=', '<', '>', '(', ')', '[', ']', '"', '''', ',', '.', '/', '\', ':', ';', '*', '#', '&',
    '@', '$', '%', '^', '?', '!'];
  XParsingOkay = 0;
  XIncompleteAttribute = -1;
  XInvalidNodeDeclaration = -2;
  XInvalidDocumentSymbol = -3;
  XInvalidClosingTagName = -4;
  XInvalidClosingTagSyntax = -5;
  XNodeHasNotBeenClosed = -6;

var
  CurrentParseStatus: Integer = 0;

function IsNameCharacter(const ScanCh: Char): Boolean;
begin
  Result := (not (ScanCh in XWhiteSpace)) and (not (ScanCh in XSeparators));
end;

procedure SkipBlankSpace(const Text: StdString; var TextPos: Integer);
begin
  while (TextPos <= Length(Text)) and (Text[TextPos] in XWhiteSpace) do
    Inc(TextPos);
end;

function HasTextPortion(const Text: StdString; const TextPos: Integer; const SubText: StdString): Boolean;
var
  TestText: StdString;
begin
  if 1 + Length(Text) - TextPos < Length(SubText) then
    Exit(False);

  TestText := Copy(Text, TextPos, Length(SubText));
  Result := SameText(TestText, SubText);
end;

procedure ScanAfterPortion(const Text: StdString; var TextPos: Integer; const SubText: StdString);
begin
  while TextPos <= Length(Text) do
  begin
    if HasTextPortion(Text, TextPos, SubText) then
    begin
      Inc(TextPos, Length(SubText));
      Break;
    end;

    Inc(TextPos);
  end;
end;

procedure ScanAfterChar(const Text: StdString; var TextPos: Integer; const ScanCh: Char);
begin
  while TextPos <= Length(Text) do
  begin
    if Text[TextPos] = ScanCh then
    begin
      Inc(TextPos);
      Break;
    end;

    Inc(TextPos);
  end;
end;

function ScanForName(const Text: StdString; var TextPos: Integer): StdString;
var
  StartPos, CopyLen: Integer;
begin
  Result := '';

  SkipBlankSpace(Text, TextPos);

  if (TextPos > Length(Text)) or (Text[TextPos] in XSeparators) then
    Exit;

  StartPos := TextPos;
  CopyLen := 0;

  while TextPos <= Length(Text) do
  begin
    if not IsNameCharacter(Text[TextPos]) then
      Break;

    Inc(CopyLen);
    Inc(TextPos);
  end;

  Result := Copy(Text, StartPos, CopyLen);
end;

function ScanForTextValue(const Text: StdString; var TextPos: Integer): StdString;
var
  StartPos, CopyLen: Integer;
  QuoteCh: Char;
begin
  Result := '';

  SkipBlankSpace(Text, TextPos);

  if (TextPos > Length(Text)) or ((Text[TextPos] in XSeparators) and (not (Text[TextPos] in XQuotes))) then
    Exit;

  // Opening quote?
  if Text[TextPos] in XQuotes then
  begin
    QuoteCh := Text[TextPos];
    Inc(TextPos);
  end
  else
    QuoteCh := #0;

  StartPos := TextPos;
  CopyLen := 0;

  while TextPos <= Length(Text) do
  begin
    // Closing quote.
    if (QuoteCh <> #0) and (Text[TextPos] = QuoteCh) then
    begin
      Inc(TextPos);
      Break;
    end;

    if (QuoteCh = #0) and (not IsNameCharacter(Text[TextPos])) then
      Break;

    Inc(CopyLen);
    Inc(TextPos);
  end;

  Result := Copy(Text, StartPos, CopyLen);
end;

procedure ParseXMLField(const Node: TXMLNode; const Text: StdString; var TextPos: Integer);
var
  FieldName, FieldValue: StdString;
begin
  FieldName := ScanForName(Text, TextPos);

  // Skip any blank space.
  SkipBlankSpace(Text, TextPos);

  // Abrupt end of text?
  if TextPos > Length(Text) then
  begin
    if Length(FieldName) > 0 then
      Node.AddField(FieldName, '');

    CurrentParseStatus := XIncompleteAttribute;
    Exit;
  end;

  // Field has no value.
  if Text[TextPos] <> '=' then
  begin
    if Length(FieldName) > 0 then
      Node.AddField(FieldName, '');

    Exit;
  end;

  // Parse field value (skip "=" symbol).
  Inc(TextPos);
  FieldValue := ScanForTextValue(Text, TextPos);

  if Length(FieldName) > 0 then
    Node.AddField(FieldName, FieldValue);
end;

function ParseXMLNode(const Root: TXMLNode; const Text: StdString; var TextPos: Integer): TXMLNode;
var
  NodeName: StdString;
begin
  // Process node name.
  NodeName := ScanForName(Text, TextPos);

  if Root <> nil then
    Result := Root.AddChildNode(NodeName)
  else
    Result := TXMLNode.Create(NodeName);

  // Processing after [<NODE]...
  while TextPos <= Length(Text) do
  begin
    // Skip any blank space.
    SkipBlankSpace(Text, TextPos);
    if TextPos > Length(Text) then
      Break;

    // Skip "<!-- ... -->" comments inside node (is this allowed?)
    if HasTextPortion(Text, TextPos, '<!--') then
    begin
      Inc(TextPos, 4);
      ScanAfterPortion(Text, TextPos, '-->');
      Continue;
    end;

    // Full end of node.
    if HasTextPortion(Text, TextPos, '/>') then
    begin
      Inc(TextPos, 2);
      Exit;
    end;

    // End of node, need to parse the second part.
    if Text[TextPos] = '>' then
    begin
      Inc(TextPos);
      Break;
    end;

    // Attribute.
    if IsNameCharacter(Text[TextPos]) then
    begin
      ParseXMLField(Result, Text, TextPos);
      Continue;
    end;

    CurrentParseStatus := XInvalidNodeDeclaration;
    Exit;
  end;

  // Processing after [<NODE>]...
  while TextPos <= Length(Text) do
  begin
    // Skip any blank space.
    SkipBlankSpace(Text, TextPos);
    if TextPos > Length(Text) then
      Break;

    // Skip "<!-- ... -->" comments.
    if HasTextPortion(Text, TextPos, '<!--') then
    begin
      Inc(TextPos, 4);
      ScanAfterPortion(Text, TextPos, '-->');
      Continue;
    end;

    // Skip "<? ... ?>" tags.
    if HasTextPortion(Text, TextPos, '<?') then
    begin
      Inc(TextPos, 2);
      ScanAfterPortion(Text, TextPos, '?>');
      Continue;
    end;

    // Skip "<!doctype >" tags.
    if HasTextPortion(Text, TextPos, '<!doctype') then
    begin
      Inc(TextPos, 9);
      ScanAfterChar(Text, TextPos, '>');
      Continue;
    end;

    // End of node "</NODE>"
    if HasTextPortion(Text, TextPos, '</') then
    begin
      Inc(TextPos, 2);

      NodeName := ScanForName(Text, TextPos);
      if not SameText(NodeName, Result.Name) then
      begin
        CurrentParseStatus := XInvalidClosingTagName;
        Exit;
      end;

      SkipBlankSpace(Text, TextPos);
      if (TextPos > Length(Text)) or (Text[TextPos] <> '>') then
      begin
        CurrentParseStatus := XInvalidClosingTagSyntax;
        Exit;
      end;

      Inc(TextPos);
      Exit;
    end;

    // Start of child node.
    if Text[TextPos] = '<' then
    begin
      Inc(TextPos);

      ParseXMLNode(Result, Text, TextPos);
      Continue;
    end;

    // Skip text inside the node.
    Inc(TextPos);
  end;

  if TextPos > Length(Text) then
    CurrentParseStatus := XNodeHasNotBeenClosed;
end;

function LoadXMLFromText(const Text: StdString): TXMLNode;
var
  TextPos: Integer;
begin
  Result := nil;
  if Length(Text) < 1 then
    Exit;

  TextPos := 1;
  CurrentParseStatus := XParsingOkay;

  while TextPos <= Length(Text) do
  begin
    // Skip any blank space.
    SkipBlankSpace(Text, TextPos);
    if TextPos > Length(Text) then
      Break;

    // Skip "<!-- ... -->" comments.
    if HasTextPortion(Text, TextPos, '<!--') then
    begin
      Inc(TextPos, 4);
      ScanAfterPortion(Text, TextPos, '-->');
      Continue;
    end;

    // Skip "<? ... ?>" tags.
    if HasTextPortion(Text, TextPos, '<?') then
    begin
      Inc(TextPos, 2);
      ScanAfterPortion(Text, TextPos, '?>');
      Continue;
    end;

    // Skip "<!doctype >" tags.
    if HasTextPortion(Text, TextPos, '<!doctype') then
    begin
      Inc(TextPos, 9);
      ScanAfterChar(Text, TextPos, '>');
      Continue;
    end;

    // Start of node.
    if Text[TextPos] = '<' then
    begin
      Inc(TextPos);

      Result := ParseXMLNode(nil, Text, TextPos);
      Break;
    end;

    // Invalid text character.
    CurrentParseStatus := XInvalidDocumentSymbol;
    Break;
  end;

  if CurrentParseStatus <> XParsingOkay then
    if Result <> nil then
      FreeAndNil(Result);
end;

{$ENDREGION}
{$REGION 'XML Loading Functions'}

function LoadXMLFromStream(const Stream: TStream): TXMLNode;
var
  StStream: TStringStream;
begin
  try
    StStream := TStringStream.Create('');
    try
      StStream.CopyFrom(Stream, 0);
      Result := LoadXMLFromText(StStream.DataString);
    finally
      StStream.Free;
    end;
  except
    Exit(nil);
  end;
end;

function LoadXMLFromFile(const FileName: StdString): TXMLNode;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadXMLFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Exit(nil);
  end;
end;

function LoadXMLFromAsset(const AssetName: StdString): TXMLNode;
var
  Stream: TAssetStream;
begin
  try
    Stream := TAssetStream.Create(AssetName);
    try
      Result := LoadXMLFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Exit(nil);
  end;
end;

{$ENDREGION}

end.
