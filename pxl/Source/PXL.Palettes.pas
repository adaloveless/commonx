unit PXL.Palettes;
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
  SysUtils, Classes, Math, PXL.TypeDef, PXL.Types;

type
  TFloatNodeType = (Plain, Sine, Accel, Brake);

  PFloatColorNode = ^TFloatColorNode;
  TFloatColorNode = packed record
    Color: TFloatColor;
    NodeType: TFloatNodeType;
    Theta: VectorFloat;
  end;

  TFloatPalette = class
  private
    FData: array of TFloatColorNode;
    FTime: VectorFloat;
    FName: StdString;

    function GetCount: Integer;
    function GetItem(const Index: Integer): PFloatColorNode;
    function GetFirstColor(const Theta: VectorFloat): PFloatColorNode;
    function GetColor(const Theta: VectorFloat): TFloatColor;
    function GetNextColor(const Theta: VectorFloat): PFloatColorNode;
    procedure SetTime(const Value: VectorFloat);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Color: TFloatColor; const NodeType: TFloatNodeType; const Theta: VectorFloat): Integer; overload;
    function Add(const Node: TFloatColorNode): Integer; overload; inline;
    function Add(const Diffuse: TIntColor; const Theta: VectorFloat): Integer; overload; inline;

    procedure Remove(const Index: Integer);
    procedure Clear;

    procedure SaveToStream(const Stream: TStream);
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToFile(const FileName: StdString);
    procedure LoadFromFile(const FileName: StdString);

    procedure Assign(const Source: TFloatPalette);

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: PFloatColorNode read GetItem; default;
    property Color[const Theta: VectorFloat]: TFloatColor read GetColor;
    property Time: VectorFloat read FTime write SetTime;
    property Name: StdString read FName write FName;
  end;

  TFloatPalettes = class
  private
    FData: array of TFloatPalette;
    FTitle: StdString;

    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    function GetItem(const Index: Integer): TFloatPalette;
    procedure SetItem(const Index: Integer; const Value: TFloatPalette);
    function GetColor(const Theta, Time: VectorFloat): TFloatColor;
    function GetFirstPal(const Time: VectorFloat): TFloatPalette;
    function GetPrevPal(const Time: VectorFloat): TFloatPalette;
    function GetSuccPal(const Time: VectorFloat): TFloatPalette;
  public
    constructor Create;
    destructor Destroy; override;

    function Add: Integer; overload;
    procedure Add(const Color1, Color2, Color3, Color4: TIntColor); overload;
    procedure Remove(const Index: Integer);
    function IndexOf(const Name: StdString): Integer;

    procedure Clear;
    procedure Assign(const Source: TFloatPalettes);

    procedure SaveToStream(const Stream: TStream);
    procedure LoadFromStream(const Stream: TStream);
    function LoadFromFile(const FileName: StdString): Boolean;
    function SaveToFile(const FileName: StdString): Boolean;

    property Count: Integer read GetCount write SetCount;
    property Items[const Index: Integer]: TFloatPalette read GetItem write SetItem; default;
    property Color[const Theta, Time: VectorFloat]: TFloatColor read GetColor;
    property Title: StdString read FTitle write FTitle;
  end;

  TFloatPaletteSet = class
  private
    FData: array of TFloatPalettes;

    function GetCount: Integer;
    function GetItem(const Index: Integer): TFloatPalettes;
    procedure SetItem(const Index: Integer; const Value: TFloatPalettes);
  public
    constructor Create;
    destructor Destroy; override;

    function Add: Integer;
    procedure Remove(const Index: Integer);
    procedure Clear;
    function IndexOf(const Title: StdString): Integer;

    procedure SaveToStream(const Stream: TStream);
    procedure LoadFromStream(const Stream: TStream);
    function LoadFromFile(const FileName: StdString): Boolean;
    function SaveToFile(const FileName: StdString): Boolean;

    property Count: Integer read GetCount;
    property Item[const Index: Integer]: TFloatPalettes read GetItem write SetItem; default;
  end;

implementation

uses
  PXL.Classes;

constructor TFloatPalette.Create;
begin
  inherited;

  SetLength(FData, 0);
  FTime := 0.0;
  FName := '';
end;

destructor TFloatPalette.Destroy;
begin
  Clear;

  inherited;
end;

function TFloatPalette.GetCount: Integer;
begin
  Result := Length(FData);
end;

function TFloatPalette.GetItem(const Index: Integer): PFloatColorNode;
begin
  if (Index < 0) or (Index >= Length(FData)) then
    Exit(nil);

  Result := @FData[Index];
end;


function TFloatPalette.Add(const Color: TFloatColor; const NodeType: TFloatNodeType; const Theta: VectorFloat): Integer;
var
  Index: Integer;
begin
  Index := Length(FData);
  SetLength(FData, Index + 1);

  FData[Index].Color := Color;
  FData[Index].NodeType := NodeType;
  FData[Index].Theta := Theta;

  Result := Index;
end;

function TFloatPalette.Add(const Node: TFloatColorNode): Integer;
begin
  Result := Add(Node.Color, Node.NodeType, Node.Theta);
end;

function TFloatPalette.Add(const Diffuse: TIntColor; const Theta: VectorFloat): Integer;
begin
  Result := Add(FloatColor(Diffuse), TFloatNodeType.Plain, Theta);
end;

procedure TFloatPalette.Remove(const Index: Integer);
var
  I: Integer;
begin
  for I := Index to Length(FData) - 2 do
    FData[I] := FData[I + 1];

  SetLength(FData, Length(FData) - 1);
end;

procedure TFloatPalette.Clear;
begin
  SetLength(FData, 0);
end;

function TFloatPalette.GetFirstColor(const Theta: VectorFloat): PFloatColorNode;
var
  I, Frame, WorstIndex: Integer;
  Delta, NewDelta, WorstDelta: VectorFloat;
begin
  Delta := 65535;
  Frame := -1;
  WorstIndex := -1;
  WorstDelta := 65535;

  for I := 0 to Length(FData) - 1 do
  begin
    NewDelta := Abs(Theta - FData[I].Theta);

    if (FData[I].Theta <= Theta) and (NewDelta < Delta) then
    begin
      Frame := I;
      Delta := NewDelta;
    end;

    if NewDelta < WorstDelta then
    begin
      WorstIndex := I;
      WorstDelta := NewDelta;
    end;
  end;

  if Frame = -1 then
  begin
    if WorstIndex <> -1 then
      Result := @FData[WorstIndex]
    else
      Result := nil;
  end
  else
    Result := @FData[Frame];
end;

function TFloatPalette.GetNextColor(const Theta: VectorFloat): PFloatColorNode;
var
  I, Frame, WorstIndex: Integer;
  Delta, NewDelta, WorstDelta: VectorFloat;
begin
  Delta := 65535;
  Frame := -1;
  WorstIndex := -1;
  WorstDelta := 65535;

  for I := 0 to Length(FData) - 1 do
  begin
    NewDelta := Abs(FData[I].Theta - Theta);
    if (FData[I].Theta > Theta) and (NewDelta < Delta) then
    begin
      Frame := I;
      Delta := NewDelta;
    end;

    if WorstDelta > NewDelta then
    begin
      WorstDelta := NewDelta;
      WorstIndex := I;
    end;
  end;

  if Frame = -1 then
  begin
    if WorstIndex <> -1 then
      Result := @FData[WorstIndex]
    else
      Result := nil;
  end
  else
    Result := @FData[Frame];
end;

function TFloatPalette.GetColor(const Theta: VectorFloat): TFloatColor;
const
  PiHalf = Pi * 0.5;
var
  First, Next: PFloatColorNode;
  Kappa: VectorFloat;
begin
  Result := FloatColorBlack;
  if Length(FData) < 1 then
    Exit;

  // Retrieve initial color
  First := GetFirstColor(Theta);
  if not Assigned(First) then
    Exit;

  { use First Color info directly if one of the following is met:
      1) Color has exact Theta match
      2) Color happens after the Theta (and is the first one) }
  if (First.Theta = Theta) or (First.Theta > Theta) then
    Exit(First.Color);

  // Retrieve the next color (to interpolate with)
  Next := GetNextColor(Theta);
  if not Assigned(Next) then
    Exit(First.Color);

  // if there is no difference in time between two frames, return the next one
  if (Next.Theta = First.Theta) or (Next.Theta = Theta) or (Next = First) then
    Exit(Next.Color);

  // calculate interpolation value
  Kappa := (Theta - First.Theta) / (Next.Theta - First.Theta);

  // --> initial sine curve
  if ((First.NodeType = TFloatNodeType.Sine) or (First.NodeType = TFloatNodeType.Accel)) then
  begin
    if ((Next.NodeType = TFloatNodeType.Sine) or (Next.NodeType = TFloatNodeType.Brake)) then
      Kappa := (Sin((Kappa * Pi) - PiHalf) + 1.0) / 2.0
    else
      Kappa := Sin((Kappa * PiHalf) - PiHalf) + 1.0;
  end
  else
  begin
    if ((Next.NodeType = TFloatNodeType.Sine) or (Next.NodeType = TFloatNodeType.Brake)) then
      Kappa := Sin(Kappa * PiHalf);
  end;

  Result := LerpColors(First.Color, Next.Color, Kappa);
end;

procedure TFloatPalette.LoadFromFile(const FileName: StdString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TFloatPalette.SaveToFile(const FileName: StdString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TFloatPalette.SaveToStream(const Stream: TStream);
var
  I: Integer;
begin
  Stream.PutShortString(FName);
  Stream.PutDouble(FTime);
  Stream.PutLongInt(Length(FData));

  for I := 0 to Length(FData) - 1 do
  begin
    Stream.PutLongInt(Round(FData[I].Color.Red * 65535.0));
    Stream.PutLongInt(Round(FData[I].Color.Green * 65535.0));
    Stream.PutLongInt(Round(FData[I].Color.Blue * 65535.0));
    Stream.PutLongInt(Round(FData[I].Color.Alpha * 65535.0));
    Stream.PutByte(Integer(FData[I].NodeType));
    Stream.PutDouble(FData[I].Theta);
  end;
end;

procedure TFloatPalette.LoadFromStream(const Stream: TStream);
var
  I, ElemCount: Integer;
begin
  FName := Stream.GetShortString;
  FTime := Stream.GetDouble;
  ElemCount := Stream.GetLongInt;

  if ElemCount <= 0 then
  begin
    SetLength(FData, 0);
    Exit;
  end;

  SetLength(FData, ElemCount);

  for I := 0 to ElemCount - 1 do
  begin
    FData[I].Color.Red := Stream.GetLongInt / 65535.0;
    FData[I].Color.Green := Stream.GetLongInt / 65535.0;
    FData[I].Color.Blue := Stream.GetLongInt / 65535.0;
    FData[I].Color.Alpha := Stream.GetLongInt / 65535.0;
    FData[I].NodeType := TFloatNodeType(Stream.GetByte);
    FData[I].Theta := Stream.GetDouble;
  end;
end;

procedure TFloatPalette.Assign(const Source: TFloatPalette);
var
  I: Integer;
begin
  FName := Source.Name;
  Time := Source.Time;
  Clear;

  for I := 0 to Source.Count - 1 do
    Add(Source[I]^);
end;

procedure TFloatPalette.SetTime(const Value: VectorFloat);
begin
  FTime := Saturate(Value, 0.0, 1.0);
end;

constructor TFloatPalettes.Create;
begin
  inherited;

  SetLength(FData, 0);
end;

destructor TFloatPalettes.Destroy;
begin
  Clear;

  inherited;
end;

function TFloatPalettes.GetCount: Integer;
begin
  Result := Length(FData);
end;

procedure TFloatPalettes.SetCount(const Value: Integer);
begin
  while (Length(FData) > Value) and (Length(FData) > 0) do
    Remove(Length(FData) - 1);

  while Length(FData) < Value do
    Add;
end;

function TFloatPalettes.GetItem(const Index: Integer): TFloatPalette;
begin
  if (Index >= 0) and (Index < Length(FData)) then
    Result := FData[Index]
  else
    Result := nil;
end;

procedure TFloatPalettes.SetItem(const Index: Integer; const Value: TFloatPalette);
begin
  if (Index >= 0) and (Index < Length(FData)) then
    FData[Index].Assign(Value);
end;

function TFloatPalettes.Add: Integer;
var
  Index: Integer;
begin
  Index := Length(FData);
  SetLength(FData, Index + 1);

  FData[Index] := TFloatPalette.Create;
  Result := Index;
end;

procedure TFloatPalettes.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FData)) then
    Exit;

  if FData[Index] <> nil then
    FData[Index].Free;

  for I := Index to Length(FData) - 2 do
    FData[I] := FData[I + 1];

  SetLength(FData, Length(FData) - 1);
end;

procedure TFloatPalettes.Clear;
var
  I: Integer;
begin
  for I := 0 to Length(FData) - 1 do
    if FData[I] <> nil then
      FreeAndNil(FData[I]);

  SetLength(FData, 0);
end;

function TFloatPalettes.GetFirstPal(const Time: VectorFloat): TFloatPalette;
var
  I, LowerIndex, BestIndex: Integer;
  LowerDelta, Delta, BestDelta: VectorFloat;
begin
  LowerDelta := High(LongInt);
  BestDelta := High(LongInt);
  LowerIndex := -1;
  BestIndex := -1;

  for I := 0 to Length(FData) - 1 do
  begin
    Delta := Abs(Time - FData[I].Time);

    // -> lower index
    if (FData[I].Time <= Time) and (Delta < LowerDelta) then
    begin
      LowerIndex := I;
      LowerDelta := Delta;
    end;

    // -> best index
    if Delta < BestDelta then
    begin
      BestIndex := I;
      BestDelta := Delta;
    end;
  end;

  if LowerIndex = -1 then
  begin
    if BestIndex <> -1 then
      Result := FData[BestIndex]
    else
      Result := nil;
  end
  else
    Result := FData[LowerIndex];
end;

function TFloatPalettes.GetPrevPal(const Time: VectorFloat): TFloatPalette;
var
  I, LowerIndex, BestIndex: Integer;
  LowerDelta, Delta, BestDelta: VectorFloat;
begin
  LowerDelta := High(LongInt);
  BestDelta := High(LongInt);
  LowerIndex := -1;
  BestIndex := -1;

  for I := 0 to Length(FData) - 1 do
  begin
    Delta := Abs(Time - FData[I].Time);

    // -> lower index
    if (FData[I].Time < Time) and (Delta < LowerDelta) then
    begin
      LowerIndex := I;
      LowerDelta := Delta;
    end;

    // -> best index
    if Delta < BestDelta then
    begin
      BestIndex := I;
      BestDelta := Delta;
    end;
  end;

  if LowerIndex = -1 then
  begin
    if BestIndex <> -1 then
      Result := FData[BestIndex]
    else
      Result := nil;
  end
  else
    Result := FData[LowerIndex];
end;

function TFloatPalettes.GetSuccPal(const Time: VectorFloat): TFloatPalette;
var
  I, HigherIndex, BestIndex: Integer;
  HigherDelta, BestDelta, Delta: VectorFloat;
begin
  HigherDelta := High(LongInt);
  BestDelta := High(LongInt);
  HigherIndex := -1;
  BestIndex := -1;

  for I := 0 to Length(FData) - 1 do
  begin
    Delta := Abs(FData[I].Time - Time);

    // -> higher index
    if (FData[I].Time > Time) and (Delta < HigherDelta) then
    begin
      HigherIndex := I;
      HigherDelta := Delta;
    end;

    // -> best index
    if Delta < BestDelta then
    begin
      BestIndex := I;
      BestDelta := Delta;
    end;
  end;

  if HigherIndex = -1 then
  begin
    if BestIndex <> -1 then
      Result := FData[BestIndex]
    else
      Result := nil;
  end
  else
    Result := FData[HigherIndex];
end;

function TFloatPalettes.GetColor(const Theta, Time: VectorFloat): TFloatColor;
var
  First, Second, Left, Right: TFloatPalette;
  Alpha: VectorFloat;
  Color1, Color2, Color3, Color4: TFloatColor;
begin
  // no palettes
  if Length(FData) < 1 then
    Exit(FloatColorBlack);

  // Retrieve initial palette
  First := GetFirstPal(Time);

  { use First Palette directly if one of the following is met:
      1) Palette has exact Time match
      2) Palette appears after the Time }
  if (First.Time = Time) or (First.Time > Time) then
    Exit(First.Color[Theta]);

  // Retrieve the second palette
  Second := GetSuccPal(Time);

  // if there is no difference in time between two palettes, return the next one
  if (Second.Time = First.Time) or (Second.Time = Time) or (Second = First) then
    Exit(Second.Color[Theta]);

  // Retrieve another two palettes for cubic interpolation
  Left := GetPrevPal(First.Time);
  Right := GetSuccPal(Second.Time);

  // calculate interpolation value
  Alpha := (Time - First.Time) / (Second.Time - First.Time);

  // Retrieve all four colors
  Color1 := Left.Color[Theta];
  Color2 := First.Color[Theta];
  Color3 := Second.Color[Theta];
  Color4 := Right.Color[Theta];

  // interpolate the result
  Result.Red := Round(CatmullRom(Color1.Red, Color2.Red, Color3.Red, Color4.Red, Alpha));
  Result.Green := Round(CatmullRom(Color1.Green, Color2.Green, Color3.Green, Color4.Green, Alpha));
  Result.Blue := Round(CatmullRom(Color1.Blue, Color2.Blue, Color3.Blue, Color4.Blue, Alpha));
  Result.Alpha := Round(CatmullRom(Color1.Alpha, Color2.Alpha, Color3.Alpha, Color4.Alpha, Alpha));

  Result := WarpColor(Result);
end;

procedure TFloatPalettes.SaveToStream(const Stream: TStream);
var
  I: Integer;
begin
  Stream.PutShortString(FTitle);
  Stream.PutLongInt(Length(FData));

  for I := 0 to Length(FData) - 1 do
    FData[I].SaveToStream(Stream);
end;

procedure TFloatPalettes.LoadFromStream(const Stream: TStream);
var
  I, ElemCount: Integer;
begin
  FTitle := Stream.GetShortString;
  ElemCount := Stream.GetLongInt;

  Clear;
  if ElemCount <= 0 then
    Exit;

  SetLength(FData, ElemCount);

  for I := 0 to Length(FData) - 1 do
    FData[I] := nil;

  for I := 0 to Length(FData) - 1 do
  begin
    FData[I] := TFloatPalette.Create;
    FData[I].LoadFromStream(Stream);
  end;
end;

function TFloatPalettes.SaveToFile(const FileName: StdString): Boolean;
var
  Stream: TStream;
begin
  Result := True;
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
    try
      SaveToStream(Stream);
    finally
      Stream.Free;;
    end;
  except
    Result := False;
  end;
end;

function TFloatPalettes.LoadFromFile(const FileName: StdString): Boolean;
var
  Stream: TStream;
begin
  Result := True;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

function TFloatPalettes.IndexOf(const Name: StdString): Integer;
var
  Index: Integer;
begin
  for Index := 0 to Length(FData) - 1 do
    if SameText(Name, FData[Index].Name) then
      Exit(Index);

  Result := -1;
end;

procedure TFloatPalettes.Assign(const Source: TFloatPalettes);
var
  I: Integer;
begin
  FTitle := Source.Title;
  Count := Source.Count;

  for I := 0 to Length(FData) - 1 do
    FData[I].Assign(Source[I]);
end;

procedure TFloatPalettes.Add(const Color1, Color2, Color3, Color4: Cardinal);
var
  Index: Integer;
begin
  Index := Add;
  FData[Index].Clear;
  FData[Index].Add(Color1, 0.0);
  FData[Index].Add(Color2, 0.333);
  FData[Index].Add(Color3, 0.667);
  FData[Index].Add(Color4, 1.0);
end;

constructor TFloatPaletteSet.Create;
begin
  inherited;

  SetLength(FData, 0);
end;

destructor TFloatPaletteSet.Destroy;
begin
  Clear;

  inherited;
end;

function TFloatPaletteSet.GetCount: Integer;
begin
  Result := Length(FData);
end;

function TFloatPaletteSet.GetItem(const Index: Integer): TFloatPalettes;
begin
  if (Index >= 0) and (Index < Length(FData)) then
    Result := FData[Index]
  else
    Result := nil;
end;

procedure TFloatPaletteSet.SetItem(const Index: Integer; const Value: TFloatPalettes);
begin
  if (Index >= 0) and (Index < Length(FData)) then
    FData[Index].Assign(Value);
end;

function TFloatPaletteSet.Add: Integer;
var
  Index: Integer;
begin
  Index := Length(FData);
  SetLength(FData, Index + 1);

  FData[Index] := TFloatPalettes.Create;
  Result := Index;
end;

procedure TFloatPaletteSet.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(FData)) then
    Exit;

  FData[Index].Free;

  for I := Index to Length(FData) - 2 do
    FData[I] := FData[I + 1];

  SetLength(FData, Length(FData) - 1);
end;

procedure TFloatPaletteSet.Clear;
var
  I: Integer;
begin
  for I := 0 to Length(FData) - 1 do
    if FData[I] <> nil then
      FreeAndNil(FData[I]);

  SetLength(FData, 0);
end;

procedure TFloatPaletteSet.SaveToStream(const Stream: TStream);
var
  I: Integer;
begin
  Stream.PutLongInt(Length(FData));

  for I := 0 to Length(FData) - 1 do
    FData[I].SaveToStream(Stream);
end;

procedure TFloatPaletteSet.LoadFromStream(const Stream: TStream);
var
  I, ElemCount: Integer;
begin
  ElemCount := Stream.GetLongInt;

  Clear;
  if ElemCount <= 0 then
    Exit;

  SetLength(FData, ElemCount);
  for I := 0 to Length(FData) - 1 do
    FData[I] := nil;

  for I := 0 to Length(FData) - 1 do
  begin
    FData[I] := TFloatPalettes.Create;
    FData[I].LoadFromStream(Stream);
  end;
end;

function TFloatPaletteSet.SaveToFile(const FileName: StdString): Boolean;
var
  Stream: TStream;
begin
  Result := True;
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
    try
      SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

function TFloatPaletteSet.LoadFromFile(const FileName: StdString): Boolean;
var
  Stream: TStream;
begin
  Result := True;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

function TFloatPaletteSet.IndexOf(const Title: StdString): Integer;
var
  I: Integer;
begin
  for I := 0 to Length(FData) - 1 do
    if SameText(Title, FData[I].Title) then
      Exit(I);

  Result := -1;
end;

end.
