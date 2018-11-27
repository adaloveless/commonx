unit Engine.Scrolling;
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
  PXL.TypeDef, PXL.Types;

type
  TDragScroll = class
  private const
    MaxAveragePositions = 5;
    MaxScrollingSpeed = 32.0;
    MaxScrollingThrust = 1.0;
  private
    FDeviceScale: VectorFloat;

    FPosition: TPoint2;
    FVelocity: TPoint2;

    FReference: TPoint2px;
    FDragging: Boolean;

    FPositions: array[0..MaxAveragePositions - 1] of TPoint2px;
    FLastPosition: TPoint2px;
    FLastAveragePosition: TPoint2px;
    FLastAverageVelocity: TPoint2px;

    FViewPos: TPoint2px;

    function GetAveragePosition: TPoint2px;

    procedure StartDragging;
    procedure ContinueDragging;
  public
    constructor Create;

    procedure TouchDown(const TouchPos: TPoint2px);
    procedure TouchMove(const TouchPos: TPoint2px);
    procedure TouchUp(const TouchPos: TPoint2px);

    procedure Update;
    procedure SetPosition(const NewPosition: TPoint2);

    property DeviceScale: VectorFloat read FDeviceScale write FDeviceScale;
    property Dragging: Boolean read FDragging;
    property ViewPos: TPoint2px read FViewPos;
  end;

implementation

uses
  Math;

constructor TDragScroll.Create;
begin
  inherited;

  FDeviceScale := 1.0;

  FPosition := ZeroPoint2;
  FVelocity := ZeroPoint2;

  FDragging := False;

  FLastPosition := Undefined2px;
  FPositions[0] := Undefined2px;

  FLastAverageVelocity := ZeroPoint2px;
end;

procedure TDragScroll.Update;
var
  CurrentAveragePosition: TPoint2px;
  Amp, InitAmp, Thrust: Single;
  I: Integer;
begin
  for I := MaxAveragePositions - 1 downto 1 do
    FPositions[I] := FPositions[I + 1];

  FPositions[0] := FLastPosition;

  CurrentAveragePosition := GetAveragePosition;
  FLastAverageVelocity := FLastAveragePosition - CurrentAveragePosition;
  FLastAveragePosition := CurrentAveragePosition;

  if not FDragging then
  begin
    Amp := Length2(FVelocity);

    if Amp > 0.0 then
    begin
      Thrust := MaxScrollingThrust * FDeviceScale;

      InitAmp := Amp;
      Amp := Max(Amp - Thrust, 0.0);

      FVelocity.X := (FVelocity.X / InitAmp) * Amp;
      FVelocity.Y := (FVelocity.Y / InitAmp) * Amp;
    end;

    if Amp > VectorEpsilon then
      FPosition := FPosition + FVelocity;
  end;

  FViewPos := Point2ToPx(FPosition);
end;

function TDragScroll.GetAveragePosition: TPoint2px;
var
  I, HistoryCount: Integer;
  Weight, WeightSum, AccX, AccY: Single;
begin
  HistoryCount := 0;
  WeightSum := 0.0;
  AccX := 0.0;
  AccY := 0.0;
  Weight := 1.0;

  for I := 0 to MaxAveragePositions - 1 do
  begin
    if FPositions[I] = Undefined2px then
      Break;

    AccX := AccX + FPositions[I].X * Weight;
    AccY := AccY + FPositions[I].Y * Weight;
    WeightSum := WeightSum + Weight;
    Weight := Weight * 0.5;
    Inc(HistoryCount);
  end;

  if HistoryCount < 1 then
    Exit(Undefined2px);

  Result.X := Round(AccX / WeightSum);
  Result.Y := Round(AccY / WeightSum);
end;

procedure TDragScroll.StartDragging;
begin
  FReference := FLastPosition;
  FVelocity := ZeroPoint2;
end;

procedure TDragScroll.ContinueDragging;
var
  Shift, CurrentPos: TPoint2px;
begin
  CurrentPos := GetAveragePosition;
  if CurrentPos = Undefined2px then
    Exit;

  Shift := CurrentPos - FReference;
  FPosition := FPosition - Shift;

  FReference := CurrentPos;
end;

procedure TDragScroll.TouchDown(const TouchPos: TPoint2px);
begin
  FLastPosition := TouchPos;
  StartDragging;
  FDragging := True;
end;

procedure TDragScroll.TouchMove(const TouchPos: TPoint2px);
begin
  FLastPosition := TouchPos;

  if FDragging then
    ContinueDragging;
end;

procedure TDragScroll.TouchUp(const TouchPos: TPoint2px);
var
  MaxSpeed: Single;
begin
  FLastPosition := Undefined2px;

  if FDragging then
  begin
    FDragging := False;

    MaxSpeed := MaxScrollingSpeed * FDeviceScale;

    FVelocity := FLastAverageVelocity;

    if Length2(FVelocity) > MaxSpeed then
      FVelocity := Norm2(FVelocity) * MaxSpeed;
  end;
end;

procedure TDragScroll.SetPosition(const NewPosition: TPoint2);
begin
  FPosition := NewPosition;
  FVelocity := ZeroPoint2;
end;

end.
