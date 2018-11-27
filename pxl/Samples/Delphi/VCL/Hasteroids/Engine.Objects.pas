unit Engine.Objects;
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

{ Special note: this code was ported multiple times from earliest framework releases predating Asphyre. }

uses
  PXL.TypeDef, PXL.Types;

type
  TBaseObjects = class;

  TCollideMethod = (Distance, Rectangle);

  TBaseObject = class
  private
    FID: Integer;
    FNext: TBaseObject;
    FPrev: TBaseObject;
    FPosition: TPoint2;
    FVelocity: TPoint2;
    FOwner: TBaseObjects;
    FDying: Boolean;
    FCollideRadius: Integer;
    FCollideRect: TIntRect;
    FCollided: Boolean;

    procedure SetOwner(const Value: TBaseObjects);
    procedure SetPrev(const Value: TBaseObject);
    procedure SetNext(const Value: TBaseObject);
    procedure SetDying(const Value: Boolean);
    procedure SetPosition(const Value: TPoint2);
    procedure SetVelocity(const Value: TPoint2);
    function GetIntPos: TPoint2px; inline;
    procedure SetIntPos(const Value: TPoint2px); inline;
    procedure ChangeID(const Value: Integer);
  protected
    // links previous and next objects leaving this object unconnected
    procedure Unlink;

    // + called AFTER position has been changed
    procedure UpdatedPosition; virtual;
    // + called AFTER velocity has been changed
    procedure UpdatedVelocity; virtual;
    // + called BEFORE the object is to be destroyed
    procedure ObjectDestroy; virtual;
    // + called BEFORE object's ID is to be changed
    // NOTE: Upon entry to this function, if ACCEPT is "False", then the
    // new ID can be either accepted or denied.
    // However, if the value of "Accept" is "True", then the ID change is
    // indispensable.
    procedure UpdateID(const NewID: Integer; var Accept: Boolean); virtual;
    // + called when (before) "Dying" property is changed
    procedure Die(const StartDying: Boolean; var Accept: Boolean); virtual;

    // + called to confirm that the object can hit the destination object
    procedure CollideCheck(const DestObj: TBaseObject; const Distance: Integer; var Accept: Boolean); virtual;
    // + called AFTER object has confirmed and hit other object
    procedure ObjectCollide(const DestObj: TBaseObject); virtual;
  public
    constructor Create(const AOwner: TBaseObjects);
    destructor Destroy; override;

    procedure Move; virtual;
    procedure Render(const Tag: TObject); virtual; abstract;

    // objects's legacy and links
    property ID: Integer read FID write ChangeID;
    property Owner: TBaseObjects read FOwner write SetOwner;
    property Prev: TBaseObject read FPrev write SetPrev;
    property Next: TBaseObject read FNext write SetNext;

    // determines if the object is to be destroyed or resurrected
    property Dying: Boolean read FDying write SetDying;
    // determines if the object has collided with something
    // (that way it's excluded from current collision check)
    // NOTE: "Move" method changes this back to "False"
    property Collided: Boolean read FCollided write FCollided;

    // object position vector
    property Position: TPoint2 read FPosition write SetPosition;
    property Velocity: TPoint2 read FVelocity write SetVelocity;

    // integer position
    property IntPos: TPoint2px read GetIntPos write SetIntPos;

    // the rectangle object occupies in the space used for collision detection
    // NOTE: only applies if CollisionType is "cmRectangle"
    property CollideRect: TIntRect read FCollideRect write FCollideRect;

    // the radius from which object can collide with other objects
    // NOTE: only applies if CollisionType is "cmDistance"
    property CollideRadius: Integer read FCollideRadius write FCollideRadius;
  end;

  TBaseObjects = class
  private
    FIDNum: Integer;
    FCollide: Boolean;
    FCollideFreq: Integer;
    FCollideMethod: TCollideMethod;

    function UniqueID: Integer;
    function GetObject(const AID: Integer): TBaseObject; inline;
    function GetObjectNum(const AIndex: Integer): TBaseObject;
    function GetCount: Integer;
    procedure SetCollideFreq(const Value: Integer);
  protected
    FListHead: TBaseObject;
    FListTail: TBaseObject;

    function GenerateID: Integer; virtual;
    function FindByID(const AID: Integer): TBaseObject; virtual;
    procedure Insert(const ANode: TBaseObject); virtual;
    procedure UnlinkObj(const ANode: TBaseObject); virtual;
    procedure DoCollide(const ANode: TBaseObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Remove(const AID: Integer); virtual;

    // either moves or destroys dead objects
    procedure Update;

    // renders all objects on the screen ("Tag" is simply passed to individual object)
    procedure Render(const Tag: TObject);

    property Objects[const AID: Integer]: TBaseObject read GetObject; default;
    property ObjectNum[const AIndex: Integer]: TBaseObject read GetObjectNum;
    property Count: Integer read GetCount;

    property Collide: Boolean read FCollide write FCollide;
    property CollideFreq: Integer read FCollideFreq write SetCollideFreq;
    property CollideMethod: TCollideMethod read FCollideMethod write FCollideMethod;
  end;


implementation

uses
  Math;

{$REGION 'TBaseObject'}

constructor TBaseObject.Create(const AOwner: TBaseObjects);
var
  LAccept: Boolean;
begin
  inherited Create;

  FOwner := AOwner;
  FCollideRadius := 16;
  FCollideRect := IntRect(0, 0, 16, 16);

  if FOwner <> nil then
  begin
    // assign a new ID
    FID := FOwner.UniqueID;

    // update event
    LAccept := True;
    UpdateID(FID, LAccept);

    // insert object
    FOwner.Insert(Self);
  end;
end;

destructor TBaseObject.Destroy;
begin
  // unlink the object from the chain
  Unlink;

  // call destroy event
  ObjectDestroy;

  inherited;
end;

procedure TBaseObject.SetPrev(const Value: TBaseObject);
var
  LPrev: TBaseObject;
begin
  // determine previous forward link
  LPrev := nil;
  if (FPrev <> nil) and (FPrev.Next = Self) then
    LPrev := FPrev;

  // update link
  FPrev := Value;

  // remove previous forward link
  if LPrev <> nil then
    LPrev.Next := nil;

  // insert forward link
  if (FPrev <> nil) and (FPrev.Next <> Self) then
    FPrev.Next := Self;
end;

procedure TBaseObject.SetNext(const Value: TBaseObject);
var
  LNext: TBaseObject;
begin
  // determine previous backward link
  LNext := nil;
  if (FNext <> nil) and (FNext.Prev = Self) then
    LNext := FNext;

  // update link
  FNext := Value;

  // remove previous backward link
  if LNext <> nil then
    LNext.Prev := nil;

  // insert backward link
  if (FNext <> nil) and (FNext.Prev <> Self) then
    FNext.Prev := Self;
end;

procedure TBaseObject.Unlink;
var
  LPrev, LNext: TBaseObject;
begin
  // unlink the object from its owner
  if FOwner <> nil then
    FOwner.UnlinkObj(Self);

  // unlink previous node
  LPrev := FPrev;
  LNext := FNext;
  FPrev := nil;
  FNext := nil;

  if LPrev = nil then
  begin
    if LNext <> nil then
      LNext.Prev := nil;
  end
  else
    LPrev.Next := LNext;
end;

procedure TBaseObject.SetOwner(const Value: TBaseObjects);
begin
  // unlink the node
  Unlink;

  // switch owner
  FOwner := Value;

  // re-insert the node
  if FOwner <> nil then
    FOwner.Insert(Self);
end;

procedure TBaseObject.SetPosition(const Value: TPoint2);
begin
  FPosition := Value;
  UpdatedPosition;
end;

procedure TBaseObject.SetVelocity(const Value: TPoint2);
begin
  FVelocity := Value;
  UpdatedVelocity;
end;

function TBaseObject.GetIntPos: TPoint2px;
begin
  Result := Point2ToPx(FPosition);
end;

procedure TBaseObject.SetIntPos(const Value: TPoint2px);
begin
  Position := Value;
end;

procedure TBaseObject.SetDying(const Value: Boolean);
var
  LAccept: Boolean;
begin
  // call event
  LAccept := True;
  Die(Value, LAccept);

  // change status
  if LAccept then
    FDying := Value;
end;

procedure TBaseObject.UpdatedPosition;
begin
end;

procedure TBaseObject.UpdatedVelocity;
begin
end;

procedure TBaseObject.ObjectDestroy;
begin
end;

procedure TBaseObject.CollideCheck(const DestObj: TBaseObject; const Distance: Integer; var Accept: Boolean);
begin
end;

procedure TBaseObject.ObjectCollide(const DestObj: TBaseObject);
begin
end;

procedure TBaseObject.Die(const StartDying: Boolean; var Accept: Boolean);
begin
end;

procedure TBaseObject.UpdateID(const NewID: Integer; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TBaseObject.ChangeID(const Value: Integer);
var
  LAccept: Boolean;
begin
  // call Update event
  LAccept := False;
  UpdateID(Value, LAccept);

  // proceed only if accepted
  if LAccept then
  begin
    // unlink the node
    Unlink;

    // update ID
    FID := Value;

    // re-insert the node
    if FOwner <> nil then
      FOwner.Insert(Self);
  end;
end;

procedure TBaseObject.Move;
begin
  FPosition := FPosition + FVelocity;
  UpdatedPosition;

  FCollided := False;
end;

{$ENDREGION}
{$REGION 'TBaseObjects'}

constructor TBaseObjects.Create;
begin
  inherited;

  FIDNum := High(Integer);

  FCollide := False;
  FCollideFreq := 4;
  FCollideMethod := TCollideMethod.Distance;
end;

destructor TBaseObjects.Destroy;
begin
  Clear;

  inherited;
end;

function TBaseObjects.GenerateID: Integer;
begin
  Result := FIDNum;
  Dec(FIDNum);
end;

function TBaseObjects.UniqueID: Integer;
const
  FailTolerance = 10000;
var
  Tolerance, NewID: Integer;
begin
  Tolerance := FailTolerance;
  NewID := GenerateID;

  while (FindByID(NewID) <> nil) and (Tolerance > 0) do
  begin
    NewID := GenerateID;
    Dec(Tolerance);
  end;

  Result := NewID;
end;

procedure TBaseObjects.Insert(const ANode: TBaseObject);
var
  LID: Integer;
  LNode: TBaseObject;
begin
  // do not accept NULL objects
  if ANode = nil then
    Exit;

  // retreive object ID
  LID := ANode.ID;

  // 3. check if the object already exists
  if FindByID(LID) = ANode then
    Exit;

  // 4. if no items available - create a first element
  if FListHead = nil then
  begin
    ANode.Prev := nil;
    ANode.Next := nil;
    FListHead := ANode;
    FListTail := FListHead;
    Exit;
  end;

  // insert BEFORE first element
  if LID < FListHead.ID then
  begin
    ANode.Prev := nil;
    ANode.Next := FListHead;
    FListHead := ANode;
    Exit;
  end;

  // insert AFTER first element
  if LID > FListTail.ID then
  begin
    ANode.Next := nil;
    FListTail.Next := ANode;
    FListTail := ANode;
    Exit;
  end;

  // search using either fordward or backward method
  if Abs(Int64(FListHead.ID) - LID) < Abs(Int64(FListTail.ID) - LID) then
  begin
    // 7 forward search
    LNode := FListHead;

    while LNode.Next.ID < LID do
      LNode := LNode.Next;

    // update links
    ANode.Next := LNode.Next;
    ANode.Prev := LNode;
  end
  else
  begin
    // backward search
    LNode := FListTail;

    while LNode.Prev.ID > LID do
      LNode := LNode.Prev;

    // update links
    ANode.Prev := LNode.Prev;
    ANode.Next := LNode;
  end;
end;

function TBaseObjects.FindByID(const AID: Integer): TBaseObject;
var
  LNode: TBaseObject;
begin
  // no objects exist
  if FListHead = nil then
    Exit(nil);

  // do either forward or backward search
  if Abs(Int64(FListHead.ID) - AID) < Abs(Int64(FListTail.ID) - AID) then
  begin
    // forward search
    LNode := FListHead;

    while (LNode <> nil) and (LNode.ID <> AID) do
      LNode := LNode.Next;
  end
  else
  begin
    // backward search
    LNode := FListTail;

    while (LNode <> nil) and (LNode.ID <> AID) do
      LNode := LNode.Prev;
  end;

  Result := LNode;
end;

procedure TBaseObjects.UnlinkObj(const ANode: TBaseObject);
begin
  if FListTail = ANode then
    FListTail := FListTail.Prev;

  if FListHead = ANode then
  begin
    FListHead := nil;

    if ANode.Next <> nil then
      FListHead := ANode.Next;
  end;
end;

function TBaseObjects.GetObject(const AID: Integer): TBaseObject;
begin
  Result := FindByID(AID);
end;

function TBaseObjects.GetCount: Integer;
var
  LNode: TBaseObject;
begin
  Result := 0;
  LNode := FListHead;

  while LNode <> nil do
  begin
    LNode := LNode.Next;
    Inc(Result);
  end;
end;

procedure TBaseObjects.Remove(const AID: Integer);
var
  LNode: TBaseObject;
begin
  // find object
  LNode := FindByID(AID);

  // release object (it should unlink itself)
  if LNode <> nil then
    LNode.Free;
end;

procedure TBaseObjects.Update;
var
  LNode, TempNode: TBaseObject;
  NodeDied: Boolean;
begin
  // move all objects
  LNode := FListHead;

  while LNode <> nil do
  begin
    LNode.Move;
    LNode := LNode.Next;
  end;

  // collide objects
  LNode := FListHead;

  while LNode <> nil do
  begin
    if (not LNode.Dying) and (not LNode.Collided) then
      DoCollide(LNode);

    LNode := LNode.Next;
  end;

  // destroy "dead" objects
  LNode := FListHead;
  while LNode <> nil do
  begin
    NodeDied := False;

    // check if object dies
    if LNode.Dying then
    begin
      TempNode := LNode;
      LNode := LNode.Next;
      TempNode.Free;
      NodeDied := True;
    end;

    // move the object
    if not NodeDied then
      LNode := LNode.Next;
  end;
end;

procedure TBaseObjects.SetCollideFreq(const Value: Integer);
begin
  FCollideFreq := Max(Value, 1);
end;

procedure TBaseObjects.DoCollide(const ANode: TBaseObject);
var
  LNode: TBaseObject;
  Delta: VectorFloat;
  Accept1, Accept2, TooClose: Boolean;
begin
  LNode := ANode.Next;

  while LNode <> nil do
  begin
    if (not LNode.Dying) and (not LNode.Collided) then
    begin
      Delta := Sqrt(Sqr(ANode.Position.X - LNode.Position.X) + Sqr(ANode.Position.Y - LNode.Position.Y));

      if FCollideMethod = TCollideMethod.Distance then
        TooClose := (Delta < (ANode.CollideRadius + LNode.CollideRadius))
      else
        TooClose := OverlapRect(ANode.CollideRect, LNode.CollideRect);

      if TooClose then
      begin
        Accept1 := True;
        Accept2 := True;

        ANode.CollideCheck(LNode, Trunc(Delta), Accept1);
        LNode.CollideCheck(ANode, Trunc(Delta), Accept2);

        // only collide objects if both agree
        if Accept1 and Accept2 then
        begin
          // assume both objects have collided
          ANode.Collided := True;
          LNode.Collided := True;

          // call the apropriate events
          ANode.ObjectCollide(LNode);
          LNode.ObjectCollide(ANode);

          // stop collision check for this object, if it's marked "Collided"
          // NOTE: "ObjectCollide" event can change "Collided" variable to prevent this.
          if ANode.Collided then
            Break;
        end;
      end;
    end;

    LNode := LNode.Next;
  end;
end;

procedure TBaseObjects.Render(const Tag: TObject);
var
  LNode: TBaseObject;
begin
  LNode := FListHead;

  while LNode <> nil do
  begin
    if not LNode.Dying then
      LNode.Render(Tag);

    LNode := LNode.Next;
  end;
end;

procedure TBaseObjects.Clear;
var
  LNode, LPrev: TBaseObject;
begin
  LNode := FListTail;

  while LNode <> nil do
  begin
    LPrev := LNode.Prev;
    LNode.Free;
    LNode := LPrev;
  end;
end;

function TBaseObjects.GetObjectNum(const AIndex: Integer): TBaseObject;
var
  LNode: TBaseObject;
  LIndex: Integer;
begin
  Result := nil;
  LNode := FListHead;
  LIndex := 0;

  while LIndex < AIndex do
  begin
    LNode := LNode.Next;
    Inc(LIndex);
  end;

  if LIndex = AIndex then
    Result := LNode;
end;

{$ENDREGION}

end.
