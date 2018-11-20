unit bboxlock;

interface

uses
  systemx,typex,sharedobject,betterobject,stringx, numbers, generics.collections.fixed;

type
  TBBox = class(TBetterObject)
  public
    x1,y1,x2,y2: double;
    function Right: double;
    function Top: double;
    function Left: double;
    function Bottom: double;
    function Overlaps(lck: TBBox): boolean;
  end;

  TBBoxLock = TBBox;



  TBBoxLocker = class(TSharedObject)
  strict private
    FList: TList<TBBoxLock>;
    function OverlapsAny(lck: TBBoxLock): boolean;
  public
    constructor Create;override;
    destructor Destroy;override;

    function TryGetBBOX(lck: TBBoxLock): boolean ;
    procedure ReleaseBBOX(lck: TBBoxLock);

  end;

implementation

{ TBBoxLocker }



{ TBBoxLocker }

constructor TBBoxLocker.Create;
begin
  inherited;
  FLIst := TList<TBBoxLock>.create;
end;

destructor TBBoxLocker.Destroy;
begin
  fList.free;
  inherited;
end;

function TBBoxLocker.OverlapsAny(lck: TBBoxLock): boolean;
var
  t: ni;
begin
  result := false;
  Lock;
  try
    for t:= 0 to FList.count-1 do begin
      if FList[t].Overlaps(lck) then begin
        result := true;
        exit;
      end;
    end;
  finally
    unlock;
  end;

end;

procedure TBBoxLocker.ReleaseBBOX(lck: TBBoxLock);
begin
  Lock;
  try
    fList.Remove(lck);
//    lck.free;

  finally
    unlock;
  end;
end;

function TBBoxLocker.TryGetBBOX(lck: TBBox): boolean;
begin
  result := false;
  Lock;
  try
    if not OverlapsAny(lck) then begin
      FList.add(lck);
      result := true;
    end else begin
//      lck.free;
//      lck := nil;
    end;
  finally
    Unlock;
  end;


end;

{ TBBoxLock }

function TBBoxLock.Bottom: double;
begin
  result := greaterof(y1,y2);
end;

function TBBoxLock.Left: double;
begin
  result := lesserof(x1,x2);
end;

function TBBoxLock.Overlaps(lck: TBBoxLock): boolean;
begin
  result := false;
  if lck.Right < self.Left then begin
    result := false;
    exit;
  end;

  if lck.Left > self.Right then begin
    result := false;
    exit;
  end;

  if lck.Top > self.Bottom then begin
    result := false;
    exit;
  end;

  if lck.Bottom < self.Top then begin
    result := false;
    exit;
  end;


  if InRange(lck.Left, self.Left, self.Right) and (InRange(lck.Top, self.Top, self.Bottom) or ((lck.Top <= self.Top) and (lck.Bottom >= self.bottom))) then begin
    result := true;
    exit;
  end;

  if InRange(lck.Right, self.Left, self.Right) and (InRange(lck.Top, self.Top, self.Bottom) or ((lck.Top <= self.Top) and (lck.Bottom >= self.bottom)))then begin
    result := true;
    exit;
  end;

  if InRange(lck.Top, self.Top, self.Bottom) and (InRange(lck.LEft, self.LEft, self.Right) or ((lck.LEft <= self.LEft) and (lck.Right >= self.Right)))then begin
    result := true;
    exit;
  end;

  if InRange(lck.Bottom, self.Top, self.Bottom) and (InRange(lck.LEft, self.LEft, self.Right) or ((lck.LEft <= self.LEft) and (lck.Right >= self.Right)))then begin
    result := true;
    exit;
  end;







end;

function TBBoxLock.Right: double;
begin
  result := greaterof(x1,x2);
end;

function TBBoxLock.Top: double;
begin
  result := lesserof(y1,y2);
end;

end.
