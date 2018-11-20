unit RegionLock;

interface

uses
  sharedobject, abstractlocker, types, stringx, sysutils, collision, typex, geometry;


type
  TRegionLock = class(TAbstractLock)
  public
    filename: string;
    user: string;
    device: string;
    rect: TRect;
    expires: TDateTime;
    function CollidesWith(l: TAbstractLock): boolean;
    function ToPersistentString: string; override;
    procedure FromPersistentString(s: string);override;
  end;

  TRegionLocker = class(TAbstractLocker<TRegionLock>)
  public
    procedure ReleaseLocksForUser(sUser: string; sDevice: string);
  end;

implementation

{ TRegionLock }

function TRegionLock.CollidesWith(l: TAbstractLock): boolean;
var
  rl: TREgionLock;
begin
  rl := TRegionLock(l);
  result := false;
  if RectCollision(self.rect, rl.rect) then begin
    if rl.expires > now then begin
      if (CompareText(self.filename, rl.filename)=0) then begin
        result := true;
      end;
    end;
  end;
end;

procedure TRegionLock.FromPersistentString(s: string);
var
  s1,s2: string;
begin
  //dont call inherited!
  s2 := s;
  splitString(s2,',',s1,s2);
  rect.FromString(s1);
  splitstring(s2,',',s1,s2);
  user := s1;
  splitstring(s2,',',s1,s2);
  Device := s1;
  splitstring(s2,',',s1,s2);
  FileName := s2;


end;

function TRegionLock.ToPersistentString: string;
begin
  //no need to call inherited, nothing there  will raise exception
  result := rect.ToString+','+User+','+Device+','+FileName;


end;

{ TRegionLocker }

procedure TRegionLocker.ReleaseLocksForUser(sUser, sDevice: string);
var
  t: ni;
  l: TRegionLock;
begin
  lock;
  try
    for t:= 0 to FLocks.count-1 do begin
      l := FLocks[t];
      if Comparetext(sUser, l.user)=0 then begin
        if CompareText(sDevice, l.device)=0 then begin
          Release(l);
        end;
      end;
    end;
  finally
    unlock;
  end;

end;

end.
