unit RightsClasses;

interface

uses  Sysutils, requestinfo, systemx;

const
  MAX_RIGHT_NAME = 30;
  MAX_RIGHT_DESC = 80;

type
  TRight = object
    Fname: pointer;
    Fnamelength: byte;
    Fdesc: pointer;
    Fdesclength: byte;
    visible: boolean;
    f1,f2,f3,f4,f5,f6,f7,f8: integer;
    public
    procedure Init;

    procedure SetName(sName: ansistring);
    procedure SetDesc(sDesc: ansistring);
    function GetName: ansistring;
    function GetDesc: ansistring;
    property Name: ansistring read GetName write SetName;
    property Desc: ansistring read GetDesc write SetDesc;



  end;

  PRight = ^TRight;



var
  globalrights: array[0..300] of TRight;
  globalrightsindex: integer;



function ReadOnlyRightsCheckboxList(rqINfo: TRequestInfo; roleID: integer; sRightFilter: ansistring; bNoFilter: boolean; bReadOnly: boolean=false): ansistring;
function RightsCheckboxList(rqINfo: TRequestInfo; roleID: integer; sRightFilter: ansistring; bNoFilter: boolean; bReadOnly: boolean=false): ansistring;
function DefineRight(bVisible: boolean; sRight: ansistring; sDesc: ansistring): PRight;
function FindRight(sName: ansistring): PRight;
function GetRightCount: integer;
function GetRight(idx: integer): PRight;
function IsRightInFilter(superior, sub: PRight): boolean;

function RightCombine(r1, r2: PRight): TRight;


implementation

uses WebString, rights;

//------------------------------------------------------------------------------
function DefineRight(bVisible: boolean ;sRight: ansistring; sDesc: ansistring): PRight;
var
  i: integer;
begin
  sRight := uppercase(sRight);
                                                                                                 i := globalrightsindex;

  globalrights[i].Init;

  if sDesc = '' then
    sDesc := ' ';

  globalrights[i].Name := sRight;
  globalrights[i].Desc := sDesc;
  globalrights[i].f1 := 0;
  globalrights[i].f2 := 0;
  globalrights[i].f3 := 0;
  globalrights[i].f4 := 0;
  globalrights[i].f5 := 0;
  globalrights[i].f6 := 0;
  globalrights[i].f7 := 0;
  globalrights[i].f8 := 0;


  result := @globalrights[i];
  result.visible := bVisible;

  inc(globalrightsindex);

end;


function CompareMemory(p1,p2: pointer; size: integer): boolean;
asm
end;

function FindRight(sName: ansistring): PRight;
var
  t: integer;
  l: integer;
begin
  sName := uppercase(sName);

  l := length(sName);

  result := nil;

  for t:= 0 to globalrightsindex-1 do begin
    if l <> globalrights[t].Fnamelength then
      continue;

    if CompareMem(@sName[1], globalrights[t].FName, l) then begin
      result := @globalrights[t];
      break;
    end;
  end;
end;


function GetRightCount: integer;
begin
  result := globalrightsindex;

end;


function GetRight(idx: integer): PRight;
begin
  result := @globalrights[idx];
end;

function RightCombine(r1, r2: PRight): TRight;
begin
  result.f1 := r1.f1 or r2.f1;
  result.f2 := r1.f2 or r2.f2;
  result.f3 := r1.f3 or r2.f3;
  result.f4 := r1.f4 or r2.f4;
  result.f5 := r1.f5 or r2.f5;
  result.f6 := r1.f6 or r2.f6;
  result.f7 := r1.f7 or r2.f7;
  result.f8 := r1.f8 or r2.f8;


end;

function ReadOnlyRightsCheckboxList(rqINfo: TRequestInfo; roleID: integer; sRightFilter: ansistring; bNoFilter: boolean; bReadOnly: boolean=false): ansistring;
var
  t: integer;
  filter, right: PRight;
begin
  filter := RightsClasses.FindRight(sRightFilter);
  result := '';
  for t:= 0 to GetRightCount-1 do begin
    right := GetRight(t);
    if IsRightInFilter(filter, right) and (right.visible) and CurrentUserHasRight(rQInfo, right.Name) then begin
      result := result+'[[[:readonlyrightscheckbox('+inttostr(roleid)+','''+right.Name+''','''+EncodeWebString(right.desc)+'<BR>'')]]]';
    end;
  end;

  if result = '' then
    result := '<i>(you do not have permission to see rights in this category)</i>';
end;


function RightsCheckboxList(rqINfo: TRequestInfo; roleID: integer; sRightFilter: ansistring; bNoFilter: boolean; bReadOnly: boolean=false): ansistring;
var
  t: integer;
  filter, right: PRight;
begin
  filter := RightsClasses.FindRight(sRightFilter);
  result := '';
  for t:= 0 to GetRightCount-1 do begin
    right := GetRight(t);
    if IsRightInFilter(filter, right) and (right.visible) and CurrentUserHasRight(rQInfo, right.Name) then begin
      result := result+'[[[:rightscheckbox('+inttostr(roleid)+','''+right.Name+''','''+EncodeWebString(right.desc)+'<BR>'')]]]';
    end;
  end;

  if result = '' then
    result := '<i>(you do not have permission to assign rights in this category)</i>';
end;

function IsRightInFilter(superior, sub: PRight): boolean;
var
  f1,f2,f3,f4,f5,f6,f7,f8: integer;
begin
  f1 := superior.f1 and sub.f1;
  f2 := superior.f2 and sub.f2;
  f3 := superior.f3 and sub.f3;
  f4 := superior.f4 and sub.f4;
  f5 := superior.f5 and sub.f5;
  f6 := superior.f6 and sub.f6;
  f7 := superior.f7 and sub.f7;
  f8 := superior.f8 and sub.f8;

  result := (f1 = sub.f1) and
            (f2 = sub.f2) and
            (f3 = sub.f3) and
            (f4 = sub.f4) and
            (f5 = sub.f5) and
            (f6 = sub.f6) and
            (f7 = sub.f7) and
            (f8 = sub.f8);




end;



{ TRight }

function TRight.GetDesc: ansistring;
begin
  result := PAnsiChar(FDesc);
end;

function TRight.GetName: ansistring;
begin
  result := PAnsiChar(FName)
end;

procedure TRight.Init;
begin
  FNAme := nil;
  FDesc := nil;
  FNameLength := 0;
  FDescLength := 0;
end;

procedure TRight.SetName(sName: ansistring);
begin
  if FName <> nil then
    system.FreeMem(FNAme);

  GetMem(FName, length(sNAme)+1);
  MoveMem32(FName, @sNAme[1], length(sNAme)+1);

  FNAmeLength := length(sName);



end;

procedure TRight.SetDesc(sDesc: ansistring);
begin
  if FDesc <> nil then
    system.FreeMem(FDesc);

  GetMem(FDesc, length(sDesc)+1);
  MoveMem32(FDesc, @sDesc[1], length(sDesc)+1);

  FDescLEngth := length(sDesc);

end;

initialization
  globalrightsindex := 0;


end.
