unit rtti_helpers;

interface

uses
  system.rtti, debug, typex, classes,// dbGrids,
  variants, fleximath, stringx, sysutils;



function RecToCSVHeader(inst: PByte; TypeInfoOfRec: pointer): string;
function RectoCSV(inst:PByte; TypeInfoOfrec:pointer): string;
function RecToDebugSTring(inst: PByte; TypeInfoOfRec: pointer): string;



implementation


function RecToDebugSTring(inst: PByte; TypeInfoOfRec: pointer): string;
var
  FContext: TRTTIContext;
  typ: TRTTIType;
  rt: TRTTIRecordType;
  fld: TRttiField;
  data: TValue;
  Value: TValue;
  t: ni;
  a: TArray<TRTTIField>;
  s: string;
  psub: Pbyte;
begin
  typ := FContext.GetType(TypeInfoOfRec);
  rt := typ.AsRecord;
  a := rt.GetFields;

  result := '';

  FContext := TRTTIContext.create;
  try
    for t:= 0 to high(a) do begin

      fld := a[t];
      Data := fld.GetValue(inst) ;
      s := data.toString;
      pSub := inst+fld.Offset;
{$DEFINE CHASE_RECORDS}
{$IFDEF CHASE_RECORDS}
      if comparetext(s, '(record)') =0 then
        s := RectoDebugString(psub, data.TypeInfo);
{$ENDIF}
      if result = '' then
        result := '{'+fld.Name+'='+s
      else
        result := result + ','+fld.Name+'='+s;
    end;

    result := result + '}';

  finally
    FContext.free;
  end;


end;

function RecToCSVHeader(inst: PByte; TypeInfoOfRec: pointer): string;
var
  FContext: TRTTIContext;
  typ: TRTTIType;
  rt: TRTTIRecordType;
  fld: TRttiField;
  data: TValue;
  Value: TValue;
  t: ni;
  a: TArray<TRTTIField>;
begin
  typ := FContext.GetType(TypeInfoOfRec);
  rt := typ.AsRecord;
  a := rt.GetFields;

  result := '';

  FContext := TRTTIContext.create;
  try
    for t:= 0 to high(a) do begin

      fld := a[t];
      Data := fld.GetValue(inst) ;
      if result = '' then
        result := fld.Name
      else
        result := result + ','+fld.Name;
    end;

  finally
    FContext.free;
  end;


end;

function RectoCSV(inst:PByte; TypeInfoOfrec:pointer): string;
var
  FContext: TRTTIContext;
  typ: TRTTIType;
  rt: TRTTIRecordType;
  fld: TRttiField;
  data: TValue;
  Value: TValue;
  t: ni;
  a: TArray<TRTTIField>;
begin
  typ := FContext.GetType(TypeInfoOfRec);
  rt := typ.AsRecord;
  a := rt.GetFields;

  result := '';

  FContext := TRTTIContext.create;
  try
    for t:= 0 to high(a) do begin

      fld := a[t];
      Data := fld.GetValue(inst) ;
      if result = '' then
        result := data.toString
      else
        result := result + ','+data.ToSTring;
    end;

  finally
    FContext.free;
  end;
end;




end.
