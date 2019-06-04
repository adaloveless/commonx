unit rec2json;

interface

uses betterobject, debug, typinfo, systemx, typex, system.rtti, stringx, jsonhelpers, sysutils, variants;

type
  TThreeByteArray = array[0..2] of byte;
  TRec = record
  public
    sh: string;
    fu: double;
    da: array of byte;
    sa: TThreeByteArray;
  end;
  TTestBox = class
  private
    FA: double;
    FB: string;

    procedure SetA(const Value: double);
    procedure SetB(const Value: string);
  published
    property A: double read FA write SetA;
    property B: string read FB write SetB;
  end;


function RecToJSon(inst: pointer; TypeInfoOfRec: pointer): string;
function JSONtoRec(j: TJSON; inst: pointer; TypeInfoOfRec: pointer): string;overload;
procedure JSONToRec(s: string; inst: pointer; TypeInfoOfRec: pointer);overload;




procedure Test;

implementation

procedure Test2;
var
  Count, Loop: Integer;
  List: PPropList;
begin
  Count := GetPropList(TypeInfo(TTestBox), tkAny, nil);
  GetMem(List, Count * SizeOf(PPropInfo));
  try
    GetPropList(TypeInfo(TTestBox), tkAny, List);
    for Loop := 0 to Pred(Count) do
      Debug.Log(List^[Loop]^.Name);

  finally
    FreeMem(List, Count * SizeOf(PPropInfo))
  end;
end;

procedure Test;
var
  r: TRec;
  s: string;
  j: IHolder<TJSON>;
begin
  r.sh := 'hello';
  r.fu := 21.0;
  setlength(r.da, 3);
  r.da[0] := 1;
  r.da[1] := 2;
  r.da[2] := 3;
  r.sa[0] := 4;
  r.sa[1] := 5;
  r.sa[2] := 6;


  s := recTojson(@r, TypeInfo(TRec));
  Debug.Log(s);

  s := '{"sh":"OneTwoThreeFour", "fu":1234}';
  j := StrToJSONh(s);
  jsonToRec(j.o, @r, TypeInfo(TRec));

end;

function TValueToJsonStorage(v: TValue): string;
var
  subtyp: TRTTIType;
  t: ni;
  vSub: TValue;
begin
  case v.TypeInfo.Kind of
    TTypeKind.tkString,
    tkWideString,
    tkLString,
    tkUString,
    tkWChar,
    tkAnsiChar: begin
      result := quote(v.ToString);
    end;
    tkDynArray: begin
      result := '[';
      for t:= 0 to v.GetArrayLength-1 do begin
        if t > 0 then
          result := result + ',';
//        subtyp := TRttiDynamicArrayType(v.TypeInfo).ElementType;
        vSub := v.GetArrayElement(t);
        result := result + TvalueToJSONStorage(vSub);
      end;

      result := result + ']';

    end;
    tkArray: begin
      result := '[';
      for t:= 0 to v.GetArrayLength-1 do begin
        if t > 0 then
          result := result + ',';
//        subtyp := TRttiArrayType(v.TypeInfo).ElementType;
        vSub := v.GetArrayElement(t);
        result := result + TvalueToJSONStorage(vSub);
      end;

      result := result + ']';

    end;
  else
    result := v.Tostring;
  end;
end;

function RecToJSon(inst: pointer; TypeInfoOfRec: pointer): string;
type
  PValue = ^TValue;
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
  FContext := TRTTIContext.create;
  try
    typ := FContext.GetType(TypeInfoOfRec);
    rt := typ.AsRecord;
//    Debug.Log(typ.ToString);
    result := '{';
    a := rt.GetFields;
    for t:= 0 to high(a) do begin
      fld := a[t];
      Data := fld.GetValue(inst) ;
      if t > 0 then
        result := result + ',';
      result := result + quote(fld.Name)+':'+TValuetoJSONStorage(Data);
    end;
    result := result + '}';

  finally
    FContext.free;
  end;
end;

procedure JSONToRec(s: string; inst: pointer; TypeInfoOfRec: pointer);overload;
var
  j: TJSON;
begin
  j := TJSON.Create;
  try
    j.FromString(s);
    JSONToRec(s, inst, TypeInfoOfRec);
  finally
    j.free;
    j := nil;
  end;
end;
function JSONtoRec(j: TJSON; inst: pointer; TypeInfoOfRec: pointer): string;
var
  FContext: TRTTIContext;
  typ: TRTTIType;
  rt: TRTTIRecordType;
  fld: TRttiField;
  data,newdata, vSub: TValue;
  Value: TValue;
  x,t: ni;
  a: TArray<TRTTIField>;
  fldName: string;
  v: variant;
  nn: TJSON;
  s: string;
begin

  FContext := TRTTIContext.create;
  try
    typ := FContext.GetType(TypeInfoOfRec);
    rt := typ.AsRecord;
//    Debug.Log(typ.ToString);
    a := rt.GetFields;
    for t:= 0 to high(a) do begin
      fld := a[t];
      Data := fld.GetValue(inst) ;
      fldName := fld.Name;
      if length(fldName) >= 2 then begin
        if fldName[STRZ] = '_' then begin
          if charINSet(fldName[STRZ+1],['0','1','2','3','4','5','6','7','8','9']) then begin
            fldName := zcopy(fldName, 1, length(FldName)-1);
          end;
        end;
      end;
      if j.HasNode(fld.Name) then begin

        case Data.TypeInfo.Kind of
          tkArray: begin
            for x := 0 to data.GetArrayLength-1 do begin
              vSub := data.GetArrayElement(x);
              nn := j.GetNode(fld.Name);
              v := nn[x].Value;
              newData := TValue.FromVariant(v);
              data.SetArrayElement(x, newData);
//              fld.SetValue(inst, newData);
              fld.SetValue(inst, Data);
            end;
//            newData := data;
//            fld.SetValue(inst, newData);
          end;

        else
          v := j[fldName].value;
          if (varType(v) and varNull) = varNull then begin
            newData := TValue.FromVariant(0);
          end else begin
            newData := TValue.FromVariant(v);
          end;
    //      Debug.Log('t='+inttostr(t)+' fld='+fldName+' v='+VarToStr(v));
          if data.Kind <> newData.kind then begin
            if data.Kind in [tkString, tkUString, tkWString, tkChar, tkWChar, tkAnsiChar, tkAnsiString]
            then begin
              newData := TValue.FromVariant(vartostr(v));
            end;
            if data.Kind in [tkFloat] then begin
              case newData.kind of
                tkInteger: begin
                  s := vartostr(v);
                  if s = '' then
                    s := '0.0';
                  newData := TValue.FromVariant(strtofloat(s));
                end;
              end;
            end;
          end;
          fld.SetValue(inst, newData);

        end;

      end;
    end;

  finally
    FContext.free;
  end;
end;


procedure TTestBox.SetA(const Value: double);
begin
  FA := Value;
end;

procedure TTestBox.SetB(const Value: string);
begin
  FB := Value;
end;

end.
