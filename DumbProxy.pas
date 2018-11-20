unit DumbProxy;

interface

uses
  sysutils, typex, systemx, stringx, classes;

type
  TDumbProxyHead = packed record
  private
    function GetTargetIPString: string;
    procedure SetTargetIPString(const Value: string);
  public
    Head: array[0..2] of ansichar;
    Version: byte;
    TargetIPVersion: byte;
    TargetIP: array[0..15] of byte;
    ZoneID: word;
    TargetPort: word;
    procedure Init;
    function IsHeaderValid: boolean;
    property TargetIPString: string read GetTargetIPString write SetTargetIPString;
  end;

implementation

{ TDumbProxyHead }

function TDumbProxyHead.GetTargetIPString: string;
begin
  if TargetIPVersion = 4 then begin
    result := targetip[0].tostring+'.'+targetip[1].tostring+'.'+targetip[2].tostring+'.'+targetip[3].tostring+'.'
  end else begin
    result := targetip[0].ToHexString+targetip[1].ToHexString+':'+
              targetip[2].ToHexString+targetip[3].ToHexString+':'+
              targetip[4].ToHexString+targetip[5].ToHexString+':'+
              targetip[6].ToHexString+targetip[7].ToHexString+':'+
              targetip[8].ToHexString+targetip[9].ToHexString+':'+
              targetip[10].ToHexString+targetip[11].ToHexString+':'+
              targetip[12].ToHexString+targetip[13].ToHexString+':'+
              targetip[14].ToHexString+targetip[15].ToHexString;

    if ZoneID <> 0 then
      result := result + '%'+inttohex(zoneid,1);



  end;
end;

procedure TDumbProxyHead.Init;
begin
  head[0] := 'P';
  head[1] := 'X';
  head[2] := 'Y';
  Version := 0;
  TargetIPVersion := 0;
end;

function TDumbProxyHead.IsHeaderValid: boolean;
begin
  result := true;
  result := result and (head[0] = 'P');
  result := result and (head[1] = 'X');
  result := result and (head[2] = 'Y');
  result := result and (version = 0);
  result := result and ((targetipversion = 4) or (targetipversion =6));


end;

procedure TDumbProxyHead.SetTargetIPString(const Value: string);
var
  sl: TStringlist;
  t: ni;
  s,s1,s2: string;
  w: word;
  z: word;
begin
  ZoneId := 0;
  sl := nil;
  try
    if zpos(':', value) >=0 then begin
      TargetIPVErsion := 6;
      sl := ParseString(value, ':');
      for t := 0 to sl.Count-1 do begin
        if sl[t] = '' then begin
          sl[t] := '0';
          while sl.Count < 8 do begin
            sl.Insert(t, '0');
          end;
        end;
      end;


      for t:= 0 to sl.count-1 do begin
        s := sl[t];
        if zpos('%', s)>=0 then begin
          SplitString(s, '%', s1,s2);
          w := strtoint('$'+s1);
          z := strtoint('$'+s2);
          ZoneId := z;
        end else begin
          w := strtoint('$'+s);
        end;
        TargetIP[(t*2)+0] := (w shr 8) and 255;
        TargetIP[(t*2)+1] := (w shr 0) and 255;
      end;


    end else begin
      TargetIPVersion := 4;
      sl := ParseString(value, '.');
      if sl.Count <> 3 then
        raise ECritical.create('illegal IP Address '+value);

      TargetIP[0] := strtoint(sl[0]);
      TargetIP[1] := strtoint(sl[1]);
      TargetIP[2] := strtoint(sl[2]);
      TargetIP[3] := strtoint(sl[3]);

    end;
  finally
    sl.free;
  end;



end;

end.
