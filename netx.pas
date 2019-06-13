unit netx;

interface

uses
  sockfix, scktcomp, winapi.winsock, sysutils;

function DNSLookup(sHost: string): string;
function ReverseIP(sIp: string): string;

implementation

uses
  stringx;


function DNSLookup(sHost: string): string;
var
  WSAData: TWSAData;
  addr : Cardinal;
  lpHost1: PHostEnt;
  s: string;
  astr: ansistring;
  ws: widestring;
  pc: PAnsiChar;
begin

  Result := '';

  s := sHost;
  astr := s;
  lpHost1 := gethostbyname(PAnsiChar(astr));

  if lpHost1 <> nil then begin
    pc := lpHost1.h_addr^;
    if pc <> nil then begin
      result := inttostr(ord(pc[0]))+'.'+inttostr(ord(pc[1]))+'.'+inttostr(ord(pc[2]))+'.'+inttostr(ord(pc[3]));
    end;
  end;

end;


function ReverseIP(sIp: string): string;
var
  s1,s2,sleft, sright: string;
begin

  result := '';
  sRight := sIP;
  While SplitString(sRight, '.', sLeft, sright) do begin
    if result <> '' then
      result := '.'+result ;

    result := sLeft+result;
  end;

  result := sLeft+ '.'+result;



end;


end.
