unit networkx;

interface

uses
 {$IFDEF MSWINDOWS}
  USock,
{$ELSE}
{$ENDIF}
{$IFDEF IOS}
  IdStackVCLPosix,
{$ENDIF}
  idstack,
  typex, stringx, systemx, betterobject, classes, sysutils, debug;


function GetBroadcastIPs(): IHolder<TStringList>;
function GetInterfaceIPs(): IHolder<TStringList>;
function GetNICinfo(): IHolder<TStringList>;
function GetHostID(eport: ni): string;
function ip_to_broadcast_v4(addr: string; subnet: string): string;

implementation

function ip_to_broadcast_v4(addr: string; subnet: string): string;
var
  sladdr, slsubnet: IHolder<TStringlist>;
  a,s,b: cardinal;
  bb: array[0..3] of byte;
begin
  sladdr := ParseStringH(addr, '.');
  slsubnet := ParseStringH(subnet, '.');
  if sladdr.o.count <> 4 then
    raise ECritical.create(addr+' is not an ipv4 address');
  if slsubnet.o.count <> 4 then
    raise ECritical.create(subnet+' is not an ipv4 subnet');

  a := (cardinal(strtoint64(sladdr.o[0])) shl 0)
      +(cardinal(strtoint64(sladdr.o[1])) shl 8)
      +(cardinal(strtoint64(sladdr.o[2])) shl 16)
      +(cardinal(strtoint64(sladdr.o[3])) shl 24);
  s := (cardinal(strtoint64(slsubnet.o[0])) shl 0)
      +(cardinal(strtoint64(slsubnet.o[1])) shl 8)
      +(cardinal(strtoint64(slsubnet.o[2])) shl 16)
      +(cardinal(strtoint64(slsubnet.o[3])) shl 24);
  b := (not s) or (a and s);

  bb[0] := (b shr 0) and 255;
  bb[1] := (b shr 8) and 255;
  bb[2] := (b shr 16) and 255;
  bb[3] := (b shr 24) and 255;

  result := inttostr(bb[0])+'.'+inttostr(bb[1])+'.'+inttostr(bb[2])+'.'+inttostr(bb[3]);



end;

function GetInterfaceIPs(): IHolder<TStringList>;
{$IFNDEF MSWINDOWS}
begin
  result := THolder<TStringlist>.create;
  result.o := TStringlist.create;
  result.o.add('127.0.0.1');
end;
{$ELSE}
Var i                 : Integer;
    aNetInterfaceList : tNetworkInterfaceList;
Begin
  result := THolder<TStringlist>.create;
  result.o := TStringlist.create;

  If (GetNetworkInterfaces (aNetInterfaceList)) THen
  Begin
    result.o.Clear;
    For i := 0 to High (aNetInterfaceList) do
    Begin
      if aNetInterfaceList[i].BroadcastSupport
      and aNetInterfaceList[i].IsInterfaceUp
      and (not aNetInterfaceList[i].IsLoopback) then
        result.o.Add (aNetInterfaceList[i].AddrIp);
    end;
  end;
end;
{$ENDIF}

function GetBroadcastIPs(): IHolder<TStringList>;
{$IFNDEF MSWINDOWS}
{$IFDEF IOS}
var
  addrs: TIdStackLocalAddressList;
  addr: TIdStackLocalAddress;
  t: ni;
  a,s,b: string;
begin
  result := THolder<TStringlist>.create;
  result.o := TStringlist.create;

  addrs := TIdStackLocalAddressList.Create;
  try
    idstack.GStack.GetLocalAddressList(addrs);
    for t:= 0 to addrs.Count-1 do begin
      addr := addrs.items[t] as TIDStackLocalAddress;
      if addr is TIdStackLocalAddressIPv4 then begin
        a := TIdStackLocalAddressIPv4(addr).IPAddress;
        s := TIdStackLocalAddressIPv4(addr).SubNetMask;
        b := ip_to_broadcast_v4(a,s);
        result.o.Add(b);
      end;

    end;

  finally
    addrs.free;
  end;
end;
{$ELSE}
begin
  result := THolder<TStringlist>.create;
  result.o := TStringlist.create;
  result.o.add('255.255.255.255');
end;
{$ENDIF}
{$ELSE}
Var i                 : Integer;
    aNetInterfaceList : tNetworkInterfaceList;
Begin
  result := THolder<TStringlist>.create;
  result.o := TStringlist.create;

  If (GetNetworkInterfaces (aNetInterfaceList)) THen
  Begin
    result.o.Clear;
    For i := 0 to High (aNetInterfaceList) do
    Begin
      if aNetInterfaceList[i].BroadcastSupport
      and aNetInterfaceList[i].IsInterfaceUp
      and (not aNetInterfaceList[i].IsLoopback) then begin
        result.o.Add (aNetInterfaceList[i].AddrDirectedBroadcast);
      end;
    end;
  end;
end;

{$ENDIF}
function GetNICinfo(): IHolder<TStringList>;
{$IFNDEF MSWINDOWS}
begin
  NotImplemented;
end;
{$ELSE}
Var i                 : Integer;
    aNetInterfaceList : tNetworkInterfaceList;
Begin
  result := THolder<TStringlist>.create;
  result.o := TStringlist.create;

  If (GetNetworkInterfaces (aNetInterfaceList)) THen
  Begin
    result.o.Clear;
    result.o.Add (DateTimeToStr (Now)+ ' : ');

    For i := 0 to High (aNetInterfaceList) do
    Begin
      result.o.Add ('');
      result.o.Add ('#                          : ' + IntToStr(i));
      //result.o.Add ('Name                       : ' + aNetInterfaceList[i].ComputerName);
      result.o.Add ('IP-Address                 : ' + aNetInterfaceList[i].AddrIP);
      result.o.Add ('Subnet mask                : ' + aNetInterfaceList[i].SubnetMask);
      result.o.Add ('Net address                : ' + aNetInterfaceList[i].AddrNet);
      result.o.Add ('Limited broadcast address  : ' + aNetInterfaceList[i].AddrLimitedBroadcast);
      result.o.Add ('Directed Broadcast address : ' + aNetInterfaceList[i].AddrDirectedBroadcast);
      result.o.Add ('Interface up               : ' + BoolToStr (aNetInterfaceList[i].IsInterfaceUp, True));
      result.o.Add ('Broadcast supported        : ' + BoolToStr (aNetInterfaceList[i].BroadcastSupport, True));
      result.o.Add ('Loopback interface         : ' + BoolToStr (aNetInterfaceList[i].IsLoopback, True));
      result.o.Add ('');
    end;
  end;
end;
{$ENDIF}


function GetHostID(eport: ni): string;
begin
  result := extractfilenamepart(dllname)+'@'+GetInterfaceIPs().o.text;
  result := stringreplace(result, #13#10, '/', [rfReplaceAll]);
  result := stringreplace(result, #13, '/', [rfReplaceAll]);
  result := stringreplace(result, #10, '/', [rfReplaceAll]);
  result := result+eport.tostring;
end;



end.
