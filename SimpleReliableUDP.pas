unit SimpleReliableUDP;
{$I DelphiDefs.inc}

interface

uses
{$IFDEF RUDP2}
  rudp2;
{$ELSE}
  rudp1;
{$ENDIF}

type
{$IFDEF RUDP2}
  TReliableUDPEndpoint = rudp2.TReliableUDPEndpoint;
  TSimpleReliableUDPClient = rudp2.TSimpleReliableUDPClient;
  TMultiplexedUDPServer = rudp2.TMultiplexedUDPServer;
  TMultiplexedUDPEndpoint = rudp2.TMultiplexedUDPEndpoint;
  TSimpleReliablePrivateServerEndpoint = rudp2.TSimpleReliablePrivateServerEndpoint;
{$ELSE}
  TconnectionType = rudp1.TConnectionType;
  TReliableUDPEndpoint = rudp1.TReliableUDPEndpoint;
  TSimpleReliableUDPClient = rudp1.TSimpleReliableUDPClient;
  TMultiplexedUDPServer = rudp1.TMultiplexedUDPServer;
  TMultiplexedUDPEndpoint = rudp1.TMultiplexedUDPEndpoint;
  TSimpleReliablePrivateServerEndpoint = rudp1.TSimpleReliablePrivateServerEndpoint;

{$ENDIF}



implementation




end.
