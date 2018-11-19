unit RDTPLightShowSocketServer;

interface

uses
  RDTPSocketServer, classes, sysutils, rdtplightshowserverimplib;

type
  TRDTPLightShowSocketServer = class(TRDTPSocketServer<TLightShowServer>)
  public
    procedure Init;override;
    destructor Destroy;override;

  end;

implementation

{ TRDTPLightShowSocketServer }

destructor TRDTPLightShowSocketServer.Destroy;
begin
  inherited;
end;

procedure TRDTPLightShowSocketServer.Init;
begin
  inherited;

end;

end.
