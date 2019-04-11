unit BetterHTTPReqResp;

interface

uses
  HTTPClient_2019, classes, system.net.httpclient;

type
  TBetterHTTPReqResp = class(THTTPReqResp)
  private
    FAcceptRanges: string;
    FContentRange: string;
    procedure HTTPWebNode_BeforePost(const HTTPReqResp: THTTPReqResp; Client: THTTPClient);
  public
    constructor Create(AOwner: TComponent); override;
    property AcceptRanges: string read FAcceptRanges write FAcceptRanges;
    property ContentRange: string read FContentRange write FContentRange;
  end;




implementation

{ TBetterHTTPReqResp }

constructor TBetterHTTPReqResp.Create(AOwner: TComponent);
begin
  inherited;
  OnBeforePost := HTTPWebNode_BeforePost;
end;


{
TMyRIO = class(THTTPRIO)
public
  constructor Create(AOwner: TComponent); override;
  procedure HTTPWebNode_BeforePost(const AHTTPReqResp: THTTPReqResp;
AData: Pointer);
...
constructor TMyRIO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HTTPWebNode.OnBeforePost := HTTPWebNode_BeforePost;
end;
procedure TMyRIO.HTTPWebNode_BeforePost(const AHTTPReqResp:
THTTPReqResp; AData: Pointer);
const sCUSTOM_HEADER = 'X-Custom: value';
begin
    HttpAddRequestHeaders(AData, PChar(sCUSTOM_HEADER),
Length(sCUSTOM_HEADER), HTTP_ADDREQ_FLAG_ADD);
end;

}

procedure TBetterHTTPReqResp.HTTPWebNode_BeforePost(
  const HTTPReqResp: THTTPReqResp; Client: THTTPClient);
begin
  if acceptranges <> '' then
    client.CustomHeaders['Accept-Ranges'] := AcceptRanges;
  if contentrange <> '' then
    client.CustomHeaders['Content-Range'] := ContentRange;
end;

end.
