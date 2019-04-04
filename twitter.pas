unit twitter;

interface

uses
  systemx, typex, betterobject, stringx, JSONHelpers, https, webstring, DateUtils, tickcount, sysutils, idhmac, hmac, idhmacsha1, debug,
  REST.Client, REST.Authenticator.OAuth, rest.types, Vcl.ComCtrls, IPPeerCommon,IPPeerClient,
  Datasnap.DSClientRest, Data.Bind.Components, Data.Bind.ObjectScope;


type
  TTwitterClient = class(TBetterObject)
  private
    restclient: TRESTClient;
    restRequest: TRESTRequest;

    function Getoauth_access_token: string;
    function Getoauth_access_token_secret: string;
    function Getoauth_consumer_key: string;
    function Getoauth_consumer_secret: string;
    procedure Setoauth_access_token(const Value: string);
    procedure Setoauth_access_token_secret(const Value: string);
    procedure Setoauth_consumer_key(const Value: string);
    procedure Setoauth_consumer_secret(const Value: string);
  protected

  public
    UserName: string;
    Password: string;
    procedure Init; override;
    procedure Detach; override;
    property oauth_consumer_key: string read Getoauth_consumer_key write Setoauth_consumer_key;
    property oauth_consumer_secret: string read Getoauth_consumer_secret write Setoauth_consumer_secret;
    property oauth_access_token: string read Getoauth_access_token write Setoauth_access_token;
    property oauth_access_token_secret: string read Getoauth_access_token_secret write Setoauth_access_token_secret;
    function SearchTweets(q: string): IHolder<TJSON>;
    function Tweet(msg: string): IHolder<TJSON>;
    function Follow(handle: string): IHolder<TJSON>;

  END;

function TwitterDateToDAteTimeGMT(s: string): TDateTime;

implementation

{ TTwitterClient }

function TwitterDateToDAteTimeGMT(s: string): TDateTime;
var
  sWeekDay, sMon, sDay, sHour, sMin, sSec, sOFFSET, sYear: string;
  mn: integer;
  fs: TFormatSettings;
  function GetmonthNumber(s: string): integer;
  var
    t: ni;
  begin
    for t:= low(fs.shortMonthNames) to high(fs.shortmonthNames) do
      if comparetext(fs.shortmonthnames[t], s)=0 then
        exit(t);

    exit(1);

  end;
begin
  SplitString(s, ' ', sWeekDay, s);
  SplitString(s, ' ', sMon, s);
  SplitString(s, ' ', sDay, s);
  SplitString(s, ':', sHour, s);
  SplitString(s, ':', sMin, s);
  SplitString(s, ' ', sSec, s);
  SplitString(s, ' ', sOffset, s);
  SplitString(s, ' ', sYear, s);

  fs := TFormatSettings.create;
  fs.CurrencyString := '$d.dd';
  fs.CurrencyDecimals := 2;
  fs.DateSeparator := '/';
  fs.TimeSeparator := ':';
  fs.ShortDayNames[1] := 'sun';
  fs.ShortDayNames[2] := 'mon';
  fs.ShortDayNames[3] := 'tue';
  fs.ShortDayNames[4] := 'wed';
  fs.ShortDayNames[5] := 'thu';
  fs.ShortDayNames[6] := 'fri';
  fs.ShortDayNames[6] := 'sat';
  fs.ShortMonthNames[1] := 'jan';
  fs.ShortMonthNames[2] := 'feb';
  fs.ShortMonthNames[3] := 'mar';
  fs.ShortMonthNames[4] := 'apr';
  fs.ShortMonthNames[5] := 'may';
  fs.ShortMonthNames[6] := 'jun';
  fs.ShortMonthNames[7] := 'jul';
  fs.ShortMonthNames[8] := 'aug';
  fs.ShortMonthNames[9] := 'sep';
  fs.ShortMonthNames[10] := 'oct';
  fs.ShortMonthNames[11] := 'nov';
  fs.ShortMonthNames[12] := 'dec';
//  fs.ShortDateFormat := 'yyyymmdd hh:nn:ss';
  fs.ShortDateFormat := 'yyyymmdd';
  mn := GetmonthNumber(sMon);

  result := EncodeDateTime(
                strtoint(sYEar),
                mn,
                strtoint(sDay),
                strtoint(sHour),
                strToInt(sMin),
                strToInt(sSec), 0
              );




end;

procedure TTwitterClient.Detach;
begin
  if detached then exit;
  restclient.free;
  restClient := nil;
//  restClient.authenticator := nil;//<<---OWNED by rest client
  restRequest := nil;//<<---OWNED by rest client

  inherited;

end;

function TTwitterClient.Follow(handle: string): IHolder<TJSON>;
var
  h: IJSONHOlder;
  t: nativeint;
begin
  RESTclient.BaseURL := 'https://api.twitter.com';
//  RESTRequest1.BaseURL := 'https://api.twitter.com';
  RESTRequest.Resource := '1.1/friendships/create.json';
  RESTRequest.AddParameter('screen_name',handle, TRESTRequestParameterKind.pkGETorPOST);
  RESTRequest.AddParameter('follow','true', TRESTRequestParameterKind.pkGETorPOST);
  RESTRequest.Method := TRESTRequestMethod.rmPOST;
  RESTRequest.Execute;
  h := StrToJSONh(RestRequest.Response.JSONText);
  result := h;
end;

function TTwitterClient.Getoauth_access_token: string;
begin
  result := TOAuth1Authenticator(restClient.authenticator).AccessToken;
end;

function TTwitterClient.Getoauth_access_token_secret: string;
begin
  result := TOAuth1Authenticator(restClient.authenticator).AccessTokenSecret;
end;

function TTwitterClient.Getoauth_consumer_key: string;
begin
  result := TOAuth1Authenticator(restClient.authenticator).ConsumerKey;
end;

function TTwitterClient.Getoauth_consumer_secret: string;
begin
  result := TOAuth1Authenticator(restClient.authenticator).ConsumerSecret;
end;

function TTwitterClient.SearchTweets(q: string): IHolder<TJSON>;
var
  h: IJSONHOlder;
  t: nativeint;
begin
  RESTclient.BaseURL := 'https://api.twitter.com';
//  RESTRequest1.BaseURL := 'https://api.twitter.com';
  RESTRequest.Resource := '1.1/search/tweets.json';
  RESTRequest.AddParameter('q',q);
  RESTRequest.AddParameter('count','1000');
  RESTRequest.AddParameter('include_entities','false');
  RESTRequest.AddParameter('tweet_mode','extended');
  RESTRequest.Method := rmGEt;
  RESTRequest.Execute;
  h := StrToJSONh(RestRequest.Response.JSONText);
  result := h;

end;

procedure TTwitterClient.Init;
begin
  inherited;
  restClient := TRESTClient.create(nil);
  restClient.Authenticator := TOAuth1Authenticator.create(restClient);
  restRequest := TRESTRequest.create(restClient);

end;

procedure TTwitterClient.Setoauth_access_token(const Value: string);
begin
  TOAuth1Authenticator(restClient.authenticator).AccessToken := value;
end;

procedure TTwitterClient.Setoauth_access_token_secret(const Value: string);
begin
  TOAuth1Authenticator(restClient.authenticator).AccessTokenSecret := value;
end;

procedure TTwitterClient.Setoauth_consumer_key(const Value: string);
begin
  TOAuth1Authenticator(restClient.authenticator).consumerkey := value;
end;

procedure TTwitterClient.Setoauth_consumer_secret(const Value: string);
begin
  TOAuth1Authenticator(restClient.authenticator).ConsumerSecret := value;
end;

function TTwitterClient.Tweet(msg: string): IHolder<TJSON>;
var
  h: IJSONHOlder;
  t: nativeint;
begin
  RESTclient.BaseURL := 'https://api.twitter.com';
//  RESTRequest1.BaseURL := 'https://api.twitter.com';
  RESTRequest.Resource := '1.1/statuses/update.json';
  RESTRequest.AddParameter('status',msg, TRESTRequestParameterKind.pkGETorPOST);
  RESTRequest.Method := TRESTRequestMethod.rmPOST;
  RESTRequest.Execute;
  h := StrToJSONh(RestRequest.Response.JSONText);
  result := h;
end;

{ TTwitterEvilRobot }



end.
