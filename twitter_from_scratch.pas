unit twitter_from_scratch;

interface

uses
  systemx, typex, betterobject, stringx, JSONHelpers, https, webstring, DateUtils, tickcount, sysutils, idhmac, hmac, idhmacsha1, debug;

type
  TTwitterClient = class(TBetterObject)
  public
    UserName: string;
    Password: string;
    oauth_consumer_key: string;
    oauth_consumer_secret: string;
    oauth_access_token: string;
    oauth_access_token_secret: string;
    function GetNonce: string;
    function GetTweets(): IHolder<TJSON>;

  END;


  TTwitterEvilRobot = class(TTwitterclient)
  public
    procedure Init;override;
  end;

implementation

{ TTwitterClient }

function TTwitterClient.GetNonce: string;
begin
  result := inttostr(getcurrentthreadid)+'_'+inttostr(getticker);
end;

function TTwitterClient.GetTweets: IHolder<TJSON>;
type
  TParamType = (tpURL, tpBody, tpAuth);
  TParam = record
    name: string;
    value: string;
    typ: TParamType;
  end;
var
  s: string;
  sAuth: string;
(*
OAuth oauth_consumer_key="xvz1evFS4wEEPTGEFPHBog",
oauth_nonce="kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg",
oauth_signature="tnnArxj06cWHq44gCs1OSKk%2FjLY%3D",
oauth_signature_method="HMAC-SHA1",
oauth_timestamp="1318622958",
oauth_token="370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb",
oauth_version="1.0"
Content-Length: 76
Host: api.twitter.com
*)
  sNonce: string;
  token: string;
  sBaseURL: string;
  sURLParams: string;
  params: TArray<TParam>;
  unix_ts: string;
  sig: string;
  sURL: string;
  sigseed: string;
  signkey: string;
  authparams: string;
  procedure AddParam(name: string; value: string; typ: TParamType);
  begin
    setlength(params, length(params)+1);
    params[high(params)].name := name;
    params[high(params)].value := value;
    params[high(params)].typ := typ;
  end;

  procedure Swap(i1,i2: ni);
  var
    pmTemp: TParam;
  begin
    pmTemp := params[i1];
    params[i1] := params[i2];
    params[i2] := pmTemp;
  end;

  function SortPass: boolean;
  var
    t: ni;
  begin
    result := true;
    for t:= 0 to high(params)-1 do begin
      if params[t].name > params[t+1].name then begin
        Swap(t,t+1);
        result := false;
      end;
    end;
  end;

  function GetSigSeed: string;
  var
    x: ni;
  begin
    result := '';
    for x := 0 to High(params) do begin
      if x > 0 then
        result := result + '&';
      result := result + EncodeWebString(params[x].name)+'='+encodewebstring(params[x].value);
    end;
  end;

  function GetURLParams: string;
  var
    x: ni;
  begin
    result := '';
    for x := 0 to High(params) do begin
      if params[x].typ = tpURL then begin
        if result <> '' then
          result := result + '&';
        result := result + EncodeWebString(params[x].name)+'='+encodewebstring(params[x].value);
      end;
    end;
  end;

  function GetPostParams: string;
  var
    x: ni;
  begin
    result := '';
    for x := 0 to High(params) do begin
      if params[x].typ = tpURL then begin
        if result <> '' then
          result := result + '&';
        result := result + EncodeWebString(params[x].name)+'='+encodewebstring(params[x].value);
      end;
    end;
  end;

  function GetAuthParams: string;
  var
    x: ni;
  begin
    result := '';
    for x := 0 to High(params) do begin
      if params[x].typ = tpAuth then begin
        if result <> '' then
          result := result + ',';
        result := result + (*EncodeWebString*)(params[x].name)+'='+quote((*encodewebstring*)(params[x].value));
      end;
    end;
  end;

begin
  result := nil;

  sNonce := GetNonce;

  sBaseURL := 'https://api.twitter.com/1.1/search/tweets.json';

  AddParam('q', '@freebandnames', tpUrl);
  AddParam('since_id', '24012619984051000', tpUrl);
  AddParam('max_id', '250126199840518145', tpUrl);
  AddParam('result_type', 'mixed', tpUrl);
  AddParam('count', '4', tpUrl);
  AddParam('oauth_consumer_key',oauth_consumer_key, tpAuth);
  AddParam('oauth_nonce',sNonce, tpAuth);
  AddParam('oauth_signature_method','HMAC-SHA1', tpAuth);
  AddParam('oauth_timestamp',INTTOSTR(DateUtils.DateTimeToUnix(now)), tpAuth);
  AddParam('oauth_token',oauth_access_token, tpAuth);
  AddParam('oauth_version','1.0', tpAuth);

  while not sortpass do ;

  sURL := sBaseURL+'?'+GetURLParams;

  sigseed := GetsigSeed;
  sigseed := 'GET&'+EncodeWebString(sURL)+'&'+EncodeWebString(sigseed);

  signkey := EncodeWebString(oauth_consumer_secret)+'&'+oauth_access_token_secret;

  //AddParam('oauth_signature',adfasdf, tpAuth);//<<--- not included because this is the final sig
  sig := THMACUtils<TIDHMacSha1>.HMAC_Base64(signkey, sigseed);

  authparams := GetAuthParams+',oauth_signature="'+EncodeWebString(sig)+'"';
//  authparams := GetAuthParams+',oauth_signature="'+sig+'"';
  if QuickHTTPSGet(sURL, s, 'Authorization', 'OAuth '+authparams) then begin
    result := StrToJSONh(s);
  end else
    Debug.Log('ERROR! '+s);



end;

{ TTwitterEvilRobot }

procedure TTwitterEvilRobot.Init;
begin
  inherited;


  oauth_consumer_key:= 'yLfW78oDKMUEpC249wNMAMmNI';
  oauth_consumer_secret := 'VKsNFKTPw1xLMJxLUToi0H5xuUORitsI98CZ9q3OTJ4FDYBvH4';
  oauth_access_token := '45846522-q63snH6emJxyLJNx5g2oZZKYRLSDFbzm4JHNLAZ2J';
  oauth_access_token_secret := 'Yg7Ky1ZgmviFlnXx757QNkU5PFXgiTwSopMsafKDnb3NT';

end;

end.
