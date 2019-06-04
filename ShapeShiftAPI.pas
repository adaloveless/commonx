unit ShapeShiftAPI;

interface

uses
  rdtpdb, stringx, classes, sysutils, typex, HTTPS, commandprocessor, jsonhelpers, CCBridge, numbers;


type
  TMarketinfo = record
  public
    BaseCoin: string;
    MarketCoin: string;
    Pair: string;
    Rate: double;
    Limit: double;
    Min: double;
    MinerFee: double;
    procedure FromJSONString(s: string);
  end;

  TMarketInfos = record
  public
    a: array of TMarketInfo;
    procedure FromJSONString(s: string);
    function GetFromAPI: boolean;
  end;

  TMarketTransaction = record
    timestamp: double;
    ccFrom: string;
    ccto: string;
    amount: double;
    procedure FromJSONString(s: string);
  end;

  TMarketTransactions = record
  public
    a: array of TMarketTransaction;
    function GetFromAPI: boolean;
    procedure FromJSONString(s: string);
  end;



  TShift = class(TCommand)
  public
    slLog: TStringlist;
    fromCoin: string;
    toCoin: string;
    fromAddr: string;
    toAddr: string;
    comment: string;
    returnAddress: string;
    amtFrom: double;
    amtFromRemainingToShift: double;
    sourceWallet: TCCBridgeAbstract;
    memo: string;
    minConvert: double;
    maxConvert: double;
    function BeginShiftPart(): boolean;
    procedure waitForShift;
    procedure DoExecute;override;
    procedure Log(s: string);
    procedure Init; override;
    procedure Detach; override;
  end;








implementation

{ TMarketinfo }

procedure TMarketinfo.FromJSONString(s: string);
var
  s1,s2: string;
  sl: TStringlist;
  t: ni;
begin
  SplitString(s, '{', s1,s2);
  SplitString(s2, '}', s1,s2);
  sl := SplitStringIntoStringList(s1, ',', '"');
  try
    for t:= 0 to sl.Count-1 do begin
      SplitString(sl[t], ':', s1,s2);
      s1 := Unquote(trim(s1));
      SplitString(s2,',' ,s2,s);
      s2 := unquote(trim(s2));
      if lowercase(s1)='pair' then begin
        self.Pair := s2;
      end;
      if lowercase(s1)='rate' then begin
        self.Rate := strtofloatex(s2);
      end;
      if lowercase(s1)='limit' then begin
        self.Limit := strtofloatex(s2);
      end;
      if lowercase(s1)='min' then begin
        self.Min := strtofloatex(s2);
      end;
      if lowercase(s1)='minerfee' then begin
        self.Minerfee := strtofloatex(s2);
      end;
    end;
  finally
    sl.free;
  end;


end;



{ TMarketInfoArray }

procedure TMarketInfos.FromJSONString(s: string);
var
  s1,s2: string;
  mi: TmarketInfo;
begin
  splitString(s, '[', s1,s2);
  while splitString(s2,'}',s1,s2) do begin
    mi.FromJSONString(s1+'}');
    setlength(a, length(a)+1);
    a[high(a)] := mi;
  end;

end;

function TMarketInfos.GetFromAPI: boolean;
var
  s: string;
begin
  result := QuickHTTPSGet('https://shapeshift.io/marketinfo/', s);
  if result then begin
    self.FromJSONString(s);
  end;
end;

{ TShift }

procedure TShift.Detach;
begin
  if detached then exit;

  slLog.free;
  slLog := nil;

  inherited;

end;

procedure TShift.DoExecute;
var
  t: ni;
begin
  inherited;
  amtFromRemainingToShift := amtFrom;

  if minConvert = 0 then
    raise ECritical.create('set minConvert');
  if maxConvert = 0 then
    raise ECritical.create('set maxConvert');
  if amtFrom = 0 then
    raise ECritical.create('set amtFrom');
  if fromCoin = '' then
    raise ECritical.create('set fromCoin');
  if returnAddress = '' then
    raise ECritical.create('set returnAddress');
  if toCoin = '' then
    raise ECritical.create('set toCoin');
  if sourceWallet = nil then
    raise ECritical.create('set sourceWallet');




  //submit transactions in parts
//  setlength(shifts, 0);
  while amtFromRemainingToShift > 0 do begin
    BeginShiftPart;
//    setlength(shifts, length(shifts)+1);
//    shifts[high(shifts)] := c;
  end;

  //wait for all commands
{  StepCount := high(shifts);
  for t:= 0 to high(shifts) do begin
    shifts[t].WaitFor;
    shifts[t].free;
  end;}


end;

procedure TShift.Init;
begin
  inherited;
  slLog := Tstringlist.create;
end;

procedure TShift.Log(s: string);
begin
  slLog.add(s);

end;

procedure TShift.waitForShift;
var
  bCompletE: boolean;
  dict: TJSON;
  s: string;
begin
  inherited;


        (*

        url: shapeshift.io/txStat/[address]
        method: GET
        [address] is the deposit address to look up.

        Success Output:  (various depending on status)
        Status: No Deposits Received
            {
                status:"no_deposits",
                address:[address]           //matches address submitted
            }
        Status: Received (we see a new deposit but have not finished processing it)
            {
                status:"received",
                address:[address]           //matches address submitted
            }
        Status: Complete
        {
            status : "complete",
            address: [address],
            withdraw: [withdrawal address],
            incomingCoin: [amount deposited],
            incomingType: [coin type of deposit],
            outgoingCoin: [amount sent to withdrawal address],
            outgoingType: [coin type of withdrawal],
            transaction: [transaction id of coin sent to withdrawal address]
        }
        Status: Failed
        {
            status : "failed",
            error: [Text describing failure]
        }
        *)

  bComplete := false;

  while not bComplete do begin
    if QuickHTTPSGet('https://shapeshift.io/txStat/'+toAddr, s) then begin
      dict := nil;
      try
        dict := JSONtoDictionary(s);
        Log('***Waiting for shift to complete***');
        Log(s);
        if dict.TryGetValue('status', s) then begin
          bComplete := s = 'complete';

        end;

      finally
        dict.free;
        dict := nil;
      end;

    end;

    if not bComplete then
      sleep(10000);
  end;

end;

function TShift.BeginShiftPart(): boolean;
var
  spost: string;
  sResult: string;
  dict: TJSON;
  sDep: string;
  sDepCoin: string;
  toSend: double;
  sMax: string;
  sMin: string;
  s: string;
begin
  RESULT := true;
  //setup trade
  sPost := '{"withdrawal":"'+toAddr+'", "pair":"'+lowercase(fromCoin)+'_'+lowercase(tocoin)+'", "returnAddress":"'+returnAddress+'"}';
  sResult := QuickHTTPSPostOld('https://shapeshift.io/shift', sPost, 'text/json');
  dict := nil;
  try
    //send to address
    dict := JsontoDictionary(sResult);
{ EXPECTED RESULTS
    {
        deposit: [Deposit Address (or memo field if input coin is BTS / BITUSD)],
        depositType: [Deposit Type (input coin symbol)],
        withdrawal: [Withdrawal Address], //-- will match address submitted in post
        withdrawalType: [Withdrawal Type (output coin symbol)],
        public: [NXT RS-Address pubkey (if input coin is NXT)],
        xrpDestTag : [xrpDestTag (if input coin is XRP)],
        apiPubKey: [public API attached to this shift, if one was given]
    }
//}
    if not dict.TryGetValue('depositType', sDepCoin) then begin
      slLog.add('FAILED /shift reponse : '+sResult);
    end else begin
      if lowercase(sDepCoin) <> lowercase(fromcoin) then begin
        raise ECritical.create('expected coin '+fromcoin+' but got '+sDepCoin);
      end else begin
        toSend := lesserof(amtFromRemainingToShift, maxConvert);
        dict.TryGetValue('deposit', sDep);
        sourceWallet.Send(sDep, toSend, memo);
        WaitForShift;
      end;
    end;
  finally
    dict.free;
    dict := nil;
  end;


end;

{ TShiftWatch }




{ TMarketTransactions }

procedure TMarketTransactions.FromJSONString(s: string);
var
  s1,s2: string;
  mt: TMarketTransaction;
begin
  splitString(s, '[', s1,s2);
  while splitString(s2,'}',s1,s2) do begin
    mt.FromJSONString(s1+'}');
    setlength(a, length(a)+1);
    a[high(a)] := mt;
  end;
end;

function TMarketTransactions.GetFromAPI: boolean;
var
  s: string;
begin
  result := QuickHTTPSGet('https://shapeshift.io/recenttx/50', s);
  if result then begin
    self.FromJSONString(s);
  end;
end;

{ TMarketTransaction }

procedure TMarketTransaction.FromJSONString(s: string);
var
  s1,s2: string;
  sl: TStringlist;
  t: ni;
begin
  SplitString(s, '{', s1,s2);
  SplitString(s2, '}', s1,s2);
  sl := SplitStringIntoStringList(s1, ',', '"');
  try
    for t:= 0 to sl.Count-1 do begin
      SplitString(sl[t], ':', s1,s2);
      s1 := Unquote(trim(s1));
      SplitString(s2,',' ,s2,s);
      s2 := unquote(trim(s2));
      if lowercase(s1)='curin' then begin
        self.ccFrom := s2;
      end;
      if lowercase(s1)='curout' then begin
        self.ccTo := s2;
      end;
      if lowercase(s1)='amount' then begin
        self.amount := strtofloatex(s2);
      end;
      if lowercase(s1)='timestamp' then begin
        self.timestamp := strtofloatex(s2);
      end;
    end;
  finally
    sl.free;
  end;
end;

end.
