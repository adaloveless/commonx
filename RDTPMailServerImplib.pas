unit RDTPMailServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPMailRQs.txt}
{END}
{//$DEFINE DISABLE_ME}
interface

uses miscroutines, WebString, shareddebuglog, stringutilities, spam, RDTPProcessor, beeper, bayesian, sysutils, classes, RDTPMailServer;


type
  TMailServer = class(TMailServerBase)
  public
{INTERFACE_START}
    function RQ_GetBlacklistStatus(sIP:string; bFullCheck:boolean):integer;overload;override;
    procedure RQ_Blacklist(sIP:string; sReason:string; iCode:integer; iDays:integer);overload;override;
    function RQ_GetSpamProbability(sText:string):real;overload;override;
    function RQ_BayesianCommit(sText:string; bSpam:boolean; iMultiplier:real):boolean;overload;override;
    procedure RQ_Debug(sText:string; bSay:boolean);overload;override;
    function RQ_GetBlacklistCountIncludingContent(sIP:string; sPLainText:string; sHTMLText:string; bFullCheck:boolean):integer;overload;override;
    procedure RQ_SetWordCommonality(sWord:string; iCommonality:int64);overload;override;
    procedure RQ_MailCommand(sSubject:string; sBody:string);overload;override;

{INTERFACE_END}
  end;


implementation


function TMailServer.RQ_GetBlacklistStatus(sIP:string; bFullCheck:boolean):integer;
var
  sc: TSpamChecker;
begin
  sc := TSpamChecker.create;
  try
    beeper.BeepArray([555,777,666], [77,77,77]);
    result := sc.GetnumberOfBlacklists(sIP, bFullcheck);
    beeper.BeepArray([666,555,444], [77,77,77]);
  finally
    sc.free;
  end;
end;

procedure TMailServer.RQ_Blacklist(sIP:string; sReason:string; iCode:integer; iDays:integer);
var
  sc: TSpamChecker;
begin
  sc := TSpamChecker.create;
  try
    sc.Blacklist(sIP, sReason, iCode, iDays);
  finally
    sc.free;
  end;
end;

function TMailServer.RQ_GetSpamProbability(sText:string):real;
begin
  {$IFDEF DISABLE_ME}
  result := 0;
  exit;
  {$ENDIF}
  result := baydict.GetTextSpamProbability(sText);
end;

function TMailServer.RQ_BayesianCommit(sText:string; bSpam:boolean; iMultiplier:real):boolean;
begin
  {$IFDEF DISABLE_ME}
  result := true;
  exit;
  {$ENDIF}
  baydict.AsyncCommitText(sText, bSpam, iMultiplier);
  result := true;
end;


procedure TMailServer.RQ_Debug(sText:string; bSay:boolean);
begin
  GLOG.Debug('*************'+sText);
//  if (bSay) then
//    SayNatural(sText, true);
end;

function TMailServer.RQ_GetBlacklistCountIncludingContent(sIP:string; sPLainText:string; sHTMLText:string; bFullCheck:boolean):integer;
var
  sc: TSpamChecker;
  sl: TStringList;
  t: integer;
  sTemp: string;
  sHost, sRest: HTTPString;
begin
  sl := TStringList.create;
  try
    sc := TSpamChecker.create;
    try
      sl.text := ExtractLinks('', sHTMLTExt);


      sTemp := sl.text;
      sl.clear;
      sl.text := ExtractLinksFromPlainText(sPlainText);
      sl.text := sl.text + #13#10 + sTemp;

      for t:= sl.count-1 downto 0 do begin
        if trim(sl[t]) = '' then begin
          sl.delete(t);
          continue;
        end;
        DecodeURL(sl[t], sHost, sREst);
        sl[t] := DNSLookup(shost);

      end;
      sl.add(sIP);
      RemoveDuplicatesfromStringLIst(sl);

      result := 0;
      for t:= 0 to sl.count-1 do begin
        result := result + sc.GetnumberOfBlacklists(sl[t], bFullcheck);
      end;

    finally
      sc.free;
    end;
  finally
    sl.free;
  end;
end;

procedure TMailServer.RQ_SetWordCommonality(sWord:string; iCommonality:int64);
begin
  baydict.SetWordCommonality(sWord, iCommonality);

end;

procedure TMailServer.RQ_MailCommand(sSubject:string; sBody:string);
var
  sl: TStringlist;
  scmd: string;
  t: integer;
begin
  sl := nil;
  try
    try
      sl := SplitStringintoStringList(sSubject, ' ', '"');
      TrimStringList(sl);
      if sl.count < 1 then begin
        saynatural('Command was not found');
        exit;
      end;

      sCmd := trim(lowercase(sl[0]));
      if sCmd = 'setcommonality' then begin
        saynatural('received command to force word commonality');
        for t:= 2 to sl.count-1 do begin
          saynatural(sl[t]);
          baydict.SetWordCommonality(sl[t], strtoint64(sl[1]));
        end;

      end else
      if (sCmd = 'forcewordstats') or (sCmd = 'forcewordstats') then begin
        saynatural('received command to force word statistics');
        saynatural(sl[1]);
        for t:= 3 to sl.count-1 do begin
          saynatural(sl[t]);
          baydict.ForceWordStat(sl[t], strtoint64(sl[1]),strtoint64(sl[2]));
        end;
      end else
        saynatural('command '+sCmd+' was not recognized');

    except
      on e: exception do begin
        saynatural('there was an error processing the mail command');
        saynatural(e.message);
      end;
    end;


  finally
    sl.free;
  end;

end;


end.
