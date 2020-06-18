unit spam;

interface

uses
  helpers.list, systemx, netx,tickcount, speech, sharedobject, debug, beeper, Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs, exe, stringx, RDTPSpamPacketHelpers,orderlyinit,
  ExtCtrls, dir, dirfile, betterobject, mailreader,IdDNSResolver, managedthread, commandprocessor, winsock, RDTPMailClient, memoryfilestream;

const
  MAILROOT = 'V:\mailroot\';
  MODERATE_BLACK_LIST_THRESHOLD = 1;
  SEVERE_BLACK_LIST_THRESHOLD = 2;
  GEORGE_BUSH_BLACK_LIST_THRESHOLD = 4;
  BLACKLIST_CHECK_TIMEOUT = 480000;
const
  CODE_HONEYPOT = 100;
  BAN_TIME_HONEYPOT = 14;
  CODE_UNREGISTERED = 101;
  BAN_TIME_UNREGISTERED = 30;
  CODE_MALFORMED = 102;
  BAN_TIME_MALFORMED = 7;
  CODE_BY_PROXY = 103;
  BAN_TIME_BY_PROXY = 2;


type
  TBlackListServers = class(TFakeLockQueuedObject)
  private
    FWeedTime: TDateTime;
    function GetWeedTime: TDateTime;
    procedure SEtWeedTime(const Value: TDateTime);
    function GetBls(idx: integer): string;
  protected
    bls: TStringlist;
  public
    constructor create;override;
    destructor destroy;override;
    property WeedTime: TDateTime read GetWeedTime write SEtWeedTime;
    property items[idx: integer]: string read GetBls;default;
    function count: integer;
    procedure Weedout(cp: TCommandProcessor; sWhiteDomain:string);
    procedure remove(s: string);
  end;

  TSpamChecker = class(TBetterObject)
  private
    mr2,mr1: TMailReader;
    Fhoneypot: string;
    bls: TStringlist;
    FBlackListReport: TStringlist;
    FReason: string;
    Fmailclient: TMailClient;
    BGCmd: TcommandProcessor;
    FUseRemoteService: boolean;
    function HasMXRecordWithDomain(sCheckingDomain, sDomain: string): boolean;
    function ResolveAllIPs(sDomain: string): string;
    function GetBlackListReport: string;
    function GEtRDTPMailClient: TMailClient;
    procedure CheckSingleMessageOld(sFileName: string; out bIsSpam: boolean;
      out sReason: string);

  public
    constructor Create;override;
    destructor Destroy;override;
    procedure WeedOutBadBlacklists(sDomain: string);
    function GetnumberOfBlacklists(sIP: string;
      bCheckAll: boolean = false): integer;overload;

    procedure CheckAllMailboxes(sDomainRoot: string);
    procedure CheckMailroot(sMailRoot: string);
    procedure CheckSpam(sMailbox: string);
    procedure CheckSingleMessage(sFileName: string; sTextContent: string; sHTMLContent: string; out iBlackListCount: integer; out sReason: string; out ACtions: TSpamACtions);
    function CheckIfNoSMTPDomain(sFile: string; sMail: string): boolean;
    procedure AlterSpam(sFile: string; bSpam: boolean; sspamFlag: string);
    function ResolveIP(sDomain: string): string;
    function HasMXRecordWithIP(sIP: string; sDomain: string): boolean;
    property honeypot: string read Fhoneypot write Fhoneypot;
    procedure BlackList(sIP: string; sReason: string; iCode, iDays: integer);
    property BlackListReport: string read GetBlackListReport;
    function GetIPFromReceivedLine(sLine: string): string;
    property Reason: string read FReason write FReason;
    property MailClient: TMailClient read GEtRDTPMailClient;
    property UseRemoteService: boolean read FUseRemoteService write FUseRemoteService;
  end;


  TCmd_CheckIfBlackListed = class (TCommand)
  strict private
    FService: string;
    FIP: string;
    FResult: boolean;
  strict protected
    function checkIfBlacklisted(sIP: string; sUsingService: string): boolean;
    function checkIfBlacklistedOld(sIP: string; sUsingService: string): boolean;
  public
    constructor Create;override;
    procedure InitExpense;override;
    procedure DoExecute;override;
    property IP: string read FIP write FIP;
    property Service: string read FService write FService;
    property Result: boolean read FResult write FREsult;

  end;


  TBLRecord = class(TObject)
  private
    FBLs: integer;
    FIP: string;
  public
    property IP: string read FIP write FIP;
    property NumberOfBLs: integer read FBLs write FBLs;
  end;

  TBLCache = class(TLockQueuedObject)
  protected
    FList: TList;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Add(sIP: string; iBLs: integer);
    function GetCachedREsult(sIP: string): integer;
  end;

var
  BLCache: TBLCache;

implementation


procedure TSpamChecker.AlterSpam(sFile: string; bSpam: boolean;
  sspamFlag: string);
var
  s, sLeft, sRight: string;
  iLength: integer;
  fs1,fs2: TMemoryFileStream;
  a: array[0..8000] of byte;
  iRead: integer;
  t: integer;
begin
  Reason := sSpamFlag;
  try
    try
      //SayNatural(sSpamFlag, true);
    except
    end;
    //load the file
    s := LoadStringFromFile(sFile);
    if FileExists(sfile+'.temp') then begin
      Deletefile(sfile+'.temp');
    end;

    //if already spam then exit
    if SplitStringNoCase(s, 'mail-validator:', sLeft, sRight) then begin
      exit;
    end;

    //change the subject of the file
    if SplitStringNoCase(s, 'Subject:', sLeft, sRight) then begin

      iLength := length(sLeft+'Subject:');
      fs1 := nil;
      fs2 := nil;
      try
        fs1 := TMemoryFileStream.create(sfile, fmopenRead);
        fs2 := TMemoryFileStream.create(sfile+'.temp', fmCreate);

        if not bSpam then begin
          s := 'Mail-validator: OK'#13#10;
          for t := 1 to length(s) do begin
            a[t-1] := ord(s[t]);
          end;
          a[length(s)] := 0;
          fs2.Write(a, length(s));


        end else begin
          s := 'Mail-validator: Spam ('+sSpamflag+')'#13#10;
          for t := 1 to length(s) do begin
            a[t-1] := ord(s[t]);
          end;
          a[length(s)] := 0;
          fs2.Write(a, length(s));


          iRead :=  fs1.Read(a, iLength);
          fs2.Write(a, iRead);

          s := 'Spam ('+sSpamflag+')';
          for t := 1 to length(s) do begin
            a[t-1] := ord(s[t]);
          end;
          a[length(s)] := 0;


          fs2.Write(a, length(s));
        end;
        repeat
          iRead := fs1.read(a, 8000);
          if (iRead > 0 ) then
            fs2.Write(a, iRead);
        until iRead = 0;

//        beeper.beep(777,100);
      finally
        fs1.free;
        fs2.free;
      end;

      Deletefile(sfile);
      while true do begin
        try
          if (fileexists(sfile+'.temp')) then
            RenameFile(sfile+'.temp', sFile);
          break;
        except
          sleep(1000);
        end;
      end;


      //RenameFile(sfile+'.temp', sfile);

    end;

  finally
  end;


end;

procedure TSpamChecker.BlackList(sIP: string; sReason: string; iCode, iDays: integer);
var
  sl: TStringlist;
begin
  if iCode < 2 then iCode := 2;
  if iCode > 254 then iCode := 254;

  if UseRemoteService then begin

    MailClient.Blacklist(sIP, sReason, iCode, iDays);

  end else begin

    sl := tStringlist.create;
    try
      try
        sIP := ReverseIP(sIP);
        //exe.RunProgramAndWait('dnscmd', '/recordadd digitaltundra.com '+sIP+'.blacklist'+' A 127.0.0.'+inttostr(iCode), '', true, true);
        //exe.RunProgramAndWait('dnscmd', '/recordadd digitaltundra.com '+sIP+'.blacklist'+' TXT "'+datetimetostr(now)+'-'+sReason+'"', '', true, true);

        try
          //sl.LoadFromfile(DLLPath+'blacklist.txt');
          //sl.add(sIP+','+datetimetostr(now));
          //sl.SaveToFile(DLLPath+'blacklist.txt');
        except
        end;

      except
      end;

    finally
      sl.free;
    end;
  end;


end;

procedure TSpamChecker.CheckAllMailboxes(sDomainRoot: string);
var
  dir: Tdirectory;
  t: integer;
begin
  dir := Tdirectory.create(sdomainRoot, '*.*', 0,0, false);
  try
    for t:= 0 to dir.foldercount-1 do begin
      self.CheckSpam(dir.folders[t].fullname);
    end;
  finally
    dir.free;
  end;

end;


function TSpamChecker.CheckIfNoSMTPDomain(sFile, sMail: string): boolean;
var
  s,sLeft, sright: string;
begin
  result := false;

(*  mr1.content := sMail;
  s := mr1.Headers['received'];
  if (pos('tls secured channel', s) > 0) then begin
    result := false;
    exit;
  end;

  if SplitString(s, '([', sLeft, sright) then begin
    if SplitString(sLeft, '[', sLeft, sright) then begin
      AlterSpam(sFile, true, 'NO SMTP DOMAIN');
      if (SplitString(sRight, ']', sLeft, sRight)) then begin
        BlackList(sLeft, 'SMTP Server had no DNS entry', CODE_UNREGISTERED, CODE_UNREGISTERED);
      end;
      result := true;
      exit;
    end;
  end;

  result := false;
end;

function TSpamChecker.CheckIfSenderDomainIsSMTPMX(sFile,
  sMail: string): boolean;

var
  sLeft, sRight, sWho, s, sFrom, sFromDomain, sReceivedFrom: string;
  iTemp: integer;
  t: integer;
  act: TSpamActions;
begin
  mr1.content := sMail;

  //Find From
  s := mr1.Headers['From'];

  //if sent to hp@digitaltundra.com then blacklist it

  if (pos('tls secured channel', s) > 0) then begin
    result := false;
    exit;
  end;



  if not SplitString(s, '<', sLeft, sRight) then begin
    sFrom := sLeft;
    sWho := sFrom;
  end else
  if not SplitString(sRight, '>', sFrom, sRight) then begin
    AlterSPam(sFile, true, 'Malformed FROM address');
    BlackLIst(self.GetIPFromReceivedLine(mr1.Headers['Received']), 'Malformed: FROM Address', CODE_MALFORMED, BAN_TIME_MALFORMED);
    result := true;
    exit;
  end;



  if not SplitString(sFrom, '@', sLEft, sFromDomain) then begin
    AlterSPam(sFile, true, 'Malformed FROM address -- missing ''@''');
    BlackLIst(self.GetIPFromReceivedLine(mr1.Headers['Received']), 'Malformed: FROM Address missing @', CODE_MALFORMED, BAN_TIME_MALFORMED);
    result := true;
    exit;
  end;



//  s := mr1.Headers['Received'];
//  if not SplitString(s, ' by ', sLeft, sRight) then begin
//    AlterSPam(sFile, true, 'Malformed RECEIVED '' by '' Header');
//    result := true;
//    exit;
//  end;
//
//  if not SplitString(sRight, ' with ', sReceivedFrom, sRight) then begin
//    AlterSpam(sFile, true, 'Malformed RECEIVED '' with '' Header');
//    result := true;
//    exit;
//  end;


  for t:= 0 to 0 do begin
    if t> 0 then begin
      s := mr1.Headers['Received'+inttostr(t)];
    end else begin
      s := mr1.Headers['Received'];
    end;
    if s='' then continue;
    if not SplitString(s, '([', sLeft, sRight) then begin
      AlterSPam(sFile, true, 'No MX '' ([ '' Header');
      if SplitString(s, '[', sLeft, sRight) then begin
        if SplitString(sRight, ']', sLeft, sRight) then begin
          BlackList(sLeft, 'Missing MX entry', CODE_UNREGISTERED, BAN_TIME_UNREGISTERED);
        end;
      end;
      result := true;
      exit;
    end;

    if not SplitString(sRight, '])', sReceivedFrom, sRight) then begin
      AlterSpam(sFile, true, 'No MX '' ]) '' Header');
      result := true;
      exit;
    end;
    SayNatural('The message lists a proper MX sender', true);


    {$IFDEF LOCAL}
    iTemp := self.GetnumberOfBlacklists(sIP);
    {$ELSE}
    iTemp := self.MAilClient.GetBlacklistStatus(sIP, false);
    {$ENDIF}
    if  iTEmp >= MODERATE_BLACK_LIST_THRESHOLD then begin
      SayNatural('The sender was blacklisted', true);
      AlterSpam(sFile, true, sReceivedFrom+' was blacklisted by '+inttostr(iTemp)+' services') ;
      BlackList(sReceivedFrom, sReceivedFrom+' was blacklisted by '+inttostr(iTemp)+' services', CODE_BY_PROXY, BAN_TIME_BY_PROXY);

      result := true;
      exit;
    end else begin
      SayNatural('The sender was not blacklisted', true);
    end;
  end;


//   if not HasMXRecordWithIP(ResolveAllIPs(sReceivedFrom), sFromDomain) then begin
//    AlterSpam(sFile, true, 'MX for '+sFromDomain+' does not match RECEIVED IP'+sReceivedFRom);
//    result := true;
//    exit;
//  end;


  result := false;


*)
end;


procedure TSpamChecker.CheckMailroot(sMailRoot: string);
var
  dir: Tdirectory;
  t: integer;
begin
  dir := Tdirectory.create(sMailRoot, '*.*', 0,0, false);
  try
    for t:= 0 to dir.foldercount-1 do begin
      self.CheckAllMailBoxes(dir.folders[t].fullname);
    end;
  finally
    dir.free;
  end;

end;

procedure TSpamChecker.CheckSingleMessage(sFileName: string; sTextContent: string; sHTMLContent: string;
  out iBlackListCount:integer; out sReason: string; out Actions: TSpamActions);
var
  t: integer;
  sSpamReference: string;
  sMail: string;
  s1,s2: string;
  s, sLeft, sRight: string;
  sRecieved: string;
  sIP: string;
  iTemp: integer;
  sFrom: string; sWho: string;
  sFromDomain: string;
  sReceivedFrom: string;
begin
  iBlacklistCount := 0;
  sReason := '';
  sMail := LoadStringFromFile(sfileName);
  Actions := 0;

  //check if it is addressed to the honeypot
  self.mr1.content := LoadStringFromFile(sFileName);

  //if subject contains "BanIP:" then spam it
  if pos('banip:',lowercase(mr1.Headers['subject'])) > 0 then begin
    if splitstringnocase(mr1.headers['subject'], 'banip:', s1,s2) then begin
      splitStringNoCase(s2, ' ', s1,s2);
      BlackList(s1, 'MANUALLY ADDED', 99, 30);
      iBlackListCount := 0;
      Actions := saHandled;
      exit;
    end;
  end;



  //if NOT received on authenticated channel
  s := mr1.Headers['received'];

  if s = '' then begin
    iBlackListCount := 1;
    sREason := 'Missing Received Header';
    Actions := saDelete+saMark+saDivert;
    exit;
  end;
  //whitelist words
  if (pos('PIXIEDUST', uppercase(s)) > 0)
  or (pos('secured', lowercase(s)) > 0)then begin
    iBlackListCount := 0;
    exit;
  end;

  if (pos('tls secured channel', lowercase(s)) = 0) then begin

    //check if IP is whitelisted.
    sIP := GEtIPFromReceivedLine(s);
    if (pos('127.0.0.', sIP) = 1)
    or (pos('10.', sIP) = 1)
    or (pos('192.168.', sIP) = 1)
    or (pos('71.215.252.2', sIP) = 1)
    then begin
      iBlackListCount := 0;
      exit;
    end;


    //check if there is a DNS listing for the IP address
    if SplitString(s, '([', sLeft, sright) then begin
      if SplitString(sLeft, '[', sLeft, sright) then begin
        sREason := 'NO SMTP DOMAIN';
        AlterSpam(sFileName, true, sReason);
        if (SplitString(sRight, ']', sLeft, sRight)) then begin
          BlackList(sLeft, 'SMTP Server had no DNS entry', CODE_UNREGISTERED, CODE_UNREGISTERED);
        end;

        ACtions := saCritical;
        sReason := 'SMTP Server had no DNS entry';
        iBlackListCount := 1;
        exit;
      end else begin
        //make sure there is a '.' in the name somewhere (unless it comes from a secure channel)
//        if (pos('.', sLeft) < 1) then begin
//          bIsSpam := true;
//          sReason := 'Unsecured sender not whitelisted or member of root domain';
//          ACtions := saCritical;
//          exit;
//        end;
      end;
    end;
  end;

  if pos('hp@digitaltundra.com', lowercase(mr1.Headers['to'])) > 0 then begin
    sREason := 'Honeypot trap';
    AlterSPam(sFileName, true, sReason);
    BlackLIst(self.GetIPFromReceivedLine(mr1.Headers['Received']), 'Honeypot trapped mail from this address', CODE_HONEYPOT, BAN_TIME_HONEYPOT);
    ACtions := saCritical;
    iBlackListCount := 1;
    sReason := 'Honeypot Trap';
    exit;
  end;

  //Find From
  s := mr1.Headers['From'];

  //if sent to hp@digitaltundra.com then blacklist it


  if not SplitString(s, '<', sLeft, sRight) then begin
    sFrom := sLeft;
    sWho := sFrom;
  end else
  if not SplitString(sRight, '>', sFrom, sRight) then begin
    sREason := 'Malformed FROM address';
    AlterSPam(sFileName, true, sReason);
    BlackLIst(self.GetIPFromReceivedLine(mr1.Headers['Received']), 'Malformed: FROM Address', CODE_MALFORMED, BAN_TIME_MALFORMED);
    ACtions := saCritical;
    inc(iBlackListCount);
    exit;
  end;



  if not SplitString(sFrom, '@', sLEft, sFromDomain) then begin
    sReason := 'Malformed FROM address -- missing ''@''';
    AlterSPam(sFileName, true, sREason);
    BlackLIst(self.GetIPFromReceivedLine(mr1.Headers['Received']), 'Malformed: FROM Address missing @', CODE_MALFORMED, BAN_TIME_MALFORMED);
    ACtions := saCritical;
    inc(iBlackListCount);
    exit;
  end;



//  s := mr1.Headers['Received'];
//  if not SplitString(s, ' by ', sLeft, sRight) then begin
//    AlterSPam(sFile, true, 'Malformed RECEIVED '' by '' Header');
//    result := true;
//    exit;
//  end;
//
//  if not SplitString(sRight, ' with ', sReceivedFrom, sRight) then begin
//    AlterSpam(sFile, true, 'Malformed RECEIVED '' with '' Header');
//    result := true;
//    exit;
//  end;


  for t:= 0 to 0 do begin
    if t> 0 then begin
      s := mr1.Headers['Received'+inttostr(t)];
    end else begin
      s := mr1.Headers['Received'];
    end;
    if s='' then continue;


    //the message must always have a ([x.x.x.x]) in it
    if not SplitString(s, '([', sLeft, sRight) then begin
      sReason := 'Bad REceived Header '' ([ ''' ;
      AlterSPam(sFileNAme, true, sReason);
      if SplitString(s, '[', sLeft, sRight) then begin
        if SplitString(sRight, ']', sLeft, sRight) then begin
          BlackList(sLeft, sReason, CODE_UNREGISTERED, BAN_TIME_UNREGISTERED);
        end;
      end;
      inc(iBlackListCount);
      ACtions := saModerate;
      exit;
    end;


    //complete the split at ]) we should now have the IP
    if not SplitString(sRight, '])', sReceivedFrom, sRight) then begin
      sREason := 'Bad REceived Header '' ]) '' ';
      AlterSPam(sFileNAme, true, sReason);

      ACtions := saModerate;
      inc(iBlackListCount);
      exit;
    end;

    {$IFDEF LOCAL}
    iTemp := self.GetnumberOfBlacklists(sIP);
    {$ELSE}
    iTemp := self.MAilClient.GetBlacklistCountIncludingContent(sIP, sTextContent, sHTMLContent, false);
    {$ENDIF}
    if  iTEmp >= MODERATE_BLACK_LIST_THRESHOLD then begin
      sReason := sReceivedFrom+' was blacklisted by '+inttostr(iTemp)+' services';
      AlterSpam(sFileName, true, sReason) ;
      if iTemp >= GEORGE_BUSH_BLACK_LIST_THRESHOLD then begin
        BlackList(sReceivedFrom, sReceivedFrom+' was blacklisted by '+inttostr(iTemp)+' services', CODE_BY_PROXY, BAN_TIME_BY_PROXY);
        ACtions := saGeorgeBush;
      end else
      if iTemp >= SEVERE_BLACK_LIST_THRESHOLD then begin
        BlackList(sReceivedFrom, sReceivedFrom+' was blacklisted by '+inttostr(iTemp)+' services', CODE_BY_PROXY, BAN_TIME_BY_PROXY);
        ACtions := saCritical;
      end else begin
        Actions := saModerate;
      end;

      iBlackListCount := iTemp;
      exit;
    end else begin
      //SayNatural('The sender was not blacklisted', true);
    end;
  end;



end;


procedure TSpamChecker.CheckSingleMessageOld(sFileName: string;
  out bIsSpam: boolean; out sReason: string);
begin

  raise Exception.create('unimplemented');
end;


procedure TSpamChecker.CheckSpam(sMailbox: string);
var
  dirPot, dir : TDirectory;
  t,u:integer;
  sSPamReference: string;
  smail: string;
  bGood: boolean;
  lock: TMemoryFileStream;
  sLeft, sright: string;
begin
  bGood := true;
  lock := nil;
//  lock := TMemoryFileStream.create(sMailBox+'Lock', fmOpenWrite, fmShareExclusive);
  try

    dirPot := nil;
    dir := nil;
    try
      dirPot := Tdirectory.create(HoneyPot, '*.eml', 0,0, false);
      dir := Tdirectory.create(sMailBox, '*.eml', 0,0, false);
      for u := 0 to dir.Filecount-1 do begin
        try
          bGood := true;
          sMail := LoadStringFromFile(dir.files[u].FullName);

          if SplitStringNoCase(sMail, 'mail-validator:', sLeft, sRight) then begin
            continue;
          end;


        except
          on e:exception do begin
            if bGood then begin
              AlterSpam(dir.files[u].Fullname, false, 'exception:'+e.message);
            end;
          end;
        end;
      end;

    finally
      dir.free;
      dirPot.free;
    end;
  finally
    lock.Free;
  end;




end;




constructor TSpamChecker.Create;
begin
  inherited;
  BGCmd := TCommandProcessor.create(nil, 'SpamChecker');

  mr1 := TMailReader.create;
  mr2 := Tmailreader.create;
  bls := tStringlist.create;
  FBlackListReport := TStringlist.create;

  //bls.add('blacklist.digitaltundra.com');

  bls.add('list.dsbl.org');
  bls.add('dnsbl.njabl.org');
  bls.add('sbl.spamhaus.org');
  bls.add('xbl.spamhaus.org');
  bls.add('sbl-xbl.spamhaus.org');
  bls.add('bl.spamcop.net');
  bls.add('whois.rfc-ignorant.org');
  bls.add('cbl.abuseat.org');
  bls.add('dnsbl.sorbs.net');
//  bls.add('spam.dnsbl.sorbs.net');
  bls.add('blackholes.five-ten-sg.com');
  bls.add('no-more-funn.moensted.dk');
  bls.add('ucepn.dnsbl.net.au');
  bls.add('psbl.surriel.com');
//  bls.add('bl.spamcannibal.org');
  bls.add('dnsbl-3.uceprotect.net');
  bls.add('dnsbl-2.uceprotect.net');



//  bls.add('3y.spam.mrs.kithrup.com');
//  bls.add('access.redhawk.org');
//  bls.add('all.rbl.kropka.net');
//  bls.add('all.spamblock.unit.liu.se');
//  bls.add('.madscience.nl');
//  bls.add('bl.borderworlds.dk');
//  bls.add('bl.csma.biz');
//  bls.add('bl.redhatgate.com');
//  bls.add('bl.spamcannibal.org');
//  bls.add('bl.spamcop.net');
//  bls.add('bl.starloop.com');
//  bls.add('bl.technovision.dk');
//  bls.add('blackhole.compu.net');
//  bls.add('blackholes.five-ten-sg.com');
//  bls.add('blackholes.intersil.net');
//  bls.add('blackholes.mail-abuse.org');
//  bls.add('blackholes.sandes.dk');
//  bls.add('blackholes.uceb.org');
//  bls.add('blackholes.wirehub.net');
//  bls.add('blacklist.informationwave.net');
//  bls.add('blacklist.sci.kun.nl');
//  bls.add('blacklist.spambag.org');
//  bls.add('block.blars.org');
//  bls.add('block.dnsbl.sorbs.net');
//  bls.add('blocked.hilli.dk');
//  bls.add('blocked.secnap.net');
//  bls.add('blocklist.squawk.com');
//  bls.add('blocklist2.squawk.com');
//  bls.add('blocktest.relays.osirusoft.com');
//  bls.add('bogons.dnsiplists.completewhois.com');
//  bls.add('cart00ney.surriel.com');
//  bls.add('cbl.abuseat.org');
//  bls.add('dev.null.dk');
//  bls.add('dews.qmail.org  dialup.blacklist.jippg.org');
//  bls.add('dialup.rbl.kropka.net');
//  bls.add('dialups.mail-abuse.org');
//  bls.add('dialups.relays.osirusoft.com  dialups.visi.com');
//  bls.add('dnsbl.ahbl.org');
//  bls.add('dnsbl.antispam.or.id');
//  bls.add('dnsbl.cyberlogic.net');
//  bls.add('dnsbl.jammconsulting.com');
//  bls.add('dnsbl.kempt.net');
//  bls.add('dnsbl.njabl.org');
//  bls.add('dnsbl.solid.net  dnsbl.sorbs.net');
//  bls.add('dnsbl-1.uceprotect.net');
//  bls.add('dnsbl-2.uceprotect.net');
//  bls.add('dnsbl-3.uceprotect.net');
//  bls.add('dsbl.dnsbl.net.au');
//  bls.add('duinv.aupads.org');
//  bls.add('dul.dnsbl.sorbs.net');
  bls.add('dul.ru');
//  bls.add('dun.dnsrbl.net');
  bls.add('dynablock.njabl.org');
  bls.add('dynablock.wirehub.net');
  bls.add('fl.chickenboner.biz');
//  bls.add('flowgoaway.com');
  bls.add('forbidden.icm.edu.pl');
  bls.add('form.rbl.kropka.net');
//  bls.add('formmail.relays.monkeys.com');
  bls.add('hijacked.dnsiplists.completewhois.com');
  bls.add('hil.habeas.com');
  bls.add('http.dnsbl.sorbs.net');
  bls.add('http.opm.blitzed.org');
  bls.add('inflow.noflow.org');
//  bls.add('inputs.relays.osirusoft.com');
  bls.add('intruders.docs.uu.se');
  bls.add('ip.rbl.kropka.net');
  bls.add('korea.services.net');
  bls.add('l1.spews.dnsbl.sorbs.net');
  bls.add('l2.spews.dnsbl.sorbs.net');
  bls.add('lame-av.rbl.kropka.net');
//  bls.add('lbl.lagengymnastik.dk');
  bls.add('list.dsbl.org');
  bls.add('mail-abuse.blacklist.jippg.org');
  bls.add('map.spam-rbl.com');
  bls.add('misc.dnsbl.sorbs.net');
  bls.add('msgid.bl.gweep.ca');
  bls.add('multihop.dsbl.org');
//  bls.add('no-more-funn.moensted.dk');
//  bls.add('ohps.bl.reynolds.net.au');
//  bls.add('ohps.dnsbl.net.au');
//  bls.add('omrs.bl.reynolds.net.au');
//  bls.add('omrs.dnsbl.net.au');
//  bls.add('op.rbl.kropka.net');
//  bls.add('opm.blitzed.org');
//  bls.add('or.rbl.kropka.net');
//  bls.add('orbs.dorkslayers.com');
//  bls.add('orid.dnsbl.net.au');
//  bls.add('orvedb.aupads.org');
//  bls.add('osps.bl.reynolds.net.au');
//  bls.add('osps.dnsbl.net.au');
//  bls.add('osrs.bl.reynolds.net.au');
//  bls.add('osrs.dnsbl.net.au');
//  bls.add('outputs.relays.osirusoft.com');
//  bls.add('owfs.bl.reynolds.net.au');
//  bls.add('owfs.dnsbl.net.au');
//  bls.add('owps.bl.reynolds.net.au');
//  bls.add('owps.dnsbl.net.au');
//  bls.add('pdl.dnsbl.net.au');
//  bls.add('pm0-no-more.compu.net');
//  bls.add('ppbl.beat.st');
//  bls.add('probes.dnsbl.net.au');
//  bls.add('proxies.exsilia.net');
//  bls.add('proxies.relays.monkeys.com');
//  bls.add('proxy.bl.gweep.ca');
//  bls.add('proxy.relays.osirusoft.com');
//  bls.add('psbl.surriel.com');
//  bls.add('pss.spambusters.org.ar');
  bls.add('rbl.cluecentral.net');
  bls.add('rbl.rangers.eu.org');
//  bls.add('rbl.rope.net');
  bls.add('rbl.schulte.org');
  bls.add('rbl.snark.net');
  bls.add('rbl.triumf.ca');
  bls.add('rblmap.tu-berlin.de');
  bls.add('rdts.bl.reynolds.net.au');
  bls.add('rdts.dnsbl.net.au');
  bls.add('relays.bl.gweep.ca');
  bls.add('relays.bl.kundenserver.de');
  bls.add('relays.dorkslayers.com');
  bls.add('relays.mail-abuse.org');
  bls.add('relays.nether.net');
//  bls.add('relays.ordb.org');
//  bls.add('relays.osirusoft.com');
  bls.add('relays.visi.com');
  bls.add('relaywatcher.n13mbl.com');
  bls.add('ricn.bl.reynolds.net.au');
  bls.add('ricn.dnsbl.net.au');
  bls.add('rmst.bl.reynolds.net.au');
  bls.add('rmst.dnsbl.net.au');
  bls.add('rsbl.aupads.org');
  bls.add('satos.rbl.cluecentral.net');
//  bls.add('sbbl.they.com  sbl.csma.biz');
//  bls.add('sbl.spamhaus.org');
//  bls.add('sbl-xbl.spamhaus.org');
//  bls.add('smtp.dnsbl.sorbs.net');
//  bls.add('socks.dnsbl.sorbs.net');
//  bls.add('socks.opm.blitzed.org');
//  bls.add('socks.relays.osirusoft.com');
//  bls.add('sorbs.dnsbl.net.au');
//  bls.add('spam.dnsbl.sorbs.net');
  bls.add('spam.dnsrbl.net');
//  bls.add('spam.exsilia.net');
  bls.add('spam.olsentech.net');
  bls.add('spam.wytnij.to');
  bls.add('spamguard.leadmon.net');
//  bls.add('spamhaus.relays.osirusoft.com');
  bls.add('spammers.v6net.org');
  bls.add('spamsites.dnsbl.net.au');
//  bls.add('spamsites.relays.osirusoft.com');
  bls.add('spamsources.dnsbl.info');
  bls.add('spamsources.fabel.dk');
//  bls.add('spamsources.relays.osirusoft.com');
//  bls.add('spamsources.yamta.org');
  bls.add('spews.dnsbl.net.au');
//  bls.add('spews.relays.osirusoft.com');
  bls.add('t1.bl.reynolds.net.au');
  bls.add('t1.dnsbl.net.au');
  bls.add('ucepn.dnsbl.net.au');
  bls.add('unconfirmed.dsbl.org');
  bls.add('vbl.messagelabs.com');
  bls.add('vox.schpider.com');
  bls.add('web.dnsbl.sorbs.net');
//  bls.add('whois.rfc-ignorant.org');
  bls.add('will-spam-for-food.eu.org');
  bls.add('wingate.opm.blitzed.org');
  bls.add('xbl.selwerd.cx');
//  bls.add('xbl.spamhaus.org');
//  bls.add('ybl.megacity.org');
  bls.add('zombie.dnsbl.sorbs.net');


end;

destructor TSpamChecker.Destroy;
begin
  mr1.free;
  mr2.free;
  FBlackListReport.free;

  //SayNatural('Mail client is being freed');
  FMailCLient.free;
  FMailCLient := nil;

  //BGCmd.FreeOnTerminate := true;
  //BGCmd.Terminate;   //todo: should I free this?
  //SayNatural('Background commands are being freed');
  BGCMd.free;
  //BGCmd := nil;
  //SayNatural('Executing inherited code');
  inherited;
end;

function TSpamChecker.GetBlackListReport: string;
begin
  result := FBlackListReport.text;
end;

function TSpamChecker.GEtRDTPMailClient: TMailClient;
begin
  if (FMailClient = nil) then begin
//    FMailClient := TMailClient.create('192.168.77.15','876');
    FMailClient := TMailClient.create('localhost','876');
    FMailClient.Context := 'some_context';
  end;

  result := FMailClient;

end;

function TSpamChecker.GetIPFromReceivedLine(sLine: string): string;
var
  s, s1,s2: string;
begin
  result := '';
  if SplitString(sLIne, '[', s1,s2) then begin
    if SplitString(s2, ']', s1,s2) then begin
      result := s1;
    end;
  end;

  if SplitSTring(sLine, '([', s1,s2) then begin
    if SplitString(s1, 'from ', s1,s2) then begin
      result := DNSLookup(Trim(s2));
    end;
  end;

end;



function TSpamChecker.GetnumberOfBlacklists(sIP: string; bCheckAll: boolean = false): integer;
var
  t: integer;
  c: Tcmd_CheckIfBlackListed;
  tmStart, tmNow: cardinal;
  iCount: integer;
begin
  iCount := BLCache.GEtCachedREsult(sIP);
  if  iCount > -1 then begin
//    SayNatural('Blacklisted by '+inttostr(iCount)+' services.', true);
    result := iCount;
    exit;
  end;

  if self.UseRemoteService then begin
    result := self.MailClient.GetBlacklistStatus(sIP, bCheckAll);
    exit;
  end;
  result:= 0;
  sIP := Trim(sIP);

  if sIP = '' then begin
    result := 0;
    exit;
  end;

  //allow intranet addresses
  if pos('127.0.0.', sIP) > 1 then
    exit;

  if pos('10.', sIP) = 1 then
    exit;

  if pos('192.168.', sIP) = 1 then
    exit;


  FBlackListReport.clear;

  //SayNatural('Checking mail for spam', false);
  BGCmd.Clear;
  //BGCmd.ChildThreadCount := 32;
  //start commands
  for t:= 0 to bls.count-1 do begin
    c := Tcmd_CheckIfBlacklisted.create;
//    c.Processor  := BGCmd;
    c.Service := bls[t];
    c.IP := sIP;

    c.Process(BGCmd);
  end;

  tmStart := GetTicker();
  while (not BGCmd.IsComplete) and (not BGCmd.Cancelled) do begin
//    SayNatural('Continuing to check blacklists',true);
    iCount := 0;
    for t:= BGCmd.CommandCount-1 downto 0 do begin
      c := BGCmd.Commands[t] as Tcmd_CheckIfBlackLIsted;
      if (not bCheckAll) and (c.IsComplete) and (not c.Error) and (c.Result) then begin
        inc(iCount);
      end;
    end;

    if (iCount >= GEORGE_BUSH_BLACK_LIST_THRESHOLD) then begin
      BGCmd.CancelAll;
    end;

    tmNow := GetTicker();
    if GetTimeSince(tmNow, tmStart) > BLACKLIST_CHECK_TIMEOUT then begin
      //SayNatural('Blacklist check is ending early', false);
      BGCmd.CancelAll;
    end;

    sleep(1000);

  end;


  for t:= BGCmd.CommandCount-1 downto 0 do begin
    c := BGCmd.Commands[t] as Tcmd_CheckIfBlackLIsted;
    if c.IsComplete then begin
      if not c.Error then begin
        if c.Result then begin
          inc(result);
          FBlackListReport.Add('BL '+bls[t]);
        end else begin
          FBlackListReport.Add('OK '+bls[t]);
        end;
      end else begin
        FBlackListReport.Add('ER '+bls[t]+' '+c.ErrorMessage);
      end;
    end else begin
      FBlackListReport.Add('NA '+bls[t]+' Cancelled');
    end;
    while c.IsExecutingNow do
      sleep(100);

    c.Free;
  end;

  try
    FBlackListReport.SavetoFile(DLLPath+'bl_report.txt');

  except


  end;
  BGCmd.Clear;


  BLCache.Add(sIP, result);
//  SayNatural('Blacklisted by '+inttostr(result)+' services', false);
  exit;



//  if not bCheckAll then
//    if result >= BLACK_LIST_THRESHOLD then break;
//  end;


end;

function TSpamChecker.HasMXRecordWithIP(sIP, sDomain: string): boolean;
var
  t: integer;
  mxrec: TMXRecord;
  soarec: TSOARecord;
  dns: TIDDNSResolver;
  sIPTEmp: string;
begin
  result := false;

  dns := TIDDnsResolver.create;
  try
  //cpc1-pete4-0-0-cust844.pete.cable.ntl.com
    dns.QueryType := [qtMX];
  //  dns.CreateQuery();
    dns.Host := '192.168.77.4';
    dns.AllowRecursiveQueries := true;
    dns.Resolve(sDomain);
    dns.Port := 53;
    dns.WaitingTime := 8000;


    for t:= 0 to dns.queryresult.Count-1 do begin
      if dns.QueryResult.Items[t] is TMXRecord then begin
        mxrec := TMXRecord(dns.QueryResult.Items[t]);
        sIPTEmp := ResolveIP(mxrec.ExchangeServer);
        if pos(sIPTemp, sIP) > 0 then begin
          result := true;
          break;
        end;

      end;

      if dns.QueryResult.Items[t] is TSOARecord then begin
        soarec := TSOARecord(dns.QueryResult.Items[t]);
        sIPTEmp := ResolveIP(soarec.Primary);
        if pos(sIPTemp, sIP) > 0 then begin
          result := true;
          break;
        end;
      end;
    end;
  finally
    dns.free;
  end;

end;

function TSpamChecker.HasMXRecordWithDomain(sCheckingDomain, sDomain: string): boolean;
var
  t: integer;
  mxrec: TMXRecord;
  soarec: TSOARecord;
  dns: TIDDNSResolver;
  sLeft, sRight: string;
begin
  result := false;

  dns := TIDDnsResolver.create;
  try
  //cpc1-pete4-0-0-cust844.pete.cable.ntl.com
    dns.QueryType := [qtMX];
  //  dns.CreateQuery();
    dns.Host := '192.168.77.4';
    dns.AllowRecursiveQueries := true;
    dns.Resolve(sDomain);
    dns.Port := 53;
    dns.WaitingTime := 8000;


    for t:= 0 to dns.queryresult.Count-1 do begin
      if dns.QueryResult.Items[t] is TMXRecord then begin
        mxrec := TMXRecord(dns.QueryResult.Items[t]);
        if SplitStringNoCase(mxrec.ExchangeServer, sCheckingDomain, sLeft, sRight) then begin
//        if mxrec.ExchangeServer =  then begin
          result := true;
          break;
        end;

      end;

      if dns.QueryResult.Items[t] is TSOARecord then begin
        soarec := TSOARecord(dns.QueryResult.Items[t]);
        if SplitStringNoCase(soarec.Primary, sCheckingDomain, sLeft, sRight) then begin
          result := true;
          break;
        end;
      end;
    end;
  finally
    dns.free;
  end;

end;


function TSpamChecker.ResolveIP(sDomain: string): string;
var
  t: integer;
  arec: TARecord;
  soarec: TSOARecord;
  dns: TIDDNSResolver;
begin
  result := '';
  dns := TIDDnsResolver.create;
  try
  //cpc1-pete4-0-0-cust844.pete.cable.ntl.com
    dns.QueryType := [qtA];
  //  dns.CreateQuery();
    dns.Host := '192.168.77.4';
    dns.AllowRecursiveQueries := true;
    dns.Resolve(sDomain);
    dns.Port := 53;
    dns.WaitingTime := 8000;


    for t:= 0 to dns.queryresult.Count-1 do begin
      if dns.QueryResult.Items[t] is TARecord then begin
        arec := TARecord(dns.QueryResult.Items[t]);
        result := arec.IPAddress;
        break;
      end;

      if dns.QueryResult.Items[t] is TSOARecord then begin
        soarec := TSOARecord(dns.QueryResult.Items[t]);
        result := soarec.Primary;
        break;
      end;



    end;
  finally
    dns.free;
  end;

end;

procedure TSpamChecker.WeedOutBadBlacklists(sDomain: string);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TSpamChecker.ResolveAllIPs(sDomain: string): string;
var
  t: integer;
  arec: TARecord;
  soarec: TSOARecord;
  dns: TIDDNSResolver;
begin
  result := '';
  dns := TIDDnsResolver.create;
  try
    if sdomain = '' then exit;
    if sDomain[1] in ['0'..'9'] then begin
      result := sDomain;
      exit;
    end;
  //cpc1-pete4-0-0-cust844.pete.cable.ntl.com
    dns.QueryType := [qtA];
  //  dns.CreateQuery();
    dns.Host := '192.168.77.4';
    dns.AllowRecursiveQueries := true;
    dns.Resolve(sDomain);
    dns.Port := 53;
    dns.WaitingTime := 8000;


    for t:= 0 to dns.queryresult.Count-1 do begin
      if dns.QueryResult.Items[t] is TARecord then begin
        arec := TARecord(dns.QueryResult.Items[t]);
        if result <> '' then result := result + ';';
        result := result + arec.IPAddress;
      end;

      if dns.QueryResult.Items[t] is TSOARecord then begin
        soarec := TSOARecord(dns.QueryResult.Items[t]);
        if result <> '' then result := result + ';';
        result := result + soarec.Primary;
      end;


    end;
  finally
    dns.free;
  end;

end;



{ TCmd_CheckIfBlackListed }

function TCmd_CheckIfBlackListed.checkIfBlacklisted(sIP,
  sUsingService: string): boolean;
var
  WSAData: TWSAData;
  addr : Cardinal;
  lpHost1: PHostEnt;
  s: string;
  astr: ansistring;
  ws: widestring;
  pc: PAnsiChar;
begin
// gethostbyname( const AnsiChar* name);

  Result := false;
//  if WSAStartup(MakeWord(1,0), WSAData) = 0 then
//  begin
    try
      s := reverseip(sIP)+'.'+sUsingService ;
      astr := s;
      lpHost1 := gethostbyname(PAnsiChar(astr));

      if lpHost1 <> nil then begin
        pc := lpHost1.h_addr^;
        if pc <> nil then begin
          result := (pc[0] = #$7f) and (pc[1] = #0) and (pc[2] = #0);
        end;
      end;


    except
      on e: Exception do begin
//        beeper.beepchord([700,777],500 );
        status := e.Message;
//        SAyNatural(status);
      end;
    end;

//  end;
//      WSACleanup;

end;
(*var
	WSAData: TWSAData;
	addr : Cardinal;
	lpHost1: PHostEnt;
  s: string;
begin
	Result := false;
	if WSAStartup(MakeWord(1,0), WSAData) = 0 then
	begin
		addr := inet_addr(PAnsiChar(sIP));
		lpHost1 := GetHostByAddr(@addr, sizeof(addr), AF_INET);
		if lpHost1 <> nil then begin
			s := StrPas(lpHost1^.h_name);
      if (pos('127.0.0.', s) > 0) then
        result := true;
		end else begin
			Result := false;
    end;
		WSACleanup;
	end;

end;*)

function TCmd_CheckIfBlackListed.checkIfBlacklistedOld(sIP,
  sUsingService: string): boolean;
var
  s, sLeft, sRight: string;
  sOutput: string;
begin
  result := false;

  sIP := ReverseIP(sIP);
  sOutput := 'output_'+inttostr(GEtCurrentThreadID())+'.txt';
  if fileexists(DLLPath+sOutput) then deletefile(dllpath+sOutput);
  exe.RunProgramAndWait('nslookup', sIP+'.'+sUsingService+'>'+sOutput, Dllpath, true, true);
  s := LoadStringFromFile(dllpath+sOutput);
  if fileexists(DLLPath+sOutput) then deletefile(dllpath+sOutput);
  SplitString(s,':', sLeft, sRight);
  SplitString(sRight,':', sLeft, sRight);
  SplitString(sRight,#13, sLeft, sRight);

  if pos('127.0.0.', lowercase(sRight)) > 0 then begin
    result := true;
  end;


end;

constructor TCmd_CheckIfBlackListed.Create;
begin
  inherited;

end;

procedure TCmd_CheckIfBlackListed.DoExecute;
begin
  inherited;
  //GLOG.Debug('Tcmd_CheckIfBlackListed was started in thread '+inttostr(GEtCurrentThreadID));
  Lock;
  try
    if assigned(Thread) then
      Thread.Name := 'Check BL: '+IP+'.'+Service;
  finally
    unlock;
  end;
  result := self.checkIfBlacklisted(IP, Service);
  //GLOG.Debug('Tcmd_CheckIfBlackListed was completed in thread '+inttostr(GEtCurrentThreadID));

end;

procedure TCmd_CheckIfBlackListed.InitExpense;
begin
  inherited;
  CPUExpense := 0.01;

end;

{ TBLCache }

procedure TBLCache.Add(sIP: string; iBLs: integer);

var
  r:TBLRecord;
begin
  LockWRite;
  try
  r := TBLREcord.create;
  r.IP := sIP;
  r.NumberOfBLs := iBLs;
  FList.add(r);
  finally
    Unlockwrite;
  end;

end;

constructor TBLCache.Create;
begin
  inherited;
  FList := TList.create;
end;

destructor TBLCache.Destroy;
begin
  FreeListContents(FList);
  FList.free;
  FList := nil;

  inherited;
end;

function TBLCache.GetCachedREsult(sIP: string): integer;

var
  t: integer;
  bl : TBLREcord;
begin
  result := -1;
  LockRead;
  try
    for t:= 0 to FList.count-1 do begin
      bl := FList[t];
      if bl.IP = sIP then
        result := bl.NumberofBLs;


    end;
  finally
    UnlockREad;
  end;
end;

{ TBlackListServers }

function TBlackListServers.count: integer;
begin
  lockread;
  try
    result := bls.count;
  finally
    unlockread;
  end;
end;

constructor TBlackListServers.create;
begin
  inherited;
  bls := TStringlist.create;

  bls.add('blacklist.digitaltundra.com');

  bls.add('list.dsbl.org');
  bls.add('dnsbl.njabl.org');
  bls.add('sbl.spamhaus.org');
  bls.add('xbl.spamhaus.org');
  bls.add('sbl-xbl.spamhaus.org');
  bls.add('bl.spamcop.net');
  bls.add('whois.rfc-ignorant.org');
  bls.add('cbl.abuseat.org');
  bls.add('dnsbl.sorbs.net');
//  bls.add('spam.dnsbl.sorbs.net');
  bls.add('blackholes.five-ten-sg.com');
  bls.add('no-more-funn.moensted.dk');
  bls.add('ucepn.dnsbl.net.au');
  bls.add('psbl.surriel.com');
//  bls.add('bl.spamcannibal.org');
  bls.add('dnsbl-3.uceprotect.net');
  bls.add('dnsbl-2.uceprotect.net');



//  bls.add('3y.spam.mrs.kithrup.com');
//  bls.add('access.redhawk.org');
//  bls.add('all.rbl.kropka.net');
//  bls.add('all.spamblock.unit.liu.se');
//  bls.add('.madscience.nl');
//  bls.add('bl.borderworlds.dk');
//  bls.add('bl.csma.biz');
//  bls.add('bl.redhatgate.com');
//  bls.add('bl.spamcannibal.org');
//  bls.add('bl.spamcop.net');
//  bls.add('bl.starloop.com');
//  bls.add('bl.technovision.dk');
//  bls.add('blackhole.compu.net');
//  bls.add('blackholes.five-ten-sg.com');
//  bls.add('blackholes.intersil.net');
//  bls.add('blackholes.mail-abuse.org');
//  bls.add('blackholes.sandes.dk');
//  bls.add('blackholes.uceb.org');
//  bls.add('blackholes.wirehub.net');
//  bls.add('blacklist.informationwave.net');
//  bls.add('blacklist.sci.kun.nl');
//  bls.add('blacklist.spambag.org');
//  bls.add('block.blars.org');
//  bls.add('block.dnsbl.sorbs.net');
//  bls.add('blocked.hilli.dk');
//  bls.add('blocked.secnap.net');
//  bls.add('blocklist.squawk.com');
//  bls.add('blocklist2.squawk.com');
//  bls.add('blocktest.relays.osirusoft.com');
//  bls.add('bogons.dnsiplists.completewhois.com');
//  bls.add('cart00ney.surriel.com');
//  bls.add('cbl.abuseat.org');
//  bls.add('dev.null.dk');
//  bls.add('dews.qmail.org  dialup.blacklist.jippg.org');
//  bls.add('dialup.rbl.kropka.net');
//  bls.add('dialups.mail-abuse.org');
//  bls.add('dialups.relays.osirusoft.com  dialups.visi.com');
//  bls.add('dnsbl.ahbl.org');
//  bls.add('dnsbl.antispam.or.id');
//  bls.add('dnsbl.cyberlogic.net');
//  bls.add('dnsbl.jammconsulting.com');
//  bls.add('dnsbl.kempt.net');
//  bls.add('dnsbl.njabl.org');
//  bls.add('dnsbl.solid.net  dnsbl.sorbs.net');
//  bls.add('dnsbl-1.uceprotect.net');
//  bls.add('dnsbl-2.uceprotect.net');
//  bls.add('dnsbl-3.uceprotect.net');
//  bls.add('dsbl.dnsbl.net.au');
//  bls.add('duinv.aupads.org');
//  bls.add('dul.dnsbl.sorbs.net');
  bls.add('dul.ru');
//  bls.add('dun.dnsrbl.net');
  bls.add('dynablock.njabl.org');
  bls.add('dynablock.wirehub.net');
  bls.add('fl.chickenboner.biz');
//  bls.add('flowgoaway.com');
  bls.add('forbidden.icm.edu.pl');
  bls.add('form.rbl.kropka.net');
//  bls.add('formmail.relays.monkeys.com');
  bls.add('hijacked.dnsiplists.completewhois.com');
  bls.add('hil.habeas.com');
  bls.add('http.dnsbl.sorbs.net');
  bls.add('http.opm.blitzed.org');
  bls.add('inflow.noflow.org');
//  bls.add('inputs.relays.osirusoft.com');
  bls.add('intruders.docs.uu.se');
  bls.add('ip.rbl.kropka.net');
  bls.add('korea.services.net');
  bls.add('l1.spews.dnsbl.sorbs.net');
  bls.add('l2.spews.dnsbl.sorbs.net');
  bls.add('lame-av.rbl.kropka.net');
//  bls.add('lbl.lagengymnastik.dk');
  bls.add('list.dsbl.org');
  bls.add('mail-abuse.blacklist.jippg.org');
  bls.add('map.spam-rbl.com');
  bls.add('misc.dnsbl.sorbs.net');
  bls.add('msgid.bl.gweep.ca');
  bls.add('multihop.dsbl.org');
//  bls.add('no-more-funn.moensted.dk');
//  bls.add('ohps.bl.reynolds.net.au');
//  bls.add('ohps.dnsbl.net.au');
//  bls.add('omrs.bl.reynolds.net.au');
//  bls.add('omrs.dnsbl.net.au');
//  bls.add('op.rbl.kropka.net');
//  bls.add('opm.blitzed.org');
//  bls.add('or.rbl.kropka.net');
//  bls.add('orbs.dorkslayers.com');
//  bls.add('orid.dnsbl.net.au');
//  bls.add('orvedb.aupads.org');
//  bls.add('osps.bl.reynolds.net.au');
//  bls.add('osps.dnsbl.net.au');
//  bls.add('osrs.bl.reynolds.net.au');
//  bls.add('osrs.dnsbl.net.au');
//  bls.add('outputs.relays.osirusoft.com');
//  bls.add('owfs.bl.reynolds.net.au');
//  bls.add('owfs.dnsbl.net.au');
//  bls.add('owps.bl.reynolds.net.au');
//  bls.add('owps.dnsbl.net.au');
//  bls.add('pdl.dnsbl.net.au');
//  bls.add('pm0-no-more.compu.net');
//  bls.add('ppbl.beat.st');
//  bls.add('probes.dnsbl.net.au');
//  bls.add('proxies.exsilia.net');
//  bls.add('proxies.relays.monkeys.com');
//  bls.add('proxy.bl.gweep.ca');
//  bls.add('proxy.relays.osirusoft.com');
//  bls.add('psbl.surriel.com');
//  bls.add('pss.spambusters.org.ar');
  bls.add('rbl.cluecentral.net');
  bls.add('rbl.rangers.eu.org');
//  bls.add('rbl.rope.net');
  bls.add('rbl.schulte.org');
  bls.add('rbl.snark.net');
  bls.add('rbl.triumf.ca');
  bls.add('rblmap.tu-berlin.de');
  bls.add('rdts.bl.reynolds.net.au');
  bls.add('rdts.dnsbl.net.au');
  bls.add('relays.bl.gweep.ca');
  bls.add('relays.bl.kundenserver.de');
  bls.add('relays.dorkslayers.com');
  bls.add('relays.mail-abuse.org');
  bls.add('relays.nether.net');
//  bls.add('relays.ordb.org');
//  bls.add('relays.osirusoft.com');
  bls.add('relays.visi.com');
  bls.add('relaywatcher.n13mbl.com');
  bls.add('ricn.bl.reynolds.net.au');
  bls.add('ricn.dnsbl.net.au');
  bls.add('rmst.bl.reynolds.net.au');
  bls.add('rmst.dnsbl.net.au');
  bls.add('rsbl.aupads.org');
  bls.add('satos.rbl.cluecentral.net');
//  bls.add('sbbl.they.com  sbl.csma.biz');
//  bls.add('sbl.spamhaus.org');
//  bls.add('sbl-xbl.spamhaus.org');
//  bls.add('smtp.dnsbl.sorbs.net');
//  bls.add('socks.dnsbl.sorbs.net');
//  bls.add('socks.opm.blitzed.org');
//  bls.add('socks.relays.osirusoft.com');
//  bls.add('sorbs.dnsbl.net.au');
//  bls.add('spam.dnsbl.sorbs.net');
  bls.add('spam.dnsrbl.net');
//  bls.add('spam.exsilia.net');
  bls.add('spam.olsentech.net');
  bls.add('spam.wytnij.to');
  bls.add('spamguard.leadmon.net');
//  bls.add('spamhaus.relays.osirusoft.com');
  bls.add('spammers.v6net.org');
  bls.add('spamsites.dnsbl.net.au');
//  bls.add('spamsites.relays.osirusoft.com');
  bls.add('spamsources.dnsbl.info');
  bls.add('spamsources.fabel.dk');
//  bls.add('spamsources.relays.osirusoft.com');
//  bls.add('spamsources.yamta.org');
  bls.add('spews.dnsbl.net.au');
//  bls.add('spews.relays.osirusoft.com');
  bls.add('t1.bl.reynolds.net.au');
  bls.add('t1.dnsbl.net.au');
  bls.add('ucepn.dnsbl.net.au');
  bls.add('unconfirmed.dsbl.org');
  bls.add('vbl.messagelabs.com');
  bls.add('vox.schpider.com');
  bls.add('web.dnsbl.sorbs.net');
//  bls.add('whois.rfc-ignorant.org');
  bls.add('will-spam-for-food.eu.org');
  bls.add('wingate.opm.blitzed.org');
  bls.add('xbl.selwerd.cx');
//  bls.add('xbl.spamhaus.org');
//  bls.add('ybl.megacity.org');
  bls.add('zombie.dnsbl.sorbs.net');

end;

destructor TBlackListServers.destroy;
begin
  bls.free;

  inherited;
end;

function TBlackListServers.GetBls(idx: integer): string;
begin
  lockread;
  try
    result := bls[idx];
  finally
    unlockread;
  end;
end;

function TBlackListServers.GetWeedTime: TDateTime;
begin
  result := FWEedTime;

end;

procedure TBlackListServers.remove(s: string);
var
  t: integer;
begin
  lockwrite;
  try

    for t := 0 to bls.count-1 do begin
      if lowercase(s) = lowercase(bls[t]) then
        bls.delete(t);
    end;
  finally
    unlockwrite;
  end;

end;

procedure TBlackListServers.SEtWeedTime(const Value: TDateTime);
begin
  FWEedTime := value;
end;

procedure TBlackListServers.Weedout(cp: TCommandProcessor;
  sWhiteDomain: string);
var
  t: integer;
  c: Tcmd_CheckIfBlackLIsted;
  l: TLIst;
begin
  l := TList.create;
  lockwrite;
  try

    for t:= 0 to bls.count-1 do begin
      c := TCmd_CheckIfBlacklisted.create;
      c.Service  := bls[t];
      c.Ip := DNSLookup(sWhiteDOmain);
      c.Start(CP);
      l.add(c);
    end;

    for t:= 0 to bls.count-1 do begin
      c := l[t];
      c.WaitFor;
      if c.result then
        remove(c.service);
    end;

  finally
    l.free;
    unlockwrite
  end;

end;

procedure oinit;
begin
BLCAche := TBlCache.create;
end;

procedure ofinal;
begin
BLCache.free;


end;

initialization
  init.RegisterProcs('spam', oinit, ofinal);


finalization

end.

