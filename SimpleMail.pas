unit SimpleMail;
//This unit simplifies sending email
//it assumes there is an SMTP server running on local host (a wild assumption)
//# Sprint PCS <10 digit number>@messaging.sprintpcs.com
//# T-Mobile (USA) <10 digit number>@tmomail.net
//# Verizon <10 digit number>@vtext.com
//# Cingular <10 digit number>@mobile.mycingular.com
//# AT&T Wireless <10 digit number>@mobile.att.net

interface

uses
  typex, IDSMTP, IDMESSAGE, IDEmailAddress, sysutils, simplewinsock, better_sockets2, stringx, scktcomp, classes, textsocket, applicationparams;

type
  TCellCarrier = (ccTMobile, ccVirgin, ccCingular, ccSprint, ccVerizon, ccNextel, ccUsCellular, ccSunCom, ccPowerTel, ccAtt, ccAlltel, ccMetroPCS);

const
  DEFAULT_GLOBAL_MAIL_SENDER = '192.168.101.8';


procedure RaiseSMTPError(sCode, sMessage: string);
procedure SendMailDirect(msg: TIDMessage);
procedure SendMail(sFrom: string; sTo: string; sSubject: string; sBody: string); overload;
procedure SendMail(sFromName: string; sFrom: string; sTOName: string; sTo: string; sSubject: string; sBody: string; sReplyto: string = ''; sCC: string = ''; sServer: string = DEFAULT_GLOBAL_MAIL_SENDER); overload;
procedure PageSprint(sToNumber: string; sFrom: string; sMessage: string);
procedure SendSMS(sPhoneNumber: string; sMessage: string; carrier: TCellCarrier = ccSprint);
function GetEmailSuffixForCarrier(cc: TCellCarrier): string;



implementation

uses HTTPCLient, webstring;

procedure SendMail(sFrom: string; sTo: string; sSubject: string; sBody: string);
begin
  SendMail('', sFrom, '', sTo, sSubJect, sBody);
end;

procedure SendMail(sFromName: string; sFrom: string; sTOName: string; sTo: string; sSubject: string; sBody: string; sReplyto: string = ''; sCC: string = ''; sServer: string = DEFAULT_GLOBAL_MAIL_SENDER); overload;
var
  smtp: TIDSMTP;
  msg: TIDMessage;
  add: TIDEmailAddressItem;
begin
  smtp := nil;
  msg := nil;
  add := nil;
  try
    smtp := TIDSMTP.create(nil);
    msg := TIDMessage.create(nil);
    add := msg.Recipients.Add;
    add.Address := sTo;
    add.Name := sToName;

    if sReplyTo <> '' then begin
      add := msg.ReplyTo.Add;
      add.Address := sReplyTo;
    end;

    msg.From.Address := sFrom;
    msg.From.Name := sFromName;
    msg.Body.text := sBody;
    msg.Subject := sSubject;

    smtp.Host := sServer;
    smtp.Connect;
    smtp.Send(msg);
  finally
    smtp.free;
  end;

end;
procedure PageSprint(sToNumber: string; sFrom: string; sMessage: string);
var
  s: string;
  http: THTTPClient;
begin
  raise exception.Create('severely deprecated');

  http := THTTPClient.create;
  //sMessage := '1234';
  try                       //application/www-form-urlencoded
    http.OutContentType := 'application/x-www-form-urlencoded';
    //http.Outbody := 'phoneNumber=6515924867&message=jason&characters=155&callBackNumber=&x=2&y=1';
    http.Outbody := 'phoneNumber='+sToNumber+'&message='+EncodeWebString(sMessage)+'&characters='+inttostr(160-length(sMessage))+'&callBackNumber='+EncodeWebString(sfrom)+'&x=1&y=1'#13#10;

    http.UserAgent := 'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; .NET CLR 1.1.4322)';

    http.Post('http://messaging.sprintpcs.com/textmessaging/composeconfirm;JSESSIONID=AQQuf2tv6ZXztGxs192Qer8rQU0ybLeP9yehSXYWXj401USWVO5q!-218151699!182751416!5070!7002', 'http://messaging.sprintpcs.com/textmessaging/compose;JSESSIONID=AQQuf2tv6ZXztGxs192Qer8rQU0ybLeP9yehSXYWXj401USWVO5q!-218151699!182751416!5070!7002', -1);
  finally
    http.free;
  end;



end;

function GetData(sAddress: string): string;
var
  sLeft, sRight: string;

begin
  sLeft := '';
  sRight := '';
  SplitString(sAddress, '@', sLeft, sRight);
  result := sRight;
end;

function GetResponseCode(socket: TTextSocket; sMessage: string): string;
var
  sLeft, sRight: string;
  sStuff: string;
begin
  sLeft := '';
  sRight := '';
  sStuff := socket.ReadLine;

  while (pos(' ', sStuff) > pos('-', sStuff)) and (pos('-', sStuff)> 0) do
    sStuff := Socket.ReadLine;

  SplitString(sStuff,' ', result, sMessage);

end;

procedure SendMailDirect(msg: TIDMessage);
var
  client: TTextSocket;
  t: integer;
  sUser, sData: string;
  slDatas: TStringList;
  sResponse: string;
  sMessage: string;
  i: integer;
  ms: TMemoryStream;
const
  CRLF=#13#10;

begin
  slDatas := TStringList.create;
  try
    client := TTextSocket.create;
    slDatas.Duplicates := dupIgnore;
    for t:= 1 to msg.Recipients.Count do begin
      slDatas.Add(lowercase(GetData(msg.Recipients.items[t].Address)));
    end;

    for t:= 1 to msg.Recipients.Count do begin
      if -1 = slDatas.indexof(lowercase(GetData(msg.Recipients.Items[t].Address)))then
        continue;

      client.socket.RemoteHost := GetData(msg.Recipients.Items[t].Address);
      client.socket.RemotePort := '25';

      client.SendText('EHLO esmai.com');
      sResponse := GetResponseCode(client, sMessage);
      if not (sResponse = '250') then RaiseSMTPError(sResponse,sMessage);

      client.SendText('MAIL FROM:<'+msg.Sender.Address+'>');
      sResponse := GetResponseCode(client, sMessage);
      if not (sResponse = '250') then RaiseSMTPError(sResponse,sMessage);

      client.SendText('RCPT TO:<'+msg.Sender.Address+'>');
      sResponse := GetResponseCode(client, sMessage);
      if sResponse = '250' then begin
        i := slDatas.indexof(lowercase(getData(msg.recipients.items[t].Address)));
        if i> -1 then
        slDatas.Delete(i);
      end;

      //send the actual stuff
      client.SendText('DATA');
      client.ReadLine;
      ms := TMemorystream.create;
      try
        ms.Seek(0,0);
        msg.SaveToStream(ms);
        ms.Seek(0,0);
        client.socket.SendStream(ms);
      finally
        ms.free;
      end;
      client.SendText('.');

      client.ReadLine;
      client.SendText('QUIT');

    end;
  finally
    slDatas.free;
  end;

end;
procedure RaiseSMTPError(sCode, sMessage: string);
begin
  raise Exception.create('SMTP Server Error:'+sCode+' '+sMessage);
end;

function GetEmailSuffixForCarrier(cc: TCellCarrier): string;
begin
  case cc of
    ccTMobile: result := '@tmomail.net';
    ccvirgin: result := '@vmobl.com';
    ccCingular: result := '@cingularme.com';
    ccSprint: result := '@messaging.sprintpcs.com';
    ccVerizon: result := '@vtext.com';
    ccNextel: result := '@messaging.nextel.com';
    ccUsCellular: result := '@email.uscc.net';
    ccSunCom: result := '@tms.suncom.com';
    ccPowertel: result := '@ptel.net';
    ccAtt: result := '@txt.att.net';
    ccAlltel: result := '@message.alltel.com';
    ccMetroPCS: result := '@MyMetroPcs.com';
  else
    result := '';
  end;
end;

procedure SendSMS(sPhoneNumber: string; sMessage: string; carrier: TCellCarrier);
begin
  SendMail('e@e', sPhonenumber+GetEmailSuffixforcarrier(carrier), '', sMessage);
end;


end.
