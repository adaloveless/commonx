unit IrcConversationD;

interface

uses
  irc_abstract, systemx, typex, stringx, betterobject, classes, irc_monitor, dirfile, sysutils;

type
  TIRCConversationDaemon = class(TSharedObject)
  private
    procedure DoSendHelpHEader;
  protected
    conversation: Iholder<TIRCConversation>;
    irc: TIRCMultiUserClient;
    procedure SendHelp();
    procedure SendHelpOverView();virtual;
    procedure SendhelpCommands();virtual;
    function GetConversation: TIRCConversation;
    procedure SendHelpHeader;
    procedure SendHelpLine(sCommand, sParams, sHelp: string);
    procedure SendHelpFooter;
  public
    constructor Create(irc: TIRCMultiUserClient; channel: string); reintroduce;virtual;
    procedure Detach; override;
    function OnCommand(sOriginalLine, sCmd: string; params: TStringList): boolean;virtual;
    procedure SayHello;virtual;
    procedure TestUTF8;
    property conv: TIRCConversation read Getconversation;
  end;


implementation

{ TIRCConversationDaemon }

constructor TIRCConversationDaemon.Create(irc: TIRCMultiUserClient; channel: string);
begin
  inherited Create;
  if zcopy(channel, 0,1) <> '#' then
    channel := '#'+channel;
  self.irc := irc;
  conversation := irc.NewConversation(channel);
  conversation.o.fOnCommand := function (sOriginalLine, sCmd: string; params: TStringList): boolean
    begin
      result := OnCommand(sOriginalLine, sCmd, params);
    end;

  if irc.WaitForState(irccsStarted) then begin
    try
      SayHello;
    except
    end;
  end;




end;

procedure TIRCConversationDaemon.Detach;
begin
  if detached then exit;

  irc.EndConversation(conversation);
  conversation := nil;
  inherited;

end;

procedure TIRCConversationDaemon.DoSendHelpHEader;
begin
  conversation.o.PrivMsg('Help for '+classname);

end;

function TIRCConversationDaemon.GetConversation: TIRCConversation;
begin
  result := conversation.o;
end;

function TIRCConversationDaemon.OnCommand(sOriginalLine, sCmd: string;
  params: TStringList): boolean;
begin
  result := false;

  if not result then begin
    if sCmd = 'help' then begin
      SendHelp();
      exit(true);
    end else
    if sCmd = 'hello' then begin
      Sayhello;
    end;
    if sCmd = 'testutf8' then begin
      TestUTF8;
    end;
  end;
end;




procedure TIRCConversationDaemon.SayHello;
begin
  var d: TDateTime;
  d := dirfile.GetFileDate(dllname);
  conversation.o.PrivMsg('  _    _      _ _        ');
  conversation.o.PrivMsg(' | |  | |    | | |       ');
  conversation.o.PrivMsg(' | |__| | ___| | | ___   ');
  conversation.o.PrivMsg(' |  __  |/ _ \ | |/ _ \');
  conversation.o.PrivMsg(' | |  | |  __/ | | (_) |');
  conversation.o.PrivMsg(' |_|  |_|\___|_|_|\___/ ');
  conversation.o.PrivMsg('                        ');

  conversation.o.PrivMsg('Hi from '+self.ClassName+' in '+extractfilename(dllname)+' dated '+datetimetostr(d));

end;


procedure TIRCConversationDaemon.SendHelp;
begin
  SendHelpHeader;
  SendHelpCommands;
  SendHelpFooter;
end;

procedure TIRCConversationDaemon.SendhelpCommands;
begin
  SendHelpLine('hello', '','causes server to reply with "hello", essentially a ping');
  SendHelpLine('testutf8    ','','a test of utf8 encoding, if you get a bunch of ?s then utf8 is not supported currently');

end;

procedure TIRCConversationDaemon.SendHelpFooter;
begin
    conv.PM(ESCIRC+'</table>');
    conv.PM(ESCIRC+'<span color=red>To Send a command, type ! followed by the command name, space, and params (if applicable)</span>');

end;

procedure TIRCConversationDaemon.SendHelpHeader;
begin
  conv.PMHold(ESCIRC+'<table><tr><th align=left colspan=3><h2>Help for '+classname+'</h2></th></tr>');
  conv.PMHold(ESCIRC+'<tr><th align=left>Command</th><th align=left>Params</th><th align=left>Details</th></tr>');
end;

procedure TIRCConversationDaemon.SendHelpLine(sCommand, sParams, sHelp: string);
begin
//  conv.PM(ESCIRC+'<tr><td><b>'+sCommand+'</b></td><td>'+AmpEncode(sHelp)+'</td></tr>');
  conv.PMHold(ESCIRC+'<tr><td><b>'+sCommand+'</b></td><td><i>'+AmpEncode(sParams)+'</i></td><td>'+AmpEncode(sHelp)+'</td></tr>');

end;

procedure TIRCConversationDaemon.SendHelpOverView;
begin
  //''
end;

procedure TIRCConversationDaemon.TestUTF8;
begin
  conversation.o.PrivMsg(' ██░ ██ ▓█████  ██▓     ██▓     ▒█████');
  conversation.o.PrivMsg('▓██░ ██▒▓█   ▀ ▓██▒    ▓██▒    ▒██▒  ██▒');
  conversation.o.PrivMsg('▒██▀▀██░▒███   ▒██░    ▒██░    ▒██░  ██▒');
  conversation.o.PrivMsg('░▓█ ░██ ▒▓█  ▄ ▒██░    ▒██░    ▒██   ██░');
  conversation.o.PrivMsg('░▓█▒░██▓░▒████▒░██████▒░██████▒░ ████▓▒░');
  conversation.o.PrivMsg(' ▒ ░░▒░▒░░ ▒░ ░░ ▒░▓  ░░ ▒░▓  ░░ ▒░▒░▒░');
  conversation.o.PrivMsg(' ▒ ░▒░ ░ ░ ░  ░░ ░ ▒  ░░ ░ ▒  ░  ░ ▒ ▒░');
  conversation.o.PrivMsg(' ░  ░░ ░   ░     ░ ░     ░ ░   ░ ░ ░ ▒    ');
  conversation.o.PrivMsg(' ░  ░  ░   ░  ░    ░  ░    ░  ░    ░ ░    ');
end;

end.
