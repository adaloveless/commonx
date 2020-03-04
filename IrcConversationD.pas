unit IrcConversationD;

interface

uses
  irc_abstract, systemx, typex, stringx, betterobject, classes, irc_monitor, dirfile, sysutils;

type
  TIRCConversationDaemon = class(TSharedObject)
  protected
    conversation: Iholder<TIRCConversation>;
    irc: TIRCMultiUserClient;
    procedure SendHelp();
    procedure DoSendHelpHeader();virtual;
    procedure SendhelpCommands();virtual;
  public
    constructor Create(irc: TIRCMultiUserClient; channel: string); reintroduce;virtual;
    procedure Detach; override;
    function OnCommand(sOriginalLine, sCmd: string; params: TStringList): boolean;virtual;
    procedure SayHello;
    procedure TestUTF8;
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

  SayHello;




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
  DoSendHelpHeader;
  SendHelpCommands;
end;

procedure TIRCConversationDaemon.SendhelpCommands;
begin
  conversation.o.PrivMsg('  hello       - causes server to reply with "hello", essentially a ping');
  conversation.o.PrivMsg('  testutf8    - a test of utf8 encoding, if you get a bunch of ?s then utf8 is not supported currently');
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
