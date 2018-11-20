unit keyboard;

interface

uses windows, sysutils, systemx, typex;

type
  keys = string;

CONST
  PLAY_TAB = '^+~+';
  PLAY_ENTER = '^/~/';


procedure PlayKeys(s: keys);
function charToVK(c: char): byte;
function stringToKeys(s: string): keys;
procedure PlayString(s: string);

procedure keybd_event(vk: integer; stuff: integer; flags: integer; morestuff: integer);


implementation

procedure keybd_event(vk: integer; stuff: integer; flags: integer; morestuff: integer);
var
  struc: windows.tagKEYBDINPUT;
  inp: windows.tagINPUT;
begin

  inp.Itype := INPUT_KEYBOARD;
  inp.ki.wVk := vk;
  inp.ki.wScan := stuff;
  inp.ki.dwFlags := flags;
  inp.ki.time := GetTickCount;
  inp.ki.dwExtraInfo := morestuff;

  while sendinput(1, inp, sizeof(inp)) = 0 do
    sleep(1);

end;

procedure PlayKeys(s: keys);
var
  t: integer;
  b: byte;
begin
  for t:= STRZ to high(s) do begin
//    windows.beep(100,25);
//    sleep(50);
    if (s[t] = '^') or (s[t] = '~') then
      continue;

    if (t>1) and (s[t-1] = '^') then begin
      case s[t] of
        'a': keybd_event(VK_SHIFT,0, 0,0);
        'z': keybd_event(VK_SHIFT,0, 0,0);
        'x': keybd_event(VK_SHIFT,0, 0,0);
        'f': keybd_event(windows.VK_LMENU,0, KEYEVENTF_EXTENDEDKEY,0);
        'c': keybd_event(VK_SHIFT,0, KEYEVENTF_EXTENDEDKEY,0);
//        'v': keybd_event(VK_SHIFT,0, KEYEVENTF_EXTENDEDKEY,0);
//        'g': keybd_event(VK_SHIFT,0, KEYEVENTF_EXTENDEDKEY,0);
        '/': keybd_event(VK_RETURN,0, KEYEVENTF_EXTENDEDKEY,0);
        '<': keybd_event(VK_LEFT,0, KEYEVENTF_EXTENDEDKEY,0);
        '>': keybd_event(VK_RIGHT,0, KEYEVENTF_EXTENDEDKEY,0);
        '^': keybd_event(VK_UP,0, KEYEVENTF_EXTENDEDKEY,0);
        '~': keybd_event(VK_DOWN,0, KEYEVENTF_EXTENDEDKEY,0);
        'q': keybd_event(VK_HOME,0, KEYEVENTF_EXTENDEDKEY,0);
        'p': keybd_event(VK_END,0, KEYEVENTF_EXTENDEDKEY,0);
        't': keybd_event(VK_END,0, KEYEVENTF_EXTENDEDKEY,0);
        '{': keybd_event(VK_BACK,0, KEYEVENTF_EXTENDEDKEY,0);
        '}': keybd_event(VK_DELETE,0, KEYEVENTF_EXTENDEDKEY,0);
        '+': keybd_event(VK_TAB,0, KEYEVENTF_EXTENDEDKEY,0);
        '-': keybd_event(VK_LCONTROL,0, KEYEVENTF_EXTENDEDKEY,0);
        '$': keybd_event(VK_LMENU,0, KEYEVENTF_EXTENDEDKEY,0);
      end;
    end
    else
    if (t>1) and (s[t-1] = '~') then begin
      case s[t] of
        'a': keybd_event(VK_SHIFT,0, KEYEVENTF_KEYUP,0);
        'z': keybd_event(VK_SHIFT,0, KEYEVENTF_KEYUP,0);
        'x': keybd_event(VK_SHIFT,0, KEYEVENTF_KEYUP,0);
        'f': keybd_event(windows.VK_LMENU,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        'c': keybd_event(VK_SHIFT,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        'v': keybd_event(VK_SHIFT,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        'g': keybd_event(VK_SHIFT,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        '/': keybd_event(VK_RETURN,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        '<': keybd_event(VK_LEFT,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        '>': keybd_event(VK_RIGHT,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        '^': keybd_event(VK_UP,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        '~': keybd_event(VK_DOWN,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        'q': keybd_event(VK_HOME,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        'p': keybd_event(VK_END,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        't': keybd_event(VK_END,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        '{': keybd_event(VK_BACK,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        '}': keybd_event(VK_DELETE,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        '+': keybd_event(VK_TAB,0, KEYEVENTF_KEYUP+KEYEVENTF_EXTENDEDKEY,0);
        '-': keybd_event(VK_LCONTROL,0, KEYEVENTF_EXTENDEDKEY+KEYEVENTF_KEYUP,0);
        '$': keybd_event(VK_LMENU,0, KEYEVENTF_EXTENDEDKEY+KEYEVENTF_KEYUP,0);
      end;
    end
    else
    begin
      b := ord(s[t]);
      keybd_event(b,0, 0,0);
      keybd_event(b,0, KEYEVENTF_KEYUP,0);
//      sleep(15);
    end;
    end;

end;

function charToVK(c: char): byte;
begin
  case c of
    'a'..'z': result := ord(uppercase(c)[1]);
    ',': result := 188;
    '.': result := 190;
    '/': result := 191;
    ';': result := 186;
    '''': result := 222;
    '[': result := 219;
    ']': result := 221;
    '\': result := 220;
    '`': result := 192;
    '-': result := 189;
    '=': result := 187;
  else
    result := ord(c);
  end;
end;

function stringToKeys(s: string): keys;
var
  t: integer;

begin
  result := '';
  for t:= 1 to length(s) do begin

    case s[t] of
      'A'..'Z': result := result+'^a'+Uppercase(s[t])+'~a';
      'a'..'z': result := result+Uppercase(s[t]);
      '~': result := result + '^a`~a';
      '!': result := result + '^a1~a';
      '@': result := result + '^a2~a';
      '#': result := result + '^a3~a';
      '$': result := result + '^a4~a';
      '%': result := result + '^a5~a';
      '^': result := result + '^a6~a';
      '&': result := result + '^a7~a';
      '*': result := result + '^a8~a';
      '(': result := result + '^a9~a';
      ')': result := result + '^a0~a';
      '_': result := result + '^a'+chr(charToVK('-'))+'~a';
      '+': result := result + '^a'+chr(charToVK('='))+'~a';
      '{': result := result + '^a'+chr(charToVK('['))+'~a';
      '}': result := result + '^a'+chr(charToVK(']'))+'~a';
      '|': result := result + '^a'+chr(charToVK('\'))+'~a';
      ':': result := result + '^a'+chr(charToVK(';'))+'~a';
      '"': result := result + '^a'+chr(charToVK(''''))+'~a';
      '<': result := result + '^a'+chr(charToVK(','))+'~a';
      '>': result := result + '^a'+chr(charToVK('.'))+'~a';
      '?': result := result + '^a'+chr(charToVK('/'))+'~a';
    else
      result := result + chr(charToVK(s[t]));
    end;


  end;

end;

procedure PlayString(s: string);
begin
//  speech.Say(s);
  PLayKeys(stringToKeys(s));
end;
end.
