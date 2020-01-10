unit touchcontrols_vcl;

interface


uses
  debug, controls, forms, stdctrls,windows,messages, classes;

type
  TTouchArray = array of TOUCHINPUT;

  TTouchEvent = procedure (sender: TObject; var handled: boolean; touches: TTouchArray) of object;
  TTouchFingerEvent = procedure (sender: TObject; var handled: boolean; touch: TOUCHINPUT) of object;

  TTouchButton = class(TButton)
  private
    FonTouchEx: TTouchEvent;
    FontouchOnefinger: TTouchFingerEvent;
    FOnTouchTwoFinger: TTouchEvent;
    FontouchOnefingerRelease: TTouchFingerEvent;
    FontouchOnefingerPress: TTouchFingerEvent;
    procedure touchmsg(var msg: TMessage);
    function GetTouchLongPressContextMenu: boolean;
    procedure SetTouchLongPressContextMenu(const Value: boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(aowner: TComponent);override;
    procedure wintouch(var msg: TMessage);message WM_TOUCH;
    procedure TouchEx(touches: TTouchArray);
    procedure TouchFinger(var handled: boolean; touch: TOUCHINPUT);
    procedure TouchOneFinger(var handled: boolean;  touch: TOUCHINPUT);
    procedure TouchOneFingerPress(var handled: boolean; touch: TOUCHINPUT);
    procedure TouchOneFingerRelease(var handled: boolean; touch: TOUCHINPUT);
    procedure TouchTwoFinger(var handled: boolean; touches: TTouchArray);
  published
    property OnTouchEx: TTouchEvent read FonTouchEx write FonTouchEx;
    property OnTouchOneFinger: TTouchFingerEvent read FontouchOnefinger write FonTouchOneFinger;
    property OnTouchOneFingerPress: TTouchFingerEvent read FontouchOnefingerPress write FonTouchOneFingerPress;
    property OnTouchOneFingerRelease: TTouchFingerEvent read FontouchOnefingerRelease write FonTouchOneFingerRelease;
    property OnTouchTwoFinger: TTouchEvent read FOnTouchTwoFinger write FonTouchTwoFinger;
    property TouchLongPressContextMenu: boolean read GetTouchLongPressContextMenu write SetTouchLongPressContextMenu;


  end;

implementation

{ TTouchButton }

constructor TTouchButton.Create(aowner: TComponent);
begin
  inherited;
  self.Touch.TabletOptions := [];
end;

function TTouchButton.GetTouchLongPressContextMenu: boolean;
begin
  result :=   toPressandHold in Touch.TabletOptions;
end;

procedure TTouchButton.SetParent(AParent: TWinControl);
begin
  inherited;
  if assigned(AParent) then
    RegisterTouchWindow(self.Handle, 0);

end;

procedure TTouchButton.SetTouchLongPressContextMenu(const Value: boolean);
begin
  if value then
    self.Touch.TabletOptions := self.Touch.TabletOptions + [toPressAndHold]
  else
    self.Touch.tabletoptions := touch.tabletoptions - [toPressAndHold];

end;

procedure TTouchButton.touchmsg(var msg: TMessage);
begin

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TTouchButton.TouchEx(touches: TTouchArray);
begin
  var handled: boolean := false;
  if assigned(ontouchex) then
    ontouchex(self, handled, touches);

  if not handled then begin
    if length(touches) = 1 then begin
      TouchOneFinger(handled, touches[0]);
    end;
    if length(touches) = 2 then begin
      TouchTwoFinger(handled, touches);
    end else begin

    end;
  end;
end;

procedure TTouchButton.TouchFinger(var handled: boolean; touch: TOUCHINPUT);
begin

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TTouchButton.TouchOneFinger(var handled: boolean; touch: TOUCHINPUT);
begin
  if Assigned(FontouchOnefinger) then
    FonTouchonefinger(self, handled, touch);

  if not handled then begin
    if (touch.dwFlags and TOUCHEVENTF_DOWN) <> 0 then begin
      TouchOneFingerPress(handled, touch);
    end else
    if (touch.dwFlags and TOUCHEVENTF_UP) <> 0 then begin
      TouchOneFingerRelease(handled, touch);
    end;
  end;
end;

procedure TTouchButton.TouchOneFingerPress(var handled: boolean;
  touch: TOUCHINPUT);
begin
  if assigned(FontouchOnefingerPress) then
    FOnTouchOnefingerPress(self, handled, touch);

end;

procedure TTouchButton.TouchOneFingerRelease(var handled: boolean;
  touch: TOUCHINPUT);
begin

  if assigned(FontouchOnefingerRelease) then
    FOnTouchOnefingerRelease(self, handled, touch);

end;

procedure TTouchButton.TouchTwoFinger(var handled: boolean;
  touches: TTouchArray);
begin
  if assigned(FOnTouchTwoFinger) then
    FOnTouchTwoFinger(self, handled, touches);
end;

procedure TTouchButton.wintouch(var msg: TMessage);
var
  touches: TTouchArray;
begin
  setlength(touches, msg.WParam);

  if (GetTouchInputInfo(HTOUCHINPUT(msg.lParam), Lo(msg.WPARAM), @touches[0], sizeof(TOUCHINPUT)*msg.WPARAM)) then begin
    CloseTouchInputHandle(msg.LParam);
  end;

  TouchEx(touches);

end;

end.
