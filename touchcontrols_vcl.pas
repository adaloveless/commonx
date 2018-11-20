unit touchcontrols_vcl;

interface


uses
  stdctrls,windows,messages, classes;

type
  TTouchButton = class(TButton)
  public
    constructor Create(aowner: TComponent);override;
    procedure touch(var msg: TMessage);message WM_TOUCH;

  end;

implementation

{ TTouchButton }

constructor TTouchButton.Create(aowner: TComponent);
begin
  inherited;

end;

procedure TTouchButton.touch(var msg: TMessage);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

end.
