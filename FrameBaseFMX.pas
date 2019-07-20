unit FrameBaseFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, guihelpers_fmx;

type
  TframBaseFMX = class(TFrame)
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(aowner: TComponent);override;
    procedure OnCreate;virtual;
    procedure DisableGestures;
  end;

implementation

{$R *.fmx}

{ TframBase }

constructor TframBaseFMX.Create(aowner: TComponent);
begin
  inherited;
  OnCreate;
end;

procedure TframBaseFMX.DisableGestures;
begin
  Control_IterateChildren(self,
    procedure (f: TFMXObject; var stop: boolean)
    begin
      if f is TControl then begin
        var c := f as TControl;
        c.Touch.InteractiveGestures := [];
        c.CanFocus := false;
        c.hittest := false;
      end;
    end
  );


end;

procedure TframBaseFMX.OnCreate;
begin
  //
end;

end.
