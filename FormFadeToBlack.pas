unit FormFadeToBlack;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, numbers, geometry, tickcount, typex;

const
  FADE_TIME = 2000;
type
  TfrmFadeToBlack = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FadeToBlack;
    procedure FadeIn;
  end;

var
  frmFadeToBlack: TfrmFadeToBlack;

implementation

{$R *.dfm}

{ TfrmFadeToBlack }

procedure TfrmFadeToBlack.FadeIn;
var
  tm, tmNow, tmDif: ticker;
  a: ni;
  r: single;
begin
  alphablendvalue := 0;
  if not showing then
    show;
  refresh;

  tm := GetTicker;
  while true do begin
    tmNow := getticker;
    tmDif := GetTimeSInce(tmNow, tm);
    r := tmDif / (FADE_TIME div 8);
    if r > 1.0 then r := 1.0;
    a := round(r*255);
    alphablendvalue := 255-a;
    if a = 255 then
      break;
  end;
end;

procedure TfrmFadeToBlack.FadeToBlack;
var
  tm, tmNow, tmDif: ticker;
  a: ni;
  r: single;
begin
  alphablendvalue := 0;
  if not showing then
    show;
  refresh;

  tm := GetTicker;
  while true do begin
    tmNow := getticker;
    tmDif := GetTimeSInce(tmNow, tm);
    r := tmDif / FADE_TIME;
    if r > 1.0 then r := 1.0;
    a := round(r*255);
    alphablendvalue := a;
    if a = 255 then
      break;
  end;
end;

end.
