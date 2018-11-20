unit FramePiano;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ColorBlending,
  Dialogs, FrameBase, ExtCtrls, musictheory, beeper, advancedgraphics, easyimage;

type
  TfrmPiano = class(TfrmFrameBase)
    Panel2: TPanel;
    pnlCC: TPanel;
    pnlB: TPanel;
    pnlC: TPanel;
    pnlD: TPanel;
    pnlE: TPanel;
    pnlF: TPanel;
    pnlG: TPanel;
    pnlA: TPanel;
    pnlAsharp: TPanel;
    pnlGsharp: TPanel;
    pnlCsharp: TPanel;
    pnlDsharp: TPanel;
    pnlFsharp: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    procedure pnlCCClick(Sender: TObject);
    procedure Panel13MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FSticky: boolean;
    FNotes: integer;
    procedure SetNotes(const Value: integer);
    procedure RefreshNotes;
    { Private declarations }

  public
    { Public declarations }
    procedure PlayNote(key: TPanel);
    procedure NotesOff;
    property Sticky: boolean read FSticky write FSticky;
    function IsNoteOn(iNote: integer): boolean;
    procedure SetNoteOn(iNote: integer);
    procedure SetNoteOff(iNote: integer);
    procedure Setnote(inote: integer; bOn: boolean);
    function FindNotePanel(iNote: integer): TPanel;
    property Notes: integer read FNotes write SetNotes;


  end;

var
  frmPiano: TfrmPiano;

implementation

{$R *.dfm}

procedure TfrmPiano.pnlCCClick(Sender: TObject);
var
  p: TPanel;
  b: boolean;
begin
  inherited;
  p := TPanel(sender);


  if Sticky then begin
    b := not IsNoteOn(p.tag);
    if b then begin
      SetnoteOn(p.tag)
    end else begin
      SetnoteOff(p.tag)

    end;

  end else begin
    SetnoteOn(p.tag);
  end;

  refresh;
  PlayNote(sender as TPanel);
  sleep(100);

  if not Sticky then begin
    SetNoteOff(p.tag);
  end else begin
    RefreshNotes;
  end;

end;

procedure TfrmPiano.RefreshNotes;
var
  t: integer;
  p: TPanel;
begin
  for t:=0 to componentcount-1 do begin
    if components[t] is TPanel then begin
      p := components[t] as TPanel;
      if p.tag < 0 then continue;
      if IsNoteOn(p.tag mod 12) then begin
        if (p.tag mod 12) in [1,3,6,8,10] then begin
          p.color := ColorBlend(clYellow, clBlack, 0.5);
        end else begin
          p.color := clYellow
        end;
      end else begin
        if (p.tag mod 12) in [1,3,6,8,10] then begin
          p.color := clBlack;
        end else begin
          p.color := clCream;
        end;
      end;
    end;
  end;




end;

function TfrmPiano.FindNotePanel(iNote: integer): TPanel;
var
  t: integer;
begin
  result := nil;
  for t:= 0 to componentcount-1 do begin
    if components[t] is TPanel then
      if TPanel(Components[t]).tag = iNote then
        result := components[t] as TPanel;
  end;


end;

function TfrmPiano.IsNoteOn(iNote: integer): boolean;
var
  iMask: integer;
begin
  iNote := iNote mod 12;
  iMask := (1 shl iNote);
  result := FNotes and iMask <> 0;
end;

procedure TfrmPiano.Setnote(inote: integer; bOn: boolean);
begin
  if bOn then
    SetNoteOn(iNote)
  else
    SetNoteoff(inote);
end;

procedure TfrmPiano.SetNoteOff(iNote: integer);
var
  iMask: integer;
  p: TPanel;
  cOn, cOff: TColor;

begin
  iNote := iNote mod 12;
  iMask := (1 shl iNote);
  FNotes := FNotes and (not iMask);

  p := findNotePanel(inote);

  if p.tag in [1,3,6,8,10] then begin
    cOn := clYellow;
    cOff := clBlack;
  end else begin
    cOn := clYellow;
    cOff := clCream;
  end;
  p.color := cOff;

end;

procedure TfrmPiano.SetNoteOn(iNote: integer);
var
  iMask: integer;
  p: TPanel;
  cOn, cOff: TColor;
begin
  iNote := iNote mod 12;
  iMask := (1 shl iNote);
  FNotes := FNotes or iMask;

  p := findNotePanel(inote);

  if p.tag in [1,3,6,8,10] then begin
    cOn := ColorBlend(clblack, clYellow, 0.5);
    cOff := clBlack;
  end else begin
    cOn := clYellow;
    cOff := clCream;
  end;

  p.color := cOn;

end;

procedure TfrmPiano.SetNotes(const Value: integer);
begin
  FNotes := Value;
  RefreshNotes;
end;

procedure TfrmPiano.NotesOff;
begin
  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TfrmPiano.Panel13MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssRight in Shift then
    PlayNote(TPanel(sender));
end;

procedure TfrmPiano.PlayNote(key: TPanel);
var
  iNoteIndex: integer;
  iOctave: integer;
  f: real;
begin
  iOctave := key.tag div 12;
  iNoteindex := key.tag mod 12;

  f := note_frequencies[iNoteIndex];
  if iOctave = 0 then
    f := f / 2
  else
    f := f * iOctave;

  beeper.beep(trunc(f), 1000);
end;

end.
