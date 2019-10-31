unit BCF2000;
//display handling
//controllers start at 75 (left digit)... thru 79
//0 is blank
//1 = "A"
//26 = "Z"
//48 = "0" .. 9
//112 = "0." .. "9."
//65 = "A." ... "Z."

//buttons... NOTE ON ONLY


//Track Buttons (note differences when shift buttons are used.
//wo/s=[10][11][12][13][14][15][16][17]
//wo/s=[18][19][1a][1b][1c][1d][1e][1f]

//w/s1=[08][09][0a][0b][0c][0d][0e][0f]
//w/s1=[00][01][02][03][04][05][06][07]

//w/s2=[36][37][38][39][3a][3b][3c][3d]
//w/s2=[3e][3f][40][41][42][43][44][45]

//ROTOR BUTTONS
//wo/s=[20][21][22][23][24][25][26][27]
//w/s1=[20][21][22][23][24][25][26][27]
//w/s2=[20][21][22][23][24][25][26][27]

//DAW buttons (on the right)
//                        [s1][4c] w/s1=[  ][50] w/s2=[  ][51]
//                        [s2][46] w/s1=[  ][33] w/s2=[  ][32]
//                        [2C][28] w/s1=[2d][2b] w/s2=[4b][4e]
//                        [2A][29] w/s1=[2a][29] w/s2=[4a][4d]
//                        [2e][2f] w/s1=[30][31] w/s2=[52][53]
//                        [5B][5C] w/s1=[5a][58] w/s2=[56][54]
//                        [5D][5E] w/s1=[59][5f] w/s2=[55][57]

interface

uses
  systemx, numbers, betterobject, sharedobject, typex, midi, simplemidiport, math, simplebufferedconnection, simplemidibase;

const
  ASC_TO_BIG_DISPLAY : array [0..255] of byte =
 (00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,//0-15
  00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,//16-31
  00,00,00,00,00,00,00,00,00,00,00,00,00,00,64,00,//32-47
  48,49,50,51,52,53,54,55,56,57,00,00,00,00,00,00,//48-63
  00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,//64-79
  16,17,18,19,20,21,22,23,24,25,26,00,00,00,00,00,//80-95
  00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,//96-111
  16,17,18,19,20,21,22,23,24,25,26,00,00,00,00,00,//112-127
  00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
  00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
  00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
  00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
  00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
  00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
  00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
  00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00);

type
//  TLocalMIDI = TSimpleBufferedConnection<TSimpleMIDIPort>;
  TLocalMIDI = TSimpleMIDIPort;
type
  TRotorMode = (rmOff, rmDot, rmPan, rmVol, rmWidth);

  TBCF2000 = class;//forward
  TBCF2000DataSource = class;//forward

  TBCF2000ButtonInEvent = procedure (bcf: TBCF2000; ds: TBCF2000DAtaSource; iButton: nativeint; bDown: boolean; bChanged: boolean; var handled: boolean) of object;
  TBCF2000RotorInEvent = procedure (bcf: TBCF2000; ds: TBCF2000DAtaSource; iChannel, iTicks: nativeint; var handled: boolean) of object;
  TBCF2000RotorOutEvent = procedure (bcf: TBCF2000; ds: TBCF2000DAtaSource;iChannel: nativeint; rLevel: nativefloat; rm: TRotorMode) of object;
  TBCF2000SliderInEvent = procedure (bcf: TBCF2000; ds: TBCF2000DAtaSource;iChannel: nativeint; rLevel: nativefloat; var handled: boolean ) of object;
  TBCF2000SliderOutEvent = procedure (bcf: TBCF2000; ds: TBCF2000DAtaSource;iChannel: nativeint; rLevel: nativefloat; bForce: boolean = false) of object;

  TSlider = record
    Disabled: boolean;
    Position: nativefloat;
  end;

  TRotor = record
    mode: TRotorMode;
    level: nativefloat;
  end;

  TMeter = record
    level: single;
  end;

  TBCF2000DataSource = class(TSharedObject)
  private
    Fbigmessage: string;
    FBCF: TBCF2000;
    FOnSliderOut: TBCF2000SliderOutEvent;
    FOnRotorOut: TBCF2000RotorOutEvent;
    FOnSliderIn: TBCF2000SliderInEvent;
    FOnRotorIn: TBCF2000RotorInEvent;
    FOnButtonIn: TBCF2000ButtonInEvent;
    FStripText: string;
    procedure SliderIn(iChannel: nativeint; rValue: nativefloat);
    procedure RotorIn(iChannel, iTicks: nativeint);
    procedure ButtonCodeOut(iButton: nativeint; bDown: boolean);
    procedure ButtonCodeIn(iChannel: nativeint; bDown: boolean);
    procedure SEtBCF(const Value: TBCF2000);


    procedure BCFAttached;
    procedure UpdateBigMessage;
    procedure SetBigMessage(value: string);
    procedure SetStripText(value: string);
    procedure UpdateStripText;
  public

    sliders: array[0..8] of TSlider;
    rotors: array[0..8] of TRotor;
    meters: array[0..7] of TMeter;
    button_states: array[0..127] of boolean;
    previous_button_states: array[0..127] of boolean;

    procedure Detach;override;
    procedure Init; override;
    property BCF: TBCF2000 read FBCF write SEtBCF;



    procedure SliderOut(iChannel: nativeint; rLevel: nativefloat; bForce: boolean = false);
    procedure RotorOut(iChannel: nativeint; rLevel: nativefloat; rm: TRotorMode);
    procedure ButtonOut(iButton: nativeint; bDown: boolean);
    procedure MeterOut(iChannel: ni; rLevel: single);

    property OnSliderIn: TBCF2000SliderInEvent read FOnSliderIn write FOnSliderIn;
    property OnSliderOut: TBCF2000SliderOutEvent read FOnSliderOut write FOnSliderOut;
    property OnRotorIn: TBCF2000RotorInEvent read FOnRotorIn write FOnRotorIn;
    property OnRotorOut: TBCF2000RotorOutEvent read FOnRotorOut write FOnRotorOut;
    property OnButtonCodeIn: TBCF2000ButtonInEvent read FOnButtonIn write FOnButtonIn;

    property BigMEssage: string read FBigMessage write SetBigMessage;
    property StripText: string read FStripText write SetStripText;
  end;



  TBCF2000 = class (TSharedObject)
  strict private
    FMIDI: TLocalMIDI;
    FDataSource: TBCF2000DataSource;
    button_states: array[0..127] of boolean;
    FDisableMIDIout: boolean;
    FMeterState: array[0..7] of ni;
    procedure SetDataSource(const Value: TBCF2000DataSource);
    procedure DataSourceAttached;
    procedure ClearMeters;
    procedure ClearButtonLights;
    procedure ClearScribbleStrips;
    procedure ClearBigDisplay;

  public
    procedure Detach;override;
    constructor Create;override;
    destructor Destroy;override;
    procedure OnStandardMessage(slf: TSimpleMIDIBAse; iChannel, iType, iData1, iData2: nativeint);
    procedure SliderIn(iChannel: nativeint; rValue: nativefloat);
    procedure RotorIn(iChannel: nativeint; iTicks: nativeint);
    procedure SliderOut(iChannel: nativeint; rLevel: nativefloat; bForce: boolean = false);
    procedure RotorOut(iChannel: nativeint; rLevel: nativefloat; rm: TRotorMode);
    procedure MeterOut(iChannel: ni; rLevel: single);
    procedure ButtonCodeIn(iButtonCode: nativeint; bDown: boolean);
    procedure ButtonCodeOut(iButton: nativeint; bDown: boolean);
    property MIDI: TLocalMIDI read FMIDI;
    procedure EnableRobotics(iChannel: nativeint; bEnabled: boolean);
    property DataSource: TBCF2000DataSource read FDataSource write SetDataSource;
    procedure MIDI_StandardMessage(iChannel: nativeint; iType: nativeint; iData1, iData2: nativeint);
    procedure MIDI_ShortMessage(iChannel: nativeint; iType: nativeint; iData1: nativeint);
    procedure MIDI_PitchWheel(iChannel: nativeint; iValue: nativeint);overload;
    procedure MIDI_PitchWheel(iChannel: nativeint; rValue: nativefloat);overload;
    property DisableMIDIOut: boolean read FDisableMIDIout write FDisableMIDIOut;

    procedure ProcessMessages;
    procedure StripsOut(iStartChar: ni; sMessage: string);
    procedure ScribbleOut(iChannel, iColor: ni; sMEssage: string);

  end;

function ASCtoBigDisplay(s: string): TDynByteArray;


implementation

{ TBCF2000 }

function ASCtoBigDisplay(s: string): TDynByteArray;
var
  astr: ansistring;
  t: ni;
  outidx: ni;
  code: byte;
begin
  astr := s;

  outidx := 0;
  setlength(result, 12);
  for t := low(astr) to high(astr) do begin
    code := ASC_TO_BIG_DISPLAY[ord(astr[t])];
    if (code = 64) and (outidx >0) then begin
      result[outidx-1] := result[outidx-1] or 64;//special case for "."'s
    end else begin
      result[outidx] := code;
      inc(outidx);
    end;
  end;


end;

procedure TBCF2000.ButtonCodeIn(iButtonCode: nativeint; bDown: boolean);
begin
  if assigned(FDataSource) then
    FDataSource.ButtonCodeIn(iButtonCode,bDown);

end;

procedure TBCF2000.ButtonCodeOut(iButton: nativeint; bDown: boolean);
begin
  if FMIDI.CheckConnected = false then exit;

  if button_states[iButton] = bDown then exit;

  button_states[iButton] := bDown;
  if bDown then
    MIDI_StandardMessage(0, $90, iButton, 127)
  else
    MIDI_StandardMessage(0, $90, iButton, 0);

end;

procedure TBCF2000.ClearBigDisplay;
var
  u: ni;
begin
  for u := 0 to 127 do begin
    ButtonCodeOut(u,false);
  end;
end;

procedure TBCF2000.ClearButtonLights;
var
  u: ni;
begin
  for u := 0 to 127 do begin
    ButtonCodeOut(u,false);
  end;
end;

procedure TBCF2000.ClearMeters;
var
  t: ni;
begin
  for t:= 0 to 7 do begin
    MeterOut(t,0);
  end;
end;

procedure TBCF2000.ClearScribbleStrips;
var
  a: array[0..15] of byte;
begin
  a[0] := $F0;
  a[1] := $00;
  a[2] := $00;
  a[3] := $66;
  a[4] := $44;
  a[5] := $20;
  a[6] := $20;
  a[7] := $F7;

  FMIDI.SendData(pbyte(@a[0]), 8, true);


end;

constructor TBCF2000.Create;
begin
  inherited;
  FMIDI := TLocalMIDI.Create;
{$IFDEF USE_XTOUCH}
  FMIDI.EndPoint := 'X-Touch';
{$ELSE}
  FMIDI.EndPoint := 'BCF2000';
{$ENDIF}
  FMIDI.Connect;
  FMIDI.OnStandardMidiMEssage := self.OnStandardMessage;
end;

procedure TBCF2000.DataSourceAttached;
begin
  FdataSource.BCF := self;




end;

destructor TBCF2000.Destroy;
begin
  FMIDI.Free;
  FMIDI := nil;
  inherited;
end;

procedure TBCF2000.Detach;
begin
  inherited;
  DataSource := nil;
end;

procedure TBCF2000.EnableRobotics(iChannel: nativeint; bEnabled: boolean);
begin
  if Datasource = nil then exit;

  DataSource.sliders[iChannel].Disabled := not bEnabled;
end;

procedure TBCF2000.MIDI_PitchWheel(iChannel, iValue: nativeint);
begin
  if not FMIDI.CheckConnected then
    exit;

  if not DisableMIDIOut then
    FMIDI.MIDI_PitchWheel(iChannel, iValue);

end;

procedure TBCF2000.MeterOut(iChannel: ni; rLevel: single);
var
  oldval, val: ni;
const
  PEAK = 14;
begin
  if not FMIDI.CheckConnected then
    exit;

  if iChannel < 8 then begin
    if not DisableMIDIOUT then begin
//      val := 0;
//      val := (16*iChannel)+val;
//      FMIDI.MIDI_ShortMessage(0, $D0 {aftertouch}, val);

      val := lesserof(round(rLevel* PEAK), PEAK);
      oldval := FMeterState[iChannel];

      if val <> oldval then begin
        if oldval >= PEAK then begin
          FMIDI.MIDI_ShortMessage(0, $D0 {aftertouch}, 15+(16*iChannel));//CLEAR the clip light
        end;

        FMIDI.MIDI_ShortMessage(0, $D0 {aftertouch}, val+(16*iChannel));
        FMeterState[iChannel] := val;
      end;


    end;
  end;
end;

procedure TBCF2000.MIDI_PitchWheel(iChannel: nativeint; rValue: nativefloat);
begin
  if not DisableMIDIOut then
    FMIDI.MIDI_PitchWheel(iChannel, rValue);
end;

procedure TBCF2000.MIDI_ShortMessage(iChannel, iType, iData1: nativeint);
begin
  if not FMIDI.CheckConnected then
    exit;

  if not DisableMIDIOut then
    FMIDI.MIDI_ShortMessage(iChannel, iType, iData1);
end;

procedure TBCF2000.MIDI_StandardMessage(iChannel, iType, iData1,
  iData2: nativeint);
begin
  if not FMIDI.CheckConnected then
    exit;

  if not DisableMIDIOut then
    FMIDI.MIDI_StandardMessage(iChannel, iType, iData1, iData2);

end;

procedure TBCF2000.OnStandardMessage(slf: TSimpleMIDIBAse; iChannel, iType, iData1,
  iData2: nativeint);
var
  b: nativeint;
begin
  inherited;
  if Datasource = nil then exit;

  if iType = $90 then begin
    //note on then begin
    if (iData1 >=$68) and (iData1 <($68+8)) then begin
      if iData2 > 0 then
        EnableRobotics(iData1 - $68, false)
      else
        EnableRobotics(iData1 - $68, true);
    end else begin
      ButtonCodeIn(iData1, iData2 > 0);
    end;
  end else
  if iType = $E0 then begin
    if iChannel < length(datasource.sliders) then begin
      SliderIn(iChannel, (iData1+ (iData2 shl 8)) / $7fff);
    end;
  end else
  if iType = $B0 then begin
    if iChannel = 0 then begin
      if (iData1 >= $10) and (iData1 <=($10+7)) then begin
        b := iData2;
        if ((b and $40) > 0) then
          b := 0-(b and $3f)//round(power((b and $3f), 2))
        else
          b := b;//round(power((b and $3f), 2)) ;


        RotorIn(iData1-$10, b);

      end;
    end;

  end;

end;

procedure TBCF2000.ProcessMessages;
begin
  FMIDI.ProcessIncoming;
end;

procedure TBCF2000.ScribbleOut(iChannel, iColor: ni; sMEssage: string);
begin
  if not FMIDI.CheckConnected then
    exit;

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TBCF2000.SetDataSource(const Value: TBCF2000DataSource);
var
  old_ds: TBCF2000DataSource;
begin
  if value = Fdatasource then exit;
  old_ds := FDataSource;
  FDataSource := Value; //has to be set twice because recusion might reset it

  if old_ds <> nil then
    old_ds.BCF := nil;

  FDataSource := Value;//has to be set twice because recusion might reset it

  if FDataSource <> nil then
    DataSourceATtached;

end;

procedure TBCF2000.SliderIn(iChannel: nativeint; rValue: nativefloat);
begin
  if datasource = nil then exit;

  if assigned(FDatasource) then
    FDataSource.SliderIn(iChannel, rValue);
end;

procedure TBCF2000.SliderOut(iChannel: nativeint; rLevel: nativefloat; bForce: boolean);
begin
  if not FMIDI.CheckConnected then
    exit;

  if not bForce then begin
    if datasource.sliders[iChannel].Disabled then
      exit;
  end;

  MIDI_PitchWheel(iChannel, rLevel);

end;

procedure TBCF2000.StripsOut(iStartChar: ni; sMessage: string);
{
f0 00 00 66 14 12 38 <--start char
41 75 64 69 6f 20 20
4d 49 44 49 20 20 20
4d 49 44 49 20 20 20
41 75 64 69 6f 20 20
54 72 61 63 6b 37 20
54 72 61 63 6b 38 20
54 72 61 63 6b 39 20 f7
}
var
  astr: ansistring;
  dba: TDynByteArray;
  t: ni;
  outidx: ni;
begin
  if not FMIDI.CheckConnected then
    exit;

  astr := sMessage;
  setlength(dba, length(sMEssage) + 8);
  dba[0] := $f0;
  dba[1] := $00;
  dba[2] := $00;
  dba[3] := $66;
  dba[4] := $14;
  dba[5] := $12;
  dba[6] := iStartChar;
  outidx := 7;
  for t:= low(sMessage) to high(sMEssage) do begin
    dba[outidx] := ord(astr[t]);
    inc(outidx);
  end;

  dba[outidx] := $f7;

  FMIDI.SendData(@dba[0], length(dba), true);




end;

procedure TBCF2000.RotorIn(iChannel, iTicks: nativeint);
begin
  if assigned(FDataSource) then
    FDataSource.RotorIn(iChannel,iTicks);
end;

procedure TBCF2000.RotorOut(iChannel: nativeint; rLevel: nativefloat; rm: TRotorMode);
var
  i: integer;
begin
  if not FMIDI.CheckConnected then
    exit;

  i := ord(rm) - 1;

  if i < 0 then begin
    MIDI_StandardMessage(0, $B0, $30+ichannel, 0);
  end else
  if rm = rmWidth then begin
    MIDI_StandardMessage(0, $B0, $30+ichannel, round(rLevel * 5)+1+(16*i));
  end else
  begin
    MIDI_StandardMessage(0, $B0, $30+ichannel, round(rLevel * 10)+1+(16*i));
//    MIDI_StandardMessage(0, $B0, random()+ichannel, round(rLevel * 10)+1+(16*i));
  end;



end;


{ TBCF2000DataSource }



procedure TBCF2000DataSource.BCFAttached;
var
  t: nativeint;
begin
  UpdateBigMessage;
  UpdateStripText;
  for t:= 0 to high(sliders) do begin
    sliders[t].Disabled := false;
    SliderOut(t, sliders[t].Position);
    RotorOut(t, rotors[t].level, rotors[t].mode);
    if t < high(meters) then begin
      MeterOut(t, meters[t].level);
    end;
  end;

  for t := 0 to 127 do begin
    ButtonOut(t, button_states[t]);
  end;
end;

procedure TBCF2000DataSource.ButtonCodeIn(iChannel: nativeint; bDown: boolean);
var
  handled: boolean;
  bChanged: boolean;
begin
  handled := false;

  bChanged := previous_button_states[iChannel] <> bDown;
  previous_button_states[iChannel] := bDown;

  if assigned(FOnButtonIn) then
    FOnButtonIn(FBCF, self, iChannel, bDown, bChanged, handled);


end;

procedure TBCF2000DataSource.ButtonCodeOut(iButton: nativeint; bDown: boolean);
begin
  if assigned(FBCF) then
    FBCF.ButtonCodeOut(iButton, bDown);

end;

procedure TBCF2000DataSource.ButtonOut(iButton: nativeint; bDown: boolean);
begin
  button_states[iButton] := bDown;

  if assigned(FBCF) then
    FBCF.ButtonCodeOut(iButton, bDown);
end;

procedure TBCF2000DataSource.Detach;
begin
  inherited;
  BCF := nil;
end;


procedure TBCF2000DataSource.Init;
begin
  inherited;
  StripText := self.ClassName;
end;

procedure TBCF2000DataSource.MeterOut(iChannel: ni; rLevel: single);
begin
  if assigned(BCF) then
    BCF.MeterOut(iChannel, rLevel);
end;

procedure TBCF2000DataSource.RotorIn(iChannel, iTicks: nativeint);
var
  handled: boolean;
begin
  handled := false;
  if assigned(FOnRotorIn) then
    FOnRotorIn(bcf, self, iChannel, iTicks, handled);

  if not handled then begin


  end;
end;

procedure TBCF2000DataSource.RotorOut(iChannel: nativeint; rLevel: nativefloat;
  rm: TRotorMode);
begin
  rotors[iChannel].mode := rm;
  rotors[iChannel].level := rLevel;
  if assigned(BCF) then
    BCF.RotorOut(iChannel, rLevel, rm);
end;

procedure TBCF2000DataSource.SEtBCF(const Value: TBCF2000);
var
  Old: TBCF2000;
begin
  if value = FBCF then exit;


  old := FBCF;

  FBCF := Value; //has to be set twice because recusion might reset it


  if Old <> nil then
    old.DataSource := nil;

  FBCF := Value; //has to be set twice because recusion might reset it

  if FBCF <> nil then
    BCFAttached



end;

procedure TBCF2000DataSource.SetBigMessage(value: string);
begin
  FBigMessage := Value;
  if assigned(BCF) then
    UpdateBigMessage;
end;

procedure TBCF2000DataSource.SliderIn(iChannel: nativeint; rValue: nativefloat);
var
  handled: boolean;
begin
  handled := false;
  if assigned(FOnSliderIn) then
    FOnSliderIn(self.BCF, self, iChannel, rValue, handled);

  if not handled then
    SliderOut(iChannel, rValue, true);

end;

procedure TBCF2000DataSource.SliderOut(iChannel: nativeint; rLevel: nativefloat;
  bForce: boolean);
begin
  //if bcf = nil then exit;

  sliders[iChannel].Position := rLevel;
  if assigned(BCF) then
    BCF.SliderOut(iChannel, rLevel, bForce);
end;

procedure TBCF2000DataSource.SetStripText(value: string);
begin
  FStripText := Value;
  UpdateSTripText;


end;

procedure TBCF2000DataSource.UpdateBigMessage;
var
  a:  TDynByteArray;
  t: ni;
begin
  if assigned(FBCF) then begin
    a := ASCtoBigDisplay(FbigMessage);
    setlength(a, 12);
    for t:= 0 to high(a) do begin
      BCF.MIDI_StandardMessage(0, $B0, 75-t, a[t]);
    end;
  end;


end;

procedure TBCF2000DataSource.UpdateStripText;
begin
  if assigned(BCF) then
    BCF.StripsOut(0, StripText);
end;

end.
