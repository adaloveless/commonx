unit MusicTheory;

interface

uses
  sysutils, stringx;

const
  NOTE_FREQUENCIES : array [0..12] of real = (523.28, 554.40, 587.36, 622.24, 659.28, 698.48, 740.00, 784.00, 830.64, 880.0, 932.32, 987.84, 1046.56);

const
  NOTEA		= 0;
  NOTEASHARP	= 1;
  NOTEB		= 2;
  NOTEC		= 3;
  NOTECSHARP	= 4;
  NOTED		= 5;
  NOTEDSHARP	= 6;
  NOTEE		= 7;
  NOTEF		= 8;
  NOTEFSHARP	= 9;
  NOTEG		= 10;
  NOTEGSHARP	= 11;
  NOTE_UNDEFINED = $FFFFFF;

  OCTAVE = 12;

  NOTEDFLAT = NOTECSHARP;
  NOTEEFLAT = NOTEDSHARP;
  NOTEGFLAT = NOTEFSHARP;
  NOTEAFLAT = NOTEGSHARP;
  NOTEBFLAT = NOTEASHARP;

  WHOLESTEP = 2;
  HALFSTEP = 1;

  MODE_AOLIAN_RELATIVE = (NOTEA-NOTEA);
  MODE_LOCRIAN_RELATIVE = (NOTEB-NOTEA);
  MODE_IONIAN_RELATIVE = (NOTEC-NOTEA);
  MODE_DORIAN_RELATIVE = (NOTED-NOTEA);
  MODE_PHYGIAN_RELATIVE = (NOTEE-NOTEA);
  MODE_LYDIAN_RELATIVE = (NOTEF-NOTEA);
  MODE_MIXOLYDIAN_RELATIVE = (NOTEG-NOTEA);

  MODE_MAJOR_RELATIVE = MODE_IONIAN_RELATIVE;
  MODE_MINOR_RELATIVE = MODE_AOLIAN_RELATIVE;
  MODE_UNDEFINED = $FFFF;

function GetNearestOctave(ikeyOffset: integer): integer;
function GetTransposition( sSourceKey, sSourceMode, sTargetKey, sTargetMode: string ): integer;overload;
function GetTransposition( iSourceKey, iSourceMode, iTargetKey, iTargetMode: integer ): integer;overload;
function StringToKeyValue( sKey: string ): integer;
function StringToModeOffset( sMode: string ): integer;

implementation

//---------------------------------------------------------------------
function StringTomodeOffset(sMode: string): integer;
begin
  result := MODE_UNDEFINED;
	if lowercase(sMode)=lowercase('Minor') then result := MODE_MINOR_RELATIVE;
	if lowercase(sMode)=lowercase('Major') then result := MODE_MAJOR_RELATIVE;
	if lowercase(sMode)=lowercase('Aeolian') then result := MODE_AOLIAN_RELATIVE;
	if lowercase(sMode)=lowercase('Locrian') then result := MODE_LOCRIAN_RELATIVE;
	if lowercase(sMode)=lowercase('Ionian') then result := MODE_IONIAN_RELATIVE;
	if lowercase(sMode)=lowercase('Dorian') then result := MODE_DORIAN_RELATIVE;
	if lowercase(sMode)=lowercase('Phrygian') then result := MODE_PHYGIAN_RELATIVE;
	if lowercase(sMode)=lowercase('Lydian') then result := MODE_LYDIAN_RELATIVE;
	if lowercase(sMode)=lowercase('Mixolydian') then result := MODE_MIXOLYDIAN_RELATIVE;

end;
//---------------------------------------------------------------------
function StringToKeyValue( sKey: string ): integer;
begin
	result := NOTE_UNDEFINED;

	if ( lowercase(sKey)=lowercase('A')) then result := NOTEA;
	if ( lowercase(sKey)=lowercase('B')) then result := NOTEB;
	if ( lowercase(sKey)=lowercase('C')) then result := NOTEC;
	if ( lowercase(sKey)=lowercase('D')) then result := NOTED;
	if ( lowercase(sKey)=lowercase('E')) then result := NOTEE;
	if ( lowercase(sKey)=lowercase('F')) then result := NOTEF;
	if ( lowercase(sKey)=lowercase('G')) then result := NOTEG;
	if ( lowercase(sKey)=lowercase('AFLAT')) then result := NOTEAFLAT;
	if ( lowercase(sKey)=lowercase('BFLAT')) then result := NOTEBFLAT;
	if ( lowercase(sKey)=lowercase('DFLAT')) then result := NOTEDFLAT;
	if ( lowercase(sKey)=lowercase('EFLAT')) then result := NOTEEFLAT;
	if ( lowercase(sKey)=lowercase('GFLAT')) then result := NOTEGFLAT;
	if ( lowercase(sKey)=lowercase('Ab')) then result := NOTEAFLAT;
	if ( lowercase(sKey)=lowercase('Bb')) then result := NOTEBFLAT;
	if ( lowercase(sKey)=lowercase('Db')) then result := NOTEDFLAT;
	if ( lowercase(sKey)=lowercase('Eb')) then result := NOTEEFLAT;
	if ( lowercase(sKey)=lowercase('Gb')) then result := NOTEGFLAT;
	if ( lowercase(sKey)=lowercase('Asharp')) then result := NOTEASHARP;
	if ( lowercase(sKey)=lowercase('Csharp')) then result := NOTECSHARP;
	if ( lowercase(sKey)=lowercase('Dsharp')) then result := NOTEDSHARP;
	if ( lowercase(sKey)=lowercase('Fsharp')) then result := NOTEFSHARP		;
	if ( lowercase(sKey)=lowercase('Gsharp')) then result := NOTEGSHARP;
	if ( lowercase(sKey)=lowercase('A#')) then result := NOTEASHARP;
	if ( lowercase(sKey)=lowercase('C#')) then result := NOTECSHARP;
	if ( lowercase(sKey)=lowercase('D#')) then result := NOTEDSHARP;
	if ( lowercase(sKey)=lowercase('F#')) then result := NOTEFSHARP		;
	if ( lowercase(sKey)=lowercase('G#')) then result := NOTEGSHARP;

end;
//-------------------------------------------------------------------------------------------------
function GetNearestOctave(ikeyOffset: integer): integer;
begin
  result := iKeyOffset;

	while ( result > 6 ) do result := result- OCTAVE;

	while ( result < -6 ) do result := result + OCTAVE;

end;
function GetTransposition(iSourceKey, iSourceMode, iTargetKey, iTargetMode: integer ): integer;
var
  iResultMode, iResultKey, iResult: integer;
begin
	if (( iSourceKey = NOTE_UNDEFINED ) or ( iTargetKey = NOTE_UNDEFINED ) ) then
  begin
    result := 0;
    exit;
	end;


	if (( iSourceMode = MODE_UNDEFINED ) or ( iTargetMode = MODE_UNDEFINED ) ) then
  begin
    result := 0;
    exit;
	end;


	iResultMode := iTargetMode - iSourceMode;
	iResultKey := iTargetKey - iSourceKey;
	iResult := iResultKey - iResultMode;
	iResult := iResult;

  result := GetNearestOctave(iResult);

	result := iResult;
end;
//-------------------------------------------------------------------------------------------------
function GetTransposition( sSourceKey, sSourceMode, sTargetKey, sTargetMode: string ): integer;
var
  iSourceKey, iTargetKey: integer;
  iSourceMode, iTargetMode: integer;
  iResultMode, iResultKey: integer;
begin
  sSourceKey := TrimStr(sSourceKey);
  sSourceMode := TrimStr(sSourceMode);
  sTargetKey := TrimStr(sTargetKey);
  sTargetMode := TrimStr(sTargetMode);


	iSourceMode := StringToModeOffset( sSourceMode );
	iSourceKey := StringToKeyValue( sSourceKey );
	iTargetMode := StringToModeOffset( sTargetMode );
	iTargetKey := StringToKeyValue( sTargetKey );
	result := GetTransposition( iSourceKey, iSourceMode, iTargetKey, iTargetMode );
end;

end.
