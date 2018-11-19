unit SimpleMIDIBase;

interface

uses
  simpleabstractconnection, typex, systemx, midiconsts;

type
  TSimpleMIDIBase = class;//forward

  TStandardMidiMessageHandler = procedure(sender: TSimpleMIDIBase; iChannel: nativeint; iType: nativeint; iData1, iData2: nativeint) of Object;

  TSimpleMIDIBase = class(TSimpleAbstractConnection)
  private

  protected
    currentmessagebody: array[0..1] of byte;
    RunningStatusIn: byte;
    RunningStatusOut: byte;
    expectedbytes:  nativeint;
    messageptr: nativeint;
    FonStandardMidiMessage: TStandardMIDIMessageHandler;
    FOnStandardMIDIMessage2: TStandardMIDIMEssageHandler;



    procedure ProcessIncoming;virtual;
    procedure HandleStandardMessage(iChannel, iType, iData1, iData2: nativeint);
  public
    outObjectRef: TObject;
    property OnStandardMidiMEssage: TStandardMIDIMEssageHandler read FOnStandardMIDIMessage write FOnStandardMIDIMessage;
    property OnStandardMidiMEssage2: TStandardMIDIMEssageHandler read FOnStandardMIDIMessage2 write FOnStandardMIDIMessage2;


  end;


implementation



procedure TSimpleMIDIBase.ProcessIncoming;
var
  i1,i2,i3: byte;
begin
{$IFDEF MIDI_ENABLE_MESSAGE_PROCESSING}
  while true do begin
    if not connected then
      exit;

    if not WaitForData(1) then
      exit;

    if ReadData(pbyte(@i1), 1, true) = 0 then
      exit;

    if (i1 and $80) > 0 then begin
        runningstatusin := i1;
      if (i1 and $f0) = $F0 then begin
          expectedbytes := -1;
          messageptr := 0;
      end else
      if (i1 and $f0) = $D0 then begin
        expectedbytes := 1;
        messageptr := 0;
      end;
      if (i1 and $f0) = $C0 then begin
        expectedbytes := 1;
        messageptr := 0;
      end else begin
        expectedbytes := 2;
        messageptr := 0;
      end;
    end else begin
      if expectedbytes > 0 then begin
        currentmessagebody[messageptr] := i1;
        inc(messageptr);
        if messageptr = expectedbytes then begin
          if ((RunningStatusIn and $f0) = MIDI_NOTE_ON) and (currentmessagebody[1] = 0) then begin
            HandleStandardMessage(MIDI_NOTE_OFF , MIDI_NOTE_OFF, currentmessagebody[0], currentmessagebody[1]);
          end else
          if expectedbytes = 2 then begin
              HandleStandardMessage(RunningStatusIn and $0f, RunningStatusIn and $f0, currentmessagebody[0], currentmessagebody[1]);
            messageptr := 0;
          end;
        end;
      end;
    end;
  end;
{$ELSE}
  raise Ecritical.create('define MIDI_ENABLE_MESSAGE_PROCESSING in project');
{$ENDIF}
end;


procedure TSimpleMIDIBase.HandleStandardMessage(iChannel, iType, iData1,
  iData2: nativeint);
begin
  if Assigned(FonStandardMidiMessage) then
    FonStandardMidiMessage(self, iChannel, iType, iData1, iData2);

  if Assigned(FonStandardMidiMessage2) then
    FonStandardMidiMessage2(self, iChannel, iType, iData1, iData2);

end;



end.
