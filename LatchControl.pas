unit LatchControl;

interface

uses
  typex, sharedobject, tickcount;

type
  TAbstractLatchControl = class(TSharedObject)
  public
    procedure SetValue(b: byte);virtual;abstract;
    procedure Biton(bit: byte);virtual;abstract;
    procedure BitOff(bit: byte);virtual;abstract;
  end;


implementation

{ TLatchControl }


end.
