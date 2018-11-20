unit DOReleaseThread;
{$Message Hint 'This code uses some bad old threading techniques.'}

interface

uses
  Classes, managedthread;

type
  TDOReleaseThread = class(TManagedThread)
  private
    { Private declarations }
  protected
    FThreadManager: TObject;
    FList: Tlist;
    iOriginalCount, iIterations: integer;

    procedure DoExecute; override;
    procedure DoRelease;
    procedure DoListFree;
    procedure DoOpenProgress;
    procedure DoCloseProgress;
  public
    property List: Tlist read FList write FList;
  end;

implementation

uses
  DataObject, DataObjectServices, ThreadManager;

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TDOReleaseThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TDOReleaseThread }




procedure TDOReleaseThread.DoCloseProgress;
begin
//TODO -cunimplemented: unimplemented block
end;

procedure TDOReleaseThread.DoListFree;
begin
  list.free;
end;

procedure TDOReleaseThread.DoOpenProgress;

begin

//TODO -cunimplemented: unimplemented block
end;

procedure TDOReleaseThread.DoRelease;
var
  obj: TObject;
begin
  try
    obj := TObject(list[list.count-1]);
    if obj is TDataObject then
      TDataObject(obj).Release
    else
      //**WARNING** FREE ONLY WORKS BECAUSE an ASSUMPTION IS MADE THAT T-H-I-S
      //TOKEN WILL N-O-T BE REFERENCED VIA A COM INTERFACE
      try
        TDataObjectToken(obj).free;
      except
      end;



    list.delete(list.count-1);
  finally
  end;
end;


procedure TDOReleaseThread.DoExecute;
begin

  if assigned(list) then begin
    { Place thread code here }
    iOriginalCount := list.count;
    while list.Count > 0 do begin
      DoRelease;
      inc(iIterations);
//      Synchronize(DoUpdateProgress);
    end;
  end;
    //!ELSE EXCEPTION

//  Synchronize(DoCloseProgress);
  DoListFree;

end;


end.

