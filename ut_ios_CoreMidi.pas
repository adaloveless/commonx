unit ut_ios_CoreMidi;

interface

uses
  typex, classes, sysutils, unittest, systemx, ioutils,
{$IFDEF IOS}
  iosapi.foundation,
  iosapi.CoreAudio,
{$ENDIF}
  stringx;


type
  TUT_IOS_CoreMIDI = class(TUnitTest)
  public
    procedure DoExecute;override;
    function Test1: string;
    function Test2: string;
    function Test3: string;
    function Test4: string;
    function Test5: string;
    function Test6: string;
    function Test7: string;
    function Test8: string;
    function Test9: string;
    function Test10: string;

  end;

implementation




{ TUT_IOS_CoreMIDI }

procedure TUT_IOS_CoreMIDI.DoExecute;
var
  sl: TStringlist;
begin
  inherited;
  sl := TStringlist.create;
  try
    case Variation of
      1: begin
        utresult := Test1();
      end;
      2: begin
        utresult := Test2();
      end;
      3: begin
        utresult := Test3();
      end;
      4: begin
        utresult := Test4();
      end;
      5: begin
        utresult := Test5();
      end;
      6: begin
        utresult := Test6();
      end;
      7: begin
        utresult := Test7();
      end;
      8: begin
        utresult := Test8();
      end;
      9: begin
        utresult := Test9();
      end;
      10: begin
        utresult := Test10();
      end;
    end;

  finally
    sl.Free;
    sl := nil;
  end;

end;

function TUT_IOS_CoreMIDI.Test1: string;
begin
  utresult := 'no execution';
end;


function TUT_IOS_CoreMIDI.Test10: string;
begin
  utresult := 'no execution';
end;

function TUT_IOS_CoreMIDI.Test2: string;
begin
  utresult := 'no execution';
end;


function TUT_IOS_CoreMIDI.Test3: string;
begin
  utresult := 'no execution';
end;

function TUT_IOS_CoreMIDI.Test4: string;
begin
  utresult := 'no execution';
end;


function TUT_IOS_CoreMIDI.Test5: string;
begin
  utresult := 'no execution';
end;

function TUT_IOS_CoreMIDI.Test6: string;
begin
  utresult := 'no execution';
end;

function TUT_IOS_CoreMIDI.Test7: string;
begin
  utresult := 'no execution';
end;

function TUT_IOS_CoreMIDI.Test8: string;
begin
  utresult := 'no execution';
end;

function TUT_IOS_CoreMIDI.Test9: string;
begin
  utresult := 'no execution';
end;

initialization
  UTF.RegisterClass(TUT_IOS_CoreMIDI);

end.
