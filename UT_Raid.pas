unit UT_Raid;

interface

uses
  typex, systemx, stringx, raid, unittest, sysutils;

type
  TUT_RAID = class(TUnitTest)
  protected
    function RaidTest(iDrives: ni; iStripeSize: ni; iDestroyDrive: ni = -1): string;
  public
    procedure DoExecute;override;


  end;



implementation





{ TUT_RAID }

procedure TUT_RAID.DoExecute;
begin
  inherited;
  case variation of
    1: begin
      VariationName := 'RAID, non-parity';
      utresult := RaidTest(1,8*2);
    end;
    10: begin
      VariationName := 'RAID, basic Mirror';
      utresult := RaidTest(2,8*2);
    end;
    20: begin
      VariationName := 'RAID, basic Mirror destroy drive 0';
      utresult := RaidTest(2,8*2,0);
    end;
    30: begin
      VariationName := 'RAID, basic Mirror destroy drive 1';
      utresult := RaidTest(2,8*2,1);
    end;
    40..49: begin
      VariationName := '512 byte RAID x'+inttostr((variation-40)+2);
      utresult := RaidTest((variation-40)+2,512);
    end;
    50..59: begin
      VariationName := '512 byte broken 0 RAID x'+inttostr((variation-50)+2);
      utresult := RaidTest((variation-50)+2,512,0);
    end;
    60..69: begin
      VariationName := '512 byte broken parity RAID x'+inttostr((variation-60)+2);
      utresult := RaidTest((variation-60)+2,512,(variation-60)+1);
    end;
    70..79: begin
      VariationName := '262,144 byte broken 0 RAID x'+inttostr((variation-50)+2);
      utresult := RaidTest((variation-70)+2,262144,0);
    end;




  end;

end;

function TUT_RAID.RaidTest(iDrives: ni; iStripeSize: ni; iDestroyDrive: ni = -1): string;
var
  rc: TRaidCalculator;
  bIn, bOut: array of byte;
  ext: TRaidAssembledBuffer;
begin

  setlength(bIn, iStripeSize);
  setlength(bout, iStripeSize);
  fillmemrandom(@bIn[0], length(bIn));


  ext := TRaidAssembledBuffer.create;
  try
    rc := TRaidCalculator.create;
    try
      //move memory into stripe calculator
      rc.assembled := ext;
      movemem32(@rc.assembled.single[0], @bIn[0], iStripeSize);
      rc.assembled.payloadsizeinbytes := iStripeSize;
      rc.drives := iDrives;
      rc.SingleToPieces;

      if iDestroyDrive > -1 then begin
        fillmemrandom(@rc.pieces[iDestroyDrive].payload[0], SizeOf(rc.pieces[iDestroyDrive].payload));
      end;

      rc.PiecesToSingle;
      movemem32(@bOut[0],@rc.assembled.single[0], iStripeSize);
      if CompareMem(@bOUt[0], @bIn[0], iStripeSize) then
        result := 'success'
      else
        result := 'fail';

    finally
      rc.free;
    end;
  finally
    //ext.free;
  end;

end;


initialization
  UTF.RegisterClass(TUT_RAID);

end.
