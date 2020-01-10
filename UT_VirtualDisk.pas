unit UT_VirtualDisk;

interface

uses
  unittest, globaltrap, typex, virtualdisk, virtualdisk_advanced, systemx, sysutils, classes, debug, helpers_stream, dirfile, memoryfilestream, raid, virtualdiskconstants;


const
  RANDOMS = 10000000;
type
  TUT_VirtualDisk_Advanced = class(TUnitTest)
  protected
    ridx: int64;
    predictable_randoms: array[0..RANDOMS-1] of double;
    function UT_CompareWithNonLinear: string;
    function UT_CompareWithNonLinear_BrokenMirror(iMirrorIDX: ni): string;
    function UT_CompareWithNonLinear_forwardbuild: string;
    function UT_CompareWithNonLinearAfterPayloadMove: string;
    function UT_CompareWithNonLinearComplexArrange: string;
    function UT_CompareWithNonLinearDuringPayloadMove: string;
    function UT_Variable(BLOCKS: ni; ITERATIONS: ni; MoveDuringPayload: boolean): string;
    function UT_VariableRaid(BLOCKS: ni; ITERATIONS: ni; payCount: ni; MoveDuringPayload: boolean): string;
    function UT_VariableRaid_Refunct(BLOCKS: ni; ITERATIONS: ni; payCount: ni; MoveDuringPayload: boolean): string;
    function UT_DirtSimple(iStopMarker: ni; iPayloadCount: ni = 1): string;
    function UT_TwoBigBlock(iStopMarker: ni; iPayloadCount: ni = 1): string;
    procedure CleanupOldStuff;
    function UT_CreateAndDestroy: string;
    procedure GenerateRandomFile;
    procedure LoadRandomFile;
    function RandomFileName: string;
    function GEtPredictableRandom(iScale: int64): int64;

  public
    PROCEDURE Init;override;
    procedure DoExecute;override;
  end;


implementation




{ TUT_VirtualDisk_Advanced }

procedure TUT_VirtualDisk_Advanced.CleanupOldStuff;
var
  f1,f2,f3,f4,f5: string;
  t: ni;
begin
  f1 := GetTempPath+'vdnl.vd';
  f2 := GEtTempPath+'vda.vda';
  f3 := GEtTempPath+'vda.vdpayload';
  f4 := GEtTempPath+'vda2.vdpayload';
  f5 := GEtTempPath+'mirror.vdpayload';

  if fileexists(f1) then deletefile(f1);
  if fileexists(f2) then deletefile(f2);
  if fileexists(f3) then deletefile(f3);
  if fileexists(f4) then deletefile(f4);
  if fileexists(f5) then deletefile(f5);

  for t:= 0 to 10 do begin
    f3 := GEtTempPath+'vda'+inttostr(t)+'.vdpayload';
    if fileexists(f3) then deletefile(f3);
  end;

end;

procedure TUT_VirtualDisk_Advanced.DoExecute;
begin
  inherited;
  CleanupOldStuff;
//  if variation < 82 then begin
//    utresult := 'success';
//    exit;
//  end;

    //if variation < 60 then exit;//variation := 60;
    case variation of
      1: begin
//        VariationName := 'Dirt Simple(1)';
//        utresult := UT_DirtSimple(-1, 1);

      end;
      2: begin
//        VariationName := 'Dirt Simple(2)';
//        utresult := UT_DirtSimple(-1, 2);
      end;
      3: begin
//        VariationName := 'Dirt Simple(3)';
//        utresult := UT_DirtSimple(-1, 3);
      end;
      4: begin
//        VariationName := 'TwoBigBlock(1)';
//        utresult := UT_TwoBigBlock(-1, 1);
      end;
      5: begin
//        VariationName := 'TwoBigBlock(2)';
//        utresult := UT_TwoBigBlock(-1, 2);
      end;
      6: begin
//        VariationName := 'TwoBigBlock(3)';
//        utresult := UT_TwoBigBlock(-1, 3);
      end;
      7: begin
        VariationNAme := 'calculations';
        utresult := '';
        utresult := utresult + 'BLOCK_SIZE='+inttostr(BLOCKSIZE)+NEWLINE;
        utresult := utresult + 'BIG_BLOCK_SIZE_IN_BLOCKS='+inttostr(LEGACY_BIG_BLOCK_SIZE_IN_BLOCKS)+NEWLINE;
        utresult := utresult + 'RAID_STRIPE_SIZE_IN_BLOCKS='+inttostr(RAID_STRIPE_SIZE_IN_BLOCKS)+NEWLINE;
        utresult := utresult + 'STRIPES_PER_BIG_BLOCK='+inttostr(STRIPES_PER_BIG_BLOCK)+NEWLINE;
        utresult := utresult + 'BIG_BLOCK_SIZE_IN_BYTES='+inttostr(BIG_BLOCK_SIZE_IN_BYTES)+NEWLINE;
        utresult := utresult + 'RaidPayloadSizeINQWords='+inttostr(TRaidCalculator.GetPayloadSizeInQWords(1))+NEWLINE;
        utresult := utresult + 'MAX_STRIPE_SIZE_IN_QWORDS='+inttostr(MAX_STRIPE_SIZE_IN_QWORDS)+NEWLINE;
        utresult := utresult + 'PieceSizeUnpadded='+inttostr(TRaidCalculator.GetPieceSizeUnPadded(1))+NEWLINE;
        utresult := utresult + 'PieceSizePAdded='+inttostr(TRaidCalculator.GetPieceSizePadded(1))+NEWLINE;
        utresult := utresult + 'GetBigBlockSizeInBytes(1, false)='+inttostr(GetBigBlockSizeInBytes(1, false))+NEWLINE;
        utresult := utresult + 'GetBigBlockSizeInBytes(1, true)='+inttostr(GetBigBlockSizeInBytes(1, true))+NEWLINE;
//




      end;
      8: begin
        VariationName := 'BrokenMirror(0)';
        utresult := UT_CompareWithNonLinear_BrokenMirror(0);

//        utresult := UT_TwoBigBlock(-1, 3);
      end;
      9: begin
        VariationName := 'BrokenMirror(1))';
        utresult :=  UT_CompareWithNonLinear_BrokenMirror(1);
      end;
      10: begin
        VariationName := 'Compare with older less advanced VD forward-built';
        utresult := UT_CompareWithNonLinear_ForwardBuild;
      end;
      11: begin
        VariationName := 'Compare with older less advanced VD reverse-built';
        utresult := UT_CompareWithNonLinear;
      end;
      20: begin
        VariationName := 'Compare after data migration';
        utresult := UT_CompareWithNonLinearAfterPayloadMove;
      end;
      30: begin
        VariationName := 'Compare DURING data migration';
        utresult := UT_CompareWithNonLinearDuringPayloadMove;
      end;
      40: begin
        VariationName := 'Compare DURING data migration (long x 1,000,000)';
        utresult := UT_Variable(1000000, 20000, false);
      end;
      41: begin
        VariationName := 'Compare DURING data migration (long x 1,000,000)';
        utresult := UT_Variable(1000000, 20000, true);
      end;
      50: begin
        VariationName := 'Compare DURING data migration (long x 3,000,000)';
        utresult := UT_Variable(3000000, 20000, false);
      end;
      51: begin
        VariationName := 'Compare DURING data migration (long x 3,000,000)';
        utresult := UT_Variable(3000000, 20000, true);
      end;
      60..69: begin
        VariationName := 'Compare RAID, no migration '+inttostr(variation-58)+' payloads';
        utresult := UT_VariableRaid(120000, 20000, variation-58, false);
      end;
      70..79: begin
        VariationName := 'Compare RAID, no migration '+inttostr(variation-68)+' payloads';
        utresult := UT_VariableRaid(1200000, 40000, variation-68, false);
      end;
      80..89: begin
        VariationName := 'Compare RAID with refunct drive, no migration '+inttostr(variation-78)+' payloads';
        utresult := UT_VariableRaid_Refunct(1200000, 40000, variation-78, false);
      end;



    end;
end;

procedure TUT_VirtualDisk_Advanced.GenerateRandomFile;
var
  fs: TMemoryfileStream;
  d: double;
begin
  if fileexists(randomfilename) then begin
    if getFileSize(randomfilename) >= (RANDOMS * sizeof(double)) then begin
      exit;
    end;
  end;

  fs := TMemoryfileStream.create(randomfilename, fmCreate);
  try
    fs.Seek(0,soBeginning);
    while fs.Size < (RANDOMS*sizeof(double)) do begin
      d := random(RANDOMS)/RANDOMS;
      Stream_GuaranteeWrite(fs, @d, sizeof(double));
    end;
  finally
    fs.free;
  end;


end;

function TUT_VirtualDisk_Advanced.GEtPredictableRandom(iScale: int64): int64;
begin
  result := round(predictable_randoms[ridx] * iScale);
  ridx := (ridx + 1) mod (high(predictable_randoms)+1);

end;

procedure TUT_VirtualDisk_Advanced.Init;
begin
  inherited;
  GenerateRAndomFile;
  LoadRandomFile;
  //SkipTo := 50;
end;

procedure TUT_VirtualDisk_Advanced.LoadRandomFile;
var
  fs: TFileStream;
begin
  fs := TFileStream.create(randomfilename, fmOpenRead+fmShareExclusive);
  try
    stream_guaranteeread(fs, @predictable_randoms, sizeof(predictable_randoms));
  finally
    fs.free;
  end;

end;

function TUT_VirtualDisk_Advanced.RandomFileName: string;
begin
  result := GetTempPath+'random.bin';
end;

function TUT_VirtualDisk_Advanced.UT_CompareWithNonLinear: string;
var
  vdnl: TVirtualDisk_SimpleNonLinear;
  vda: TVirtualDisk_Advanced;
  vda_array, vdnl_array, rarray: array[0..BLOCKSIZE-1] of byte;
  blk: cardinal;
  t,u: ni;
  sRandomFile: string;
  fs: TFileSTream;
  f1,f2,f3: string;
const
  BLOCKS:int64 = 65535*4;
  ITERATIONS = 20000;
begin
  vdnl := TVirtualDisk_SimpleNonLinear.create;
  vda := TVirtualDisk_Advanced.create;
  try
    sRandomFile := GEtTempPath+'random.random';
    if not fileexists(sRandomFile) then begin
      fs := TFileStream.create(sRAndomFile, fmCreate);
      try
        for t:= 0 to 65535 do begin
          blk := random(65535);
          fs.Write(blk, sizeof(blk));

        end;
      finally
        fs.free;
      end;

    end;
    f1 := GetTempPath+'vdnl.vd';
    f2 := GEtTempPath+'vda.vda';
    f3 := GEtTempPath+'vda.vdpayload';

    if fileexists(f1) then deletefile(f1);
    if fileexists(f2) then deletefile(f2);
    if fileexists(f3) then deletefile(f3);
    vdnl.FileName := f1;
    vda.FileName := f2;
    vdnl.Size := BLOCKS*BLOCKSIZE*4;
    vda.Size := 2000+MEGA;//BLOCKS*BLOCKSIZE*4;

    //fill disk with randoms
    fs := TFileStream.create(sRandomFile, fmOpenRead+fmShareExclusive);
    fs.Seek(0,0);


    //ConsoleLog('Filling '+inttostr(u));
    for t:= 0 to BLOCKSIZE-1 do begin
      rarray[t] := random(255);
    end;

    Debug.Log(self,'Initial write.');
    //vda.EnableOptionalDebugging := true;
    for u := BLOCKS-1 downto 0 do begin
      //ConsoleLog('Filling '+inttostr(u));
      for t:= 0 to BLOCKSIZE-1 do begin
        //rarray[t] := random(255);
        rarray[t] := u and $FF;
      end;
      if u = 131071 then
        Debug.Log(self,'!'+inttostr(u));
//      Debug.Log('a'+inttostr(u));

      vdnl.WriteBlocks(u, 1, @rarray[0]);
      if u = 389247 then begin
        Debug.Log(self,'Initial write.'+inttostr(u));
        debug.Log(self,vda.DebugVatStructure);
      end;
      if u = 262143 then
        Debug.log(self,'Trap');
      vda.GuaranteeWriteBlocks(u, 1, @rarray[0]);
//      Debug.Log('b'+inttostr(u));
    end;


//    vdnl.WriteBlocks(u, 1, @rarray[0]);
//    vda.WriteBlocks(u, 1, @rarray[0]);

    for u := 0 to ITERATIONS-1 do begin
      if u = 1077 then
        Debug.Log(self,'trap');
      if u mod 1000 = 0 then
        Debug.Log(self,'Iteration '+inttostr(u));
      //ConsoleLog('Testing '+inttostr(u));

//      if (u = 610) then
//        vda.EnableOptionalDebugging := true
//      else
//        vda.EnableOptionalDebugging := true;


      //blk := random(BLOCKS-1);
      fs.REad(blk, sizeof(blk));


//      Debug.Log('Random write block = '+inttostr(blk));

      //write some random block
      for t:= 0 to BLOCKSIZE-1 do begin
        rarray[t] := ((BLOCKSIZE-1)-t) and $FF;//random(255);
      end;

//      if (u = 10368) then
//        Debug.Log('trap');

//      Debug.Log('RAndom write vdnl');
      vdnl.WriteBlocks(blk, 1, @rarray[0]);
//      Debug.Log('Random Write vda');
      vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);

//      Debug.Log('RAndom Read vdnl');
      vdnl.ReadBlocks(blk, 1, @vdnl_array[0]);
//      Debug.Log('Random Read vda');
      vda.GuaranteeREadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Compare vda against original');
      if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'vdnl:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        Debug.Log(self,'*****************RECONCILE******* Attempt write of predictable values.');


        //rewrite prediactable stuff
        for t:= 0 to (BLOCKSIZE-1) do begin
          rarray[t] := t and $FF;
        end;

        Debug.Log(self,'bring vda back to predictable');
        vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
        vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'orig:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
          Debug.Log(self,'Predicatable does not match.');
        end;

        raise ECritical.create('fail @ iteration'+inttostr(u)+' blk:'+inttostr(blk));

      end;

//      Debug.Log('Compare Random Writes (should still be cached).');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @ iteration'+inttostr(u)+' blk:'+inttostr(blk));
      end;

      //if (u mod 100) = 0 then
      //read some other random block
      blk := {15700+}u;//random(BLOCKS-1);



      if (blk = 429) then
        Debug.Log(self,'Trap!');

//      Debug.Log('Linear Verify');
//      Debug.Log('Read vda');
      vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);



//      Debug.Log('Compare Linear Reads.');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @ iteration'+inttostr(u)+' blk:'+inttostr(blk));
      end;
    end;
    Debug.Log(self,vda.DebugVatSTructure);
    //showmessage('success');
    result := 'success';
  finally
    vdnl.free;
    vda.free;
    fs.free;
    fs := nil;
  end;

  if fileexists(f1) then deletefile(f1);
  if fileexists(f2) then deletefile(f2);
  if fileexists(f3) then deletefile(f3);

end;
function TUT_VirtualDisk_Advanced.UT_CompareWithNonLinearAfterPayloadMove: string;
var
  vdnl: TVirtualDisk_SimpleNonLinear;
  vda: TVirtualDisk_Advanced;
  vda_array, vdnl_array, rarray: array[0..(BLOCKSIZE-1)] of byte;
  blk: cardinal;
  t,u: ni;
  sRandomFile: string;
  fs: TFileSTream;
  f1,f2,f3,f4: string;
const
  BLOCKS:int64 = 100000*4;
  ITERATIONS = 20000;
begin
  vdnl := TVirtualDisk_SimpleNonLinear.create;
  vda := TVirtualDisk_Advanced.create;
  try
    sRandomFile := GetTempPath+'random.random';
    if not fileexists(sRandomFile) then begin
      fs := TFileStream.create(sRAndomFile, fmCreate);
      try
        for t:= 0 to 65535 do begin
          blk := random(65535);
          fs.Write(blk, sizeof(blk));

        end;
      finally
        fs.free;
      end;

    end;
    f1 := GetTempPath+'vdnl.vd';
    f2 := GEtTempPath+'vda.vda';
    f3 := GEtTempPath+'vda.vdpayload';
    f4 := GEtTempPath+'vda2.vdpayload';


    if fileexists(f1) then deletefile(f1);
    if fileexists(f2) then deletefile(f2);
    if fileexists(f3) then deletefile(f3);
    if fileexists(f4) then deletefile(f4);
    vdnl.FileName := f1;
    vda.FileName := f2;
    vdnl.Size := BLOCKS*BLOCKSIZE;
    vda.Size := BLOCKS*BLOCKSIZE;

    //fill disk with randoms
    fs := TFileStream.create(sRandomFile, fmOpenRead+fmShareExclusive);
    fs.Seek(0,0);


    //ConsoleLog('Filling '+inttostr(u));
    for t:= 0 to (BLOCKSIZE-1) do begin
      rarray[t] := random(255);
    end;
//    vdnl.WriteBlocks(6555648, 1, @rarray[0]);
//    vda.WriteBlocks(6555648, 1, @rarray[0]);

    //this is the WRITE part
    for u := BLOCKS-1 downto 0 do begin
      //ConsoleLog('Filling '+inttostr(u));
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := random(255);
//        rarray[t] := t and $FF;
      end;
      vdnl.GuaranteeWriteBlocks(u, 1, @rarray[0]);
      vda.GuaranteeWriteBlocks(u, 1, @rarray[0]);
    end;

    //now lets add payload
    vda.AddPayload(f4, -1, 1, 0, 0);
    //and change the payload quota for the original file
    vda.SetPayloadQuota(0, 0);
    //wait a few seconds or data migration to start
    sleep(4000);
    //wait for data migration to end.
    while vda.Migrating do
      sleep(100);

//    vdnl.WriteBlocks(u, 1, @rarray[0]);
//    vda.WriteBlocks(u, 1, @rarray[0]);

    for u := 0 to ITERATIONS-1 do begin
      if u mod 1000 = 0 then
        Debug.Log(self,'Iteration '+inttostr(u));
      //ConsoleLog('Testing '+inttostr(u));

      //blk := random(BLOCKS-1);
      fs.REad(blk, sizeof(blk));

//      Debug.Log('Random write block = '+inttostr(blk));

      //write some random block
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := ((BLOCKSIZE-1)-t) and $FF;//random(255);
      end;

//      Debug.Log('RAndom write vdnl');
      vdnl.WriteBlocks(blk, 1, @rarray[0]);
//      Debug.Log('Random Write vda');
      vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);

//      Debug.Log('RAndom Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);
//      Debug.Log('Random Read vda');
      vda.GuaranteeREadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Compare vda against original');
      if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'vdnl:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        Debug.Log(self,'*****************RECONCILE******* Attempt write of predictable values.');


        //rewrite prediactable stuff
        for t:= 0 to (BLOCKSIZE-1) do begin
          rarray[t] := t and $FF;
        end;

        Debug.Log(self,'bring vda back to predictable');
        vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
        vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'orig:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
          Debug.Log(self,'Predicatable does not match.');
        end;

        raise ECritical.create('fail @'+inttostr(blk));

      end;

//      Debug.Log('Compare Random Writes (should still be cached).');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @'+inttostr(blk));
      end;

      //if (u mod 100) = 0 then
      //read some other random block
      blk := {15700+}u;//random(BLOCKS-1);

//      Debug.Log('Linear Verify');
//      Debug.Log('Read vda');
      vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);

//      if (blk = 122) then
//        Debug.Log('Trap!');


//      Debug.Log('Compare Linear Reads.');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @'+inttostr(blk));
      end;
    end;
    Debug.Log(self,vda.DebugVatSTructure);
    //showmessage('success');
    result := 'success';
  finally
    vdnl.free;
    vda.free;
    fs.free;
    fs := nil;
  end;

  if fileexists(f1) then deletefile(f1);
  if fileexists(f2) then deletefile(f2);
  if fileexists(f3) then deletefile(f3);
  if fileexists(f4) then deletefile(f4);

end;

function TUT_VirtualDisk_Advanced.UT_CompareWithNonLinearComplexArrange: string;
var
  vdnl: TVirtualDisk_SimpleNonLinear;
  vda: TVirtualDisk_Advanced;
  vda_array, vdnl_array, rarray: array[0..(BLOCKSIZE-1)] of byte;
  blk: cardinal;
  t,u: ni;
  sRandomFile: string;
  fs: TFileSTream;
  f1,f2: string;
  fPayloads: array of string;
const
  BLOCKS:int64 = 100000*4;
  ITERATIONS = 20000;
  TOTAL_PAYLOADS = 5;
  PRIORITY_0_PAYLOADS = 4;
begin
  vdnl := TVirtualDisk_SimpleNonLinear.create;
  vda := TVirtualDisk_Advanced.create;
  try
    sRandomFile := GetTempPath+'random.random';
    if not fileexists(sRandomFile) then begin
      fs := TFileStream.create(sRAndomFile, fmCreate);
      try
        for t:= 0 to 65535 do begin
          blk := random(65535);
          fs.Write(blk, sizeof(blk));

        end;
      finally
        fs.free;

      end;

    end;
    f1 := GetTempPath+'vdnl.vd';
    f2 := GEtTempPath+'vda.vda';
    setlength(Fpayloads, 5);
    FPayloads[0] := GEtTempPath+'vda.vdpayload';
    for t := 1 to TOTAL_PAYLOADS-1 do begin
      Fpayloads[t] := GEtTempPath+'vda'+t.tostring+'.vdpayload';
      if fileexists(FPayloads[t]) then
        deletefile(FPayloads[t]);
    end;


    if fileexists(f1) then deletefile(f1);
    if fileexists(f2) then deletefile(f2);

    vdnl.FileName := f1;
    vda.FileName := f2;
    vdnl.Size := BLOCKS*BLOCKSIZE;
    vda.Size := BLOCKS*BLOCKSIZE;

    //fill disk with randoms
    fs := TFileStream.create(sRandomFile, fmOpenRead+fmShareExclusive);
    fs.Seek(0,0);


    //ConsoleLog('Filling '+inttostr(u));
    for t:= 0 to (BLOCKSIZE-1) do begin
      rarray[t] := random(255);
    end;
//    vdnl.WriteBlocks(6555648, 1, @rarray[0]);
//    vda.WriteBlocks(6555648, 1, @rarray[0]);

    //this is the WRITE part
    for u := BLOCKS-1 downto 0 do begin
      //ConsoleLog('Filling '+inttostr(u));
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := random(255);
//        rarray[t] := t and $FF;
      end;
      vdnl.GuaranteeWriteBlocks(u, 1, @rarray[0]);
      vda.GuaranteeWriteBlocks(u, 1, @rarray[0]);
    end;

    //now lets add payload
    vda.AddPayload(f4, -1, 1, 0, 0);
    //and change the payload quota for the original file
    vda.SetPayloadQuota(0, 0);
    //wait a few seconds or data migration to start
    sleep(4000);
    //wait for data migration to end.
    while vda.Migrating do
      sleep(100);

//    vdnl.WriteBlocks(u, 1, @rarray[0]);
//    vda.WriteBlocks(u, 1, @rarray[0]);

    for u := 0 to ITERATIONS-1 do begin
      if u mod 1000 = 0 then
        Debug.Log(self,'Iteration '+inttostr(u));
      //ConsoleLog('Testing '+inttostr(u));

      //blk := random(BLOCKS-1);
      fs.REad(blk, sizeof(blk));

//      Debug.Log('Random write block = '+inttostr(blk));

      //write some random block
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := ((BLOCKSIZE-1)-t) and $FF;//random(255);
      end;

//      Debug.Log('RAndom write vdnl');
      vdnl.WriteBlocks(blk, 1, @rarray[0]);
//      Debug.Log('Random Write vda');
      vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);

//      Debug.Log('RAndom Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);
//      Debug.Log('Random Read vda');
      vda.GuaranteeREadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Compare vda against original');
      if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'vdnl:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        Debug.Log(self,'*****************RECONCILE******* Attempt write of predictable values.');


        //rewrite prediactable stuff
        for t:= 0 to (BLOCKSIZE-1) do begin
          rarray[t] := t and $FF;
        end;

        Debug.Log(self,'bring vda back to predictable');
        vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
        vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'orig:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
          Debug.Log(self,'Predicatable does not match.');
        end;

        raise ECritical.create('fail @'+inttostr(blk));

      end;

//      Debug.Log('Compare Random Writes (should still be cached).');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @'+inttostr(blk));
      end;

      //if (u mod 100) = 0 then
      //read some other random block
      blk := {15700+}u;//random(BLOCKS-1);

//      Debug.Log('Linear Verify');
//      Debug.Log('Read vda');
      vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);

//      if (blk = 122) then
//        Debug.Log('Trap!');


//      Debug.Log('Compare Linear Reads.');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @'+inttostr(blk));
      end;
    end;
    Debug.Log(self,vda.DebugVatSTructure);
    //showmessage('success');
    result := 'success';
  finally
    vdnl.free;
    vda.free;
    fs.free;
    fs := nil;
  end;

  if fileexists(f1) then deletefile(f1);
  if fileexists(f2) then deletefile(f2);
  if fileexists(f3) then deletefile(f3);
  if fileexists(f4) then deletefile(f4);

end;

function TUT_VirtualDisk_Advanced.UT_CompareWithNonLinearDuringPayloadMove: string;
var
  vdnl: TVirtualDisk_SimpleNonLinear;
  vda: TVirtualDisk_Advanced;
  vda_array, vdnl_array, rarray: array[0..(BLOCKSIZE-1)] of byte;
  blk: cardinal;
  t,u: ni;
  sRandomFile: string;
  fs: TFileSTream;
  f1,f2,f3,f4: string;
const
  BLOCKS:int64 = 100000*4;
  ITERATIONS = 20000;
begin
  vdnl := TVirtualDisk_SimpleNonLinear.create;
  vda := TVirtualDisk_Advanced.create;
  try
    sRandomFile := GetTempPath+'random.random';
    if not fileexists(sRandomFile) then begin
      fs := TFileStream.create(sRAndomFile, fmCreate);
      try
        for t:= 0 to 65535 do begin
          blk := random(65535);
          fs.Write(blk, sizeof(blk));

        end;
      finally
        fs.free;
      end;

    end;
    f1 := GetTempPath+'vdnl.vd';
    f2 := GEtTempPath+'vda.vda';
    f3 := GEtTempPath+'vda.vdpayload';
    f4 := GEtTempPath+'vda2.vdpayload';


    if fileexists(f1) then deletefile(f1);
    if fileexists(f2) then deletefile(f2);
    if fileexists(f3) then deletefile(f3);
    if fileexists(f4) then deletefile(f4);
    vdnl.FileName := f1;
    vda.FileName := f2;
    vdnl.Size := BLOCKS*BLOCKSIZE;
    vda.Size := BLOCKS*BLOCKSIZE;

    //fill disk with randoms
    fs := TFileStream.create(sRandomFile, fmOpenRead+fmShareExclusive);
    fs.Seek(0,0);


    //ConsoleLog('Filling '+inttostr(u));
    for t:= 0 to (BLOCKSIZE-1) do begin
      rarray[t] := random(255);
    end;
//    vdnl.WriteBlocks(6555648, 1, @rarray[0]);
//    vda.WriteBlocks(6555648, 1, @rarray[0]);

    //this is the WRITE part
    for u := BLOCKS-1 downto 0 do begin
      //ConsoleLog('Filling '+inttostr(u));
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := random(255);
//        rarray[t] := t and $FF;
      end;
      vdnl.GuaranteeWriteBlocks(u, 1, @rarray[0]);
      vda.GuaranteeWriteBlocks(u, 1, @rarray[0]);
    end;

    //now lets add payload
    vda.AddPayload(f4, -1,1,0,0);
    //and change the payload quota for the original file
    vda.SetPayloadQuota(0, 0);

//    vdnl.WriteBlocks(u, 1, @rarray[0]);
//    vda.WriteBlocks(u, 1, @rarray[0]);

    for u := 0 to ITERATIONS-1 do begin
      if u mod 1000 = 0 then
        Debug.Log(self,'Iteration '+inttostr(u));
      //ConsoleLog('Testing '+inttostr(u));

      //blk := random(BLOCKS-1);
      fs.REad(blk, sizeof(blk));

//      Debug.Log('Random write block = '+inttostr(blk));

      //write some random block
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := ((BLOCKSIZE-1)-t) and $FF;//random(255);
      end;

//      Debug.Log('RAndom write vdnl');
      vdnl.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
//      Debug.Log('Random Write vda');
      vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);

//      Debug.Log('RAndom Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);
//      Debug.Log('Random Read vda');
      vda.GuaranteeREadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Compare vda against original');
      if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'vdnl:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        Debug.Log(self,'*****************RECONCILE******* Attempt write of predictable values.');


        //rewrite prediactable stuff
        for t:= 0 to (BLOCKSIZE-1) do begin
          rarray[t] := t and $FF;
        end;

        Debug.Log(self,'bring vda back to predictable');
        vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
        vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'orig:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
          Debug.Log(self,'Predicatable does not match.');
        end;

        raise ECritical.create('fail @'+inttostr(blk));

      end;

//      Debug.Log('Compare Random Writes (should still be cached).');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @'+inttostr(blk));
      end;

      //if (u mod 100) = 0 then
      //read some other random block
      blk := {15700+}u;//random(BLOCKS-1);

//      Debug.Log('Linear Verify');
//      Debug.Log('Read vda');
      vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);

//      if (blk = 122) then
//        Debug.Log('Trap!');


//      Debug.Log('Compare Linear Reads.');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @'+inttostr(blk));
      end;
    end;
    Debug.Log(self,vda.DebugVatSTructure);
    //showmessage('success');
    result := 'success';
  finally
    vdnl.free;
    vda.free;
    fs.free;
    fs := nil;
  end;

  if fileexists(f1) then deletefile(f1);
  if fileexists(f2) then deletefile(f2);
  if fileexists(f3) then deletefile(f3);
  if fileexists(f4) then deletefile(f4);

end;

function TUT_VirtualDisk_Advanced.UT_CompareWithNonLinear_BrokenMirror(
  iMirrorIDX: ni): string;
var
  vdnl: TVirtualDisk_SimpleNonLinear;
  vda: TVirtualDisk_Advanced;
  vda_array, vdnl_array, rarray: array[0..BLOCKSIZE-1] of byte;
  blk: cardinal;
  t,u: ni;
  sRandomFile: string;
  fs: TFileSTream;
  f1,f2,f3: string;
const
  BLOCKS:int64 = 65536*4;
  ITERATIONS:int64 = 65536*4;
begin
  vdnl := TVirtualDisk_SimpleNonLinear.create;
  vda := TVirtualDisk_Advanced.create;
  try
    sRandomFile := GEtTempPath+'random.random';
    if not fileexists(sRandomFile) then begin
      fs := TFileStream.create(sRAndomFile, fmCreate);
      try
        for t:= 0 to 65535 do begin
          blk := random(BLOCKS-1);
          fs.Write(blk, sizeof(blk));

        end;
      finally
        fs.free;
      end;

    end;
    f1 := GetTempPath+'vdnl.vd';
    f2 := GEtTempPath+'vda.vda';
    f3 := GEtTempPath+'vda.vdpayload';

    if fileexists(f1) then deletefile(f1);
    if fileexists(f2) then deletefile(f2);
    if fileexists(f3) then deletefile(f3);
    vdnl.FileName := f1;
    vda.FileName := f2;
    vdnl.Size := BLOCKS*BLOCKSIZE*4;
    vda.Size := 2000+MEGA;//BLOCKS*BLOCKSIZE*4;
    vda.AddPayload(GetTempPath+'mirror.vda',400*GIGA, 1, 0,0);

    //fill disk with randoms
    fs := TFileStream.create(sRandomFile, fmOpenRead+fmShareExclusive);
    fs.Seek(0,0);


    //ConsoleLog('Filling '+inttostr(u));
    for t:= 0 to BLOCKSIZE-1 do begin
      rarray[t] := random(255);
    end;

    //-------------------------------------------------------------------------
    Debug.Log(self,'Initial write.');
    //vda.EnableOptionalDebugging := true;
    for u := 0 to BLOCKS-1  do begin
      for t:= 0 to BLOCKSIZE-1 do begin
        //rarray[t] := random(255);
        rarray[t] := u and $FF;
      end;
      if u = 131071 then
        Debug.Log(self,'!'+inttostr(u));

      vdnl.WriteBlocks(u, 1, @rarray[0]);
      if u = 389247 then begin
        Debug.Log(self,'Initial write.'+inttostr(u));
        debug.Log(self,vda.DebugVatStructure);
      end;
      if u = 262143 then
        Debug.log(self,'Trap');
      vda.GuaranteeWriteBlocks(u, 1, @rarray[0]);
    end;


    vda.Free;
    vda := nil;

    if iMirrorIDX = 0 then
      scramblefile(GetTempPath+'vda.vdpayload');
    if iMirrorIDX = 1 then
      scramblefile(GetTempPath+'mirror.vdpayload');

    vda := TVirtualDisk_Advanced.create;
    vda.FileName := f2;


    //COMPARE-------------------------------------------------------------

    for u := 0 to ITERATIONS-1 do begin
      if u mod 1000 = 0 then
        Debug.Log(self,'Iteration '+inttostr(u));

//      fs.REad(blk, sizeof(blk));
      blk := u;

      //write some random block
    (*  for t:= 0 to BLOCKSIZE-1 do begin
        rarray[t] := ((BLOCKSIZE-1)-t) and $FF;//random(255);
      end;


      vdnl.WriteBlocks(blk, 1, @rarray[0]);
      vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);*)

     vdnl.ReadBlocks(blk, 1, @vdnl_array[0]);
      vda.GuaranteeREadBlocks(blk, 1, @vda_array[0]);
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,'***** MISMATch! ***************');
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'vdnl:'+MemoryDebugString(@vdnl_array[0], BLOCKSIZE));



(*
        //rewrite prediactable stuff
        for t:= 0 to (BLOCKSIZE-1) do begin
          rarray[t] := t and $FF;
        end;

        Debug.Log(self,'bring vda back to predictable');
        vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
        vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'orig:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
          Debug.Log(self,'Predicatable does not match.');
        end;

        raise ECritical.create('fail @ iteration'+inttostr(u)+' blk:'+inttostr(blk));*)

      end;

(*      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @ iteration'+inttostr(u)+' blk:'+inttostr(blk));
      end;

      blk := u;

      vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);



//      Debug.Log('Compare Linear Reads.');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log('vda='+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log('vdnl='+MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @ iteration'+inttostr(u)+' blk:'+inttostr(blk));
      end;*)
    end;
    Debug.Log(self,vda.DebugVatSTructure);
    //showmessage('success');
    result := 'success';
  finally
    vdnl.free;
    vda.free;
    fs.free;
    fs := nil;
  end;

  if fileexists(f1) then deletefile(f1);
  if fileexists(f2) then deletefile(f2);
  if fileexists(f3) then deletefile(f3);

end;

function TUT_VirtualDisk_Advanced.UT_CompareWithNonLinear_forwardbuild: string;
var
  vdnl: TVirtualDisk_SimpleNonLinear;
  vda: TVirtualDisk_Advanced;
  vda_array, vdnl_array, rarray: array[0..BLOCKSIZE-1] of byte;
  blk: cardinal;
  t,u: ni;
  sRandomFile: string;
  fs: TFileSTream;
  f1,f2,f3: string;
  iDif, iDifSize: ni;
const
  BLOCKS:int64 = 65535*4;
  ITERATIONS = 20000;
begin
  vdnl := TVirtualDisk_SimpleNonLinear.create;
  vda := TVirtualDisk_Advanced.create;
  try
    sRandomFile := GEtTempPath+'random.random';
    if not fileexists(sRandomFile) then begin
      fs := TFileStream.create(sRAndomFile, fmCreate);
      try
        for t:= 0 to 65535 do begin
          blk := random(65535);
          fs.Write(blk, sizeof(blk));

        end;
      finally
        fs.free;
      end;

    end;
    f1 := GetTempPath+'vdnl.vd';
    f2 := GEtTempPath+'vda.vda';
    f3 := GEtTempPath+'vda.vdpayload';

    if fileexists(f1) then deletefile(f1);
    if fileexists(f2) then deletefile(f2);
    if fileexists(f3) then deletefile(f3);
    vdnl.FileName := f1;
    vda.FileName := f2;
    vdnl.Size := BLOCKS*BLOCKSIZE*4;
    vda.Size := 2000+MEGA;//BLOCKS*BLOCKSIZE*4;

    //fill disk with randoms
    fs := TFileStream.create(sRandomFile, fmOpenRead+fmShareExclusive);
    fs.Seek(0,0);


    //ConsoleLog('Filling '+inttostr(u));
    for t:= 0 to BLOCKSIZE-1 do begin
      rarray[t] := random(255);
    end;

    Debug.Log(self,'Initial write.');
    //vda.EnableOptionalDebugging := true;
    for u := 0 to BLOCKS-1 do begin
      //ConsoleLog('Filling '+inttostr(u));
      for t:= 0 to BLOCKSIZE-1 do begin
        //rarray[t] := random(255);
        rarray[t] := u and $FF;
      end;
      if u = 1 then
        Debug.Log(self,'!'+inttostr(u));
      if u = 131071 then
        Debug.Log(self,'!'+inttostr(u));
//      Debug.Log('a'+inttostr(u));

      vdnl.WriteBlocks(u, 1, @rarray[0]);
      if u = 389247 then begin
        Debug.Log(self,'Initial write.'+inttostr(u));
        debug.Log(self,vda.DebugVatStructure);
      end;
      if u = 262143 then
        Debug.log(self,'Trap');
      vda.GuaranteeWriteBlocks(u, 1, @rarray[0]);
//      Debug.Log('b'+inttostr(u));
    end;


//    vdnl.WriteBlocks(u, 1, @rarray[0]);
//    vda.WriteBlocks(u, 1, @rarray[0]);


    for u := 0 to ITERATIONS-1 do begin
      if u = 1758 then
        Debug.Log(self,'trap');
      if u mod 1000 = 0 then
        Debug.Log(self,'Iteration '+inttostr(u));
      //ConsoleLog('Testing '+inttostr(u));

//      if (u = 610) then
//        vda.EnableOptionalDebugging := true
//      else
//        vda.EnableOptionalDebugging := true;


      blk := random(BLOCKS-1);
      fs.REad(blk, sizeof(blk));


//      Debug.Log('Random write block = '+inttostr(blk));

      //write some random block

      if (blk = 1758) then
        Debug.Log(self,'trap');

      for t:= 0 to BLOCKSIZE-1 do begin
        rarray[t] := ((BLOCKSIZE-1)-t) and $FF;//random(255);
      end;


//      Debug.Log('RAndom write vdnl');
      vdnl.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
//      Debug.Log('Random Write vda');
      vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);

//      Debug.Log('RAndom Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);
//      Debug.Log('Random Read vda');
      vda.GuaranteeREadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Compare vda against original');
      if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'vdnl:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        Debug.Log(self,'*****************RECONCILE******* Attempt write of predictable values.');


        //rewrite prediactable stuff
        for t:= 0 to (BLOCKSIZE-1) do begin
          rarray[t] := t and $FF;
        end;

        Debug.Log(self,'bring vda back to predictable');
        vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
        vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'orig:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
          Debug.Log(self,'Predicatable does not match.');
        end;

        raise ECritical.create('fail @ iteration'+inttostr(u)+' blk:'+inttostr(blk));

      end;

//      Debug.Log('Compare Random Writes (should still be cached).');
      if (not CompareMemEx(@vda_array[0], @vdnl_array[0], sizeof(vda_array), iDif, iDifSize)) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        Debug.Log(self,'Diff @'+inttostr(iDif)+' = ');
        Debug.Log(self,MemoryDebugString(@vda_array[iDif], iDifSize));
        Debug.Log(self,MemoryDebugString(@vdnl_array[iDif], iDifSize));
        raise ECritical.create('fail @ iteration'+inttostr(u)+' blk:'+inttostr(blk));
      end;

      //if (u mod 100) = 0 then
      //read some other random block
      blk := {15700+}u;//random(BLOCKS-1);


      if (u = 1758) then
        Debug.Log(self,'trap');

//      Debug.Log('Linear Verify');
//      Debug.Log('Read vda');
      vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);

//      Debug.Log('Compare Linear Reads.');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.log(self,'Fail @ iteration '+inttostr(u)+' read of block '+inttostr(blk));
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @ iteration'+inttostr(u)+' blk:'+inttostr(blk));
      end;
    end;
    Debug.Log(self,vda.DebugVatSTructure);
    //showmessage('success');
    result := 'success';
  finally
    vdnl.free;
    vda.free;
    fs.free;
    fs := nil;
  end;

  if fileexists(f1) then deletefile(f1);
  if fileexists(f2) then deletefile(f2);
  if fileexists(f3) then deletefile(f3);

end;

function TUT_VirtualDisk_Advanced.UT_CreateAndDestroy: string;
var
  t: ni;
  vda: TVirtualDisk_Advanced;
begin
  for t:= 0 to 4 do begin
    CleanupOldStuff;
    vda := TVirtualDisk_Advanced.create;
    try
      vda.FileName := GetTempPath+'vda.vda';
    finally
      vda.free;
      vda := nil;
    end;
  end;
  result := 'success';
end;

function TUT_VirtualDisk_Advanced.UT_DirtSimple(iStopMarker: ni; iPayloadCount: ni = 1): string;
var
  f1: string;
  vda: TVirtualDisk_Advanced;
  blk, blk2,blk3: array[0..BLOCKSIZE-1] of byte;
  iMarker: ni;
  t: ni;
begin
  iMarker := 0;
  cleanupoldstuff;
  result := 'success';

  f1 := GetTempPath+'vda.vda';
  vda := TVirtualDisk_ADvanced.Create;

  try
    vda.filename := f1;
    for t:= 2 to iPayloadCount do begin
      vda.AddPayload(gettemppath+'vda'+inttostr(t)+'.vdpayload',-1,t-1,0,0);
    end;


    fillmemrandom(@blk[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(0, 1, @blk[0]);
    vda.GuaranteeReadBlocks(0, 1, @blk2[0]);
    if not CompareMem(@blk[0], @blk2[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;

    //end 0
    //----------------------------------
    if iStopMarker = iMarker then exit;
    inc(iMarker);
    //----------------------------------

    fillmemrandom(@blk[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(1, 1, @blk[0]);
    vda.GuaranteeReadBlocks(1, 1, @blk2[0]);
    if not CompareMem(@blk[0], @blk2[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;

    //end 1
    //----------------------------------
    if iStopMarker = iMarker then exit;
    inc(iMarker);
    //----------------------------------

    fillmemrandom(@blk[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(4, 1, @blk[0]);
    vda.GuaranteeReadBlocks(4, 1, @blk2[0]);
    if not CompareMem(@blk[0], @blk2[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;

    //end 2
    //----------------------------------
    if iStopMarker = iMarker then exit;
    inc(iMarker);
    //----------------------------------


    //write set 1 @ 3
    fillmemrandom(@blk[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(3, 1, @blk[0]);
    //write set 2 @ 4
    fillmemrandom(@blk2[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(4, 1, @blk2[0]);
    //read @ 3 into set 3
    vda.GuaranteeReadBlocks(3, 1, @blk3[0]);
    //compare set 1 and 3 (should be the same)
    if not CompareMem(@blk[0], @blk3[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;
    //read @ 4 into set 3
    vda.GuaranteeReadBlocks(4, 1, @blk3[0]);
    //compare set 2 and 3 (shouuld be the same)
    if not CompareMem(@blk2[0], @blk3[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;



  finally
    vda.free;
  end;







end;

function TUT_VirtualDisk_Advanced.UT_TwoBigBlock(iStopMarker,
  iPayloadCount: ni): string;
var
  f1: string;
  vda: TVirtualDisk_Advanced;
  blk, blk2,blk3: array[0..(BLOCKSIZE-1)] of byte;
  iMarker: ni;
  t: ni;
begin
  iMarker := 0;
  cleanupoldstuff;
  result := 'success';

  f1 := GetTempPath+'vda.vda';
  vda := TVirtualDisk_advanced.Create;

  try
    vda.filename := f1;
    for t:= 2 to iPayloadCount do begin
      vda.AddPayload(gettemppath+'vda'+inttostr(t)+'.vdpayload',-1,t-1,0,0);
    end;


    fillmemrandom(@blk[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS-1, 1, @blk[0]);
    vda.GuaranteeReadBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS-1, 1, @blk2[0]);
    if not CompareMem(@blk[0], @blk2[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;

    //end 0
    //----------------------------------
    if iStopMarker = iMarker then exit;
    inc(iMarker);
    //----------------------------------

    fillmemrandom(@blk[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS, 1, @blk[0]);
    vda.GuaranteeReadBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS, 1, @blk2[0]);
    if not CompareMem(@blk[0], @blk2[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;

    //end 1
    //----------------------------------
    if iStopMarker = iMarker then exit;
    inc(iMarker);
    //----------------------------------


    //write set 1 @ 3
    fillmemrandom(@blk[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS-1, 1, @blk[0]);
    //write set 2 @ 4
    fillmemrandom(@blk2[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS, 1, @blk2[0]);
    //read @ 3 into set 3
    vda.GuaranteeReadBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS-1, 1, @blk3[0]);
    //compare set 1 and 3 (should be the same)
    if not CompareMem(@blk[0], @blk3[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;
    //read @ 4 into set 3
    vda.GuaranteeReadBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS, 1, @blk3[0]);
    //compare set 2 and 3 (shouuld be the same)
    if not CompareMem(@blk2[0], @blk3[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;


    //end 2
    //----------------------------------
    if iStopMarker = iMarker then exit;
    inc(iMarker);
    //----------------------------------


    //rewrite set 1 @ 3
    fillmemrandom(@blk[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS-1, 1, @blk[0]);
    //write set 2 @ 4
    fillmemrandom(@blk2[0], sizeof(blk));
    vda.GuaranteeWriteBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS, 1, @blk2[0]);
    //read @ 3 into set 3
    vda.GuaranteeReadBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS-1, 1, @blk3[0]);
    //compare set 1 and 3 (should be the same)
    if not CompareMem(@blk[0], @blk3[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;
    //read @ 4 into set 3
    vda.GuaranteeReadBlocks(_BIG_BLOCK_SIZE_IN_BLOCKS, 1, @blk3[0]);
    //compare set 2 and 3 (shouuld be the same)
    if not CompareMem(@blk2[0], @blk3[0], sizeof(blk)) then begin
      result := 'fail @ marker '+inttostr(iMarker);
      exit;
    end;




  finally
    vda.free;
  end;







end;

function TUT_VirtualDisk_Advanced.UT_Variable(BLOCKS, ITERATIONS: ni;
  MoveDuringPayload: boolean): string;
var
  vdnl: TVirtualDisk_SimpleNonLinear;
  vda: TVirtualDisk_Advanced;
  vda_array, vdnl_array, rarray: array[0..(BLOCKSIZE-1)] of byte;
  blk: cardinal;
  t,u: ni;
  f1,f2,f3,f4: string;
begin
  vdnl := TVirtualDisk_SimpleNonLinear.create;
  vda := TVirtualDisk_Advanced.create;
  try
    f1 := GetTempPath+'vdnl.vd';
    f2 := GEtTempPath+'vda.vda';
    f3 := GEtTempPath+'vda.vdpayload';
    f4 := GEtTempPath+'vda2.vdpayload';


    if fileexists(f1) then deletefile(f1);
    if fileexists(f2) then deletefile(f2);
    if fileexists(f3) then deletefile(f3);
    if fileexists(f4) then deletefile(f4);
    vdnl.FileName := f1;
    vda.FileName := f2;
    vdnl.Size := BLOCKS*BLOCKSIZE;
    vda.Size := BLOCKS*BLOCKSIZE;


    //ConsoleLog('Filling '+inttostr(u));
    for t:= 0 to (BLOCKSIZE-1) do begin
      rarray[t] := random(255);
    end;
//    vdnl.WriteBlocks(6555648, 1, @rarray[0]);
//    vda.WriteBlocks(6555648, 1, @rarray[0]);

    //this is the WRITE part
    for u := BLOCKS-1 downto 0 do begin
      //ConsoleLog('Filling '+inttostr(u));
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := random(255);
//        rarray[t] := t and $FF;
      end;
      vdnl.GuaranteeWriteBlocks(u, 1, @rarray[0]);
      vda.GuaranteeWriteBlocks(u, 1, @rarray[0]);
    end;

    //now lets add payload
    vda.AddPayload(f4, -1,1,0,0);
    //and change the payload quota for the original file
    //vda.SetPayloadQuota(0, 0);

//    vdnl.WriteBlocks(u, 1, @rarray[0]);
//    vda.WriteBlocks(u, 1, @rarray[0]);

//    vda.EnableOptionalDebugging := true;
    for u := 0 to Iterations-1 do begin
      IF t = 0 THEN
        DEBUG.LOG(self,'trap');
      if u mod 1000 = 0 then begin
        Debug.Log(self,'Iteration '+inttostr(u));
      //ConsoleLog('Testing '+inttostr(u));
        if MoveDuringPayload then begin
          blk := Random(1000000000);
          Debug.log(self,'setting quota to '+inttostr(blk));
          vda.SetPayloadQuota(0, blk);
        end;
      end;


      blk := GEtPredictableRandom(BLOCKS-1);

//      Debug.Log('Random write block = '+inttostr(blk));

      //write some random block
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := ((BLOCKSIZE-1)-t) and $FF;//random(255);
      end;

//      Debug.Log('RAndom write vdnl');
      vdnl.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
//      Debug.Log('Random Write vda');
      vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);

//      Debug.Log('RAndom Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);
//      Debug.Log('Random Read vda');
      vda.GuaranteeREadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Compare vda against original');
      if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
        Debug.Log(self,'vda did not match rarray');
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'vdnl:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        Debug.Log(self,'*****************RECONCILE******* Attempt write of predictable values.');


        //rewrite prediactable stuff
        for t:= 0 to (BLOCKSIZE-1) do begin
          rarray[t] := t and $FF;
        end;

        Debug.Log(self,'bring vda back to predictable');
        vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
        vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'orig:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
          Debug.Log(self,'Predicatable does not match.');
        end;

        raise ECritical.create('fail in iteration '+inttostr(u)+' @ block '+inttostr(blk));

      end;

//      Debug.Log('Compare Random Writes (should still be cached).');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @'+inttostr(blk));
      end;

      //if (u mod 100) = 0 then
      //read some other random block
      blk := GEtPredictableRandom(BLOCKS-1);

//      Debug.Log('Linear Verify');
//      Debug.Log('Read vda');
      vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);

//      if (blk = 122) then
//        Debug.Log('Trap!');


//      Debug.Log('Compare Linear Reads.');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail in iteration '+inttostr(u)+' @ block '+inttostr(blk));
//        raise ECritical.create('fail @'+inttostr(blk));
      end;
    end;
    Debug.Log(self,vda.DebugVatSTructure);
    //showmessage('success');
    result := 'success';
  finally
    vdnl.free;
    vda.free;
  end;

  if fileexists(f1) then deletefile(f1);
  if fileexists(f2) then deletefile(f2);
  if fileexists(f3) then deletefile(f3);
  if fileexists(f4) then deletefile(f4);

end;


function TUT_VirtualDisk_Advanced.UT_VariableRaid(BLOCKS, ITERATIONS,
  payCount: ni; MoveDuringPayload: boolean): string;
var
  vdnl: TVirtualDisk_SimpleNonLinear;
  vda: TVirtualDisk_Advanced;
  vda_array, vdnl_array, rarray: array[0..(BLOCKSIZE-1)] of byte;
  blk: cardinal;
  t,u,f: ni;
  f1,f2,f3: string;
  fn: array of string;

begin
  cleanupOldStuff;
  vdnl := TVirtualDisk_SimpleNonLinear.create;
  vda := TVirtualDisk_Advanced.create;
  try
    f1 := GetTempPath+'vdnl.vd';
    f2 := GEtTempPath+'vda.vda';
    f3 := GEtTempPath+'vda.vdpayload';
    setlength(fn, paycount-1);

    for f := 0 to length(fn)-1 do begin
      fn[f] := GEtTempPath+'vda'+inttostr(f)+'.vdpayload';
      if fileexists(fn[f]) then deletefile(fn[f]);
    end;

    if fileexists(f1) then deletefile(f1);
    if fileexists(f2) then deletefile(f2);
    if fileexists(f3) then deletefile(f3);

    vdnl.FileName := f1;
    vda.FileName := f2;
    vdnl.Size := BLOCKS*BLOCKSIZE;
    vda.Size := BLOCKS*BLOCKSIZE;

    debug.log(self,'adding payloads');
    //now lets add payload
    for f:= 0 to length(fn)-1 do
      vda.AddPayload(fn[f], -1,f+1,0,0);



     //ConsoleLog('Filling '+inttostr(u));
    for t:= 0 to (BLOCKSIZE-1) do begin
      rarray[t] := random(255);
    end;
//    vdnl.WriteBlocks(6555648, 1, @rarray[0]);
//    vda.WriteBlocks(6555648, 1, @rarray[0]);

    debug.log(self,'writing...');
    //this is the WRITE part
    for u := BLOCKS-1 downto 0 do begin
      //ConsoleLog('Filling '+inttostr(u));
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := random(255);
//        rarray[t] := t and $FF;
      end;
      if u = 19189 then begin
        debug.log(self,'trap');
      end;
      vdnl.GuaranteeWriteBlocks(u, 1, @rarray[0]);
      vda.GuaranteeWriteBlocks(u, 1, @rarray[0]);
    end;
  finally
    debug.log(self,'closing, will reopen...');
    vda.free;
    vda := nil;
  end;
  debug.log(self,'creating...');
  vda := TVirtualDisk_Advanced.create;
  try
    debug.log(self,'opening...');
    vda.FileName := f2;


    //and change the payload quota for the original file
    //vda.SetPayloadQuota(0, 0);

//    vdnl.WriteBlocks(u, 1, @rarray[0]);
//    vda.WriteBlocks(u, 1, @rarray[0]);

//    vda.EnableOptionalDebugging := true;
    for u := 0 to Iterations-1 do begin
      IF t = 0 THEN
        DEBUG.LOG(self,'trap');
      if u mod 1000 = 0 then begin
        Debug.Log(self,'Iteration '+inttostr(u));
      //ConsoleLog('Testing '+inttostr(u));
        if MoveDuringPayload then begin
          blk := Random(1000000000);
          Debug.log(self,'setting quota to '+inttostr(blk));
          vda.SetPayloadQuota(0, blk);
        end;
      end;


      blk := GEtPredictableRandom(BLOCKS-1);

//      Debug.Log('Random write block = '+inttostr(blk));

      //write some random block
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := ((BLOCKSIZE-1)-t) and $FF;//random(255);
      end;

//      Debug.Log('RAndom write vdnl');
      vdnl.WriteBlocks(blk, 1, @rarray[0]);
//      Debug.Log('Random Write vda');
      vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);

//      Debug.Log('RAndom Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);
//      Debug.Log('Random Read vda');
      vda.GuaranteeREadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Compare vda against original');
      if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
        Debug.Log(self,'vda did not match rarray');
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'vdnl:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        Debug.Log(self,'*****************RECONCILE******* Attempt write of predictable values.');


        //rewrite prediactable stuff
        for t:= 0 to (BLOCKSIZE-1) do begin
          rarray[t] := t and $FF;
        end;

        Debug.Log(self,'bring vda back to predictable');
        vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
        vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'orig:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
          Debug.Log(self,'Predicatable does not match.');
        end;

        raise ECritical.create('fail in iteration '+inttostr(u)+' @ block '+inttostr(blk));

      end;

//      Debug.Log('Compare Random Writes (should still be cached).');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @'+inttostr(blk));
      end;

      //if (u mod 100) = 0 then
      //read some other random block
      blk := GEtPredictableRandom(BLOCKS-1);

//      Debug.Log('Linear Verify');
//      Debug.Log('Read vda');
      vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);

//      if (blk = 122) then
//        Debug.Log('Trap!');


//      Debug.Log('Compare Linear Reads.');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail in iteration '+inttostr(u)+' @ block '+inttostr(blk));
//        raise ECritical.create('fail @'+inttostr(blk));
      end;
    end;
    Debug.Log(self,vda.DebugVatSTructure);
    //showmessage('success');
    result := 'success';
  finally
    vdnl.free;
    vda.free;
  end;

  if fileexists(f1) then deletefile(f1);
  if fileexists(f2) then deletefile(f2);
  if fileexists(f3) then deletefile(f3);
  for f := 0 to length(fn)-1 do begin
    if fileexists(fn[f]) then deletefile(fn[f]);
  end;
end;


function TUT_VirtualDisk_Advanced.UT_VariableRaid_Refunct(BLOCKS, ITERATIONS,
  payCount: ni; MoveDuringPayload: boolean): string;
var
  vdnl: TVirtualDisk_SimpleNonLinear;
  vda: TVirtualDisk_Advanced;
  vda_array, vdnl_array, rarray: array[0..(BLOCKSIZE-1)] of byte;
  blk: cardinal;
  t,u,f: ni;
  f1,f2,f3: string;
  fn: array of string;
  iDefunct: ni;

begin
  CleanupOldStuff;
  vdnl := TVirtualDisk_SimpleNonLinear.create;
  vda := TVirtualDisk_Advanced.create;
  try
    f1 := GetTempPath+'vdnl.vd';
    f2 := GEtTempPath+'vda.vda';
    f3 := GEtTempPath+'vda.vdpayload';
    setlength(fn, paycount-1);

    for f := 0 to length(fn)-1 do begin
      fn[f] := GEtTempPath+'vda'+inttostr(f)+'.vdpayload';
      if fileexists(fn[f]) then deletefile(fn[f]);
    end;

    if fileexists(f1) then deletefile(f1);
    if fileexists(f2) then deletefile(f2);
    if fileexists(f3) then deletefile(f3);

    vdnl.FileName := f1;
    vda.FileName := f2;
    vdnl.Size := BLOCKS*BLOCKSIZE;
    vda.Size := BLOCKS*BLOCKSIZE;

    //now lets add payload
    for f:= 0 to length(fn)-1 do
      vda.AddPayload(fn[f], -1,f+1,0,0);


    //ConsoleLog('Filling '+inttostr(u));
    for t:= 0 to (BLOCKSIZE-1) do begin
      rarray[t] := random(255);
    end;
//    vdnl.WriteBlocks(6555648, 1, @rarray[0]);
//    vda.WriteBlocks(6555648, 1, @rarray[0]);

    //this is the WRITE part
    for u := BLOCKS-1 downto 0 do begin
      //ConsoleLog('Filling '+inttostr(u));
      for t:= 0 to (BLOCKSIZE-1) do begin
        //rarray[t] := random(255);
        rarray[t] := t and $FF;
      end;
      vdnl.GuaranteeWriteBlocks(u, 1, @rarray[0]);
      vda.GuaranteeWriteBlocks(u, 1, @rarray[0]);
    end;
    vda.GuaranteeReadBlocks(0, 1, @rarray[0]);

  finally
    vda.free;
    vda := nil;
  end;

  iDefunct := (payCount-1) div 2;
  Debug.Log(self,'Delete payload '+inttostr(iDefunct));
  deletefile(fn[iDefunct]);

  vda := TVirtualDisk_Advanced.create;
  try
    vda.FileName := f2;
    vda.ReFunctPayload(iDefunct+1, fn[iDefunct]);
    while not vda.Operational do begin
      Debug.Log('Wait for operational');
      sleep(4000);
    end;
    while vda.migrating do begin
      Debug.Log('Wait for migration');
      sleep(4000);
    end;
    while not vda.online do begin
      Debug.Log('Wait for online');
      sleep(4000);
    end;

    Debug.Log('-------------------------------');
    Debug.Log('-------------------------------');
    Debug.Log('-------------------------------');
    Debug.Log(vda.vat.DebugVatSTructure);




    //and change the payload quota for the original file
    //vda.SetPayloadQuota(0, 0);

//    vdnl.WriteBlocks(u, 1, @rarray[0]);
//    vda.WriteBlocks(u, 1, @rarray[0]);

//    vda.EnableOptionalDebugging := true;
    for u := 0 to Iterations-1 do begin
//      IF u = 19 THEN
//        DEBUG.LOG(self,'trap');
      if u mod 1000 = 0 then begin
        Debug.Log(self,'Iteration '+inttostr(u));
      //ConsoleLog('Testing '+inttostr(u));
        if MoveDuringPayload then begin
          blk := Random(1000000000);
          Debug.log(self,'setting quota to '+inttostr(blk));
          vda.SetPayloadQuota(0, blk);
        end;
      end;


      blk := GEtPredictableRandom(BLOCKS-1);

//      Debug.Log('Random write block = '+inttostr(blk));

      //write some random block
      for t:= 0 to (BLOCKSIZE-1) do begin
        rarray[t] := ((BLOCKSIZE-1)-t) and $FF;//random(255);
      end;

//      Debug.Log('RAndom write vdnl');
      vdnl.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
//      Debug.Log('Random Write vda');
      vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);

//      Debug.Log('RAndom Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);
//      Debug.Log('Random Read vda');
      vda.GuaranteeREadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Compare vda against original');
      if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
        Debug.Log(self,'vda did not match rarray');
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'vdnl:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        Debug.Log(self,'*****************RECONCILE******* Attempt write of predictable values.');


        //rewrite prediactable stuff
        for t:= 0 to (BLOCKSIZE-1) do begin
          rarray[t] := t and $FF;
        end;

        Debug.Log(self,'bring vda back to predictable');
        vda.GuaranteeWriteBlocks(blk, 1, @rarray[0]);
        vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
        Debug.Log(self,'vda: '+MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,'orig:'+MemoryDebugString(@rarray[0], BLOCKSIZE));
        if (not CompareMem(@vda_array[0], @rarray[0], sizeof(vda_array))) then begin
          Debug.Log(self,'Predicatable does not match.');
        end;

        raise ECritical.create('fail in iteration '+inttostr(u)+' @ block '+inttostr(blk));

      end;

//      Debug.Log('Compare Random Writes (should still be cached).');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail @'+inttostr(blk));
      end;

      //if (u mod 100) = 0 then
      //read some other random block
      blk := GEtPredictableRandom(BLOCKS-1);

//      Debug.Log('Linear Verify');
//      Debug.Log('Read vda');

      IF u = 19 THEN begin
        DEBUG.LOG(self,'trap');
        g_traphit := true;
      end;

      vda.GuaranteeReadBlocks(blk, 1, @vda_array[0]);
//      Debug.Log('Read vdnl');
      vdnl.GuaranteeReadBlocks(blk, 1, @vdnl_array[0]);

//      if (blk = 122) then
//        Debug.Log('Trap!');



//      Debug.Log('Compare Linear Reads.');
      if (not CompareMem(@vda_array[0], @vdnl_array[0], sizeof(vda_array))) then begin
        Debug.Log(self,vda.DebugVatSTructure);
        Debug.Log(self,MemoryDebugString(@vda_array[0], BLOCKSIZE));
        Debug.Log(self,MemoryDebugString(@vdnl_array[0], BLOCKSIZE));
        raise ECritical.create('fail in iteration '+inttostr(u)+' @ block '+inttostr(blk));
//        raise ECritical.create('fail @'+inttostr(blk));
      end;
    end;
    Debug.Log(self,vda.DebugVatSTructure);
    //showmessage('success');
    result := 'success';
  finally
    vdnl.free;
    vda.free;
  end;

  if fileexists(f1) then deletefile(f1);
  if fileexists(f2) then deletefile(f2);
  if fileexists(f3) then deletefile(f3);
  for f := 0 to length(fn)-1 do begin
    if fileexists(fn[f]) then deletefile(fn[f]);
  end;
end;

initialization

  UTF.RegisterClass(TUT_VirtualDisk_Advanced);


end.
