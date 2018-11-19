unit raid;
{$DEFINE RELEASE}
{x$DEFINE USE_LINKED_RAID}
{$DEFINE SHIFTOPT}
interface

uses linked_list_btree_compatible, sysutils, btree, typex, debug, orderlyinit, virtualdiskconstants, betterobject, systemx, tickcount, linked_list, generics.collections.fixed, better_collections, stringx;

//get stripe
//put stripe



type
  PUint32 = ^integer;
  TRaidFetchStatus = (rsCritical, rsRepaired, rsSuccess, rsRecoveredFromArchive);
  TRaidPieceInfo = record
    piece_idx: ni;
    big_block_index: int64;
    PayloadAddr: int64;
    VirtualAddr: int64;
    BigBlockFileBaseAddr: int64;
    FileOrigin: string;
    function DebugString: string;
    procedure Init;
    function PieceIndex: int64;
  end;

  TRaidPiece = packed record
    _piece_checksum: int64;
    _stripe_checksum: int64;
    payload: array[0..MAX_STRIPE_SIZE_IN_QWORDS-1] of int64;
    procedure Reset;inline;
    procedure Put(idx: ni; val: int64);inline;
    function IsValidPiece(size: ni; idx_ForDebugging: ni): boolean;inline;
    function payload32: PUInt32;inline;

  private
    function CalcPieceChecksum(sizeInQwords: ni): int64;
  public
    sizeinQWords: ni;
    class function HeaderSize: int64;static;inline;
    function DebugString: string;
  end;

  PRaidPiece = ^TRaidPiece;

  TRaidTreeItem_ByBlock = class;//forward
  TRaidTreeItem_ByDirtyTime = class;//forward
  TRaidTreeItem_ByLastUsedTime = class;//forward


  TRaidAssembledBuffer = class(TBetterObject)
  strict private
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    Flastused: ticker;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    Fdirtytime: ticker;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    Fstartingblock: int64;

  private
    procedure SetDirtyTime(const Value: int64);
    procedure SEtLastUsed(const Value: int64);
    procedure SetStartingBlock(const Value: int64);
    procedure SetAnyDirty(const Value: boolean);
  public

    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    single_size: ni;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    single: array[0..MAX_STRIPE_SIZE_IN_QWORDS-1] of int64;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    _dirty: array[0..RAID_STRIPE_SIZE_IN_BLOCKS-1] of boolean;
  strict private
    FAnyDirty: boolean;
  public

    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    archive: boolean;
    readfromdisk: boolean;
    treeItemBlock: TRaidTreeItem_ByBlock;
    treeItemDirtyTime: TRaidTreeItem_ByDirtyTime;
    treeItemLastUsed: TRaidTreeItem_ByLastUsedTime;
    legit: boolean; //was this buffer legitimately used, or just prefetched?
    procedure CopyFrom(rab: TRaidAssembledBuffer);
    procedure init;override;
    function GetSizeInBytes: ni;inline;

    procedure SEtSizeInBytes(const Value: ni);inline;
    property PayloadSizeInBytes: ni read GetSizeInBytes write SEtSizeInBytes;
    procedure SetDirty(idx: ni; const Value: boolean);inline;
    function GetDirty(idx: ni): boolean;inline;
    function SlowAnyDirty: boolean;inline;
    property dirty[idx: ni]: boolean read GetDirty write SetDirty;
    property AnyDirty: boolean read FAnyDirty write SetAnyDirty;
    function AllDirty: boolean;inline;
    function FirstDirty(iAfter: ni): ni;inline;
    function FirstClean(iAfter: ni): ni;inline;
    procedure cleardirty;inline;
    function byteptr(addr: int64): pbyte;inline;
    function CoversBlock(lba: int64): boolean;inline;
    property StartingBlock: int64 read FStartingBlock write SetStartingBlock;
    property LAstUsed: int64 read FLastUsed write SEtLastUsed;
    property DirtyTime: int64 read FDirtyTime write SetDirtyTime;
    function DebugString: string;
    function GetChecksum: int64;
    procedure SetdirtyRange(idx, cnt: ni);
    function VatIndex: int64;
  end;


  TRaidTreeSortMethod = (rtsBlock, rtsDirtyTime, rtsLastUsedTime);

  TRaidTreeItem = class(TBTreeItem)
  public
    rab: TRaidAssembledBuffer;
    procedure Copy(const [unsafe] ACopyTo:TBTreeItem); override;
  end;

  TLinkableRaidTreeItem = class(TLinkable)
  public
    rab: TRaidAssembledBuffer;
  end;

  TLocalList<T: TLinkable> = class(linked_list_btree_compatible.TDirectlyLinkedList<T>);

  TRaidLinkedList<T: TLinkableRaidTreeItem, constructor> = class(TLocalList<T>)
  public
    function AddRaid(rab: TRaidAssembledBuffer; var reverselink: TObject): T;
    procedure RemoveRaid(rab: TRaidAssembledBuffer; var reverselink: TObject);
  end;


  TRaidTreeItem_ByBlock = class(TRaidTreeItem)
  public
    function Compare(const [unsafe] ACompareTo:TBTreeItem):ni;override;
  end;

  TRaidTreeItem_ByDirtyTime = class(TLinkableRaidTreeItem)
  public
    function Compare(const ACompareTo:TBTreeItem):ni;

  end;

  TRaidTreeItem_ByLastUsedTime = class(TLinkableRaidTreeItem)
  public
    function Compare(const ACompareTo:TBTreeItem):ni;
  end;



  TRaidTree = class (TBTree)
  private
    FSortMethod: TRaidTreeSortMethod;
    function GEtItems_Slow(idx: ni): TRaidTreeItem;

  public
    procedure DebugItems_ByBlock;
    procedure DebugItems_ByDirtyTime;
    procedure DebugItems_ByLAstUsed;

    procedure DebugItems;
    property SortMethod: TRaidTreeSortMethod read FSortMethod write FSortMethod;
    function AddRaid(rab: TRaidAssembledBuffer): TRaidTreeItem;
    function Add(const [unsafe] AItem:TBTreeItem):boolean;override;

    procedure ReAdd(itm: TRaidTreeItem);
    procedure RemoveRaid(rab: TRaidAssembledBuffer);
    function Search_StartingBlock_Exact(startingblock: int64): TRaidtreeitem;
    property Items_Slow[idx: ni]: TRaidTreeItem read GEtItems_Slow;
    procedure CheckForDuplicates;

  end;

  TRaidCalculator = class(TBetterObject)
  strict private
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    piece_size: ni;

  private


    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    function CountInvalidPieces: ni;inline;

    //TO convert a single into pieces
    //1. set SizeInBytes (must be multiple of 8)
    //2. set drives (number of pieces to output)
    //3. put data in single
    //4. call singleToPieces
    //5. copy the memory of the TRaidPiece instances to your media, paying attention to piece size

    //TO convert picees into singles
    //1. put your data into the payload arrays for the various pieces (write directly onto TRaidPiece instances including checksums
    //2. set SizeInBytes (must be multiple of 8)
    //3. call PiecesToSingle
    //4. get raw data driectly from single[] (copy to wherever)




  public
    assembled: TRAidAssembledBuffer;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    pieces: array[0..15] of TRaidPiece;
    pieceDebug: array[0..15] of TRaidPieceInfo;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    drives: ni;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    invalid_piece: ni;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    invalid_count: ni;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    invalid_determined: boolean;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    skipped_read: ni;
    //IF YOU ADD ANY FIELDS... CHANGE CopyFROM() function
    procedure CopyFrom(const r: TRaidCalculator);
    procedure BeforeDestruction;override;

    procedure Init;override;

    procedure REset;
    procedure SingleToPieces;
    procedure SingleToPieces1;inline;
    procedure SingleToPieces2;inline;
    procedure PiecesToSingle_Interlace(iMissingPiece: ni = -1);
    procedure PiecesToSingle(iMissingPiece: ni = -1);
    function ScoreCheckSum(cs: int64): ni;inline;
    function DetermineBestCheckSum: int64;inline;
    procedure DetermineInvalidPiece(iSkippedDrive: ni);
    function GetRebuiltQWORD(const iMissing: ni; const piece_idx: ni): int64;inline;


    class function GetPieceSizeUnPadded(iRaidCount: ni): int64;inline;
    class function GetPieceSizePadded(iRaidCount: ni): int64;inline;
    class function GetPayloadSizeInQWords(iRaidCount: ni): int64;inline;
    class function BytesToQWORDS(iBytes: ni): ni;inline;



    procedure DebugInvalidRaid;




  end;

{$IFDEF USE_LINKED_RAID}
  TRaidList = class(TDirectlyLinkedList)
{$ELSE}
  TRaidList = class(TBetterList<TRaidAssembledBuffer>)
{$ENDIF}
  public
    procedure CheckForDeadObjects;inline;
  end;

var
  RAID_TINY_BLOCK_SIZES : array[1..31] of ni;



implementation

{ TRaidCalculator }

function TRaidAssembledBuffer.SlowAnyDirty: boolean;
var
  t: ni;
begin
  result := false;
  for t := low(_dirty) to high(_dirty) do begin
    result := dirty[t];
    if result then exit;
  end;

end;
function TRaidAssembledBuffer.VatIndex: int64;
begin
  result := FStartingBlock shr BIG_BLOCk_BLOCK_SHIFT;
end;

function TRaidAssembledBuffer.AllDirty: boolean;
var
  t: ni;
begin
  result := true;
  for t := low(_dirty) to high(_dirty) do begin
    if _dirty[t] = false then begin
      result := false;
      exit;
    end;
  end;

end;

procedure TRaidCalculator.BeforeDestruction;
begin
  inherited;
//  Debug.Log(self.classname+'.BeforeDestruction');
end;

function TRaidAssembledBuffer.byteptr(addr: int64): pbyte;
begin
  result := @((pbyte(@single))[addr]);
end;

class function TRaidCalculator.BytesToQWORDS(iBytes: ni): ni;
begin
  if iBytes <= 0 then
    exit(0);
  result := (iBytes-1) shr 3;
  inc(result);

end;

procedure TRaidCalculator.CopyFrom(const r: TRaidCalculator);
begin
  raise ECritical.create('Not Supported anymore... I think you wanted something else');
  pieces := r.pieces;
  //single := r.single;
  drives := r.drives;
  //startingblock := r.startingblock;

  invalid_piece := r.invalid_piece;
  invalid_count := r.invalid_count;
  invalid_determined := r.invalid_determined;
  skipped_read := r.skipped_read;

  //single_size := r.single_size;
  piece_size := r.piece_size;

end;

function TRaidCalculator.CountInvalidPieces: ni;
var
  t: ni;
  ps: ni;
begin
  ps := GEtPAyloadSizeInQwords(drives);
  result := 0;
  for t:= 0 to drives-1 do begin
    if not pieces[t].IsValidPiece(ps,t) then begin
      result := result + 1;
    end;
  end;
end;


function TRaidAssembledBuffer.CoversBlock(lba: int64): boolean;
begin
  if (StartingBlock < 0) or (lba < StartingBlock) or (lba >= StartingBlock+RAID_STRIPE_SIZE_IN_BLOCKS) then
    result := false
  else
    result := true;

end;

function TRaidAssembledBuffer.DebugString: string;
var
  tm: ticker;
begin
  tm := GEtTicker;
  result := '['+Self.Fstartingblock.tohexstring+',Sz:'+Getsizeinbytes.tostring+' AGE:'+GEtTimeSince(tm, self.LAstUsed).tostring+' Dirty: '+booltostr(self.AnyDirty)+']';
end;

procedure TRaidCalculator.DebugInvalidRaid;
var
  t: ni;
  ps: ni;
begin
  ps := GEtPAyloadSizeInQwords(drives);
//  debug.Log('Piece '+inttostr(invalid_piece)+' was invalid.');
  debug.Log('Stripe has '+Self.CountInvalidPieces.tostring+' invalid pieces');
  for t:= 0 to drives-1 do begin
    debug.log(booltostrex(self.pieces[t].IsValidPiece(ps, -1), 'Ok', 'XX')+' Piece['+inttostr(t)+'] = '+Self.pieceDebug[t].DebugString+' // '+self.pieces[t].DebugString);
  end;

end;

function TRaidCalculator.DetermineBestCheckSum: int64;
var
  t: ni;
  best, score, temp: int64;
begin
  best := 0;
  score := ScoreCheckSum(self.pieces[0]._stripe_checksum);

  if skipped_read = 0 then
    score := -1;

  for t:= 1 to drives-1 do begin
    temp := ScoreChecksum(self.pieces[t]._stripe_checksum);

    if temp > score then begin
      best := t;
      score := temp;
    end;
  end;


  result := self.pieces[best]._stripe_checksum;



end;

procedure TRaidCalculator.DetermineInvalidPiece(iSkippedDrive: ni);
var
  t: ni;
  ps: ni;
  bestcheck: int64;
begin
  ps := GEtPAyloadSizeInQwords(drives);
  invalid_piece := -1;
  invalid_count := 0;
  bestcheck := DetermineBestCheckSum;
  for t:= 0 to drives-1 do begin
    if t = iSkippedDrive then begin
      invalid_piece := t;
      inc(invalid_count);
    end else
    if (not pieces[t].IsValidPiece(ps,t)) or (pieces[t]._stripe_checksum <> bestcheck) then begin
      invalid_piece := t;
      inc(invalid_count);
      if invalid_piece <> iSkippedDrive then begin
        DebugInvalidRaid;
      end;
    end;
  end;
  invalid_determined := true;
end;

function TRaidAssembledBuffer.FirstClean(iAfter: ni): ni;
begin

  result := iAfter;
  while (dirty[result]) and (result < +RAID_STRIPE_SIZE_IN_BLOCKS) do
    inc(result);

end;

function TRaidAssembledBuffer.FirstDirty(iAfter: ni): ni;
begin
  result := iAfter;
  while (not dirty[result]) and (result < +RAID_STRIPE_SIZE_IN_BLOCKS) do
    inc(result);

end;

function TRaidAssembledBuffer.GetChecksum: int64;
var
  cs: int64;
begin
  CalculatecheckSum(@single[0], single_size, cs);
  result := cs;
end;

function TRaidAssembledBuffer.GetDirty(idx: ni): boolean;
begin
  result := _Dirty[idx];
end;

class function TRaidCalculator.GetPayloadSizeInQWords(iRaidCount: ni): int64;
begin
  result := BytesToQWords(RAID_TINY_BLOCK_SIZES[iRaidCount]);


end;


class function TRaidCalculator.GetPieceSizePadded(iRaidCount: ni): int64;
begin
  result := 0;
  if iRaidCount >= 0 then begin
    result := (GetPayloadSizeInQWords(iRAidCount) * sizeof(int64))+TRaidPiece.HeaderSize;
  end;
end;

class function TRaidCalculator.GetPieceSizeUnpadded(iRaidCount: ni): int64;
begin
  result := 0;
  if iRaidCount >= 0 then begin
    result := (RAID_TINY_BLOCK_SIZES[iRaidCount]+TRaidPiece.HeaderSize);
  end;
end;


function TRaidCalculator.GetRebuiltQWORD(const iMissing: ni; const piece_idx: ni): int64;
var
  t: ni;
  bIsParity: boolean;
begin
  result := 0;
  for t:= 0 to drives-1 do begin
    bIsParity := t = (drives-1);
    if t <> iMissing then
      result := result xor pieces[t].payload[piece_idx];
  end;

end;

procedure TRaidAssembledBuffer.CopyFrom(rab: TRaidAssembledBuffer);
begin
  single_size := rab.single_size;
  movemem32(@single[0], @rab.single[0], sizeof(self.single));
  _dirty := rab._dirty;
  anyDirty := rab.AnyDirty;
  dirtytime := rab.dirtytime;
  startingblock := rab.startingblock;
  lastused := rab.lastused;
  readfromdisk := rab.readfromdisk;
//  Self.treeItemBlock := rab.treeItemBlock;
//  self.treeItemDirtyTime := rab.treeItemDirtyTime;
//  self.treeItemLastUsed := rab.treeItemLastUsed;
end;

function TRaidAssembledBuffer.GetSizeInBytes: ni;
begin
  result := single_size * sizeof(int64);
end;

procedure TRaidAssembledBuffer.init;
begin
  inherited;
  startingblock := -1;
  readfromdisk := false;
  legit := false;
  Archive := false;

end;

procedure TRaidAssembledBuffer.cleardirty;
begin
  fillmem(pointer(@_dirty), sizeof(_dirty),0);
  AnyDirty := false;
end;

procedure TRaidCalculator.Init;
begin
  inherited;
  Reset;
end;

procedure TRaidCalculator.PiecesToSingle_Interlace(iMissingPiece: ni);
var
  idxSingle, idxPiece: fi;
  iDrive: ni;
  accum: int64;
  stripesum: int64;
  temp: int64;
  bIsParity: boolean;
  bIsMissing: boolean;
  bMissingParity: boolean;
  interlace: ni;
  pp: PRaidPiece;
  pSingle: Pint64;
begin
  piece_size := BytesToQWORDS(GEtPieceSizeUnpadded(drives));

  if not invalid_determined then
    DetermineInvalidPiece(iMissingPiece);

  iMIssingPiece := invalid_piece;

  bMissingParity := false;
  //if still no missing piece, then ignore the parity drive
  if (drives > 1) and (iMissingPiece = -1) then begin
    iMissingPiece := drives-1;
    bMissingParity := true;
  end;


  idxSingle := 0;
  pSingle := @assembled.single[0];
  idxPiece := 0;
  iDrive := 0;
  accum := 0;
  stripesum := 0;
  interlace := 0;
  while interlace < drives do begin
    iDrive := interlace;
    idxpiece := 0;
    pp := @pieces[iDrive];
    idxSingle := interlace;
    pSingle := @assembled.single[interlace];

    while idxSingle < assembled.single_size do begin
      if idrive = interlace then begin
        bIsParity := (drives > 1) and (idrive = (drives-1));
        bIsMissing := iDrive = iMissingPiece;

        //if this piece is not missing
        if not bIsMissing then begin
          temp := pp.payload[idxPiece];
        end else begin
          temp := GetRebuiltQWORD(iMissingPiece, idxPiece);
        end;
      end else
        temp := 0;

      //if not the parity drive, then save this value
 //     single[idxSingle] := temp;
      inc(idxSingle);//next index
      psingle^ := temp;
      inc(psingle);


      //move to the next drive
      if (drives > 1) then begin
        inc(iDrive);
        if iDrive = (drives-1) then begin
          iDrive := 0;
          inc(idxPiece);//next index in the piece array
        end;
      end else begin
        inc(idxPiece);
      end;
    end;
    inc(interlace);
  end;

end;

procedure TRaidCalculator.REset;
var
  t: ni;
begin
  invalid_piece := -1;
  invalid_count := 0;
  invalid_determined := false;
  skipped_read := -1;

  for t:= 0 to High(pieces) do begin
    pieces[t].Reset;
  end;


end;

procedure TRaidCalculator.PiecesToSingle(iMissingPiece: ni = -1);
{$IFDEF DOASM2}
                            //var
                            //  idxSingle, idxPiece: fi;
                            //  iDrive: fi;
                            //  accum: int64;
                            //  stripesum: int64;
                            //  temp: int64;
                            //  bIsParity: boolean;
                            //  bIsMissing: boolean;
                            //  bMissingParity: boolean;
                            //begin
                            //  piece_size := BytesToQWORDS(GEtPieceSizeUnpadded(drives));
                            //
                            //  if not invalid_determined then
                            //    DetermineInvalidPiece(iMissingPiece);
                            //
                            //  iMIssingPiece := invalid_piece;
                            //
                            //  bMissingParity := false;
                            ////  if still no missing piece, then ignore the parity drive
                            //  if (drives > 1) and (iMissingPiece = -1) then begin
                            //    iMissingPiece := drives-1;
                            //    bMissingParity := true;
                            //  end;
                            //
                            //
                            //  idxSingle := 0;
                            //  idxPiece := 0;
                            //  iDrive := 0;
                            //  accum := 0;
                            //  stripesum := 0;
                            //  while idxSingle < single_size do begin
                            //    bIsParity := (drives > 1) and (idrive = (drives-1));
                            //    bIsMissing := iDrive = iMissingPiece;
                            //
                            ////    if this piece is not missing
                            //    if not bIsMissing then begin
                            //      temp := pieces[iDrive].payload[idxPiece];
                            //    end else begin
                            //      temp := GetRebuiltQWORD(iMissingPiece, idxPiece);
                            //    end;
                            //
                            ////    if not the parity drive, then save this value
                            //    single[idxSingle] := temp;
                            //    inc(idxSingle);//next index
                            //
                            ////    move to the next drive
                            //    if (drives > 1) then begin
                            //      inc(iDrive);
                            //      if iDrive = (drives-1) then begin
                            //        iDrive := 0;
                            //        inc(idxPiece);//next index in the piece array
                            //      end;
                            //    end else begin
                            //      inc(idxPiece);
                            //    end;
                            //  end;
                            //end;

{$ELSE}
var
  idxSingle, idxPiece: fi;
  iDrive: fi;
  accum: int64;
  stripesum: int64;
  temp: int64;
  bIsParity: boolean;
  bIsMissing: boolean;
  bMissingParity: boolean;
  iParityDrive: fi;
  iLastDrive: fi;


begin
  temp := GEtPieceSizeUnpadded(drives);
  piece_size := BytesToQWORDS(temp);

  if not invalid_determined then
    DetermineInvalidPiece(iMissingPiece);

  iMIssingPiece := invalid_piece;

  bMissingParity := false;
  //if still no missing piece, then ignore the parity drive
  if (drives > 1) and (iMissingPiece = -1) then begin
    iMissingPiece := drives-1;
    bMissingParity := true;
  end;


  idxSingle := 0;
  idxPiece := 0;
  iDrive := 0;
  accum := 0;
  stripesum := 0;
  iLAstDrive := drives-1;
  if drives < 1 then
    iParityDrive := -1
  else
    iParityDrive := iLAstDrive;

  while idxSingle < assembled.single_size do begin
    bIsParity := iDrive = iParityDrive;
    bIsMissing := iDrive = iMissingPiece;

    //if this piece is not missing
    if not bIsMissing then begin
      temp := pieces[iDrive].payload[idxPiece];
    end else begin
      temp := GetRebuiltQWORD(iMissingPiece, idxPiece);
    end;

    //if not the parity drive, then save this value
    assembled.single[idxSingle] := temp;
    inc(idxSingle);//next index

    inc(iDrive);
    if iDrive >= iLAstDrive then begin
      iDrive := 0;
      inc(idxPiece);//next index in the piece array
    end;
  end;


end;
{$ENDIF}

function TRaidCalculator.ScoreCheckSum(cs: int64): ni;
var
  t: ni;
begin
  result := 0;
  for t:= 0 to drives-1 do begin
    if pieces[t]._stripe_checksum = cs then
      inc(result);

    if (drives = 2) and (pieces[t]._stripe_checksum <> 0) then
      inc(result);
  end;

end;

procedure TRaidAssembledBuffer.SetAnyDirty(const Value: boolean);
var
  bDoTree: boolean;
  tr: TLocalList<TRaidTreeItem_ByDirtyTime>;
begin
  if value = FanyDirty then exit;

  bDoTree := (treeItemDirtyTime <> nil) and (treeItemDirtyTime.tree <> nil);
  tr := nil;
  if bDoTree then
    tr := treeItemDirtyTime.tree as TLocalList<TRaidTreeItem_ByDirtyTime>;

  if tr <> nil then
    tr.Remove(treeItemDirtyTime,true);

  FAnyDirty := Value;

  if tr <> nil then begin
    if VAlue then
      tr.Add(treeItemDirtyTime);
  end;
end;

procedure TRaidAssembledBuffer.SetDirty(idx: ni; const Value: boolean);
var
  tr: TLocalList<TRaidTreeItem_ByDirtyTime>;
begin
  _Dirty[idx] := value;
  if not anydirty then begin
    tr := nil;
    if (treeItemDirtyTime <> nil) and (treeItemDirtyTime.tree <> nil) then begin
      tr := treeItemDirtyTime.tree as TLocalList<TRaidTreeItem_ByDirtyTime>;
      tr.Remove(treeItemDirtyTime,true);
      FAnyDirty := true;
      Fdirtytime := GEtTicker;
      tr.Add(treeItemDirtyTime);

    end else begin
      FAnyDirty := Value;
      Fdirtytime := GEtTicker;
    end;
  end;
end;

procedure TRaidAssembledBuffer.SetdirtyRange(idx, cnt: ni);
var
  t: ni;
begin
  t := 0;
  while cnt > 0 do begin
    Dirty[t] := true;
    inc(t);
    dec(cnt);
  end;
end;

procedure TRaidAssembledBuffer.SetDirtyTime(const Value: int64);
var
  bDoTree: boolean;
  tr: TLocalList<TRaidTreeItem_ByDirtyTime>;
begin
  bDoTree := (treeItemDirtyTime <> nil) and (treeItemDirtyTime.tree <> nil);
  tr := nil;
  if bDoTree then
    tr := treeItemDirtyTime.tree as TLocalList<TRaidTreeItem_ByDirtyTime>;

  if tr <> nil then
    tr.Remove(treeItemDirtyTime,true);

  FDirtyTime := Value;

  if tr <> nil then
    tr.Add(treeItemDirtyTime);
end;

procedure TRaidAssembledBuffer.SEtLastUsed(const Value: int64);
var
  bDoTree: boolean;
  tr: TLocalList<TRaidTreeItem_ByLAstUsedTime>;
begin
  bDoTree := (treeItemLastUsed <> nil) and (treeItemLastUsed.tree <> nil);
  tr := nil;
  if bDoTree then
    tr := treeItemLastUsed.tree as TLocalList<TRaidTreeItem_ByLAstUsedTime>;

  if tr <> nil then
    tr.Remove(treeItemLastUsed,true);

  FLAstUsed := Value;

  if tr <> nil then
    tr.Add(treeItemLastUsed);
end;

procedure TRaidAssembledBuffer.SEtSizeInBytes(const Value: ni);
var
  d,m: ni;
begin
{$IFDEF SHIFTOPT}
  d := value shr 3;
  m := value and (not int64(7));
{$ELSE}
  d := value div (sizeof(int64));
  m := value mod (sizeof(int64));

{$ENDIF}
  single_size := d;
  if (m > 0) then
    inc(single_size);

end;

procedure TRaidAssembledBuffer.SetStartingBlock(const Value: int64);
var
  bDoTree: boolean;
  tr: TRaidTree;
begin
  bDoTree := (treeItemBlock <> nil) and (treeItemBlock.tree <> nil);
  tr := nil;
  if bDoTree then
    tr := treeItemBlock.tree as TRaidTree;

  if tr <> nil then
    tr.Remove(treeItemBlock,true);

  FStartingBlock := Value;

  if tr <> nil then
    tr.Add(treeItemBlock);
end;

procedure TRaidCalculator.SingleToPieces1;
var
  idxSingle, idxPiece: nativeint;
  iDrive: fi;
  accum: int64;
  stripesum: int64;
  temp: int64;
  iLastDrive: fi;
  psz: fi;
begin
  idxSingle := 0;
  idxPiece := 0;
  iDrive := 0;
  accum := 0;
  stripesum := 0;
  iLAstDrive := (drives-1);
{$IFDEF BETTER}
  if drives = 1 then begin
    while idxSingle < single_size do begin
      //accumulate value
      temp := single[idxSingle];
      accum := accum xor temp;
      stripesum := stripesum xor temp;
      //write actual value to piece
      pieces[iDrive].payload[idxPiece] := temp;
      //move forward in single array
      inc(idxSingle);
      inc(iDrive)
    end;
  end else begin
    while idxSingle < single_size do begin
      //if this is the last drive
      if (iDrive = iLastDrive) then begin
        //write the accumulator
        pieces[iDrive].payload[idxPiece] := accum;
        //reset the accumulator
        accum := 0;
        //reset the drive
        iDrive := 0;
        //increment the piece index
        inc(idxPiece);
      end else begin
        //accumulate value
        temp := single[idxSingle];
        accum := accum xor temp;
        stripesum := stripesum xor temp;
        //write actual value to piece
        pieces[iDrive].payload[idxPiece] := temp;
        //move forward in single array
        inc(idxSingle);
        inc(iDrive);
        inc(idxPiece);
      end;
    end;
  end;
{$ELSE}
  while idxSingle < assembled.single_size do begin
    //if this is the last drive
    if (drives > 1) and (iDrive = iLastDrive) then begin
      //write the accumulator
      pieces[iDrive].payload[idxPiece] := accum;
      //reset the accumulator
      accum := 0;
      //reset the drive
      iDrive := 0;
      //increment the piece index
      inc(idxPiece);
    end else begin
      //accumulate value
      temp := assembled.single[idxSingle];
      accum := accum xor temp;
      stripesum := stripesum xor temp;
      //write actual value to piece
      pieces[iDrive].payload[idxPiece] := temp;
      //move forward in single array
      inc(idxSingle);
      if (drives > 1) then
        inc(iDrive)
      else
        inc(idxPiece);
    end;


  end;
{$ENDIF}

  //if odd number of drives, we need to fill out the last group properly with ZEROS plus the accumulated value
  while iDrive < iLastDrive do begin
    pieces[iDrive].payload[idxPiece] := 0;
    inc(iDrive);
  end;

  //put the final accum in the payload
  if (drives> 1) then
    pieces[iDrive].payload[idxPiece] := accum;


  psz := GetPayloadSizeInQWords(drives);
  for iDrive := 0 to drives-1 do begin
    if idxpiece <> psz then
      idxpiece := psz;
    pieces[iDRive]._piece_checksum := pieces[iDRive].CalcPieceChecksum(idxpiece);
    pieces[iDrive]._stripe_checksum := stripesum;
  end;

//  if pieces[iDRive]._piece_checksum <> pieces[iDrive]._stripe_checksum then
//    Debug.Log(self,'wtf');

  piece_size := idxPiece;



end;

procedure TRaidCalculator.SingleToPieces;
begin
  SingleToPieces1;

end;

procedure TRaidCalculator.SingleToPieces2;
type
  PPint64 = ^PInt64;
var
  idxSingle, idxPiece: nativeint;
  iDrive: fi;
  accum: int64;
  stripesum: int64;
  temp: int64;
  iLastDrive: fi;
  psz,t: fi;
  qwords, lacedqwords,cx: fi;
  pStart, pIdx, pLEaf: PInt64;
  pardrive, top_non_parity: fi;
{$IFDEF ARRAYME}
  pptrs: array[0..15] of PInt64;
  pppFirst: PPInt64;
  ppp: PPInt64;
{$ELSE}
  pp: PInt64;
{$ENDIF}


begin
  idxSingle := 0;
  idxPiece := 0;
  iDrive := 0;
  accum := 0;
  stripesum := 0;
  iLAstDrive := (drives-1);
  qwords := assembled.PayloadSizeInBytes shr 3;
  lacedqwords := ((qwords -1) div drives)+1;
  pStart := @assembled.single[0];
  if drives > 1 then begin
    //go through the main drives
    top_non_parity := drives-2;
    pardrive := drives-1;
    iDrive := 0;
{$IFDEF ARRAYME}
    for t:= 0 to pardrive do begin
      pptrs[t] := @pieces[iDrive].payload[0];
    end;
{$ENDIF}

{$IFDEF ARRAYME}
    pppFirst := @pptrs[0];
    ppp := pppFirst;
{$ELSE}
{$ENDIF}

    while iDrive < top_non_parity do begin
      {$IFNDEF ARRAYME}
      pp := @pieces[iDrive].payload[0];
      {$ENDIF}
      pIdx := pStart;
      inc(pIdx, drives);
      cx := lacedqwords;
      idxPiece := 0;
      while cx > 0 do begin
        temp := pIDX^;
        {$IFDEF ARRAYME}
        ppp^^ := temp;
        {$ELSE}
        pp^ := temp;
        {$ENDIF}
        stripesum := stripesum xor temp;
        inc(pIDX, drives);//moves by 8*drives
        dec(cx);
        inc(idxPiece);
        {$IFDEF ARRAYME}
        inc(ppp^);
        {$ELSE}
        inc(pp);
        {$ENDIF}
      end;
      inc(iDrive);
      {$IFDEF ARRAYME}
      inc(ppp,1);
      {$ENDIF}
    end;

    //parity drive
    pIDX := pStart;
    cx := qwords;
    idxPiece := 0;
    while cx > 0 do begin
      accum := 0;
      {$IFNDEF ARRAYME}
      pp := @pieces[pardrive].payload[0];
      {$ENDIF}
      for t := 0 to top_non_parity do begin
        accum := accum xor pIDX^;
        if (t=top_non_parity) then begin
          {$IFDEF ARRAYME}
          pptrs[pardrive]^ := accum;
          inc(pptrs[pardrive]);
          {$ELSE}
          pp^ := accum;
          inc(pp);
          {$ENDIF}
        end;

      end;
      dec(cx, drives-1);
    end;


  end else begin
    pIdx := pStart;
    cx := lacedqwords;
    pLEaf := @pieces[0].payload[0];
    while cx>0 do begin
      temp := pIDX^;
      pLeaf^ := temp;
      dec(cx);
      inc(pIDX);
      inc(pLEAF);
    end;
  end;

  psz := GetPayloadSizeInQWords(drives);
  for iDrive := 0 to drives-1 do begin
    if idxpiece <> psz then
      idxpiece := psz;
    pieces[iDRive]._piece_checksum := pieces[iDRive].CalcPieceChecksum(idxpiece);
    pieces[iDrive]._stripe_checksum := stripesum;
  end;



end;

{ TRaidPiece }


function TRaidPiece.CalcPieceChecksum(sizeInQwords: ni): int64;
{$IFDEF CPUX64}
{x$DEFINE DOASM}
{$ENDIF}
{$IFDEF DOASM}
asm
  .NOFRAME
  mov r8, rcx  //self
  add r8,16    //self+16 is start of payload
  mov rax, 0   //result initialize
  mov rcx, rdx //rdx is the sizeinQwords parameter, move to cx as counter
@rep:
  mov r9, qword ptr [r8]
  xor rax, r9
  add r8,8
  sub rcx,1
@recomp:
  cmp rcx,0
  jnz @rep
  ret
end;
{$ELSE}
var
  accum: int64;
  t: ni;
begin
  self.sizeinQwords := sizeInQWords;
  accum := 0;
  for t:= 0 to sizeInQwords-1 do begin
    accum := accum xor payload[t];
  end;
  result := accum;
end;
{$ENDIF}

function TRaidPiece.DebugString: string;
begin
  result := 'scs:'+inttohex(_stripe_checksum,16)+' pcs:'+inttohex(_piece_checksum,16)+' xpcs:'+inttohex(CalcPieceChecksum(self.sizeinQWords),16);
end;

class function TRaidPiece.HeaderSize: int64;
begin
  result := 16;
end;

function TRaidPiece.IsValidPiece(size: ni; idx_ForDebugging: ni): boolean;
var
  accum: int64;
begin
  accum := CalcPieceCheckSum(size);

  result := accum = _piece_checksum;
  if not result then begin
//    WriteLn('piece is not valid.');
    if idx_fordebugging >=0 then
      Debug.Log('piece '+inttostr(idx_fordebugging)+' is not valid (invalid piece checksum)');
  end;


end;

function TRaidPiece.payload32: PUInt32;
begin
  result := PUInt32(@payload[0]);
end;

procedure TRaidPiece.Put(idx: ni; val: int64);
begin
  payload[idx] := val;
  _piece_checksum := _piece_checksum xor val;
end;

procedure TRaidPiece.Reset;
begin
  _piece_checksum := $BAD;
  _stripe_checksum := $BAD;
  //fillmem(@payload[0], sizeof(payload), 0);

end;





procedure oinit;
var
  t: ni;
  tiny, rem: ni;
begin


  for t:= low(RAID_TINY_BLOCK_SIZES) to high(RAID_TINY_BLOCK_SIZES) do begin
    tiny := RAID_STRIPE_SIZE_IN_BYTES;
    if t > 1 then begin
      tiny := RAID_STRIPE_SIZE_IN_BYTES div (t-1);
      if ((RAID_STRIPE_SIZE_IN_BYTES mod (t-1)) > 0) then begin
        tiny := tiny + 1;
      end;


    end;
    RAID_TINY_BLOCK_SIZES[t] := tiny;
  end;




end;

procedure ofinal;
begin
  //dni
end;



{ TRaidList }

procedure TRaidList.CheckForDeadObjects;
var
  t: ni;
begin
{$IFNDEF RELEASE}
  for t:= 0 to count-1 do begin
    if items[t].Detached then
      raise ECritical.create('wtf');
  end;
{$ENDIF}

end;

{ TRaidTreeItem }


procedure TRaidTreeItem.Copy(const [unsafe] ACopyTo: TBTreeItem);
begin
  inherited;
  TRaidTreeItem(ACopyto).rab := rab;

end;

{ TRaidTreeItem_ByBlock }

function TRaidTreeItem_ByBlock.Compare(const [unsafe] ACompareTo: TBTreeItem): ni;
var
  cto: TRAidTreeItem_ByBlock;
begin
  cto := TRaidTreeItem_byblock(acompareTo);
  result := cto.rab.startingblock - self.rab.startingblock;
end;

{ TRaidTreeItem_BytIME }

function TRaidTreeItem_ByDirtytIME.Compare(const ACompareTo: TBTreeItem): ni;
var
  cto: TRAidTreeItem_ByDirtyTime;
var
  d1,d2: ni;
begin
  if acompareto = nil then
    debug.consolelog('whooooa dere little bugger');
  cto := TRaidTreeItem_byDirtyTime(acompareTo);
  d1 := booltoint(cto.rab.AnyDirty);
  d2 := booltoint(self.rab.AnyDirty);
  result := (d1-d2);
  if result = 0 then begin
    result := 0-(cto.rab.dirtytime - self.rab.dirtytime);
  end;
end;

{ TRaidTree }


function TRaidTree.Add(const [unsafe] AItem: TBTreeItem): boolean;
begin
  if TRaidTreeItem(AItem).rab.startingblock = 1758 then
    debug.Log(self,'trap wtf');

  result := inherited;

end;

function TRaidTree.AddRaid(rab: TRaidAssembledBuffer): TRaidTreeItem;
begin
  result := nil;
  case Sortmethod of
    rtsBlock: result := TRaidTreeItem_ByBlock.create;
    rtsDirtyTime: raise ECritical.create('deprecated');//result := TRaidTreeItem_ByDirtyTime.create;
    rtsLastUsedTime: raise ECritical.create('deprecated');//TRaidTreeItem_ByLastUsedTime.create;
  end;

  if result = nil then
    raise ECritical.create('unhanded raid tree type');
  result.rab := rab;
  case Sortmethod of
    rtsBlock: rab.treeitemBlock := result as TRaidTreeItem_ByBlock;
    rtsDirtyTime: raise ECritical.create('deprecated');//result as TRaidTreeItem_ByDirtyTime;
    rtsLastUsedTime: raise ECritical.create('deprecated');//result as TRaidTreeItem_ByLastUsedTime;
  end;
  self.Add(result);
end;

procedure TRaidTree.CheckForDuplicates;
var
  iStart, iThis: TRaidTreeItem;
  t,u: ni;
begin
  exit;
  Debug.ConsoleLog('Start check For Duplicates');
  for u := 0 to count-1 do begin
    iStart := items_slow[u];
    for t := u+1 to count-1 do begin
      iThis := items_slow[t];
      //Debug.consolelog('compare:' +inttohex(ni(pointer(iStart)),8)+' to '+inttohex(ni(pointer(iThis)),8));
      if iStart = iThis then
        raise ECritical.create('MEGA wtf duplicates!');
      if iStart.rab = iThis.rab then
        raise ECritical.create('wtf duplicates!');
    end;
  end;
  Debug.ConsoleLog('End Check For Duplicates');

end;

procedure TRaidTree.DebugItems;
var
  res,itm: TRaidTreeItem;
  cnt: ni;
begin
  itm := nil;
  res := nil;
  Debug.ConsoleLog('Begin Iterate');
  Iterate(
    procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
    begin
      debug.consolelog('iterate '+inttohex(nativeint(pointer(TRaidTreeItem(ABtreeItem))),8));
      itm := TRaidTreeItem(ABTreeItem);
    end
  );
  Debug.ConsoleLog('End Iterate returning '+inttohex(nativeint(pointer(TRaidTreeItem(itm))),8));

end;

procedure TRaidTree.DebugItems_ByBlock;
var
  res,itm: TRaidTreeItem;
  cnt: ni;
begin
  itm := nil;
  res := nil;
  Debug.ConsoleLog('Begin Iterate');
  Iterate(
    procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
    begin
      debug.consolelog('iterate @'+inttohex(nativeint(pointer(TRaidTreeItem(ABtreeItem))),8)+':'+inttostr(TRaidTreeItem_ByBlock(ABtreeItem).rab.startingblock)+' balance='+inttostr(ABTreeItem.Fbalance));
      itm := TRaidTreeItem(ABTreeItem);
    end
  );
  Debug.ConsoleLog('End Iterate returning '+inttohex(nativeint(pointer(TRaidTreeItem(itm))),8));

end;


procedure TRaidTree.DebugItems_ByLAstUsed;
var
  res,itm: TRaidTreeItem;
  cnt: ni;
begin
  raise ECritical.create('deprecated');
  itm := nil;
  res := nil;
  Debug.ConsoleLog('Begin Iterate');
  Iterate(
    procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
    begin
      debug.consolelog('iterate @'+inttohex(nativeint(pointer(TRaidTreeItem(ABtreeItem))),8)+':'+inttostr(TRaidTreeItem_ByLAstUsedTime(ABtreeItem).rab.lastused));
      itm := TRaidTreeItem(ABTreeItem);
    end
  );
  Debug.ConsoleLog('End Iterate returning '+inttohex(nativeint(pointer(TRaidTreeItem(itm))),8));

end;

procedure TRaidTree.DebugItems_ByDirtyTime;
var
  res,itm: TRaidTreeItem;
  cnt: ni;
begin
  raise ECritical.create('deprecated');
  itm := nil;
  res := nil;
  Debug.ConsoleLog('Begin Iterate');
  Iterate(
    procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
    begin
      debug.consolelog('iterate @'+inttohex(nativeint(pointer(TRaidTreeItem(ABtreeItem))),8)+':'+inttostr(TRaidTreeItem_ByDirtytime(ABtreeItem).rab.dirtytime)+' balance='+inttostr(ABTreeItem.Fbalance));
      itm := TRaidTreeItem(ABTreeItem);
    end
  );
  Debug.ConsoleLog('End Iterate returning '+inttohex(nativeint(pointer(TRaidTreeItem(itm))),8));

end;



function TRaidTree.GEtItems_Slow(idx: ni): TRaidTreeItem;
var
  res,itm: TRaidTreeItem;
  cnt: ni;
begin
  itm := nil;
  res := nil;
  result := nil;
  cnt := 0;
  Debug.ConsoleLog('Begin Iterate');
  Iterate(
    procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
    begin
      debug.consolelog('iterate '+inttostr(cnt)+' sb='+inttostr(TRaidTreeItem(ABtreeItem).rab.startingblock)+' @'+inttohex(ni(pointer(abtreeitem)),1));
      itm := TRaidTreeItem(ABTreeItem);
      if cnt = idx then begin
        res := itm;
        ANeedStop := true;
        Debug.ConsoleLog('FOUND @'+inttostr(cnt));
      end;

      inc(cnt);
    end
  );
  Debug.ConsoleLog('End Iterate returning '+inttohex(nativeint(pointer(TRaidTreeItem(itm))),8));
  result := res;

end;

procedure TRaidTree.ReAdd(itm: TRaidTreeItem);
var

  t:ni;
begin
  if itm = nil then
    ECritical.create('itm is nil');
  raise ECritical.create('don''t do this.  you cannot change the index while the item is in the tree... remove, change index, add');
  DebugItems;
  remove(itm, true);
  DebugItems;
  for t:= 0 to count-1 do
    if Items_Slow[t] = itm then begin
      remove(itm,true);
    end;
  add(itm);
  DebugItems;
end;

procedure TRaidTree.RemoveRaid(rab: TRaidAssembledBuffer);
var
  itm: TRaidTreeItem;
begin
  itm := nil;
  Iterate(
    procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
    begin
      itm := TRaidTreeItem(ABTreeItem);
      if rab = itm.rab then
        ANeedSTop := true;
    end
  );

  if itm <> nil then
    remove(itm);

end;

function TRaidTree.Search_StartingBlock_Exact(startingblock: int64): TRaidtreeitem;
var
  itm: TRaidTreeItem;
  sb: int64;
begin
  result := nil;
  itm := TRaidtreeItem(FRoot);
  while itm <> nil do begin
    sb := itm.rab.startingblock;
    if sb > startingblock then
      itm := TRaidTreeItem(itm.FLeftNode)
    else
    if sb < startingblock then
      itm := TRaidTreeItem(itm.FRightNode)
    else begin
      result := itm;
      break;
    end;
  end;
end;

{ TRaidTreeItem_ByLastUsedTime }

function TRaidTreeItem_ByLastUsedTime.Compare(
  const ACompareTo: TBTreeItem): ni;
var
  cto: TRaidTreeItem_byLAstUsedTime;
begin
  cto := TRaidTreeItem_byLAstUsedTime(acompareTo);
  result := cto.rab.lastused - self.rab.lastused;
end;

{ TRaidLinkedList<T> }

function TRaidLinkedList<T>.AddRaid(rab: TRaidAssembledBuffer; var reverselink: TObject): T;
var
  itm: T;
begin
  itm := T.create;
  itm.rab := rab;
  reverselink := itm;
  Add(itm);

end;

procedure TRaidLinkedList<T>.RemoveRaid(rab: TRaidAssembledBuffer; var reverselink: TObject);
var
  itm: TLinkableRaidTreeItem;
begin
  itm := TLinkableRaidTreeItem(reverselink);
  if itm <> nil then begin
    Remove(itm as T,false);
    reverselink := nil;
    //rab.free;
  end;
end;

{ TRaidPieceInfo }

function TRaidPieceInfo.DebugString: string;
begin
  result := '['+piece_idx.tostring+'] BB:0x'+big_block_index.ToHexString+' PI:'+PieceIndex.tostring+' @'+inttohex(self.PAyloadAddr,16)+'base @'+inttohex(self.BigBlockFileBaseAddr,16)+' F:'+self.FileOrigin;
end;

procedure TRaidPieceInfo.Init;
begin
  piece_idx := 0;
  big_block_index := 0;
  PayloadAddr := 0;
  BigBlockFileBaseAddr := 0;
  FileOrigin := 'Skipped';

end;

function TRaidPieceInfo.PieceIndex: int64;
begin
  result := (VirtualAddr-((big_block_index shl BIG_BLOCK_BYTE_SHIFT) shr STRIPE_SHIFT));
end;

initialization

orderlyinit.init.RegisterProcs('raid', oinit, ofinal, 'ManagedThread,CommandProcessor');


end.
