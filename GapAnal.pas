unit GapAnal;

interface


//The gap management algorithm
//-- ConsumeRegion
// . We need to find a gap that complete spans the region we want to use,
//   else error
// . We need to Split This region into two, thus consuming space, flagging that
//   space as allocated


//-- Free Region
// . We need to Find all Gaps that overlap that region and expand/combine them
// . Into basically one gap.  This may involve deleting gaps or simply
// . resizing one single gap.





uses
  typex, systemx, betterobject, sharedobject, commandprocessor, Btree, classes, sysutils, debug, stringx, numbers;

type
  TGapContainment = (gcNoRules, gcExternal, gcTouching, gcOverlappingOrTouching, gcWithin, gcSpanning);
  TGapInfo = record
    start: int64;
    length: int64;
  end;
  TRegion = class(TBTreeItem)
  strict private
    FConsumed: boolean;
    FLength: int64;
    FStart: int64;
    function GetBeyond: int64;
  private
    function GetEnding: int64;
    procedure SetBeyond(const Value: int64);
    procedure SetEnding(const Value: int64);
  public
    function Compare(const [unsafe] ACompareTo:TBTreeItem):ni; override;
    procedure Copy(const [unsafe] ACopyTo:TBTreeItem); override;

    property Start: int64 read FStart write FStart;
    property Ending: int64 read GetEnding write SetEnding;
    property Length: int64 read FLength write FLength;
    property Beyond: int64 read GetBeyond write SetBeyond;
    property Consumed: boolean read FConsumed write FConsumed;
    function DebugString: string;
    function IsTangentRight: boolean;
  end;

  TGABTree = class(TBTree)

    procedure TryCombineAll;

    procedure CheckConsistency;
  private
    function SearchRight(const iSpanAddress: int64; const iBeyondAddress: int64; gc: TGapContainment): TRegion;
    function SearchLeft(const iSpanAddress, iBeyondAddress: int64; gc: TGapContainment): TRegion;
    function MeetsSpanningRules(n: TRegion; const iSpanAddress, iBEyondAddress: int64; gc: TGapContainMent): boolean;
  end;
  TIterateRegionsProcedure = reference to procedure([unsafe] ABTreeItem:TRegion; var ANeedStop:boolean);


  TGapCAlcMode = (gcmNoOverlapAssumed, gcmOverlapAllowed);
  TGapAnalyzer = class(TSharedObject)
  //How to Use:
  //Declare the length of the content
  //    -- First Gap is Declared automatically

  //Record the consumed regions in the GapAnalyzer
  //    -- As consumed regions are added, gaps will be split
  //        . find a gap that starts before the consumed region and ends after
  //        . if no gap is found, throw an exception if gcmNoOverlapAssumed
  //        . if there's a gap in frong of the consumed region
  //          . set the end of the gap to the start of the consumed region
  //        . add the consumed region
  //        . if there's a gap at the end of the consumed region,
  //              . add another gap

  // to remove a consumed region
  //    -- add the consumed region to the gap list
  //    -- if the gap to the left is adjacent, combine
  //    -- if the gap to the right is adjacent, combine


  //when length of content changes
  //   -- IF the length is less than before
  //    . find the last gap
  //    . if the gap is at the end of length, then resize gap
  //    . if the gap size results to 0, then remove
  //    . find the last consumed
  //    . if the consumed is at the end of length, then resize consumed
  //    . if the consumed size results to 0, then remove
  //   -- IF the length is greater than before
  //    . add a gap
  //    . look left and combine if necessary


  strict
  private
    FGaps: TGABTree;
    FGapCalcMode: TGapCalcMode;
    procedure SetGapCalcMode(const Value: TGapCalcMode);
  protected
    FLength: int64;
    procedure SetLength(const Value: int64); protected
    function GEtDebugSTringForTree(tree: TBTree): string;
    function  TryCombineLeft(r: TRegion; tree: TGABTree): TRegion;
    function TryCombineRight(r: TRegion; tree: TBTree): TRegion;

  public
    procedure Init;override;
    destructor Destroy;override;

    property CalcMode: TGapCalcMode read FGapCalcMode write SetGapCalcMode;
    function GEtDebugConsumed: string;
    function GEtDebugGAps: string;
    property Length: int64 read FLength write SetLength;

    function CanConsumeRegion(iStart, iLength: int64): boolean;
    procedure ConsumeRegion(iStart, iLength: int64);
    procedure FreeRegion(iStart, iLength: int64; bExtralogging: boolean = false);
    function FindGap(minimum_size: int64; less_than: int64; bMinimizeFragmentation: boolean = false): TGapInfo;
    function FindGapSpanning(start, minimum_size: int64): TGapInfo;
    procedure Clear;
    function Sum: int64;
    procedure Iterate(AProcedure:TIterateRegionsProcedure);

  end;


implementation

{ TConsumedRegion }


{ TGapAnalyzer }


function TGapAnalyzer.CanConsumeRegion(iStart, iLength: int64): boolean;
var
  gi: TGapInfo;
begin

  gi := FindGapSpanning(iStart, iLength);
  result :=  gi.start >= 0;

end;

procedure TGapAnalyzer.Clear;
begin
  lock;
  try
    FGaps.clearBruteForce;
    FLength := 0;
  finally
    unlock;
  end;
end;

procedure TGapAnalyzer.ConsumeRegion(iStart, iLength: int64);
var
  nugap, gap: TRegion;
  iOldBeyond: int64;
begin
//  Debug.Log('CONSUME REGION');
  if iLength = 0 then
    exit;

  lock;
  try
    nugap := TREgion.Create;
    try
      with nugap do begin
        nugap.Start := iStart;
        nugap.Length := iLength;
      end;
  //    if nugap.Beyond > Length then
  //      Length := nugap.Beyond;

      gap := FGaps.SearchLeft(nugap.start, nugap.Beyond, gcSpanning);
      if gap = nil then begin
        Debug.consolelog(GEtDebugGAps);
        raise ECritical.Create('Gap not found in which to place region: '+nugap.DebugString+' when length='+inttohex(self.Length,1));
      end;


      //if it consumes the entire gap, then just delete the gap
      if (gap.Start = nugap.Start) and (gap.Length = nugap.Length) then begin
        FGaps.Remove(gap);//frees item as well
      end else
      //if ONLY the start matches
      if (gap.Start = nugap.Start) then begin
        //the start of the existing gap needs to be moved forward
        iOldBeyond := gap.Beyond;
        gap.Start := nugap.Beyond;
        gap.Beyond := iOldBeyond;

      end else
      //if ONLY the end matches
      if gap.Beyond = nugap.Beyond then begin
        //the existing gap end needs to be moved backward
        gap.Beyond := nugap.Start;
      end
      //else, this must be in the middle of a gap region surrounded by gaps
      else begin
        //the gap has to be split into two gaps
        iOldBeyond := gap.Beyond;
        gap.Beyond := nugap.start;

        gap := TRegion.Create;
        gap.Start := nugap.Beyond;
        gap.Beyond := iOldBeyond;
        FGaps.Add(gap);
      end;
    finally
      nugap.Free;//the nugap isn't actually a gap, so we will never actualyl record it
      FGaps.CheckConsistency;
    end;
  finally
    unlock;
  end;





end;

destructor TGapAnalyzer.Destroy;
begin
  FGaps.ClearBruteForce;
  FGaps.free;
  FGaps := nil;
  inherited;


end;

function TGapAnalyzer.FindGapSpanning(start, minimum_size: int64): TGapInfo;
var
  res: TBTreeItem;
begin
  lock;
  try
    res := nil;
    result.Start := -1;
    result.length := 0;
    FGaps.Iterate(
      procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
      var
        tmp: TRegion;
      begin
        tmp := TRegion(abtreeitem);
        if (tmp.Start <= start) and  ((tmp.Start+tmp.Length) >= (start+minimum_size)) then begin
          ANeedSTop := true;
          res := abtreeitem;
        end;
      end
    );

    if res <> nil then begin
  //    Debug.ConsoleLog('Found gap was '+Tregion(res).debugstring);
      result.start := TRegion(res).Start;
      result.length := TRegion(res).Length;
    end;
  finally
    unlock;
  end;


end;

function TGapAnalyzer.FindGap(minimum_size: int64; less_than: int64;
  bMinimizeFragmentation: boolean): TGapInfo;
var
  res: TBTreeItem;
begin
  lock;
  try
    res := nil;
    result.Start := -1;
    result.length := 0;
    FGaps.Iterate(
      procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
      begin
        if less_than > 0 then begin
          if (TRegion(abtreeitem).Start < less_than) and  (TRegion(abtreeitem).Length >= minimum_size) then begin
            ANeedSTop := true;
            res := abtreeitem;
          end;

        end else
        if TRegion(abtreeitem).Length >= minimum_size then begin
          ANeedSTop := true;
          res := abtreeitem;

        end;
      end
    );

    if res <> nil then begin
      //Debug.ConsoleLog('Found gap was '+Tregion(res).debugstring);
      result.start := TRegion(res).Start;
      result.length := TRegion(res).Length;
    end;
  finally
    unlock;
  end;


end;

procedure TGapAnalyzer.FreeRegion(iStart, iLength: int64; bExtralogging: boolean = false);
var
  nugap: TRegion;
  gapleft, gapright: TRegion;
  nuEnd: int64;
begin
//  Debug.Log('FREE REGION');
  if iLength = 0 then
    exit;

  lock;
  try
    Fgaps.CheckConsistency;
    nugap := TRegion.Create;
    try
      nugap.Start := iStart;
      nugap.Length := iLength;
      if nugap.beyond > length then
        length := nugap.beyond;
      nuEnd := nugap.Beyond;

      repeat
        gapleft := FGaps.SearchLeft(nugap.start, nugap.beyond, gcOverlappingOrTouching);

        //if we're freeing a region that is inside of an already free region
        if gapleft <> nil then
          if (gapleft.beyond > nugap.beyond) then begin
            nuEnd := gapleft.beyond;
            if (gapleft.start <= nugap.start) then
              exit//forget about it..do nothing.
            else begin
              nugap.start := lesserof(gapleft.start, nugap.start);
              FGaps.Remove(gapleft);
            end;

          end else begin
            nugap.start := lesserof(gapleft.start, nugap.start);
            FGaps.Remove(gapleft);
          end;


      until gapleft = nil;

//      Debug.Log('FREE REGION.1');

      //while we find stuff on the right, delete them all
      //but remember the end of it all.

      repeat
//        Debug.Log('FREE REGION.2(loop)');
        gapright := FGaps.SearchRight(nugap.Start, nugap.Beyond, gcOverlappingOrTouching);
        if gapright <> nil then begin
          nuEnd := gapright.Beyond;
          fGaps.Remove(gapright);
          Fgaps.CheckConsistency;
        end;
      until gapright = nil;

{      repeat
        Debug.Log('FREE REGION.3(loop)');
        gapright := FGaps.FindRightLeftAny(nugap);
        if gapright <> nil then begin
          nuEnd := gapright.Beyond;
          if gapright <> gapleft then
            fGaps.Remove(gapright)
          else
            gapright := nil;
          Fgaps.CheckConsistency;
        end;
      until gapright = nil;}



      //if we found something on the left, then just modify
      //the existing gap
      if gapleft <> nil then begin
//        Debug.Log('FREE REGION.4a');
        gapleft.Beyond := nuEnd;
        Fgaps.CheckConsistency;
      end
      else begin
//        Debug.Log('FREE REGION.4b');
  //      if FGaps.lastitem <> FGaps.search(FLength-1) then
  //        FreeRegion(iStart, iLength, true);
  //      Debug.ConsoleLog(GEtDebugGaps);
        nuGap.Beyond := nuEnd;
        FGaps.Add(nugap);
  //      Debug.ConsoleLog(GEtDebugGAps);
        Fgaps.CheckConsistency;
        nugap := nil;//set nugap to nil so that it isn't freed later.
      end;

    finally
      nugap.Free;
      Fgaps.CheckConsistency;
    end;
  finally
    unlock;
  end;


end;



function TGapAnalyzer.GEtDebugConsumed: string;
begin
  result := '';
end;

function TGapAnalyzer.GEtDebugGAps: string;
begin
  lock;
  try
    result := 'Gaps:'+NEWLINE+Self.GEtDebugSTringForTree(FGAps);
  finally
    unlock;
  end;

end;

function TGapAnalyzer.GEtDebugSTringForTree(tree: TBTree): string;
var
  sl: TStringlist;
begin
  sl := TStringlist.create;
  try
    tree.Iterate(
      procedure ([unsafe] itm: TBtreeItem)//; anonymous
      begin
        sl.add(TRegion(itm).DebugString);
      end


    );
    result := sl.text;
  finally
    sl.free;
  end;

end;

procedure TGapAnalyzer.Init;
begin
  inherited;
  FGaps := TGABtree.create;
end;

procedure TGapAnalyzer.Iterate(AProcedure: TIterateRegionsProcedure);
begin
  Lock;
  try
    FGaps.Iterate(
      procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
      begin
        AProcedure(TRegion(ABTreeItem), ANeedStop);
      end
    );

  finally
    Unlock;
  end;
end;

procedure TGapAnalyzer.SetGapCalcMode(const Value: TGapCalcMode);
begin
  if value = gcmOverlapAllowed then
    raise ECritical.create('gcmOverlapAllowed not implemented');
  FGapCalcMode := Value;

end;

procedure TGapAnalyzer.SetLength(const Value: int64);
var
  last: TRegion;
  strt, len: int64;
begin
  lock;
  try

    if value = FLength then exit;

    if value < FLength then begin
      //find final gap
      FreeREgion(value, FLength-value);
      last := FGaps.SearchLeft(FLength-1, FLength,gcOverlappingOrTouching);

  //    if last <> FGaps.lastitem then
  //      raise ECritical.create('wtf'+last.debugstring+','+Tregion(fgaps.lastitem).debugstring);
      //final gap must start before new length or error
      if last = nil then begin

        Debug.Log(self, self.GEtDebugConsumed);
        Debug.Log(self, self.GEtDebugGAps);
        Debug.Log(self, '**GAP crash!!**');
        Debug.Log(self, 'will see what happens when I force-free the region.');
        FreeREgion(value, FLength-value);
        last := FGaps.SearchLEft(FLength-1, FLength,gcOverlappingOrTouching);
        //raise ECritical.create('There is no gap at the end of file.  Cannot reduce size from '+inttohex(self.length,1)+' to '+inttohex(value, 1));
      end;

      if last.start > value then begin
        Debug.Log(self, self.GEtDebugConsumed);
        Debug.Log(self, self.GEtDebugGAps);
        Debug.Log(self, '**GAP CRASH!!**');
        raise ECritical.create('Gap at end of file '+last.debugstring+' is not large enough to reduce size to '+inttohex(value,1)+' debug consumed:'+self.GEtDebugConsumed+NEWLINE+self.GEtDebugGAps);
      end;

      FLength := value;
      if value = last.start then
        FGaps.Remove(last)
      else
        last.Beyond := value;

    end else begin
      len := value-FLength;
      strt := FLength;
      Flength := value;
      FreeRegion(strt, len);


    end;
  finally
    unlock;
  end;


end;

function TGapAnalyzer.Sum: int64;
var
  res: int64;
begin
  lock;
  try
    res := 0;
    FGaps.Iterate(
      procedure([unsafe] ABTreeItem:TBTreeItem; var ANeedStop:boolean)
      var
        tmp: TRegion;
      begin
        tmp := TRegion(abtreeitem);
        inc(res, tmp.Beyond-tmp.Start);
      end
    );

    exit(res);
  finally
    unlock;
  end;

end;

function TGapAnalyzer.TryCombineLeft(r: TRegion; tree: TGABTree): TRegion;
//NOTE! this is kinda wierd.   The result will be
//the TRegion that was REPLACED by the combination.
//since it was replaced, it will no longer point to a valid object
//so you can use it to determine if you should throw away the pointers to it.
var
  lstart, llen: int64;
  left: TRegion;
begin
  result := nil;
  left := tree.SearchLeft(r.Start, r.Beyond, gcOverlappingOrTouching);
  if left <> nil then begin
    result := left;
    lstart := result.Start;
    llen := result.Length;
    tree.Remove(result);
    r.Start := lstart;
    r.Length := r.Length + llen;
  end;


end;

function TGapAnalyzer.TryCombineRight(r: TRegion; tree: TBTree): TRegion;
//NOTE! this is kinda wierd.   The result will be
//the TRegion that was REPLACED by the combination.
//since it was replaced, it will no longer point to a valid object
//so you can use it to determine if you should throw away the pointers to it.
var
  lstart, llen: int64;
begin
  result := nil;
  if r.IsTangentRight then begin
    //lstart := TRegion(r.FRightNode).Start;
    result := TRegion(r.FRightNode);
    llen := result.Length;

    tree.Remove(result);
    //r.Start := lstart;
    r.Length := r.Length + llen;
  end;

end;

{ TRegion }

function TRegion.Compare(const [unsafe] ACompareTo: TBTreeItem): ni;
var
  a,b: TRegion;
begin
  a := self;
  b := TREgion(ACompareTo);
  if self.Start < b.Start then
    result := 1
  else
  if self.Start >= b.Beyond then
    result := -1
  else
    result := 0;
end;

procedure TRegion.Copy(const [unsafe] ACopyTo: TBTreeItem);
var
  b: TRegion;
begin
  inherited;
{$IFNDEF NO_CAST_CHECKING}
  if not (ACopyTo is TRegion) then
    raise ECritical.create(ACopyTo.ClassName + ' is not TRegion');
{$ENDIF}
  b := TRegion(ACopyTo);
  b.Start := self.start;
  b.Length := self.length;



end;

function TRegion.DebugString: string;
begin
  result := '['+inttohex(Start,1)+'..'+inttohex(Ending,1)+']';

end;

function TRegion.GetBeyond: int64;
begin
  result := Start + Length;
end;

function TRegion.GetEnding: int64;
begin
  result := Beyond-1;
end;


function TRegion.IsTangentRight: boolean;
begin
  result := self.FRightNode <> nil;
  if result then begin
    result := TRegion(self.FRightNode).Start = self.Beyond;
  end;
end;

procedure TRegion.SetBeyond(const Value: int64);
begin
  Length := value-Start;
end;

procedure TRegion.SetEnding(const Value: int64);
begin
  Beyond := value + 1;
end;

{ TGABTree }

procedure TGABTree.CheckConsistency;
var
  prev: TRegion;
begin
  exit;
  prev := nil;

  Iterate(
    procedure([unsafe] ABTreeItem:TBTreeItem)
    var
      tmp: TRegion;
    begin
      tmp := TRegion(abtreeitem);
      if prev <> nil then
        if tmp.start < prev.beyond then
          raise ECritical.create('consistency check fail');
      prev := tmp;
    end
  );
end;




function TGABTree.MeetsSpanningRules(n: TRegion; const iSpanAddress,
  iBEyondAddress: int64; gc: TGapContainMent): boolean;
begin
  if n = nil then
    exit(true);

  //now that we have a result to the right
  //it is only valid if overlapping?
  if n <> nil then begin
    case gc of
      //no rules, duh
      gcNoRules: begin
        exit(true);
      end;
      //no parts of found gap are allowed to cross the region we're searching
      gcExternal: begin
        exit((n.Ending < iSpanAddress) or (n.Start >= iBeyondAddress));
      end;
      //found gap must (at least) partially overlap region we're searching
      gcOverlappingOrTouching: begin
        if iSpanAddress > n.Beyond then exit(false);
        if n.Start > iBeyondAddress then exit(false);
        exit(true);
      end;
      //found gap must completely curround the region we're saerching (or be identical)
      gcSpanning: begin
        exit((n.Start <= iSpanAddress) and (n.Beyond >= iBeyondAddress));
      end;
      //found gap must be exactly adjacent to the region we're searching for, but not overlapping
      gcTouching: begin
        exit((n.Beyond = iSpanAddress) or (n.Start = iBeyondAddress));
      end;
      //found gap must be completely inside the region we're searching for
      gcWithin: begin
        if iBeyondAddress < n.Beyond then exit(false);
        if n.Start > iSpanAddress then exit(false);

      end;

    end;
  end;

  exit(true);
end;

function TGABTree.SearchLeft(const iSpanAddress: int64; const iBeyondAddress: int64; gc: TGapContainment): TRegion;
var
  n: TRegion;
begin
//  Debug.Log('Search Left');
  result := nil;
  //look for the highest address in a gap to the left
  n := TRegion(FRoot);
  while n <> nil do begin
    //any node that is less than iBeyondAdress MIGHT be our answer, but we must continue
    //searching RIGHT to verify this
    if (n.Start < iBeyondAddress) then begin
      result := n;
      n := TRegion(n.FRightNode);
//      Debug.Log('--move right');
    end else begin
      //ELSE keep looking left as long as result is nil
      n := TRegion(n.FLEftNode);
//      Debug.Log('--move left');
    end;
  end;


  if not MeetsSpanningRules(result, iSpanAddress, iBeyondAddress, gc) then
    result := nil;



end;

function TGABTree.SearchRight(const iSpanAddress: int64; const iBeyondAddress: int64; gc: TGapContainment): TRegion;
var
  n: TRegion;
begin
//  Debug.Log('Search Right');
  result := nil;
  n := TRegion(FRoot);
  while n <> nil do begin
//    if (n.Start >= iSpanAddress) and ((bIgnoreBeyond or (n.Beyond > iSpanAddress))) then begin
//      result := n;
//      n := TRegion(n.FLeftNode);
//    end else
    //this node is greater than target.. remember as the potential result
    //but keep searching for closer nodes
    if (n.start > iSpanAddress) then begin
      result := n ;
      n := TRegion(n.FLeftNode);
//      Debug.Log('--Move Left');

    end else
    //look right as long as result wasn't found
    begin
      if result <> nil then
        break;
      n := TRegion(n.FRightNode);
//      Debug.Log('--Move Right');

    end;
  end;

  if not MeetsSpanningRules(result, iSpanAddress, iBeyondAddress, gc) then begin
    result := nil;
  end;


end;



procedure TGABTree.TryCombineAll;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

end.
