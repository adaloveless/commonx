unit Search;

interface

uses
  typex;


type
  TBinarySearchOp = (bsoTest, bsoResult, bsoNoResult);
  TBinarySearchFunctionEval = function(test: int64; data: pointer; op: TBinarySearchOp): nativeint;
  TBinarySearchFunctionEvalOfObject = function(test: int64; op: TBinarySearchOp): int64 of object;

function BinarySearch(func: TBinarySearchFunctionEVal; data: pointer; out res: TBinarySearchOp): nativeint;overload;
function BinarySearch(func: TBinarySearchFunctionEValOfObject; out res: TBinarySearchOp): int64;overload;


implementation

function BinarySearch(func: TBinarySearchFunctionEVal; data: pointer; out res: TBinarySearchOp): nativeint;
var
  ipos: nativeint;
  test: int64;
  iTemp: nativeint;
begin
  iPos := 63;
  result := 0;
  res := bsoNoResult;

  for iPos := 63 downto 0 do begin
    test :=   result or (1 shl iPos);
    iTemp := func(test,data, bsoTest);
    //if the function reports that the value is LOW then
    if iTemp < 0 then
      //put a 1 in the result position
      result := result or test;

    //if we're dead on, break
    if iTemp = 0 then begin
      func(test, data, bsoResult);
      result := test;
      res := bsoResult;
      exit;
    end;
  end;

  //no rsult
  result := -1;
  func(test, data, bsoNoResult);








end;

function BinarySearch(func: TBinarySearchFunctionEValOfObject; out res: TBinarySearchOp): int64;
var
  ipos: nativeint;
  test: int64;
  iTemp: nativeint;
begin
  iPos := 63;
  result := 0;
  res := bsoNoResult;

  for iPos := 62 downto -1 do begin
    if iPos < 0 then
      test := 0
    else
      test :=   result or ((int64(1) shl int64(iPos)));
    iTemp := func(test, bsoTest);
    //if the function reports that the value is LOW then
    if iTemp < 0 then
      //put a 1 in the result position
      result := result or test;

    //if we're dead on, break
    if iTemp = 0 then begin
      func(test, bsoResult);
      res := bsoResult;
      result := test;
      exit;
    end;
  end;

  //no result
  result := -1;
  func(test, bsoNoResult);


end;


end.
