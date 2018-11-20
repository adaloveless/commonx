unit commands_string;

//when we find a split
//

{x$DEFINE USE_MULTI_QUEUE}

//TODO - Optimize for strings of same length



interface

uses
  commandprocessor, typex, classes, generics.collections, sysutils, debug, numbers, simplequeue,
{$ifDEF USE_MULTI_QUEUE}
  globalmultiqueue,
{$endif}
  linked_list;
type
  Tcmd_ParseString = class(TCommand)
  private
    FOutput: TStringlist;
    FInput: string;
    FDelimiter: string;
    FDelimiterChar: char;
  public
    destructor Destroy;override;
    property Input: string read FInput write FInput;
    property Delimiter: string read FDelimiter write FDelimiter;
    property Output: TStringlist read FOutput;
    procedure DoExecute;override;
  end;

  Tcmd_StringReplace = class(TQueueItem)
  public
    result: string;
    sSource: string;
    sPAttern: string;
    sReplacement: string;
    flags: TReplaceFlags;
    procedure DoExecute; override;
  end;



  TStringLoadCommand = class(TCommand)
  private
    FFileName: string;
    FREsult: string;
  public
    property FileName: string read FFileName write FFileName;
    property Result: string read FREsult write FResult;
    procedure DoExecute;override;
  end;


procedure ParseStringEx(sInput: string; sDelimiter: string; slOutputList: TStringlist; iThreadSplitSize: ni = 1024);
function StringReplace_ComparativeSample(sSource: string; sPattern, sReplacement: string; flags: TReplaceFlags): string;
function StringReplace_Fastest(sSource: string; sPattern, sReplacement: string; flags: TReplaceFlags): string;
function StringReplace_MultiThreadedX(sSource: string; sPattern, sReplacement: string; flags: TReplaceFlags): string;


implementation

uses
  stringx;

function StringReplace_ComparativeSample(sSource: string; sPattern, sReplacement: string; flags: TReplaceFlags): string;
//this is a simpler reimplementation of string replace,
//intended to be used to verify that functions such as ParseStringEX and UnparseStringEx are
//properly implemented while comparing speed with StringReplace_Fastest
var
  sl: TStringlist;
begin
  sl := TStringList.create;
  try
    ParseStringEx(sSource,sPattern, sl,2000);
    result := UnParseString(sReplacement, sl);
  finally
    sl.free;
  end;


end;

function StringReplace_MultiThreadedX(sSource: string; sPattern, sReplacement: string; flags: TReplaceFlags): string;
var
  cl: TDirectlyLInkedLIst<Tcmd_StringReplace>;
  c: Tcmd_StringReplace;
  sWorkingChunk,s1,s2: string;
  bSplit: boolean;
  iStart: string;
  sl: TStringlist;
  t: ni;
  iSplits: ni;
  iPOs: ni;
  function MaxLen: ni;
  var
    x: ni;
  begin
    result := 0;
    for x:= 0 to sl.count-1 do begin
      result := greaterof(sl[x].length, result);
    end;
  end;
begin

  sl := TStringlist.create;
  try
    sl.add(sSource);

{$IFDEF USE_MULTI_QUEUE}
    cl := TDirectlyLInkedLIst<Tcmd_STringReplace>.create;
{$ENDIF}
    try
      //split into chunks of 200k or less
      while maxlen > 2000 do begin
        iSplits := 0;
        t := 0;
        while t < sl.count do begin
          if rfIgnorecase in flags then
            iPOs := ZFindSplitMidPoint(sPattern, sl[t],true)//ZPos(sPattern, sl[t], length(sl[t]) shr 1)
          else
            iPos := ZFindSplitMidPoint(sPattern, sl[t], false);//ZPos(sPattern, sl[t], length(sl[t]) shr 1)

          if iPOs >= 0 then begin
            s1 := zcopy(sl[t], 0, iPOs);
            s2 := zcopy(sl[t], iPos+length(sPattern), length(sl[t]));
            sl[t] := s1;
            sl.Insert(t+1,s2);
            inc(iSplits);
            inc(t);//extra increment
          end;
          inc(t);
        end;

        //if we couldn't split anything then we have no choice but to continue regardless of chunk size
        if iSplits = 0 then
          break;
      end;

      //create commands for the chunks
      for t:= 0 to sl.Count-1 do begin

{$IFDEF USE_MULTI_QUEUE}
        c := Tcmd_StringReplace.Create;
        c.sSource := sl[t];
        c.sPAttern := sPattern;
        c.sReplacement := sReplacement;
        c.flags := flags;
        cl.Add(c);
        gmq.additem(c);
{$ELSE}
        sl[t] := stringreplace(sl[t], sPattern, sReplacement, flags);
{$ENDIF}
        //c.Execute;
        //c.Start;
      end;

      //reassemble
{$IFDEF USE_MULTI_QUEUE}
      sl.clear;
      c := cl.First;
      while c <> nil do begin
        c.waitfor;
        sl.add(c.result);
        c := Tcmd_StringReplace(c.Next);
      end;
{$ENDIF}

      result := UnparseString(sReplacement, sl);
{$IFDEF USE_MULTI_QUEUE}
      while true do begin
        c := cl.first;
        if c <> nil then begin
          cl.remove(c);
          c.free;
        end else
          break;
     end;
      cl.Clear;
{$ENDIF}



    finally
{$IFDEF USE_MULTI_QUEUE}
      cl.free;
{$ENDIF}
    end;
  finally
    sl.free;
  end;

end;


{ TStringLoadCommand }


procedure TStringLoadCommand.DoExecute;
begin
  inherited;
  FResult := LoadStringFromFile(FileName);
end;


{ Tcmd_ParseString }

destructor Tcmd_ParseString.Destroy;
begin
  FOutput.Free;
  inherited;
end;

procedure Tcmd_ParseString.DoExecute;
begin
  inherited;
  FOutput := TStringlist.Create;
//  Debug.ConsoleLog('Starting Parse string command with length '+inttostr(length(input)));
  if length(FDelimiter)= 1 then
    ParseString(Input, FDelimiter[STRZ], Output)
  else
    ParseString(Input, Delimiter, Output);
//  Debug.ConsoleLog('Finished Parse string command with length '+inttostr(length(input)));
  Input := '';
end;


procedure ParseStringEx(sInput: string; sDelimiter: string; slOutputList: TStringlist; iThreadSplitSize: ni);
var
  c: Tcmd_ParseString;
  cl: TList<TCmd_ParseString>;
  s1, s2: string;
  iSplitAt: ni;
  t: ni;
  s3,s4: string;
  bStop: boolean;
begin
{$DEFINE PARSE_STRING_EX_BROKEN}
{$IFDEF PARSE_STRING_EX_BROKEN}
  ParseString(sInput, sDelimiter, slOutputLIst);
  exit;
{$ELSE}

  bStop := false;
  if length(sInput) < iThreadSplitSize then begin
    ParseString(sInput, sDelimiter, slOutputList);
    exit;
  end;
  cl := TList<Tcmd_ParseString>.create;
  try
    //create a single command
    c := Tcmd_ParseString.create;
    c.INput := sInput;
    c.Delimiter := sDelimiter;
    cl.add(c);

    while length(cl[0].INput) > iThreadSplitSize do begin
      t := 0;
      while t < cl.count do begin
        c := cl[t];
        iSplitAt := length(c.Input) div 2;
        s1 := zcopy(c.Input, 0, iSplitAt);
        s2 := zcopy(c.Input, iSplitAt, length(c.Input));
        if not SplitString(s2, sDelimiter, s3,s4) then begin
          bStop := true;
          break;
        end;
        s1 := s1 + s3;
        s2 := s4;
        c.Input := s1;
        c := Tcmd_ParseString.create;
        c.Input := s2;
        c.Delimiter := sDelimiter;
        cl.Insert(t+1, c);
        inc(t,2);
      end;
      if bStop then break;
    end;

    for t:= 0 to cl.count-1 do begin
      cl[t].start;
    end;

    for t:= 0 to cl.count-1 do begin
      c := cl[t];
      c.waitfor;
      slOutputList.AddStrings(c.Output);
      c.free;
      c := nil;
    end;
  finally
    cl.free;
  end;

{$ENDIF}
end;



{ Tcmd_StringReplace }

procedure Tcmd_StringReplace.DoExecute;
begin
  inherited;
  result := StringReplace(sSource, sPattern, sReplacement, flags);
end;

function StringReplace_Fastest(sSource: string; sPattern, sReplacement: string; flags: TReplaceFlags): string;
begin
  Result := StringReplace_MultiThreadedX(sSource, sPattern, sReplacement, flags);
end;

end.
