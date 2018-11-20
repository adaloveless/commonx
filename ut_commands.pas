unit ut_commands;
{$D+}
{$I 'DelphiDefs.inc'}

interface


uses typex, commandprocessor, stringx, stringx.ansi, classes, sysutils, commandtypes, tickcount, systemx;

type
  Tcmd_RandomStringSplit = class(TFunctionCommand<ansistring>)
  public
    procedure DoExecute;override;
    procedure InitExpense;override;
  end;

  Tcmd_RandomAllocations = class(TFunctionCommand<ansistring>)
  private
    FCount: int64;
  public
    procedure Init;
    property Count: int64 read FCount write FCount;
    procedure DoExecute;override;
    procedure InitExpense;override;
  end;

  Tcmd_NthPrime = class(TFunctionCommand<integer>)
  private
    FNth: int64;
//    FResult: integer;
  public
    property Nth: int64 read FNth write FNth;
    procedure DoExecute;override;
    function IsPrime(i: int64): boolean;
//    property Result: integer read FResult write FResult;
  end;

  TCmd_IsPrimeMP = class(TfunctionCommand<integer>)
  private
    FSubject: integer;
  public
    property Subject: integer read FSubject write FSubject;
    procedure DoExecute;override;
  end;

  TCmd_IsFactorOf = class(TfunctionCommand<boolean>)
  private
    FSubject: int64;
  public
    property Subject: int64 read FSubject write FSubject;
    property Divisor: int64 read FSubject write Fsubject;
    procedure DoExecute;override;
  end;

  Tcmd_IsPrime = class(TFunctionCommand<boolean>)
  private
    FSubject: int64;
//    FResult: boolean;
  public
    property Subject: int64 read FSubject write FSubject;
    procedure DoExecute;override;
//    property Result: boolean read FResult write FResult;
  end;

implementation

{ TPrimeCommand }
uses
  unittest;

type
  TUT_Commands = class(TUnitTest)
  public
    procedure DoExecute;override;
  end;


procedure Tcmd_NthPrime.DoExecute;
var
  t: int64;
  iSubject: int64;
  iPrimesFound: int64;

begin
  inherited;
  self.StepCount := Nth;

  iSubject := 999999999;
  iPrimesFound := 0;
  repeat
    inc(iSubject);
    if IsPrime(iSubject) then
      inc(iPrimesFound);




  until not (iPrimesFound < Nth);
  result := iSubject;

end;

{ Tcmd_IsPrime }

procedure Tcmd_IsPrime.DoExecute;
var
  t: int64;
begin
  inherited;

  //sleep(random(3));
  t := 2;
  while (t < (self.Subject div 2)) do begin
    if (self.Subject mod t) = 0 then begin
      result := false;
      exit;
    end;
    inc(t);
  end;

  result := true;


end;

function Tcmd_NthPrime.IsPrime(i: int64): boolean;
var
  c: Tcmd_IsPrime;
begin
  c := Tcmd_isPrime.create();
  try
    c.Subject := i;
    self.ProcessLater;
    c.Start;
    c.WaitFor;
    result := c.result;

  finally
    c.free;
  end;

end;
(*var
  t: integer;
begin
  inherited;

  for t:= 2 to i div 2 do begin
    if (i mod t) = 0 then begin
      result := false;
      exit;
    end;
  end;

  result := true;


end;*)

{ TCmd_NthPrimeMP }

procedure TCmd_IsPrimeMP.DoExecute;
var
  c: Tcmd_IsFactorOf;
  t: integer;
begin
  inherited;
  //create all commands
  for t:= 1 to self.Subject div 2 do begin
    c := Tcmd_IsFactorOf.create();
    c.Subject := self.Subject;
    c.Divisor := t;
    c.Start;


  end;

end;

{ TCmd_IsFactorOf }

procedure TCmd_IsFactorOf.DoExecute;
begin
  inherited;
  result := (self.Subject mod self.Divisor) = 0;
end;

{ Tcmd_RandomStringSplit }

procedure Tcmd_RandomStringSplit.DoExecute;
var
  s: ansistring;
  t: integer;
  s1, s2, s3: ansistring;
  sl: TStringlist;
  ac: ansichar;
begin

  inherited;
  sl := Tstringlist.create;

  setlength(s, 1000000);

  for t:= 1 to length(s) do begin
    {$IFNDEF WINDOWS}
      ac.FromOrd($50+random(72));
      s.chars[t] := ac;
      //s[t] := ansichar(ord('a')+random(72));
    {$ELSE}
      s[t] := ansichar(ord('a')+random(72));
    {$ENDIF}
  end;


  s3 := s;
  stepcount := length(s);
  while splitString(s3, 'a', s1,s3) do begin
    //step := length(s)-length(s3);
    sl.Add(s1);

  end;
  result := sl.Text;
  sl.Free;

end;

procedure Tcmd_RandomStringSplit.InitExpense;
begin
  CPuExpense := 0;
end;

{ Tcmd_RandomAllocations }

procedure Tcmd_RandomAllocations.DoExecute;
var
  l: TList;
  t: integer;
  p: pointer;
  r: integer;
  mm: TMemoryManager;
begin
  inherited;
  l := Tlist.create;
  try

    GetMemoryManager(mm);

    stepcount := 10000000;
    for t:= 0 to stepcount do begin
      step := t;
      r := random(100);

      if r > 50 then begin
        p := mm.GetMem(random(1024)+1);
//        OutputDebugString(pchar('Got '+inttohex(nativeint(p),16)));
        if p <> nil then
          l.add(p);
      end else begin
        r := random(l.count);
        if r> (l.count-1) then continue;
//        OutputDebugString(pchar('Freeing '+inttohex(nativeint(l[r]),16)));
        mm.FreeMem(l[r]);
        l.delete(r);
      end;
    end;

    while l.count > 0 do begin
      mm.FreeMem(l[0]);
      l.delete(0);
    end;

  finally
    l.free;
  end;

end;

procedure Tcmd_RandomAllocations.Init;
begin
  Count := 100000000;
end;

procedure Tcmd_RandomAllocations.InitExpense;
begin
  inherited;
  CPUExpense := 0;
end;




{ TUT_Commands }

procedure TUT_Commands.DoExecute;
const
  prime_subject = $7FFFF;
var
  cl: TWaitableCommandList;
  c1,c2: Tcmd_RandomAllocations;
  p1,p2: Tcmd_IsPrime;
  a,b: ticker;
  t: ni;
begin
  cl := TWaitableCommandList.create;
  try
    case variation of
      10,20: begin
        VariationName := 'Single Random Allocations';
        c1 := Tcmd_RandomAllocations.Create;
        a := GetTicker;
        c1.Start;
        c1.waitfor;
        b := GetTicker;
        c1.DetachAndFree;
        utresult := 'success}}time='+inttostr(GEtTimeSince(b,a));
      end;
      30: begin
        VariationName := '2 Random Allocations ('+inttostr(GetProcessorCount)+' cpus available)';
        a := GetTicker;
        c1 := Tcmd_RandomAllocations.Create;
        c1.Start;
        c2 := Tcmd_RandomAllocations.Create;
        c2.Start;
        c1.WaitFor;
        c2.WaitFor;

        b := GetTicker;
//        cl.ClearAndDestroyCommands;
        c1.DetachAndFree;
        c2.DetachAndFree;
        utresult := 'success}}time='+inttostr(GEtTimeSince(b,a));
      end;
      40: begin
        VariationName := 'IsPrime';
        p1 := Tcmd_IsPrime.Create;
        a := GetTicker;
        p1.Subject := prime_subject;
        p1.Start;
        p1.waitfor;
        b := GetTicker;
        p1.DetachAndFree;
        utresult := 'success}}time='+inttostr(GEtTimeSince(b,a));
      end;
      50: begin
        VariationName := '2 IsPrime ('+inttostr(GetProcessorCount)+' cpus available)';
        a := GetTicker;
        p1 := Tcmd_IsPrime.Create;
        p1.Subject := prime_subject;
        p1.Start;
        p2 := Tcmd_IsPrime.Create;
        p2.Subject := prime_subject;
        p2.Start;
        p1.WaitFor;
        p2.WaitFor;

        b := GetTicker;
//        cl.ClearAndDestroyCommands;
        p1.DetachAndFree;
        p2.DetachAndFree;
        utresult := 'success}}time='+inttostr(GEtTimeSince(b,a));
      end;

    end;
  finally
    cl.ClearAndDestroyCommands;
    cl.Free;
    cl := nil;
  end;
end;

initialization

UTF.RegisterClass(TUT_Commands);

end.
