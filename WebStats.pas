unit WebStats;

interface

uses Classes, Windows, betterobject, SharedObject;

type
  TWebStats = class; //forward declaration

  TWebStat = class(TSharedObject)
    //This class represents the INDIVIUAL STATISTIC. Multiple copies of this
    //class would exist in the TWebStats class.  THIS CLASS IS NOT THREAD SAFE.
    //REad it only when TWebSTats is locked, and DO NOT reference it outside of
    //the time when TWebStats is unlocked.
  private
    FAverage: double;
    FValue: integer;
    FWeight: integer;
    FStatName: ansistring;
    FCategory: ansistring;
    FDisabled: boolean;
    FOwner: TWebStats;
    procedure SetValue(const iValue: integer);
    function GetCategory: ansistring;
    function GetStatName: ansistring;
    procedure SetCategory(const Value: ansistring);
    procedure SetStatName(const Value: ansistring);
  public
    constructor create(AOwner: TWebStats); reintroduce;

    property Category: ansistring read GetCategory write SetCategory;
    property StatName: ansistring read GetStatName write SetStatName;
    property Value: integer read FValue write SetValue;
      //The most recent value written to the statistic
    property Average: double read FAverage;
      //The average of all values sent to this statistic
    property Weight: integer read FWeight;
      //Number of updates that have been made to this statistic

    property Owner: TWebStats read FOwner write FOwner;

    property Disabled: boolean read FDisabled;

    procedure Reset;

    procedure Disable;
    procedure Enable;

  end;



  //----------------------------------------------------------------------------
  TWebStats = class(TLockQueuedObject)
  private
    lstStats: TList;
    FDisabled: boolean;
    function GetStats(idx: integer): TWebStat;
    function GetStatCount: integer;

    procedure SortStats;

  public
    constructor Create; override;
    destructor Destroy; override;

    //THESE ROUTINES ARE NOT THREAD SAFE.... CALL LOCK BEFORE CALLING
    property Stats[idx: integer]: TWebStat read GetStats;
    property StatCount: integer read GetStatCount;
    function GetStatIndex(sCategory, sStatName: ansistring): integer;

    //THESE ROUTINES ARE THREAD SAFE
    procedure AddStat(sCategory: ansistring; sStatName: ansistring; iValue: int64); overload;
    procedure AddStat(sCategory: ansistring; sStatName: ansistring; iValue: integer); overload;


    procedure Clear;

    procedure Disable;
    procedure Enable;
    property Disabled: boolean read FDisabled;


  end;

var
  WebServerStats: TWebStats;

implementation

procedure TWebStat.SetValue(const iValue: integer);
//Setter for Value property
begin
  //If the statistic was turned off (usually for the purposes of reading the
  //statistic VIA a WEBPAGE -- the generation of statistics would step on the
  //reading of statistics) THEN JUST EXIT
  if Disabled then
    exit;

  //Increment the weight of the statistic (for averaging purposes);

  inc(FWeight);

  //New value is weighted
  // New Value carries the weight of 1/TotalWeight
  // Previous average value carry the weight of (TotalWeight-1)/TotalWeight
  FAverage := (iValue+(fWeight-1)*FAverage)/fWeight;


  //Set the new value into the field LAST
  FValue := iValue;

end;
//------------------------------------------------------------------------------
function TWebStats.GetStatCount: integer;
//NOTE: NOT THREAD SAFE... CALL LOCK BEFORE CALLING GetStatCount
begin
  result := lstStats.count;
end;

//------------------------------------------------------------------------------
procedure TWebStats.AddStat(sCategory, sStatName: ansistring; iValue: integer);
//NOTE: This routine is thread safe because the overloaded AddStat routine is THREAD
//SAFE
var
  iBIG: int64;
begin
//  exit;
  iBIG := iValue;
  AddStat(sCategory, sStatName, iBIG); //Add stat is thread safe
end;
//------------------------------------------------------------------------------
procedure TWebStats.AddStat(sCategory: ansistring; sStatName: ansistring; iValue: int64);
var
  iIndex: integer;
  stat: TWebStat;

begin
//  exit;
  LockWrite;
  try
    iIndex := GetStatIndex(sCategory, sStatName);

    //if stat deesn't exists then create the stat
    if iIndex = -1 then begin
      //create the class
      stat := TWebStat.create(self);
      //Add the class to the list
      lstStats.add(stat);

      //Set properties of stat
      stat.Category := sCategory;
      stat.StatName := sStatName;
      stat.Value := iValue;

    end
    //else update the stat
    else begin
      stats[iIndex].Value := iValue;
    end;

    //SortStats;

  finally
    UnlockWrite;
  end;
end;

//------------------------------------------------------------------------------
function TWebStats.GetStatIndex(sCategory, sStatName: ansistring): integer;
var
  t: integer;
begin
  result:= -1;
  for t:= 0 to StatCount-1 do begin
    if (Stats[t].Category = sCategory) and (Stats[t].StatName = sStatName) then begin
      result := t;
      break;
    end;
  end;

end;

//------------------------------------------------------------------------------
procedure TWebStats.SortStats;
//Uses a very primitive method to sort the statics information.
//not very optimal... but adequate and easy to understand;
var
  bSweep: boolean; //indicates whether there we're no changes made in a articular pass
  t: integer;
  ptrTemp: Pointer;
begin

  bSweep := false;
  WHILE NOT bSweep DO BEGIN

    //Start with bSweep as TRUE (will be marked false at first reordering)
    bSweep := true;

    for t:= 0 to StatCount -2 do begin
      //if out of order
      if Stats[t].Category > Stats[t+1].Category then begin
        //swap stat at T with stat at T+1
        ptrTemp := lstStats[t];
        lstStats[t] := lstStats[t+1];
        lstStats[t+1] := ptrTemp;
        //Mark sort as incomplete
        bSweep := false;
      end;
    end;
  END;

end;

//------------------------------------------------------------------------------
constructor TWebStats.Create;
begin
  inherited Create;
  lstStats := TList.create;
  Fdisabled := false;
end;

//------------------------------------------------------------------------------
destructor TWebStats.Destroy;
var
  t: integer;
begin

  //free statistic objects
  for t:= lstStats.count-1 downto 0 do begin
    TWebStat(lstStats[t]).free;
  end;

  //free the rest
  lstStats.free;
  inherited;
end;

//------------------------------------------------------------------------------
function TWebStats.GetStats(idx: integer): TWebStat;
begin
  result := TWebStat(lstStats[idx]);
end;


//------------------------------------------------------------------------------
constructor TWebStat.create(AOwner: TWebStats);
begin
  inherited create;
  //Init descriptors
  FStatName := '';
  FCategory := '';
  FDisabled := false;
  FOwner := AOwner;

  //Reset -- initializes values of:
  //  FAverage: double;
  //  FValue: integer;
  //  FWeight: integer;
  Reset;
end;

//------------------------------------------------------------------------------
procedure TWebStat.Reset;
begin
  FAverage := 0.0;
  FValue := 0;
  FWeight := 0;

end;

//------------------------------------------------------------------------------
procedure TWebStat.Disable;
begin
  FDisabled := true;
end;

//------------------------------------------------------------------------------
procedure TWebStat.Enable;
begin
  FDisabled := False;
end;

//------------------------------------------------------------------------------
procedure TWebStats.Disable;
begin
  FDisabled := true;
end;
//------------------------------------------------------------------------------
procedure TWebStats.Enable;
begin
  Fdisabled := false;
end;

procedure TWebStats.Clear;
var
  t: integer;
begin
  LockWrite;
  try
    //free statistic objects
    for t:= lstStats.count-1 downto 0 do begin
      TWebStat(lstStats[t]).free;
      lstStats.delete(t);
    end;
  finally
    UnlockWrite;
  end;

end;

function TWebStat.GetCategory: ansistring;
begin
  Lock;
  try
    result := FCategory;
    UniqueSTring(result);
  finally
    Unlock;
  end;
end;

function TWebStat.GetStatName: ansistring;
begin
  Lock;
  try
    result := FstatName;
    uniqueString(result);
  finally
    Unlock;
  end;

end;

procedure TWebStat.SetCategory(const Value: ansistring);
begin
  Lock;
  try
    FCategory := value;
    UniqueString(FCategory);
  finally
    Unlock;
  end;
end;

procedure TWebStat.SetStatName(const Value: ansistring);
begin
  Lock;
  try
    FStatName := value;
    UniqueString(FCategory);
  finally
    Unlock;
  end;
end;

end.
