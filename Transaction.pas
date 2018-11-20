unit Transaction;

interface

uses
  typex, stringx, systemx, commandprocessor, sysutils, debug;

//what this is:
// - this is an abstract transactional model.  As it is, it does nothing
//   but setup the scaffolding you'll need to build your own transactional
//   models.

//How derives classes will be used:
// - The user will create an instance of the command and set some initial states
// - The user will either set the number of TriesBeforeFail or leave it as default.  The default (unless overridden) is 0 = Retry infinitely or until exception
// - The user will call TCommand.Start();
// - The user will call TCommand.WaitFor;
// - The user will check the results (if any)
// - The user will destroy the command

//How this works:
//-- DoExecute() is pre-programmed to call a few of the abstract scaffolding
//   procedures to setup and commit transactions.  You probably don't want to
//   override DoExecute (as you would normally do for CommandDesrivatives)
//-- It is important to note that the critical state will only
//   normally be locked during the Setup and Commit parts of the transaction
//-- The Perform() method will operate WITHOUT LOCKS (LockState,UnlockState)
//   Collisions are sometimes expected in this model, at which point the
//   transaction will fail or retry.
//-- In order to control the retry, you must override
//   > GatherStartState to get the initial state
//   > ReevalAgainstStartState to check whether there was a collision
//

type
  TTransactionResult = (trCommitted, trCollision, trNoCollision, trFailed, trPerformed, trCancelled);

  TTransactionalCommand = class(TCommand)
  private
    FTriesBeforeFail: ni;
    FCResult: TTransactionResult;
    FTries: ni;
    Fretryonexception: boolean;
    procedure SetCResult(const Value: TTransactionResult);inline;
  protected
    function LockState: boolean;virtual;//<<<-----------OVERRIDE ME!
    procedure UnlockState;virtual;//<<<-----------OVERRIDE ME!
    procedure DoCommit;virtual;//<<<-----------OVERRIDE ME!
    procedure DoRollback;virtual;//<<<-----------OVERRIDE ME!
    procedure GatherStartSTate;virtual;//<<<-----------OVERRIDE ME!
    function Perform: TTransactionResult; virtual;//<<<-----------OVERRIDE ME!
    function ReEvalAgainstStartState: TTransactionResult;virtual;//<<<-----------OVERRIDE ME!
    function CanCommit: boolean;virtual;//<<<-----------OVERRIDE ME!

    property Tries: ni read FTries write FTries;

  public
    procedure BeginTransaction;
    function CommitTransaction: boolean;
    procedure DoExecute;override;
    property CResult: TTransactionResult read FCResult write SetCResult;
    property TriesBeforeFail: ni read FTriesBeforeFail write FTriesBeforeFail;
    property RetryOnPerformException: boolean read Fretryonexception write FRetryonException;
  end;

implementation

{ TTransactionalCommand }

procedure TTransactionalCommand.BeginTransaction;
begin
  if LockState then
  try
    GatherStartState;
  finally
    UnlockState;
  end;
end;

function TTransactionalCommand.CanCommit: boolean;
begin
  result := ReEvalAgainstStartState = trNoCollision;
end;

function TTransactionalCommand.CommitTransaction: boolean;
begin
  if LockState then
  try
    if CanCommit and (Cresult in [trPErformed, trNoCollision]) then begin
      DoCommit;
      exit(true);
    end else begin
      DoRollback;
      exit(false);
    end;
  finally
    UnlockState;
  end;
  exit(false);

end;

procedure TTransactionalCommand.DoCommit;
begin
  //
end;

procedure TTransactionalCommand.DoExecute;
begin
  inherited;

  Cresult := trFailed;
  try
    while (CResult <> trCommitted) and (CResult <> trCancelled)and ((TriesBeforeFail = 0) or (Tries < TriesBeforeFail)) do begin
      if isCancelled then begin
        CResult := trCancelled;
        exit;
      end;

      BeginTransaction;
      try
        CResult := Perform;
      except
        if not RetryOnPerformException then
          raise
        else
          CResult := trFailed;
      end;
      if CommitTransaction then
        CResult := trCommitted
      else
        CResult := trCollision;

      inc(FTries);
      if ((Tries > TriesBeforeFail) and (TriesBeforeFail > 0)) then begin
        DEbug.Log('TRANSACTION FAILED! '+self.classname);
      end;
    end;
  except
    on E: Exception do begin
      Status := e.message;
      CResult := trFailed;
    end;
  end;



end;

procedure TTransactionalCommand.DoRollback;
begin
  //
end;

procedure TTransactionalCommand.GatherStartSTate;
begin
  //
end;

function TTransactionalCommand.LockState: boolean;
begin
  //
  result := true;
end;

function TTransactionalCommand.Perform: TTransactionResult;
begin
  //
  result := trPerformed;
end;

function TTransactionalCommand.ReEvalAgainstStartState: TTransactionResult;
begin
  //
  result := trNoCollision;
end;

procedure TTransactionalCommand.SetCResult(const Value: TTransactionResult);
begin
  FCResult := Value;
end;

procedure TTransactionalCommand.UnlockState;
begin
  //
end;

end.
