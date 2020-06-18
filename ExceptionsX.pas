unit ExceptionsX;
//This unit defines custom defined exeptions for PWLN.

interface

uses SysUtils;

const
  sErrPostFailure = 'Failure posting %s.  Message from server follows: %s';
  sErrInvalidResponse = 'The server returned an invalid response.';
  sErrTransportConnect = 'Unable to connect to the Pathways server.';
  sErrOutOfBounds = 'Value passed to %s out of %s bounds.  Limit: %d.  Value: %d';
  sErrXrefTypeName = 'Could not lookup a class for object type %s';
  sErrServer = 'Server returned error: %d %s';
  sErrVariantUnsupported = 'Variant type not supported. Type %d';
  sErrTransportSend = 'Could not send data because connection was not active to server.';

type
  EClassException = class(Exception);
  ENewException = class(EClassException)
    //PWLN's more-advanced exception class.  This class tracks a few more bits
    //of information, including error codes, and UserMessages and is fully
    //backwards compatible with standard Exception classes.
  private
    FUserMessage: string;
    FErrorCode: integer;
  public
    constructor create(iCode: integer; sUserMessage, sTechMessage: string); reintroduce; virtual;
    property UserMessage: string read FUserMessage write FUserMessage;
    //A message for a User (non technical)
    property ErrorCode: integer read FErrorCode write FErrorCode;
    //Error code-- a code for the error -- a number that is the code that is the error.
  end;

  EBasicException = class (Exception);
  EUserError = class(ENewException)
  public
  constructor create(sMessage: string);reintroduce;virtual;
  end;

  EBusyFailure = class(ENewException);
  EPwayException = class(Exception);
  EArchitecturalInconsistency = class(EPwayException);
  ETransportError = class(EPwayException);
  EIncomplete = class(EPwayException);
  EDataFieldError = class(EPwayException);
  EDataObjectError = class(EPwayException);
  EClassFactoryError = class(EPwayException);
  EServerError = class(EPwayException);
  EConfigurationError = class(EPwayException);
  EUnMarshallingError = class(Exception);
  EMarshallingError = class(EPwayException);

  EMissingInputValue = class(Exception)
  protected
    FMissing: string;
  public
    constructor create(sMissing: string);reintroduce;
    property missing: string read FMissing write FMissing;
  end;

  EVariableNeeded = class(Exception);

procedure SetExtPwayError(sMsg: string);
function GetExtPwayError: string;
var
  sExtError: string;

implementation


//-----------------------------------------------------------------------------
procedure SetExtPwayError(sMsg: string);
begin
  sExtError := sMsg;

end;

//-----------------------------------------------------------------------------
function GetExtPwayError: string;
begin
  result := sExtError;

end;


{ ENewException }

constructor ENewException.create(iCode: integer; sUserMessage,
  sTechMessage: string);
//A constructor... slightly different than a standard exception, accepts some new information.
begin
  inherited Create(sTechMessage);
  UserMessage := sUserMessage;
  ErrorCode := iCode;
  if sUserMessage = '' then begin
    if iCode > 1000 then begin
      sUserMessage := sTechMessage;
    end else begin
      sUserMessage := 'An internal Error Occured';
    end;

  end;
end;

{ EMissingInputValue }

constructor EMissingInputValue.create(sMissing: string);
begin
  inherited create('Missing Input Value: '+sMissing);
  Fmissing := sMissing;

end;

{ EUserError }

constructor EUserError.create(sMessage: string);
begin
  inherited create(888, sMessage, 'Audit:');
end;

end.
