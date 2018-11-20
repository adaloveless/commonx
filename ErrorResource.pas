unit ErrorResource;

//This unit serves to provide information about friendly error messages and error
//constants.

interface

uses
  SharedObject, classes, stringx, orderlyinit;

const
  //orphan
  ERR_FETCH = 'We were unable to process your request.';
  ERR_SAVE = 'Some of the information could not be saved';
  ERR_DELETE = 'Some of the data could not be deleted';
  ERR_INTERNAL = 'Internal Problem';
  ERR_CWTIMEOUT = 'We are still processing the results from your last activity.  You will not be able to continue until this processing is complete.  Please retry your request in a few seconds.';
  ERR_CWRUNNING = 'Already running courseware';
  ERR_COPY = 'We were unable to copy the information you requested.';
  ERR_HOMEROOMDOWN = 'We''re sorry, the testing server is down at the moment. Please try again later';

  //specific failures
  ERRMSG3000 = 'A structure cannot contain itself.';

  ERRMSG3001 = 'Information could not be saved. Please verify the information is correct.';
  ERRMSG3002 = 'Invalid User, Group, or Password';

  ERC_LEGACY = 899;
  ERC_SECURITY = 900;
  ERC_LIST_COPY = 901;
  ERC_CWTIMEOUT = 902;
  ERC_INVALID_PASSWORD = 903;
  ERC_XML = 905;
  ERC_ORPHANED = 906;
  ERC_ERROR_SETTING_EXEMPTIONS = 907;
  ERC_OBJECT_RANGE = 908;
  ERC_UNMARSHALL = 909;
  ERC_OBJECT_CACHE = 910;
  ERC_FACTORY = 911;
  ERC_CWRUNNING = 916;
  ERC_SERVER_TO_SERVER_ERROR = 917;
  ERC_HOMEROOMDOWN = 918;
  ERC_MISSING_DISTRICT = 919;
  ERC_CANNOT_RENAME_ADMIN_GROUP = 926;
  ERC_FETCH_ACCOUNT_LIST = 927;
  ERC_SERVER_BUSY = 932;
  ERC_BATCH_ENROLL_TIME_WINDOW = 933;
  ERC_BATCH_ENROLL_LIMIT = 934;
  ERC_PROXY = 935;
  ERC_SCRIPT = 938;
  ERC_BLANK_PASSWORD = 904;

type
  TErrorResource = class(TSharedObject)
  //TErrorREsource stores default error messages for certain error codes so that
  //we can relay a more friendly error message to users.  This class is currently
  //underused, and because of this, most error messages in PWLN aren't all that friendly.
  //Its esssentially a ansistring factory, give it a number, it gives you a message.
  private
    slErrorMessages: TStringList;
    function GetErrorMessages(iCode: integer): ansistring;
  public
    constructor create; override;
    destructor destroy; override;
    procedure DefineErrors;
    procedure DefineError(iCode: integer; sMessage: ansistring);
    property ErrorMessages[iCode: integer]: ansistring read GetErrorMessages;
    //Array property that returns ansistrings for error codes.  If a message for an error code is not defined,
    //the message will be "General Error"

  end;

var
  ErrorRes: TErrorResource;


implementation


{ TErrorResource }

constructor TErrorResource.create;
begin
  inherited;
  slErrorMessages := TStringList.create;
  DefineErrors;
end;

procedure TErrorResource.DefineError(iCode: integer; sMessage: ansistring);
//p: iCode: The error code for the message.
//p: sMessage: The message to display... this will be the BIG message that goes in the green box error.
//Adds a cross-reference for an error code and message.  Makes an explicit copy of sMessage for cross-thread communication.
begin
  Lock;
  try
    slErrorMessages.AddObject(MakethreadSafe(sMessage), pointer(iCode));
  finally
    UnLock;
  end;
end;

procedure TErrorResource.DefineErrors;
//Add custom error messages here.  If a code is received that is not defined here,
//then the message will be "General Error".
begin
  //*****DEFINE ERRORS AND ERROR CODES HERE*********
  DefineError(-1, 'General Error');
  DefineError(101, 'Internal Data-Tier Error');
  DefineError(102, 'Internal Data-Tier Error');
  DefineError(103, 'Internal Communications Error');
  DefineError(104, 'Some of the requested information was unavailable');
  DefineError(105, 'Some of the information could not be saved');
  DefineError(106, 'Login information was invalid');
  DefineError(107, 'Already logged on');
  DefineError(108, 'One or more values could not be saved');
  DefineError(109, 'Duplicate information already exists in the database');
  DefineError(110, 'One or more values are illegal');
  DefineError(111, 'Internal Data-Tier Error');
  DefineError(112, 'Internal Communications Error');
  DefineError(113, 'This operation is not allowed');
  DefineError(114, 'The maximum number of users from your account are already on the system');
  DefineError(115, 'Your account has expired or is not yet activated');
  DefineError(116, 'You are not allowed to access information contained in this page');
  DefineError(117, 'Your session has expired');
  DefineError(118, 'Some of the data was invalid');
  DefineError(119, 'Required field missing');
  DefineError(120, 'Your account is currently disabled');
  DefineError(121, 'Illegal value');
  DefineError(122, 'Your login credentials are incorrect');
  DefineError(125, 'Your account has exceeded its instructional month limit.  Please contact your account coordinator.');
  DefineError(126, 'Your account data has been corrupted.  Please contact your account coordinator.');
  DefineError(1001, 'Your login credentials are incorrect');
  DefineError(123, 'You cannot build a structure that has circular references to itself.');
  DefineError(124, 'Structure is too complex');
  DefineError(503, 'The computer which holds your information could not be contacted.');
  DefineError(504, 'This report cannot be run on a class.  Please choose a group only.');

  DefineError(700, 'Warning: Bad Method in Header Repaired');
  DefineError(899, 'General Error');
  DefineError(900, 'You are not permitted to view this page.  Your session has been terminated.');
  DefineError(901, 'Internal Loss of Information'); //Saving an orphaned object
  DefineError(902, 'Passwords do not match.');
  DefineError(903, 'You have entered an invalid GroupName or Password');
  DefineError(904, 'Password cannot be blank.');
  DefineError(905, 'Transport error');
  DefineError(916, 'You are already running courseware');
  DefineError(917, 'Unable to contact server');
  DefineError(918, ERR_HOMEROOMDOWN);
  DefineError(907, 'An incorrect password was submitted.');
  DefineError(919, 'You have checked Digital Tundra LINK without defining a District');
  DefineError(920, 'When creating a Learner, choose ''Student'' as the testing system user type.');
  DefineError(921, 'When creating a Group Coordinator, valid testing system user types are: ''Teacher'' and ''School Admin''.');
  DefineError(922, 'When creating a Account Coordinator, valid values testing system user types are: ''School Admin'' and ''District Admin''.');
  DefineError(923, 'This server is shutting down, reload to try another server');
  DefineError(924, 'We could not find a datacenter with your account number.  Please check the number and try again.');
  DefineError(925, 'We could not find a server with your account number.  Please check the number and try again.');
  DefineError(926, 'You cannot rename the Account Administrator group');
  DefineError(927, 'Could not fetch account list');

  DefineError(928, 'An account name is required in the ''Name:'' field.');
  DefineError(929, 'A ''Maximum WS (License Count)'' value is required.');
  DefineError(930, 'An ''Expiration Date'' value is required.');
  DefineError(931, 'A ''Subscription Type'' is required.');
  DefineError(932, 'Server busy.');
  DefineError(933, 'Batch Enroll is not available during peak usage hours.  Please try again later');
  DefineError(934, 'Batch Enroll file is too large.');
  DefineError(935, 'We were unable to process your request. Please try again.');

  DefineError(936, 'A non-blank ''Country'' value required.');
  DefineError(937, 'A non-blank ''State'' value is required.');
  DefineError(938, 'General Script Error');

  DefineError(939, 'XML is Invalid');

  DefineError(940, 'Unable to create demographic set.');
  DefineError(941, 'Unable to create demographic field.');

  DefineError(942, 'When creating a Section Coordinator, choose ''Teacher'' as user type for testing system.');
  DefineError(943, 'This user has a testing system user type that is not valid for this role type.');
  DefineError(944, 'Your eduTest account has not been activated.  Please wait a while and try again.');
  DefineError(1000, '');
  DefineError(1502, 'Input needed');


  //*****DEFINE ERRORS AND ERROR CODES HERE*********
end;

destructor TErrorResource.destroy;
//Standard
begin
  slErrorMessages.free;
  inherited;

end;

function TErrorResource.GetErrorMessages(iCode: integer): ansistring;
//p: iCode: The Error code
//For iCode returns a message for the error.
var
  t: integer;
begin
  result := '';
  Lock;
  try
    for t:= 0 to slErrorMessages.count-1 do begin
      if integer(slErrorMessages.objects[t]) = iCode then
        result := MakethreadSafe(slErrorMessages[t]);
    end;
  finally
    UnLock;
  end;

end;

procedure oinit;
begin
  ErrorRes := TErrorResource.create;

end;

procedure ofinal;
begin

  ErrorRes.free;


end;

initialization
  oinit;
//  init.RegisterProcs('ErrorResource', oinit, ofinal);


finalization
  ofinal;

end.
