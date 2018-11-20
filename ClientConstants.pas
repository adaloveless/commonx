unit ClientConstants;
//This unit defines constants for all kinds of stuff, from network timeouts, to
//browser_type IDs, PWLN Execution types, Activity Types, etc.
//There is only one function defined in this unit.



interface

const
  //BrowserType
  BT_UNKNOWN = 0;
  BT_IE = $01000000;
  BT_NETSCAPE =$02000000;

  //SYSTEM TYPE
  ST_UNKNOWN = 0;
  ST_PC = $00010000;
  ST_MAC = $00020000;

(*type
  TErrVal = cardinal;
  TImageId = type integer;
const
  //Feature flags (Account Options)
  FF_LRT = 1;
  FF_CLASSTOOLS = 2;
  FF_PWLN = 3;
  FF_HOMEROOM = 4;
  FF_ISRN = 8;
  FF_EDUTEST = 16;

  // Session Options Flags
  SO_FASTCONNECT = 1;






  //If no interface is returned from QueryInterface.
//  E_NOINTERFACE = $80004002;

  //ACtivity Types -- ported from Pway16
	{Species}
	CStructure 				= 0;   { Menu/sequence }
	CLesson 					= 1;   { Actual computer-based courseware lessons }
	CLegacyOffLine 					= 2;   { Non-computer based courseware }
  CTest             = 3;   { Examiner test }
  CAssessment       = 4;   { Multiple Examiner tests, for placement }
  CRecord           = 5;   { Performance records only (no execution) }
  CNoActivity       = 30000;   { Used for missing activities }

	{Style}
  CMenu             = 1;
  CSeqWithReview    = 2;
  CSeqWithoutReview = 3;

  {Type}
	COrganizer 				= 1;
	CCurriculum 			= 2;
	CCourse 					= 3;
	CModule 					= 4;
	CCustom 					= 5;

  {PrereqStyle}     {Mastery, Completion, or Score}
  CPrereqStyleMastery = -1;
  CPrereqStyleCompletion = -2;
  CPrereqStyleExecution = -3;
  CPrereqStyleMastOrComp = -4;  {Either mastery or completion }


  CTutorial 				 = 101;
  CApplication 			 = 102;
	CDrillAndPractice  = 103;
  CPresentation 		 = 104;
  CProblem 					 = 105;
  CSkillGame 				 = 106;
  CLogicGame 				 = 107;
  CReview 					 = 108;
	CSimulation 			 = 109;
  CChallenge 				 = 110;
  CPractice					 = 111;
  CDrill						 = 112;
  CProbSolvingAct    = 113;
	CProject					 = 114;
	CWordBank					 = 115;
	CEditor						 = 116;
	CProgram					 = 117;

  {MastMethod}
	CMActComplete 		= 1;  {Mastered when all activities complete}
  CMAct 						= 2;	{Mastered when activity ID is complete}
  CMNum 						= 3;	{Mastered when n number of activities complete}
	CMPer 						= 4; 	{Mastered when x percent of activities complete}
  CMNone 						= 5;	{No mastery data}
  CMActList 				= 6;	{Mastered when all activities in a list are complete}
  CMScore           = 7;  {Numerical Score }
  CMSequence        = 8;  {Sequence - mastered when last item done}
  CMList            = 9;  {List, mastered, complete or (?) score}
  CMActMast         = 10; {Mastered when all activities in a list are mastered}
  CMNumMast         = 11; { Mastered when specified # of children are mastered }
  CMAllMast         = 12; {Mastered when all children mastered or exempt}

  {Completion Method}
  CCActComplete 		= 1;	{Complete when all activities complete}
  CCAct 						= 2; 	{Complete when activity ID is complete}
  CCNum 						= 3;	{Complete when n activities complete}
  CCPer							= 4;	{Complete when x percent of activiites complete}
  CCActComp 				= 5;	{Complet when Act ID in CompInfo is mastered}
  CCActScore 				= 6;	{Complete when score >= x (x in CompInfo)}
  CCNone 						= 7;	{No completion data}
  CCActListComp 		= 8;	{Complete when list of act in MastLt are complete}
	CCActListMast 		= 9;	{Complete when list of act in MastLt are mastered}
  CCSequence        = 10; {Sequence - complete when last item done}
  CCList            = 11; {Complete when list of mastered, complete or (?) score}
  CCNumMast         = 12; { Complete when specified # of children are mastered }


	CMasteryTest 			= 301;
  CCourseAssessmentTest = 302;
  CFastTrack 				= 303;
  CCLAMasteryTest   = 304;
  CCLAAssessmentTest = 305;

  CXamAssessPlacement = 351;
  CXamAssessCurriculum = 352;

  COfflineType = 401;
  CWebActivity = 402;
  CPDFActivity = 403;


  { - even though cRecord is a species, we need to create a constant here
      because filters operated on the linkAct table which uses these numbers. }
  CRecordType  = 501;
  CTestType    = 601;    { Test using The Examiner -- possibly to become several }
	CTestExemptionType = 602;
	CAssessmentType	   = 701;

	{ExecType}
	CIPCD3            = 2002;  {IPCD3}
  CWinDigital Tundra         = 2004;  {For WinDigital Tundra (citrix)}
  CFLASH            = 2005;  {WEBDigital Tundra}
  CPstURL           = 2006;
  CURL              = 2007;
  COffline          = 2008;
  CWebDigital TundraPDF              = 2009;
  CWasatch          = 2010;
  CMicroDigital Tundra       = 2011;
  CHomeroomURL      = 2012;
  CCyberEd          = 2013;
  CFLASHMX          = 2014;
  CWebDigital TundraMXPDF    = 2015;
  CGeneralPDF       = 2016;
  CEduTest          = 2017;



  CDosMPAS          = 1002;
	CDosQuest         = 1003;
  CDOSOther					= 1004;

//  CWebRemoteURL     = 2006;  {For courseware on EXTERNAL servers (global address)}
//  CWebURL           = 2002;  {For courseware launched with a URL on THIS SERVER}
//  CWinBase          = 2001;  {For generic Windows stuff }
//  CExaminer         = 2003;  {For The Examiner test }
//  CWebDigital Tundra         = 2002;  {Web Digital Tundra URLS (java script included)}





  //Error Categories by severity
  CERR_SEV_EXCEPTION : TErrVal = $00010000;
  CERR_SEV_SEVERE    : TErrVal = $00020000;
  CERR_SEV_REASON    : TErrVal = $00040000;
  CERR_SEV_WARNING   : TErrVal = $00080000;
  CERR_SEV_HINT      : TErrVal = $00100000;
  CERR_SEV_INSTRUMENT: TErrVal = $00200000;

  //Error Categorys by area of application
  CERR_CAT_DATAVALIDATION  : TErrVal = $01000000;
  CERR_CAT_COMMUNICATIONS  : TErrVal = $02000000;
  CERR_CAT_DataOBJECTS   : TErrVal = $04000000;
  CERR_CAT_GENERAL         : TErrVal = $08000000;

  //Typical Error Masks
  CERR_ALL_CATEGORIES  :TErrVal = $FFFF0000;
  CERR_ALL_NUMBERS         :TErrVal = $0000FFFF;
  CERR_ALL_SEVERITY        :TErrVal = $00DF0000; //All except instruments
  CERR_ALL_LOCATIONS       :TErrVal = $FF000000;

  //*******ERRORS*******

  //Server Errors

  //Post Failure
  CERR_DO_POST_FAILURE = $02040001;



  CREGISTRY_KEY = 'HKEY_LOCAL_MACHINE'; //Registry entry key
  CREG_CONFIG_BASE = '\SOFTWARE\Digital Tundra\Pathways'; //Registry entry location

  CDIR_LOGROOT = 'RootLogDir';    //Root directory for logging information
                                  // like errors, exceptions, and installs

  CDLL_COUNT = 'DLLCount';
  CDLL_NAMEPREFIX = 'DLLName';
  CMAX_ERR_FILES = 5;

// do not change the case of these two constants
  CTRUE = 'true';
  CFALSE = 'false';

{$ifdef Autotest}
  CControlDrive = 'q:\jeffj\';
{$endif}

function ExecTypeToServiceID(iExecType: integer): integer;
*)
implementation(*
//------------------------------------------------------------------------------
function ExecTypeToServiceID(iExecType: integer): integer;
//For a given PWLN Execution type, converts the Execution type to a service ID type
//for doing license validation checking.
//p: iExecType: the PWLN ExecType field from the Lesson record.
//p: result: Integer service ID for calling service license validation.
begin
  case iExecType of
    CPstURL: result := 3;
    CHomeroomURL: result := 2;
  else
    result := 1;
  end;

end;*)

end.
