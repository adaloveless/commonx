unit ServerInterfaceInterface;

interface

uses
  DataObjectCache, DataObject, classes,tickcount;

const
  STANDARD_TIMEOUT = 0;
type
  INetworkClient = interface
    function GetPoolTime: ticker;
    procedure SetpoolTime(const tm: ticker);
    function GetMWEndPoint: string;
    function GetMWHost: string;
    procedure SetMWEndPoint(Value: string);
    procedure SetMWHost(Value: string);
    property MWEndPoint: string read GetMWEndPoint write SetMWEndPoint;
    property MWHost: string read GetMWHost write SetMWHost;
    property Pooltime: ticker read Getpooltime write SetPoolTime;
  end;

  ISmartNetworkClient = interface(INetworkClient)
    function GetContext: string;
    procedure SetContext(Value: string);
    property Context: string read GetContext write SetContext;
  end;


  IServerInterface = interface(ISmartNetworkClient)
  ['{42931BAB-98CC-44E2-A6FE-BADC86ECFB9D}']
    function LazyQuery(cache: TDataObjectCache; out obj: TDataObject; sQuery: string; iSessionID: integer; bExpectMany: boolean; slDebug: TStringList = nil; iTimeoutMS: integer = STANDARD_TIMEOUT): boolean;
    function Query(cache: TDataObjectCache; out obj: TDataObject; sQuery: string; iSessionID: integer; bExpectMany: boolean; slDebug: TStringList = nil; iTimeoutMS: integer = STANDARD_TIMEOUT): boolean;
    function UpdateQuery(cache: TDataObjectCache; sQuery: string; iSessionID: integer; bExpectMany: boolean=false; slDebug: TStringList = nil; iTimeoutMS: integer = STANDARD_TIMEOUT): boolean;
    function NoTransUpdateQuery(cache: TDataObjectCache; sQuery: string; iSessionID: integer): boolean;
    function FireForgetQuery(cache: TDataObjectCache; sQuery: string; iSessionID: integer; bSerialize: boolean= false): boolean;
    function RecordQuery(cache: TDataObjectCache; out obj: TDataObject; sQuery: string; iSessionID: integer; bExpectMany: boolean; iTimeoutMS: integer = STANDARD_TIMEOUT): boolean;
    function GetNextID(iType: integer; iSessionID: integer=0): integer;
    function SetNextID(iType: integer; iID: int64): boolean;

    function Ghost(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer): boolean; overload;
    function New(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer): boolean; overload;
    procedure Delete(cache: TdataObjectCache; obj: TDataObject);

    function GhostQueryMap(cache: TDataObjectCache;
      out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
      sBaseType: string; vBaseKeys: variant;
      slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;

      function QueryMap(cache: TDataObjectCache;
                        out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
                        bLazy: boolean; iIgnoreKeys: integer; sBaseType: string;
                        vBaseKeys: variant; slDebug: TStringList; sSubType: string = '';
                        iSubKeys: integer = 0): boolean;


      function LazyQueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
        iIgnoreKeys: integer; sBaseType: string; vBaseKeys: variant;
        slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;overload;

      function LazyQueryMap(cache: TDataObjectCache;
        out obj: TDataObject; sQuery: string; iSessionID, iTimeoutMS: integer;
        sBaseType: string; vBaseKeys: variant;
        slDebug: TStringList; sSubType: string; iSubKeys: integer): boolean;overload;

      function Login(cache: TDataObjectCache; out doSession: TDataObject; sUserName, sGroupName, sPassword: string; iAccountID: integer; iSystemType: integer; sIPAddress: string; bSystemAccount: boolean = false): boolean;
      procedure Logout(iSessionID: integer);

      function Fetch(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer; iTimeoutMS: integer = STANDARD_TIMEOUT): boolean; overload;
      function Fetch(cache: TDataObjectCache; out obj: TDataObject; token: TDataObjectToken; iSessionID: integer; iTimeoutMS: integer = STANDARD_TIMEOUT): boolean; overload;
      function LazyFetch(cache: TDataObjectCache; out obj: TDataObject; iType: integer; params: variant; iSessionID: integer; iTimeoutMS: integer = STANDARD_TIMEOUT): boolean; overload;
      function LazyFetch(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer; iTimeoutMS: integer = STANDARD_TIMEOUT): boolean; overload;
      function GhostFetch(cache: TDataObjectCache; out obj: TDataObject; sType: string; params: variant; iSessionID: integer; bLazy: boolean = true; iTimeoutMS: integer = STANDARD_TIMEOUT; bcheckCacheOnly: boolean = false): boolean; overload;

      function Post(obj: TDataObject; out objOutput: TDataObject; cache: TDataObjectCache; iSessionID: integer; bIncludeSubFields:boolean =false):boolean; overload;
      function Post(obj: TDataObject; iSessionID: integer; bIncludeSubFields:boolean =false):boolean; overload;




    function GetLastErrorMessage: string;
    function GetLastErrorCode: integer;


    procedure ContinueConnection;
    procedure Rollback;
    procedure Commit;

    function GetTimeout: integer;
    procedure SetTimeout(const value: integer);
    property Timeout: integer read GetTimeOut write SetTimeout;

    function GetStructureFromEngine(id: integer): string;
    function PingRDTP: boolean;
    function PingDB: boolean;
  end;



implementation

end.
