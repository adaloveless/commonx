unit iSCSI;
{$DEFINE SHIFTOPT}
{x$DEFINE INDY}
{$DEFINE NEWCMDSN}
{x$INLINE AUTO}
//Next Steps:
//[x] 1. Read up on what an iSCSI Command Response looks like
//[x] 2. Capture Mode Sense data and just return it all.  Figure out how to interpret it later.
//[x] 3.


//Reeval number systems

//x 1. Need to separate "session" from "connection"
//x 2. Need a construct within session to place "tasks" which may or may not derive from TCommand
//x 3. Need a mechanism to look up tasks by InitiatorTaskTag
//x 4. Commands may be potentially looked up by InitiatorTaskTag to be reassigned, cancelled, etc.
//x 5. Commands marked for immediate delivery will not advance CmdSN
//x 6. CmdSN will advance for each new command
//x 7. Acknowledge that we've received each command by Returning ExpCMdDSn.
//x 8. DataSN is used if there's  multiple packets to be returned for a particular data response
//9. All packets out must be saved until acknowledged.  May potentially be retransmitted
//10.
{x$DEFINE MCS}
{$DEFINE IL}
{x$DEFINE USE_FIFO_ON_READ}
{$DEFINE NEW_STATSN}
{$IFDEF NEW_STATSN}
  {$DEFINE STATSN_AUTO_LOGIN}
  {$DEFINE STATSN_DATAOUT_INTERIM}
  {$DEFINE STATSN_R2T_INTERIM}
{$ELSE}
  {$DEFINE OLD_STATSN}
{$ENDIF}
{x$INLINE AUTO}
{$DEFINE LOCAL_DEBUG}
{x$DEFINE ENABLE_LOCAL_DEBUG}
{$IFDEF INDY}
{x$DEFINE FLUSH_IOHANDLER}
{$ENDIF}





//Your wireshark filter
//(ip.dst == 172.16.0.33) && (ip.src == 172.16.0.82) && (tcp.port==3260)

//http://www.ietf.org/rfc/rfc3720.txt


//basic header format implemented as "COMMON Header"
//   The format of the BHS is:
//
//   Byte/     0       |       1       |       2       |       3       |
//      /              |               |               |               |
//     |0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|
//     +---------------+---------------+---------------+---------------+
//    0|.|I| Opcode    |F|  Opcode-specific fields                     |
//     +---------------+---------------+---------------+---------------+
//    4|TotalAHSLength | DataSegmentLength                             |
//     +---------------+---------------+---------------+---------------+
//    8| LUN or Opcode-specific fields                                 |
//     +                                                               +
//   12|                                                               |
//     +---------------+---------------+---------------+---------------+
//   16| Initiator Task Tag                                            |
//     +---------------+---------------+---------------+---------------+
//   20/ Opcode-specific fields                                        /
//    +/                                                               /
//     +---------------+---------------+---------------+---------------+

//   48




//Initiator Op-Codes
//0x00 NOP-Out
//0x01 SCSI Command (encapsulates a SCSI Command Descriptor Block)
//0x02 SCSI Task Management function request
//0x03 Login Request
//0x04 Text Request
//0x05 SCSI Data-Out (for WRITE operations)
//0x06 Logout Request
//0x10 SNACK Request
//0x1c-0x1e Vendor specific codes

//Target op-Codes
//0x20 NOP-In
//0x21 SCSI Response - contains SCSI status and possibly sense information or other response information.
//0x22 SCSI Task Management function response
//0x23 Login Response
//0x24 Text Response
//0x25 SCSI Data-In - for READ operations.
//0x26 Logout Response
//0x31 Ready To Transfer (R2T) - sent by target when it is ready to receive data.
//0x32 Asynchronous Message - sent by target to indicate certain special conditions.
//0x3c-0x3e Vendor specific codes
//0x3f Reject

interface

uses
  typex,systemx,
  ringbuffer,
  skill, herro,
  sysutils,
  managedthread,
  AdminAlert,
  ringstats, virtualdiskconstants,
  ApplicationParams, numbers, debugpinger, VirtualDisk_Advanced, scsi, classes, commonconstants,netbytes, Sharedobject,
  endian,  stringx, namevaluepair, betterobject, generics.collections.fixed,
  orderlyinit, tickcount,
  windows,
  //still required because INDY is used for RDTP servers
  idTCPConnection,
  IdIOHandler, better_collections,
  better_indy,
  idglobal, idcontext,
{$IFDEF INDY}
  helpers.sockets,
{$ELSE}
  better_sockets2, winsock,
{$ENDIF}
  debug;

const
  DEFAULT_MAX_CMD_AHEAD = $40;
  OPCODE_MASK: byte = $3f;
  FLAG_LOGIN_TRANSIT_NEXT = 1 shl 7;
  FLAG_TEXT_IS_COMPLETE = 1 shl 6;
  MASK_CSG = 4+8;
  MASK_NSG = 1+2;
  STATUS_CLASS_SUCCESS = 0;
//  STATIC_DISK_SIZE = 2000000;
//  STATIC_BLOCK_SIZE = 512;
var
  MAX_CMD_AHEAD : nativeint = DEFAULT_MAX_CMD_AHEAD;

type
  TiSCSI_Connection = class;//forward
  TopSpec = (opGeneral, opLoginReq, opLoginResp);
  ESCSIProtocolError = class(Exception);

  TISCSI_ISID = packed record
  strict private
    _tanda: byte;
    _b: word;
    _c: byte;
    _d: word;
    function GetB: word;
    function GetD: word;
    procedure SetB(b: word);
    procedure SetD(d: word);
  public
    property tanda: byte read _tanda write _tanda;
    property b: word read getb write setb;
    property c: byte read _c write _c;
    property d: word read getd write setd;
  end;

  PISCSI_ISID = ^TISCSI_ISID;


  TISCSI_CommonHeader = packed record
  strict private
    function GEtFlagFinal: boolean;{$IFDEF IL}inline;{$ENDIF}
    function GetFlagimmediate: boolean;{$IFDEF IL}inline;{$ENDIF}
    procedure SetFlagFinal(const Value: boolean);{$IFDEF IL}inline;{$ENDIF}
    procedure SEtFlagImmediate(const Value: boolean);{$IFDEF IL}inline;{$ENDIF}
  strict private
    function GEtFlag_Login_CSG: byte;{$IFDEF IL}inline;{$ENDIF}
    function GEtFlag_Login_NSG: byte;{$IFDEF IL}inline;{$ENDIF}
    procedure SetFlag_Login_CSG(const Value: byte);{$IFDEF IL}inline;{$ENDIF}
    procedure SetFlag_Login_NSG(const Value: byte);{$IFDEF IL}inline;{$ENDIF}
    function GetOpSpec_LUN: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetOpSpec_LUN(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function GetDataSegmentLength: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetDataSegmentLength(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function getOpCodeFlags: byte;{$IFDEF IL}inline;{$ENDIF}
    procedure setOpCodeFlags(const Value: byte);{$IFDEF IL}inline;{$ENDIF}
    function GetOpCode: byte;{$IFDEF IL}inline;{$ENDIF}
    procedure SetOpCode(const Value: byte);{$IFDEF IL}inline;{$ENDIF}
    function getTotalAHSLength: byte;{$IFDEF IL}inline;{$ENDIF}
    procedure setTotalAHSLength(const Value: byte);{$IFDEF IL}inline;{$ENDIF}
    function GetExpCmdSN: cardinal;{$IFDEF IL}inline;{$ENDIF}
    function getMaxCmdSN: cardinal;{$IFDEF IL}inline;{$ENDIF}
    function GetStatSN: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetExpCmdSN(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    procedure SetMaxCmdSn(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    procedure SetStatSN(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function GetLoginStatusClass: byte;{$IFDEF IL}inline;{$ENDIF}
    function GetLoginStatusDetail: byte;{$IFDEF IL}inline;{$ENDIF}
    procedure SetLoginStatusClass(const Value: byte);{$IFDEF IL}inline;{$ENDIF}
    procedure SetLoginStatusDetail(const Value: byte);{$IFDEF IL}inline;{$ENDIF}
    function GetExpStatSN: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetExpStatSN(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function GetCID: smallint;{$IFDEF IL}inline;{$ENDIF}
    function GetCmdSN: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetCID(const Value: smallint);{$IFDEF IL}inline;{$ENDIF}
    procedure SetCmdSN(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function GEtFlagTRansit: boolean;{$IFDEF IL}inline;{$ENDIF}
    procedure SetFlagTRansit(const Value: boolean);{$IFDEF IL}inline;{$ENDIF}
    function GetITT: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetITT(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function GetTSIH: smallint;{$IFDEF IL}inline;{$ENDIF}
    procedure SetTSIH(const Value: smallint);{$IFDEF IL}inline;{$ENDIF}
    function GetTArgetTransferTag: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SEtTargetTransferTAg(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function GetLUN: int64;{$IFDEF IL}inline;{$ENDIF}
    procedure SetLUN(const Value: int64);{$IFDEF IL}inline;{$ENDIF}
    function GetTime2Retain: word;{$IFDEF IL}inline;{$ENDIF}
    function GetTime2Wait: word;{$IFDEF IL}inline;{$ENDIF}
    procedure SetTime2Wait(const Value: word);{$IFDEF IL}inline;{$ENDIF}
    procedure SetTime2Retain(const Value: word);{$IFDEF IL}inline;{$ENDIF}
    function GetDataSN: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetDAtaSn(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function GetSnackTag: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetSnackTAg(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function GEtFlagAckRequested: boolean;{$IFDEF IL}inline;{$ENDIF}
    procedure SetFlagAckRequested(const Value: boolean);{$IFDEF IL}inline;{$ENDIF}
    function GEtResponseContainsSCSIStatus: boolean;{$IFDEF IL}inline;{$ENDIF}
    procedure SetResponseContainsSCSIStatus(const Value: boolean);{$IFDEF IL}inline;{$ENDIF}
    function GetStatusClass: byte;{$IFDEF IL}inline;{$ENDIF}
    function GetStatusDetail: byte;{$IFDEF IL}inline;{$ENDIF}
    procedure SetStatusClass(const Value: byte);{$IFDEF IL}inline;{$ENDIF}
    procedure SetStatusDetail(const Value: byte);{$IFDEF IL}inline;{$ENDIF}
    function GetResidualCount: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetResidualCount(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function GEtResidualUnderflow: boolean;{$IFDEF IL}inline;{$ENDIF}
    procedure SEtResidualUnderflow(const Value: boolean);{$IFDEF IL}inline;{$ENDIF}
    function GEtResidualOverflow: boolean;{$IFDEF IL}inline;{$ENDIF}
    procedure SEtResidualOverflow(const Value: boolean);{$IFDEF IL}inline;{$ENDIF}

    function GetServiceAction: byte;{$IFDEF IL}inline;{$ENDIF}
    procedure SetServiceAction(const Value: byte);{$IFDEF IL}inline;{$ENDIF}
    function GetEDTL: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetEDTL(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}
    function GetBufferOffset: cardinal;{$IFDEF IL}inline;{$ENDIF}
    procedure SetBufferOffset(const Value: cardinal);{$IFDEF IL}inline;{$ENDIF}

  public
    OpCode_UnMasked: byte;
    //Bit 6 = "I" Queued Delivery
    //Login Request 0x03
    //Login Response 0x23
   //SCSI Command = 0x01
    //SCSI Data In: 0x25
    osb1: byte;
    osb2: byte;
    osb3: byte;
    __LengthField: cardinal;
    //@8
    opspecbytes: array [0..7] of byte;
    //@16
    __InitiatorTaskTag: cardinal; //done 1: endianswap
    //@20
    __CID: smallint; //DONE 1: endianswap
    //@22
    Reserved1: smallint;
    //@24
    __CmdSN: cardinal;//DONE 1: endianswap
    //@28
    __ExpStatSN: cardinal;//DONE 1: endianswap
    //@32
    Reserved2: cardinal;
    //@36
    Reserved3: cardinal;
    //@40
    Reserved4: cardinal;
    Reserved5: cardinal;
    //@48



    property ServiceAction: byte read GetServiceAction write SetServiceAction;
    property OpCode: byte read GetOpCode write SetOpCode;
    property OpCodeFlags: byte read getOpCodeFlags write setOpCodeFlags;
    property TotalAHSLength: byte read getTotalAHSLength write setTotalAHSLength;
    property DataSEgmentLength: cardinal read GetDataSegmentLength write SetDataSegmentLength;
    property Flag_Immediate: boolean read GetFlagimmediate write SEtFlagImmediate;
    property Flag_Final: boolean read GEtFlagFinal write SetFlagFinal;
    property Flag_Ack: boolean read GEtFlagAckRequested write SetFlagAckRequested;
    property Flag_ResidualUnderFlow: boolean read GEtResidualUnderflow write SEtResidualUnderflow;
    property Flag_ResidualOverFlow: boolean read GEtResidualOverflow write SEtResidualOverflow;
    property Flag_ResponseContainsSCSIStatus: boolean read GEtResponseContainsSCSIStatus write SetResponseContainsSCSIStatus;
    property Flag_TRansit: boolean read GEtFlagTRansit write SetFlagTRansit;
    property Flag_Login_NSG: byte read GEtFlag_Login_NSG write SetFlag_Login_NSG;
    property Flag_Login_CSG: byte read GEtFlag_Login_CSG write SetFlag_Login_CSG;

    property OpSpec_LUN: cardinal read GetOpSpec_LUN write SetOpSpec_LUN;
    function AddrOf(byte: nativeint): PByte;

    property StatSN: cardinal read GetStatSN write SetStatSN;
    property ExpStatSN: cardinal read GetExpStatSN write SetExpStatSN;
    property CmdSN: cardinal read GetCmdSN write SetCmdSN;
    property CID: smallint read GetCID write SetCID;
    property ExpCmdSN: cardinal read GetExpCmdSN write SetExpCmdSN;
    property MaxCmdSN: cardinal read getMaxCmdSN write SetMaxCmdSn;
    property ExpDataSN: cardinal read GetDataSN write SetDAtaSn;//Shares getters and setters
    property DataSN: cardinal read GetDataSN write SetDAtaSn;//shared getters and setters
    property R2TSN: cardinal read GetDataSN write SetDAtaSn;//shared getters and setters
    property BufferOffset: cardinal read GetBufferOffset write SetBufferOffset;
    property InitiatorTaskTAg: cardinal read GetITT write SetITT;
    property ExpectedDataTransferLength: cardinal read GetEDTL write SetEDTL;

    property StatusClass: byte read GetStatusClass write SetStatusClass;
    property LoginStatusClass: byte read GetLoginStatusClass write SetLoginStatusClass;
    property StatusDetail: byte read GetStatusDetail write SetStatusDetail;
    property TSIH: smallint read GetTSIH write SetTSIH;
    property TargetTransferTag: cardinal read GetTArgetTransferTag write SEtTargetTransferTAg;
    property LUN: int64 read GetLUN write SetLUN;
    property Time2Wait: word read GetTime2Wait write SetTime2Wait;
    property Time2Retain: word read GetTime2Retain write SetTime2Retain;
    property SnackTag: cardinal read GetSnackTag write SetSnackTAg;

    procedure InitResponse(req: TISCSI_CommonHeader; ctx: TiSCSI_Connection; bForLogin: boolean);inline;
    procedure PrepareToSend(req: TISCSI_CommonHeader; ctx: TiSCSI_Connection);inline;
    property ResidualCount: cardinal read GetResidualCount write SetResidualCount;
    property DesiredR2TTransfer: cardinal read GetResidualCount write SetResidualCount;

    procedure SetOptGroup2(const a, b, c: boolean; const d, lun: ni);inline;
  end;

  TiSCSI_DataIn = packed record
    //     Byte/     0       |       1       |       2       |       3       |
    //      /              |               |               |               |
    //     |0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|
    //     +---------------+---------------+---------------+---------------+
    //    0|.|.| 0x25      |F|A|0 0 0|O|U|S| Reserved      |Status or Rsvd |
    //     +---------------+---------------+---------------+---------------+
    //    4|TotalAHSLength | DataSegmentLength                             |
    //     +---------------+---------------+---------------+---------------+
    //    8| LUN or Reserved                                               |
    //     +                                                               +
    //   12|                                                               |
    //     +---------------+---------------+---------------+---------------+
    //   16| Initiator Task Tag                                            |
    //     +---------------+---------------+---------------+---------------+
    //   20| Target Transfer Tag or 0xffffffff                             |
    //     +---------------+---------------+---------------+---------------+
    //   24| StatSN or Reserved                                            |
    //     +---------------+---------------+---------------+---------------+
    //   28| ExpCmdSN                                                      |
    //     +---------------+---------------+---------------+---------------+
    //   32| MaxCmdSN                                                      |
    //     +---------------+---------------+---------------+---------------+
    //   36| DataSN                                                        |
    //     +---------------+---------------+---------------+---------------+
    //   40| Buffer Offset                                                 |
    //     +---------------+---------------+---------------+---------------+
    //   44| Residual Count                                                |
    //     +---------------+---------------+---------------+---------------+
    //   48| Header-Digest (Optional)                                      |
    //     +---------------+---------------+---------------+---------------+
    //     / DataSegment                                                   /
    //    +/                                                               /
    //     +---------------+---------------+---------------+---------------+
    //     | Data-Digest (Optional)                                        |
    //     +---------------+---------------+---------------+---------------+
  public


  end;

  TISCSI_AdditionalHeaderSection = packed record
  strict private
    function GetAHSType_Flag_63Reserved: boolean;
    function GetAHSType_Flag_EpectedBidirectionalReadDataLength: boolean;
    function GetAHSType_Flag_ExtendedCDB: boolean;
    function GetAHSType_Flag_Reserved: boolean;
    procedure SetAHSType_Flag_63Reserved(const Value: boolean);
    procedure SetAHSType_Flag_EpectedBidirectionalReadDataLength(
      const Value: boolean);
    procedure SetAHSType_Flag_ExtendedCDB(const Value: boolean);
    procedure SetAHSType_Flag_Reserved(const Value: boolean);
  public
    length: word;
    ahsType: byte;
    ahd_type_specific: byte;
    property AHSType_Flag_Reserved: boolean read GetAHSType_Flag_Reserved write SetAHSType_Flag_Reserved;
    property AHSType_Flag_ExtendedCDB: boolean read GetAHSType_Flag_ExtendedCDB write SetAHSType_Flag_ExtendedCDB;
    property AHSType_Flag_EpectedBidirectionalReadDataLength: boolean read GetAHSType_Flag_EpectedBidirectionalReadDataLength write SetAHSType_Flag_EpectedBidirectionalReadDataLength;
    property AHSType_Flag_63Reserved: boolean read GetAHSType_Flag_63Reserved write SetAHSType_Flag_63Reserved;

    procedure InitResponse(req: TISCSI_CommonHeader);
  end;


  TiSCSITarget = class(TSharedObject)
  protected
    udps: TDTUDPServer;
  public
    constructor Create;override;
    destructor Destroy;override;


  end;

  TSCSIResponseType = (srtDataIn_0x25 = $25, srtResponse_0x21 = $21, srtResponse_None = $00, srtR2T_0x31 = $31);

  TSCSI_CommandResponse = class(TBetterObject)
  strict
  private
    FData: pbyte;
  private
    FLength: nativeint;
    FSuccess: boolean;
    FAllocationLength: cardinal;
    FBytesProcessed: cardinal;
    FResponseType: TSCSIResponseType;
    function GEtPaddedLength: nativeint;
    function GetIsOverflow: boolean;
    function GetIsUnderflow: boolean;
    function GetResidualCount: cardinal;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Allocate(const iSizE: nativeint);
    property Length: nativeint read FLength;
    property PAddedLength: nativeint read GEtPaddedLength;
    property Data: pbyte read FData write FData;
    property Success: boolean read FSuccess write FSuccess;
    property Original_AllocationLength: cardinal read FAllocationLength write FAllocationLength;
    property IsUnderFlow: boolean read GetIsUnderflow;
    property IsOverFlow: boolean read GetIsOverflow;
    property ResidualCount: cardinal read GetResidualCount;

    property BytesProcessed: cardinal read FBytesProcessed write FBytesProcessed;
    property ResponseType: TSCSIResponseType read FResponseType write FResponseTYpe;
  end;

{$IFDEF INDY}
  TiSCSIContext = TIdContext;
  TLocalContextConnection = TIDTCPConnection;
  TLocalIOHandler = TIdIOHandler;
  TLocalTCPSErver = TDTTCPServer;
{$ELSE}
  TFakeIOHandler = class(TBetterObject)
  private
    FJunkBool: boolean;
  published
    property WriteBufferingActive: boolean read FJunkBool write FJunkBool;
    procedure WriteBufferOpen(iThres: ni);inline;
    procedure WriteBufferFlush;inline;
    procedure WriteBufferClose;inline;
  end;
  TLocalContextConnection = TBetterCustomIPClient;
  TLocalIOHandler = TFakeIOHandler;

  TiSCSIContext = class(TBetterObject)
  private
    FData: TiSCSI_Connection;
    FCOnnection: TLocalContextConnection;
    FIO: TFakeIOHandler;
    procedure WriteBufferOpen(iThres: ni);
  public
    constructor create;override;
    destructor destroy;override;
    property IOHandler: TFakeIOHandler read FIO;
    property Data: TiSCSI_Connection read FData write FData;
    property Connection: TLocalContextConnection read FCOnnection write FConnection;

  end;
  TLocalTCPSErver = TBetterTCPServer;
{$ENDIF}


  TiSCSITargetPortal = class(TSharedObject)
  private
    procedure AllocateCombinedAndSend(const context: TiSCSIContext; var common: TISCSI_CommonHeader;
      di: TISCSI_CommonHeader; r: TSCSI_CommandResponse);
    procedure SendNOPIn(context: TiSCSIContext; var ref: TISCSI_CommonHeader; request_pong: boolean);

  protected
    tcps: TLocalTCPServer;
{$IFNDEF INDY}
    procedure TcpServerAccept(Sender: TObject; ClientSocket: TBetterCustomIpClient);
{$ENDIF}
  public
    last_good_command: ni;
    rsTCP1, rsTCP2, rsTCP3, rsTCP4, rsTCP5: TRingStats;
    function MaxDataSize(const ctx: TiSCSIContext): nativeint;
    constructor Create;override;
    destructor Destroy;override;
    procedure Start;
    procedure Stop;
    procedure OntcpsExecute(AContext: TiSCSIContext);
    function Dispatch(const context: TiSCSIContext; var common: TISCSI_CommonHeader): boolean;
    //---------------------------------------
    function Dispatch_NOP(const context: TiSCSIContext; var common: TISCSI_CommonHeader): boolean;
    //---------------------------------------
    function Dispatch_Login(const context: TiSCSIContext; var common: TISCSI_CommonHeader): boolean;
    function Dispatch_Logout(const context: TiSCSIContext; var common: TISCSI_CommonHeader): boolean;
    //---------------------------------------
    function Dispatch_Text(const context: TiSCSIContext; var common: TISCSI_CommonHeader): boolean;overload;
    function Dispatch_Text(const context: TiSCSIContext; var common: TISCSI_CommonHeader; slRequest, slREsponse: Tstringlist): boolean;overload;
    function Dispatch_Text_SendTargets(const context: TiSCSIContext; var common: TISCSI_CommonHeader; slRequest: TStringlist; slREsponse: Tstringlist): boolean;overload;
    function Dispatch_Text_SendTargets(const context: TiSCSIContext; var common: TISCSI_CommonHeader; sRequestSearchQuery: string; slREsponse: Tstringlist): boolean;overload;
    //---------------------------------------
    function Dispatch_TaskManagementFunction(const context: TiSCSIContext; var common: TISCSI_CommonHeader): boolean;overload;
    //---------------------------------------
    function Dispatch_DataOut(const context: TiSCSIContext; var common: TISCSI_CommonHeader): boolean;overload;
    //---------------------------------------
    function Dispatch_SCSI(const context: TiSCSIContext; var common: TISCSI_CommonHeader): boolean;
    //---------------------------------------
    function Dispatch_SCSI_Read6(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;{adapt}
    function Dispatch_SCSI_Read10(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;{adapt}
    function Dispatch_SCSI_Read12(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;{adapt}
    function Dispatch_SCSI_Read16(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;{adapt}
    function Dispatch_SCSI_Read32(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;{primary}

    function Dispatch_SCSI_Read32(context: TiSCSIContext; var common: TISCSI_CommonHeader; var scsirec: TSCSI_Read32): TSCSI_CommandResponse;overload;
    //---------------------------------------
    function Dispatch_SCSI_ModeSense6(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;{adapt}
    function Dispatch_SCSI_ModeSense10(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;{primary}
    function Dispatch_SCSI_ModeSense10(const context: TiSCSIContext; var scsirec: TSCSI_ModeSense10; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;
    //---------------------------------------
    function Dispatch_SCSI_ReadCapacity10(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;{adapt}
    function Dispatch_SCSI_ReadCapacity10(const context: TiSCSIContext; var scsirec: TSCSI_ReadCapacity10; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;
    function Dispatch_SCSI_ReadCapacity16(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;{adapt}
    function Dispatch_SCSI_ReadCapacity16(const context: TiSCSIContext; var scsirec: TSCSI_ReadCapacity16; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;
    //---------------------------------------
    function Dispatch_SCSI_TestUnitReady(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;{adapt}
    function Dispatch_SCSI_TestUnitReady(const context: TiSCSIContext; var scsirec: TSCSI_TestUnitReady; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;
    //---------------------------------------
    function Dispatch_SCSI_Write6(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;{adapt}
    function Dispatch_SCSI_Write10(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;{adapt}
    function Dispatch_SCSI_Write12(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;{adapt}
    function Dispatch_SCSI_Write16(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;{adapt}
    function Dispatch_SCSI_Write32(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;{primary}
    function Dispatch_SCSI_Write32(const context: TiSCSIContext; var common: TISCSI_CommonHeader; var scsirec: TSCSI_Write32): TSCSI_CommandResponse;overload;
    //---------------------------------------
    function Dispatch_SCSI_ReportLUNs12(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;{primary}
    function Dispatch_SCSI_ReportLUNs12(const context: TiSCSIContext; var common: TISCSI_CommonHeader; var scsirec: TSCSI_ReportLuns12): TSCSI_CommandResponse;overload;
    //---------------------------------------
    function Dispatch_SCSI_Inquery(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;{primary}
    function Dispatch_SCSI_Inquery(const context: TiSCSIContext; var common: TISCSI_CommonHeader; var scsirec: TSCSI_INquery): TSCSI_CommandResponse;overload;
    //---------------------------------------
    function Dispatch_SCSI_ServiceActionIn16(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
    //---------------------------------------
    function Dispatch_SCSI_SynchronizeCache10(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
    function Dispatch_SCSI_SynchronizeCache16(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;overload;{primary}
    function Dispatch_SCSI_SynchronizeCache16(const context: TiSCSIContext; var common: TISCSI_CommonHeader; var scsirec: TSCSI_SynchronizeCache16): TSCSI_CommandResponse;overload;
    //---------------------------------------
    function Dispatch_SCSI_GenericStub6(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
    function Dispatch_SCSI_GenericStub10(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
    function Dispatch_SCSI_GenericStub12(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
    function Dispatch_SCSI_GenericStub16(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
    function Dispatch_SCSI_GenericStub32(var opcode: TSCSILeadingBytes; const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
  end;

  TiSCSI_PDU = class(TBetterObject)
  public
    header: TiSCSI_CommonHeader;
    data: pbyte;
//    procedure AllocateData(iSize: nativeint);
//    function DataSize: nativeint;

  end;


  TiSCSI_RequestInfo = class(TBetterObject)
  public
//    property PDUs[idx: integer]:TiSCSI_CommonHeader read  read
  end;

  TiSCSI_Command = class(TSharedObject)
  private
    FCmdSN: cardinal;
  public
    property CmdSN: cardinal read FCmdSN write FCmdSN;
  end;

  TiSCSI_CommandList = class(TBetterList<TiSCSI_Command>)
  strict private
  strict protected
  public
  end;

  TiSCSI_CommandLists = class(TSharedObject)
  private
    FCommands: TiSCSI_CommandList;
    FNextExecutableCmdSN: nativeint;
    function GetCommand(CmdSN: cardinal): TiSCSI_Command;
  public
    property command_list: TiSCSI_CommandList read FCommands;
    property NextExecutableCmdSN: nativeint read FNextExecutableCmdSN write FNextExecutableCmdSN;
    function IndexOfCommand(CmdSN: cardinal): nativeint;
    function HasCommand(CmdSN: cardinal): boolean;
    property Commands[CmdSN: cardinal]: TiSCSI_Command read GetCommand;

    constructor Create;override;
    destructor Destroy;override;
  end;

  TiSCSI_Task = class(TSharedObject)
  public
    header: TiSCSI_CommonHeader;
    connection: TiSCSI_Connection;
    clear_time: ticker;
  end;

  TiSCSI_Task_Write = class(TiSCSI_Task)
  public
    scsirec: TSCSI_Write32;
  end;

  TiSCSI_Task_Read = class(TiSCSI_Task)
  public
    scsirec: TSCSI_Read32;
    sent: nativeint;
  end;

  TiSCSI_Task_Class = class of TiSCSI_TAsk;



  TiSCSI_Session = class(TSharedObject)
  strict
  private
    function GetSessionid: string;
    procedure SetSessionid(const Value: string); protected
    Ftasks: TBetterList<TiSCSI_Task>;
    Fsessionid: string;
  public

    rsIdle, rsTotal, rsDisk, rsIOHandlerRead, rsIOHandlerWrite: TRingStats;

    bTSG1Done: boolean;

    function FindTarget(const sName: string): TVirtualDisk;

    function IndexOfTask(id: cardinal): nativeint;
    function FindTask(id: cardinal): TiSCSI_Task;
    procedure ClearTask(id: cardinal);
    procedure KillStaleTasks(iTime: ticker);
    function AddTask(typ: TiSCSI_Task_Class; header: TISCSI_CommonHeader): TiSCSI_Task;

    constructor Create;override;
    destructor Destroy;override;
    property SessionID: string read GetSessionid write SetSessionid;
  end;

  TiSCSI_Connection = class(TSharedObject)
  private
    function GEtNextDataSn: cardinal;
    procedure SetNextStatSN(const Value: cardinal);
    function GetMaxImmediateDataSize: ni;
  protected
    FNextStatSN: cardinal;
    LastSeenCmdSN: cardinal;
    ExpCmdSN: cardinal;
    max_imm: ni;
    function GetNextStatSN: cardinal;
  public
    /// <link>aggregation</link>
    context: TiSCSIContext;
    debugpinger: TDebugPinger;
    session: TiSCSI_Session;
    loginstage: nativeint;
    login_pairs: TNameValuePairList;
    localpairs: TNameValuePairList;
    DataSN: cardinal;
//    ioprefetcher: TExternalEventThread;
    connection: TLocalContextConnection;
{$IFNDEF INDY}
    fifo_incoming: TSocketRingBuffer;
{$ENDIF}
    function Target: TVirtualDisk;
    constructor Create;override;
    destructor Destroy;override;
    procedure ApplySNs(var header: TiSCSI_CommonHeader);
{$IFDEF NEW_STATSN}
    property NextStatSn:cardinal read GEtNextStatSN write SetNextStatSN;
{$ENDIF}
    property NextDataSn: cardinal read GEtNextDataSn;

{$IFDEF INDY}
    procedure ioh_guarantee_write(p: pbyte; iLength: nativeint);
    procedure ioh_guarantee_read(p: pbyte; iLength: nativeint);
{$ENDIF}
    procedure EstablishSession(sid: string);
    property MaxImmediateDataSize: ni read GetMaxImmediateDataSize;

  end;
  TiSCSISessionManager = class(TSharedObject)
  protected
    FList: TBetterList<TiSCSI_Session>;
  public
    constructor Create;override;
    destructor Destroy;override;
    function IndexOf(sid: string): ni;inline;
    function GetSession(sid: string): TiSCSI_Session;
    procedure REgisterSession(s: TiSCSI_Session);
    procedure UnREgisterSession(s: TiSCSI_Session);
  end;



procedure ctx_guarantee_write(context: TiSCSIContext; const p: pbyte; const iLength: nativeint);{$IFNDEF INDY}inline;{$ENdIF}
procedure ctx_guarantee_read(context: TiSCSIContext; const p: pbyte; const iLength: nativeint);{$IFNDEF INDY}inline;{$ENdIF}


function getIOH(conn: TiSCSI_Connection): TLocalIOHandler;overload;inline;
function getIOH(context: TiSCSIcontext): TLocalIOHandler;overload;inline;
function iSCSIOpCodeToString(opcode: byte): string;
function AllocateCombined(var common: TISCSI_CommonHeader; res: TSCSI_CommandResponse): pbyte;

type
  TCommandDebug = record
    state: int64;
    cmd: byte;
  end;

threadvar
  debugpinger: TDebugPinger;
var
  cmddebug: TCommandDebug;

var
  scsi_session_manager: TiSCSISessionManager;
//  scsi_session: TiSCSI_Session;
  longfound: boolean;
  iscsidebug: boolean = false;


implementation

function getIOH(conn: TiSCSI_Connection): TLocalIOHandler;overload;inline;
begin
  {$IFDEF INDY}
  result := conn.Connection.IOHandler;
  {$ELSE}
    result := conn.context.iohandler;
  {$ENDIF}
end;
function getIOH(context: TiSCSIcontext): TLocalIOHandler;overload;inline;
begin
  {$IFDEF INDY}
  result := context.Connection.IOHandler;
  {$ELSE}
    result := context.iohandler;
  {$ENDIF}
end;

{ TiSCSITarget }
function AllocateCombined(var common: TISCSI_CommonHeader; res: TSCSI_CommandResponse): pbyte;
begin
  if res.Data <> nil then begin
    common.DataSEgmentLength := res.PaddedLength;
    result := GetMemory(sizeof(common)+ res.PAddedLength);
    movemem32(result, @common, sizeof(common));
    movemem32(result+sizeof(common), res.data, res.length);
  end else begin
    common.DataSEgmentLength := res.PaddedLength;
    result := GetMemory(sizeof(common)+ res.PAddedLength);
    movemem32(result, @common, sizeof(common));
  end;
end;

function ContextData(ctx: TiSCSIContext): TiSCSI_Connection;
begin
  result := TiSCSI_Connection(ctx.Data);
end;

procedure StatDebug(const s: string);inline;
var
  sas: ansistring;
begin
{$IFDEF LOCAL_DEBUG}
  if not iscsidebug then exit;
  Debug.log(inttostr(GetCurrentThreadID)+':'+s);
  if assigned(debugpinger) then begin
    sas := ansistring(s);
    debugpinger.Ping(@sas[strz], length(sas));
  end;
{$ENDIF}
end;
procedure LocalDebug(const s: string);inline;
var
  sas: ansistring;
begin
  LogToThreadStatus(s);
  Debug.Log(s);
//{$IFDEF ENABLE_LOCAL_DEBUG}
//  if not iscsidebug then exit;
//  Debug.log(inttostr(GetCurrentThreadID)+':'+s);
//  if assigned(debugpinger) then begin
//    sas := ansistring(s);
//    debugpinger.Ping(@sas[strz], length(sas));
//  end;
//{$ENDIF}
end;

constructor TiSCSITarget.Create;
begin
  inherited;

end;

destructor TiSCSITarget.Destroy;
begin

  inherited;
end;

{ TiSCSITargetPortal }

constructor TiSCSITargetPortal.Create;
begin
  inherited;
  rsTCP1 := TRingStats.create;
  rsTCP2 := TRingStats.create;
  rsTCP3 := TRingStats.create;
  rsTCP4 := TRingStats.create;
  rsTCP5 := TRingStats.create;
  rstcp1.setsize(2048);
  rstcp2.setsize(2048);
  rstcp3.setsize(2048);
  rstcp4.setsize(2048);
  rstcp5.setsize(2048);


end;

destructor TiSCSITargetPortal.Destroy;
var
  useless: nativeint;
begin
  useless := 0;
  Stop;
  rsTCP1.free;
  rsTCP2.free;
  rsTCP3.free;
  rsTCP4.free;
  rsTCP5.free;
  rsTCP1 := nil;
  rsTCP2 := nil;
  rsTCP3 := nil;
  rsTCP4 := nil;
  rsTCP5 := nil;




  inherited;
end;


function TiSCSITargetPortal.Dispatch(const context: TiSCSIContext; var common: TISCSI_CommonHeader): boolean;
//return true if handled
var
  cdata: TiSCSI_Connection;
begin
//  if context.connection.iohandler.RecvBufferSize <> 10000000 then
//    context.connection.iohandler.RecvBufferSize := 10000000;

//  context.connection.IOHandler.CheckForDataOnSource(0);
{$IFDEF INDY}
  if not getioh(context).WriteBufferingActive then begin
    getioh(context).WriteBufferOpen(10000000);
    //context.Connection.IOHandler.WriteBufferThreshold := 1000000;
  end;
{$ENDIF}



  try
    cdata := nil;
    cdata := contextdata(context);
{$DEFINE ALWAYS_PROCESS_CMDSN}
{$IFDEF ALWAYS_PROCESS_CMDSN}
   if not common.Flag_Immediate then begin
      if common.cmdsn <> cdata.ExpCmdSN then begin
//        if iscsidebug then LocalDebug('NOT EXPECTED COMMAND SN! '+common.cmdsn.tostring + ' <> ' + cdata.ExpCMDSN.ToString);
      end else begin
        cdata.ExpCmdSN := int64(common.CmdSN)+int64(1);
      end;
    end;
{$ENDIF}

    if assigned(cdata.session) then begin
      cdata.session.rsIdle.EndTime;
      cdata.session.rsTotal.BeginTime;
    end;

    try
      {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('OpCode: 0x'+inttohex(common.OpCode,2)+' '+iSCSIOpCodeToString(common.OpCode));{$ENDIF}


      case common.OpCode of
        $00: //noop
        begin
          Dispatch_NOP(context,common);
          result := true;
        end;
        $01: //SCSI Command
        begin
          Dispatch_SCSI(context, common);
          result := true;
        end;
        $02: //Task management function
        begin
          Dispatch_TaskManagementFunction(context, common);
          result := true;
        end;
        $03: //Login
        begin
          AlertAdmin('iSCSI login event');
          Dispatch_Login(context, common);
          result := true;
        end;
        $04: //Text Request
        begin
          Dispatch_Text(context, common);
          result := true;
        end;
        $05: //Data-Out
        begin
//          if longfound then
//            {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Dataout after longfound***********************************');{$ENDIF}
          Dispatch_DataOut(context, common);
          result := true;
        end;
        $06: //LogOut
        begin
          AlertAdmin('iSCSI logout event');
          Dispatch_Logout(context, common);
          result := true;
        end;


      else begin
        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('**********DON''T UNDERSTAND OPCODE:0x'+inttohex(common.OpCode,2)+' (unmasked: 0x'+inttohex(common.OpCode_UnMasked,2)+')');{$ENDIF}
        AlertAdmin('BAD OPCODE:0x'+inttohex(common.OpCode,2)+' (unmasked: 0x'+inttohex(common.OpCode_UnMasked,2)+')');
        result := false;
        end;
      end;
    finally
      contextdata(context).session.rsTotal.EndTime;
      contextdata(context).session.rsIdle.BeginTime;
    end;
  finally
//    context.connection.IOHandler.CheckForDataOnSource(0);
    getioh(context).WriteBufferFLush;
    //context.Connection.iohandler.WriteBufferClose;
  end;



end;

function TiSCSITargetPortal.Dispatch_DataOut(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): boolean;
var
  cd: TiSCSI_Connection;
  resmem: pbyte;
  res,di: TISCSI_CommonHeader;
  dat: PBYte;
  task: TiSCSI_Task_Write;
  iTotalToWrite, iTotalWritten: nativeint;
  vd: TVirtualDisk;
  dsl,bo: ni;
begin
  //this is where we do stuff
  cd := ContextData(context);

  task := TiSCSI_Task_Write(contextdata(context).session.FindTask(common.InitiatorTaskTAg));
  vd := cd.Target;

  dsl := common.DataSegmentLength;
  bo := common.bufferoffset;
  if task <> nil then begin
    //write data based on offset from original task
    res.InitResponse(common, cd, false);
    if dsl > 0 then begin
      dat := GEtMemory(dsl);
      try
        ctx_guarantee_read(context, dat, dsl);
        {$IFDEF LOCAL_DEBUG}if iscsidebug then lOCALDebug('WRite task LBA :'+inttostr(task.scsirec.LBA)+' off:'+inttostr(bo)+' '+inttostr(dsl)+' bytes.');{$ENDIF}
        vd := cd.Target;
        cd.session.rsDisk.BeginTime;
        vd.WriteData((task.scsirec.LBA*BlockSize)+bo, dsl, dat);
        cd.session.rsDisk.EndTime;
//        if cd.session.rsDisk.NewBAtch then
//          debug.consolelog('rsDisk='+cd.session.rsDisk.DebugString);

        iTotalToWrite := task.scsirec.TransferLengthInBlocks * BlockSize;
        iTotalWritten := bo+dsl;
        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('iTotalToWrite='+inttostr(iTotalToWrite));{$ENDIF}
        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('iTotalWritten='+inttostr(iTotalWritten));{$ENDIF}
{$IFNDEF NOEXP}
        di.InitResponse(common, cd, false);

//        di.Flag_ResidualUnderFlow := iTotalWritten < iTotalToWrite;

        di.ResidualCount := lesserof(iTotalToWrite - iTotalWritten, MaxDataSize(context));
        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('DesiredTransferLength='+inttostr(di.ResidualCount));{$ENDIF}
        di.OpCode_UnMasked := $21;
        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Writing SCSI Response. Haven''t decided if R2t yet');{$ENDIF}
        di.BufferOffset := iTotalWritten;
        di.Flag_ResponseContainsSCSIStatus := di.ResidualCount = 0;
        di.LUN := common.LUN;


        if di.ResidualCount > 0 then begin
          di.OpCode_UnMasked := $31;
          di.ExpCmdSN := int64(common.CmdSN)+int64(1);
          di.MaxCmdSN := int64(contextData(context).ExpCmdSN)+int64(MAX_CMD_AHEAD);

          {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Writing SCSI Response. Responding with R2T, I think.');{$ENDIF}
        end else begin
          di.OpCode_UnMasked := $21;
          di.ExpCmdSN := int64(common.CmdSN)+int64(1);
          di.MaxCmdSN := int64(contextData(context).ExpCmdSN)+int64(MAX_CMD_AHEAD);

          {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Writing SCSI Response. Responding with ScsiResponse, I think.');{$ENDIF}
        end;
        di.Flag_Final := true;//di.ResidualCount = 0;

        //di.cmdsn := common.cmdsn;


        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('About to write (LUN, ITT, DSN, CMDSN): 0x'+inttohex(di.LUN,16)+' 0x'+inttohex(di.InitiatorTaskTAg,8)+' 0x'+inttohex(di.DataSN,8)+' 0x'+inttohex(di.CmdSN,8));{$ENDIF}
        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('In Response to (LUN, ITT, DSN, CMDSN): 0x'+inttohex(common.LUN,16)+' 0x'+inttohex(common.InitiatorTaskTAg,8)+' 0x'+inttohex(common.DataSN,8)+' 0x'+inttohex(common.CmdSN,8));{$ENDIF}
        di.PrepareToSend(common, cd);
        ctx_guarantee_write(context, Pbyte(@di), sizeof(di));
        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Wrote DI');{$ENDIF}
        {$IFDEF FLUSH_IOHANDLER}context.Connection.IOHandler.WriteBufferFlush;{$ENDIF}

        //result.ResponseType := srtResponse_none;

{$ELSE}
        result.ResponseType := srtResponse_0x21;
{$ENDIF}

      finally
        FreeMemory(dat);
      end;
    end else begin
      raise Ecritical.create('don''t understand dataout datasegment length of 0');
    end;
  end else begin
    raise Ecritical.create('don''t understand dataout without task record.');
    res.InitResponse(common, cd, false);

    if dsl > 0 then begin
      dat := GEtMemory(dsl);
      try
        ctx_guarantee_read(context, dat, dsl);
        {$IFDEF LOCAL_DEBUG}if iscsidebug then lOCALDebug('WRite non-task LBA :'+inttostr(task.scsirec.LBA)+'off:'+inttostr(bo)+' '+inttostr(dsl)+' bytes.');{$ENDIF}
        cd.session.rsDisk.BeginTime;
        vd.WriteData((task.scsirec.LBA*BlockSize)+bo, dsl, dat);
        cd.session.rsDisk.EndTime;
        if cd.session.rsDisk.NewBAtch then
          debug.consolelog('rsDisk='+cd.session.rsDisk.DebugString);
      finally
        FreeMemory(dat);
      end;
    end;
  end;


//  if iscsidebug then LocalDebug('Writing DatOut/R2T Response.');
//  IOHandler_GuaranteeWrite(context.Connection.IOHandler, @res, sizeof(res));


  if (common.Flag_Final) and (di.Flag_Final) and (di.residualcount = 0) then begin
    {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Final PDU, clearing task:'+inttohex(common.InitiatorTaskTag,8));{$ENDIF}
    if not di.Flag_ResponseContainsSCSIStatus then
      raise exception.Create('can''t clear a task when the PDU does not contain status.');
    cd.session.ClearTask(common.InitiatorTaskTAg);
  end;

  result := true;


end;

function TiSCSITargetPortal.Dispatch_Login(const context: TiSCSIContext; var common: TISCSI_CommonHeader): boolean;
var
  p: pointer;
  pb, pb2: PByte;
  s: string;
  sResp: ansistring;
  resp: TISCSI_CommonHeader;
  fsz, sz1, sz2, szpad: nativeint;
const
  MAX_IMMEDIATE_DATA_SIZE = 65536*64;
  FIRST_BURST_LENGTH = 65536*64;
  MAX_PDU_LENGTH =MAX_IMMEDIATE_DATA_SIZE+120;

begin
  {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Dispatch_Login');{$ENDIF}

  //Get string data segment
  p := GetMemory(common.DataSegmentLength);
  try
    pb := p;

    ctx_guarantee_read(context, pb, GetPaddedSize(common.DataSegmentLength,4));
    s := StringsFromMemory(pb, common.DataSegmentLength);

    //put the content into the context pairs
    {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug(s);{$ENDIF}
    ContextData(context).login_pairs.loadFromString(s, true);

//    if ContextData(context).loginstage < 2 then begin
      //generate a response with some info
    resp.InitResponse(common, ContextData(context),true);
    resp.Flag_TRansit := true;

    case ContextData(context).loginstage of
      1: begin
      LocalDebug('Login Stage 1');
        if common.Flag_Login_NSG <> 3 then begin
          LocalDebug('common.Flag_Login_NSG= ????? '+common.Flag_Login_NSG.tostring);
        end else begin
          LocalDebug('common.Flag_Login_NSG=3');
          sResp :=  '';
          sResp := ContextData(context).login_pairs.ToString;
          sResp := StringReplace(sResp, #13, '', [rfReplaceAll]);
          sResp := StringReplace(sResp, #10, #0, [rfReplaceAll]);

//          sResp := sResp + 'ErrorRecoveryLevel=0'#0;
//          sResp := sResp + 'MaxConnections=1'#0;
//          sResp := sResp + 'HeaderDigest=None'#0;
//          sResp := sResp + 'DataDigest=None'#0;
//          sResp := sResp + 'ErrorRecoveryLevel=0'#0;
//          sResp := sResp + 'InitialR2T=No'#0;
//          sResp := sResp + 'ImmediateData=Yes'#0;
//          sResp := sResp + 'MaxRecvDataSegmentLength='+inttostr(MAX_IMMEDIATE_DATA_SIZE)+#0;
//          sResp := sResp + 'MaxBurstLength='+inttostr(MAX_IMMEDIATE_DATA_SIZE)+#0;
//          sResp := sResp + 'FirstBurstLength=65536'#0;
//          sResp := sResp + 'DefaultTime2Wait=0'#0;
//          sResp := sResp + 'DefaultTime2Retain=20'#0;
//          sResp := sResp + 'MaxOutstandingR2T=1'#0;
//          sResp := sResp + 'DataPDUInOrder=Yes'#0;
//          sResp := sResp + 'DataSequenceInOrder=Yes'#0;

          if ContextData(context).session.bTSG1Done then begin
            resp.Flag_Login_NSG := 3;
            resp.Flag_Login_CSG := 1;
            resp.TSIH := 512;
          end else begin
            ContextData(context).session.bTSG1Done := true;
            resp.Flag_TRansit := false;
            resp.Flag_Login_NSG := 1;
            resp.Flag_Login_CSG := 1;
            resp.TSIH := 512;

          end;
        end;
      end;
      0: begin
        LocalDebug('Login STAGE 0');
        ContextData(context).EstablishSession(common.CID.tostring);
        ContextData(context).session.bTSG1Done := false;
        sResp := 'TargetPortalGroupTag=1'#0'AuthMethod=None'#0;
        ContextData(context).ExpCmdSN := common.CmdSN+1;
        ContextData(context).NextStatSn := common.ExpStatSN;
        resp.Flag_Login_NSG := 1;
        resp.Flag_Login_CSG := 0;

        //setup default pairs but don't send them
          ContextData(context).login_pairs.AutoAdd := true;
          ContextData(context).login_pairs.Items['ErrorRecoveryLevel'].Value:='0';
          ContextData(context).login_pairs.items['MaxConnections'].value:='1';
          ContextData(context).login_pairs.items['HeaderDigest'].value:='None';
          ContextData(context).login_pairs.items['DataDigest'].value:='None';
          ContextData(context).login_pairs.items['ErrorRecoveryLevel'].value:='0';
          ContextData(context).login_pairs.items['InitialR2T'].value:='No';
          ContextData(context).login_pairs.items['ImmediateData'].value:='Yes';
          ContextData(context).login_pairs.items['MaxRecvPDULength'].value:= inttostr(MAX_PDU_LENGTH);
          ContextData(context).login_pairs.items['MaxRecvDataSegmentLength'].value:=inttostr(MAX_IMMEDIATE_DATA_SIZE);
          ContextData(context).login_pairs.items['MaxBurstLength'].value:=inttostr(MAX_IMMEDIATE_DATA_SIZE);
          ContextData(context).login_pairs.items['FirstBurstLength'].value:=inttostr(FIRST_BURST_LENGTH);
          ContextData(context).login_pairs.items['DefaultTime2Wait'].value:='600';
          ContextData(context).login_pairs.items['DefaultTime2Retain'].value:='600';
          ContextData(context).login_pairs.items['MaxOutstandingR2T'].value:='2';
          ContextData(context).login_pairs.items['DataPDUInOrder'].value:='Yes';
          ContextData(context).login_pairs.items['DataSequenceInOrder'].value:='Yes';

      end;
      else begin
        LocalDebug('unknown login stage!!!!!!!!!!!!!!!!!!');
        sResp := '';
        resp.Flag_Login_NSG := 3;
        resp.Flag_Login_CSG := 1;

      end;
    end;


    contextData(context).ExpCmdSN := 1;//Force 1 for login
    resp.DataSegmentLength := length(sResp);


    resp.ExpCmdSN := int64(common.CmdSN)+int64(1);
    resp.MaxCmdSN := int64(contextData(context).ExpCmdSN)+int64(MAX_CMD_AHEAD);
    resp.StatusClass := 0;
    contextData(context).SetNextStatSN(common.ExpStatSN);
{$IFDEF OLD_STATSN}
    resp.StatSN := common.ExpStatSN;
{$ENDIF}

    resp.PrepareToSend(common, ContextData(context));
    sz1 := sizeof(resp);
    sz2 := GetPAddedSize(length(sResp),4);
    szPad := sz1+length(sResp);
    fsz := sz1+sz2;
    pb2 := GetMemory(fsz);
    try
      movemem32(@pb2[0], @resp, sz1);
      if sz2 > 0 then begin
        movemem32(@pb2[sz1], @sREsp[STRZ()], sz2);
        fillmem(@pb2[szPad], fsz-szPad, 0);
      end;
      LocalDebug('writing LOGIN response');
     ctx_guarantee_write(context, pb2, fsz);
    finally
      FreeMemory(pb2);
    end;
    inc(ContextData(context).loginstage);




  finally
    system.FreeMemory(p);
  end;

  result := true;

end;

function TiSCSITargetPortal.Dispatch_Logout(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): boolean;
var
  pb: PByte;
  s: string;
  resp: TISCSI_CommonHeader;
  fsz, sz1, sz2, szpad: nativeint;
begin
  {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Dispatch_Logout');{$ENDIF}
  resp.InitResponse(common, ContextData(context),true);
  resp.DataSegmentLength := 0;

  resp.ExpCmdSN := int64(common.cmdsn)+int64(1);
  resp.MaxCmdSN := int64(resp.ExpCmdSN)+int64(MAX_CMD_AHEAD);
  resp.LoginStatusClass := 0;
{$IFDEF OLD_STATSN}
  resp.StatSN := common.ExpStatSN;
{$ENDIF}  resp.Time2Wait := 0;
  resp.Time2Retain := $14;

  sz1 := sizeof(resp);
  resp.PrepareToSend(common, ContextData(context));
  ctx_guarantee_write(context, Pbyte(@resp), sz1);


  result := true;
end;

function TiSCSITargetPortal.Dispatch_NOP(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): boolean;
begin
  if common.InitiatorTaskTAg <> $FFFFFFFF then
    SendNOPIn(context, common, false);
  result := true;
end;

function TiSCSITargetPortal.Dispatch_SCSI_ModeSense10(var opcode: TSCSILeadingBytes;
  const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  ms10: TSCSI_ModeSense10;
begin
  {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Dispatch_SCSI_ModeSense10');{$ENDIF}
  ms10.a.AddrForRead(opcode);
  result := Dispatch_SCSI_ModeSense10(context, ms10, common);
end;

function TiSCSITargetPortal.Dispatch_SCSI_Inquery(var opcode: TSCSILeadingBytes;
  const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l12: TSCSI_Inquery;
begin
  {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Dispatch_SCSI_Inquery (outer)');{$ENDIF}
  l12.a.AddrForRead(opcode);
  result := Dispatch_SCSI_Inquery(context, common, l12);
end;


function TiSCSITargetPortal.Dispatch_SCSI_GenericStub10(
  var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  a: array of byte;
begin
  if common.DataSegmentLength > 0 then begin
    setlength(a, common.DataSegmentLength);
    ctx_guarantee_read(context, @a[0], common.DataSegmentLength);
  end;

  result := TSCSI_CommandResponse.Create;
  result.Success := true;
end;

function TiSCSITargetPortal.Dispatch_SCSI_GenericStub6(
  var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  a: array of byte;
begin
  if common.DataSegmentLength > 0 then begin
    setlength(a, common.DataSegmentLength);
    ctx_guarantee_read(context, @a[0], common.DataSegmentLength);
  end;

  result := TSCSI_CommandResponse.Create;
  result.Success := true;
end;


function TiSCSITargetPortal.Dispatch_SCSI_GenericStub12(
  var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  a: array of byte;
begin
  if common.DataSegmentLength > 0 then begin
    setlength(a, common.DataSegmentLength);
    ctx_guarantee_read(context, @a[0], common.DataSegmentLength);
  end;
  result := TSCSI_CommandResponse.Create;
  result.Success := true;
end;

function TiSCSITargetPortal.Dispatch_SCSI_GenericStub16(
  var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  a: array of byte;
begin
  if common.DataSegmentLength > 0 then begin
    setlength(a, common.DataSegmentLength);
    ctx_guarantee_read(context, @a[0], common.DataSegmentLength);
  end;

  result := TSCSI_CommandResponse.Create;
  result.Success := true;
end;

function TiSCSITargetPortal.Dispatch_SCSI_GenericStub32(
  var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  a: array of byte;
begin
  if common.DataSegmentLength > 0 then begin
    setlength(a, common.DataSegmentLength);
    ctx_guarantee_read(context, @a[0], common.DataSegmentLength);
  end;
  result := TSCSI_CommandResponse.Create;
  result.Success := true;
end;

function TiSCSITargetPortal.Dispatch_SCSI_Inquery(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader;
  var scsirec: TSCSI_Inquery): TSCSI_CommandResponse;
const
  STATIC_INQUERY_EVPD0: array[0..95] of byte =
    ( $00,$00,$04,$12, $5b,$00,$00,$02,
      $41,$44,$56,$41, $4e,$43,$45,$44,//ROCKET   //ADVA NCED
      $20,$56,$41,$54, $46,$49,$4c,$45,//IMAGEFIL // VAT FILE
      $20,$20,$20,$20, $20,$20,$20,$20,//E

      $30,$30,$30,$31, $00,$00,$00,$00,$00,$00,                        //0001
      $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
      $03,$20,$09,$60,$02,$76,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
      $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
      $00,$00,$00,$00,$00,$00
    );

  STATIC_INQUERY_00: array[0..6] of byte =
    ($00,$00,$00,$03,$00,$83,$80);
  STATIC_INQUERY_83: array[0..15] of byte =
    ($00,$03,$00,$0c,$01,$02,$00,$08,$5e,$ae,$63,$9d,$a1,$8e,$b0,$0f);
                {len}
  STATIC_INQUERY_80: array[0..19] of byte =
    ($00,$80,$00,$10,$35,$45,$41,$45,$36,$33,$39,$44,$41,$31,$38,$45,$42,$30,$30,$00);


var
  iToWrite: nativeint;
  pres,pp: Pbyte;
  cd: TiSCSI_Connection;
  t,u: ni;
  si: ni;
  ilen, iCount: ni;
  s: string;
  su: ansistring;
  deschead: TSCSIIdentificationDescriptorHeader;

begin
  {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Dispatch_SCSI_Inquery');{$ENDIF}
  result := TSCSI_CommandResponse.Create;
  iToWrite := 0;
  if not scsirec.EVPD then begin
    pRes := @STATIc_INQUERY_EVPD0[0];
    iToWrite := LesserOf(sizeof(STATIC_INQUERY_EVPD0), scsirec.AllocationLength);
    result.Allocate(iToWrite);//allocate handles padding
    result.Original_AllocationLength := common.ExpectedDataTransferLength;
    result.BytesProcessed := result.Length;
    movemem32(result.Data, pREs, iToWRite);

  end else
  case scsirec.PAgeCode of
    $00: begin
      pRes := @STATIC_INQUERY_00[0];
      iToWrite := LesserOf(sizeof(STATIC_INQUERY_00), scsirec.AllocationLength);
      result.Allocate(iToWrite);//allocate handles padding
      result.Original_AllocationLength := common.ExpectedDataTransferLength;
      result.BytesProcessed := result.Length;
      movemem32(result.Data, pREs, iToWRite);

    end;
    $83: begin
{x$DEFINE STATIC_INQUIRY_83}
{$IFDEF STATIC_INQUIRY_83}
      pRes := @STATIC_INQUERY_83[0];
      iToWrite := LesserOf(sizeof(STATIC_INQUERY_83), scsirec.AllocationLength);
      result.Allocate(iToWrite);//allocate handles padding
      result.Original_AllocationLength := common.ExpectedDataTransferLength;
      result.BytesProcessed := result.Length;
      movemem32(result.Data, pREs, iToWRite);
{$ELSE}
      //[   7   ][   6   ][   5   ][   4   ][   3   ][   2   ][   1   ][   0   ]
      //[ Peripheral Qualifier    ][ Peripheral Device Type                    ]
      //[                          0x83                                        ]
      //[ page                                                                 |
      //  length                                                               ]
      s := systemx.GetComputerName+'00';
      iCount := 1;//vdh.vdlist.count;
      iLen := 4+(iCount * ((length(s)*sizeof(ansichar))+sizeof(TSCSIIdentificationDescriptorHeader)));
      pRes := getmemory(iLen);
      pp := @pres[0];
      pp[0] := $00;
      pp[1] := $83;
      pp[2] := (iLen shr 8) and $ff;
      pp[3] := iLen and $ff;
      pp := @pRes[4];
      deschead.Init;
      for t:= 0 to iCount-1 do begin
        su := systemx.getcomputername+inttohex(common.lun, 2);
        deschead.identifier_length := length(su);
        PSCSIIdentificationDescriptorHeader(pp)^ := deschead;
        inc(pp, sizeof(deschead));
        for u := low(su) to high(su) do begin
          pp[0] := ord(ansichar(su[u]));
          inc(pp);
        end;
      end;
      result.Allocate(iLen);//allocate handles padding
      result.Original_AllocationLength := common.ExpectedDataTransferLength;
      result.BytesProcessed := result.Length;
      movemem32(result.Data, pREs, iLen);
      freememory(pres);


{$ENDIF}

    end;
    $80: begin
{$IFDEF STATIC_SERIAL_NUMBER}
      pRes := @STATIC_INQUERY_80[0];
      iToWrite := LesserOf(sizeof(STATIC_INQUERY_80), scsirec.AllocationLength);
      result.Allocate(iToWrite);//allocate handles padding
      result.Original_AllocationLength := common.ExpectedDataTransferLength;
      result.BytesProcessed := result.Length;
      movemem32(result.Data, pREs, iToWRite);
{$ELSE}
      pRes := @STATIC_INQUERY_80[0];
      iToWrite := LesserOf(sizeof(STATIC_INQUERY_80), scsirec.AllocationLength);
      result.Allocate(iToWrite);//allocate handles padding
      result.Original_AllocationLength := common.ExpectedDataTransferLength;
      result.BytesProcessed := result.Length;
      movemem32(result.Data, pREs, iToWRite);
      PByte(result.data)[0] := $00;
      PByte(result.data)[1] := $80;
      PByte(result.data)[2] := $00;
      PByte(result.data)[3] := $10;//serial number length
      ContextData(context).DataSN := 0;
      //this is where we do stuff
      cd := ContextData(context);

      PByte(result.data)[3] := $00;
      for t:= 0 to PByte(result.Data)[3]-1 do begin
        PByte(result.data)[t+4] := $00;
      end;


      s := inttohex(common.LUN,4)+':'+systemx.getcomputername+':'+cd.target.filename;
      for t:= 0 to PByte(result.Data)[3]-1 do begin
        if t < (length(s)+STRZ) then begin
          PByte(result.data)[t+4] := ord(ansichar(s[STRZ+t]));
        end;
      end;

{$ENDIF}

    end;
  else
    raise Exception.Create('Unsupported inquery 0x'+inttohex(scsirec.PAgeCode,2));
  end;




end;

function TiSCSITargetPortal.Dispatch_SCSI_ModeSense10(const context: TiSCSIContext;
  var scsirec: TSCSI_ModeSense10;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
const
  STATIC_MODE_SENSE_PAYLOAD0: array[0..$43] of byte =
                         ($43,$00,$10,$08,$00,$00,$00,$00,$00,$00,
  $02,$00,$04,$16,$00,$04,$00,$20,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$08,$12,$04,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1c,$0a,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

  STATIC_MODE_SENSE_PAYLOAD1: array[0..23] of byte =
                         ($17,$00,$10,$08,$00,$00,$00,$00,$00,$00,
    $02,$00,$1c,$0a,$00,$00,$00,$20,$00,$00,$00,$00,$00,$00);

begin
  //do stuff here
  result := TSCSI_CommandResponse.Create;
  result.Original_AllocationLength := scsirec.AllocationLength;
  result.Allocate(sizeof(STATIC_MODE_SENSE_PAYLOAD0));
  result.Success := true;
  movemem32(result.Data, @STATIC_MODE_SENSE_PAYLOAD0[0], sizeof(STATIC_MODE_SENSE_PAYLOAD0));

end;

function TiSCSITargetPortal.Dispatch_SCSI_ModeSense6(var opcode: TSCSILeadingBytes;
  const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_ModeSense6;
  ms10: TSCSI_ModeSense10;
begin
  l.a.AddrForRead(opcode);
  ms10.From(l);
  result := Dispatch_SCSI_ModeSense10(context, ms10, common);
end;

function TiSCSITargetPortal.Dispatch_SCSI_Read10(var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_Read10;
  l32: TSCSI_REad32;
begin
  l.a.AddrForRead(opcode);
  l32.From(l);
  result := Dispatch_SCSI_Read32(context, common, l32);
end;

function TiSCSITargetPortal.Dispatch_SCSI_Read12(var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_Read12;
  l32: TSCSI_REad32;
begin
  l.a.AddrForRead(opcode);
  l32.From(l);
  result := Dispatch_SCSI_Read32(context, common, l32);
end;


procedure TiSCSITargetPortal.AllocateCombinedAndSend(const context: TiSCSIContext; var common: TISCSI_CommonHeader; di: TISCSI_CommonHeader; r: TSCSI_CommandResponse);
var
  resmem: pbyte;
  useless: ni;
begin
  useless := 0;
    resmem := AllocateCombined(di, r);
    try


      {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Writing SCSI Response.');{$ENDIF}

        Socket_GuaranteeWrite(context.connection, resmem, sizeof(common)+r.PaddedLength);
//      ctx_guarantee_write(context, resmem, sizeof(common)+result.PaddedLength);
      {$IFDEF FLUSH_IOHANDLER}context.Connection.IOHandler.WriteBufferFlush;{$ENDIF}


    finally
      freememory(resmem);
    end;

end;

function TiSCSITargetPortal.Dispatch_SCSI_Read16(var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_Read16;
  l32: TSCSI_REad32;
begin
  l.a.AddrForRead(opcode);
  l32.From(l);
  result := Dispatch_SCSI_Read32(context, common, l32);
end;

function TiSCSITargetPortal.Dispatch_SCSI_Read32(context: TiSCSIContext;
  var common: TISCSI_CommonHeader; var scsirec: TSCSI_Read32): TSCSI_CommandResponse;
var
  cd: TiSCSI_Connection;
  iToRead: nativeint;
  iTotalToRead: nativeint;
  iThisRead: nativeint;
  iThisAddr: int64;
  iPDUCount: nativeint;
  iSent: nativeint;
  MAX_IMMEDIATE_DATA_SIZE: nativeint;
  di: TISCSI_CommonHeader;
  vd: TVirtualDisk;
  useless: nativeint;
  nopCommon: TISCSI_CommonHeader;
  o: TVDReadCommand;
begin
//  Debug.Log('READ32 ITT: 0x'+inttohex(common.InitiatorTaskTag,8));

  nopCommon := common;
//  if iscsidebug then LocalDebug('Read: CmdSN='+common.CmdSN.ToString);
  useless := 0;
  ContextData(context).DataSN := 0;
  //this is where we do stuff
  cd := ContextData(context);

  MAX_IMMEDIATE_DATA_SIZE := cd.MaxImmediateDataSize;

  Result := TSCSI_CommandResponse.Create;
  result.Original_AllocationLength := common.ExpectedDataTransferLength;
  vd := cd.Target;

  iTotalToRead := scsirec.TransferLengthInBlocks * BlockSize;

  iSent := 0;

  di.InitResponse(common, cd, false);

  iPDUCount := 0;

  while iSent < (iTotalToRead) do begin

    iToRead := (lesserof(MAX_IMMEDIATE_DATA_SIZE, iTotalToRead-iSent));

    result.Allocate(iToRead);

    iThisRead := iToRead;
    iThisAddr :=(scsirec.LBA*BlockSize)+iSent;
    {$IFDEF LOCAL_DEBUG}if iscsidebug then lOCALDebug('Read LBA:'+inttostr(scsirec.lba)+' '+inttostr(scsirec.TransferLengthInBlocks)+' blocks. PDU# '+inttostr(iPDUCount)+' This Addr:'+inttostr(iThisAddr)+' This Read: '+inttostr(iToRead));{$ENDIF}

    cd.session.rsDisk.BeginTime;
    vd.ACtiveUseTime := GetTicker;
    o := vd.ReadData_Begin(iThisAddr, iThisRead, result.data);
    while not o.WAitFor(4000) do begin
      Debug.Log(self, 'NOP in Read');
      SendNOPIn(context, common, false);
    end;
    vd.ReadData_End(o);
//    if (iSent+iThisRead) = iTotalToRead then
//      vd.ReadData_Begin(iThisAddr+iThisRead, 262144,nil,true);




    cd.session.rsDisk.EndTime;
    result.BytesProcessed := result.length;
    di.LUN := scsirec.LUN;

    di.OpCode := ord(result.ResponseType);//default 0x25
    di.InitiatorTaskTAg := common.InitiatorTaskTAg;

    di.flag_Ack := false;

    di.DataSEgmentLength := iToRead;
    di.DataSN := 1+contextdata(context).NextDataSN;
    di.TargetTransferTag := common.InitiatorTaskTAg;
    di.StatusClass := STATUS_CLASS_SUCCESS;
    di.ExpCmdSN := int64(common.CmdSN)+int64(1);
    di.Flag_ResidualOverFlow := false;
    //---------------------------------
    di.BufferOffset := iSent;
    //---------------------------------
    iSent := iSent + result.length;
    //---------------------------------


{
    di.Flag_ResponseContainsSCSIStatus := iSent >= iTotalToRead;
    di.Flag_ResidualUnderFlow := iSent < iTotalToRead;
    di.Flag_Final := iSent >= iTotalToRead;
    di.ResidualCount := iTotalToREad - iSent;
    di.LUN := common.LUN;
}
    di.SetOptGRoup2(iSent >= iTotalToRead, iSent < iTotalToRead, iSent >= iTotalToRead, iTotalToREad - iSent, common.LUN);

//    if di.ResidualCount = 0 then begin  <<-- don't do this... generates error in vent log
//      di.opcode := $21;
//    end;
    di.Flag_ResponseContainsSCSIStatus := di.ResidualCount = 0;


    di.PrepareToSend(common, cd);

    AllocateCombinedANdSEnd(context, common, di, result);

{$IFDEF FUCK}
//    try


      {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Writing SCSI Response.');{$ENDIF}
        Socket_GuaranteeWrite(context.connection, resmem, sizeof(common)+result.PaddedLength);
//      ctx_guarantee_write(context, resmem, sizeof(common)+result.PaddedLength);
      {$IFDEF FLUSH_IOHANDLER}context.Connection.IOHandler.WriteBufferFlush;{$ENDIF}


//    finally
      freememory(resmem);
//    end;

{$ENDIF}




    iPDUCount := iPDUCOunt+1;

  end;

  result.ResponseType := srtResponse_None;
//  if iscsidebug then LocalDebug('Read: CmdSN='+common.CmdSN.ToString+' Done');
//  Debug.Log('FINISH ITT: 0x'+inttohex(common.InitiatorTaskTag,8));


end;

procedure TiSCSITargetPortal.SendNOPIn(context: TiSCSIContext;
  var ref: TISCSI_CommonHeader; request_pong: boolean);
var
  cd: TiSCSI_Connection;
  iToRead: nativeint;
  iTotalToRead: nativeint;
  iThisRead: nativeint;
  iThisAddr: int64;
  iPDUCount: nativeint;
  iSent: nativeint;
  MAX_IMMEDIATE_DATA_SIZE: nativeint;
  di: TISCSI_CommonHeader;
  vd: TVirtualDisk;
  useless: nativeint;
  common: TiSCSI_CommonHeader;
begin
  common := ref;
//  if iscsidebug then LocalDebug('Read: CmdSN='+common.CmdSN.ToString);
  useless := 0;
  ContextData(context).DataSN := 0;
  //this is where we do stuff
  cd := ContextData(context);


  MAX_IMMEDIATE_DATA_SIZE := cd.MaxImmediateDataSize;

  di.InitResponse(common, cd, false);
  common.opcode := $20;//NOP
  //common.Flag_ResponseContainsSCSIStatus := true;
  di.PrepareToSend(common, cd);
  if request_pong then begin
    di.InitiatorTaskTAg := $12345678;
    di.TargetTransferTag := $12345678;
  end else begin
    di.InitiatorTaskTAg := $FFFFFFFF;
    di.TargetTransferTag := $FFFFFFFF;
  end;

  Socket_GuaranteeWrite(context.connection, @common, sizeof(common));


end;

function TiSCSITargetPortal.Dispatch_SCSI_Read32(var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l32: TSCSI_REad32;
begin
  //ContextData(context).DataSN := common.ExpDataSN;
  ctx_guarantee_read(context, l32.a.AddrForRead(opcode), sizeof(l32)-sizeof(opcode));
  result := Dispatch_SCSI_Read32(context, common, l32);
end;

function TiSCSITargetPortal.Dispatch_SCSI_Read6(var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_Read6;
  l32: TSCSI_REad32;
begin
  l.a.AddrForRead(opcode);
  l32.From(l);
  result := Dispatch_SCSI_Read32(context, common, l32);
end;

function TiSCSITargetPortal.Dispatch_SCSI_ReadCapacity10(var opcode: TSCSILeadingBytes;
  const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_ReadCapacity10;
begin
  l.a.AddrForRead(opcode);
  result := Dispatch_SCSI_ReadCapacity10(context, l, common);
end;


function TiSCSITargetPortal.Dispatch_SCSI_ReadCapacity10(const context: TiSCSIContext;
  var scsirec: TSCSI_ReadCapacity10;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  rcres: TSCSI_PAYLOAD_ReadCapacity10;
  vd: TvirtualDisk;
  isz: int64;
begin
  vd := ContextData(context).Target;


  isz := vd.Size shr BLOCKSHIFT;
  if isz > $FFFFFFFE then begin
    rcres.LBA := $FFFFFFFF;//<--- out of range for this command
    rcres.BlockSizeInBytes := BlockSize;
  end else begin
    rcres.LBA := cardinal(vd.Size shr BLOCKSHIFT);
    rcres.BlockSizeInBytes := BlockSize;

  end;

  result := TSCSI_CommandResponse.Create;
  Result.Original_AllocationLength := common.ExpectedDataTransferLength;
  result.Allocate(sizeof(rcres));
  result.BytesProcessed := result.Length;

  movemem32(result.Data, @rcres, sizeof(rcres));

end;


function TiSCSITargetPortal.Dispatch_SCSI_ReadCapacity16(
  var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_ReadCapacity16;
begin
  l.a.AddrForRead(opcode);
  result := Dispatch_SCSI_ReadCapacity16(context, l, common);
end;

function TiSCSITargetPortal.Dispatch_SCSI_ReadCapacity16(const context: TiSCSIContext;
  var scsirec: TSCSI_ReadCapacity16;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  rcres: TSCSI_PAYLOAD_ReadCapacity16;
  vd: TvirtualDisk;
  a, b: int64;
begin
//  raise ECritical.create('not supported because scsirec does not contain a LUN');


  vd := ContextData(context).Target;
  rcres.init;
  rcres.LBA := vd.Size shr BLOCKSHIFT;
  rcres.BlockSizeInBytes := BlockSize;


  result := TSCSI_CommandResponse.Create;
  Result.Original_AllocationLength := common.ExpectedDataTransferLength;
  result.Allocate(common.ExpectedDataTransferLength);
  FillMem(@result.Data[0], common.ExpectedDataTransferLength, 0);
  if (common.ExpectedDataTransferLength) > 15 then
    result.Data[14] := $80;//Flag for thin-provisioning


  //result.Allocate(sizeof(rcres));
  //result.BytesProcessed := sizeof(rcres);
  result.BytesProcessed := common.ExpectedDataTransferLength;
//  result.ResidualCount := 0;
  movemem32(result.Data, @rcres, common.ExpectedDataTransferLength);

end;


function TiSCSITargetPortal.Dispatch_SCSI_ReportLUNs12(var opcode: TSCSILeadingBytes;
  const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l12: TSCSI_ReportLuns12;
begin
  l12.a.AddrForRead(opcode);
  result := Dispatch_SCSI_ReportLuns12(context, common, l12);
end;

function TiSCSITargetPortal.Dispatch_SCSI_ReportLUNs12(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader;
  var scsirec: TSCSI_ReportLuns12): TSCSI_CommandResponse;
const
  STATIC_REPORT_LUNS: array[0..15] of byte =
    ($00,$00,$00,$08,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
begin
  result := TSCSI_CommandResponse.Create;
  result.Allocate(sizeof(STATIC_REPORT_LUNS));
  movemem32(result.Data, @static_report_luns[0], sizeof(static_report_luns));
end;

function TiSCSITargetPortal.Dispatch_SCSI_TestUnitReady(
  var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_TestUnitReady;
begin
  l.a.AddrForRead(opcode);
  result := Dispatch_SCSI_TestUnitREAdy(context, l, common);
end;

function TiSCSITargetPortal.Dispatch_SCSI_ServiceActionIn16(
  var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
begin
  result := TSCSI_CommandResponse.create;
  result.Success := false;
end;

function TiSCSITargetPortal.Dispatch_SCSI_SynchronizeCache10(
  var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_SynchronizeCache10;
  l16: TSCSI_SynchronizeCache16;
begin
  l.a.AddrForRead(opcode);
  l16.From(l);
  result := Dispatch_SCSI_SynchronizeCache16(context, common, l16);
end;

function TiSCSITargetPortal.Dispatch_SCSI_SynchronizeCache16(
  var opcode: TSCSILeadingBytes; const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l16: TSCSI_SynchronizeCache16;
begin
  l16.a.AddrForRead(opcode);
  result := Dispatch_SCSI_SynchronizeCache16(context, common, l16);
end;

function TiSCSITargetPortal.Dispatch_SCSI_SynchronizeCache16(
  const context: TiSCSIContext; var common: TISCSI_CommonHeader;
  var scsirec: TSCSI_SynchronizeCache16): TSCSI_CommandResponse;
begin
  result := TSCSI_CommandResponse.Create;
  result.Success := true;
end;

function TiSCSITargetPortal.Dispatch_SCSI_TestUnitReady(
  const context: TiSCSIContext;
  var scsirec: TSCSI_TestUnitReady;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
begin
  result := TSCSI_CommandResponse.Create;
  result.Success := true;

end;

function TiSCSITargetPortal.Dispatch_SCSI_Write10(
  var opcode: TSCSILeadingBytes;
  const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_Write10;
  l32: TSCSI_Write32;
begin
  l.a.AddrForRead(opcode);
  l32.From(l);
  result := Dispatch_SCSI_Write32(context, common, l32);
end;

function TiSCSITargetPortal.Dispatch_SCSI_Write12(
  var opcode: TSCSILeadingBytes;
  const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_Write12;
  l32: TSCSI_Write32;
begin

  l.a.AddrForRead(opcode);
  l32.From(l);
  result := Dispatch_SCSI_Write32(context, common, l32);
end;

function TiSCSITargetPortal.Dispatch_SCSI_Write16(var opcode: TSCSILeadingBytes;
  const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_Write16;
  l32: TSCSI_Write32;
begin
  l.a.AddrForRead(opcode);
  l32.From(l);
  result := Dispatch_SCSI_Write32(context, common, l32);
end;

function TiSCSITargetPortal.Dispatch_SCSI_Write32(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader;
  var scsirec: TSCSI_Write32): TSCSI_CommandResponse;
var
  cd: TiSCSI_Connection;
  dat: PBYte;
  tsk: TiSCSI_Task_Write;
  iTotalToWrite: nativeint;
  iTotalWritten: nativeint;
  di: TiSCSI_CommonHeader;
  resmem: Pbyte;
  vd: TVirtualDisk;
begin
//  SendNOPIn(context, common);
  tsk := nil;
  //this is where we do stuff
  ContextData(context).DataSN := 0;
  cd := ContextData(context);

  Result := TSCSI_CommandResponse.Create;
  result.Original_AllocationLength := common.ExpectedDataTransferLength;

  if scsirec.TransferLengthInBlocks > 0 then begin
    if common.DataSegmentLength = 0 then begin
      {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('0 DataSegmentLEngth ***********************************');{$ENDIF}
      {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug(contextdata(context).login_pairs.ToString);{$ENDIF}
      {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('*******************************************************');{$ENDIF}
    end;
{$IFNDEF CONFUSED}
    if scsirec.TransferLengthinBlocks > 128 then begin
      {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('LONG WRITE *********************************************************************************************************');{$ENDIF}
      longfound := true;
    end;
    if common.DataSEgmentLength > 0 then begin
      dat := GEtMemory(common.DataSEgmentLength);
      try

        cd.session.rsIOHandlerRead.BeginTime;
        ctx_guarantee_read(context, dat, common.DataSegmentLength);
        cd.session.rsIOHandlerRead.EndTime;
        {$IFDEF LOCAL_DEBUG}if iscsidebug then lOCALDebug('Write immediate LBA :'+inttostr(scsirec.lba)+' '+inttostr(scsirec.TransferLengthinBlocks)+'blocks '+inttostr(common.datasegmentlength)+' bytes of immediate data.');{$ENDIF}
        vd := cd.Target;
        iTotalToWrite := scsirec.TransferLengthInBlocks*BLOCKSIZE;
//        if not vd.WaitForQueueSpace(4000) then begin
//          Debug.Log(self, 'NOP in Write');
//          vd.ACtiveUseTime := GetTicker;
//          iTotalWritten := 0;
//          result.BytesProcessed := 0;
//          iTotalWritten := 0;
//        end else begin
          cd.session.rsDisk.BeginTime;
          vd.ACtiveUseTime := GetTicker;
          vd.WriteData(scsirec.LBA*BLOCKSIZE, common.DataSEgmentLength, dat);
          cd.session.rsDisk.EndTime;
          {$IFDEF LOCAL_DEBUG}if iscsidebug then lOCALDebug('Written :'+inttostr(scsirec.lba)+' '+inttostr(common.datasegmentlength)+' bytes.');{$ENDIF}
          result.BytesProcessed := common.DataSegmentLength;
          iTotalWritten := common.DataSEgmentLength;
          {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('iTotalToWrite='+inttostr(iTotalToWrite));{$ENDIF}
          {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('iTotalWritten='+inttostr(iTotalWritten));{$ENDIF}
//        end;


{$IFNDEF NOEXP}
        di.InitResponse(common, cd, false);

//        di.Flag_ResidualUnderFlow := iTotalWritten < iTotalToWrite;
        di.Flag_Final := true;
        di.ResidualCount := lesserof(iTotalToWrite - iTotalWritten, MaxDataSize(context));
//        di.ResidualCount := iTotalToWrite - iTotalWritten;
        di.OpCode_UnMasked := $21;
        di.BufferOffset := iTotalWritten;
        di.Flag_ResponseContainsSCSIStatus := di.ResidualCount = 0;

        if di.ResidualCount > 0 then begin
          di.OpCode_UnMasked := $31;
          di.Flag_Final := true;
          tsk := TiSCSI_Task_Write(cd.session.AddTask(TiSCSI_Task_Write, common));
          tsk.scsirec := scsirec;
          {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Making task because this shit is gonna take a while.'+inttostr(tsk.header.InitiatorTaskTAg));{$ENDIF}
        end;

        di.PrepareToSend(common, cd);
        resmem := AllocateCombined(di, result);
        try
          {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Writing SCSI Response to initial Write request.');{$ENDIF}
          {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('About to write (LUN, ITT, DSN, CMDSN): 0x'+inttohex(di.LUN,16)+' 0x'+inttohex(di.InitiatorTaskTAg,8)+' 0x'+inttohex(di.DataSN,8)+' 0x'+inttohex(di.CmdSN,8));{$ENDIF}
          {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('In Response to (LUN, ITT, DSN, CMDSN): 0x'+inttohex(common.LUN,16)+' 0x'+inttohex(common.InitiatorTaskTAg,8)+' 0x'+inttohex(common.DataSN,8)+' 0x'+inttohex(common.CmdSN,8));{$ENDIF}


          ctx_guarantee_write(context, resmem, sizeof(di)+result.PaddedLength);
          {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Wrote DI');{$ENDIF}
          {$IFDEF FLUSH_IOHANDLER}context.Connection.IOHandler.WriteBufferFlush;{$ENDIF}
        finally
          freememory(resmem);
        end;

        result.ResponseType := srtResponse_none;

{$ELSE}
        result.ResponseType := srtResponse_0x21;
{$ENDIF}

      finally
        FreeMemory(dat);
      end;
    end;

    if (not common.Flag_Final) and (tsk = nil) then begin
      {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Not final PDU, creating WRITE Task');{$ENDIF}
      tsk := TiSCSI_Task_Write(cd.session.AddTask(TiSCSI_Task_Write, common));
//       tsk.header := common;
      tsk.scsirec := scsirec;
      tsk.connection := cd;
    end;


{$ELSE}
    dat := GEtMemory(scsirec.TransferLengthInBlocks*cd.session.BLOCKSIZE);
    try
      cd.ioh_guarantee_read(dat, common.DataSegmentLength);
      fillmem(@dat[common.DataSEgmentLength], (scsirec.TransferLengthInBlocks*cd.session.BLOCKSIZE)-common.DataSEgmentLength, 0);
      {$IFDEF LOCAL_DEBUG}if iscsidebug then lOCALDebug('WRite LBA:'+inttostr(scsirec.lba));{$ENDIF}
      cd.session.vd.WriteBlock(scsirec.LBA, dat);
      result.BytesProcessed := common.DataSEgmentLength;
      result.ResponseType := srtResponse_0x21;
    finally
      FreeMemory(dat);
    end;
{$ENDIF}
  end;

end;

function TiSCSITargetPortal.Dispatch_SCSI_Write32(var opcode: TSCSILeadingBytes;
  const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l32: TSCSI_Write32;
begin
  ctx_guarantee_read(context, l32.a.AddrForRead(opcode), sizeof(l32)-sizeof(opcode));
  result := Dispatch_SCSI_Write32(context, common, l32);
end;

function TiSCSITargetPortal.Dispatch_SCSI_Write6(var opcode: TSCSILeadingBytes;
  const context: TiSCSIContext; var common: TISCSI_CommonHeader): TSCSI_CommandResponse;
var
  l: TSCSI_Write6;
  l32: TSCSI_Write32;
begin
  l.a.AddrForRead(opcode);
  l32.From(l);
  result := Dispatch_SCSI_Write32(context, common, l32);
end;

function TiSCSITargetPortal.Dispatch_SCSI(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): boolean;
var
  b: TSCSILeadingBytes;
  res: TSCSI_CommandResponse;
  resres: TISCSI_CommonHeader;
  resmem: pbyte;
  cmd: byte;
  sError: string;
  cdata: TiSCSI_Connection;
begin
  res := nil;
  try
    //read first byte
    //copy 16 bytes from common header
    movemem32(@b[0], common.AddrOf(sizeof(common)-sizeof(b)), sizeof(b));

    if b[0] = $9e then begin
      cmd := b[1] and $1f
    end else begin
      cmd := b[0] and $ff;
    end;

    {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Scsi cmd: 0x'+inttohex(cmd,2)+' '+SCSICommandToString(cmd));{$ENDIF}

{$IFNDEF ALWAYS_PROCESS_CMDSN}
   if not common.Flag_Immediate then begin
      cdata := contextdata(context);
      if common.cmdsn <> cdata.ExpCmdSN then begin
        if iscsidebug then LocalDebug('NOT EXPECTED COMMAND SN! '+common.cmdsn.tostring);
      end;
      cdata.ExpCmdSN := int64(common.CmdSN)+int64(1);
    end;
{$ENDIF}

    sError := '';
    //dispatch to handler
    //Debug.ConsoleLog('Begin cmd:'+inttohex(cmd,2));
    try
      cmddebug.state := 0;
      cmddebug.cmd := cmd;
      case cmd of
        $08: res := Dispatch_SCSI_Read6(b, context, common);
        $15: res := Dispatch_SCSI_GenericStub6(b, context, common);//verify
        $55: res := Dispatch_SCSI_GenericStub10(b, context, common);//verify
        $28: res := Dispatch_SCSI_Read10(b, context, common);
        $a8: res := Dispatch_SCSI_Read12(b, context, common);
        $88: res := Dispatch_SCSI_Read16(b, context, common);
        //todo 3: fix variable cdb length dispatching
        //$7f: res := Dispatch_SCSI_Read32(b, context, common);
        //--------------------------------------------------
        $1a: res := Dispatch_SCSI_ModeSense6(b, context, common);
        $5a: res := Dispatch_SCSI_ModeSense10(b, context, common);
        //--------------------------------------------------
        $0a: res := Dispatch_SCSI_Write6(b, context, common);
        $2a: res := Dispatch_SCSI_Write10(b, context, common);
        $aa: res := Dispatch_SCSI_Write12(b, context, common);
        $8a: res := Dispatch_SCSI_Write16(b, context, common);
        //todo 3: fix variable cdb length dispatching
        //$7f: res := Dispatch_SCSI_Write32(b, context, common);
        //--------------------------------------------------
        $a0: res := Dispatch_SCSI_ReportLuns12(b, context, common);
        //--------------------------------------------------
        $12: res := Dispatch_SCSI_Inquery(b, context, common);
        $25: res := Dispatch_SCSI_ReadCapacity10(b, context, common);
        $10: res := Dispatch_SCSI_ReadCapacity16(b, context, common);
        //--------------------------------------------------
        $00: res := Dispatch_SCSI_TestUnitReady(b, context,common);
        //--------------------------------------------------
        $9e: res := Dispatch_SCSI_ServiceActionIn16(b, context, common);
        //--------------------------------------------------
        $35: res := Dispatch_SCSI_SynchronizeCache10(b, context, common);
        $91: res := Dispatch_SCSI_SynchronizeCache16(b, context, common);
        //--------------------------------------------------
        $2f: res := Dispatch_SCSI_GenericStub10(b, context, common);//verify
        $af: res := Dispatch_SCSI_GenericStub12(b, context, common);//verify
        $8f: res := Dispatch_SCSI_GenericStub16(b, context, common);//verify
        $7f: res := Dispatch_SCSI_GenericStub32(b, context, common);//verify
      else
        raise ESCSIProtocolError.Create('Unimplemented SCSI command:0x'+inttohex(cmd, 2)+' last known good command was 0x'+inttohex(LAST_GOOD_COMMAND,2));
      end;
      //Debug.ConsoleLog('End cmd:'+inttohex(cmd,2));
      cmddebug.state := 1;
    except
      on E:Exception do begin
        sError := 'Error in SCSI dispatch:'+e.message;
        AlertAdmin(sError);
        res := TSCSI_CommandResponse.Create;
        res.responsetype := srtResponse_0x21;
        res.FSuccess := false;
      end;
    end;



    resres.InitResponse(common, contextdata(context),false);
    resres.OpCode := ord(res.ResponseType);//default 0x25
    resres.InitiatorTaskTAg := common.InitiatorTaskTAg;

    if res.ResponseType = srtDataIn_0x25 then begin
      resres.Flag_Final := true;
      resres.flag_Ack := false;
      resres.Flag_ResponseContainsSCSIStatus := true;

      resres.DataSEgmentLength := res.BytesProcessed;
      resres.DataSN := contextdata(context).NextDataSN;
      resres.TargetTransferTag := resres.DataSN;
      resres.StatusClass := STATUS_CLASS_SUCCESS;
      resres.ExpCmdSN := int64(common.CmdSN)+int64(1);
      resres.Flag_ResidualUnderFlow := res.IsUnderFlow;
      resres.Flag_ResidualOverFlow := res.IsOverFlow;
      resres.ResidualCount := res.ResidualCount;
      resres.Flag_Final := (res.ResidualCount = 0) or (res.IsUnderFlow);
//      Debug.ConsoleLog(booltostr(resres.Flag_Final)+' '+booltostr(res.IsUnderFlow)+' '+booltostr(res.IsOverFlow));
    end else
    if res.ResponseType = srtResponse_0x21 then begin
      resres.Flag_Final := true;
      resres.flag_Ack := false;
      resres.Flag_ResponseContainsSCSIStatus := true;
      resres.ExpDataSN := 0;

      resres.StatusClass := STATUS_CLASS_SUCCESS;
      resres.ExpCmdSN := int64(common.CmdSN)+int64(1);
      resres.Flag_ResidualUnderFlow := false;
      resres.Flag_ResidualOverFlow := false;
      resres.ResidualCount := 0;
      resres.Flag_ResponseContainsSCSIStatus := false;
    end else
    begin
      //
    end;


    if res.ResponseType <> srtResponse_None then begin
      resres.PrepareToSend(common, ContextData(context));
      resmem := AllocateCombined(resres, res);
      try
        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Writing SCSI Response.');{$ENDIF}
        ctx_guarantee_write(context, resmem, sizeof(common)+res.PaddedLength);
      finally
        freememory(resmem);
      end;
    end;

  finally

    res.Free;
  end;
  result := true;
end;

function TiSCSITargetPortal.Dispatch_TaskManagementFunction(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): boolean;
var
  resp: TISCSI_CommonHeader;
begin
  result := true;

  case (common.osb1 and $7f) of
    $05: begin
      Debug.Log('TASK MANAGEMENT function! RESET AFter : 0x'+inttohex(Self.last_good_command,1));
      resp.InitResponse(common, contextdata(context), false);
      resp.StatusClass := 0;
      resp.PrepareToSend(common, ContextData(context));
      resp.StatSN := common.ExpStatSN;
      ctx_guarantee_write(context, Pbyte(@resp), sizeof(resp));
    end;
    $02: begin
      resp.InitResponse(common, contextdata(context), false);
      resp.StatusClass := 0;
      resp.PrepareToSend(common, ContextData(context));
      resp.StatSN := common.ExpStatSN;
      ctx_guarantee_write(context, Pbyte(@resp), sizeof(resp));
    end;
  else
      resp.InitResponse(common, contextdata(context), false);
      resp.StatusClass := 0;
      resp.PrepareToSend(common, ContextData(context));
      ctx_guarantee_write(context, Pbyte(@resp), sizeof(resp));

  end;
end;

function TiSCSITargetPortal.Dispatch_Text(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader; slRequest, slREsponse: Tstringlist): boolean;
var
  s1,s2: string;
begin
  result := true;

  if slRequest.Count < 1 then
    exit;

  SplitString(slREquest[0], '=', s1,s2);
  s1:= lowercase(s1);
  if lowercase(s1) = 'sendtargets' then begin
    Dispatch_Text_SendTargets(context, common, slRequest, slResponse);
  end;

end;

function TiSCSITargetPortal.Dispatch_Text_SendTargets(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader; sRequestSearchQuery: string;
  slREsponse: Tstringlist): boolean;
var
  t: nativeint;
begin
  vdh.Lock;
  try
    for t:= 0 to vdh.vdlist.Count-1 do begin
  //    slResponse.Add('TargetName='+iqn.2008-08.com.digitaltundrallc:ssdhog.zivix.net-bollacks');
      slResponse.Add('TargetName='+vdh.vdList[t].Identifier);
    end;
  finally
    vdh.unlock;
  end;

  result := true;
end;

function TiSCSITargetPortal.MaxDataSize(const ctx: TiSCSIContext): nativeint;
begin
  result := ContextData(ctx).login_pairs.GetItemEx('MaxRecvDataSegmentLength', 65536);

end;

function TiSCSITargetPortal.Dispatch_Text_SendTargets(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader; slRequest, slREsponse: Tstringlist): boolean;
var
  t: nativeint;
  s1,s2: string;
begin
  result := false;
  for t:= 0 to slRequest.Count-1 do begin
    if splitString(slRequest[t], '=', s1,s2) then begin
      result := result or Dispatch_Text_SendTargets(context, common, s1, slREsponse);
    end;
  end;

end;

function TiSCSITargetPortal.Dispatch_Text(const context: TiSCSIContext;
  var common: TISCSI_CommonHeader): boolean;
var
  p: pointer;
  pb, pb2: PByte;
  s: string;
  sResp: ansistring;
  resp: TISCSI_CommonHeader;
  fsz, sz1, sz2, szpad: nativeint;
  slREquestStrings, slREsponseStrings: TStringlist;
begin
  slREquestStrings := nil;
  slREsponseStrings := nil;
  //Get string data segment
  p := GetMemory(common.DataSegmentLength);
  try
    slREquestStrings := TStringlist.create;
    slREsponseStrings := TStringlist.create;
    pb := p;

    ctx_guarantee_read(context, pb, GetPaddedSize(common.DataSegmentLength,4));
    s := StringsFromMemory(pb, common.DataSegmentLength);
    slRequestSTrings.text := s;

    {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Text Request: '+s);{$ENDIF}

    slRequestStrings.text := s;

    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------

    Dispatch_Text(context, common, slRequestStrings, slREsponseStrings);

    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------
    //-------------------------------------------------------------------------

    //generate a response with some info
    resp.InitResponse(common, ContextData(context),false);

    sResp := slResponseStrings.text+#0;
    sResp := stringreplace(sResp, #13#10, #8, [rfReplaceAll]);
    sResp := stringreplace(sResp, #13, #8, [rfReplaceAll]);
    sResp := stringreplace(sResp, #10, #8, [rfReplaceAll]);
    sResp := stringreplace(sResp, #8, #0, [rfReplaceAll]);
    sResp := stringreplace(sResp, #0#0, #0, [rfReplaceAll]);
    sResp := stringreplace(sResp, #0#0, #0, [rfReplaceAll]);

    resp.DataSegmentLength := length(sResp);

    resp.ExpCmdSN := contextData(context).ExpCmdSN;
    resp.MaxCmdSN := int64(contextData(context).ExpCmdSN)+int64(MAX_CMD_AHEAD);
    resp.StatusClass := 0;
{$IFDEF OLD_STATSN}
    resp.StatSN := common.ExpStatSN;
{$ENDIF}
    resp.TargetTransferTag := $FFFFFFFF;
    resp.LUN := 0;



    resp.PrepareToSend(common, ContextData(context));
    sz1 := sizeof(resp);
    sz2 := GetPAddedSize(length(sResp),4);
    szPad := sz1+length(sResp);
    fsz := sz1+sz2;
    pb2 := GetMemory(fsz);
    try
      movemem32(@pb2[0], @resp, sz1);
      if sz2 > 0 then begin
        movemem32(@pb2[sz1], @sREsp[STRZ()], sz2);
        fillmem(@pb2[szPad], fsz-szPad, 0);
      end;
      ctx_guarantee_write(context, pb2, fsz);
    finally
      FreeMemory(pb2);
    end;
    //inc(ContextData(context).loginstage);

  finally
    slREquestSTrings.free;
    slREsponseStrings.free;
    system.FreeMemory(p);
  end;


  result := true;
end;


procedure TiSCSITargetPortal.OntcpsExecute(AContext: TiSCSIContext);
var
  common: TISCSI_CommonHeader;
begin
  if AContext = nil then
    exit;

  while acontext.Connection.Connected do begin
    rsTCP3.EndTime;
    {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Start of Processing');{$ENDIF}

    if Acontext.Data = nil then begin
      Acontext.Data := TiSCSI_Connection.create;
      contextdata(Acontext).connection := Acontext.connection;
    end;

    rsTCP1.BeginTime;
    ctx_guarantee_read(Acontext, Pbyte(@common), sizeof(common));
    rsTCP1.EndTime;
    rsTCP2.BeginTime;
    if contextdata(Acontext).session <> nil then begin
      contextdata(Acontext).session.lock;
      try
        Dispatch(acontext, common);
      finally
        contextdata(Acontext).session.unlock;
      end;
    end else begin
      Dispatch(acontext, common);
    end;
    rsTCP2.EndTime;
    rsTCP3.beginTime;
//    if rsTCp1.NewBAtch then
//      debug.ConsoleLog('TCP1='+rsTCP1.DebugString);
//    if rsTCp2.NewBAtch then
//      debug.ConsoleLog('TCP1='+rsTCP1.DebugString);
  end;


end;

procedure TiSCSITargetPortal.Start;
var
  sh: TDTSocketHandle;
  ap: TAppParams;
begin
  ap := NeedAppParams;
  try
    tcps := TlocalTCPServer.Create(nil);

{$IFDEF INDY}

    sh := tcps.Bindings.Add;
    sh.IP := '0.0.0.0';
    sh.Port := ap.GetItemEx('Port', 3260);
    tcps.OnExecute := self.OntcpsExecute;
    herro.RegisterLocalSkill('iSCSI', 0, tcps.DefaultPort.tostring, 'iSCSI');
    tcps.Active := true;
{$ELSE}
    tcps.BlockMode := bmThreadBlocking;
    tcps.LocalPort := ap.GetItemEx('Port', '3260');
    tcps.OnAccept := self.TcpServerAccept;
    tcps.Active := true;
    herro.RegisterLocalSkill('iSCSI', 0, tcps.localport, 'iSCSI');
{$ENDIF}



  finally
    NoNeedAppParams(ap);
  end;
end;

procedure TiSCSITargetPortal.Stop;
begin
  vdh.preShutdown;

  if assigned(tcps) then begin
    tcps.Active := false;
    tcps.Free;
  end;
  tcps := nil;
end;

{$IFNDEF INDY}
procedure TiSCSITargetPortal.TcpServerAccept(Sender: TObject;
  ClientSocket: TBetterCustomIpClient);
var
  ctx: TiSCSIContext;
  common: TISCSI_CommonHeader;
  thr: TBetterClientSocketThread;
begin

  ctx := TiSCSIContext.create;
  try
    ctx.data := TiSCSI_Connection.create;
    ctx.Data.context := ctx;
    ctx.Connection := clientsocket;
    thr := clientsocket.getthreadobject;
    thr.stepcount := 1;
    try
      while clientsocket.Connected do begin
        if clientsocket.KillMe then
          exit;

        rsTCP3.endtime;
//        if rsTCp3.NewBAtch then
//          debug.ConsoleLog('TCP3='+rsTCP1.DebugString);
        rsTCP1.BeginTime;

//        ClientSocket.GetThreadObject.Status := 'Idle/Reading';
        thr.Step := 0;

        CurrentThreadObject.EndActiveTime;
        ctx_guarantee_read(ctx, Pbyte(@common), sizeof(common));
        CurrentThreadObject.BeginActivetime;
        rsTCP1.EndTime;
        rsTCP2.BeginTime;
        thr.step := 1;
//        ClientSocket.GetThreadObject.Status := 'Dispatching';
        Dispatch(ctx, common);
//        ClientSocket.GetThreadObject.Status := 'Dispatched';

        rsTCP2.EndTime;
        rsTCP3.beginTime;

//        if rsTCp1.NewBAtch then
//          debug.ConsoleLog('TCP1='+rsTCP1.DebugString);
//        if rsTCp2.NewBAtch then
//          debug.ConsoleLog('TCP2='+rsTCP1.DebugString);

      end;
    finally
      if assigned(ctx.data) then begin
        ctx.data.free;
        ctx.data := nil;
      end;
    end;
  finally
    if assigned(ctx) then
      ctx.Free;
  end;
end;
{$ENDIF}

{ TISCSI_ISID }


{ TISCSI_LoginRequestHeader }



{ TISCSI_CommonHeader }

function TISCSI_CommonHeader.AddrOf(byte: nativeint): PByte;
begin
  result := pbyte(@self) + byte;
end;



function TISCSI_CommonHeader.GetBufferOffset: cardinal;
begin
  result := NetBytes.AsCardinal(addrof(40));
end;

function TISCSI_CommonHeader.GetCID: smallint;
begin
  result := NetBytes.AsSmallint(addrof(20));
end;

function TISCSI_CommonHeader.GetCmdSN: cardinal;
begin
  result := NetBytes.AsCardinal(addrof(24));
end;

function TISCSI_CommonHeader.GetDataSegmentLength: cardinal;
begin
  result := NetBytes.As3ByteUnsignedInt(AddrOf(5));
end;

function TISCSI_CommonHeader.GetDataSN: cardinal;
begin
  result := AsCardinal(addrof(36));
end;

function TISCSI_CommonHeader.GetEDTL: cardinal;
begin
  result := AsCardinal(Addrof(20));
end;

function TISCSI_CommonHeader.GetExpCmdSN: cardinal;
begin
  result := Netbytes.AsCardinal(AddrOf(28));
end;

function TISCSI_CommonHeader.GetExpStatSN: cardinal;
begin
  result := NetBytes.AsCardinal(AddrOf(28));
end;

function TISCSI_CommonHeader.GEtFlagFinal: boolean;
begin
  result := (osb1 and $80) > 0;
end;

function TISCSI_CommonHeader.GetFlagimmediate: boolean;
begin
  result := (OpCode_UnMasked and $40) > 0
end;

function TISCSI_CommonHeader.GEtFlagAckRequested: boolean;
begin
  result := (osb1 and $40) > 0;
end;

function TISCSI_CommonHeader.GEtFlagTRansit: boolean;
begin
  result := (AddrOf(1)[0] and (1 shl 7) <> 0);
end;

function TISCSI_CommonHeader.GEtFlag_Login_CSG: byte;
begin
  result := (osb1 and (MASK_CSG)) shr 2;
end;

function TISCSI_CommonHeader.GEtFlag_Login_NSG: byte;
begin
  result := (osb1 and (MASK_NSG)) shr 0;

end;

function TISCSI_CommonHeader.GetITT: cardinal;
begin
  result := AsCardinal(Addrof(16));
end;

function TISCSI_CommonHeader.GetLoginStatusClass: byte;
begin
  result := AddrOf(36)[0];
end;

function TISCSI_CommonHeader.GetLoginStatusDetail: byte;
begin
  result := AddrOf(37)[0];
end;

function TISCSI_CommonHeader.GetLUN: int64;
begin
  result := AsInt64(AddrOf(8));
end;

function TISCSI_CommonHeader.getMaxCmdSN: cardinal;
begin
  result := Netbytes.AsCardinal(AddrOf(32));
end;

function TISCSI_CommonHeader.getOpCodeFlags: byte;
begin
  result := AddrOf(0)[0] and $B0;
end;

function TISCSI_CommonHeader.GetOpSpec_LUN: cardinal;
var
  c: cardinal;
begin
  result := NetBytes.AsCardinal(AddrOf(8));
end;

function TISCSI_CommonHeader.GetResidualCount: cardinal;
begin
  result := AsCardinal(Addrof(44));
end;

function TISCSI_CommonHeader.GEtResidualOverflow: boolean;
begin
  result := BitGet(Addrof(1), 2);
end;

function TISCSI_CommonHeader.GEtResidualUnderflow: boolean;
begin
  result := BitGet(Addrof(1), 1);
end;

function TISCSI_CommonHeader.GEtResponseContainsSCSIStatus: boolean;
begin
  result := BitGet(@osb1, 0);
end;

function TISCSI_CommonHeader.GetServiceAction: byte;
begin
  result := AddrOf(1)[0] and $1f;
end;

function TISCSI_CommonHeader.GetSnackTag: cardinal;
begin
  result := ascardinal(Self.AddrOf(20));
end;

function TISCSI_CommonHeader.GetStatSN: cardinal;
begin
  result := Netbytes.AsCardinal(AddrOf(24));
end;

function TISCSI_CommonHeader.GetStatusClass: byte;
begin
  result := AddrOf(3)^;
end;

function TISCSI_CommonHeader.GetStatusDetail: byte;
begin
  result := AddrOf(4)^;
end;

function TISCSI_CommonHeader.GetTArgetTransferTag: cardinal;
begin
  result := AsCardinal(AddrOf(20));
end;

function TISCSI_CommonHeader.GetTime2Retain: word;
begin
  result := AsWord(AddrOf(42));
end;

function TISCSI_CommonHeader.GetTime2Wait: word;
begin
  result := AsWord(AddrOf(38));
end;

function TISCSI_CommonHeader.getTotalAHSLength: byte;
begin
  result := NetBytes.As3ByteUnsignedInt(AddrOf(5));
end;

function TISCSI_CommonHeader.GetTSIH: smallint;
begin
  RESULT := NetBytes.AsSmallint(AddrOf(14));
end;

procedure TISCSI_CommonHeader.InitResponse(req: TISCSI_CommonHeader; ctx: TiSCSI_Connection; bForLogin: boolean);
begin
  fillmem(pointer(@self), sizeof(self), 0);
  self.OpCode := req.OpCode + $20;//default response is generally request + $20... not always
  movemem32(AddrOf(8), req.AddrOf(8), 8);  //Copy IANA stuff from source
  self.InitiatorTaskTag := req.InitiatorTaskTag;
{$IFDEF NEWCMDSN}
//  ExpCmdSN := int64(ctx.ExpCmdSN);
  MaxCmdSN := int64(req.CmdSN)+int64(MAX_CMD_AHEAD);
{$ELSE}
  MaxCmdSN := req.ExpCmdSN+int64(MAX_CMD_AHEAD);
{$ENDIF}
  if not bForLogin then begin
{$IFDEF OLD_STATSN}
    self.StatSN := req.ExpStatSN;
{$ELSE}
    self.StatSN := 0;
{$ENDIF}

  end;
  Flag_Final := true;

  if req.OpCode = $9e then begin
    self.ServiceACtion := req.ServiceACtion;
  end;

end;

procedure TISCSI_CommonHeader.PrepareToSend(req: TISCSI_CommonHeader;
  ctx: TiSCSI_Connection);
begin


  if Flag_ResponseContainsSCSIStatus then begin
{$IFDEF OLD_STATSN}
    StatSN := req.ExpStatSN;
{$ELSE}
    StatSN := ctx.NextStatSn;
{$ENDIF}
  end;
{$IFDEF LOCAL_DEBUG}
  if self.Flag_ResponseContainsSCSIStatus then
    StatDebug('+StatSN: 0x'+inttohex(statsn, 8))
  else
    StatDebug('-StatSN: 0x'+inttohex(statsn, 8));
{$ENDIF}

end;

function TISCSI_CommonHeader.GetOpCode: byte;
begin
  result := OpCode_Unmasked and OPCODE_MASK;
end;

procedure TISCSI_CommonHeader.SetBufferOffset(const Value: cardinal);
begin
  NetBytes.FromCardinal(addrof(40), value);
end;

procedure TISCSI_CommonHeader.SetCID(const Value: smallint);
begin
  NetBytes.FromSmallint(addrof(20), value);
end;

procedure TISCSI_CommonHeader.SetCmdSN(const Value: cardinal);
begin
  NetBytes.FromCardinal(addrof(24),value);
end;

procedure TISCSI_CommonHeader.SetDataSegmentLength(const Value: cardinal);
begin
  NetBytes.From3ByteUnsignedInt(AddrOf(5), value);
end;

procedure TISCSI_CommonHeader.SetDAtaSn(const Value: cardinal);
begin
  NEtbytes.FromCardinal(addrof(36), value);
end;

procedure TISCSI_CommonHeader.SetEDTL(const Value: cardinal);
begin
  FromCardinal(Addrof(20), value);
end;

procedure TISCSI_CommonHeader.SetExpCmdSN(const Value: cardinal);
begin
  NEtbytes.FromCardinal(addrof(28), value);
end;

procedure TISCSI_CommonHeader.SetExpStatSN(const Value: cardinal);
begin
  NetBytes.FromCardinal(AddrOf(28), value);
end;

procedure TISCSI_CommonHeader.SetFlagFinal(const Value: boolean);
begin
  if value then
    osb1 := osb1 or $80
  else
    osb1 := osb1 and (not $80);

end;

procedure TISCSI_CommonHeader.SEtFlagImmediate(const Value: boolean);
begin
  if value then
    opcode_unmasked := opcode_unmasked or $40
  else
    opcode_unmasked := opcode_unmasked and (not $40);

end;

procedure TISCSI_CommonHeader.SetFlagAckRequested(const Value: boolean);
begin
  BitSet(@osb1, 6, value);
end;

procedure TISCSI_CommonHeader.SetFlagTRansit(const Value: boolean);
begin
  if value then
    AddrOf(1)[0] := AddrOf(1)[0] or ( (1 shl 7))
  else
    AddrOf(1)[0] := AddrOf(1)[0] and (not (1 shl 7));
end;

procedure TISCSI_CommonHeader.SetFlag_Login_CSG(
  const Value: byte);
begin
  osb1 := osb1 and (not MASK_CSG);
  osb1 := osb1 or (value shl 2);
end;

procedure TISCSI_CommonHeader.SetFlag_Login_NSG(
  const Value: byte);
begin
  osb1 := osb1 and (not MASK_NSG);
  osb1 := osb1 or (value shl 0);
end;

procedure TISCSI_CommonHeader.SetITT(const Value: cardinal);
begin
  FromCardinal(AddrOf(16), value);
end;

procedure TISCSI_CommonHeader.SetLoginStatusClass(const Value: byte);
begin
  AddrOf(36)[0] := value;
end;

procedure TISCSI_CommonHeader.SetLoginStatusDetail(const Value: byte);
begin
  AddrOf(37)[0] := value;
end;

procedure TISCSI_CommonHeader.SetLUN(const Value: int64);
begin
  FromInt64(AddrOf(8), value);
end;

procedure TISCSI_CommonHeader.SetMaxCmdSn(const Value: cardinal);
begin
  NEtbytes.FromCardinal(addrof(32), value);
end;

procedure TISCSI_CommonHeader.SetOpCode(const Value: byte);
begin
  AddrOf(0)[0] := AddrOf(0)[0] and (not $3f);
  AddrOf(0)[0] := AddrOf(0)[0] or value;
end;

procedure TISCSI_CommonHeader.setOpCodeFlags(const Value: byte);
begin
  AddrOf(0)[0] := AddrOf(0)[0] and (not $B0);
  AddrOf(0)[0] := AddrOf(0)[0] or value;
end;

procedure TISCSI_CommonHeader.SetOpSpec_LUN(const Value: cardinal);
var
  c: cardinal;
begin
  NetBytes.FromCardinal(AddrOf(8), value);

end;

procedure TISCSI_CommonHeader.SetOptGroup2(const a, b, c: boolean; const d, lun: ni);
begin
{$IFNDEF SUPERSLOW}
  BitSet(@osb1, 0, a);
  BitSet(Addrof(1), 1, b);
  if c then
    osb1 := osb1 or $80
  else
    osb1 := osb1 and (not $80);
  FromCardinal(AddrOf(44), d);
  FromInt64(AddrOf(8), lun);
{$ELSE}
  SetResponseContainsSCSIStatus(a);
  SEtResidualUnderFlow(b);
  SetFlagFinal(c);
  SetResidualCount(d);
  self.LUN := lun;
{$ENDIF}

end;

procedure TISCSI_CommonHeader.SetResidualCount(const Value: cardinal);
begin
  FromCardinal(AddrOf(44), value);
end;

procedure TISCSI_CommonHeader.SEtResidualOverflow(const Value: boolean);
begin
  BitSet(Addrof(1), 2, value);
end;

procedure TISCSI_CommonHeader.SEtResidualUnderflow(const Value: boolean);
begin
  BitSet(Addrof(1), 1, value);
end;

procedure TISCSI_CommonHeader.SetResponseContainsSCSIStatus(
  const Value: boolean);
begin
  BitSet(@osb1, 0, value);
end;

procedure TISCSI_CommonHeader.SetServiceAction(const Value: byte);
begin
  AddrOf(1)[0] := AddrOf(1)[0] and (not $1f);
  AddrOf(1)[0] := AddrOf(1)[0] or value;
end;

procedure TISCSI_CommonHeader.SetSnackTAg(const Value: cardinal);
begin
  Fromcardinal(Self.AddrOf(20), value);
end;

procedure TISCSI_CommonHeader.SetStatSN(const Value: cardinal);
begin
  NEtbytes.FromCardinal(addrof(24), value);
end;

procedure TISCSI_CommonHeader.SetStatusClass(const Value: byte);
begin
  AddrOf(3)[0] := value;
end;

procedure TISCSI_CommonHeader.SetStatusDetail(const Value: byte);
begin
  AddrOf(4)[0] := value;
end;

procedure TISCSI_CommonHeader.SEtTargetTransferTAg(const Value: cardinal);
begin
  FromCardinal(addrof(20), value);
end;

procedure TISCSI_CommonHeader.SetTime2Retain(const Value: word);
begin
  FromWord(addrof(42), value);
end;

procedure TISCSI_CommonHeader.SetTime2Wait(const Value: word);
begin
  FromWord(addrof(38), value);
end;

procedure TISCSI_CommonHeader.setTotalAHSLength(const Value: byte);
begin
  NEtBytes.From3ByteUnsignedInt(AddrOf(5), value);
end;


procedure TISCSI_CommonHeader.SetTSIH(const Value: smallint);
begin
  NEtBytes.FromSmallint(addrof(14), value);
end;


{ TISCSI_AdditionalHeaderSection }


function TISCSI_AdditionalHeaderSection.GetAHSType_Flag_63Reserved: boolean;
begin
  result := (ahstype and (1 shl 5)) <> 0;
end;

function TISCSI_AdditionalHeaderSection.GetAHSType_Flag_EpectedBidirectionalReadDataLength: boolean;
begin
  result := (ahstype and (1 shl 4)) <> 0;
end;

function TISCSI_AdditionalHeaderSection.GetAHSType_Flag_ExtendedCDB: boolean;
begin
  result := (ahstype and (1 shl 3)) <> 0;
end;

function TISCSI_AdditionalHeaderSection.GetAHSType_Flag_Reserved: boolean;
begin
  result := (ahstype and (1 shl 2)) <> 0;
end;

procedure TISCSI_AdditionalHeaderSection.InitResponse(req: TISCSI_CommonHeader);
begin
  fillmem(pointer(@self), sizeof(self), 0);


end;

procedure TISCSI_AdditionalHeaderSection.SetAHSType_Flag_63Reserved(
  const Value: boolean);
begin
  if value then
    ahstype := (ahstype or ((1 shl 5)))
  else
    ahstype := (ahstype and (not (1 shl 5)));
end;

procedure TISCSI_AdditionalHeaderSection.SetAHSType_Flag_EpectedBidirectionalReadDataLength(
  const Value: boolean);
begin
  if value then
    ahstype := (ahstype or ((1 shl 4)))
  else
    ahstype := (ahstype and (not (1 shl 4)));


end;

procedure TISCSI_AdditionalHeaderSection.SetAHSType_Flag_ExtendedCDB(
  const Value: boolean);
begin
  if value then
    ahstype := (ahstype or ((1 shl 3)))
  else
    ahstype := (ahstype and (not (1 shl 3)));

end;

procedure TISCSI_AdditionalHeaderSection.SetAHSType_Flag_Reserved(
  const Value: boolean);
begin
  if value then
    ahstype := (ahstype or ((1 shl 2)))
  else
    ahstype := (ahstype and (not (1 shl 2)));

end;

{ TiSCSI_Context }

procedure TiSCSI_Connection.ApplySNs(var header: TiSCSI_CommonHeader);
begin
  //todo 2: go through header and determine how this command affects the SNs
end;

constructor TiSCSI_Connection.Create;
begin
  inherited;
  login_pairs := TNameValuePairList.Create;
  localpairs := TNameValuePairList.create;
  session := nil;//global
  debugpinger := TDebugPinger.Create;
  iScsi.debugpinger := debugpinger;
//  Self.ioprefetcher := TPM.NeedThread<TExternalEventThread>(self);
//  ioprefetcher.Loop := true;
//  ioprefetcher.onExecute := self.ioh_prefetcher_execute;
//  ioprefetcher.start;
{$IFNDEF INDY}
  fifo_incoming := TSocketRingBuffer.create;
  fifo_incoming.Size := 1*MEGA;
  fifo_incoming.Socket := self.connection;
{$ENDIF}
  ExpCmdSn := 1;



end;

destructor TiSCSI_Connection.Destroy;
begin
//  TPM.NoNeedthread(ioprefetcher);
  debugpinger.Free;
  debugpinger := nil;
  localpairs.free;
  localpairs := nil;
  login_pairs.Free;
  login_pairs := nil;
{$IFNDEF INDY}
  fifo_incoming.free;
  fifo_incoming := nil;
{$ENDIF}
  inherited;
end;

procedure TiSCSI_Connection.EstablishSession(sid: string);
begin
  session := scsi_session_manager.GetSession(sid);
end;

function TiSCSI_Connection.GetMaxImmediateDataSize: ni;
begin
  if max_imm = 0 then
    max_imm := login_pairs.GetItemEx('MaxRecvDataSegmentLength', 262144);

  result := max_imm;
end;

function TiSCSI_Connection.GEtNextDataSn: cardinal;
begin
  result := DataSN;
  inc(DataSN);

end;

function TiSCSI_Connection.GetNextStatSN: cardinal;
begin
  inc(FNExtStatSN);
  result := FNExtStatSN;
end;

{$IFDEF INDY}
procedure TiSCSI_Connection.ioh_guarantee_read(p: pbyte; iLength: nativeint);
var
  idb: TIDBytes;
  iToRead: nativeint;
  iRead, iJustRead: nativeint;
  pp: pbyte;
begin
  Lock;
  try
{$IFDEF USE_FIFO_ON_READ}
    iRead := 0;
    //if there is nothing to do then sleep
    while ((connection.IOHandler.InputBuffer.Size = 0) and (not fifo_incoming.IsDataAvailable)) do begin
      sleep(1);
      if not connection.Connected then
        raise ENetworkError.create('connection dropped in ioh_guarantee_read');
    end;

    //loop until all the desired data has been read
    while iRead < iLength do begin
      if not connection.Connected then
        raise Exception.create('connection dropped in ioh_guarantee_read');

//      connection.IOHandler.CheckForDataOnSource(0);
      //amount to read is lesser of available data on indy pipe
      //or amount of data space available in the FIFO
      //figure out how much data we can actually red
      iToRead := lesserof(fifo_incoming.BufferSpaceAvailable,connection.IOHandler.InputBuffer.Size);


//{$IFDEF IOH_SINGLE_THREAD}

      //if there's stuff to read

      if iToRead > 0 then begin
        setlength(idb, 0);
        //read the maximum data we can fit into the fifo
        //connection.IOHandler.ReadBytes(idb, iToRead, false);

        pp := GetMemory(iToRead);
        try
          IOHandler_GuaranteeRead(connection.IOHandler, pp, iToRead);
          fifo_incoming.BufferData(@pp[0], iToRead);

        finally
          FreeMemory(pp);
        end;

        //put this data in the fifo
        //fifo_incoming.BufferData(@idb[0], length(idb));
      end;
//{$ENDIF}

      iJustRead := fifo_incoming.GetAvailableChunk(@p[iRead], iLength-iRead);
//      if iJustRead = 0 then begin
//        IOHandler_GuaranteeRead(connection.IOHandler, p, iLength);
//        iJustRead := iLength;
//      end;

      inc(iREad, iJustRead);

    end;

{$ELSE}
    IOHandler_GuaranteeRead(connection.IOHandler, p, iLength);
{$ENDIF}
  finally
    Unlock;
  end;
end;
{$ENDIF}

{$IFDEF INDY}
procedure TiSCSI_Connection.ioh_guarantee_write(p: pbyte; iLength: nativeint);
begin
  Lock;
  try
    IOHandler_GuaranteeWrite(connection.IOHandler, p, iLength);
  finally
    Unlock;
  end;

end;
{$ENDIF}

procedure TiSCSI_Connection.SetNextStatSN(const Value: cardinal);
begin
  FNextStatSN := value;
end;

function TiSCSI_Connection.Target: TVirtualDisk;
begin
  result := session.FindTarget(login_pairs.GetItemEx('TargetName',''));
end;

{ TISCSI_ISID }

function TISCSI_ISID.GetB: word;
begin
  result := _b;
  EndianSwap(Pbyte(@result), 2);
end;

function TISCSI_ISID.GetD: word;
begin
  result := _d;
  EndianSwap(Pbyte(@result), 2);
end;

procedure TISCSI_ISID.SetB(b: word);
begin

  _b := b;
  EndianSwap(Pbyte(@_b), 2);

end;

procedure TISCSI_ISID.SetD(d: word);
begin
  _d := d;
  EndianSwap(Pbyte(@_d), 2);
end;

{ TSCSI_CommandResponse }

procedure TSCSI_CommandResponse.Allocate(const iSizE: nativeint);
begin
  if iSize = FLength then
    exit;

  if assigned(Fdata) then begin
    FreeMemory(FData)  ;
    FData := nil;
  end;
  FData := GetMemory(iSize);
  FLength := iSize;
end;

constructor TSCSI_CommandResponse.Create;
begin
  inherited;
  fResponseType := srtDataIn_0x25;
end;

destructor TSCSI_CommandResponse.Destroy;
begin
  if FData <> nil then
    FreeMemory(FData);

  inherited;
end;

function TSCSI_CommandResponse.GetIsOverflow: boolean;
begin
  result := FLength > FAllocationLength;
end;

function TSCSI_CommandResponse.GetIsUnderflow: boolean;
begin
  result := FLength < FAllocationLength;

end;

function TSCSI_CommandResponse.GEtPaddedLength: nativeint;
begin
  result := GetPaddedSize(FLength, 4);
end;

function TSCSI_CommandResponse.GetResidualCount: cardinal;
begin
  if IsOverFlow then
    result := BytesProcessed - Original_AllocationLength
  else
    result := Original_AllocationLength - BytesProcessed;
end;


{ TiSCSISession }

function TiSCSI_Session.AddTask(typ: TiSCSI_Task_Class; header: TISCSI_CommonHeader): TiSCSI_Task;
begin
  Lock;
  try
    result := typ.Create;
    result.header := header;
    Ftasks.add(result);


  finally
    Unlock;
  end;
end;

procedure TiSCSI_Session.ClearTask(id: cardinal);
var
  t: Tiscsi_task;
  i: nativeint;
begin
  Lock;
  try
    i := IndexOfTask(id);
{$IFNDEF MCS}
    if i >=0 then begin
      FTasks[i].Free;
      FTasks.Delete(i);
    end;
{$ELSE}
    if i >=0 then begin
      FTasks[i].clear_time := getticker;
    end;
    KillStaleTasks(4000);
{$ENDIF}



  finally
    Unlock;
  end;

end;

constructor TiSCSI_Session.Create;
begin
  inherited;
  FTasks := TBetterList<TiSCSI_Task>.create;
  rsTotal := TRingStats.create;
  rsDisk := TRingStats.Create;
  rsIdle := TRingStats.create;
  rsIOHandlerRead := TRingStats.create;
  rsIOHandlerWrite := TRingStats.Create;
  scsi_session_manager.REgisterSession(self);

end;

destructor TiSCSI_Session.Destroy;
begin
  scsi_session_manager.UnREgisterSession(self);
  FTasks.Free;
  FTasks := nil;
  rsTotal.free;
  rsDisk.Free;
  rsIdle.free;
  rsIOHandlerRead.free;
  rsIOHandlerWrite.free;


  inherited;
end;

function TiSCSI_Session.FindTarget(const sName: string): TVirtualDisk;
var
  t: nativeint;
begin
  vdh.lock;
  try
    for t:= 0 to vdh.vdlist.count-1 do begin
      result := vdh.vdList[t];
      if lowercase(result.Identifier) = lowercase(sName) then begin
        exit;
      end;
    end;

    raise EUserError.create('Target not found:'+sName);
  finally
    vdh.Unlock;
  end;




end;

function TiSCSI_Session.FindTask(id: cardinal): TiSCSI_Task;
var
  t: nativeint;
begin
  Lock;
  try
    result := nil;
    for t:= 0 to FTasks.Count-1 do begin
      if (FTasks[t].header.InitiatorTaskTAg = id) then begin
        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Found task:'+inttohex(id,8));{$ENDIF}
        result := FTasks[t];
        exit;
      end;
    end;
     {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Did not find task:'+inttohex(id,8));{$ENDIF}
  finally
    Unlock;
  end;
end;


function TiSCSI_Session.GetSessionid: string;
begin
  lock;
  try
    result := FSessionid;
  finally
    unlock;
  end;
end;

function TiSCSI_Session.IndexOfTask(id: cardinal): nativeint;
var
  t: nativeint;
begin
  Lock;
  try
    result := -1;
    for t:= 0 to FTasks.Count-1 do begin
      if (FTasks[t].header.InitiatorTaskTAg = id) then begin
        result := t;
        {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Index of task:'+inttostr(id)+' is '+inttostr(result));{$ENDIF}
        exit;
      end;
    end;
     {$IFDEF LOCAL_DEBUG}if iscsidebug then LocalDebug('Did not find index of task:'+inttostr(id));{$ENDIF}
  finally
    Unlock;
  end;
end;


procedure TiSCSI_Session.KillStaleTasks(iTime: ticker);
var
  t: ni;
  tsk: TiSCSI_Task;
begin
  for t:= Ftasks.count-1 downto 0 do begin
    tsk := FTasks[t];
    IF ((TSK.clear_time <> 0) or (iTime = 0)) or (gettimesince(tsk.clear_time) > iTime) then
    begin
      tsk.free;
      FTasks.delete(t);
    end;
  end;

end;

procedure TiSCSI_Session.SetSessionid(const Value: string);
begin
  lock;
  try
    FSessionid := value;
  finally
    unlock;
  end;
end;

{ TiSCSI_CommandLists }

constructor TiSCSI_CommandLists.Create;
begin
  inherited;

end;

destructor TiSCSI_CommandLists.Destroy;
begin

  inherited;
end;

function TiSCSI_CommandLists.GetCommand(CmdSN: cardinal): TiSCSI_Command;
var
  i: nativeint;
  sError: string;
begin
  i := IndexOfCommand(CmdSN);
  if i< 0 then begin
    sError := 'Command not found CmdSn='+inttostr(CmdSN);
    AlertAdmin(sError);
    raise Exception.Create(sError);
  end;

  result := FCommands[i];
end;

function TiSCSI_CommandLists.HasCommand(CmdSN: cardinal): boolean;
begin
  result := IndexOfCommand(CmdSN) >= 0;
end;

function TiSCSI_CommandLists.IndexOfCommand(CmdSN: cardinal): nativeint;
var
  t: nativeint;
begin
  result := -1;

  for t:= 0 to FCommands.Count-1 do begin
    if FCommands[t].CmdSN = CmdSn then begin
      result := t;
      break;
    end;
  end;
end;

procedure oinit;
begin
  scsi_session_manager := TiSCSISessionManager.create;
//  scsi_session := TiSCSI_Session.create;
end;

procedure ofinal;
begin
  scsi_session_manager.free;
  scsi_session_manager := nil;
 // scsi_session.Free;

end;


function iSCSIOpCodeToString(opcode: byte): string;
begin
  case opcode of

  //Initiator Op-Codes
    $00: result := 'NOP-Out';
    $01: result := 'SCSI Command';
    $02: result := 'SCSI Task Management function request';
    $03: result := 'Login Request';
    $04: result := 'Text Request';
    $05: result := 'SCSI Data-Out (for WRITE operations)';
    $06: result := 'Logout Request';
    $10: result := 'SNACK Request';
    $1c: result := '0x1e Vendor specific codes';

  //Target op-Codes
    $20: result := 'NOP-In';
    $21: result := 'SCSI Response';
    $22: result := 'SCSI Task Management function response';
    $23: result := 'Login Response';
    $24: result := 'Text Response';
    $25: result := 'SCSI Data-In - for READ operations.';
    $26: result := 'Logout Response';
    $31: result := 'Ready To Transfer (R2T)';
    $32: result := 'Asynchronous Message';
    $3c: result := '0x3e Vendor specific codes';
    $3f: result := 'Reject';
  else
    result := 'unknown iSCSi Opcode '+inttohex(opcode, 2);
  end;
end;

{ TiSCSIContext }

{$IFNDEF INDY}
constructor TiSCSIContext.create;
begin
  inherited;
  FIO := TFakeIOHandler.create;
end;




destructor TiSCSIContext.destroy;
begin
  FIO.Free;
  FIO := nil;
  inherited;
end;

procedure TiSCSIContext.WriteBufferOpen(iThres: ni);
begin
  //do nothing
end;

{ TFakeIOHandler }

procedure TFakeIOHandler.WriteBufferClose;
begin
  //
end;

procedure TFakeIOHandler.WriteBufferFlush;
begin
  //
end;

procedure TFakeIOHandler.WriteBufferOpen(iThres: ni);
begin
  //do nothing
end;




{$ENDIF}

procedure ctx_guarantee_read(context: TiSCSIContext; const p: pbyte; const iLength: nativeint);
var
  iRead, iToRead: ni;
  iJustRead: ni;
  pp: pbyte;
  iTime: ni;
begin
{$IFDEF USE_FIFO_ON_READ}
    iRead := 0;
    //if there is nothing to do then sleep
    {$IFDEF INDY}
      while ((connection.IOHandler.InputBuffer.Size = 0) and (not fifo_incoming.IsDataAvailable)) do begin
        sleep(1);
        if not connection.Connected then
          raise ENetworkError.create('connection dropped in ioh_guarantee_read');
      end;
    {$ENDIF}

    //loop until all the desired data has been read
    while iRead < iLength do begin
      {$IFDEF INDY}
        if not connection.Connected then
          raise Exception.create('connection dropped in ctx_guarantee_read');
      {$ELSE}
        if not context.Connection.Connected then
          raise Exception.create('connection dropped in ctx_guarantee_read');
      {$ENDIF}


      //amount to read is lesser of available data on indy pipe
      //or amount of data space available in the FIFO
      //figure out how much data we can actually red
      {$IFDEF INDY}
      iToRead := lesserof(lesserof(fifo_incoming.BufferSpaceAvailable,connection.IOHandler.InputBuffer.Size), 16384);
      {$ELSE}
      iToRead := lesserof(context.Data.fifo_incoming.BufferSpaceAvailable,16384);
      {$ENDIF}

      iJustRead := 0;
      //if there's stuff to read
      if (iToRead > 0)
{$IFNDEF INDY}
      //and (context.data.fifo_incoming.availabledatasize < iLength)
{$ENDIF}
      then begin
        {$IFDEF INDY}
        setlength(idb, 0);
        {$ENDIF}
        //read the maximum data we can fit into the fifo
        //connection.IOHandler.ReadBytes(idb, iToRead, false);

        pp := GetMemory(iToRead);
        try
          {$IFDEF INDY}
          IOHandler_GuaranteeRead(connection.IOHandler, pp, iToRead);
          fifo_incoming.BufferData(@pp[0], iToRead);
          {$ELSE}

          if context.Data.fifo_incoming.IsDataAvailable then
            iTime := 0
          else
            iTime := 1000;

          iJustRead := Socket_Read(context.Connection, pp, iToRead, iTime);
          if (iJustRead < 0) and not (context.Data.fifo_incoming.IsDataAvailable) then
            raise ESocketError.create('Socket dropped during expected read.');
          if iJustRead > 0 then
            context.Data.fifo_incoming.BufferData(@pp[0], iJustRead);
          {$ENDIF}

        finally
          FreeMemory(pp);
        end;

        //put this data in the fifo
        //fifo_incoming.BufferData(@idb[0], length(idb));
      end else begin
        if not context.Data.fifo_incoming.IsConnected then
            raise ESocketError.create('Socket dropped during expected read.');

      end;
//{$ENDIF}
      {$IFDEF INDY}
      iJustRead := fifo_incoming.GetAvailableChunk(@p[iRead], iLength-iRead);
      {$ELSE}
      iJustRead := context.data.fifo_incoming.GetAvailableChunk(@p[iRead], iLength-iRead);
      {$ENDIF}
      inc(iREad, iJustRead);

    end;

{$ELSE}
  {$IFDEF INDY}
    contextdata(context).ioh_guarantee_read(p, iLength);
  {$ELSE}
    Socket_GuaranteeRead(context.Connection, p, iLength);
  {$ENDIF}

{$ENDIF}
end;

procedure ctx_guarantee_write(context: TiSCSIContext; const p: pbyte; const iLength: nativeint);{$IFNDEF INDY}inline;{$ENdIF}
var
  dw:dword;
begin
{$IFDEF INDY}
    contextdata(context).ioh_guarantee_write(p, iLength);
{$ELSE}
  Socket_GuaranteeWrite(context.Connection, p, iLength);
  dw := 1;
//  winsock.setsockopt(context.connection.Handle, SOL_SOCKET, TCP_NODELAY, pointer(@dw),sizeof(dw));
{$ENDIF}
end;


{ TiSCSISessionManager }

constructor TiSCSISessionManager.Create;
begin
  inherited;
  FList := TBetterList<TISCSI_Session>.create;
end;

destructor TiSCSISessionManager.Destroy;
begin
  FList.free;
  FList := nil;
  inherited;
end;

function TiSCSISessionManager.GetSession(sid: string): TiSCSI_Session;
var
  i: ni;
begin
  lock;
  try
    i := indexof(sid);
    if i < 0 then begin
      result := TiSCSI_Session.create;
      Debug.Log(self, 'Create iSCSI session '+sid);
    end
    else
      result := FList[i];


  finally
    unlock;
  end;

end;

function TiSCSISessionManager.IndexOf(sid: string): ni;
var
  t: ni;
begin
  result := -1;
  lock;
  try
    for t:= 0 to FList.count-1 do begin
      if Flist[t].sessionid = sid then begin
        result := t;
        break;
      end;
    end;
  finally
    unlock;
  end;


end;

procedure TiSCSISessionManager.REgisterSession(s: TiSCSI_Session);
begin
  lock;
  FList.add(s);
  unlock;
end;

procedure TiSCSISessionManager.UnREgisterSession(s: TiSCSI_Session);
begin
  lock;
  FList.remove(s);
  unlock;
end;

initialization
  init.RegisterProcs('iSCSI', oinit, ofinal);
  longfound := false;


finalization





end.
