unit SCSI;

//implementation priorities
//-- Mode Sense(6)
//--   returns data-in
//-- Read Capacity(10)
//--   returns data-in
//-- Read(10)
//--   returns data-in
//-- Write(10)
//--   returns no data, response in iSCSI response  (command complete)


//-- Test Unity Ready
//--   returns no data.  standard iSCSI response
{x$INLINE AUTO}






interface

uses
  endian, temp, NEtbytes, systemx, sysutils, numbers;

type
  Tscsi_id_codeset = (idcReserved, idcBinary, idcASCII, idcUTF8);
  Tscsi_id_type = (idtVendorSpecific, idtT10, idtEUI64, idtNAA, idtRelativeTargetPortID, idtTargetPortGroup, idtLUNGroup, idtMD5LUN, idtSCSINameString);
  Tscsi_id_association = (idaLun, idaPort, idaContainer);
  TSCSIIdentificationDescriptorHeader = packed record
  private
    flags0: byte;
    flags1: byte;
    reserved: byte;

    function GetAssociation: Tscsi_id_association;
    function GetCodeSet: Tscsi_id_codeset;
    function GetIdentifierType: Tscsi_id_type;
    procedure SetAssociation(const Value: Tscsi_id_association);
    procedure SetCodeset(const Value: Tscsi_id_codeset);
    procedure SetIdentifierType(const Value: Tscsi_id_type);
  public
    identifier_length: byte;
    property IdentifierType: Tscsi_id_type read GetIdentifierType write SetIdentifierType;
    property CodeSet: Tscsi_id_codeset read GetCodeSet write SetCodeset;
    property Association: Tscsi_id_association read GetAssociation write SetAssociation;
    procedure Init;
  end;
  PSCSIIdentificationDescriptorHeader = ^TSCSIIdentificationDescriptorHeader;

  TSCSILeadingBytes = array[0..15] of byte;

  TSCSI_GenericCommand6 = packed record
  strict private
    bytes: array[0..5] of byte;
    function GetA(idx: nativeint): byte;
    procedure SetA(idx: nativeint; const Value: byte);
  public
    property a[idx: nativeint]: byte read GetA write SetA;default;
    function Addr(byte: nativeint): PByte;
    property adr[idx: nativeint]: Pbyte read Addr;
    procedure AddrForRead(opCode_external: TSCSILeadingBytes);
  end;

  TSCSI_GenericCommand10 = packed record
  strict private
    bytes: array[0..9] of byte;
    function GetA(idx: nativeint): byte;
    procedure SetA(idx: nativeint; const Value: byte);
  public
    property a[idx: nativeint]: byte read GetA write SetA;default;
    function Addr(byte: nativeint): PByte;
    property adr[idx: nativeint]: Pbyte read Addr;
    procedure AddrForRead(opCode_external: TSCSILeadingBytes);
  end;

  TSCSI_GenericCommand12 = packed record
  strict private
    bytes: array[0..11] of byte;
    function GetA(idx: nativeint): byte;
    procedure SetA(idx: nativeint; const Value: byte);
  public
    property a[idx: nativeint]: byte read GetA write SetA;default;
    function Addr(byte: nativeint): PByte;
    property adr[idx: nativeint]: Pbyte read Addr;
    procedure AddrForRead(opCode_external: TSCSILeadingBytes);
  end;

  TSCSI_GenericCommand16 = packed record
  strict private
    bytes: array[0..15] of byte;
    function GetA(idx: nativeint): byte;
    procedure SetA(idx: nativeint; const Value: byte);
  public
    property a[idx: nativeint]: byte read GetA write SetA;default;
    function Addr(byte: nativeint): PByte;
    property adr[idx: nativeint]: Pbyte read Addr;
    procedure AddrForRead(opCode_external: TSCSILeadingBytes);
  end;

  TSCSI_GenericCommand32 = packed record
  strict private
    bytes: array[0..31] of byte;
    function GetA(idx: nativeint): byte;
    procedure SetA(idx: nativeint; const Value: byte);
  public
    property a[idx: nativeint]: byte read GetA write SetA;default;
    function Addr(byte: nativeint): PByte;
    property adr[idx: nativeint]: Pbyte read Addr;
    function AddrForRead(opCode_external: TSCSILeadingBytes): PByte;
  end;

  TSCSI_ModeSense6 = packed record
  private
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);
    function GetAllolcationLength: byte;
    function GetControl: byte;
    function GEtDBD: boolean;
    function GEtPageCode: byte;
    function GetPC: byte;
    function GetSubpageCode: byte;
    procedure SetAllocationLength(const Value: byte);
    procedure SetControl(const Value: byte);
    procedure SetDBD(const Value: boolean);
    procedure SEtPageCode(const Value: byte);
    procedure SetPC(const Value: byte);
    procedure SetSubPageCode(const Value: byte);
    function GEtLLBAA: boolean;
    procedure SetLLBAA(const Value: boolean);
  public
    a: TSCSI_GenericCommand6;
    property opcode: byte read GetopCode write SetOpCode;
    property DBD: boolean read GEtDBD write SetDBD;
    property PC: byte read GetPC write SetPC;
    property PageCode: byte read GEtPageCode write SEtPageCode;
    property SubpageCode: byte read GetSubpageCode write SetSubPageCode;
    property AllocationLength: byte read GetAllolcationLength write SetAllocationLength;
    property Control: byte read GetControl write SetControl;
  end;

  TSCSI_ModeSense10 = packed record
  private
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);
    function GetAllolcationLength: byte;
    function GetControl: byte;
    function GEtDBD: boolean;
    function GEtPageCode: byte;
    function GetPC: byte;
    function GetSubpageCode: byte;
    procedure SetAllocationLength(const Value: byte);
    procedure SetControl(const Value: byte);
    procedure SetDBD(const Value: boolean);
    procedure SEtPageCode(const Value: byte);
    procedure SetPC(const Value: byte);
    procedure SetSubPageCode(const Value: byte);
    function GEtLLBAA: boolean;
    procedure SetLLBAA(const Value: boolean);
  public
    a: TSCSI_GenericCommand10;

    property opcode: byte read GetopCode write SetOpCode;
    property LLBAA: boolean read GEtLLBAA write SetLLBAA;
    property DBD: boolean read GEtDBD write SetDBD;
    property PC: byte read GetPC write SetPC;
    property PageCode: byte read GEtPageCode write SEtPageCode;
    property SubpageCode: byte read GetSubpageCode write SetSubPageCode;
    property AllocationLength: byte read GetAllolcationLength write SetAllocationLength;
    property Control: byte read GetControl write SetControl;
    procedure From(rec: TSCSI_ModeSense6);
  end;

  TSCSI_Read6 = packed record
  private

    function GetControl: byte;
    function GetLBA: cardinal;
    function GetLUN: byte;
    function GetTransferLengthInBlocks: nativeint;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: cardinal);
    procedure SetLun(const Value: byte);
    procedure SEtTransferLengthInBlocks(const Value: nativeint);
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);


  public
    a: TSCSI_GenericCommand6;

    property opcode: byte read GetopCode write SetOpCode;
    property LUN: byte read GetLUN write SetLun;
    property LBA: cardinal read GetLBA write SetLBA;
    property TransferLengthInBlocks: nativeint read GetTransferLengthInBlocks write SEtTransferLengthInBlocks;
    property Control: byte read GetControl write SetControl;
  end;


  TSCSI_TestUnitReady = packed record
  public
    a: TSCSI_GenericCommand6;
    property OpCode: byte read a.bytes[0] write a.bytes[0];
    property Control: byte read a.bytes[5] write a.bytes[5];

  end;


  TSCSI_Read10 = packed record
  private
    function GetControl: byte;
    function GetLBA: cardinal;
    function GetLUN: byte;
    function GetTransferLengthInBlocks: nativeint;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: cardinal);
    procedure SetLun(const Value: byte);
    procedure SEtTransferLengthInBlocks(const Value: nativeint);
    function GetDPO: boolean;
    function GetFUA: boolean;
    function GetRelAddr: boolean;
    procedure SetDPO(const Value: boolean);
    procedure SetFUA(const Value: boolean);
    procedure SetRelAddr(const Value: boolean);
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);

  public
     a: TSCSI_GenericCommand10;
    property opcode: byte read GetopCode write SetOpCode;
    property DPO: boolean read GetDPO write SetDPO;
    property FUA: boolean read GetFUA write SetFUA;
    property RelAddr: boolean read GetRelAddr write SetRelAddr;
    property LUN: byte read GetLUN write SetLun;
    property LBA: cardinal read GetLBA write SetLBA;
    property TransferLengthInBlocks: nativeint read GetTransferLengthInBlocks write SEtTransferLengthInBlocks;
    property Control: byte read GetControl write SetControl;
  end;


  TSCSI_Read12 = packed record
  private

    function GetControl: byte;
    function GetLBA: cardinal;
    function GetTransferLengthInBlocks: nativeint;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: cardinal);
    procedure SEtTransferLengthInBlocks(const Value: nativeint);
    function GetDPO: boolean;
    function GetFUA: boolean;
    function GetRelAddr: boolean;
    procedure SetDPO(const Value: boolean);
    procedure SetFUA(const Value: boolean);
    procedure SetRelAddr(const Value: boolean);
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);

  public
    a: TSCSI_GenericCommand12;

    property opcode: byte read GetopCode write SetOpCode;
    property DPO: boolean read GetDPO write SetDPO;
    property FUA: boolean read GetFUA write SetFUA;
    property RelAddr: boolean read GetRelAddr write SetRelAddr;
    property LBA: cardinal read GetLBA write SetLBA;
    property TransferLengthInBlocks: nativeint read GetTransferLengthInBlocks write SEtTransferLengthInBlocks;
    property Control: byte read GetControl write SetControl;
  end;


  TSCSI_Read16 = packed record
  private
    function GetControl: byte;
    function GetLBA: int64;
    function GetTransferLengthInBlocks: nativeint;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: int64);
    procedure SEtTransferLengthInBlocks(const Value: nativeint);
    function GetDPO: boolean;
    function GetFUA: boolean;
    procedure SetDPO(const Value: boolean);
    procedure SetFUA(const Value: boolean);
    function GetFUA_NV: boolean;
    procedure SetFUA_NV(const Value: boolean);
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);
    function GetProtect: byte;
    procedure SetProtect(const Value: byte);

  public
    a: TSCSI_GenericCommand16;
    property opcode: byte read GetopCode write SetOpCode;
    property DPO: boolean read GetDPO write SetDPO;
    property FUA: boolean read GetFUA write SetFUA;
    property FUA_NV: boolean read GetFUA_NV write SetFUA_NV;
    property Protect: byte read GetProtect write SetProtect;
    property LBA: int64 read GetLBA write SetLBA;
    property TransferLengthInBlocks: nativeint read GetTransferLengthInBlocks write SEtTransferLengthInBlocks;
    property Control: byte read GetControl write SetControl;
  end;



  TSCSI_Read32   = packed record
  private

    function GetControl: byte;
    function GetLBA: int64;
    function GetLUN: byte;
    function GetTransferLengthInBlocks: nativeint;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: int64);
    procedure SetLun(const Value: byte);
    procedure SEtTransferLengthInBlocks(const Value: nativeint);
    function GetDPO: boolean;
    function GetFUA: boolean;
    function GetFUA_NV: boolean;
    function GetGroupNumber: nativeint;
    function GetProtect: nativeint;
    function GetServiceAction: nativeint;
    procedure SetDPO(const Value: boolean);
    procedure SetFUA(const Value: boolean);
    procedure SetFUA_NV(const Value: boolean);
    procedure SetGroupNumber(const Value: nativeint);
    procedure SetProtect(const Value: nativeint);
    function GEtELBAT: cardinal;
    function GetILBRT: cardinal;
    function GetLBATM: cardinal;
    procedure SetELBAT(const Value: cardinal);
    procedure SetILBRT(const Value: cardinal);
    procedure SetLBATM(const Value: cardinal);
    function GetRelAddr: boolean;
    procedure SetRelAddr(const Value: boolean);
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);
    procedure SetServiceACtion(const Value: nativeint);
  public
    a: TSCSI_GenericCommand32;
    property opcode: byte read GetopCode write SetOpCode;
    property LUN: byte read GetLUN write SetLun;
    property LBA: int64 read GetLBA write SetLBA;
    property TransferLengthInBlocks: nativeint read GetTransferLengthInBlocks write SEtTransferLengthInBlocks;
    property Control: byte read GetControl write SetControl;
    property GroupNumber: nativeint read GetGroupNumber write SetGroupNumber;
    property ServiceACtion: nativeint read GetServiceAction write SetServiceACtion;
    property Protect: nativeint read GetProtect write SetProtect;
    property DPO: boolean read GetDPO write SetDPO;
    property FUA: boolean read GetFUA write SetFUA;
    property FUA_NV: boolean read GetFUA_NV write SetFUA_NV;
    property RelAddr: boolean read GetRelAddr write SetRelAddr;
    property InitialLogicalBlockReferenceTag: cardinal read GetILBRT write SetILBRT;
    property ExpectedLogicalBlockApplicationTag: cardinal read GEtELBAT write SetELBAT;
    property LogicalBlockApplicationTagMask: cardinal read GetLBATM write SetLBATM;
    procedure FromRead16(var rec: TSCSI_Read16);
    procedure FromRead12(var rec: TSCSI_Read12);
    procedure FromRead10(var rec: TSCSI_Read10);
    procedure FromRead6(var rec: TSCSI_Read6);
    procedure From(var rec: TSCSI_Read6);overload;
    procedure From(var rec: TSCSI_Read10);overload;
    procedure From(var rec: TSCSI_Read12);overload;
    procedure From(var rec: TSCSI_Read16);overload;
  end;


  TSCSI_ReadCapacity10 = packed record
  private
    function GetControl: byte;
    function GetLBA: cardinal;
    function GetLUN: byte;
    function GetOpcode: byte;
    function GetPMI: boolean;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: cardinal);
    procedure SetLUN(const Value: byte);
    procedure Setopcode(const Value: byte);
    procedure SetPMI(const Value: boolean);
    function GetRelAddr: boolean;
    procedure Setreladdr(const Value: boolean);
  public
    a: TSCSI_GenericCommand10;
    property OpCode: byte read GetOpcode write Setopcode;
    property LUN: byte read GetLUN write SetLUN;
    property LBA: cardinal read GetLBA write SetLBA;
    property PMI: boolean read GetPMI write SetPMI;
    property RelAddr: boolean read GetRelAddr write Setreladdr;
    property Control: byte read GetControl write SetControl;
  end;


  TSCSI_ReadCapacity16 = packed record
  private
    function GetControl: byte;
    function GetLBA: cardinal;
    function GetOpcode: byte;
    function GetPMI: boolean;
    function GetRelAddr: boolean;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: cardinal);
    procedure Setopcode(const Value: byte);
    procedure SetPMI(const Value: boolean);
    procedure Setreladdr(const Value: boolean);
    function GetAllocationLength: cardinal;
    procedure SetAllocationLength(const Value: cardinal);
  public
    a: TSCSI_GenericCommand16;
    property OpCode: byte read GetOpcode write Setopcode;
    property LBA: cardinal read GetLBA write SetLBA;
    property AllocationLength: cardinal read GetAllocationLength write SetAllocationLength;
    property PMI: boolean read GetPMI write SetPMI;
    property RelAddr: boolean read GetRelAddr write Setreladdr;
    property Control: byte read GetControl write SetControl;
  end;



  TSCSI_Write6 = packed record
  private
    function GetControl: byte;
    function GetLBA: cardinal;
    function GetLUN: byte;
    function GetTransferLengthInBlocks: nativeint;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: cardinal);
    procedure SetLun(const Value: byte);
    procedure SEtTransferLengthInBlocks(const Value: nativeint);
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);
  public
    a: TSCSI_GenericCommand6;

    property opcode: byte read GetopCode write SetOpCode;
    property LUN: byte read GetLUN write SetLun;
    property LBA: cardinal read GetLBA write SetLBA;
    property TransferLengthInBlocks: nativeint read GetTransferLengthInBlocks
      write SEtTransferLengthInBlocks;
    property Control: byte read GetControl write SetControl;
  end;

  TSCSI_Write10 = packed record
  private
    function GetControl: byte;
    function GetLBA: cardinal;
    function GetLUN: byte;
    function GetTransferLengthInBlocks: nativeint;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: cardinal);
    procedure SetLun(const Value: byte);
    procedure SEtTransferLengthInBlocks(const Value: nativeint);
    function GetDPO: boolean;
    function GetFUA: boolean;
    function GetRelAddr: boolean;
    procedure SetDPO(const Value: boolean);
    procedure SetFUA(const Value: boolean);
    procedure SetRelAddr(const Value: boolean);
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);
    function GetFUA_NV: boolean;
    procedure SetFUA_NV(const Value: boolean);
    function getProtect: byte;
    procedure setProtect(const Value: byte);
    function GetGroupNumber: byte;
    procedure SetGroupNumber(const Value: byte);
  public
    a: TSCSI_GenericCommand10;
    property opcode: byte read GetopCode write SetOpCode;
    property DPO: boolean read GetDPO write SetDPO;
    property FUA: boolean read GetFUA write SetFUA;
    property FUA_NV: boolean read GetFUA_NV write SetFUA_NV;
    property RelAddr: boolean read GetRelAddr write SetRelAddr;
    property LUN: byte read GetLUN write SetLun;
    property LBA: cardinal read GetLBA write SetLBA;
    property Protect: byte read getProtect write setProtect;
    property Groupnumber: byte read GetGroupNumber write SetGroupNumber;
    property TransferLengthInBlocks: nativeint read GetTransferLengthInBlocks
      write SEtTransferLengthInBlocks;
    property Control: byte read GetControl write SetControl;
  end;

  TSCSI_Write12 = packed record
  private
    function GetControl: byte;
    function GetLBA: cardinal;
    function GetTransferLengthInBlocks: nativeint;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: cardinal);
    procedure SEtTransferLengthInBlocks(const Value: nativeint);
    function GetDPO: boolean;
    function GetFUA: boolean;
    function GetRelAddr: boolean;
    procedure SetDPO(const Value: boolean);
    procedure SetFUA(const Value: boolean);
    procedure SetRelAddr(const Value: boolean);
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);
    function GetFUA_NV: boolean;
    function GetGroupNumber: byte;
    procedure SetFUA_NV(const Value: boolean);
    procedure SetGroupNumber(const Value: byte);
    function GetRestrictedForMMC6: boolean;
    procedure SetRestrictedForMMC6(const Value: boolean);
    function getProtect: byte;
    procedure setProtect(const Value: byte);
  public
    a: TSCSI_GenericCommand12;
    property opcode: byte read GetopCode write SetOpCode;
    property DPO: boolean read GetDPO write SetDPO;
    property FUA: boolean read GetFUA write SetFUA;
    property FUA_NV: boolean read GetFUA_NV write SetFUA_NV;
    property GroupNumber: byte read GetGroupNumber write SetGroupNumber;
    property Protect: byte read getProtect write setProtect;
    property RestrictedForMMC6: boolean read GetRestrictedForMMC6 write SetRestrictedForMMC6;
    property RelAddr: boolean read GetRelAddr write SetRelAddr;
    property LBA: cardinal read GetLBA write SetLBA;
    property TransferLengthInBlocks: nativeint read GetTransferLengthInBlocks
      write SEtTransferLengthInBlocks;
    property Control: byte read GetControl write SetControl;
  end;

  TSCSI_Write16 = packed record
  private
    function GetControl: byte;
    function GetLBA: int64;
    function GetTransferLengthInBlocks: nativeint;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: int64);
    procedure SEtTransferLengthInBlocks(const Value: nativeint);
    function GetDPO: boolean;
    function GetFUA: boolean;
    procedure SetDPO(const Value: boolean);
    procedure SetFUA(const Value: boolean);
    function GetFUA_NV: boolean;
    procedure SetFUA_NV(const Value: boolean);
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);
    function GetGroupNumber: byte;
    procedure SetGroupNumber(const Value: byte);
    function GetRestrictedForMMC6: boolean;
    procedure SetRestrictedForMMC6(const Value: boolean);
    function getProtect: byte;
    procedure setProtect(const Value: byte);
  public
    a: TSCSI_GenericCommand16;
    property opcode: byte read GetopCode write SetOpCode;
    property DPO: boolean read GetDPO write SetDPO;
    property FUA: boolean read GetFUA write SetFUA;
    property FUA_NV: boolean read GetFUA_NV write SetFUA_NV;
    property LBA: int64 read GetLBA write SetLBA;
    property RestrictedForMMC6: boolean read GetRestrictedForMMC6 write SetRestrictedForMMC6;
    property TransferLengthInBlocks: nativeint read GetTransferLengthInBlocks
      write SEtTransferLengthInBlocks;
    property Control: byte read GetControl write SetControl;
    property Groupnumber: byte read GetGroupNumber write SetGroupNumber;
    property Protect: byte read GEtProtect write SetProtect;
  end;

  TSCSI_Write32 = packed record
  private
    function GetControl: byte;
    function GetLBA: int64;
    function GetLUN: byte;
    function GetTransferLengthInBlocks: nativeint;
    procedure SetControl(const Value: byte);
    procedure SetLBA(const Value: int64);
    procedure SetLun(const Value: byte);
    procedure SEtTransferLengthInBlocks(const Value: nativeint);
    function GetDPO: boolean;
    function GetFUA: boolean;
    function GetFUA_NV: boolean;
    function GetGroupNumber: nativeint;
    function GetProtect: nativeint;
    function GetServiceAction: nativeint;
    procedure SetDPO(const Value: boolean);
    procedure SetFUA(const Value: boolean);
    procedure SetFUA_NV(const Value: boolean);
    procedure SetGroupNumber(const Value: nativeint);
    procedure SetProtect(const Value: nativeint);
    function GEtELBAT: cardinal;
    function GetILBRT: cardinal;
    function GetLBATM: cardinal;
    procedure SetELBAT(const Value: cardinal);
    procedure SetILBRT(const Value: cardinal);
    procedure SetLBATM(const Value: cardinal);
    function GetRelAddr: boolean;
    procedure SetRelAddr(const Value: boolean);
    function GetopCode: byte;
    procedure SetOpCode(const Value: byte);
    procedure SetServiceACtion(const Value: nativeint);
  public
    a: TSCSI_GenericCommand32;
    FRestrictedForMMC6: boolean;

    property opcode: byte read GetopCode write SetOpCode;
    property LUN: byte read GetLUN write SetLun;
    property LBA: int64 read GetLBA write SetLBA;
    property TransferLengthInBlocks: nativeint read GetTransferLengthInBlocks
      write SEtTransferLengthInBlocks;
    property Control: byte read GetControl write SetControl;
    property GroupNumber: nativeint read GetGroupNumber write SetGroupNumber;
    property ServiceACtion: nativeint read GetServiceAction
      write SetServiceACtion;
    property Protect: nativeint read GetProtect write SetProtect;
    property DPO: boolean read GetDPO write SetDPO;
    property FUA: boolean read GetFUA write SetFUA;
    property FUA_NV: boolean read GetFUA_NV write SetFUA_NV;
    property RestrictedForMMC6: boolean read FRestrictedForMMC6 write FRestrictedForMMC6;
    property RelAddr: boolean read GetRelAddr write SetRelAddr;
    property InitialLogicalBlockReferenceTag: cardinal read GetILBRT
      write SetILBRT;
    property ExpectedLogicalBlockApplicationTag: cardinal read GEtELBAT
      write SetELBAT;
    property LogicalBlockApplicationTagMask: cardinal read GetLBATM
      write SetLBATM;
    procedure FromWrite16(var rec: TSCSI_Write16);
    procedure FromWrite12(var rec: TSCSI_Write12);
    procedure FromWrite10(var rec: TSCSI_Write10);
    procedure FromWrite6(var rec: TSCSI_Write6);
    procedure From(var rec: TSCSI_Write6); overload;
    procedure From(var rec: TSCSI_Write10); overload;
    procedure From(var rec: TSCSI_Write12); overload;
    procedure From(var rec: TSCSI_Write16); overload;
  end;

  TSCSI_PAYLOAD_ReadCapacity10 = packed record
  private
    function GEtBS: cardinal;
    function GEtLBA: cardinal;
    procedure SEtBS(const Value: cardinal);
    procedure SEtLBA(const Value: cardinal);
  public
    a: array[0..7] of byte;
    property LBA: cardinal read GEtLBA write SEtLBA;
    property BlockSizeInBytes: cardinal read GEtBS write SEtBS;

  end;

  TSCSI_PAYLOAD_ReadCapacity16 = packed record
  private
    function GEtBS: cardinal;
    function GEtLBA: int64;
    procedure SEtBS(const Value: cardinal);
    procedure SEtLBA(const Value: int64);
  public
    a: array[0..31] of byte;
    procedure Init;
    property LBA: int64 read GEtLBA write SEtLBA;
    property BlockSizeInBytes: cardinal read GEtBS write SEtBS;

  end;

  TSCSI_ReportLuns = packed record
  private
    function GetAllocationLength: cardinal;
    procedure SetAllocationLength(const Value: cardinal);
  public
    a: TSCSI_GenericCommand12;
    property SelectReport: byte read a.bytes[2] write a.bytes[2];
    property AllocationLength: cardinal read GetAllocationLength write SetAllocationLength;
  end;

  TSCSI_ReportLuns12 = TSCSI_ReportLuns;


  TSCSI_Inquery = packed record
  private
    function GetAllocationLength: cardinal;
    procedure SetAllocationLength(const Value: cardinal);
    function GEtEVPD: boolean;
    procedure SetEVPD(const Value: boolean);
  public
    a: TSCSI_GenericCommand6;
    property PAgeCode: byte read a.bytes[2] write a.bytes[2];
    property AllocationLength: cardinal read GetAllocationLength write SetAllocationLength;
    property EVPD: boolean read GEtEVPD write SetEVPD;
    property Control: byte read a.bytes[5] write a.bytes[5];
  end;

  TSCSI_SynchronizeCache10 = packed record
  private
    function GEtFlagSyncNV: boolean;
    function GEtGroupNumber: byte;
    function GEtImmed: boolean;
    function GEtLBA: word;
    function GEtNumberOfBlocks: word;
    procedure SetFlagSyncNV(const Value: boolean);
    procedure SetGroupNumber(const Value: byte);
    procedure SetImmed(const Value: boolean);
    procedure SetLBA(const Value: word);
    procedure SetNumberOfBlocks(const Value: word);
  public
    a: TSCSI_GenericCommand10;
    property FLag_Immed: boolean read GEtImmed Write SetImmed;
    property FLag_SyncNV: boolean read GEtFlagSyncNV Write SetFlagSyncNV;
    property LBA: word read GEtLBA Write SetLBA;
    property GroupNumber: byte read GEtGroupNumber Write SetGroupNumber;
    property NumberOfBlocks: word read GEtNumberOfBlocks Write SetNumberOfBlocks;
    property Control: byte read a.bytes[9] write a.bytes[9];
  end;

  TSCSI_SynchronizeCache16 = packed record
  private
    function GEtFlagSyncNV: boolean;
    function GEtGroupNumber: word;
    function GEtImmed: boolean;
    function GEtLBA: int64;
    function GEtNumberOfBlocks: cardinal;
    procedure SetFlagSyncNV(const Value: boolean);
    procedure SetGroupNumber(const Value: word);
    procedure SetImmed(const Value: boolean);
    procedure SetLBA(const Value: int64);
    procedure SetNumberOfBlocks(const Value: cardinal);
  public
    a: TSCSI_GenericCommand16;
    property FLag_Immed: boolean read GEtImmed Write SetImmed;
    property FLag_SyncNV: boolean read GEtFlagSyncNV Write SetFlagSyncNV;
    property LBA: int64 read GEtLBA Write SetLBA;
    property GroupNumber: word read GEtGroupNumber Write SetGroupNumber;
    property NumberOfBlocks: cardinal read GEtNumberOfBlocks Write SetNumberOfBlocks;
    property Control: byte read a.bytes[15] write a.bytes[15];
    procedure From(src: TSCSI_SynchronizeCache10);
  end;


function SCSICommandToString(cmd: byte): string;



implementation

{ TSCSI_Read6 }


function TSCSI_Read6.GetControl: byte;
begin
  result := a.Addr(5)^;
end;

function TSCSI_Read6.GetLBA: cardinal;
begin
  result := NetBytes.As3ByteUnsignedInt(a.Addr(1)) and $1FFFFF;
end;

function TSCSI_Read6.GetLUN: byte;
begin
  result := a.Addr(1)^ shr 5;
end;

function TSCSI_Read6.GetopCode: byte;
begin
  result := a[1];
end;

function TSCSI_Read6.GetTransferLengthInBlocks: nativeint;
begin
  result := a[4];
end;

procedure TSCSI_Read6.SetControl(const Value: byte);
begin
  a[5] := value;
end;

procedure TSCSI_Read6.SetLBA(const Value: cardinal);
var
  temp: byte;
begin
  temp := a[1] and $E0;
  From3ByteUnsignedInt(a.adr[1], value);
  a[1] := a[1] AND $1F;
  a[1] := a[1] or temp;


end;

procedure TSCSI_Read6.SetLun(const Value: byte);
begin
  a[1] := a[1] AND $1F;
  a[1] := a[1] or (value shl 5);
end;

procedure TSCSI_Read6.SetOpCode(const Value: byte);
begin
  a[0] := value;
end;

procedure TSCSI_Read6.SEtTransferLengthInBlocks(const Value: nativeint);
begin
  a[4] := value;
end;

{ TSCSI_Read32 }



procedure TSCSI_Read32.From(var rec: TSCSI_Read10);
begin
  self.FromRead10(rec);
end;

procedure TSCSI_Read32.From(var rec: TSCSI_Read6);
begin
  self.FromRead6(rec);
end;

procedure TSCSI_Read32.From(var rec: TSCSI_Read16);
begin
  self.FromRead16(rec);
end;

procedure TSCSI_Read32.From(var rec: TSCSI_Read12);
begin
  self.FromRead12(rec);
end;

procedure TSCSI_Read32.FromRead10(var rec: TSCSI_Read10);
begin
  self.TransferLengthInBlocks := rec.TransferLengthInBlocks;
  self.LogicalBlockApplicationTagMask := 0;
  self.ExpectedLogicalBlockApplicationTag := 0;
  self.InitialLogicalBlockReferenceTag := 0;
  self.LBA := rec.LBA;
  if self.LBA <> rec.LBA then
    raise Exception.create('sanity check failed');
  self.Protect := 0;
  self.dpo := rec.dpo;
  self.FUA := rec.FUA;
  self.FUA_NV := false;
  self.RelAddr := rec.RelAddr;
  self.ServiceACtion := $09;
  self.GroupNumber := 0;
  self.Control := rec.Control;
  self.opcode := rec.opcode;
  self.LUN := rec.LUN;
end;

procedure TSCSI_Read32.FromRead12(var rec: TSCSI_Read12);
begin
  self.TransferLengthInBlocks := rec.TransferLengthInBlocks;
  self.LogicalBlockApplicationTagMask := 0;
  self.ExpectedLogicalBlockApplicationTag := 0;
  self.InitialLogicalBlockReferenceTag := 0;
  self.LBA := rec.LBA;
  self.Protect := 0;
  self.dpo := rec.dpo;
  self.FUA := rec.FUA;
  self.FUA_NV := false;
  self.RelAddr := rec.RelAddr;
  self.ServiceACtion := $09;
  self.GroupNumber := 0;
  self.Control := rec.Control;
  self.opcode := rec.opcode;
  self.LUN := 0;
end;

procedure TSCSI_Read32.FromRead16(var rec: TSCSI_Read16);
begin
  self.TransferLengthInBlocks := rec.TransferLengthInBlocks;
  self.LogicalBlockApplicationTagMask := 0;
  self.ExpectedLogicalBlockApplicationTag := 0;
  self.InitialLogicalBlockReferenceTag := 0;
  self.LBA := rec.LBA;
  self.Protect := 0;
  self.dpo := rec.dpo;
  self.FUA := rec.FUA;
  self.FUA_NV := rec.FUA_NV;
  self.RelAddr := false;
  self.ServiceACtion := $09;
  self.GroupNumber := 0;
  self.Control := rec.Control;
  self.opcode := rec.opcode;
  self.LUN := 0;

end;

procedure TSCSI_Read32.FromRead6(var rec: TSCSI_Read6);
begin
  if rec.TransferLengthInBlocks = 0 then
    self.TransferLengthInBlocks := 256
  else
    self.TransferLengthInBlocks := rec.TransferLengthInBlocks;
  self.LogicalBlockApplicationTagMask := 0;
  self.ExpectedLogicalBlockApplicationTag := 0;
  self.InitialLogicalBlockReferenceTag := 0;
  self.LBA := rec.LBA;
  self.Protect := 0;
  self.dpo := false;
  self.FUA := false;
  self.FUA_NV := false;
  self.RelAddr := false;
  self.ServiceACtion := $09;
  self.GroupNumber := 0;
  self.Control := rec.Control;
  self.opcode := rec.opcode;
  self.LUN := rec.LUN;

end;

function TSCSI_Read32.GetControl: byte;
begin
  result := a[1];
end;

function TSCSI_Read32.GetDPO: boolean;
begin
  result := (a[10] and $10) <> 0;
end;

function TSCSI_Read32.GEtELBAT: cardinal;
begin
  result := AsWord(a.addr(24));
end;

function TSCSI_Read32.GetFUA: boolean;
begin
  result := (a[10] and $08) <> 0;
end;

function TSCSI_Read32.GetFUA_NV: boolean;
begin
  result := (a[10] and $02) <> 0;
end;

function TSCSI_Read32.GetGroupNumber: nativeint;
begin
  result := a[6] and $1f;
end;

function TSCSI_Read32.GetILBRT: cardinal;
begin
  result := AsCardinal(a.addr(20));
end;

function TSCSI_Read32.GetLBA: int64;
begin
  result := As6ByteUnsigned(A.addr(12));
end;

function TSCSI_Read32.GetLBATM: cardinal;
begin
  result := AsWord(A.addr(26));
end;

function TSCSI_Read32.GetLUN: byte;
begin
  result := a[6] shr 5;
end;

function TSCSI_Read32.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_Read32.GetProtect: nativeint;
begin
  result := a[10] shr 5;
end;

function TSCSI_Read32.GetRelAddr: boolean;
begin
  result := bitget(a.Addr(10), 0);

end;

function TSCSI_Read32.GetServiceAction: nativeint;
begin
  result := AsWord(a.Addr(8));
end;

function TSCSI_Read32.GetTransferLengthInBlocks: nativeint;
begin
  result := AsCArdinal(a.addr(28));
end;

procedure TSCSI_Read32.SetControl(const Value: byte);
begin
  a[1] := value;
end;

procedure TSCSI_Read32.SetDPO(const Value: boolean);
begin
  bitset(a.Addr(10), 4, value);
end;

procedure TSCSI_Read32.SetELBAT(const Value: cardinal);
begin
  NetBytes.FromCardinal(a.Addr(20),value);
end;

procedure TSCSI_Read32.SetFUA(const Value: boolean);
begin
  bitset(a.Addr(10), 3, value);
end;

procedure TSCSI_Read32.SetFUA_NV(const Value: boolean);
begin
  bitset(a.Addr(10), 1, value);
end;

procedure TSCSI_Read32.SetGroupNumber(const Value: nativeint);
begin
  a[6] := a[6] and (not $1F);
  a[6] := a[6] or value;
end;

procedure TSCSI_Read32.SetILBRT(const Value: cardinal);
begin
  NetBytes.FromWord(a.Addr(24),value);
end;

procedure TSCSI_Read32.SetLBA(const Value: int64);
begin
  NetBytes.From6ByteUnsigned(a.Addr(12),value);
end;

procedure TSCSI_Read32.SetLBATM(const Value: cardinal);
begin
  NetBytes.FromWord(a.Addr(26),value);
end;

procedure TSCSI_Read32.SetLun(const Value: byte);
begin
  a[1] := a[1] AND $1F;
  a[1] := a[1] or (value shl 5);
end;

procedure TSCSI_Read32.SetOpCode(const Value: byte);
begin
  a[0] := value;
end;

procedure TSCSI_Read32.SetProtect(const Value: nativeint);
begin
  a[10] := a[10] and $1f;
  a[10] := a[10] or (value shl 5);
end;

procedure TSCSI_Read32.SetRelAddr(const Value: boolean);
begin
  bitset(a.Addr(10), 0, value);
end;

procedure TSCSI_Read32.SetServiceACtion(const Value: nativeint);
begin
  FromWord(a.addr(8), value);
end;

procedure TSCSI_Read32.SEtTransferLengthInBlocks(const Value: nativeint);
begin
  FromCardinal(a.Addr(28), value);

end;


{ TSCSI_Read10 }


function TSCSI_Read10.GetControl: byte;
begin
  result := a[9];
end;

function TSCSI_Read10.GetDPO: boolean;
begin
  result := BitGet(a.addr(1), 4);
end;

function TSCSI_Read10.GetFUA: boolean;
begin
  result := BitGet(a.addr(1), 3);
end;

function TSCSI_Read10.GetLBA: cardinal;
begin
  result := AsCArdinal(a.addr(2));
end;

function TSCSI_Read10.GetLUN: byte;
begin
  result := a[1] shr 5;
end;

function TSCSI_Read10.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_Read10.GetRelAddr: boolean;
begin
  result := BitGet(a.adr[1], 0);
end;

function TSCSI_Read10.GetTransferLengthInBlocks: nativeint;
begin
  result := AsWord(a.Addr(7));
end;

procedure TSCSI_Read10.SetControl(const Value: byte);
begin
  a[9] := value;
end;

procedure TSCSI_Read10.SetDPO(const Value: boolean);
begin
  BitSet(a.Addr(1), 4, value);
end;

procedure TSCSI_Read10.SetFUA(const Value: boolean);
begin
  BitSet(a.Addr(1), 3, value);
end;

procedure TSCSI_Read10.SetLBA(const Value: cardinal);
begin
  FromCardinal(a.addr(2), value);
end;

procedure TSCSI_Read10.SetLun(const Value: byte);
begin
  a[1] := a[1] and $1F;
  a[1] := a[1] or (value shl 5);
end;

procedure TSCSI_Read10.SetOpCode(const Value: byte);
begin
  a[0] := value;
end;

procedure TSCSI_Read10.SetRelAddr(const Value: boolean);
begin
  BitSet(a.Addr(1), 0, value);
end;

procedure TSCSI_Read10.SEtTransferLengthInBlocks(const Value: nativeint);
begin
  FromWord(a.addr(7), value);
end;

{ TSCSI_Read12 }



function TSCSI_Read12.GetControl: byte;
begin
  result := a[11];
end;

function TSCSI_Read12.GetDPO: boolean;
begin
  result := BitGet(a.addr(1), 4);
end;

function TSCSI_Read12.GetFUA: boolean;
begin
  result := BitGet(a.addr(1), 3);
end;

function TSCSI_Read12.GetLBA: cardinal;
begin
  result := AsCardinal(a.addr(2));
end;


function TSCSI_Read12.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_Read12.GetRelAddr: boolean;
begin
  result := BitGet(a.addr(1), 0);
end;

function TSCSI_Read12.GetTransferLengthInBlocks: nativeint;
begin
  result := Ascardinal(a.addr(6));
end;

procedure TSCSI_Read12.SetControl(const Value: byte);
begin
  a[11] := value;
end;

procedure TSCSI_Read12.SetDPO(const Value: boolean);
begin
  BitSet(a.addr(1), 4, value);
end;

procedure TSCSI_Read12.SetFUA(const Value: boolean);
begin
  BitSet(a.addr(1), 3, value);
end;

procedure TSCSI_Read12.SetLBA(const Value: cardinal);
begin
  FromCardinal(a.addr(2), value);
end;


procedure TSCSI_Read12.SetOpCode(const Value: byte);
begin
  a[0] := value;
end;

procedure TSCSI_Read12.SetRelAddr(const Value: boolean);
begin
  BitSet(a.addr(1), 0, value);
end;

procedure TSCSI_Read12.SEtTransferLengthInBlocks(const Value: nativeint);
begin
  FromCardinal(a.addr(6), value);
end;

{ TSCSI_Read16 }


function TSCSI_Read16.GetControl: byte;
begin
  result := a[15];
end;

function TSCSI_Read16.GetDPO: boolean;
begin
  result := BitGet(a.addr(1), 4);
end;

function TSCSI_Read16.GetFUA: boolean;
begin
  result := BitGet(a.addr(1), 3);
end;

function TSCSI_Read16.GetFUA_NV: boolean;
begin
  result := BitGet(a.addr(1), 1);
end;

function TSCSI_Read16.GetLBA: int64;
begin
  result := ASInt64(a.addr(2));
end;


function TSCSI_Read16.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_Read16.GetProtect: byte;
begin
  result := a[1] shr 5;
end;

function TSCSI_Read16.GetTransferLengthInBlocks: nativeint;
begin
  result := Ascardinal(a.addr(10));
end;

procedure TSCSI_Read16.SetControl(const Value: byte);
begin
  a[15] := value;
end;

procedure TSCSI_Read16.SetDPO(const Value: boolean);
begin
  BitSet(a.addr(1), 4, value);
end;

procedure TSCSI_Read16.SetFUA(const Value: boolean);
begin
  BitSet(a.addr(1), 3, value);
end;

procedure TSCSI_Read16.SetFUA_NV(const Value: boolean);
begin
  BitSet(a.addr(1), 1, value);
end;

procedure TSCSI_Read16.SetLBA(const Value: int64);
begin
  fromint64(a.addr(2), value);
end;


procedure TSCSI_Read16.SetOpCode(const Value: byte);
begin
  a[0] := value;
end;

procedure TSCSI_Read16.SetProtect(const Value: byte);
begin
  a[1] := a[1] and $3f;
  a[1] := a[1] or (value shl 5);

end;

procedure TSCSI_Read16.SEtTransferLengthInBlocks(const Value: nativeint);
begin
  fromcardinal(a.addr(10), value);
end;

{ TSCSI_ModeSense6 }


function TSCSI_ModeSense6.GetAllolcationLength: byte;
begin
  result := a[4];
end;

function TSCSI_ModeSense6.GetControl: byte;
begin
  result := a[5];
end;

function TSCSI_ModeSense6.GEtDBD: boolean;
begin
  result := bitget(a.Addr(1), 3);
end;

function TSCSI_ModeSense6.GEtLLBAA: boolean;
begin
  result := bitget(a.Addr(1), 4);
end;

function TSCSI_ModeSense6.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_ModeSense6.GEtPageCode: byte;
begin
  result := a[2] and $3f;
end;

function TSCSI_ModeSense6.GetPC: byte;
begin
  result := a[2] shr 6;
end;

function TSCSI_ModeSense6.GetSubpageCode: byte;
begin
  result := a[3];
end;

procedure TSCSI_ModeSense6.SetAllocationLength(const Value: byte);
begin
  a[4] := value;
end;

procedure TSCSI_ModeSense6.SetControl(const Value: byte);
begin
  a[5] := value;
end;

procedure TSCSI_ModeSense6.SetDBD(const Value: boolean);
begin
  BitSet(a.addr(1), 3, value);
end;

procedure TSCSI_ModeSense6.SetLLBAA(const Value: boolean);
begin
  BitSet(a.addr(1), 4, value);
end;

procedure TSCSI_ModeSense6.SetOpCode(const Value: byte);
begin
  a[0] := value;
end;

procedure TSCSI_ModeSense6.SEtPageCode(const Value: byte);
begin
  a[2] := a[2] and $B0;
  a[2] := a[2] or value;
end;

procedure TSCSI_ModeSense6.SetPC(const Value: byte);
begin
  a[2] := a[2] and $3F;
  a[2] := a[2] or (value shl 6);
end;

procedure TSCSI_ModeSense6.SetSubPageCode(const Value: byte);
begin
  a[3] := value;
end;

{ TSCSI_ModeSense10 }


procedure TSCSI_ModeSense10.From(rec: TSCSI_ModeSense6);
begin
  self.opcode := rec.opcode;
  self.LLBAA := false;
  self.DBD := rec.DBD;
  self.PageCode := rec.PageCode;
  self.AllocationLength := rec.AllocationLength;
  self.PC := rec.PC;
end;

function TSCSI_ModeSense10.GetAllolcationLength: byte;
begin
  result := AsWord(a.addr(7));
end;

function TSCSI_ModeSense10.GetControl: byte;
begin
  result := a[9];
end;

function TSCSI_ModeSense10.GEtDBD: boolean;
begin
  result := BitGet(a.addr(1), 3);

end;

function TSCSI_ModeSense10.GEtLLBAA: boolean;
begin
  result :=   BitGet(a.addr(1), 4);
end;

function TSCSI_ModeSense10.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_ModeSense10.GEtPageCode: byte;
begin
  result := a[2] and $3f;
end;

function TSCSI_ModeSense10.GetPC: byte;
begin
  result := a[2] shr 6;
end;

function TSCSI_ModeSense10.GetSubpageCode: byte;
begin
  result := a[3];
end;

procedure TSCSI_ModeSense10.SetAllocationLength(const Value: byte);
begin
  FromWord(a.Addr(7), value);
end;

procedure TSCSI_ModeSense10.SetControl(const Value: byte);
begin
  a[9] := value;
end;

procedure TSCSI_ModeSense10.SetDBD(const Value: boolean);
begin
  BitSet(a.Addr(1), 3, value);
end;

procedure TSCSI_ModeSense10.SetLLBAA(const Value: boolean);
begin
  BitSet(a.Addr(1), 4, value);
end;


procedure TSCSI_ModeSense10.SetOpCode(const Value: byte);
begin
  a[0] := value;
end;

procedure TSCSI_ModeSense10.SEtPageCode(const Value: byte);
begin
  a[2] := a[2] and $B0;
  a[2] := a[2] or value;
end;

procedure TSCSI_ModeSense10.SetPC(const Value: byte);
begin
  a[2] := a[2] and $3f;
  a[2] := a[2] or (value shl 6);
end;

procedure TSCSI_ModeSense10.SetSubPageCode(const Value: byte);
begin
  a[3] := value;
end;

{ TSCSI_GenericCommand6 }

function TSCSI_GenericCommand6.Addr(byte: nativeint): PByte;
begin
  result := @bytes[byte];
end;

procedure TSCSI_GenericCommand6.AddrForRead(opCode_external: TSCSILeadingBytes);
begin
  movemem32(Addr(0), @opcode_external[0], lesserof(sizeof(opcode_external), sizeof(self)));
end;

function TSCSI_GenericCommand6.GetA(idx: nativeint): byte;
begin
  result := bytes[idx];
end;

procedure TSCSI_GenericCommand6.SetA(idx: nativeint; const Value: byte);
begin
  bytes[idx] := value;
end;

{ TSCSI_ReadCapacity10 }

function TSCSI_GenericCommand10.Addr(byte: nativeint): PByte;
begin
  result := @bytes[byte];
end;

procedure TSCSI_GenericCommand10.AddrForRead(opCode_external: TSCSILeadingBytes);
begin
  movemem32(Addr(0), @opcode_external[0], lesserof(sizeof(opcode_external), sizeof(self)));
end;

function TSCSI_GenericCommand10.GetA(idx: nativeint): byte;
begin
  result := bytes[idx];
end;

procedure TSCSI_GenericCommand10.SetA(idx: nativeint; const Value: byte);
begin
  bytes[idx] := value;
end;

{ TSCSI_ReadCapacity12 }

function TSCSI_GenericCommand12.Addr(byte: nativeint): PByte;
begin
  result := @bytes[byte];
end;

procedure TSCSI_GenericCommand12.AddrForRead(opCode_external: TSCSILeadingBytes);
begin
  movemem32(Addr(0), @opcode_external[0], lesserof(sizeof(opcode_external), sizeof(self)));
end;

function TSCSI_GenericCommand12.GetA(idx: nativeint): byte;
begin
  result := bytes[idx];
end;

procedure TSCSI_GenericCommand12.SetA(idx: nativeint; const Value: byte);
begin
  bytes[idx] := value;
end;

{ TSCSI_ReadCapacity16 }

function TSCSI_GenericCommand16.Addr(byte: nativeint): PByte;
begin
  result := @bytes[byte];
end;

procedure TSCSI_GenericCommand16.AddrForRead(opCode_external: TSCSILeadingBytes);
begin
  movemem32(Addr(0), @opcode_external[0], lesserof(sizeof(opcode_external), sizeof(self)));
end;

function TSCSI_GenericCommand16.GetA(idx: nativeint): byte;
begin
  result := bytes[idx];
end;

procedure TSCSI_GenericCommand16.SetA(idx: nativeint; const Value: byte);
begin
  bytes[idx] := value;
end;

{ TSCSI_ReadCapacity32 }

function TSCSI_GenericCommand32.Addr(byte: nativeint): PByte;
begin
  result := @bytes[byte];
end;

function TSCSI_GenericCommand32.AddrForRead(opCode_external: TSCSILeadingBytes): PByte;
begin
  movemem32(Addr(0), @opcode_external[0], lesserof(sizeof(opcode_external), sizeof(self)));
  result := Addr(16);
end;

function TSCSI_GenericCommand32.GetA(idx: nativeint): byte;
begin
  result := bytes[idx];
end;

procedure TSCSI_GenericCommand32.SetA(idx: nativeint; const Value: byte);
begin
  bytes[idx] := value;
end;

{ TSCSI_ReadCapacity16 }

function TSCSI_ReadCapacity16.GetAllocationLength: cardinal;
begin
  result := AsCArdinal(a.Addr(10));
end;

function TSCSI_ReadCapacity16.GetControl: byte;
begin
  result := a.a[15];
end;

function TSCSI_ReadCapacity16.GetLBA: cardinal;
begin
  result := AsCArdinal(a.Addr(2));
end;


function TSCSI_ReadCapacity16.GetOpcode: byte;
begin
  result := a.a[0];
end;

function TSCSI_ReadCapacity16.GetPMI: boolean;
begin
  result := BitGet(a.adr[14], 0);
end;


function TSCSI_ReadCapacity16.GetRelAddr: boolean;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TSCSI_ReadCapacity16.SetAllocationLength(const Value: cardinal);
begin
  FromCardinal(a.adr[10], value);
end;

procedure TSCSI_ReadCapacity16.SetControl(const Value: byte);
begin
  a.a[15] := value;
end;

procedure TSCSI_ReadCapacity16.SetLBA(const Value: cardinal);
begin
  FromCardinal(a.adr[2], value);
end;


procedure TSCSI_ReadCapacity16.Setopcode(const Value: byte);
begin
  a.a[0] := value;
end;

procedure TSCSI_ReadCapacity16.SetPMI(const Value: boolean);
begin
  BitSet(a.adr[14], 0, value);
end;


procedure TSCSI_ReadCapacity16.Setreladdr(const Value: boolean);
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

{ TSCSI_ReadCapacity10 }

function TSCSI_ReadCapacity10.GetControl: byte;
begin
  result := a[sizeof(a)-1];
end;

function TSCSI_ReadCapacity10.GetLBA: cardinal;
begin
  result := AsCardinal(a.Addr(2));
end;

function TSCSI_ReadCapacity10.GetLUN: byte;
begin
  result := a[1] shr 5;
end;

function TSCSI_ReadCapacity10.GetOpcode: byte;
begin
  result := a[0];
end;

function TSCSI_ReadCapacity10.GetPMI: boolean;
begin
  result := BitGet(a.adr[8],0);
end;

function TSCSI_ReadCapacity10.GetRelAddr: boolean;
begin
  result := bitget(a.adr[1],0);
end;

procedure TSCSI_ReadCapacity10.SetControl(const Value: byte);
begin
  a[9] := value;
end;

procedure TSCSI_ReadCapacity10.SetLBA(const Value: cardinal);
begin
  FromCardinal(a.Addr(2), value);
end;

procedure TSCSI_ReadCapacity10.SetLUN(const Value: byte);
begin
  a[1] := a[1] and $3f;
  a[1] := a[1] or (value shl 5);
end;

procedure TSCSI_ReadCapacity10.Setopcode(const Value: byte);
begin
  a[0] := value;
end;

procedure TSCSI_ReadCapacity10.SetPMI(const Value: boolean);
begin
  BitSet(a.adr[8],0, value);
end;

procedure TSCSI_ReadCapacity10.Setreladdr(const Value: boolean);
begin
  bitset(a.adr[1],0,value);
end;

function TSCSI_Write6.GetControl: byte;
begin
  result := a[5];
end;

function TSCSI_Write6.GetLBA: cardinal;
begin
  result := NEtbytes.As3ByteUnsignedInt(a.adr[1]) and $1FFFFF;
end;

function TSCSI_Write6.GetLUN: byte;
begin
  result := a[1] shr 5;
end;

function TSCSI_Write6.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_Write6.GetTransferLengthInBlocks: nativeint;
begin
  result := a[4];
end;

procedure TSCSI_Write6.SetControl(const Value: byte);
begin
  a[5] := Value;
end;

procedure TSCSI_Write6.SetLBA(const Value: cardinal);
var
  temp: byte;
begin
  temp := a[1] and $E0;
  From3ByteUnsignedInt(a.adr[1], Value);
  a[1] := a[1] AND $1F;
  a[1] := a[1] or temp;
end;

procedure TSCSI_Write6.SetLun(const Value: byte);
begin
  a[1] := a[1] AND $1F;
  a[1] := a[1] or (Value shl 5);
end;

procedure TSCSI_Write6.SetOpCode(const Value: byte);
begin
  a[0] := Value;
end;

procedure TSCSI_Write6.SEtTransferLengthInBlocks(const Value: nativeint);
begin
  a[4] := Value;
end;


function TSCSI_Write10.GetControl: byte;
begin
  result := a[9];
end;

function TSCSI_Write10.GetDPO: boolean;
begin
  result := bitget(a.addr(1), 4);
end;

function TSCSI_Write10.GetFUA: boolean;
begin
  result := bitget(a.Addr(1), 3);
end;

function TSCSI_Write10.GetFUA_NV: boolean;
begin
  result := bitget(a.Addr(1), 1);
end;

function TSCSI_Write10.GetGroupNumber: byte;
begin
  result := a[6] and $3f;
end;

function TSCSI_Write10.GetLBA: cardinal;
begin
  result := AsCardinal(a.Addr(2));
end;

function TSCSI_Write10.GetLUN: byte;
begin
  result := a[1] shr 5;
end;

function TSCSI_Write10.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_Write10.GetRelAddr: boolean;
begin
  result := bitget(a.adr[1], 0);
end;

function TSCSI_Write10.GetTransferLengthInBlocks: nativeint;
begin
  result := AsWord(a.Addr(7));
end;

function TSCSI_Write10.getProtect: byte;
begin
  result := a[1] shr 5;
end;

procedure TSCSI_Write10.SetControl(const Value: byte);
begin
  a[9] := Value;
end;

procedure TSCSI_Write10.SetDPO(const Value: boolean);
begin
  bitset(a.Addr(1), 4, Value);
end;

procedure TSCSI_Write10.SetFUA(const Value: boolean);
begin
  bitset(a.Addr(1), 3, Value);
end;

procedure TSCSI_Write10.SetFUA_NV(const Value: boolean);
begin
  bitset(a.Addr(1), 1,value);
end;

procedure TSCSI_Write10.SetGroupNumber(const Value: byte);
begin
  a[6] := a[6] and (not $3f);
  a[6] := a[6] or value;
end;

procedure TSCSI_Write10.SetLBA(const Value: cardinal);
begin
  FromCardinal(a.Addr(2), Value);
end;

procedure TSCSI_Write10.SetLun(const Value: byte);
begin
  a[1] := a[1] and $1F;
  a[1] := a[1] or (Value shl 5);
end;

procedure TSCSI_Write10.SetOpCode(const Value: byte);
begin
  a[0] := Value;
end;

procedure TSCSI_Write10.SetRelAddr(const Value: boolean);
begin
  bitset(a.Addr(1), 0, Value);
end;

procedure TSCSI_Write10.SEtTransferLengthInBlocks(const Value: nativeint);
begin
  FromWord(a.Addr(7), Value);
end;

procedure TSCSI_Write10.setProtect(const Value: byte);
begin
  a[1] := a[1] and $3f;
  a[1] := a[1] or (value shl 5);
end;


function TSCSI_Write12.GetControl: byte;
begin
  result := a[11];
end;

function TSCSI_Write12.GetDPO: boolean;
begin
  result := bitget(a.Addr(1), 4);
end;

function TSCSI_Write12.GetFUA: boolean;
begin
  result := bitget(a.Addr(1), 3);
end;

function TSCSI_Write12.GetFUA_NV: boolean;
begin
  result := bitget(a.adr[1],1);
end;

function TSCSI_Write12.GetGroupNumber: byte;
begin
  result := a[10] and $3f;
end;

function TSCSI_Write12.GetLBA: cardinal;
begin
  result := AsCardinal(a.Addr(2));
end;

function TSCSI_Write12.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_Write12.GetRelAddr: boolean;
begin
  result := bitget(a.Addr(1), 0);
end;

function TSCSI_Write12.GetRestrictedForMMC6: boolean;
begin
  result := BitGet(a.adr[10], 7);
end;

function TSCSI_Write12.GetTransferLengthInBlocks: nativeint;
begin
  result := AsCardinal(a.Addr(6));
end;

function TSCSI_Write12.getProtect: byte;
begin
  result := a[1] shr 5;
end;

procedure TSCSI_Write12.SetControl(const Value: byte);
begin
  a[11] := Value;
end;

procedure TSCSI_Write12.SetDPO(const Value: boolean);
begin
  bitset(a.Addr(1), 4, Value);
end;

procedure TSCSI_Write12.SetFUA(const Value: boolean);
begin
  bitset(a.Addr(1), 3, Value);
end;

procedure TSCSI_Write12.SetFUA_NV(const Value: boolean);
begin
  bitset(a.adr[1], 1, value);
end;

procedure TSCSI_Write12.SetGroupNumber(const Value: byte);
begin
  a[10] := a[10] and (not $3f);
  a[10] := a[10] or value;
end;

procedure TSCSI_Write12.SetLBA(const Value: cardinal);
begin
  FromCardinal(a.Addr(2), Value);
end;

procedure TSCSI_Write12.SetOpCode(const Value: byte);
begin
  a[0] := Value;
end;

procedure TSCSI_Write12.SetRelAddr(const Value: boolean);
begin
  bitset(a.Addr(1), 0, Value);
end;

procedure TSCSI_Write12.SetRestrictedForMMC6(const Value: boolean);
begin
  BitSet(a.adr[10], 7, value);
end;

procedure TSCSI_Write12.SEtTransferLengthInBlocks(const Value: nativeint);
begin
  FromCardinal(a.Addr(6), Value);
end;

procedure TSCSI_Write12.setProtect(const Value: byte);
begin
  a[1] := a[1] and $3f;
  a[1] := a[1] or (value shl 5);
end;


function TSCSI_Write16.GetControl: byte;
begin
  result := a[15];
end;

function TSCSI_Write16.GetDPO: boolean;
begin
  result := bitget(a.Addr(1), 4);
end;

function TSCSI_Write16.GetFUA: boolean;
begin
  result := bitget(a.Addr(1), 3);
end;

function TSCSI_Write16.GetFUA_NV: boolean;
begin
  result := bitget(a.Addr(1), 1);
end;

function TSCSI_Write16.GetGroupNumber: byte;
begin
  result := a[14] and $3f;

end;

function TSCSI_Write16.GetLBA: int64;
begin
  result := ASInt64(a.Addr(2));
end;

function TSCSI_Write16.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_Write16.GetRestrictedForMMC6: boolean;
begin
  result := BitGet(a.adr[14], 7);
end;

function TSCSI_Write16.GetTransferLengthInBlocks: nativeint;
begin
  result := AsCardinal(a.Addr(10));
end;

function TSCSI_Write16.getProtect: byte;
begin
  result := a[1] shr 5;
end;

procedure TSCSI_Write16.SetControl(const Value: byte);
begin
  a[15] := Value;
end;

procedure TSCSI_Write16.SetDPO(const Value: boolean);
begin
  bitset(a.Addr(1), 4, Value);
end;

procedure TSCSI_Write16.SetFUA(const Value: boolean);
begin
  bitset(a.Addr(1), 3, Value);
end;

procedure TSCSI_Write16.SetFUA_NV(const Value: boolean);
begin
  bitset(a.Addr(1), 1, Value);
end;

procedure TSCSI_Write16.SetGroupNumber(const Value: byte);
begin
  a[6] := a[14] and (not $3f);
  a[6] := a[14] or value;

end;

procedure TSCSI_Write16.SetLBA(const Value: int64);
begin
  FromInt64(a.Addr(2), Value);
end;

procedure TSCSI_Write16.SetOpCode(const Value: byte);
begin
  a[0] := Value;
end;

procedure TSCSI_Write16.SetRestrictedForMMC6(const Value: boolean);
begin
  BitSEt(a.adr[14], 7,value);
end;

procedure TSCSI_Write16.SEtTransferLengthInBlocks(const Value: nativeint);
begin
  FromCardinal(a.Addr(10), Value);
end;

procedure TSCSI_Write16.setProtect(const Value: byte);
begin
  a[1] := a[1] and $3f;
  a[1] := a[1] or (value shl 5);
end;


procedure TSCSI_Write32.From(var rec: TSCSI_Write10);
begin
  self.FromWrite10(rec);
end;

procedure TSCSI_Write32.From(var rec: TSCSI_Write6);
begin
  self.FromWrite6(rec);
end;

procedure TSCSI_Write32.From(var rec: TSCSI_Write16);
begin
  self.FromWrite16(rec);
end;

procedure TSCSI_Write32.From(var rec: TSCSI_Write12);
begin
  self.FromWrite12(rec);
end;

procedure TSCSI_Write32.FromWrite10(var rec: TSCSI_Write10);
begin
  self.TransferLengthInBlocks := rec.TransferLengthInBlocks;
  self.LogicalBlockApplicationTagMask := 0;
  self.ExpectedLogicalBlockApplicationTag := 0;
  self.InitialLogicalBlockReferenceTag := 0;
  self.LBA := rec.LBA;
  self.Protect := rec.Protect;
  self.DPO := rec.DPO;
  self.FUA := rec.FUA;
  self.FUA_NV := rec.FUA_NV;
  self.RelAddr := rec.RelAddr;
  self.ServiceACtion := $0B;
  self.GroupNumber := rec.Groupnumber;
  self.Control := rec.Control;
  self.opcode := rec.opcode;
  self.LUN := rec.LUN;
end;

procedure TSCSI_Write32.FromWrite12(var rec: TSCSI_Write12);
begin
  self.TransferLengthInBlocks := rec.TransferLengthInBlocks;
  self.LogicalBlockApplicationTagMask := 0;
  self.ExpectedLogicalBlockApplicationTag := 0;
  self.InitialLogicalBlockReferenceTag := 0;
  self.LBA := rec.LBA;
  self.Protect := 0;
  self.DPO := rec.DPO;
  self.FUA := rec.FUA;
  self.FUA_NV := rec.FUA_NV;
  self.RestrictedForMMC6 := rec.RestrictedForMMC6;
  self.RelAddr := rec.RelAddr;
  self.ServiceACtion := $0B;
  self.GroupNumber := rec.GroupNumber;
  self.Control := rec.Control;
  self.opcode := rec.opcode;
  self.LUN := 0;
end;

procedure TSCSI_Write32.FromWrite16(var rec: TSCSI_Write16);
begin
  self.TransferLengthInBlocks := rec.TransferLengthInBlocks;
  self.LogicalBlockApplicationTagMask := 0;
  self.ExpectedLogicalBlockApplicationTag := 0;
  self.InitialLogicalBlockReferenceTag := 0;
  self.LBA := rec.LBA;
  self.Protect := rec.protect;
  self.DPO := rec.DPO;
  self.FUA := rec.FUA;
  self.FUA_NV := rec.FUA_NV;
  self.RelAddr := false;
  self.ServiceACtion := $0B;
  self.RestrictedForMMC6 := rec.RestrictedForMMC6;
  self.GroupNumber := 0;
  self.Control := rec.Control;
  self.opcode := rec.opcode;
  self.LUN := 0;
end;

procedure TSCSI_Write32.FromWrite6(var rec: TSCSI_Write6);
begin
  if rec.TransferLengthInBlocks = 0 then
    self.TransferLengthInBlocks := 256
  else
    self.TransferLengthInBlocks := rec.TransferLengthInBlocks;
  self.LogicalBlockApplicationTagMask := 0;
  self.ExpectedLogicalBlockApplicationTag := 0;
  self.InitialLogicalBlockReferenceTag := 0;
  self.LBA := rec.LBA;
  self.Protect := 0;
  self.DPO := false;
  self.FUA := false;
  self.FUA_NV := false;
  self.RelAddr := false;
  self.ServiceACtion := $09;
  self.GroupNumber := 0;
  self.Control := rec.Control;
  self.opcode := rec.opcode;
  self.LUN := rec.LUN;
end;

function TSCSI_Write32.GetControl: byte;
begin
  result := a[1];
end;

function TSCSI_Write32.GetDPO: boolean;
begin
  result := (a[10] and $10) <> 0;
end;

function TSCSI_Write32.GEtELBAT: cardinal;
begin
  result := AsWord(a.Addr(24));
end;

function TSCSI_Write32.GetFUA: boolean;
begin
  result := (a[10] and $08) <> 0;
end;

function TSCSI_Write32.GetFUA_NV: boolean;
begin
  result := (a[10] and $02) <> 0;
end;

function TSCSI_Write32.GetGroupNumber: nativeint;
begin
  result := a[6] and $1F;
end;

function TSCSI_Write32.GetILBRT: cardinal;
begin
  result := AsCardinal(a.Addr(20));
end;

function TSCSI_Write32.GetLBA: int64;
begin
  result := As6ByteUnsigned(a.Addr(12));

end;

function TSCSI_Write32.GetLBATM: cardinal;
begin
  result := AsWord(a.Addr(26));
end;

function TSCSI_Write32.GetLUN: byte;
begin
  result := a[6] shr 5;
end;

function TSCSI_Write32.GetopCode: byte;
begin
  result := a[0];
end;

function TSCSI_Write32.GetProtect: nativeint;
begin
  result := a[10] shr 5;
end;

function TSCSI_Write32.GetRelAddr: boolean;
begin
  result := bitget(a.Addr(10), 0);
end;

function TSCSI_Write32.GetServiceAction: nativeint;
begin
  result := AsWord(a.Addr(8));
end;

function TSCSI_Write32.GetTransferLengthInBlocks: nativeint;
begin
  result := AsCardinal(a.Addr(28));
end;

procedure TSCSI_Write32.SetControl(const Value: byte);
begin
  a[1] := Value;
end;

procedure TSCSI_Write32.SetDPO(const Value: boolean);
begin
  bitset(a.Addr(10), 4, Value);
end;

procedure TSCSI_Write32.SetELBAT(const Value: cardinal);
begin
  NEtbytes.FromCardinal(a.Addr(20), Value);
end;

procedure TSCSI_Write32.SetFUA(const Value: boolean);
begin
  bitset(a.Addr(10), 3, Value);
end;

procedure TSCSI_Write32.SetFUA_NV(const Value: boolean);
begin
  bitset(a.Addr(10), 1, Value);
end;

procedure TSCSI_Write32.SetGroupNumber(const Value: nativeint);
begin
  a[6] := a[6] and (not $1F);
  a[6] := a[6] or Value;
end;

procedure TSCSI_Write32.SetILBRT(const Value: cardinal);
begin
  NEtbytes.FromWord(a.Addr(24), Value);
end;

procedure TSCSI_Write32.SetLBA(const Value: int64);
begin
  From6ByteUnsigned(a.Addr(12), value);
end;

procedure TSCSI_Write32.SetLBATM(const Value: cardinal);
begin
  NEtbytes.FromWord(a.Addr(26), Value);
end;

procedure TSCSI_Write32.SetLun(const Value: byte);
begin
  a[1] := a[1] AND $1F;
  a[1] := a[1] or (Value shl 5);
end;

procedure TSCSI_Write32.SetOpCode(const Value: byte);
begin
  a[0] := Value;
end;

procedure TSCSI_Write32.SetProtect(const Value: nativeint);
begin
  a[10] := a[10] and $1F;
  a[10] := a[10] or (Value shl 5);
end;

procedure TSCSI_Write32.SetRelAddr(const Value: boolean);
begin
  bitset(a.Addr(10), 0, Value);
end;

procedure TSCSI_Write32.SetServiceACtion(const Value: nativeint);
begin
  FromWord(a.Addr(8), Value);
end;

procedure TSCSI_Write32.SEtTransferLengthInBlocks(const Value: nativeint);
begin
  FromCardinal(a.Addr(28), Value);
end;

{ TSCSI_PAYLOAD_ReadCapacity10 }

function TSCSI_PAYLOAD_ReadCapacity10.GEtBS: cardinal;
begin
  result := AsCardinal(@a[4]);
end;

function TSCSI_PAYLOAD_ReadCapacity10.GEtLBA: cardinal;
begin
  result := AsCardinal(@a[0]);
end;

procedure TSCSI_PAYLOAD_ReadCapacity10.SEtBS(const Value: cardinal);
begin
  FRomCardinal(@a[4], value);
end;

procedure TSCSI_PAYLOAD_ReadCapacity10.SEtLBA(const Value: cardinal);
begin
  FRomCardinal(@a[0], value);
end;

{ TSCSI_ReportLuns }

function TSCSI_ReportLuns.GetAllocationLength: cardinal;
begin
  result := AsCardinal(a.Addr(6));
end;

procedure TSCSI_ReportLuns.SetAllocationLength(const Value: cardinal);
begin
  FromCArdinal(a.Addr(6), value);
end;

{ TSCSI_Inquery }

function TSCSI_Inquery.GetAllocationLength: cardinal;
begin
  result := Asword(a.Addr(3));
end;

function TSCSI_Inquery.GEtEVPD: boolean;
begin
  result := BitGet(a.addr(1), 0);
end;

procedure TSCSI_Inquery.SetAllocationLength(const Value: cardinal);
begin
  FromWord(a.Addr(3), value);
end;

procedure TSCSI_Inquery.SetEVPD(const Value: boolean);
begin
  bitset(a.Addr(1), 0, value);
end;


function SCSICommandToString(cmd: byte): string;
begin
  case(cmd) of
    $08: result := 'SCSI_Read6';
    $28: result := 'SCSI_Read10';
    $a8: result := 'SCSI_Read12';
    $88: result := 'SCSI_Read16';
    //todo 3: fix variable cdb length dispatching
    //$7f: res := Dispatch_SCSI_Read32(b, context, common);
    //--------------------------------------------------
    $1a: result := 'SCSI_ModeSense6';
    $5a: result := 'SCSI_ModeSense10';
    //--------------------------------------------------
    $0a: result := 'SCSI_Write6';
    $2a: result := 'SCSI_Write10';
    $aa: result := 'SCSI_Write12';
    $8a: result := 'SCSI_Write16';
    //todo 3: fix variable cdb length dispatching
    //$7f: res := Dispatch_SCSI_Write32(b, context, common);
    //--------------------------------------------------
    $a0: result := 'SCSI_ReportLuns12';
    //--------------------------------------------------
    $12: result := 'SCSI_Inquery';
    $25: result := 'SCSI_ReadCapacity10';
    $10: result := 'SCSI_ReadCapacity16';
    //--------------------------------------------------
    $00: result := 'SCSI_TestUnitReady';
    //--------------------------------------------------
    $35: result := 'Dispatch_SCSI_SynchronizeCache10';
    $91: result := 'Dispatch_SCSI_SynchronizeCache16';
    //--------------------------------------------------
    $2f: result := 'Dispatch_SCSI_GenericStub10';
    $af: result := 'Dispatch_SCSI_GenericStub12';
    $8f: result := 'Dispatch_SCSI_GenericStub16';
    $7f: result := 'Dispatch_SCSI_GenericStub32';
  else
    result := 'undefinied SCSI command '+inttohex(cmd, 2)+' has no string conversion';
  end;
end;

{ TSCSI_SynchronizeCache10 }

function TSCSI_SynchronizeCache10.GEtFlagSyncNV: boolean;
begin
  result := (a.a[1] and (1 shl 2)) > 0;
end;

function TSCSI_SynchronizeCache10.GEtGroupNumber: byte;
begin
  result := a.a[6];
end;

function TSCSI_SynchronizeCache10.GEtImmed: boolean;
begin
  result := (a.a[1] and (1 shl 1)) > 0;
end;

function TSCSI_SynchronizeCache10.GEtLBA: word;
begin
  result := NEtBytes.AsWord(a.Addr(2));
end;

function TSCSI_SynchronizeCache10.GEtNumberOfBlocks: word;
begin
  result := NEtBytes.AsWord(a.Addr(7));

end;

procedure TSCSI_SynchronizeCache10.SetFlagSyncNV(const Value: boolean);
begin
  Bitset(a.Addr(1), 2, value);
end;

procedure TSCSI_SynchronizeCache10.SetGroupNumber(const Value: byte);
begin
  a.a[6] := value;
end;

procedure TSCSI_SynchronizeCache10.SetImmed(const Value: boolean);
begin
  Bitset(a.Addr(1), 1, value);
end;

procedure TSCSI_SynchronizeCache10.SetLBA(const Value: word);
begin
  NetBytes.FromWord(a.Addr(2), value);
end;

procedure TSCSI_SynchronizeCache10.SetNumberOfBlocks(const Value: word);
begin
  NetBytes.FromWord(a.Addr(7), value);
end;

{ TSCSI_SynchronizeCache16 }

procedure TSCSI_SynchronizeCache16.From(src: TSCSI_SynchronizeCache10);
begin
  a.a[1] := src.a.a[1];
  self.NumberOfBlocks := src.NumberOfBlocks;
  self.Control := src.Control;
  self.GroupNumber := src.GroupNumber;
  self.LBA := src.LBA;


end;

function TSCSI_SynchronizeCache16.GEtFlagSyncNV: boolean;
begin
  result :=  (a.a[1] and (1 shl 2)) > 0;
end;

function TSCSI_SynchronizeCache16.GEtGroupNumber: word;
begin
  result := a.a[14] and 31;
end;

function TSCSI_SynchronizeCache16.GEtImmed: boolean;
begin
  result :=  (a.a[1] and (1 shl 1)) > 0;
end;

function TSCSI_SynchronizeCache16.GEtLBA: int64;
begin
  result := As6ByteUnsigned(a.Addr(10));
end;

function TSCSI_SynchronizeCache16.GEtNumberOfBlocks: cardinal;
begin
  result := AsWord(a.Addr(10));
end;

procedure TSCSI_SynchronizeCache16.SetFlagSyncNV(const Value: boolean);
begin
  BitSet(a.Addr(1), 2, value);
end;

procedure TSCSI_SynchronizeCache16.SetGroupNumber(const Value: word);
begin
  a.a[14] := value;
end;

procedure TSCSI_SynchronizeCache16.SetImmed(const Value: boolean);
begin
  BitSet(a.Addr(1), 1, value);
end;

procedure TSCSI_SynchronizeCache16.SetLBA(const Value: int64);
begin
  From6ByteUnsigned(a.Addr(2), value);
end;

procedure TSCSI_SynchronizeCache16.SetNumberOfBlocks(const Value: cardinal);
begin
  FromCardinal(a.Addr(10), value);
end;

{ TSCSI_PAYLOAD_ReadCapacity16 }

function TSCSI_PAYLOAD_ReadCapacity16.GEtBS: cardinal;
begin
  result := AsCardinal(@a[8]);
end;

function TSCSI_PAYLOAD_ReadCapacity16.GEtLBA: int64;
begin
  result := AsInt64(@self.a[0]);
end;

procedure TSCSI_PAYLOAD_ReadCapacity16.Init;
begin
  fillmem(@a[0], sizeof(a), 0);
end;

procedure TSCSI_PAYLOAD_ReadCapacity16.SEtBS(const Value: cardinal);
begin
  FRomCArdinal(@a[8], value);
end;

procedure TSCSI_PAYLOAD_ReadCapacity16.SEtLBA(const Value: int64);
begin
  FromInt64(@a[0], value);

end;

{ TSCSIIdentificationDescriptorHeader }

function TSCSIIdentificationDescriptorHeader.GetAssociation: Tscsi_id_association;
begin
  result := Tscsi_id_association(MaskGet(@flags1, 4, 2));
end;

function TSCSIIdentificationDescriptorHeader.GetCodeSet: Tscsi_id_codeset;
begin
  result := Tscsi_id_codeset(MaskGet(@flags0, 0, 4));
end;

function TSCSIIdentificationDescriptorHeader.GetIdentifierType: Tscsi_id_type;
begin
  result := Tscsi_id_type(MaskGet(@flags1, 0, 4));
end;

procedure TSCSIIdentificationDescriptorHeader.Init;
begin
  CodeSet := idcUTF8;
  Association := idaLun;
  IdentifierType := idtVendorSpecific;
end;

procedure TSCSIIdentificationDescriptorHeader.SetAssociation(
  const Value: Tscsi_id_association);
begin
  MaskSet(@flags1, 4, 2, ord(value));
end;

procedure TSCSIIdentificationDescriptorHeader.SetCodeset(
  const Value: Tscsi_id_codeset);
begin
  MaskSet(@flags0, 0, 4, ord(value));
end;

procedure TSCSIIdentificationDescriptorHeader.SetIdentifierType(
  const Value: Tscsi_id_type);
begin
  MaskSet(@flags1, 0, 4, ord(value));

end;

end.
