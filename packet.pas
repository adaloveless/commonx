unit Packet;
//This unit defines
// TRDTPPacket -- which builds packet for network communication
//

//JASON! if you change the format of the packets... there's a ZILLION places
//to mess up.
//1. GetDataCount
//2. GetInsertionPOint <--- you forgot about this oen last time
//3. The "Add" functions <--- you forgot about these ones last time
//4. The NetworkBuffer routines (should probably be rewritten to be intel optimized
//5. The GetData property getter for the Data[] array


{x$INLINE AUTO}
{$DEFINE VERBOSE_PACKET_LOGGING}
interface
uses SysUtils, betterobject, dtnetconst, NetworkBuffer, typex, Classes, stringx, variants, debug, ExceptionsX, systemx, numbers, zip;

const
  CK : array of byte =
    [55,33,77,88,22,56,34,24,77,134,245,234,123,234,255,245,67,45,34,23,12,
     34,56,45,34,23,12,123,45,234,243,132,98,76,43,65,32,21,76,65,54,43,87,76,32];


//  PACKET_MARKER = $119B92A8;
//  PACKET_ADDRESS_MARKER = 0;
//  PACKET_ADDRESS_LENGTH = 4;
//  PACKET_ADDRESS_TYPE = 8;
//  PACKET_ADDRESS_CRC = 12;
//  PACKET_ADDRESS_ENCRYPTION = 16;
//  PACKET_ADDRESS_USERDATA = 18;
  PACKET_INDEX_RESPONSE_TYPE = 0;
  PACKET_INDEX_SESSION = 1;
  PACKET_INDEX_RESULT = 2;
  PACKET_INDEX_ERROR_CODE = 3;
  PACKET_INDEX_MESSAGE = 4;
  PACKET_INDEX_RESULT_DETAILS = 5;

  PACKET_PLATFORM_OPTION_SUPPORTS_COMPRESSION = 1;
  ENCRYPT_VERBATIM = 0;//PACKET_PLATFORM_OPTION_SUPPORTS_COMPRESSION xor 1;
  ENCRYPT_RLE = 1;//PACKET_PLATFORM_OPTION_SUPPORTS_COMPRESSION;

type
  TRDTPPacket = class;//forward
  TRDTPCHar = char;
  EPacketError = class(EClassException);

//  PACKET_ADDRESS_MARKER = 0;
//  PACKET_ADDRESS_PACKED_LENGTH = 4;
//  PACKET_ADDRESS_UNPACKED_LENGTH = 8;
//  PACKET_ADDRESS_TYPE = 13;
//  PACKET_ADDRESS_CRC = 14;
//  PACKET_ADDRESS_ENCRYPTION = 12;
//  PACKET_ADDRESS_USERDATA = 18;


  TRDTPHeader = packed record
    marker: integer;//0-3
    packedlength: integer;//4-7
    unpackedlength: integer;//8-11
    emcryption: byte;//12
    packettype: byte;//13
    crc: integer;//14-17
  end;




  TRDTPDataType = byte;
  TPacketOrigin = (poClient, poServer);
//----------------------------------------------------------------------------
  TRDTPPacket = class(TBetterObject)
  private
    FOrigin: TPacketOrigin;
    FPlatformOptions: cardinal;
    function GetBufferLength: integer;
  protected
    iSeqReadPos: integer;
    FBuffer: TNetworkBuffer;
    FCRCUpdated: boolean;
    FEncrypted: boolean;
    iLastSearchIndex: integer;
    iLastSearchPointer: integer;
    bDataCountOutOfDate: boolean;
    FDataCountCache: integer;
    fPreviousGetDataType: integer;
    FPreviousGetDataCountType: integer;
    FPreviousGetinsertionpoinType: integer;
    iInsertionPoint: integer; //keeps dibs on where the next UserData value is inserted
    //Property-related functions
    function GetMarker: integer;inline;
    procedure SetMarker(l: integer);inline;
    function GetPackedLength: integer;inline;
    function GetUnPackedLength: integer;inline;
    function GetPacketType: byte;inline;
    procedure SetPacketType(l: byte);inline;
    function GetEncryption: byte;inline;
    procedure SetEncryption(i:byte);inline;
    function GetDataType(idx: integer): TRDTPDataType;inline;
    function GetNextDataType: TRDTPDataType;inline;
    function GetSessionID: integer;inline;
    function GetCRC: cardinal;inline;
    function GetData(idx: integer): variant;inline;
    function GetDataCount: integer;inline;
    function GetResponseType: smallint;inline;
    function GetIsResponse: boolean;inline;
    function GetResult: boolean;inline;
    function GetErrorCode: smallint;inline;
    function GetMessage: string;inline;
    function GetEncryptedBuffer: TNetworkBuffer;inline;
    function GetDecryptedBuffer: TNetworkBuffer;inline;
    function GetDebugMessage2: string;
   //These calls must be used to mark the beginnings and ends of changes and
    //reads, to make sure the packet is in the correct state for reading/writing/etc.

    function BeginPacketRead: boolean;inline;
    function BeginPacketChange: boolean;inline;
    function EndPacketChange: boolean;inline;

    //Internal house-keeping functions

    procedure CalcCRC;inline;
    procedure RecalcLength;inline;

    //Occasionally useful Funcitons
    function GetPointerToData(idx: integer): integer;inline;
    function GetLengthFromDataPointer(ptr: integer): integer;inline;
    function ComputeCRC: cardinal;inline;

  public
    constructor Create;override;
    destructor Destroy; override;
    procedure Initialize;
    procedure Clear;

    //Debugging functions
    procedure ShowDebugMessage;
    function GetDebugMessage: string;

    //Packet definition functions
    property DecryptedBuffer: TNetworkBuffer read GetDecryptedBuffer;
    property EncryptedBuffer: TNetworkBuffer read GetEncryptedBuffer;
      //This is the buffer in which the raw packet data is stored;
    property Marker: integer read GetMarker write SetMarker;
      //The marker should always be the same
    property UnPackedLength: integer read GetUnPackedLength;
    property PackedLength: integer read GetPackedLength;
      //The length of the packet... as reported by the packet header.
      //When building packets, the packet header is automatically updated.
    property PacketType: byte read GetPacketType write SetPacketType;
      //Packet type... almost always 1... will default to 1
    property CRC: cardinal read GetCRC;
      //The CRC Reported by packet header
    property CRCUpdated: boolean read FCRCUpdated;
    property Encryption: byte read GetEncryption write SetEncryption;
      //The encryption method as reported by packet header
    property Encrypted: boolean read FEncrypted;
      //Tells whether or not the Packet data is encrypted currently
    property DataCount: integer read GetDataCount;
      //The number of items in the UserData porton of the packet.
    property Data[idx: integer]: variant read GetData;  default;
      //An index of shorts, strings, longs, and byte types included in the
      //UserData portion of the packet.
    property DataType[idx: integer]: TRDTPDataType read GetDataType;

    //Extended data reading
    procedure DecodeObjectHeader(index: integer; OUT iObjectType, iKeys,
        iFields, iAssociates, iObjects: cardinal);

    //Packet Buidling Rountines
    procedure Addlong(l: integer);
    procedure AddShort(i: smallint);
    procedure AddDouble(d: double);
    procedure AddNull;
    procedure AddBoolean(b: boolean);
    procedure AddString(s: string);
    procedure AddDateTime(d: TDateTime);

    ///	<summary>
    ///	  Adds bytes to the packet..
    ///	</summary>
    ///	<param name="pc">
    ///	  Function does NOT take ownership of memory.
    ///	</param>
    procedure AddBytes(pc: PByte; iLength: int64);
    procedure AddObject(iObjectType, iKeys, iFields, iAssociates, iObjects: integer);
    procedure AddShortObject(iObjectType, iKeys, iFields, iAssociates, iObjects: integer);
    procedure AddLongObject(iObjectType, iKeys, iFields, iAssociates, iObjects: integer);
    procedure UpdateLongObject(iPos: integer; iObjectType, iKeys, iFields, iAssociates, iObjects: integer);
    procedure AddLongLong(int: int64);
    procedure AddFlexInt(int: int64);
    procedure AddInt(i: int64);
    function AddVariant(v: variant): boolean;

    //Sequetial reading function
    procedure SeqDecodeObjectHeader(OUT iObjectType, iKeys,
        iFields, iAssociates, iObjects: cardinal);

    function SeqRead: variant;
    procedure SeqSeek(iPos: integer);
    function SeqReadBytes(out iLength: int64): PByte;overload;
    function SeqReadBytes: TDynByteArray;overload;

    function Eof: boolean;

    procedure Encrypt;
    procedure Decrypt;

    function GetBufferAsString: string;
    property BufferAsString: string read GetBufferAsString;


    //These definitions are standard for response packets.
    //They cannot be accessed unless the packet was recieved from the network.
    property ResponseType: smallint read GetResponseType;
    property Result: boolean read GetResult;
    property SessionID: integer read GetSessionID;
    property ErrorCode: smallint read GetErrorCode;
    property Message: string read GetMessage;

    //Packet Integrity Checking
    function IsMarkerValid: boolean;
      //Tests the marker value against Pre-defined constant
    function IsCRCValid: boolean;
      //Performs CRC Check and compares against CRC Property of packet
    function IsValid: boolean;
      //Combines marker and CRC check in one call

    //Additional Packet Attributes
    property IsResponse: boolean read GetIsResponse;
      //tells whether or not the packet is intended for sending or recieving
    property SeqReadPos: integer read iSeqReadPos write iSeqReadPos;
    property NextDataType: TRDTPDataType read GetNextDataType;
    property Origin: TPacketOrigin read FOrigin write FOrigin;
    property SeqWritePos: integer read iInsertionPOint write iInsertionPOint;
    property BufferLength: integer read GetBufferLength;

    procedure ReturnError(sMessage: string);
    property PlatformOptions: cardinal read FPlatformOptions write FPlatformOptions;
    procedure AssignBuffer(p: pbyte; l: ni);
  end;

//Supplemental functions
function VerbBool(b: boolean) : string;
function BoolToStr(b: boolean) : string;
function VArBoolToStr(v: variant) : string;
procedure xorCrypt(buffer: PByte; iLength: integer; key: string);
(*var
  DefEncKey : string;*)

implementation


//###########################################################################
// TRDTPPacket
//###########################################################################

constructor TRDTPPacket.create;
//Things to do on create
//--Create buffer for howling the RAW packet data
//--Initialize field FEncrypted to FALSE
//--Initialize InsertionPoint to 18 (beginning of UserData area)
//--Initialize header values
begin
  inherited;
  bDataCountOutOfDate  := true;

  //Allocate the maximum possible size for the packet in memory
  FBuffer := TNetworkBuffer.create(MAX_PACKET_SIZE);

  FOrigin := poClient;

  //Initialize default packet values
  initialize;

end;
//----------------------------------------------------------------------------
procedure TRDTPPacket.Initialize;
begin
  with FBuffer do begin
    //Place initial values in packet header
    //(rest of packet remains uninitialized)
    Pokelong(PACKET_MARKER,  PACKET_ADDRESS_MARKER);
    Pokelong($00000000,      PACKET_ADDRESS_PACKED_LENGTH);
    Pokelong($00000000,      PACKET_ADDRESS_UnPACKED_LENGTH);
    PokeByte($00000001,      PACKET_ADDRESS_TYPE);
    PokeLong($000000000,     PACKET_ADDRESS_CRC);
    PokeByte($0000,         PACKET_ADDRESS_ENCRYPTION);

  end;
  FEncrypted := false;
  FCRCUpdated := false;
  iInsertionPoint:=PACKET_ADDRESS_USERDATA;
  iLastSearchIndex := 0;
  iLastSearchPointer := PACKET_ADDRESS_USERDATA;
  iSeqReadPos := 0;
  bDataCountOutOfDate := true;
  FDataCountCache:=0;
  SeqSeek(0);
end;

//----------------------------------------------------------------------------
destructor TRDTPPacket.destroy;
begin
  FBuffer.free;
  inherited;
//  Debug.Log(self, 'Freeing packet.');
end;
//----------------------------------------------------------------------------
function TRDTPPacket.GetMarker: integer;
begin
  result := FBuffer.PeekLong(PACKET_ADDRESS_MARKER);
end;
//----------------------------------------------------------------------------
procedure TRDTPPacket.SetMarker(l: integer);
begin
  FBuffer.PokeLong(l, PACKET_ADDRESS_MARKER);
end;
//----------------------------------------------------------------------------
function TRDTPPacket.GetPackedLength: integer;
begin
  if Fbuffer = nil then
    raise ECritical.create('packet has no Fbuffer');
  result := FBuffer.PeekLong(PACKET_ADDRESS_PACKED_LENGTH);
end;
function TRDTPPacket.GetUnPackedLength: integer;
begin
  if Fbuffer = nil then
    raise ECritical.create('packet has no Fbuffer');
  result := FBuffer.PeekLong(PACKET_ADDRESS_UNPACKED_LENGTH);
end;

//----------------------------------------------------------------------------
function TRDTPPacket.GetPacketType: byte;
begin
  result := FBuffer.PeekByte(PACKET_ADDRESS_TYPE);
end;
//----------------------------------------------------------------------------
procedure TRDTPPacket.SetPacketType(l: byte);
begin
  BeginPacketChange;
  FBuffer.PokeByte(l, PACKET_ADDRESS_TYPE);
  EndPacketChange;
end;
//----------------------------------------------------------------------------

function TRDTPPacket.GetCRC: cardinal;
begin
  result := cardinal(FBuffer.PeekLong(PACKET_ADDRESS_CRC));
end;
//----------------------------------------------------------------------------
function TRDTPPacket.GetDataType(idx: integer): TRDTPDataType;
var
  idxSearchPointer: integer;
begin
  BeginPacketRead;
  idxSearchPointer := GetPointerToData(idx); //<<-Start Seraching at byte 18
  result := DecryptedBuffer.PeekByte(idxSearchPointer);
end;
//----------------------------------------------------------------------------
function TRDTPPacket.GetData(idx: integer): variant;
//This function is wierd
var
  idxSearchPointer: integer;
  iDataType: TRDTPDataType;
  i64Temp: int64;
begin
  BeginPacketRead;
  //Get Pointer to data
  idxSearchPointer := GetPointerToData(idx); //<<-Start Seraching at byte 18

  //Now that the pointer (idxSearchPointer) is placed in the proper spot in the
  //buffer... return the proper value
  iDataType := DecryptedBuffer.PeekByte(idxSearchPointer); inc(idxSearchPointer);
  try
    case iDataType of
      PDT_SHORT:  result := smallint(DecryptedBuffer.PeekShort(idxSearchPointer));
      PDT_LONG:   result := DecryptedBuffer.PeekLong(idxSearchPointer);
      PDT_STRING: begin
                  //if a string, increment the pointer by the length of the string
                  //as reported by the LONG following the Data Type Byte
                  result := DecryptedBuffer.PeekFmtString(idxSearchPointer);
                end;
      PDT_BYTES:  begin
                  //if a byte array, increment the pointer by the length of the string
                  //as reported by the LONG following the Data Type Byte
                  RAISE Exception.Create('bytes found... cannot read variant');

                end;
      PDT_DOUBLE: begin
                    result := DecryptedBuffer.PeekDouble(idxSearchPointer);
                  end;
      PDT_SHORT_OBJECT: begin
                    result := '(short object)';
                  end;
      PDT_LONG_OBJECT: begin
                    result := '(long object)';
                  end;
      PDT_DATETIME: begin
                    result := DecryptedBuffer.PeekDateTime(idxSearchPointer);
                  end;
      PDT_LONG_LONG: begin
                    i64Temp := DecryptedBuffer.PeekLongLong(idxSearchPointer);
                    result := i64Temp;
                  end;
      PDT_BOOLEAN:  begin
                    result := DecryptedBuffer.PeekBoolean(idxSearchPointer);
                  end;
      PDT_NULL:  begin
                    result := null;
                  end;



    else begin
        //windows.beep(764, 200);
        raise EPacketError.createFMT('GetData() Packet format error, unrecognized type byte at %d.  Value: %d ... Previous Data Type was %d '+#13#10+GetDebugMessage, [idxSearchPOinter,iDataTYpe, FPreviousgetDataType]);
      end;
    end;
  except
    on E: Exception do begin
      e.message := 'Packet format error, value type at '+inttostr(idxSearchPOinter)+' could not be converted to type '+inttostr(iDAtaType)+' ** '+e.message;
      raise;
    end;
  end;

  FPreviousGetDataType := iDataType;
end;
//----------------------------------------------------------------------------
function TRDTPPacket.GetDataCount: integer;
var
  idxSearchPointer: int64;
  iDataType: TRDTPDataType;
  iDataLength: int64;
begin
  BeginPacketRead;
  if not bDataCountOutOfDate then begin
    result := FDataCountCache;
    exit;
  end;

  result := 0;
  idxSearchPointer := PACKET_ADDRESS_USERDATA; //<<-Start Seraching at byte 18

  //Sift through all the entries in the packet... entries can vary
  //in length.
  //DO UNTIL THE SEARCH POINTER REACHES THE REPORTED LENGTH OF THE PACKET
  while idxSearchPointer<self.UnpackedLength do begin
    iDataType := DecryptedBuffer.PeekByte(idxSearchPointer); inc(idxSearchPointer);

(*      showmessage(inttostr(idxSearchPointer)+' of '+inttostr(self.length)+'  pos: '+inttostr(result)+' Type: '+inttostr(iDataType));*)

    case iDataType of
      PDT_NULL:  inc(idxSearchPointer, 0);
      PDT_SHORT:  inc(idxSearchPointer, 2);
      PDT_LONG:   inc(idxSearchPointer, 4);
      PDT_STRING: begin
                  //if a string, increment the pointer by the length of the string
                  //as reported by the LONG following the Data Type Byte
                  iDataLength := DecryptedBuffer.PeekLongLong(idxSearchPointer);
                  inc(idxSearchPointer, (iDataLength*sizeof(TRDTPchar))+8);
                end;
      PDT_BYTES:  begin
                  //if a byte array, increment the pointer by the length of the string
                  //as reported by the LONG following the Data Type Byte
                  iDataLength := DecryptedBuffer.PeekLongLong(idxSearchPointer);
                  inc(idxSearchPointer, iDataLength+8);
                end;
      PDT_DOUBLE: begin
                  inc(idxSearchPointer, PDT_LENGTH_DOUBLE);
                end;
      PDT_SHORT_OBJECT: begin
                  inc(idxSearchPointer, PDT_LENGTH_SHORT_OBJECT);
                end;
      PDT_LONG_OBJECT: begin
                  inc(idxSearchPointer, PDT_LENGTH_LONG_OBJECT);
                end;
      PDT_DATETIME: begin
                  inc(idxSearchPointer, PDT_LENGTH_DATETIME);
                end;
      PDT_LONG_LONG: begin
                  inc(idxSearchPointer, PDT_LENGTH_LONG_LONG);
                end;
      PDT_BOOLEAN: begin
                  inc(idxSearchPointer, PDT_LENGTH_BOOLEAN);
                end;

    else begin
        //windows.beep(764, 200);
        raise EPacketError.createFMT('GetDataCount() Packet format error, unrecognized type byte at %d.  Value: %d LastType=%d '+#13#10+GetDebugMessage, [idxSearchPointer,iDataTYpe, FPreviousGetDataCountType]);
      end;
    end;
    //Increment the result count after readjusting pointer
    inc(result);
    FDataCountCache := result;
    bDataCountOutOfDate := false;
    FPreviousGetDataCountType := iDataType;
  end;
end;
//---------------------------------------------------------------------------
function TRDTPPacket.GetEncryption: byte;
begin
  result := FBuffer.PeekByte(PACKET_ADDRESS_ENCRYPTION);
end;
//---------------------------------------------------------------------------
procedure TRDTPPacket.Addlong(l: integer);
begin
{$IFDEF LOG_PACKET_DATA}GLOG.Debug('AddLong @'+inttostr(iInsertionPoint));{$ENDIF}
  BeginPacketChange;
  //put the data type byte
  FBuffer.PokeByte(PDT_LONG, iInsertionPoint);
  inc(iInsertionPoint);

  //put the stuff
  FBuffer.PokeLong(l, iInsertionPoint);
  inc(iInsertionPoint,4);

  //Recalc internal length stored in packet
  EndPacketChange;

end;
procedure TRDTPPacket.Clear;
begin
  BeginPacketCHange;
  self.iInsertionPoint := GetPointerToData(0);
  iLastSearchIndex := 0;
  iLastSearchPointer := PACKET_ADDRESS_USERDATA;

  EndPacketChange;



end;
//---------------------------------------------------------------------------
procedure TRDTPPacket.AddShort(i: smallint);
begin
{$IFDEF LOG_PACKET_DATA} GLOG.Debug('AddShort @'+inttostr(iInsertionPoint));{$ENDIF}

  BeginPacketChange;
  //put the data type byte
  FBuffer.PokeByte(PDT_SHORT, iInsertionPoint);
  inc(iInsertionPoint);

  //put the stuff
  FBuffer.PokeShort(i, iInsertionPoint);
  inc(iInsertionPoint,2);

  //Recalc internal length stored in packet
  EndPacketChange;
end;
//---------------------------------------------------------------------------
procedure TRDTPPacket.AddString(s: string);
var
  iTemp: integer;
begin
{$IFDEF LOG_PACKET_DATA}GLOG.Debug('AddString "'+s+'"@'+inttostr(iInsertionPoint));{$ENDIF}
  BeginPacketChange;

  //put the data type byte
  FBuffer.PokeByte(PDT_STRING, iInsertionPoint);
  inc(iInsertionPoint);

  //put the stuff
  FBuffer.PokeFmtString(s, iInsertionPoint);
  iTemp := (System.length(s)*sizeof(char))+8;
  inc(iInsertionPoint, iTemp);

  //Recalc internal length stored in packet
  EndPacketChange;
{$IFDEF LOG_PACKET_DATA}GLOG.Debug('After AddString @'+inttostr(iInsertionPoint));{$ENDIF}

end;
//---------------------------------------------------------------------------
procedure TRDTPPacket.AddBytes(pc: Pbyte; iLength: int64);
var
  t: integer;
  b: byte;
begin
  BeginPacketChange;
  //put the data type byte
  Fbuffer.PokeByte(PDT_BYTES, iInsertionPoint);
  inc(iInsertionPoint);

  Fbuffer.PokeLongLong(iLength, iInsertionPoint);
  inc(iInsertionPoint, 8);


  //put the stuff
{$IFDEF SLOW_POKE_BYTES}
  for t := 0 to iLength-1 do begin
    b := ord((PAddable(pc)+t)^);
    Fbuffer.PokeByte(b, iInsertionPoint);
    inc(iInsertionPoint,1);
  end;
{$ELSE}
  FBuffer.pokeBytes(pc, iInsertionPoint, iLength);
  inc(iInsertionPoint,ilength);
{$ENDIF}
  EndPacketChange;
end;
//---------------------------------------------------------------------------
function TRDTPPacket.AddVariant(v: variant): boolean;
//Adds a variant type to the packet by querying its type information
//Returns true if successful
//False if variant type not supported.... in which case you may handle externally
var
  t: integer;
  i: integer;
begin
  result := true;
  i := VarType(v);

  if (i = 271) then //custom variant type for 64-bit mySQL integers
{$IFNDEF VER180}    v := VarAsType(v, varInt64);
{$ELSE}             v := VarAsType(v, varDate);{$ENDIF}
  if (i = 272) then      //custom variant type for MYSQL Dates
{$IFNDEF VER180}    v := VarAsType(v, varDate);
{$ELSE}             v := VarAsType(v, varInt64);{$ENDIF}



  case Vartype(v) of
    varInteger, varInt64: AddFlexInt(v);
    varSmallint, varShortInt, varByte, varWord, varLongWord: AddFlexInt(v);
    varString, varUString, varOleStr: AddString(v);
    varDate: AddDateTime(v);
    varDouble, varSingle, varCurrency, 273, 274: AddDouble(v);
    varNull,0: AddNull;

    varBoolean: AddBoolean(v);
  else
    if (VarType(v) and VarArray) = varArray then begin
      for t := VarArrayLowBound(v,1) to VarArrayHighBound(v,1) do begin
        AddVariant(v[t]);
      end;
    end
    else raise EPacketError.CreateFmt(sErrVariantUnsupported, [varType(v)]);

    result :=false;
  end;
end;

procedure TRDTPPacket.AssignBuffer(p: pbyte; l: ni);
begin
  FEncrypted := true;
  FBuffer.AssignBuffer(p,l);
end;

//---------------------------------------------------------------------------
function TRDTPPacket.IsMarkerVAlid;
begin
  result := (marker = PACKET_MARKER);
end;

//---------------------------------------------------------------------------
function TRDTPPacket.IsCRCValid: boolean;
begin
  result := (ComputeCRC = CRC);
end;

//---------------------------------------------------------------------------
function TRDTPPacket.IsValid: boolean;
begin
  result := (IsMarkerValid and IsCRCValid);
end;
//---------------------------------------------------------------------------
procedure TRDTPPacket.RecalcLength;
begin
  FBuffer.PokeLong(iInsertionPoint, PACKET_ADDRESS_UNPACKED_LENGTH);
  bDataCountOutOfDate := true;
end;
procedure TRDTPPacket.ReturnError(sMessage: string);
var
  iRQ: integer;
  iSessionID: integer;
  sService: string;
begin
  iRQ := data[0];
  iSessionID := data[1];
  sService := data[2];
  Clear;

  AddShort(iRQ);
  AddLong(iSessionID);
  //AddString(sService);
  AddBoolean(false);//result
  AddShort(0);//error code
  AddString(sMessage);//error message
end;

//---------------------------------------------------------------------------
function TRDTPPacket.GetResponseType: smallint;
begin
  BeginPacketRead;
  if not IsResponse then
    raise EPacketError.createFMT('Attempting to read a response property '+
    'from a packet that is not a response.',[]);
  result := Data[PACKET_INDEX_RESPONSE_TYPE];
end;
//---------------------------------------------------------------------------
function TRDTPPacket.GetResult: boolean;
begin
  BeginPacketRead;
  if not IsResponse then
    raise EPacketError.createFMT('Attempting to read a response property '+
    'from a packet that is not a response.',[]);
  result := Data[PACKET_INDEX_RESULT]<>0;
end;
//---------------------------------------------------------------------------
function TRDTPPacket.GetErrorCode: smallint;
begin
  BeginPacketRead;
  if not IsResponse then
    raise EPacketError.createFMT('Attempting to read a response property '+
    'from a packet that is not a response.',[]);
  result := Data[PACKET_INDEX_ERROR_CODE];
end;
//---------------------------------------------------------------------------
function TRDTPPacket.GetMessage: string;
begin
  BeginPacketRead;
  if not IsResponse then
    raise exception.create('Attempting to read a '+
    'response property from a packet that is not a response.');

  result := VarToStr(Data[PACKET_INDEX_MESSAGE]);


end;
//---------------------------------------------------------------------------
function TRDTPPacket.GetIsResponse : boolean;
begin
  if self = nil then
    raise exception.create('self is nil');
  result := Origin = poServer;

end;
//---------------------------------------------------------------------------
function TRDTPPacket.GetLengthFromDataPointer(ptr: integer): integer;
begin
  BeginPacketRead;
  result := DecryptedBuffer.PeekLong(ptr+1);

end;
//---------------------------------------------------------------------------

procedure TRDTPPacket.SetEncryption(i: byte);
begin
//  BeginPacketChange;
  FBuffer.PokeByte(i, PACKET_ADDRESS_ENCRYPTION);
//  EndPacketChange;
end;
//---------------------------------------------------------------------------
function TRDTPPacket.GetEncryptedBuffer: TNetworkBuffer;
begin
  if not Encrypted then Encrypt;
  result := Fbuffer;

end;
//---------------------------------------------------------------------------
function TRDTPPacket.GetDecryptedBuffer: TNetworkBuffer;
begin
  if Encrypted then Decrypt;
  result := Fbuffer;

end;

//---------------------------------------------------------------------------
function TRDTPPacket.GetPointerToData(idx: integer): integer;
//returns a pointer to the start of the data at a particular index
//returns pointer will point to the type byte before the actual type payload
var
  iCurrentIndex: integer;
  idxSearchPointer: integer;
  iDataType: TRDTPDataType;
  iDataLength: integer;
begin
  BeginPacketRead;
  if Encrypted then Decrypt;
  if (idx >= iLastSearchIndex) then begin
    iCurrentIndex := iLastSearchIndex;
    idxSearchPointer := iLastSearchPointer;
  end else begin
    iCurrentIndex :=0; //<<-Must sift though packet and increment
    idxSearchPointer := PACKET_ADDRESS_USERDATA; //<<-Start Seraching at byte 18
  end;

  //Part II

  //Sift through all the preceding entries in the packet... entries can vary
  //in length.
  //DO UNTIL WE LAND ON THE INDEX REQUESTED
  while iCurrentIndex<idx do begin
    //Get the data type byte from packet and increment search pointer
    iDataType := FBuffer.PeekByte(idxSearchPointer);
    inc(idxSearchPointer);
    //depending on data type, increment search pointer by varying amounts
    case iDataType of
      PDT_NULL:  inc(idxSearchPointer, PDT_LENGTH_NULL);
      PDT_SHORT:  inc(idxSearchPointer, PDT_LENGTH_SHORT);
      PDT_LONG:   inc(idxSearchPointer, PDT_LENGTH_LONG);
      PDT_LONG_LONG: inc(idxSearchPointer, PDT_LENGTH_LONG_LONG);
      PDT_STRING: begin
                  //if a string, increment the pointer by the length of the string
                  //as reported by the LONG following the Data Type Byte
                  iDataLength := FBuffer.PeekLong(idxSearchPointer);
                  inc(idxSearchPointer, (iDataLength*sizeof(char))+8);
                end;
      PDT_BYTES:  begin
                  //if a byte array, increment the pointer by the length of the string
                  //as reported by the LONG following the Data Type Byte
                  iDataLength := FBuffer.PeekLong(idxSearchPointer);
                  inc(idxSearchPointer, iDataLength+8);
                end;
      PDT_DOUBLE: begin
                  inc(idxSearchPointer, PDT_LENGTH_DOUBLE);
                end;
      PDT_SHORT_OBJECT: begin
                  inc(idxSearchPointer, PDT_LENGTH_SHORT_OBJECT);
                end;
      PDT_LONG_OBJECT: begin
                  inc(idxSearchPointer, PDT_LENGTH_LONG_OBJECT);
                end;
      PDT_DATETIME: begin
                    inc(idxSearchPointer, PDT_LENGTH_DATETIME);
                end;
      PDT_BOOLEAN: begin
                    inc(idxSearchPointer, PDT_LENGTH_BOOLEAN);
                end;
    else begin
        //windows.beep(764, 200);
        raise EPacketError.createFMT('GetInsertionpoint() Packet format error, unrecognized type byte at %d.  Value: %d', [idxSearchPOinter,iDataTYpe]);
      end;
    end;
    //FINALLY: Increment the index after readjusting pointers

    inc(iCurrentIndex);
  end;
  iLastSearchPOinter := idxSearchPointer;
  iLastSearchIndex := iCurrentIndex;

  result := idxSearchPointer;
end;
//---------------------------------------------------------------------------
//INTERNAL HOUSEKEEPING
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------

procedure TRDTPPacket.Encrypt;
var
  pfrom, pto: Pbyte;
  szfrom, szto: ni;
begin
  if encrypted then exit;
  if unpackedlength = 0 then begin
    Fencrypted := true;
  end;
(*  case self.Encryption of
    1:;
    0:
      xorCrypt(FBuffer.RawBuffer+PACKET_ADDRESS_USERDATA,
        self.Length-PACKET_HEADER_SIZE, DefEncKey);
    2:
      Raise EPacketError.createFMT('Unsupported encryption %d', [self.Encryption]);
  else
    Raise EPacketError.createFMT('Illegal encryption %d', [self.Encryption]);
  end;*)

  if 0<>(PlatformOptions and PACKET_PLATFORM_OPTION_SUPPORTS_COMPRESSION) then begin
    pfrom := self.FBuffer.FRealBuffer;
    szFrom := self.UnpackedLength;
    if szFrom <= 0 then
      raise ETransportError.create('cannot encrypt a packet that is '+inttostr(szFrom)+' length.');
    pto := GetMemory(szFrom);
    //zip ram starting BEYOND header
    szto := zip.ZipRam(@pFrom[sizeof(TRDTPHEader)], @pTo[sizeof(TRDTPHEader)], szFrom-sizeof(TRDTPHEADER), szFrom-sizeof(TRDTPHEADER), nil);
    //if compression successful
    if szto > 0 then begin
      //move header
      movemem32(pto, pFrom, sizeof(TRDTPHEader));

      //encrypt everything AFTER header
      XOREncrypt(@pto[sizeof(TRDTPHeader)], szTo, @CK[0], length(CK));

      self.FBuffer.AssignBuffer(pto, szTo+sizeof(TRDTPHEADER));
      FBuffer.PokeLong(szTo+sizeof(TRDTPHEADER), PACKET_ADDRESS_PACKED_LENGTH);
      Encryption := ENCRYPT_RLE;
      FEncrypted := true;
    end else begin
      FBuffer.PokeLong(UnpackedLength, PACKET_ADDRESS_PACKED_LENGTH);
      Encryption := ENCRYPT_VERBATIM;
      FreeMemory(pTo);
    end;
  end else begin
    FBuffer.PokeLong(UnpackedLength, PACKET_ADDRESS_PACKED_LENGTH);
    Encryption := ENCRYPT_VERBATIM;
  end;


  FEncrypted := true;
end;
//---------------------------------------------------------------------------
procedure TRDTPPacket.Decrypt;
var
  szFrom, szTo, szNew: ni;
  pFrom, pCopy, pTo: pbyte;
begin
  if not encrypted then exit;

(*  case self.Encryption of
    1:; //None

    0:
      xorCrypt(FBuffer.RawBuffer+PACKET_ADDRESS_USERDATA,
        self.Length-PACKET_HEADER_SIZE, DefEncKey);
    2:
      Raise EPacketError.createFMT('Unsupported encryption %d', [self.Encryption]);
  else
    Raise EPacketError.createFMT('Illegal encryption %d', [self.Encryption]);
  end;*)
  case Encryption of
    ENCRYPT_RLE:
    begin
      szFrom := Self.Packedlength;
      szTo := self.UnPackedLength;
      pTo := GetMemory(szTo);
      try
        pFrom := Fbuffer.FRealBuffer;
        pCopy := GetMemory(szTo);
        try
          movemem32(pCopy, pFrom, szFrom);
          //decrypt everything AFTER header
          XORDecrypt(@pcopy[sizeof(TRDTPHeader)], szFrom-sizeof(TRDTPHEADER), @CK[0], length(CK));
          //unzip decrypted results
          szNew := UnZipRam(@pCopy[sizeof(TRDTPHEADER)], @pTo[sizeof(TRDTPHEADER)], szFrom-sizeof(TRDTPHEADER), szTo-sizeof(TRDTPHEADER), nil);
          Movemem32(pTo, pCopy, sizeof(TRDTPHEADER));
          self.FBuffer.AssignBuffer(pTo, szNew+sizeof(TRDTPHEADER), szTo+sizeof(TRDTPHEADER));//destroys pFrom
          pTo := nil;//pTO is consumed by AssignBuffer
          Encryption := ENCRYPT_VERBATIM;
        finally
          FreeMemory(pCopy);
        end;
      finally
        if pTo <> nil then
          FreeMemory(pTo);
      end;
    end;
    ENCRYPT_VERBATIM:
    begin
      //
    end;
  else
    raise EPacketError.create('Unknown encryption 0x'+inttohex(Encryption,4));
  end;


  FEncrypted := false;

end;

procedure TRDTPPacket.CalcCRC;
begin
  DecryptedBuffer.PokeLong(ComputeCRC, PACKET_ADDRESS_CRC);
end;


//---------------------------------------------------------------------------
//STATE CHANGING
//---------------------------------------------------------------------------
function TRDTPPacket.BeginPacketRead: boolean;
begin
  if Encrypted then Decrypt;
  result := true;
end;
//---------------------------------------------------------------------------
function TRDTPPacket.BeginPacketChange: boolean;
begin
  if Encrypted then Decrypt;
  result := true;

end;
//---------------------------------------------------------------------------
function TRDTPPacket.EndPacketChange: boolean;
begin
  RecalcLength;
  CalcCRC;
  result := true;
end;
//---------------------------------------------------------------------------
//SUPPLEMENTAL FUNCTIONS
//---------------------------------------------------------------------------
function TRDTPPacket.ComputeCRC: cardinal;
var
  checksum : cardinal;
  i: integer;
  orgCRC: integer;
begin
  result := 0;
  {$IFDEF DISABLE_CRC}
  exit;
  {$ENDIF}
  if Encrypted then Decrypt;

  checksum := 0;

  //Get the original CRC from the packet;
  orgCRC := self.CRC;

  //Set the CRC to 0 so that is does not corrupt the CRC after
  //being inserted into the packet
  DecryptedBuffer.PokeLong($00000000, PACKET_ADDRESS_CRC);

  for i := 0 to self.PackedLength-1 do begin
    inc(CheckSum, ord(Fbuffer.RawBuffer[i]));

    if (checksum and $80000000) = $80000000 then
      checksum := LOWORD(checksum)+HIWORD(checksum);
  end;
  DecryptedBuffer.PokeLong(orgCRC, PACKET_ADDRESS_CRC);

  result := checksum;
end;

//---------------------------------------------------------------------------
procedure TRDTPPacket.ShowDebugMessage;
begin
  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;
//---------------------------------------------------------------------------
procedure TRDTPPacket.DecodeObjectHeader(index: integer; OUT iObjectType, iKeys,
    iFields, iAssociates, iObjects: cardinal);
var
  iPointer: Integer;
  iError: integer;
  iType: integer;
begin
  iPointer := GetPointerToData(index);

  iType := FBuffer.PeekByte(iPOinter);
  case iType of
    PDT_SHORT_OBJECT: begin
      iObjectType := Word(Fbuffer.PeekShort(iPointer+1));
      iKeys := Word(Fbuffer.PeekByte(iPointer+3));
      iFields := Fbuffer.PeekByte(iPointer+4);
      iAssociates := Fbuffer.PeekByte(iPointer+5);
      iObjects := word(Fbuffer.PeekShort(iPointer+6));
    end;
    PDT_LONG_OBJECT: begin
      iObjectType := word(Fbuffer.PeekShort(iPointer+1));
      iKeys := word(Fbuffer.PeekShort(iPointer+3));
      iFields := word(Fbuffer.PeekShort(iPointer+5));
      iAssociates := word(Fbuffer.PeekShort(iPointer+7));
      iObjects := cardinal(Fbuffer.PeekLong(iPointer+9));
    end;
  else
    begin
      iError := FBuffer.PeekByte(ipointer);
      raise EPacketError.createFMT('Object header not found when attempting to read object from packet. Type found %d at index %d', [iError, index]);
    end;
  end;

end;
//---------------------------------------------------------------------------
procedure TRDTPPacket.SeqDecodeObjectHeader(OUT iObjectType, iKeys, iFields,
              iAssociates, iObjects: cardinal);
begin
  DecodeObjectHeader(iSeqReadPos, iObjectType, iKeys, iFields, iAssociates, iObjects);
  inc(iSeqReadPos);
end;
//---------------------------------------------------------------------------
function TRDTPPacket.SeqRead: variant;
begin
  //validation
  if iSeqReadPos >= DataCount then begin
    //windows.beep(763,200);
    raise EPacketError.CreateFMT('Sequential read past end of packet. @'+inttostr(iSeqReadPos)+' where data count is '+inttostr(DataCount), []);

  end;
  result := Data[iSeqReadPos];
  inc(iSeqReadPos);

end;
function TRDTPPacket.SeqReadBytes: TDynByteArray;
var
  iType: byte;
  idxSearchPointer: integer;
  iLength: ni;
begin
  //check to make sure the next tyype is bytes
  if datatype[self.SeqReadPos] <> PDT_BYTES then begin
    raise EPacketError.create('expected bytes type in packet, but found type:'+inttostr(datatype[SeqReadPos])+#13#10+GetDebugMessage);

  end;
  //read the length
  idxSearchPointer := GetPointerToData(SeqReadPos);
  inc(idxSearchPointer);
  //get the length of the buffer
  iLength := self.DecryptedBuffer.PeekLongLong(idxSearchPointer);
  setlength(result, iLength);
  inc(IdxSearchPointer,8);
  //allocate result buffer to length
  if iLength > 0 then
    MoveMem32(@result[0], Decryptedbuffer.FRealBuffer+idxSearchPointer, iLength);
  SeqReadPos := SeqReadPos + 1;
end;

function TRDTPPacket.SeqReadBytes(out iLength: int64): PByte;
var
  iType: byte;
  idxSearchPointer: integer;
begin
  //check to make sure the next tyype is bytes
  if datatype[self.SeqReadPos] <> PDT_BYTES then begin
    raise EPacketError.create('expected bytes type in packet, but found type:'+inttostr(datatype[SeqReadPos])+#13#10+GetDebugMessage);

  end;
  //read the length
  idxSearchPointer := GetPointerToData(SeqReadPos);
  inc(idxSearchPointer);
  //get the length of the buffer
  iLength := self.DecryptedBuffer.PeekLongLong(idxSearchPointer);
  inc(IdxSearchPointer,8);
  //allocate result buffer to length
  Getmem(result, iLength);
  MoveMem32(result, Decryptedbuffer.FRealBuffer+idxSearchPointer, iLength);
  SeqReadPos := SeqReadPos + 1;


end;

//---------------------------------------------------------------------------
procedure TRDTPPacket.SeqSeek(iPos: integer);
begin
  iSeqReadPos := iPos;
end;
//---------------------------------------------------------------------------
function TRDTPPacket.GetNextDataType: TRDTPDataType;
begin
  result := GetDataType(iSeqReadPos);
end;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
procedure xorCrypt(buffer: PByte; iLength: integer; key: string);
var
  i: integer;
  j: byte;
begin
  j:=STRZ;
  for i := 0 to iLength-1 do begin
    buffer[i] := buffer[i] xor (ord(key[j]) and 255);
    inc(j);
    if (j > ((Length(key)-1)+STRZ)) then j:=STRZ;
  end;
end;
//----------------------------------------------------------------------------
function VerbBool(b: boolean) : string;
begin
  if b then result := 'VALID' else result := 'INVALID';
end;
//----------------------------------------------------------------------------
function BoolToStr(b: boolean) : string;
begin
  if b then result := 'TRUE' else result := 'FALSE';
end;

function VArBoolToStr(v: variant) : string;
begin
  if varType(v) <> varBoolean then
    result := '**NOT A BOOL**'
  else
  if v then result := 'TRUE' else result := 'FALSE';
end;
//----------------------------------------------------------------------------
procedure TRDTPPacket.AddObject(iObjectType, iKeys, iFields, iAssociates,
  iObjects: integer);
var
  bUseLong: boolean;
begin
  //range checking

  //if the negative bit is turned on for any values passes, then EXCEPT
  if (iObjectType or iKeys or iFields or iAssociates or iObjects) < 0 then
    raise Exception.create('Negative value passed to TRDTPPacket.AddObject');

  //if object type constant is greater than 16-bits wide then except
  if not ((iObjectType and $FFFF0000) = 0) then
    raise Exception.create('Object constant out of range.  Value: '+inttostr(iObjectType));

  //if number of keys is greater than 16-bits wide then except
  if not ((iKeys and $FFFF0000) = 0) then
    raise Exception.create('Key count out of range.  Value: '+inttostr(iKeys));

  //if number of fields is greater than 16-bits wide then except
  if not ((iFields and $FFFF0000) = 0) then
    raise Exception.create('Field count out of range.  Value: '+inttostr(iFields));

  //if number of associates is greater than 16-bits wide then except
  if not ((iAssociates and $FFFF0000) = 0) then
    raise Exception.create('Associate count out of range.  Value: '+inttostr(iAssociates));

  //number of objects can be 32-bits wide, so we don't need checking

  //check to see what type of object header to use SHORT or LONG
  bUseLong := false;
  //if any of the parameters are out of range for the SHORT form, then use the long

  //Key limit is 255 (one byte)
  if not ((iKeys and $FFFFFF00) = 0) then
    bUseLong := true;

  //Field limit is 255 (one byte)
  if not ((iFields and $FFFFFF00) = 0) then
    bUseLong := true;

  //Associate limit is 255 (one byte)
  if not ((iAssociates and $FFFFFF00) = 0) then
    bUseLong := true;

  //Object limit is 65536/32768
  if not ((iObjects and $FFFF0000) = 0) then
    bUseLong := true;

  //call appropriate SHORT/LONG call.
  if bUseLong then
    AddLongObject(iObjectType, iKeys, iFields, iAssociates, iObjects)
  else
    AddShortObject(iObjectType, iKeys, iFields, iAssociates, iObjects);


end;
//----------------------------------------------------------------------------
procedure TRDTPPacket.AddShortObject(iObjectType, iKeys, iFields,
  iAssociates, iObjects: integer);
//Description: Adds a header to mark the beginning of a Data object mashalled
//into the packet in SHORT form.  See docuement "Marshall or UnMarshall".
//NOTE:
//this function should be protected and ONLY called by AddObject.  AddObject
//should handle all range checking for this routine.
begin
  //put packet into change state
  BeginPacketChange;

  //put the data type BYTE
  Fbuffer.PokeByte(PDT_SHORT_OBJECT, iInsertionPoint);
  inc(iInsertionPoint);

  //put the object type WORD
  Fbuffer.PokeShort(iObjectType, iInsertionPoint);
  inc(iInsertionPoint,2);

  //put the key count BYTE
  Fbuffer.PokeByte(iKeys, iInsertionPoint);
  inc(iInsertionPoint);

  //put the Field Count BYTE
  Fbuffer.Pokebyte(iFields, iInsertionPoint);
  inc(iInsertionPoint);

  //put the Associate Count BYTE
  Fbuffer.Pokebyte(iAssociates, iInsertionPoint);
  inc(iInsertionPoint);

  //put the Object Count WORD
  Fbuffer.PokeShort(iObjects, iInsertionPoint);
  inc(iInsertionPoint,2);

  EndPacketChange;

end;
//----------------------------------------------------------------------------
procedure TRDTPPacket.AddLongObject(iObjectType, iKeys, iFields,
  iAssociates, iObjects: integer);
//Description: Adds a header to mark the beginning of a Data object mashalled
//into the packet in LONG form.  See docuement "Marshall or UnMarshall".
//NOTE:
//this function should be protected and ONLY called by AddObject.  AddObject
//should handle all range checking for this routine.
begin
  //put packet into change state
  BeginPacketChange;

  //put the data type BYTE
  Fbuffer.PokeByte(PDT_LONG_OBJECT, iInsertionPoint);
  inc(iInsertionPoint);

  //put the object type WORD
  Fbuffer.PokeShort(iObjectType, iInsertionPoint);
  inc(iInsertionPoint,2);

  //put the key count WORD
  Fbuffer.PokeShort(iKeys, iInsertionPoint);
  inc(iInsertionPoint,2);

  //put the Field Count WORD
  Fbuffer.PokeShort(iFields, iInsertionPoint);
  inc(iInsertionPoint,2);

  //put the Associate Count WORD
  Fbuffer.PokeShort(iAssociates, iInsertionPoint);
  inc(iInsertionPoint,2);

  //put the Object Count DWORD
  Fbuffer.PokeLong(iObjects, iInsertionPoint);
  inc(iInsertionPoint,4);

  EndPacketChange;

end;

//----------------------------------------------------------------------------
procedure TRDTPPacket.AddLongLong(int: int64);
begin
  BeginPacketChange;
  //put the data type byte
  FBuffer.PokeByte(PDT_LONG_LONG, iInsertionPoint);
  inc(iInsertionPoint);

  //put the stuff
  FBuffer.PokeLongLong(int, iInsertionPoint);
  inc(iInsertionPoint,8);

  //Recalc internal length stored in packet
  EndPacketChange;

end;
//----------------------------------------------------------------------------
procedure TRDTPPacket.AddDateTime(d: TDateTime);
begin
  BeginPacketChange;

  //put the data type BYTE
  Fbuffer.PokeByte(PDT_DATETIME, iInsertionPoint);
  inc(iInsertionPoint);

  Fbuffer.PokeDateTime(d, iInsertionPoint);
  inc(iInsertionPoint,8);

  EndPacketChange;

end;
//----------------------------------------------------------------------------
procedure TRDTPPacket.AddBoolean(b: boolean);
begin
{$IFDEF LOG_PACKET_DATA}GLOG.Debug('AddBoolean @'+inttostr(iInsertionPoint));{$ENDIF}
  BeginPacketChange;
  //put the data type byte
  FBuffer.PokeByte(PDT_BOOLEAN, iInsertionPoint);
  inc(iInsertionPoint);

  //put the stuff
  FBuffer.PokeBoolean(b, iInsertionPoint);
  inc(iInsertionPoint,PDT_LENGTH_BOOLEAN);

  //Recalc internal length stored in packet
  EndPacketChange;

end;

procedure TRDTPPacket.AddDouble(d: double);
begin
  BeginPacketChange;
  //put the data type byte
  FBuffer.PokeByte(PDT_DOUBLE, iInsertionPoint);
  inc(iInsertionPoint);

  //put the stuff
  FBuffer.PokeDouble(d, iInsertionPoint);
  inc(iInsertionPoint,PDT_LENGTH_DOUBLE);

  //Recalc internal length stored in packet
  EndPacketChange;
end;

procedure TRDTPPacket.AddFlexInt(int: int64);
var
  dn: integer;
begin
  if (int < int64(integer($80000000))) or (int > int64(integer($7FFFFFFF))) then
    AddLongLong(int)
  else begin
    dn := int;
    if (dn > 32767) or (dn < -32768) then
      AddLong(int)
    else
      AddShort(int);

  end;
end;

procedure TRDTPPacket.AddInt(i: int64);
begin
//  if (int <= 255) and (int >=0) then
//    AddByte(smallint(i));

//  if (i <= 32767) and (i >=-32768) then begin
//    AddShort(smallint(i));
//    exit;
//  end;
//
//  if (i <= $7fffffff) and (i >=(-2147483648)) then begin
//    Addlong(integer(i));
//    exit;
//  end;

   Addlonglong(i);




end;

function TRDTPPacket.GetSessionID: integer;
begin
  BeginPacketRead;
  if not IsResponse then
    raise EPacketError.createFMT('Cannot read SessionID from a packet during '+
    'construction.',[]);

  result := Data[PACKET_INDEX_SESSION];

end;

function TRDTPPacket.GetDebugMessage2: string;
var
  t: integer;
  i: integer;
begin
  i := 0;
  try
    for t:= 0 to self.DataCount-1 do begin
      i := t;
      result := result+vartostr(Data[t])+#13#10;
    end;
  except
    on E: Exception do begin
      raise Exception.create('Exception getting debug message while reading data element '+inttostr(i)+': '+E.Message);
    end;
  end;
end;

function TRDTPPacket.GetBufferAsString: string;
var
  t: integer;
begin
  SetLength(result, self.UnpackedLength);
  for t:= 1 to self.UnpackedLength do begin
    result := result + chr(DecryptedBuffer.RawBuffer[t]);
  end;
end;


function TRDTPPacket.GetBufferLength: integer;
begin

  result := self.FBuffer.FSize;
end;

function TRDTPPacket.Eof: boolean;
begin
  result := iSeqReadPos >= DataCount;
end;

function TRDTPPacket.GetDebugMessage: string;
var
  memo: Tstringlist;
  iLoopStart: integer;
  t: integer;
  v: variant;
  iDataType: integer;
  iTemp : integer;
  sDisp: string;
begin
  result := '';

  {$IFNDEF VERBOSE_PACKET_LOGGING}
  exit;
  {$ENDIF}

  memo := TStringlist.create;
  try
    try
      memo.Clear;
      memo.add('**********************************');
      memo.add(verbBool(IsValid)+' packet');


      memo.Add('  IsResponse = '+booltoStr(IsResponse));

      memo.add('----------------------------------');
      Memo.add('  MARKER: '+inttohex(Marker,8)+' '+VerbBool(IsMArkerValid));
      Memo.add('  CRC: '+inttohex(CRC,8)+' '+VerbBool(IsCRCValid));
      Memo.add('  ENCRYPTION: '+inttohex(Encryption,4));
      Memo.add('  PackedLength: '+inttostr(PackedLength));
      Memo.add('  UnpackedLength: '+inttostr(UnpackedLength));
      Memo.add('  Type: '+inttostr(PacketType));
      memo.Add('  ');
      memo.Add('  DataCount = '+inttostr(datacount));

//      if IsResponse then begin
//        memo.add('  ------------------------');
//        memo.add('  STANDARD RESPONSE FIELDS');
//        memo.add('  ------------------------');
//        memo.Add('  Data[0] ResponseType = '+inttostr(ResponseType));
//        memo.Add('  Data[1] Result = '+varbooltostr(self.Result));
//        memo.Add('  Data[2] ErrorCode = '+inttostr(ErrorCode));
//        memo.Add('  Data[3] Message = '+Message);
        iLoopStart:= 0;
//      end else iLoopStart:=0;

      memo.add('  -------------------------');
      memo.add('  ADDITIONAL FIELDS');
      memo.add('  -------------------------');
      for t:=iLoopStart to DataCount -1 do begin
        iDataType := DataType[t];
        if iDAtaType <> PDT_BYTES then
          v := Data[t];
  (*      if (varSmallint = Vartype(v)) then
          sDisp := inttostr(v);
        if (Vartype(v)= varInteger) then
          sDisp := inttostr(v);
        if (varType(v) = varString) then
          sDisp := v;
        if (varType(v) = varNull) then
          sDisp := 'NULL (Binary)';*)
        //GLog.Debug(VArToStr(v));
        case iDataType of
          PDT_SHORT: sDisp := '(short): '+varToStr(v);
          PDT_LONG: sDisp := '(long): ' +varToStr(v);
          PDT_LONG_LONG: sDisp := '(longlong): '+varToStr(v);
          PDT_DOUBLE: sDisp := '(double): '+ varToStr(v);
          PDT_DATETIME: begin
            sDisp := '(datetime): '+ varToStr(v);
          end;
          PDT_STRING: sDisp := '(string): '+ varToStr(v);
          PDT_BYTES: sDisp := 'bytes (' + inttostr(DecryptedBuffer.PeekLong(GetPointerToData(t)+1)) + ')';
          PDT_BOOLEAN: sDisp := varbooltostr(v);
          PDT_NULL: sDisp := 'NULL (special type, must be independently handled)';
          PDT_SHORT_OBJECT: begin
              iTemp := GetPointerToData(t);
              sDisp := 'short object';
              sDisp := sDisp + ' ID ' + inttohex(DecryptedBuffer.PeekShort(iTemp + 1), 4);//1+2=3
              sDisp := sDisp + ' #Params ' + inttostr(DecryptedBuffer.PeekByte(iTemp + 3));//3+1=4
              sDisp := sDisp + ' #Fields ' + inttostr(DecryptedBuffer.PeekByte(iTemp + 4));//4+1=5
              sDisp := sDisp + ' #Associates ' + inttostr(DecryptedBuffer.PeekByte(iTemp + 5));//5+1=6
              sDisp := sDisp + ' #Objects ' + inttostr(DecryptedBuffer.PeekShort(iTemp + 6));//6+2=8-1=7

              end;
          PDT_LONG_OBJECT: begin
              iTemp := GetPointerToData(t);
              sDisp := 'long object';
              sDisp := sDisp + ' ID ' + inttohex(DecryptedBuffer.PeekShort(iTemp + 1), 4);//1+2=3
              sDisp := sDisp + ' #Params ' + inttostr(DecryptedBuffer.PeekShort(iTemp + 3));//3+2=5
              sDisp := sDisp + ' #Fields ' + inttostr(DecryptedBuffer.PeekShort(iTemp + 5));//5+2=7
              sDisp := sDisp + ' #Associates ' + inttostr(DecryptedBuffer.PeekShort(iTemp + 7));//7+2=9
              sDisp := sDisp + ' #Objects ' + inttostr(DecryptedBuffer.PeekLong(iTemp + 9));//9+4=13-1=12
              end;

        else
          begin
            sDisp := sDisp + ' UNKNOWN TYPE, CANNOT CONTINUE: '+inttostr(iDATAType);
//          raise EPacketError.createFMT('Debug message for data type at byte %d is Unimplemented.  Value: %d', [t, iDataTYpe]);
            break;
          end;
        end;
        Memo.add('  Data['+inttostr(t)+'] = '+sDisp);
      end;

      //PacketDebugForm.ShowDebugMessage(memo.text);

    except
      Memo.add('  Exception reading packet');
      Memo.add(Exception(ExceptObject).Message);
      raise EPacketError.create('PACKET FORMAT ERROR: DEBUG INFO FOLLOWS'#13#10+memo.text);

    end;
  finally
    result := memo.text;
    memo.free;

  end;
end;

procedure TRDTPPacket.UpdateLongObject(iPos, iObjectType, iKeys, iFields,
  iAssociates, iObjects: integer);
begin
  //put packet into change state
  BeginPacketChange;

  //put the data type BYTE
  Fbuffer.PokeByte(PDT_LONG_OBJECT, iPos);
  //put the object type WORD
  Fbuffer.PokeShort(iObjectType, iPos+1);
  //put the key count WORD
  Fbuffer.PokeShort(iKeys, iPos+3);
  //put the Field Count WORD
  Fbuffer.PokeShort(iFields, iPos+5);
  //put the Associate Count WORD
  Fbuffer.PokeShort(iAssociates, iPos+7);
  //put the Object Count DWORD
  Fbuffer.PokeLong(iObjects, iPos+9);

  EndPacketChange;

end;

procedure TRDTPPacket.AddNull;
begin
  BeginPacketChange;
  //put the data type byte
  FBuffer.PokeByte(PDT_NULL, iInsertionPoint);
  inc(iInsertionPoint);
  //Recalc internal length stored in packet
  EndPacketChange;

end;


{ EPacketError }


initialization
(*  DefEncKey :=
    #$4f+#$5f+#$f8+#$40+#$e1+#$82+#$3e+#$9e+#$74+#$06+#$3e+#$39+#$12+
    #$1a+#$7c+#$51+#$90+#$8c+#$9c+#$ce+#$c3+#$32+#$2f+#$ca+#$f1+#$88+
    #$cd+#$4f+#$86+#$9a+#$64+#$62+#$08+#$25+#$cf+#$40+#$4d+#$f9+#$ab+
    #$e7+#$ec+#$a1+#$66+#$d8+#$f1+#$46+#$98+#$1c+#$d5+#$d1+#$97+#$85+
    #$ad+#$50+#$46+#$36+#$a1+#$9b+#$2d+#$65+#$a0+#$37+#$cf+#$60+#$52+
    #$7a+#$37+#$ce+#$4e+#$f0+#$d5+#$38+#$8e+#$fe+#$88+#$c8+#$1f+#$c5+
    #$fc+#$50+#$1d+#$49+#$36+#$8d+#$df+#$d3+#$6d+#$ae+#$6f+#$94+#$1c+
    #$10+#$3d+#$8a+#$55+#$4c+#$14+#$a8+#$58+#$73+#$4c+#$31+#$64+#$98+
    #$42+#$65+#$cd+#$8f+#$05+#$5f+#$4f+#$f5+#$52+#$3e+#$a2+#$70+#$c3+
    #$83+#$4e+#$38+#$43+#$ba+#$c1+#$d7+#$c5+#$5d+#$9f+#$2d+#$36+#$f5+
    #$59+#$b6+#$af+#$82+#$01+#$10+#$f0;*)

end.




