unit DelphiCryptlib;
//{$Message Error 'Not Supported'}

{------------------------------------------------------------------------------}
{ DelphiCryptlib.pas            Copyright © Wolfgang Gothier 2003-2005         }
{------------------------------------------------------------------------------}
{                                                                              }
{ Delphi interface objects for                                                 }
{     cryptlib Security Toolkit Version 3.11                                   }
{     Copyright © 1992-2005 Peter Gutmann                                      }
{                                                                              }
{ This source is free for commercial and non-commercial use as long as         }
{ the following conditions are accepted:                                       }
{                                                                              }
{ 1. Copyright for this module remains Wolfgang Gothier's, and as such any     }
{    Copyright notices and disclaimer in the code are not to be removed.       }
{ 2. Altered source versions must be plainly marked as such, and               }
{    must not be misrepresented as being the original software.                }
{                                                                              }
{ DISCLAIMER:                                                                  }
{ THIS SOFTWARE IS PROVIDED BY WOLFGANG GOTHIER "AS IS" AND                    }
{ ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE        }
{ IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   }
{ ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE      }
{ FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL   }
{ DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS      }
{ OR SERVICES, LOSS OF USE, DATA, OR PROFITS OR BUSINESS INTERRUPTION)         }
{ HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT   }
{ LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    }
{ OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF       }
{ SUCH DAMAGE.                                                                 }
{------------------------------------------------------------------------------}

interface

uses Classes, SysUtils, Cryptlib;

{------------------------------------------------------------------------------}
{ Cryptlib Exception types                                                     }
{------------------------------------------------------------------------------}

type
  ECryptError = class(Exception)
  public
    constructor Create(TheCode: Integer; calledFunc: string);
  private
    FErrorCode: Integer;
  public
    property ErrorCode: Integer read FErrorCode;
  end;

  ECryptParamError = class(ECryptError); { Error in parameters passed to function }
  ECryptResourceError = class(ECryptError); { Errors due to insufficient resources }
  ECryptSecurityError = class(ECryptError); { Security violations }
  ECryptHighLevelError = class(ECryptError); { High-level function errors }
  ECryptDatasetError = class(ECryptError); { Data access function errors }
  ECryptEnvelopeError = class(ECryptError); { Data enveloping errors }

  { Forward types }

  TCryptKeyset = class;
  TCryptCert = class;

{------------------------------------------------------------------------------}
{ Cryptlib basic class                                                         }
{------------------------------------------------------------------------------}

  TCryptObject = class(TObject)
  protected
    CryptHandle: CRYPT_HANDLE;
    constructor CreateFromHandle(Handle: CRYPT_HANDLE); virtual;
    function GetAttributeWithDefault(const attributeType: CRYPT_ATTRIBUTE_TYPE): Integer;
    function GetAttributeStringWithDefault(const attributeType: CRYPT_ATTRIBUTE_TYPE): string;
    function MoveGroup(moveto: Integer): Boolean;
    function MoveAttrib(moveto: Integer): Boolean;
    function MoveInstance(moveto: Integer): Boolean;
  public
    destructor Destroy; override;
    class procedure AddRandom(const randomData: Pointer;
      const randomDataLength: Integer);
    class procedure QueryCapability(const cryptAlgo: CRYPT_ALGO_TYPE;
      var QueryInfo: CRYPT_QUERY_INFO);
    class function AlgoAvailable(const cryptAlgo: CRYPT_ALGO_TYPE): Boolean;
    class procedure SetOption(const cryptOption: CRYPT_ATTRIBUTE_TYPE;
      const value: Boolean); overload;
    class procedure SetOption(const cryptOption: CRYPT_ATTRIBUTE_TYPE;
      const value: Integer); overload;
    class procedure SetOption(const cryptOption: CRYPT_ATTRIBUTE_TYPE;
      const value: string); overload;
    class function GetOption(const attributeType: CRYPT_ATTRIBUTE_TYPE): Integer;
    class function GetOptionBool(const attributeType: CRYPT_ATTRIBUTE_TYPE): Boolean;
    class function GetOptionString(const attributeType: CRYPT_ATTRIBUTE_TYPE): string;
    function GetAttribute(const attributeType: CRYPT_ATTRIBUTE_TYPE): Integer;
    function GetAttributeBool(const attributeType: CRYPT_ATTRIBUTE_TYPE): Boolean;
    function GetAttributeString(const attributeType: CRYPT_ATTRIBUTE_TYPE): string;
    procedure GetAttributeBinary(const attributeType: CRYPT_ATTRIBUTE_TYPE;
      value: Pointer; var valueLength: Integer);
    procedure SetAttribute(const attributeType: CRYPT_ATTRIBUTE_TYPE;
      const value: Integer = CRYPT_UNUSED);
    procedure SetAttributeBool(const attributeType: CRYPT_ATTRIBUTE_TYPE;
      const value: Boolean = true);
    procedure SetAttributeString(const attributeType: CRYPT_ATTRIBUTE_TYPE;
      const value: string);
    procedure SetAttributeBinary(const attributeType: CRYPT_ATTRIBUTE_TYPE;
      const value: Pointer; const valueLength: Integer);

    property Handle: CRYPT_HANDLE read CryptHandle;
    property ErrorText: string
      index CRYPT_ATTRIBUTE_INT_ERRORMESSAGE read GetAttributeString;
  end;

{------------------------------------------------------------------------------}
{ TCryptCommonEnvSession as basic class for Envelope/DeEnvelope and Sessions   }
{------------------------------------------------------------------------------}

  TCryptCommonEnvSession = class(TCryptObject)
  private
    procedure CheckResourceNeeded(errcode: Integer); virtual;

  public
    procedure PushData(buffer: Pointer; const length: Integer;
      var BytesPushed: Integer);
    procedure FlushData;
    function PopData(buffer: Pointer; const length: Integer): Integer; { returns bytesCopied }

    property Algorithm: string index CRYPT_CTXINFO_NAME_ALGO read GetAttributeStringWithDefault;
    property Mode: string index CRYPT_CTXINFO_NAME_MODE read GetAttributeStringWithDefault;
    property KeySize: Integer index CRYPT_CTXINFO_KEYSIZE read GetAttributeWithDefault;
  end;

{------------------------------------------------------------------------------}
{ TCryptEnv as basic class for Envelope/DeEnvelope                             }
{------------------------------------------------------------------------------}

  TCryptEnv = class(TCryptCommonEnvSession)
  public
    destructor Destroy; override;
    procedure StreamIO(Source, Dest: TStream); virtual;
    function stringIO(SourceString: string): string;

    property BufferSize: Integer
      index CRYPT_ATTRIBUTE_BUFFERSIZE write SetAttribute;
    property Password: string
      index CRYPT_ENVINFO_PASSWORD write SetAttributeString;
    property SessionKey: CRYPT_HANDLE
      index CRYPT_ENVINFO_SESSIONKEY write SetAttribute;
    property SigningKey: CRYPT_HANDLE
      index CRYPT_ENVINFO_SIGNATURE write SetAttribute;
  end;

{------------------------------------------------------------------------------}
{ TCryptSession                                                                }
{------------------------------------------------------------------------------}
  TCryptKey = class; { forward }

  TCryptSession = class(TCryptCommonEnvSession)
  private
    procedure SetPrivateKey(PrivateKey: TCryptKey);
    function GetVerifyingCert: TCryptCert;

  public
    constructor Create(const sessionType: CRYPT_SESSION_TYPE);
    destructor Destroy; override;
    procedure Activate;

    property ServerName: string
      index CRYPT_SESSINFO_SERVER_NAME read GetAttributeString write SetAttributeString;
    property UserName: string
      index CRYPT_SESSINFO_USERNAME read GetAttributeString write SetAttributeString;
    property Password: string
      index CRYPT_SESSINFO_PASSWORD write SetAttributeString;
    property PrivateKey: TCryptKey write SetPrivateKey;
    property isActive: Boolean
      index CRYPT_SESSINFO_ACTIVE read GetAttributeBool;
    property VerifyingCert: TCryptCert read GetVerifyingCert;
  end;

{------------------------------------------------------------------------------}
{ TCryptEnvelope                                                               }
{------------------------------------------------------------------------------}

  TCryptEnvelope = class(TCryptEnv)
  public
    constructor Create(const formatType: CRYPT_FORMAT_TYPE = CRYPT_FORMAT_CRYPTLIB);

    property Compressed: Integer
      index CRYPT_ENVINFO_COMPRESSION write SetAttribute;
    property EncryptingKey: CRYPT_HANDLE
      index CRYPT_ENVINFO_PUBLICKEY write SetAttribute;
    property ContentType: Integer
      index CRYPT_ENVINFO_CONTENTTYPE write SetAttribute;
    property DataSize: Integer
      index CRYPT_ENVINFO_DATASIZE write SetAttribute;

  end;

{------------------------------------------------------------------------------}
{ TCryptDeEnvelope                                                             }
{------------------------------------------------------------------------------}

  TCryptDeEnvelope = class(TCryptEnv)
  private
    FOnQueryResource: TNotifyEvent;

    procedure CheckResourceNeeded(errcode: Integer); override;
    function GetSigningCert: TCryptCert;
    function GetContentType: CRYPT_CONTENT_TYPE;
    property SigningCertHandle: CRYPT_HANDLE
      index CRYPT_ENVINFO_SIGNATURE read GetAttribute;

  public
    constructor Create;

    property OnQueryResource: TNotifyEvent write FOnQueryResource;
    property ResourceRequired: CRYPT_ATTRIBUTE_TYPE
      index CRYPT_ATTRIBUTE_CURRENT read GetAttribute;

    property FirstAttribute: Boolean index CRYPT_CURSOR_FIRST read MoveAttrib;
    property NextAttribute: Boolean index CRYPT_CURSOR_NEXT read MoveAttrib;
    property PrevAttribute: Boolean index CRYPT_CURSOR_PREVIOUS read MoveAttrib;
    property LastAttribute: Boolean index CRYPT_CURSOR_LAST read MoveAttrib;

    property DecryptingKey: CRYPT_HANDLE
      index CRYPT_ENVINFO_PRIVATEKEY write SetAttribute;
    property DecryptingKeyset: CRYPT_HANDLE
      index CRYPT_ENVINFO_KEYSET_DECRYPT write SetAttribute;
    property SigCheckKeyset: CRYPT_HANDLE
      index CRYPT_ENVINFO_KEYSET_SIGCHECK write SetAttribute;
    property CheckSignature: Integer
      index CRYPT_ENVINFO_SIGNATURE_RESULT read GetAttribute;
    property ContentType: CRYPT_CONTENT_TYPE read GetContentType;
    property SigningCert: TCryptCert read GetSigningCert;
  end;

{------------------------------------------------------------------------------}
{ TCryptKey                                                                    }
{------------------------------------------------------------------------------}

  TCryptKey = class(TCryptObject)
  public
    class function QueryObject(ObjectData: string): CRYPT_OBJECT_INFO;

    constructor Create(const cryptAlgo: CRYPT_ALGO_TYPE); overload;
    constructor Create(const cryptAlgo: CRYPT_ALGO_TYPE; const cryptUser: CRYPT_USER); overload;
    constructor GetPrivateKey(var Keyset: TCryptKeyset;
      const keyIDtype: CRYPT_KEYID_TYPE; const keyID: string; const password: string);
    constructor GetPublicKey(var Keyset: TCryptKeyset;
      const keyIDtype: CRYPT_KEYID_TYPE; const keyID: string);

    destructor Destroy; override;
    procedure AsyncCancel;
    function AsyncQuery: Integer;
    procedure Decrypt(buffer: Pointer; const length: Integer);
    procedure Encrypt(buffer: Pointer; const length: Integer);
    procedure GenerateKey;
    procedure GenerateKeyAsync;
    function Xport(const SessionKey: TCryptKey): string; overload;
    function Xport(const formatType: CRYPT_FORMAT_TYPE;
      const SessionKey: TCryptKey): string;  overload;
    function Sign(const hashContext: TCryptKey): string; overload;
    function Sign(const hashContext: TCryptKey; const formatType: CRYPT_FORMAT_TYPE): string; overload;
    procedure CheckSignature(const signature: string;
      const hashContext: TCryptKey);
    property Algorithm: Integer
      index CRYPT_CTXINFO_ALGO write SetAttribute;
    property Mode: Integer
      index CRYPT_CTXINFO_MODE write SetAttribute;
    property KeySize: Integer
      index CRYPT_CTXINFO_KEYSIZE read GetAttribute write SetAttribute;
    property Labeled: string
      index CRYPT_CTXINFO_LABEL write SetAttributeString;
    property Salt: string
      index CRYPT_CTXINFO_KEYING_SALT write SetAttributeString;
    property Password: string
      index CRYPT_CTXINFO_KEYING_VALUE write SetAttributeString;
    property HashValue: string
      index CRYPT_CTXINFO_HASHVALUE read GetAttributeString;
  end;

{------------------------------------------------------------------------------}
{ TCryptCert                                                                   }
{------------------------------------------------------------------------------}

  TCryptCert = class(TCryptObject)
  protected
    function GetCryptTime(index: Integer): string;
    function CertificateCursor(moveto: Integer): Boolean;
    procedure GetExtension(const oid: PAnsiChar; var criticalFlag: Integer;
      extension: Pointer; const extensionMaxLength: Integer; var extensionLength: Integer);

  public
    constructor Create(const certType: CRYPT_CERTTYPE_TYPE);
    constructor Import(const certObject: string); overload;
    constructor Import(const certObject: Pointer; const certObjectLength: Integer); overload;
    constructor GetPublicKey(var Keyset: TCryptKeyset;
      const keyIDtype: CRYPT_KEYID_TYPE; const keyID: string);
    destructor Destroy; override;

    procedure AddExtension(const oid: PAnsiChar; const criticalFlag: Integer;
      const extension: Pointer; const extensionLength: Integer);
    procedure AddPublicKey(const Key: TCryptKey);
    procedure Check; overload;
    procedure Check(const sigCheckKey: TCryptObject); overload;
    procedure DeleteExtension(const oid: PAnsiChar);
    procedure SignWith(const signContext: TCryptKey);
    function CertExport(const certFormatType: CRYPT_CERTFORMAT_TYPE
      = CRYPT_CERTFORMAT_TEXT_CERTIFICATE): string;
    procedure DeleteAttribute(const attributeType: CRYPT_ATTRIBUTE_TYPE);

    procedure SetIssuer;
    procedure SetSubject;

    property FirstCert: Boolean index CRYPT_CURSOR_FIRST read CertificateCursor;
    property NextCert: Boolean index CRYPT_CURSOR_NEXT read CertificateCursor;
    property PrevCert: Boolean index CRYPT_CURSOR_PREVIOUS read CertificateCursor;
    property LastCert: Boolean index CRYPT_CURSOR_LAST read CertificateCursor;

    property FirstExt: Boolean index CRYPT_CURSOR_FIRST read MoveGroup;
    property NextExt: Boolean index CRYPT_CURSOR_NEXT read MoveGroup;
    property PrevExt: Boolean index CRYPT_CURSOR_PREVIOUS read MoveGroup;
    property LastExt: Boolean index CRYPT_CURSOR_LAST read MoveGroup;

    property FirstAttribute: Boolean index CRYPT_CURSOR_FIRST read MoveAttrib;
    property NextAttribute: Boolean index CRYPT_CURSOR_NEXT read MoveAttrib;
    property PrevAttribute: Boolean index CRYPT_CURSOR_PREVIOUS read MoveAttrib;
    property LastAttribute: Boolean index CRYPT_CURSOR_LAST read MoveAttrib;

    property FirstComponent: Boolean index CRYPT_CURSOR_FIRST read MoveInstance;
    property NextComponent: Boolean index CRYPT_CURSOR_NEXT read MoveInstance;
    property PrevComponent: Boolean index CRYPT_CURSOR_PREVIOUS read MoveInstance;
    property LastComponent: Boolean index CRYPT_CURSOR_LAST read MoveInstance;

    procedure SetValidityInterval(Starting, Ending: TDateTime);

    property ValidFrom: string index CRYPT_CERTINFO_VALIDFROM read GetCryptTime;
    property ValidTo: string index CRYPT_CERTINFO_VALIDTO read GetCryptTime;
    property Update: string index CRYPT_CERTINFO_THISUPDATE read GetCryptTime;
    property NextUpdate: string index CRYPT_CERTINFO_NEXTUPDATE read GetCryptTime;
    property RevocationDate: string index CRYPT_CERTINFO_REVOCATIONDATE read GetCryptTime;

    property CertType: Integer
      index CRYPT_CERTINFO_CERTTYPE read GetAttribute write SetAttribute;
    property AttributeID: Integer
      index CRYPT_CERTINFO_CURRENT_FIELD read GetAttribute;
    property ExtensionID: Integer
      index CRYPT_CERTINFO_CURRENT_EXTENSION read GetAttribute;
    property KeySize: Integer
      index CRYPT_CTXINFO_KEYSIZE read GetAttribute;
    property AlgorithmName: string
      index CRYPT_CTXINFO_NAME_ALGO read GetAttributeString;
    property isSelfSigned: Boolean
      index CRYPT_CERTINFO_SELFSIGNED read GetAttributeBool write SetAttributeBool;
    property isCA: Boolean
      index CRYPT_CERTINFO_CA read GetAttributeBool write SetAttributeBool;

    property CreateFromCR: Integer
      index CRYPT_CERTINFO_CERTREQUEST write SetAttribute;
    property SimpleGenCert: Boolean
      index CRYPT_CERTINFO_XYZZY write SetAttributeBool;

    property CountryName: string
      index CRYPT_CERTINFO_COUNTRYNAME read GetAttributeString write SetAttributeString;
    property ProvinceName: string
      index CRYPT_CERTINFO_STATEORPROVINCENAME read GetAttributeString write SetAttributeString;
    property Locality: string
      index CRYPT_CERTINFO_LOCALITYNAME read GetAttributeString write SetAttributeString;
    property Organisation: string
      index CRYPT_CERTINFO_ORGANISATIONNAME
      read GetAttributeString write SetAttributeString;
    property OrganisationalUnit: string
      index CRYPT_CERTINFO_ORGANISATIONALUNITNAME
      read GetAttributeString write SetAttributeString;
    property CommonName: string
      index CRYPT_CERTINFO_COMMONNAME
      read GetAttributeString write SetAttributeString;
    property URL: string
      index CRYPT_CERTINFO_UNIFORMRESOURCEIDENTIFIER
      read GetAttributeString write SetAttributeString;
    property EMail: string
      index CRYPT_CERTINFO_RFC822NAME
      read GetAttributeString write SetAttributeString;
    property DistinguishedName: string
      index CRYPT_CERTINFO_DN
      read GetAttributeString write SetAttributeString;
    property KeyUsage: Integer
      index CRYPT_CERTINFO_KEYUSAGE read GetAttribute write SetAttribute;
    property ErrorLocation: Integer
      index CRYPT_ATTRIBUTE_ERRORLOCUS read GetAttribute;
    property ErrorType: Integer
      index CRYPT_ATTRIBUTE_ERRORTYPE read GetAttribute;
  end;

{------------------------------------------------------------------------------}
{ TCryptKeyset                                                                 }
{------------------------------------------------------------------------------}

  TCryptKeyset = class(TCryptObject)
  public
    constructor Create(const keysetType: CRYPT_KEYSET_TYPE; const name: PAnsiChar;
      const options: CRYPT_KEYOPT_TYPE);
    destructor Destroy; override;
    procedure AddPrivateKey(const cryptKey: TCryptKey;
      const password: string);
    procedure AddPublicKey(const certificate: TCryptCert);
    procedure DeleteKey(const keyIDtype: CRYPT_KEYID_TYPE; const keyID: string);
  end;

{------------------------------------------------------------------------------}
{ Begin IMPLEMENTATION ********************************************************}
{------------------------------------------------------------------------------}

implementation

uses Windows;

const
  UnixStartDate: Double = 25569; { StrToDate('1.1.1970') }
  DELPHI_ERROR_EXTINVALID = 90;

var
  UTCDiff: TDateTime = 0;

{------------------------------------------------------------------------------}
{ utility definitions and routines                                             }
{------------------------------------------------------------------------------}

resourcestring
  ErrTxt01 = 'Bad argument, parameter 1';
  ErrTxt02 = 'Bad argument, parameter 2';
  ErrTxt03 = 'Bad argument, parameter 3';
  ErrTxt04 = 'Bad argument, parameter 4';
  ErrTxt05 = 'Bad argument, parameter 5';
  ErrTxt06 = 'Bad argument, parameter 6';
  ErrTxt07 = 'Bad argument, parameter 7';

  ErrTxt08 = 'Out of memory';
  ErrTxt09 = 'Data has not been initialised';
  ErrTxt10 = 'Data has already been initialised';
  ErrTxt11 = 'Operation not available at requested security level';
  ErrTxt12 = 'No reliable random data available';
  ErrTxt13 = 'Operation failed';

  ErrTxt14 = 'This type of operation not available';
  ErrTxt15 = 'No permission to perform this operation';
  ErrTxt16 = 'Incorrect key used to decrypt data';
  ErrTxt17 = 'Operation incomplete/still in progress';
  ErrTxt18 = 'Operation complete/can''t continue';
  ErrTxt19 = 'Operation timed out before completion';
  ErrTxt20 = 'Invalid/inconsistent information';
  ErrTxt21 = 'Resource destroyed by external event';

  ErrTxt22 = 'Resources/space exhausted';
  ErrTxt23 = 'Not enough data available';
  ErrTxt24 = 'Bad/unrecognised data format';
  ErrTxt25 = 'Signature/integrity check failed';

  ErrTxt26 = 'Cannot open object';
  ErrTxt27 = 'Cannot read item from object';
  ErrTxt28 = 'Cannot write item to object';
  ErrTxt29 = 'Requested item not found in object';
  ErrTxt30 = 'Item already present in object';

  ErrTxt31 = 'Need resource to proceed';
  { private errorcodes }
  ErrTxt90 = 'Invalid format in certificate extension';

  ErrTxt99 = 'Unknown cryptlib error';
  ErrTxtDll = 'Cryptlib library "CL32.DLL" not found';

  ErrTxtStream0 = 'Size for encode/decode is 0 (Zero)';

type
  ExceptClass = class of ECryptError;

  ErrType = (ParErr, {  Error in parameters passed to function  }
    ResErr, {  Errors due to insufficient resources  }
    SecErr, {  Security violations  }
    FuncErr, {  High-level function errors  }
    DataErr, {  Data access function errors  }
    EnvErr); {  Data enveloping errors  }
const
  ErrCodeTab: array[0..32] of
  record code: Integer;
    ex: ExceptClass;
    txt: string;
  end =
  ((code: CRYPT_ERROR_PARAM1; ex: ECryptParamError; txt: ErrTxt01),
    (code: CRYPT_ERROR_PARAM2; ex: ECryptParamError; txt: ErrTxt02),
    (code: CRYPT_ERROR_PARAM3; ex: ECryptParamError; txt: ErrTxt03),
    (code: CRYPT_ERROR_PARAM4; ex: ECryptParamError; txt: ErrTxt04),
    (code: CRYPT_ERROR_PARAM5; ex: ECryptParamError; txt: ErrTxt05),
    (code: CRYPT_ERROR_PARAM6; ex: ECryptParamError; txt: ErrTxt06),
    (code: CRYPT_ERROR_PARAM7; ex: ECryptParamError; txt: ErrTxt07),

    (code: CRYPT_ERROR_MEMORY; ex: ECryptResourceError; txt: ErrTxt08),
    (code: CRYPT_ERROR_NOTINITED; ex: ECryptResourceError; txt: ErrTxt09),
    (code: CRYPT_ERROR_INITED; ex: ECryptResourceError; txt: ErrTxt10),
    (code: CRYPT_ERROR_NOSECURE; ex: ECryptResourceError; txt: ErrTxt11),
    (code: CRYPT_ERROR_RANDOM; ex: ECryptResourceError; txt: ErrTxt12),
    (code: CRYPT_ERROR_FAILED; ex: ECryptResourceError; txt: ErrTxt13),

    (code: CRYPT_ERROR_NOTAVAIL; ex: ECryptSecurityError; txt: ErrTxt14),
    (code: CRYPT_ERROR_PERMISSION; ex: ECryptSecurityError; txt: ErrTxt15),
    (code: CRYPT_ERROR_WRONGKEY; ex: ECryptSecurityError; txt: ErrTxt16),
    (code: CRYPT_ERROR_INCOMPLETE; ex: ECryptSecurityError; txt: ErrTxt17),
    (code: CRYPT_ERROR_COMPLETE; ex: ECryptSecurityError; txt: ErrTxt18),
    (code: CRYPT_ERROR_TIMEOUT; ex: ECryptSecurityError; txt: ErrTxt19),
    (code: CRYPT_ERROR_INVALID; ex: ECryptSecurityError; txt: ErrTxt20),
    (code: CRYPT_ERROR_SIGNALLED; ex: ECryptSecurityError; txt: ErrTxt21),

    (code: CRYPT_ERROR_OVERFLOW; ex: ECryptHighLevelError; txt: ErrTxt22),
    (code: CRYPT_ERROR_UNDERFLOW; ex: ECryptHighLevelError; txt: ErrTxt23),
    (code: CRYPT_ERROR_BADDATA; ex: ECryptHighLevelError; txt: ErrTxt24),
    (code: CRYPT_ERROR_SIGNATURE; ex: ECryptHighLevelError; txt: ErrTxt25),

    (code: CRYPT_ERROR_OPEN; ex: ECryptDatasetError; txt: ErrTxt26),
    (code: CRYPT_ERROR_READ; ex: ECryptDatasetError; txt: ErrTxt27),
    (code: CRYPT_ERROR_WRITE; ex: ECryptDatasetError; txt: ErrTxt28),
    (code: CRYPT_ERROR_NOTFOUND; ex: ECryptDatasetError; txt: ErrTxt29),
    (code: CRYPT_ERROR_DUPLICATE; ex: ECryptDatasetError; txt: ErrTxt30),

    (code: CRYPT_ENVELOPE_RESOURCE; ex: ECryptEnvelopeError; txt: ErrTxt31),

    { private errorcodes: }
    (code: DELPHI_ERROR_EXTINVALID; ex: ECryptHighLevelError; txt: ErrTxt90),

    (code: 0; ex: ECryptError; txt: ErrTxt99));

function FindCode(TheCode: Integer): Integer;
begin
  result := low(ErrCodeTab);
  while result < high(ErrCodeTab) do
  begin
    if ErrCodeTab[result].code = TheCode then
      break;
    Inc(result);
  end;
end;

constructor ECryptError.Create(TheCode: Integer; calledFunc: string);
begin
  inherited Create(calledFunc + ': ' + ErrCodeTab[FindCode(TheCode)].txt);
  FErrorCode := TheCode;
end;

{***************************************************************************
*                                                                          *
*                                Utility Routines                          *
*                                                                          *
***************************************************************************}

{ Time conversion routines }

function GetUTCDifference: TDateTime;
var
  TimeZoneInformation: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TimeZoneInformation);
  result := (TimeZoneInformation.Bias + TimeZoneInformation.DaylightBias) / (24 * 60);
end;

function UnixDateToStr(UDate: LongInt): string;
begin
  raise Exception.Create('unimplemented due to missing functions');
//  DateTimeToString(result, ShortDateFormat + ' hh:nn:ss',
//    UnixStartDate + UDate / SecsPerDay - UTCDiff);
end;

function StrToUnixDate(Date: string): Longint;
begin
  result := Round((StrToDateTime(Date) - UnixStartDate + UTCDiff) * SecsPerDay);
end;

{------------------------------------------------------------------------------}
{ TCryptObject methods }
{------------------------------------------------------------------------------}

constructor TCryptObject.CreateFromHandle(Handle: CRYPT_HANDLE);
begin { to be used only internally !!! }
  inherited;
  CryptHandle := Handle;
end;

destructor TCryptObject.Destroy;
var
  err: Integer;
begin
  if CryptHandle > 0 then
  begin
    err := cryptDestroyObject(CryptHandle);
    if err < 0 then
      raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptDestroyObject');
  end;
end;

class procedure TCryptObject.QueryCapability(const cryptAlgo: CRYPT_ALGO_TYPE; var QueryInfo: CRYPT_QUERY_INFO);
var
  err: Integer;
begin
  err := cryptQueryCapability(cryptAlgo, QueryInfo);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptQueryCapability');
end;

class function TCryptObject.AlgoAvailable(const cryptAlgo: CRYPT_ALGO_TYPE): Boolean;
var
  dummy: CRYPT_QUERY_INFO;
begin
  result := cryptQueryCapability(cryptAlgo, dummy) >= 0;
end;

{ Oddball functions: Add random data to the pool }

class procedure TCryptObject.AddRandom(const randomData: Pointer; const randomDataLength: Integer);
var
  err: Integer;
begin
  err := cryptAddRandom(randomData, randomDataLength);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptAddRandom');
end;

{ Get/set/delete attribute functions }

procedure TCryptObject.SetAttribute(const attributeType: CRYPT_ATTRIBUTE_TYPE; const value: Integer = CRYPT_UNUSED);
var
  err: Integer;
begin
  err := cryptSetAttribute(CryptHandle, attributeType, value);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSetAttribute');
end;

procedure TCryptObject.SetAttributeBool(const attributeType: CRYPT_ATTRIBUTE_TYPE; const value: Boolean);
var
  err: Integer;
begin
  if value then
    err := cryptSetAttribute(CryptHandle, attributeType, 1)
  else
    err := cryptSetAttribute(CryptHandle, attributeType, 0);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSetAttribute');
end;

procedure TCryptObject.SetAttributeString(const attributeType: CRYPT_ATTRIBUTE_TYPE;
  const value: string);
var
  err: Integer;
begin
  err := cryptSetAttributeString(CryptHandle, attributeType, PAnsiChar(value), Length(value));
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSetAttributeString');
end;

procedure TCryptObject.SetAttributeBinary(const attributeType: CRYPT_ATTRIBUTE_TYPE;
  const value: Pointer; const valueLength: Integer);
var
  err: Integer;
begin
  err := cryptSetAttributeString(CryptHandle, attributeType, value, valueLength);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSetAttributeString');
end;

function TCryptObject.GetAttribute(const attributeType: CRYPT_ATTRIBUTE_TYPE): Integer;
var
  err: Integer;
begin
  err := cryptGetAttribute(CryptHandle, attributeType, result);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetAttribute');
end;

function TCryptObject.GetAttributeWithDefault(const attributeType: CRYPT_ATTRIBUTE_TYPE): Integer;
begin
  if cryptGetAttribute(CryptHandle, attributeType, result) < 0 then
    result := 0; { default value is zero }
end;

function TCryptObject.MoveGroup(moveto: Integer): Boolean;
begin
  result := cryptSetAttribute(Handle, CRYPT_ATTRIBUTE_CURRENT_GROUP, moveto) = CRYPT_OK;
end;

function TCryptObject.MoveAttrib(moveto: Integer): Boolean;
begin
  result := cryptSetAttribute(Handle, CRYPT_ATTRIBUTE_CURRENT, moveto) = CRYPT_OK;
end;

function TCryptObject.MoveInstance(moveto: Integer): Boolean;
begin
  result := cryptSetAttribute(Handle, CRYPT_ATTRIBUTE_CURRENT_INSTANCE, moveto) = CRYPT_OK;
end;

function TCryptObject.GetAttributeBool(const attributeType: CRYPT_ATTRIBUTE_TYPE): Boolean;
var
  err: Integer;
  ret: Integer;
begin
  err := cryptGetAttribute(CryptHandle, attributeType, ret);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetAttribute');
  result := ret <> 0;
end;

function TCryptObject.GetAttributeString(const attributeType: CRYPT_ATTRIBUTE_TYPE): string;
var
  err: Integer;
  Len: Integer;
begin
  err := cryptGetAttributeString(CryptHandle, attributeType, nil, Len);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetAttributeString');
  SetString(result, nil, Len);
  err := cryptGetAttributeString(CryptHandle, attributeType, PAnsiChar(result), Len);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetAttributeString');
end;

function TCryptObject.GetAttributeStringWithDefault(const attributeType: CRYPT_ATTRIBUTE_TYPE): string;
var
  Len: Integer;
begin
  if cryptGetAttributeString(CryptHandle, attributeType, nil, Len) < 0 then
    result := EmptyStr
  else
    result := GetAttributeString(attributeType);
end;

procedure TCryptObject.GetAttributeBinary(const attributeType: CRYPT_ATTRIBUTE_TYPE;
  value: Pointer; var valueLength: Integer);
var
  err: Integer;
begin
  err := cryptGetAttributeString(CryptHandle, attributeType, value, valueLength);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetAttributeString');
end;

class procedure TCryptObject.SetOption(const cryptOption: CRYPT_ATTRIBUTE_TYPE;
  const value: Boolean);
var
  err: Integer;
begin
  if value then
    err := cryptSetAttribute(CRYPT_UNUSED, cryptOption, 1)
  else
    err := cryptSetAttribute(CRYPT_UNUSED, cryptOption, 0);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSetAttribute');
end;

class procedure TCryptObject.SetOption(const cryptOption: CRYPT_ATTRIBUTE_TYPE;
  const value: Integer);
var
  err: Integer;
begin
  err := cryptSetAttribute(CRYPT_UNUSED, cryptOption, value);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSetAttribute');
end;

class procedure TCryptObject.SetOption(const cryptOption: CRYPT_ATTRIBUTE_TYPE;
  const value: string);
var
  err: Integer;
begin
  err := cryptSetAttributeString(CRYPT_UNUSED, cryptOption, PAnsiChar(value), Length(value));
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSetAttributeString');
end;

class function TCryptObject.GetOptionString(const attributeType: CRYPT_ATTRIBUTE_TYPE): string;
var
  err: Integer;
  Len: Integer;
begin
  err := cryptGetAttributeString(CRYPT_UNUSED, attributeType, nil, Len);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetAttributeString');
  SetString(result, nil, Len);
  err := cryptGetAttributeString(CRYPT_UNUSED, attributeType, PAnsiChar(result), Len);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetAttributeString');
end;

class function TCryptObject.GetOptionBool(const attributeType: CRYPT_ATTRIBUTE_TYPE): Boolean;
var
  err: Integer;
  ret: Integer;
begin
  err := cryptGetAttribute(CRYPT_UNUSED, attributeType, ret);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetAttribute');
  result := ret <> 0;
end;

class function TCryptObject.GetOption(const attributeType: CRYPT_ATTRIBUTE_TYPE): Integer;
var
  err: Integer;
begin
  err := cryptGetAttribute(CRYPT_UNUSED, attributeType, result);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetAttribute');
end;

{------------------------------------------------------------------------------}
{ Envelope Functions                                                           }
{------------------------------------------------------------------------------}

{ Create/destroy an envelope }

constructor TCryptEnvelope.Create(const formatType: CRYPT_FORMAT_TYPE = CRYPT_FORMAT_CRYPTLIB);
var
  err: Integer;
begin
  err := cryptCreateEnvelope(CryptHandle, CRYPT_UNUSED, formatType);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCreateEnvelope');
end;

constructor TCryptDeEnvelope.Create;
var
  err: Integer;
begin
  err := cryptCreateEnvelope(CryptHandle, CRYPT_UNUSED, CRYPT_FORMAT_AUTO);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCreateEnvelope');
end;

function TCryptDeEnvelope.GetSigningCert: TCryptCert;
var
  CertHandle: CRYPT_HANDLE;
  CertType: Integer;
begin
  try
    CertHandle := SigningCertHandle;
    cryptGetAttribute(CertHandle, CRYPT_CERTINFO_CERTTYPE, CertType);
    result := TCryptCert.Create(CRYPT_CERTTYPE_TYPE(CertType));
    cryptDestroyCert(result.Handle);
    result.CryptHandle := CertHandle;
  except
    result := nil;
  end;
end;

procedure TCryptDeEnvelope.CheckResourceNeeded(errcode: Integer);
begin
  if (errcode < 0) then
  begin
    if (errcode = CRYPT_ENVELOPE_RESOURCE) and Assigned(FOnQueryResource) then
    begin
      { loop through all possible resources }
      if FirstAttribute then
        repeat
          FOnQueryResource(Self) { signal "resource needed" }
        until not NextAttribute
    end
    else if (errcode <> CRYPT_ERROR_OVERFLOW) then
      raise ErrCodeTab[FindCode(errcode)].ex.Create(errcode, 'cryptPushData');
  end;
end;

{------------------------------------------------------------------------------}
{ TCryptCommonEnvSession  ( common code for TEnvelope/TDeEnvelope/TSession)    }
{------------------------------------------------------------------------------}

procedure TCryptCommonEnvSession.PushData(buffer: Pointer; const length: Integer;
  var BytesPushed: Integer);
var
  err: Integer;
begin
  BytesPushed := 0;
  err := cryptPushData(CryptHandle, buffer, length, BytesPushed);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptPushData');
end;

procedure TCryptCommonEnvSession.FlushData;
var
  err: Integer;
begin
  err := cryptFlushData(CryptHandle);
  if (err < 0) and (err <> CRYPT_ERROR_OVERFLOW) then { overflow is NOT an error }
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptPushData');
end;

function TCryptCommonEnvSession.PopData(buffer: Pointer; const length: Integer): Integer;
var
  err: Integer;
begin
  result := 0;
  err := cryptPopData(CryptHandle, buffer, length, result);
  if (err < 0) and (err <> CRYPT_ERROR_OVERFLOW) then { overflow is NOT an error }
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptPopData');
end;

procedure TCryptCommonEnvSession.CheckResourceNeeded(errcode: Integer);
begin
  if (errcode <> 0) and (errcode <> CRYPT_ERROR_OVERFLOW) then
    raise ErrCodeTab[FindCode(errcode)].ex.Create(errcode, 'cryptPushData');
end;

{------------------------------------------------------------------------------}
{ TCryptEnv  ( common code for Envelope/DeEnvelope )                           }
{------------------------------------------------------------------------------}

destructor TCryptEnv.Destroy;
var
  err: Integer;
begin
  if CryptHandle > 0 then
  begin
    err := cryptDestroyEnvelope(CryptHandle);
    if err < 0 then
      raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptDestroyEnvelope');
  end;
end;

procedure TCryptEnv.StreamIO(Source, Dest: TStream);
const
  BufferMaxSize = 32768;
  BufferMax = BufferMaxSize - 4096;
var
  FSize: Longint;
  BufferLen: Word;
  InBuffer,
    Outbuffer: PAnsiChar;
  ActBuffer: PAnsiChar;

  ActLen: Longint;
  WriteLen: Word;
  BytesToPush,
    BytesPushed: Longint;
  errcode: Integer;

begin
  FSize := Source.Size - Source.Position;
  if FSize <= 0 then
    raise EStreamError.CreateRes(@ErrTxtStream0);
  if FSize >= BufferMax then
    BufferLen := BufferMax
  else
    BufferLen := FSize;
  InBuffer := nil;
  OutBuffer := nil;
  try
    GetMem(InBuffer, BufferLen);
    GetMem(OutBuffer, BufferMaxSize);
    repeat
      ActLen := Source.Read(InBuffer^, BufferLen);
      BytesToPush := ActLen;
      ActBuffer := InBuffer;
      if BytesToPush > 0 then
        repeat
          BytesPushed := 0;
          errcode := cryptPushData(CryptHandle, ActBuffer, BytesToPush, BytesPushed);
          CheckResourceNeeded(errcode);
          if BytesPushed <> BytesToPush then
            repeat
              WriteLen := PopData(OutBuffer, BufferMaxSize);
              if WriteLen > 0 then
                Dest.WriteBuffer(OutBuffer^, WriteLen);
            until WriteLen < BufferMaxSize;
          BytesToPush := BytesToPush - BytesPushed;
          ActBuffer := ActBuffer + BytesPushed;
        until BytesToPush = 0;
    until ActLen = 0;
    repeat
      errcode := cryptFlushData(CryptHandle);
      CheckResourceNeeded(errcode);
      WriteLen := PopData(OutBuffer, BufferMaxSize);
      if WriteLen > 0 then
        Dest.WriteBuffer(OutBuffer^, WriteLen);
    until WriteLen = 0;
  finally
    if InBuffer <> nil then
      Freemem(InBuffer);
    if OutBuffer <> nil then
      Freemem(OutBuffer);
  end;
end;

function TCryptEnv.StringIO(SourceString: string): string;
const
  BufferMaxSize = 32768;
  BufferMax = BufferMaxSize - 4096;
var
  FSize: Longint;
  Outbuffer: PAnsiChar;
  ActBuffer: PAnsiChar;
  Dest: TMemoryStream;

  ActLen: Longint;
  WriteLen: Word;
  BytesToPush,
    BytesPushed: Longint;
  errcode: Integer;

begin
  FSize := Length(SourceString);
  if FSize <= 0 then
    raise EStreamError.CreateRes(@ErrTxtStream0);
  ActBuffer := PAnsiChar(SourceString);
  ActLen := FSize;
  OutBuffer := nil;
  try
    GetMem(OutBuffer, BufferMaxSize);
    Dest := TMemoryStream.Create;
    repeat
      BytesToPush := ActLen;
      if BytesToPush > 0 then
        repeat
          BytesPushed := 0;
          errcode := cryptPushData(CryptHandle, ActBuffer, BytesToPush, BytesPushed);
          CheckResourceNeeded(errcode);
          if BytesPushed <> BytesToPush then
          begin
            WriteLen := PopData(OutBuffer, BufferMaxSize);
            if WriteLen > 0 then
              Dest.WriteBuffer(OutBuffer^, WriteLen);
          end;
          BytesToPush := BytesToPush - BytesPushed;
          ActLen := ActLen - BytesPushed;
          ActBuffer := ActBuffer + BytesPushed;
        until BytesToPush = 0;
    until ActLen = 0;
    repeat
      errcode := cryptFlushData(CryptHandle);
      CheckResourceNeeded(errcode);
      WriteLen := PopData(OutBuffer, BufferMaxSize);
      if WriteLen > 0 then
        Dest.WriteBuffer(OutBuffer^, WriteLen);
    until WriteLen = 0;
    SetString(result, PAnsiChar(Dest.Memory), Dest.Size);
  finally
    if OutBuffer <> nil then
      Freemem(OutBuffer);
  end;
end;

function TCryptDeEnvelope.GetContentType: CRYPT_CONTENT_TYPE;
begin
  result := CRYPT_CONTENT_TYPE(GetAttributeWithDefault(CRYPT_ENVINFO_CONTENTTYPE));
end;

{------------------------------------------------------------------------------}
{ TCryptSession     Session Functions                                          }
{------------------------------------------------------------------------------}

{ Create and destroy a session }

constructor TCryptSession.Create(const sessionType: CRYPT_SESSION_TYPE);
var
  err: Integer;
begin
  err := cryptCreateSession(CryptHandle, CRYPT_UNUSED, sessionType);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCreateSession');
end;

destructor TCryptSession.Destroy;
var
  err: Integer;
begin
  if CryptHandle > 0 then
  begin
    err := cryptDestroySession(CryptHandle);
    if err < 0 then
      raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptDestroySession');
  end;
end;

procedure TCryptSession.SetPrivateKey(PrivateKey: TCryptKey);
begin
  SetAttribute(CRYPT_SESSINFO_PRIVATEKEY, PrivateKey.Handle);
end;

procedure TCryptSession.Activate;
begin
  SetAttribute(CRYPT_SESSINFO_ACTIVE, 1);
end;

function TCryptSession.GetVerifyingCert: TCryptCert;
var
  VerifyingCert: CRYPT_HANDLE;
begin
  VerifyingCert := GetAttribute(CRYPT_SESSINFO_RESPONSE);
  result := TCryptCert.CreateFromHandle(VerifyingCert);
end;

{------------------------------------------------------------------------------}
{ TCryptKey                                                                    }
{------------------------------------------------------------------------------}

{ Create and destroy an encryption context }

constructor TCryptKey.Create(const cryptAlgo: CRYPT_ALGO_TYPE);
var
  err: Integer;
begin
  err := cryptCreateContext(CryptHandle, CRYPT_UNUSED, cryptAlgo);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCreateContext');
end;

constructor TCryptKey.Create(const cryptAlgo: CRYPT_ALGO_TYPE; const cryptUser: CRYPT_USER);
var
  err: Integer;
begin
  err := cryptCreateContext(CryptHandle, cryptUser, cryptAlgo);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCreateContext');
end;

{ Create a key from a keyset }

constructor TCryptKey.GetPublicKey(var Keyset: TCryptKeyset;
  const keyIDtype: CRYPT_KEYID_TYPE; const keyID: string);
var
  err: Integer;
begin
  err := cryptGetPublicKey(Keyset.CryptHandle, Self.CryptHandle, keyIDtype, PAnsiChar(keyID));
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetPublicKey')
end;

constructor TCryptKey.GetPrivateKey(var Keyset: TCryptKeyset;
  const keyIDtype: CRYPT_KEYID_TYPE; const keyID: string; const password: string);
var
  err: Integer;
begin
  if Length(password) = 0 then 
    err := cryptGetPrivateKey(Keyset.CryptHandle, Self.CryptHandle, keyIDtype, PAnsiChar(keyID), nil)
  else
    err := cryptGetPrivateKey(Keyset.CryptHandle, Self.CryptHandle, keyIDtype, PAnsiChar(keyID), PAnsiChar(password));
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetPrivateKey')
end;

destructor TCryptKey.Destroy;
var
  err: Integer;
begin
  if CryptHandle > 0 then
  begin
    err := cryptDestroyContext(CryptHandle);
    if err < 0 then
      raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptDestroyContext');
  end;
end;

{ Generate a key into a context }

procedure TCryptKey.GenerateKey;
var
  err: Integer;
begin
  err := cryptGenerateKey(CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGenerateKey');
end;

procedure TCryptKey.GenerateKeyAsync;
var
  err: Integer;
begin
  err := cryptGenerateKeyAsync(CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGenerateKeyAsync');
end;

function TCryptKey.AsyncQuery: Integer;
begin
  result := cryptAsyncQuery(CryptHandle);
end;

procedure TCryptKey.AsyncCancel;
var
  err: Integer;
begin
  err := cryptAsyncCancel(CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptAsyncCancel');
end;

{ Encrypt/decrypt/hash a block of memory }

procedure TCryptKey.Encrypt(buffer: Pointer; const length: Integer);
var
  err: Integer;
begin
  err := cryptEncrypt(CryptHandle, buffer, length);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptEncrypt');
end;

procedure TCryptKey.Decrypt(buffer: Pointer; const length: Integer);
var
  err: Integer;
begin
  err := cryptDecrypt(CryptHandle, buffer, length);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptDecrypt');
end;

{ Mid-level Encryption Functions }
{ Export and import an encrypted session key }

function TCryptKey.Xport(const SessionKey: TCryptKey): string;
var
  err: Integer;
  exportedKeyLength: Integer;
begin
  err := cryptExportKey(nil, 0, exportedKeyLength, Self.CryptHandle, SessionKey.CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptExportKey');
  SetString(result, nil, exportedKeyLength);
  err := cryptExportKey(PAnsiChar(result), exportedKeyLength, exportedKeyLength, Self.CryptHandle, SessionKey.CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptExportKey');
end;

function TCryptKey.Xport(const formatType: CRYPT_FORMAT_TYPE;
  const SessionKey: TCryptKey): string;
var
  err: Integer;
  exportedKeyLength: Integer;
begin
  err := cryptExportKeyEx(nil, 0, exportedKeyLength, formatType,
    Self.CryptHandle, SessionKey.CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptExportKeyEx');
  SetString(result, nil, exportedKeyLength);
  err := cryptExportKeyEx(PAnsiChar(result), exportedKeyLength, exportedKeyLength, formatType,
    Self.CryptHandle, SessionKey.CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptExportKeyEx');
end;

{ Create and check a digital signature }

function TCryptKey.Sign(const hashContext: TCryptKey): string;
var
  err: Integer;
  signatureLength: Integer;
  actualLength: Integer;
begin
  err := cryptCreateSignature(nil, 0, signatureLength,
    Self.CryptHandle, hashContext.CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCreateSignature');
  SetString(result, nil, signatureLength);
  err := cryptCreateSignature(PAnsiChar(result), signatureLength, actualLength,
    Self.CryptHandle, hashContext.CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCreateSignature');
  if signatureLength > actualLength then
    SetString(result, PAnsiChar(result), actualLength);
end;

function TCryptKey.Sign(const hashContext: TCryptKey; const formatType: CRYPT_FORMAT_TYPE): string;
var
  err: Integer;
  extras: CRYPT_CERTIFICATE;
  signatureLength: Integer;
  actualLength: Integer;
begin
  if (formatType = CRYPT_FORMAT_CRYPTLIB) or (formatType = CRYPT_FORMAT_PGP) then
    extras := CRYPT_UNUSED
  else
    extras := CRYPT_USE_DEFAULT;
  err := cryptCreateSignatureEx(nil, 0, signatureLength,
    formatType, Self.CryptHandle, hashContext.CryptHandle, extras);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCreateSignatureEx');
  SetString(result, nil, signatureLength);
  err := cryptCreateSignatureEx(PAnsiChar(result), signatureLength, actualLength,
    formatType, Self.CryptHandle, hashContext.CryptHandle, extras);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCreateSignatureEx');
  if signatureLength > actualLength then
    SetString(result, PAnsiChar(result), actualLength);
end;

procedure TCryptKey.CheckSignature(const signature: string;
  const hashContext: TCryptKey);
var
  err: Integer;
begin
  err := cryptCheckSignature(PAnsiChar(signature), Length(signature), Self.CryptHandle, hashContext.CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCheckSignature');
end;

{ Query an encoded signature or key data }

class function TCryptKey.QueryObject(ObjectData: string): CRYPT_OBJECT_INFO;
var
  err: Integer;
begin
  err := cryptQueryObject(PAnsiChar(ObjectData), Length(ObjectData), result);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptQueryObject');
end;

{   Keyset Functions }
{ Open and close a keyset }

constructor TCryptKeyset.Create(const keysetType: CRYPT_KEYSET_TYPE; const name: PAnsiChar; const options:
  CRYPT_KEYOPT_TYPE);
var
  err: Integer;
begin
  CryptHandle := 0;
  err := cryptKeysetOpen(CryptHandle, CRYPT_UNUSED, keysetType, name, options);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptKeysetOpen');
end;

destructor TCryptKeyset.Destroy;
var
  err: Integer;
begin
  if CryptHandle > 0 then
  begin
    err := cryptKeysetClose(CryptHandle);
    if err < 0 then
      raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptKeysetClose');
  end;
end;

{ Add/delete a key to/from a keyset }

procedure TCryptKeyset.AddPublicKey(const certificate: TCryptCert);
var
  err: Integer;
begin
  err := cryptAddPublicKey(Self.CryptHandle, certificate.CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptAddPublicKey');
end;

{ Params: const keyset: CRYPT_KEYSET;
    const cryptKey: CRYPT_HANDLE;
    const password: Pointer }

procedure TCryptKeyset.AddPrivateKey(const cryptKey: TCryptKey; const password: string);
var
  err: Integer;
begin
  err := cryptAddPrivateKey(CryptHandle, cryptKey.CryptHandle, PAnsiChar(password));
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptAddPrivateKey');
end;

{ Params: const keyset: CRYPT_KEYSET;
    const keyIDtype: CRYPT_KEYID_TYPE;
    const keyID: Pointer }

procedure TCryptKeyset.DeleteKey(const keyIDtype: CRYPT_KEYID_TYPE; const keyID: string);
var
  err: Integer;
begin
  err := cryptDeleteKey(CryptHandle, keyIDtype, PAnsiChar(keyID));
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptDeleteKey');
end;

{ Certificate Functions }
{ Create/destroy a certificate }

constructor TCryptCert.Create(const certType: CRYPT_CERTTYPE_TYPE);
var
  err: Integer;
begin
  err := cryptCreateCert(CryptHandle, CRYPT_UNUSED, certType);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCreateCert');
end;

constructor TCryptCert.GetPublicKey(var Keyset: TCryptKeyset;
  const keyIDtype: CRYPT_KEYID_TYPE; const keyID: string);
var
  err: Integer;
begin
  err := cryptGetPublicKey(Keyset.CryptHandle, Self.CryptHandle, keyIDtype, PAnsiChar(keyID));
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetPublicKey')
end;

destructor TCryptCert.Destroy;
var
  err: Integer;
begin
  if CryptHandle > 0 then
  begin
    err := cryptDestroyCert(CryptHandle);
    if err < 0 then
      raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptDestroyCert');
  end;
end;

function TCryptCert.GetCryptTime(index: Integer): string;
var
  DateValue: Longint;
  len: Integer;
begin
  case index of
    CRYPT_CERTINFO_VALIDFROM,
      CRYPT_CERTINFO_VALIDTO,
      CRYPT_CERTINFO_THISUPDATE,
      CRYPT_CERTINFO_NEXTUPDATE,
      CRYPT_CERTINFO_REVOCATIONDATE,
      CRYPT_CERTINFO_PRIVATEKEY_NOTBEFORE,
      CRYPT_CERTINFO_PRIVATEKEY_NOTAFTER:
      GetAttributeBinary(index, @DateValue, len);
  else
    raise ErrCodeTab[FindCode(DELPHI_ERROR_EXTINVALID)].ex.
      Create(DELPHI_ERROR_EXTINVALID, 'GetCryptTime')
  end;
  if len <> SizeOf(DateValue) then
    raise ErrCodeTab[FindCode(DELPHI_ERROR_EXTINVALID)].ex.
      Create(DELPHI_ERROR_EXTINVALID, 'GetCryptTime');
  result := UnixDateToStr(DateValue);
end;

procedure TCryptCert.SetValidityInterval(Starting, Ending: TDateTime);
var
  DateValue: Longint;
begin
  DateValue := Round((Ending - UnixStartDate + UTCDiff) * SecsPerDay);
  SetAttributeBinary(CRYPT_CERTINFO_VALIDTO, @DateValue, sizeof(DateValue));
  DateValue := Round((Starting - UnixStartDate + UTCDiff) * SecsPerDay);
  SetAttributeBinary(CRYPT_CERTINFO_VALIDFROM, @DateValue, sizeof(DateValue));
end;

procedure TCryptCert.DeleteAttribute(const attributeType: CRYPT_ATTRIBUTE_TYPE);
var
  err: Integer;
begin
  err := cryptDeleteAttribute(CryptHandle, attributeType);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptDeleteAttribute');
end;

procedure TCryptCert.AddPublicKey(const Key: TCryptKey);
var
  err: Integer;
begin
  err := cryptSetAttribute(CryptHandle, CRYPT_CERTINFO_SUBJECTPUBLICKEYINFO, Key.Handle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSetAttribute');
end;

procedure TCryptCert.SetIssuer;
var
  err: Integer;
begin
  err := cryptSetAttribute(CryptHandle, CRYPT_CERTINFO_ISSUERNAME, CRYPT_UNUSED);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSetAttribute');
end;

procedure TCryptCert.SetSubject;
var
  err: Integer;
begin
  err := cryptSetAttribute(CryptHandle, CRYPT_CERTINFO_SUBJECTNAME, CRYPT_UNUSED);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSetAttribute');
end;

{ Get/add/delete certificate extensions }

procedure TCryptCert.GetExtension(const oid: PAnsiChar; var criticalFlag: Integer;
  extension: Pointer; const extensionMaxLength: Integer; var extensionLength: Integer);
var
  err: Integer;
begin
  err := cryptGetCertExtension(CryptHandle, oid, criticalFlag, extension,
    extensionMaxLength, extensionLength);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptGetCertExtension');
end;

procedure TCryptCert.AddExtension(const oid: PAnsiChar; const criticalFlag: Integer;
  const extension: Pointer; const extensionLength: Integer);
var
  err: Integer;
begin
  err := cryptAddCertExtension(CryptHandle, oid, criticalFlag, extension, extensionLength);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptAddCertExtension');
end;

procedure TCryptCert.DeleteExtension(const oid: PAnsiChar);
var
  err: Integer;
begin
  err := cryptDeleteCertExtension(CryptHandle, oid);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptDeleteCertExtension');
end;

procedure TCryptCert.SignWith(const signContext: TCryptKey);
var
  err: Integer;
begin
  err := cryptSignCert(Self.CryptHandle, signContext.CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptSignCert');
end;

procedure TCryptCert.Check(const sigCheckKey: TCryptObject);
var
  err: Integer;
begin
  err := cryptCheckCert(Self.CryptHandle, sigCheckKey.CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCheckCert');
end;

procedure TCryptCert.Check;
var
  err: Integer;
begin
  err := cryptCheckCert(Self.CryptHandle, CRYPT_UNUSED);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptCheckCert');
end;

{ Import/export a certificate/certification request }

constructor TCryptCert.Import(const certObject: string);
var
  err: Integer;
begin
  err := cryptImportCert(PAnsiChar(certObject), Length(certObject), CRYPT_UNUSED, CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptImportCert');
end;

constructor TCryptCert.Import(const certObject: Pointer; const certObjectLength: Integer);
var
  err: Integer;
begin
  err := cryptImportCert(certObject, certObjectLength, CRYPT_UNUSED, CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptImportCert');
end;

function TCryptCert.CertExport(const certFormatType: CRYPT_CERTFORMAT_TYPE): string;
var
  err: Integer;
  certObjectLength: Integer;
  actObjectLength: Integer;
begin
  err := cryptExportCert(nil, 0, certObjectLength, certFormatType, CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptExportCert');
  SetString(result, nil, certObjectLength);
  err := cryptExportCert(PAnsiChar(result), certObjectLength, actObjectLength, certFormatType, CryptHandle);
  if err < 0 then
    raise ErrCodeTab[FindCode(err)].ex.Create(err, 'cryptExportCert');
  if certObjectLength > actObjectLength then
    SetString(result, PAnsiChar(result), actObjectLength);
end;

function TCryptCert.CertificateCursor(moveto: Integer): Boolean;
begin
  result := cryptSetAttribute(Handle, CRYPT_CERTINFO_CURRENT_CERTIFICATE, moveto) = CRYPT_OK;
end;

{------------------------------------------------------------------------------}

initialization

  UTCDiff := GetUTCDifference;

  CryptInit;

finalization
  CryptEnd;

end.

