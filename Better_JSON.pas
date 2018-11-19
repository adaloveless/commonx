{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Better_JSON;

/// <summary>
/// System.JSON implements a TJson class that offers several convenience methods:
/// - converting Objects to Json and vice versa
/// - formating Json  </summary>

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Rtti, System.TypInfo, System.Generics.Collections,
  System.Types;

type
  TInt15 = 0..15;
  TJSONValue = class;
  TJSONString = class;
  EJSONPathException = class(Exception);

  /// <summary> Parses a JSON path with names and indexes.</summary>
  /// <remarks>
  ///  The syntax to write paths is similar to XPath but in a Json way.
  ///  The following XPath expression:
  ///    /entities/urls[0]/indices[1]
  ///  would look like
  ///    entities.urls[0].indices[1]   (dot notation)
  ///  or 
  ///    entities["urls"][0]["indices"][1]   (bracket notation)
  ///
  ///  The dot (.) token is used to access the object elements:
  ///    ex: object.key
  ///
  ///  The bracket ([]) token is used to access array or object elements:
  ///    In array: cities[0]
  ///    In object: city["name"] or city['name']
  ///    In object: ["city"]["name"] or ['city']['name']
  ///
  ///  The quote (" or ') is used to introduce a literal when the element is being written in bracket notation:
  ///    ex:["first"]["second"] or ['first']['second']
  ///
  ///  To escape the quote in a literal use backslash (\): \"
  ///    ex: ["quotes(\").should.be.escaped"] or ['quotes(\').should.be.escaped']
  ///
  ///  Note: The backslash will only escape quotes within a literal. Bracket notation can be useful when 
  ///  names can not be written in dot notation, like the objects keys with dot characters:
  ///    ex: object["key.with.dots"] or object['key.with.dots']
  ///
  /// </remarks>
  TJSONPathParser = class
  public type
    TToken = (Undefined, Name, ArrayIndex, Eof, Error);
  private
    FPath: string;
    FPos: Integer;
    FTokenArrayIndex: Integer;
    FToken: TToken;
    FTokenName: string;
    function GetIsEof: Boolean; inline;
    procedure RaiseError(const AMsg: string); overload;
    procedure RaiseErrorFmt(const AMsg: string; const AParams: array of const); overload;
    procedure SetToken(const AToken: TToken); overload;
    procedure SetToken(const AToken: TToken; const AValue); overload;
    procedure ParseName;
    procedure ParseQuotedName(AQuote: Char);
    procedure ParseArrayIndex;
    procedure ParseIndexer;
    function EnsureLength(ALength: Integer): Boolean; inline;
    procedure EatWhiteSpaces;
  public
    constructor Create(const APath: string);
    function NextToken: TToken;
    property IsEof: Boolean read GetIsEof;
    property Token: TToken read FToken;
    property TokenName: string read FTokenName;
    property TokenArrayIndex: Integer read FTokenArrayIndex;
  end;

  /// <summary> JSON top level class. All specific classes are descendant of it.</summary>
  /// <remarks> All specific classes are descendant of it. More on JSON can be found on www.json.org </remarks>
  TJSONAncestor = class abstract
  private
    /// <summary> True if the instance is own by the container</summary>
    FOwned: Boolean;
  protected
    /// <summary> Returns true if the instance represent JSON null value </summary>
    /// <returns>true if the instance represents JSON null value</returns>
    function IsNull: Boolean; virtual;

    /// <summary> Method used by parser to re-constitute the JSON object structure </summary>
    /// <param name="descendent">descendant to be added</param>
    procedure AddDescendant(const Descendent: TJSONAncestor); virtual; abstract;
    procedure SetOwned(const Own: Boolean); virtual;
  public
    /// <summary> Default constructor, sets owned flag to true </summary>
    constructor Create;

    /// <summary> Where appropriate, returns the instance representation as String </summary>
    /// <returns>string representation, can be null</returns>
    function Value: string; virtual;

    /// <summary> Returns estimated byte size of current JSON object. The actual size is smaller</summary>
    /// <remarks> The actual size is smaller</remarks>
    /// <returns>integer - the byte size</returns>
    function EstimatedByteSize: Integer; virtual; abstract;

    /// <summary> Serializes the JSON object content into bytes. Returns the actual used size.
    /// It assumes the byte container has sufficient capacity to store it. </summary>
    /// <remarks> Returns the actual used size. It assumes the byte container has sufficient capacity to store it.
    /// It is recommended that the container capacity is given by estimatedByteSize </remarks>
    /// <param name="data">- byte container</param>
    /// <param name="offset">- offset from which the object is serialized</param>
    /// <returns>integer - the actual size used</returns>
    function ToBytes(const Data: TArray<Byte>; const Offset: Integer): Integer; virtual; abstract;

    function ToJSON: string;

    /// <summary> Perform deep clone on current value</summary>
    /// <returns>an exact copy of current instance</returns>
    function Clone: TJSONAncestor; virtual; abstract;
    function GetOwned: Boolean; virtual;

    /// <summary> Returns true if the instance represent JSON null value </summary>
    /// <returns>true if the instance represents JSON null value</returns>
    property Null: Boolean read IsNull;
    property Owned: Boolean write SetOwned;
  end;


  /// <summary> Generalizes byte consumption of JSON parser. It accommodates UTF8, default it</summary>
  /// <remarks> It accommodates UTF8, default it assumes the content is generated by JSON toBytes method. </remarks>
  TJSONByteReader = class
  private
    FData: TArray<Byte>;
    FOffset: Integer;
    FRange: Integer;
    FIsUTF8: Boolean;
    FUtf8data: TArray<Byte>;
    FUtf8offset: Integer;
    FUtf8length: Integer;

    /// <summary> Consumes byte-order mark if any is present in the byte data </summary>
    procedure ConsumeBOM;
    procedure MoveOffset;
  protected
    function GetOffset: Integer; virtual;
  public
    constructor Create(const Data: TArray<Byte>; const Offset: Integer; const Range: Integer); overload;
    constructor Create(const Data: TArray<Byte>; const Offset: Integer; const Range: Integer; const IsUTF8: Boolean); overload;
    function ConsumeByte: Byte; virtual;
    function PeekByte: Byte; virtual;
    function Empty: Boolean; virtual;
    function HasMore(const Size: Integer): Boolean; virtual;
    property Offset: Integer read GetOffset;
  end;


  /// <summary> Signals a JSON exception, usually generated by parser code </summary>
  EJSONException = class(Exception)
  private
    const FSerialVersionUID = 1964987864664789863;
  public
    constructor Create(const ErrorMessage: string);
  end;


  /// <summary> Implements JSON string : value </summary>
  TJSONPair = class sealed(TJSONAncestor)
  private
    FJsonString: TJSONString;
    FJsonValue: TJSONValue;
  protected
    /// <summary> see com.borland.dbx.transport.JSONAncestor#addDescendent(com.borland.dbx.transport.JSONAncestor) </summary>
    procedure AddDescendant(const Descendant: TJSONAncestor); override;

    /// <summary> Sets the pair's string value </summary>
    /// <param name="descendant">string object cannot be null</param>
    procedure SetJsonString(const Descendant: TJSONString);

    /// <summary> Sets the pair's value member </summary>
    /// <param name="val">string object cannot be null</param>
    procedure SetJsonValue(const Val: TJSONValue);

    /// <summary> Returns the pair's string. </summary>
    /// <returns>JSONString - pair's string</returns>
    function GetJsonString: TJSONString;

    /// <summary> Returns the pair value. </summary>
    /// <returns>JSONAncestor - pair's value</returns>
    function GetJsonValue: TJSONValue;
  public
    constructor Create; overload;

    /// <summary> Utility constructor providing pair members </summary>
    /// <param name="str">- JSONString member, not null</param>
    /// <param name="value">- JSONValue member, never null</param>
    constructor Create(const Str: TJSONString; const Value: TJSONValue); overload;

    /// <summary> Convenience constructor. Parameters will be converted into JSON equivalents</summary>
    /// <remarks> Parameters will be converted into JSON equivalents </remarks>
    /// <param name="str">- string member</param>
    /// <param name="value">- JSON value</param>
    constructor Create(const Str: string; const Value: TJSONValue); overload;

    /// <summary> Convenience constructor. Parameters are converted into JSON strings pair </summary>
    /// <remarks> Parameters are converted into JSON strings pair </remarks>
    /// <param name="str">- string member</param>
    /// <param name="value">- converted into a JSON string value</param>
    constructor Create(const Str: string; const Value: string); overload;

    /// <summary> Frees string and value</summary>
    destructor Destroy; override;

    /// <summary> see com.borland.dbx.transport.JSONAncestor#estimatedByteSize() </summary>
    function EstimatedByteSize: Integer; override;

    /// <summary> see com.borland.dbx.transport.JSONAncestor#toBytes(byte[], int) </summary>
    function ToBytes(const Data: TArray<Byte>; const Offset: Integer): Integer; override;
    function ToString: string; override;
    function Clone: TJSONAncestor; override;

    /// <summary> Returns the pair's string. </summary>
    /// <returns>JSONString - pair's string</returns>
    property JsonString: TJSONString read GetJsonString write SetJsonString;

    /// <summary> Returns the pair value. </summary>
    /// <returns>JSONAncestor - pair's value</returns>
    property JsonValue: TJSONValue read GetJsonValue write SetJsonValue;
  end;


  /// <summary> Groups string, number, object, array, true, false, null </summary>
  TJSONValue = class abstract(TJSONAncestor)
    private
    function Cast<T>: T;
    function AsTValue(ATypeInfo: PTypeInfo; out AValue: TValue): Boolean; virtual;
  protected
    function FindValue(const APath: string): TJSONValue; virtual;
  public
    /// <summary>Converts a JSON value to a specified type </summary>
    /// <remarks> Returns False when the JSON object could not be converted </remarks>
    function TryGetValue<T>(out AValue: T): Boolean; overload;
	
    /// <summary>Finds a JSON value and converts to a specified type </summary>
    /// <remarks> Returns False when a JSON object could not be found or could not be converted </remarks>
    function TryGetValue<T>(const APath: string; out AValue: T): Boolean; overload;
	
    /// <summary>Finds a JSON value and converts to a specified type </summary>
    /// <remarks> Raises an exception when a JSON object could not be found or the JSON object could not be converted </remarks>
    function GetValue<T>(const APath: string = ''): T; overload;
	
    /// <summary>Finds a JSON value if possible.  If found, the JSON value is converted to a specified type.
    /// If not found or if the JSON value is null, then returns a default value. </summary>
    /// <remarks>Raises an exception when a JSON value is found but can't be converted. </remarks>
    function GetValue<T>(const APath: string; ADefaultValue: T): T; overload;
  end;

  TJSONString = class(TJSONValue)
  protected
    FStrBuffer: TStringBuilder;

    /// <seealso cref="TJSONAncestor.addDescendant(TJSONAncestor)"/>
    procedure AddDescendant(const Descendant: TJSONAncestor); override;

    /// <summary> see com.borland.dbx.transport.JSONAncestor#isNull() </summary>
    function IsNull: Boolean; override;

    function AsTValue(ATypeInfo: PTypeInfo; out AValue: TValue): Boolean; override;

  public
    /// <summary> Converts 0..15 to the equivalent hex digit </summary>
    /// <param name="digit">0 to 15 number</param>
    /// <returns>byte ASCII hex digit code</returns>
    class function Hex(const Digit: TInt15): Byte; static;

    /// <summary> Constructor for null string. No further changes are supported. </summary>
    /// <remarks> No further changes are supported. </remarks>
    constructor Create; overload;

    /// <summary> Constructor for a given string </summary>
    /// <param name="value">String initial value, cannot be null</param>
    constructor Create(const Value: string); overload;
    destructor Destroy; override;

    /// <summary> Adds a character to current content </summary>
    /// <param name="ch">char to be appended</param>
    procedure AddChar(const Ch: WideChar); virtual;

    /// <summary> see com.borland.dbx.transport.JSONAncestor#estimatedByteSize() </summary>
    function EstimatedByteSize: Integer; override;

    /// <summary> see com.borland.dbx.transport.JSONAncestor#toBytes(byte[], int) </summary>
    function ToBytes(const Data: TArray<Byte>; const Idx: Integer): Integer; override;

    /// <summary> Returns the quoted string content. </summary>
    function ToString: string; override;

    /// <summary> Returns the string content </summary>
    function Value: string; override;
    function Clone: TJSONAncestor; override;
  end;

  TJSONNumber = class sealed(TJSONString)
  protected
    /// <summary> Utility constructor with numerical argument represented as string </summary>
    /// <param name="value">- string equivalent of a number</param>
    constructor Create(const Value: string); overload;

    /// <summary> Returns the double representation of the number </summary>
    /// <returns>double</returns>
    function GetAsDouble: Double;

    /// <summary> Returns the integer part of the number </summary>
    /// <returns>int</returns>
    function GetAsInt: Integer;

    /// <summary> Returns the int64 part of the number </summary>
    /// <returns>int64</returns>
    function GetAsInt64: Int64;

  public
    constructor Create; overload;

    /// <summary> Constructor for a double number </summary>
    /// <param name="value">double to be represented as JSONNumber</param>
    constructor Create(const Value: Double); overload;

    /// <summary> Constructor for integer </summary>
    /// <param name="value">integer to be represented as JSONNumber</param>
    constructor Create(const Value: Integer); overload;

    /// <summary> Constructor for integer </summary>
    /// <param name="value">integer to be represented as JSONNumber</param>
    constructor Create(const Value: Int64); overload;

    /// <seealso cref="TJSONString.estimatedByteSize()"/>
    function EstimatedByteSize: Integer; override;

    /// <summary> see com.borland.dbx.transport.JSONString#toBytes(byte[], int) </summary>
    function ToBytes(const Data: TArray<Byte>; const Idx: Integer): Integer; override;

    /// <summary> Returns the non-localized string representation </summary>
    function ToString: string; override;

    /// <summary> Returns the localized representation </summary>
    function Value: string; override;
    function Clone: TJSONAncestor; override;

    /// <summary> Returns the double representation of the number </summary>
    /// <returns>double</returns>
    property AsDouble: Double read GetAsDouble;

    /// <summary> Returns the integer part of the number </summary>
    /// <returns>int</returns>
    property AsInt: Integer read GetAsInt;

    /// <summary> Returns the number as an int64 </summary>
    /// <returns>int64</returns>
    property AsInt64: Int64 read GetAsInt64;
  end;

  /// <summary> Enumerator for JSON pairs </summary>
  TJSONPairEnumerator = class
  private
    FIndex: Integer;
    FList: TList<TJSONPair>;
  public
    constructor Create(const AList: TList<TJSONPair>);
    function GetCurrent: TJSONPair; inline;
    function MoveNext: Boolean;
    property Current: TJSONPair read GetCurrent;
  end;

  /// <summary> JSON object represents {} or { members } </summary>
  TJSONObject = class sealed(TJSONValue)
  public type
    /// <summary> JSON Parser Option </summary>
    TJSONParseOption = (IsUTF8, UseBool);
    /// <summary>
    ///   JSON Parser Options
    ///     IsUTF8 - data should be treated as UTF8
    ///     UseBool - Parser should create a TJSONBool object for each instance of "true" or "false" seen in the JSON data
    /// </summary>
    TJSONParseOptions = set of TJSONParseOption;
  strict protected const

    HexDecimalConvert: array[Byte] of Byte = (
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {00-0F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {10 0F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {20-2F}
       0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0, {30-3F}
       0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0, {40-4F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {50-5F}
       0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0, {60-6F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {70-7F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {80-8F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {90-9F}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {A0-AF}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {B0-BF}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {C0-CF}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {D0-DF}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {E0-EF}
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0); {F0-FF}
  private
    FMembers: TList<TJSONPair>;

    function Parse(const Br: TJSONByteReader; UseBool: Boolean): Integer; overload;
    class procedure ConsumeWhitespaces(const Br: TJSONByteReader); static;
    class function MakeParseOptions(IsUTF8, UseBool: Boolean): TJSONObject.TJSONParseOptions; inline;

    /// <summary> Consumes a JSON object </summary>
    /// <param name="Br"> raw byte data</param>
    /// <param name="Parent"> parent JSON entity</param>
    /// <param name="UseBool"> create a TJSONBool for "true" or "false" seen
    /// in the JSON data</param>
    /// <returns>next offset</returns>
    class function ParseObject(const Br: TJSONByteReader; const Parent: TJSONAncestor; UseBool: Boolean): Integer; static;

    /// <summary> Consumes JSON pair string:value </summary>
    /// <param name="Br">raw byte data</param>
    /// <param name="Parent">parent JSON entity</param>
    /// <param name="UseBool"> create a TJSONBool for "true" or "false" seen
    /// in the JSON data</param>
    /// <returns>next offset</returns>
    class function ParsePair(const Br: TJSONByteReader; const Parent: TJSONObject; UseBool: Boolean): Integer; static;

    /// <summary> Consumes JSON array [...] </summary>
    /// <param name="Br"> raw byte data</param>
    /// <param name="Parent"> parent JSON entity</param>
    /// <param name="UseBool"> create a TJSONBool for "true" or "false" seen
    /// in the JSON data</param>
    /// <returns>next offset</returns>
    class function ParseArray(const Br: TJSONByteReader; const Parent: TJSONAncestor; UseBool: Boolean): Integer; static;

    /// <summary> Consumes JSON values: string, number, object, array, true, false, null </summary>
    /// <param name="Br">raw byte data</param>
    /// <param name="Parent">parent JSON entity</param>
    /// <param name="UseBool"> create a TJSONBool for "true" or "false" seen
    /// in the JSON data</param>
    /// <returns>next offset</returns>
    class function ParseValue(const Br: TJSONByteReader; const Parent: TJSONAncestor; UseBool: Boolean): Integer; static;

    /// <summary> Consumes numbers: int | int frac | int exp | int frac exp </summary>
    /// <param name="Br">raw byte data</param>
    /// <param name="Parent">parent JSON entity</param>
    /// <returns>next offset</returns>
    class function ParseNumber(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;

    /// <summary> Consumes a JSON string "..." </summary>
    /// <param name="Br">raw byte data</param>
    /// <param name="Parent">parent JSON entity</param>
    /// <returns>next offset</returns>
    class function ParseString(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;

  protected
    function FindValue(const APath: string): TJSONValue; override;

    /// <summary> Adds a new member </summary>
    /// <param name="Descendant">- JSON pair</param>
    procedure AddDescendant(const Descendant: TJSONAncestor); override;

    /// <summary> Returns the number of members in its content. May be zero </summary>
    /// <remarks> May be zero </remarks>
    /// <returns>number of members in its content</returns>
    function GetCount: Integer;
	
    /// <summary> Returns the i-th pair or null if i is out of range </summary>
    /// <param name="I">- pair index</param>
    /// <returns>the i-th pair or null if index is out of range</returns>
    function GetPair(const I: Integer): TJSONPair; overload;
	
    /// <summary> Returns a JSON pair based on the pair string part.
    ///  The search is case sensitive and it returns the fist pair with string part matching the argument </summary>
    /// <param name="pairName">- string: the  pair string part</param>
    /// <returns>- JSONPair : first pair encountered, null otherwise</returns>
    function GetPairByName(const PairName: string): TJSONPair; overload;

  public
    /// <summary> Utility function, converts a hex character into hex value [0..15] </summary>
    /// <param name="Value">byte - hex character</param>
    /// <returns>integer - hex value</returns>
    class function HexToDecimal(const Value: Byte): Integer; static; inline;

    /// <summary> Parses a byte array and returns the JSON value from it. </summary>
    /// <remarks> Assumes buffer has only JSON pertinent data. </remarks>
    /// <param name="Data">- byte array, not null</param>
    /// <param name="Offset">- offset from which the parsing starts</param>
    /// <param name="IsUTF8">- true if the Data should be treated as UTF-8. Optional, defaults to true</param>
    /// <param name="Options">- See TJSONParseOptions for correct values</param>
    /// <returns>JSONValue - null if the parse fails</returns>
    class function ParseJSONValue(const Data: TArray<Byte>; const Offset: Integer; IsUTF8: Boolean = True): TJSONValue; overload; inline; static;
    class function ParseJSONValue(const Data: TArray<Byte>; const Offset: Integer; Options: TJSONParseOptions): TJSONValue; overload; inline; static;

    /// <summary> Parses a byte array and returns the JSON value from it. </summary>
    /// <param name="Data">- byte array, not null</param>
    /// <param name="Offset">- offset from which the parsing starts</param>
    /// <param name="ALength">- buffer capacity</param>
    /// <param name="IsUTF8">- true if the Data should be treated as UTF-8. Optional, defaults to true</param>
    /// <param name="Options">- See TJSONParseOptions for correct values</param>
    /// <returns>JSONValue - null if the parse fails</returns>
    class function ParseJSONValue(const Data: TArray<Byte>; const Offset: Integer; const ALength: Integer; IsUTF8: Boolean = True): TJSONValue; overload; inline; static;
    class function ParseJSONValue(const Data: TArray<Byte>; const Offset: Integer; const ALength: Integer; Options: TJSONParseOptions): TJSONValue; overload; static;

    /// <summary> Parses a string and returns the JSON value from it. </summary>
    /// <param name="Data">- String to parse</param>
    /// <param name="UseBool">- Create a TJSONBool for "true" or "false" seen in the JSON data</param>
    /// <returns>JSONValue - null if the parse fails</returns>
    class function ParseJSONValue(const Data: string; UseBool: Boolean = False): TJSONValue; overload; static;
{$IFNDEF NEXTGEN}
    class function ParseJSONValue(const Data: UTF8String; UseBool: Boolean = False): TJSONValue; overload; static;
{$ENDIF !NEXTGEN}

    /// <summary> Default constructor, initializes the members container </summary>
    constructor Create; overload;

    /// <summary> Convenience constructor - builds an object around a given pair </summary>
    /// <param name="Pair">first pair in the object definition, must not be null</param>
    constructor Create(const Pair: TJSONPair); overload;

    /// <summary> Returns an enumerator for pairs </summary>
    /// <remarks> Allows JSONPairs to be accessed using a for-in loop. </remarks>
    /// <returns>The enumerator</returns>
    function GetEnumerator: TJSONPairEnumerator;


    /// <summary> Returns a JSON pair value based on the pair string part. The search is case sensitive and it returns
    /// the fist pair with string part matching the argument </summary>
    /// <param name="Name">- string: the  pair string part</param>
    /// <returns>- JSONValue : value of the first pair encountered, null otherwise</returns>
    function GetValue(const Name: string): TJSONValue; overload;
	
    /// <summary> Releases the stored members </summary>
    destructor Destroy; override;

    /// <summary> Adds a new pair </summary>
    /// <param name="Pair">- a new pair, cannot be null</param>
    function AddPair(const Pair: TJSONPair): TJSONObject; overload;

    /// <summary> Convenience method for adding a pair (name, value). </summary>
    /// <param name="Str">- pair name</param>
    /// <param name="Val">- pair value</param>
    function AddPair(const Str: TJSONString; const Val: TJSONValue): TJSONObject; overload;

    /// <summary> Convenience method for adding a pair to current object. </summary>
    /// <param name="Str">- string: pair name</param>
    /// <param name="Val">- JSONValue: pair value</param>
    function AddPair(const Str: string; const Val: TJSONValue): TJSONObject; overload;
    function AddPair(const Str: string; const Val: string): TJSONObject; overload;

    function RemovePair(const PairName: string): TJSONPair;

    /// <summary> Returns the number of bytes needed to serialize this object </summary>
    function EstimatedByteSize: Integer; override;

    /// <summary> see JSONAncestor#toBytes(byte[], int) </summary>
    function ToBytes(const Data: TArray<Byte>; const Idx: Integer): Integer; override;
    function Clone: TJSONAncestor; override;

    /// <summary> Consumes a JSON object byte representation. </summary>
    /// <remarks> It is recommended to use static function parseJSONValue, unless you are familiar
    ///  with parsing technology. It assumes the buffer has only JSON bytes. </remarks>
    /// <param name="Data">byte[] with JSON stream</param>
    /// <param name="Pos">position within the byte array to start from, negative number if
    ///    parser fails. If negative, the absolute value is the offset where the failure happens. </param>
    /// <param name="UseBool"> create a TJSONBool for "true" or "false" seen
    /// in the JSON data</param>
    /// <returns>negative number on parse error, byte buffer length on success.</returns>
    function Parse(const Data: TArray<Byte>; const Pos: Integer; UseBool: Boolean = False): Integer; overload;

    /// <summary> Consumes a JSON object byte representation. </summary>
    /// <remarks> It is recommended to use static function parseJSONValue, unless you are familiar
    ///  with parsing technology. </remarks>
    /// <param name="Data">byte[] with JSON stream</param>
    /// <param name="Pos">position within the byte array to start from</param>
    /// <param name="Count">number of bytes</param>
    /// <param name="UseBool"> create a TJSONBool for "true" or "false" seen
    /// in the JSON data</param>
    /// <returns>negative number on parse error</returns>
    function Parse(const Data: TArray<Byte>; const Pos: Integer; const Count: Integer; UseBool: Boolean = False): Integer; overload;
    procedure SetPairs(const AList: TList<TJSONPair>);
    function ToString: string; override;
    property Count: Integer read GetCount;
    property Pairs[const Index: Integer]: TJSONPair read GetPair;
    property Values[const Name: string]: TJSONValue read GetValue;
	
	{ Deprecated functions }
    function Size: Integer; inline; deprecated 'Use Count Property';
    function Get(const Index: Integer): TJSONPair; overload; deprecated 'Use Pairs property';
    function Get(const Name: string): TJSONPair; overload; // deprecated
{$IFNDEF NEXTGEN}
    class function ParseJSONValueUTF8(const Data: TArray<Byte>; const Offset: Integer;
                                      const ACount: Integer): TJSONValue; overload; static; deprecated 'Use ParseJSONValue';
    class function ParseJSONValueUTF8(const Data: TArray<Byte>;
                                      const Offset: Integer): TJSONValue; overload; static; deprecated 'Use ParseJSONValue';
{$ENDIF !NEXTGEN}
  end;


  /// <summary> Implements JSON null value </summary>
  TJSONNull = class sealed(TJSONValue)
  private
    const  NULLString: string = 'null';
  protected
    /// <summary> see com.borland.dbx.transport.JSONAncestor#addDescendent(com.borland.dbx.transport.JSONAncestor) </summary>
    procedure AddDescendant(const Descendant: TJSONAncestor); override;

    function AsTValue(ATypeInfo: PTypeInfo; out AValue: TValue): Boolean; override;

    /// <summary> see com.borland.dbx.transport.JSONAncestor#isNull() </summary>
    function IsNull: Boolean; override;

  public
    /// <summary> see com.borland.dbx.transport.JSONAncestor#estimatedByteSize() </summary>
    function EstimatedByteSize: Integer; override;

    /// <summary> see com.borland.dbx.transport.JSONAncestor#toBytes(byte[], int) </summary>
    function ToBytes(const Data: TArray<Byte>; const Offset: Integer): Integer; override;
    function ToString: string; override;
    function Value: string; override;
    function Clone: TJSONAncestor; override;
  end;

  /// <summary> Implements JSON Boolean type from which TJSONTrue and TJSONFalse derive </summary>
  TJSONBool = class(TJSONValue)
  private
    FValue: Boolean;
  strict protected const
    FalseString: string = 'false';
    TrueString: string = 'true';
    FalseBytes: array[0..4] of Byte = (Ord('f'), Ord('a'), Ord('l'), Ord('s'), Ord('e'));
    TrueBytes: array[0..3] of Byte = (Ord('t'), Ord('r'), Ord('u'), Ord('e'));
  protected
    function AsTValue(ATypeInfo: PTypeInfo; out AValue: TValue): Boolean; override;
    /// <summary> see com.borland.dbx.transport.JSONAncestor#addDescendent(com.borland.dbx.transport.JSONAncestor) </summary>
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
  public
    constructor Create(AValue: Boolean);
    /// <summary> see com.borland.dbx.transport.JSONAncestor#estimatedByteSize() </summary>
    function EstimatedByteSize: Integer; override;
    /// <summary> see com.borland.dbx.transport.JSONAncestor#toBytes(byte[], int) </summary>
    function ToBytes(const Data: TArray<Byte>; const Offset: Integer): Integer; override;
    function ToString: string; override;
    function Value: string; override;
    function Clone: TJSONAncestor; override;
    property AsBoolean: Boolean read FValue;
  end;

  /// <summary> Implements JSON true value </summary>
  TJSONTrue = class sealed(TJSONBool)
  public
    constructor Create;
    function Clone: TJSONAncestor; override;
  end;

  /// <summary> Implements JSON false value </summary>
  TJSONFalse = class sealed(TJSONBool)
  public
    constructor Create;
    function Clone: TJSONAncestor; override;
  end;

  TJSONArray = class;

  /// <summary> Support enumeration of values in a JSONArray. </summary>
  TJSONArrayEnumerator = class
  private
    FIndex: Integer;
    FArray: TJSONArray;
  public
    constructor Create(const AArray: TJSONArray);
    function GetCurrent: TJSONValue; inline;
    function MoveNext: Boolean;
    property Current: TJSONValue read GetCurrent;
  end;

  /// <summary> Implements JSON array [] | [ elements ] </summary>
  TJSONArray = class sealed(TJSONValue)
  private
    FElements: TList<TJSONValue>;

  protected
    function FindValue(const APath: string): TJSONValue; override;

    /// <summary> see com.borland.dbx.transport.JSONAncestor#addDescendent(com.borland.dbx.transport.JSONAncestor) </summary>
    procedure AddDescendant(const Descendant: TJSONAncestor); override;

    /// <summary> Removes the first element from the element list. </summary>
    /// <remarks> No checks are made, it is the caller responsibility to check if there is at least one element. </remarks>
    /// <returns>JSONValue</returns>
    function Pop: TJSONValue;

    /// <summary> Returns the array component, null if index is out of range </summary>
    /// <param name="Index">- element index</param>
    /// <returns>JSONValue element, null if index is out of range</returns>
    function GetValue(const Index: Integer): TJSONValue; overload;
    /// <summary> Returns the array size </summary>
    /// <returns>int - array size</returns>
    function GetCount: Integer;


  public
    /// <summary> Default constructor, initializes the container </summary>
    constructor Create; overload;

    /// <summary> Convenience constructor, wraps an array around a JSON value </summary>
    /// <param name="FirstElem">JSON value</param>
    constructor Create(const FirstElem: TJSONValue); overload;

    /// <summary> Convenience constructor, wraps an array around a JSON value </summary>
    /// <param name="FirstElem">JSON value</param>
    /// <param name="SecondElem">JSON value</param>
    constructor Create(const FirstElem: TJSONValue; const SecondElem: TJSONValue); overload;

    constructor Create(const FirstElem: string; const SecondElem: string); overload;
    /// <summary> frees the container elements </summary>
    destructor Destroy; override;

    /// <summary> Returns the array size </summary>
    /// <returns>int - array size</returns>
    property Count: Integer read GetCount;


    /// <summary> Returns the array component, null if index is out of range </summary>
    /// <param name="Index">- element index</param>
    /// <returns>JSONValue element, null if index is out of range</returns>
    property Items[const Index: Integer]: TJSONValue read GetValue;


    /// <summary>Removes the pair at the given index, returning the removed pair (or nil)</summary>
    function Remove(Index: Integer): TJSONValue;

    /// <summary> Adds a non-null value to the current element list </summary>
    /// <param name="Element">string object cannot be null</param>
    procedure AddElement(const Element: TJSONValue);
    function Add(const Element: string): TJSONArray; overload;
    function Add(const Element: Integer): TJSONArray; overload;
    function Add(const Element: Double): TJSONArray; overload;
    function Add(const Element: Boolean): TJSONArray; overload;
    function Add(const Element: TJSONObject): TJSONArray; overload;
    function Add(const Element: TJSONArray): TJSONArray; overload;

    /// <summary> see com.borland.dbx.transport.JSONAncestor#estimatedByteSize() </summary>
    function EstimatedByteSize: Integer; override;

    procedure SetElements(const AList: TList<TJSONValue>);

    // / <seealso cref="TJSONAncestor.ToBytes(TArray<Byte>,Integer)"/>
    function ToBytes(const Data: TArray<Byte>; const Pos: Integer): Integer; override;
    function ToString: string; override;
    function Clone: TJSONAncestor; override;
    function GetEnumerator: TJSONArrayEnumerator;
	
	{ Deprecated functions }
    function Size: Integer; inline; deprecated 'Use Count Property';
    function Get(const Index: Integer): TJSONValue; deprecated 'Use Items property';
  end;

  function GetJSONFormat: TFormatSettings;
  function FloatToJson(const Value: Double): string;
  function JsonToFloat(const DotValue: string): Double;
  function TryJsonToFloat(const DotValue: string; var Value: Double): Boolean;

implementation

uses
  System.Classes, System.DateUtils, System.SysConst, System.StrUtils, System.Character,
  System.JSONConsts;

const
  HexChars = '0123456789ABCDEF';
var
  JSONFormatSettings: TFormatSettings;


function GetJSONFormat: TFormatSettings;
begin
  Result := JSONFormatSettings;
end;

function IncrAfter(var Arg: Integer): Integer;
begin
  Result := Arg;
  Inc(Arg);
end;

function DecrAfter(var Arg: Integer): Integer;
begin
  Result := Arg;
  Dec(Arg);
end;

function FloatToJson(const Value: Double): string;
begin
  Result := FloatToStr(Value, JSONFormatSettings);
end;

function JsonToFloat(const DotValue: string): Double;
begin
  Result := StrToFloat(DotValue, JSONFormatSettings);
end;

function TryJsonToFloat(const DotValue: string; var Value: Double): Boolean;
begin
  Result := TryStrToFloat(DotValue, Value, JSONFormatSettings);
end;


function StrToTValue(const Str: string; const TypeInfo: PTypeInfo; out AValue: TValue): Boolean;

  function CheckRange(const Min, Max: Int64; const Value: Int64; const Str: string): Int64;
  begin
    Result := Value;
    if (Value < Min) or (Value > Max) then
      raise EConvertError.CreateFmt(System.SysConst.SInvalidInteger, [Str]);
  end;
var
  TypeData: TTypeData;
  TypeName: string;
begin
  Result := True;
  case TypeInfo.Kind of
    tkInteger:
      case GetTypeData(TypeInfo)^.OrdType of
        otSByte: AValue := CheckRange(Low(Int8), High(Int8), StrToInt(Str), Str);
        otSWord: AValue := CheckRange(Low(Int16), High(Int16), StrToInt(Str), Str);
        otSLong: AValue := StrToInt(Str);
        otUByte: AValue := CheckRange(Low(UInt8), High(UInt8), StrToInt(Str), Str);
        otUWord: AValue := CheckRange(Low(UInt16), High(UInt16), StrToInt(Str), Str);
        otULong: AValue := CheckRange(Low(UInt32), High(UInt32), StrToInt64(Str), Str);
      end;
    tkInt64:
      begin
        TypeData := GetTypeData(TypeInfo)^;
        if TypeData.MinInt64Value > TypeData.MaxInt64Value then
          AValue := StrToUInt64(Str)
        else
          AValue := StrToInt64(Str);
      end;
    tkEnumeration:
      begin
        TypeName := TypeInfo.NameFld.ToString;
        if SameText(TypeName, 'boolean') or SameText(TypeName, 'bool') then
          AValue := StrToBool(Str)
        else
          Result := False;
      end;
    tkFloat:
      case GetTypeData(TypeInfo)^.FloatType of
        ftSingle: AValue := StrToFloat(Str, JSONFormatSettings);
        ftDouble:
        begin
          if TypeInfo = System.TypeInfo(TDate) then
            AValue := ISO8601ToDate(Str)
          else if TypeInfo = System.TypeInfo(TTime) then
            AValue := ISO8601ToDate(Str)
          else if TypeInfo = System.TypeInfo(TDateTime) then
            AValue := ISO8601ToDate(Str)
          else
            AValue := StrToFloat(Str, JSONFormatSettings);
        end;
        ftExtended: AValue := StrToFloat(Str, JSONFormatSettings);
        ftComp: AValue := StrToFloat(Str, JSONFormatSettings);
        ftCurr: AValue := StrToCurr(Str, JSONFormatSettings);
      end;
{$IFNDEF NEXTGEN}
    tkChar,
{$ENDIF !NEXTGEN}
    tkWChar:
      begin
        if Str.Length = 1 then
          AValue := Str[Low(string)]
        else
          Result := False;
      end;
    tkString, tkLString, tkUString, tkWString:
      AValue := Str;
    else
      Result := False;
  end;
end;


// Traverse a JSONObject or TJSONArray and find the TJSONValue identified by a path string
function FindJSONValue(const AJSON: TJSONValue; const APath: string): TJsonValue; overload;
var
  LCurrentValue: TJSONValue;
  LParser: TJSONPathParser;
  LError: Boolean;
begin
  LParser := TJSONPathParser.Create(APath);
  try
    LCurrentValue := AJSON;
    LError := False;
    while (not LParser.IsEof) and (not LError) do
    begin
      case LParser.NextToken of
        TJSONPathParser.TToken.Name:
        begin
          if LCurrentValue is TJSONObject then
          begin
            if LCurrentValue = nil then
              exit(nil);
            LCurrentValue := TJSONObject(LCurrentValue).Values[LParser.TokenName];
            if LCurrentValue = nil then
              LError := True;
          end
          else
            LError := True;
        end;
        TJSONPathParser.TToken.ArrayIndex:
        begin
          if LCurrentValue is TJSONArray then
            if LParser.TokenArrayIndex < TJSONArray(LCurrentValue).Count then
              LCurrentValue := TJSONArray(LCurrentValue).Items[LParser.TokenArrayIndex]
            else
              LError := True
          else
            LError := True
        end;
        TJSONPathParser.TToken.Error:
          LError := True;
      else
        Assert(LParser.Token = TJSONPathParser.TToken.Eof); // case statement is not complete
      end;
    end;

    if LParser.IsEof and not LError then
      Result := LCurrentValue
    else
      Result := nil;

  finally
    LParser.Free;
  end;
end;

{ TJSONAncestor }

constructor TJSONAncestor.Create;
begin
  inherited Create;
  FOwned := True;
end;

function TJSONAncestor.IsNull: Boolean;
begin
  Result := False;
end;

function TJSONAncestor.Value: string;
begin
  Result := '';
end;

procedure TJSONAncestor.SetOwned(const Own: Boolean);
begin
  FOwned := Own;
end;

function TJSONAncestor.ToJSON: string;
var
  LBytes: TBytes;
begin
  SetLength(LBytes, ToString.Length * 6);
  SetLength(LBytes, ToBytes(LBytes, 0));
  Result := TEncoding.UTF8.GetString(LBytes);
end;

function TJSONAncestor.GetOwned: Boolean;
begin
  Result := FOwned;
end;

{ TJSONByteReader }

constructor TJSONByteReader.Create(const Data: TArray<Byte>; const Offset: Integer; const Range: Integer);
begin
  inherited Create;
  FData := Data;
  FOffset := Offset;
  FRange := Range;
  ConsumeBOM;
end;

constructor TJSONByteReader.Create(const Data: TArray<Byte>; const Offset: Integer; const Range: Integer; const IsUTF8: Boolean);
begin
  inherited Create;
  FData := Data;
  FOffset := Offset;
  FRange := Range;
  FIsUTF8 := IsUTF8;
  if IsUTF8 then
    ConsumeBOM;
end;

procedure TJSONByteReader.ConsumeBOM;
begin
  if FOffset + 3 < FRange then
  begin
    if (FData[FOffset] = Byte(239)) and (FData[FOffset + 1] = Byte(187)) and (FData[FOffset + 2] = Byte(191)) then
    begin
      FIsUTF8 := True;
      FOffset := FOffset + 3;
    end;
  end;
end;

procedure TJSONByteReader.MoveOffset;
begin
  if FUtf8offset < FUtf8length then
    IncrAfter(FUtf8offset)
  else
    IncrAfter(FOffset);
end;

function TJSONByteReader.ConsumeByte: Byte;
var
  Data: Byte;
begin
  Data := PeekByte;
  MoveOffset;
  Result := Data;
end;

function TJSONByteReader.PeekByte: Byte;
var
  Bmp: Int64;
  W1: Integer;
  W2: Integer;
begin
  if not FIsUTF8 then
    Exit(FData[FOffset]);
  if FUtf8offset < FUtf8length then
    Exit(FUtf8data[FUtf8offset]);
  if (FData[FOffset] and (Byte(128))) <> 0 then
  begin
    FUtf8offset := 0;
    if (FData[FOffset] and (Byte(224))) = Byte(192) then
    begin
      if FOffset + 1 >= FRange then
        raise EJSONException.CreateFmt(SUTF8Start, [FOffset]);
      if (FData[FOffset + 1] and (Byte(192))) <> Byte(128) then
        raise EJSONException.CreateFmt(SUTF8UnexpectedByte, [2, FOffset + 1]);
      SetLength(FUtf8data,6);
      FUtf8length := 6;
      FUtf8data[0] := Ord('\');
      FUtf8data[1] := Ord('u');
      FUtf8data[2] := TJSONString.Hex(0);
      FUtf8data[3] := TJSONString.Hex((Byte((FData[FOffset] and Byte(28)))) shr 2);
      FUtf8data[4] := TJSONString.Hex((Byte((Byte(FData[FOffset]) and Byte(3))) shl 2) or (Byte((Byte((FData[FOffset + 1] and Byte(48))) shr 4))));
      FUtf8data[5] := TJSONString.Hex(FData[FOffset + 1] and Byte(15));
      FOffset := FOffset + 2;
    end
    else if (FData[FOffset] and (Byte(240))) = Byte(224) then
    begin
      if FOffset + 2 >= FRange then
        raise EJSONException.CreateFmt(SUTF8Start, [FOffset]);
      if (FData[FOffset + 1] and (Byte(192))) <> Byte(128) then
        raise EJSONException.CreateFmt(SUTF8UnexpectedByte, [3, FOffset + 1]);
      if (FData[FOffset + 2] and (Byte(192))) <> Byte(128) then
        raise EJSONException.CreateFmt(SUTF8UnexpectedByte, [3, FOffset + 2]);
      SetLength(FUtf8data,6);
      FUtf8length := 6;
      FUtf8data[0] := Ord('\');
      FUtf8data[1] := Ord('u');
      FUtf8data[2] := TJSONString.Hex(FData[FOffset] and Byte(15));
      FUtf8data[3] := TJSONString.Hex((Byte((FData[FOffset + 1] and Byte(60)))) shr 2);
      FUtf8data[4] := TJSONString.Hex((Byte((Byte(FData[FOffset + 1]) and Byte(3))) shl 2) or (Byte((Byte((FData[FOffset + 2] and Byte(48))) shr 4))));
      FUtf8data[5] := TJSONString.Hex(FData[FOffset + 2] and Byte(15));
      FOffset := FOffset + 3;
    end
    else if (FData[FOffset] and (Byte(248))) = Byte(240) then
    begin
      if FOffset + 3 >= FRange then
        raise EJSONException.CreateFmt(SUTF8Start, [FOffset]);
      if (FData[FOffset + 1] and (Byte(192))) <> Byte(128) then
        raise EJSONException.CreateFmt(SUTF8UnexpectedByte, [4,FOffset + 1]);
      if (FData[FOffset + 2] and (Byte(192))) <> Byte(128) then
        raise EJSONException.CreateFmt(SUTF8UnexpectedByte, [4,FOffset + 2]);
      if (FData[FOffset + 3] and (Byte(192))) <> Byte(128) then
        raise EJSONException.CreateFmt(SUTF8UnexpectedByte, [4,FOffset + 3]);
      Bmp := FData[FOffset] and Byte(7);
      Bmp := (Bmp shl 6) or (FData[FOffset + 1] and Byte(63));
      Bmp := (Bmp shl 6) or (FData[FOffset + 2] and Byte(63));
      Bmp := (Bmp shl 6) or (FData[FOffset + 3] and Byte(63));
      Bmp := Bmp - 65536;
      W1 := 55296;
      W1 := W1 or ((Integer((Bmp shr 10))) and 2047);
      W2 := 56320;
      W2 := W2 or Integer((Bmp and 2047));
      SetLength(FUtf8data,12);
      FUtf8length := 12;
      FUtf8data[0] := Ord('\');
      FUtf8data[1] := Ord('u');
      FUtf8data[2] := TJSONString.Hex((W1 and 61440) shr 12);
      FUtf8data[3] := TJSONString.Hex((W1 and 3840) shr 8);
      FUtf8data[4] := TJSONString.Hex((W1 and 240) shr 4);
      FUtf8data[5] := TJSONString.Hex(W1 and 15);
      FUtf8data[6] := Ord('\');
      FUtf8data[7] := Ord('u');
      FUtf8data[8] := TJSONString.Hex((W2 and 61440) shr 12);
      FUtf8data[9] := TJSONString.Hex((W2 and 3840) shr 8);
      FUtf8data[10] := TJSONString.Hex((W2 and 240) shr 4);
      FUtf8data[11] := TJSONString.Hex(W2 and 15);
      FOffset := FOffset + 4;
    end
    else
      raise EJSONException.CreateFmt(SUTF8InvalidHeaderByte, [FOffset]);
    Result := FUtf8data[FUtf8offset];
  end
  else
    Result := FData[FOffset];
end;

function TJSONByteReader.Empty: Boolean;
begin
  Result := (FOffset >= FRange) and (FUtf8offset >= FUtf8length);
end;

function TJSONByteReader.GetOffset: Integer;
begin
  Result := FOffset;
end;

function TJSONByteReader.HasMore(const Size: Integer): Boolean;
begin
  if FOffset + Size < FRange then
    Result := True
  else if FUtf8offset + Size < FUtf8length then
    Result := True
  else
    Result := False;
end;

{ EJSONException }

constructor EJSONException.Create(const ErrorMessage: string);
begin
  inherited Create(ErrorMessage);
end;

{ TJSONPair }

constructor TJSONPair.Create;
begin
  inherited Create;
end;

constructor TJSONPair.Create(const Str: TJSONString; const Value: TJSONValue);
begin
  inherited Create;
  FJsonString := Str;
  FJsonValue := Value;
end;

constructor TJSONPair.Create(const Str: string; const Value: TJSONValue);
begin
  Create(TJSONString.Create(Str), Value);
end;

constructor TJSONPair.Create(const Str: string; const Value: string);
begin
  Create(TJSONString.Create(Str), TJSONString.Create(Value));
end;

destructor TJSONPair.Destroy;
begin
  if FJsonString <> nil then
    FreeAndNil(FJsonString);
  if (FJsonValue <> nil) and FJsonValue.GetOwned then
    FreeAndNil(FJsonValue);
  inherited Destroy;
end;

procedure TJSONPair.AddDescendant(const Descendant: TJSONAncestor);
begin
  if FJsonString = nil then
    FJsonString := TJSONString(Descendant)
  else
    FJsonValue := TJSONValue(Descendant);
end;

procedure TJSONPair.SetJsonString(const Descendant: TJSONString);
begin
  if Descendant <> nil then
    FJsonString := Descendant;
end;

procedure TJSONPair.SetJsonValue(const Val: TJSONValue);
begin
  if Val <> nil then
    FJsonValue := Val;
end;

function TJSONPair.EstimatedByteSize: Integer;
begin
  Result := 1 + FJsonString.EstimatedByteSize + FJsonValue.EstimatedByteSize;
end;

function TJSONPair.ToBytes(const Data: TArray<Byte>; const Offset: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := FJsonString.ToBytes(Data, Offset);
  Data[IncrAfter(Idx)] := Ord(':');
  Result := FJsonValue.ToBytes(Data, Idx);
end;

function TJSONPair.GetJsonString: TJSONString;
begin
  Result := FJsonString;
end;

function TJSONPair.GetJsonValue: TJSONValue;
begin
  Result := FJsonValue;
end;

function TJSONPair.ToString: string;
begin
  if (FJsonString <> nil) and (FJsonValue <> nil) then
    Result := FJsonString.ToString + ':' + FJsonValue.ToString
  else
    Result := '';
end;

function TJSONPair.Clone: TJSONAncestor;
begin
  Result := TJSONPair.Create(TJSONString(FJsonString.Clone), TJSONValue(FJsonValue.Clone));
end;

{ TJSONValue }

function TJSONValue.GetValue<T>(const APath: string; ADefaultValue: T): T;
var
  LValue: T;
  LJSONValue: TJSONValue;
  LTypeInfo: PTypeInfo;
  LReturnDefault: Boolean;
begin
  LJSONValue := FindValue(APath);
  LReturnDefault := LJSONValue = nil;

  // Treat JSONNull as nil
  if LJSONValue is TJSONNull then
  begin
    LTypeInfo := System.TypeInfo(T);
    if LTypeInfo.TypeData.ClassType <> nil then
      LJSONValue := nil;
  end;

  if LJSONValue <> nil then
    Result := LJSONValue.Cast<T>
  else
    Result := ADefaultValue
end;

function TJSONValue.GetValue<T>(const APath: string): T;
var
  LValue: T;
  LJSONValue: TJSONValue;
begin
  LJSONValue := FindValue(APath);
  if LJSONValue = nil then
    raise EJSONException.Create(Format(SValueNotFound, [APath]));
  Result := LJSONValue.Cast<T>;
end;

function TJSONValue.FindValue(const APath: string): TJSONValue;
begin
  if APath = '' then
    Result := Self
  else
    Result := nil;
end;

function TJSONValue.AsTValue(ATypeInfo: PTypeInfo; out AValue: TValue): Boolean;
begin
  Result := True;
  case ATypeInfo^.Kind of
    tkClass:
      AValue := Self;
    else
      Result := False;
  end;
end;

function TJSONValue.Cast<T>: T;
var
  Size: Integer;
  LTypeInfo: PTypeInfo;
  LValue: TValue;
begin
  LTypeInfo := System.TypeInfo(T);
  if not AsTValue(LTypeInfo, LValue) then
    raise EJSONException.CreateFmt(sCannotConvertJSONValueToType, [Self.ClassName, LTypeInfo.Name]);
  Result := LValue.AsType<T>;
end;

function TJSONValue.TryGetValue<T>(out AValue: T): Boolean;
begin
  Result := TryGetValue<T>('', AValue);
end;

function TJSONValue.TryGetValue<T>(const APath: string; out AValue: T): Boolean;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := FindValue(APath);
  Result := LJSONValue <> nil;
  if Result then
  begin
    try
      AValue := LJSONValue.Cast<T>;
    except
      Result := False;
    end;
  end;
end;

{ TJSONTrue }

constructor TJSONTrue.Create;
begin
  inherited Create(True);
end;

function TJSONTrue.Clone: TJSONAncestor;
begin
  Result := TJSONTrue.Create;
end;

{ TJSONString }

class function TJSONString.Hex(const Digit: TInt15): Byte;
begin
  Result := Byte(HexChars.Chars[Digit]);
end;

constructor TJSONString.Create;
begin
  inherited Create;
end;

constructor TJSONString.Create(const Value: string);
begin
  inherited Create;
  FStrBuffer := TStringBuilder.Create(Value);
end;

destructor TJSONString.Destroy;
begin
  FreeAndNil(FStrBuffer);
  inherited Destroy;
end;

procedure TJSONString.AddChar(const Ch: WideChar);
begin
  FStrBuffer.Append(Ch);
end;

procedure TJSONString.AddDescendant(const Descendant: TJSONAncestor);
begin
//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TJSONString.IsNull: Boolean;
begin
  if FStrBuffer = nil then
    Result := True
  else
    Result := False;
end;

function TJSONString.AsTValue(ATypeInfo: PTypeInfo; out AValue: TValue): Boolean;
begin
  case ATypeInfo.Kind of
    tkInteger, tkInt64, tkFloat,
    tkString, tkLString, tkWString, tkUString,
{$IFNDEF NEXTGEN}
    tkChar,
{$ENDIF !NEXTGEN}
    tkWChar,
    tkEnumeration:
      Result := StrToTValue(FStrBuffer.ToString, ATypeInfo, AValue)
    else
      Result := inherited;
  end;
end;

function TJSONString.EstimatedByteSize: Integer;
begin
  if Null then
    Result := 4
  else
    Result := 2 + 6 * FStrBuffer.Length;
end;

function TJSONString.ToBytes(const Data: TArray<Byte>; const Idx: Integer): Integer;
var
  Offset: Integer;
  Index: Integer;
  Count: Integer;
  CurrentChar: WideChar;
  UnicodeValue: Integer;
begin
  Offset := Idx;
  if Null then
  begin
    Data[IncrAfter(Offset)] := Ord('n');
    Data[IncrAfter(Offset)] := Ord('u');
    Data[IncrAfter(Offset)] := Ord('l');
    Data[IncrAfter(Offset)] := Ord('l');
  end
  else
  begin
    Data[IncrAfter(Offset)] := Ord('"');
    Index := 0;
    Count := FStrBuffer.Length;
    while Index < Count do
    begin
      CurrentChar := FStrBuffer.Chars[IncrAfter(Index)];
      case CurrentChar of
        '"':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('"');
          end;
        '\':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('\');
          end;
        '/':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('/');
          end;
        #$8:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('b');
          end;
        #$c:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('f');
          end;
        #$a:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('n');
          end;
        #$d:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('r');
          end;
        #$9:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('t');
          end;
        else
          if (CurrentChar < WideChar(32)) or (CurrentChar > WideChar(127)) then
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('u');
            UnicodeValue := Ord(CurrentChar);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 61440) shr 12);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 3840) shr 8);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 240) shr 4);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 15));
          end
          else
            Data[IncrAfter(Offset)] := Ord(CurrentChar);
      end;
    end;
    Data[IncrAfter(Offset)] := Ord('"');
  end;
  Result := Offset;
end;

function TJSONString.ToString: string;
begin
  if FStrBuffer = nil then
    Result := ''
  else
    Result := '"' + AnsiReplaceStr(FStrBuffer.ToString, '"', '\"') + '"';
end;

function TJSONString.Value: string;
begin
  if FStrBuffer = nil then
    Result := ''
  else
    Result := FStrBuffer.ToString;
end;

function TJSONString.Clone: TJSONAncestor;
begin
  if FStrBuffer = nil then
    Result := TJSONString.Create
  else
    Result := TJSONString.Create(Value);
end;

{ TJSONNumber }

constructor TJSONNumber.Create;
begin
  inherited Create('');
end;

constructor TJSONNumber.Create(const Value: Double);
begin
  inherited Create(FloatToJson(Value));
end;

constructor TJSONNumber.Create(const Value: string);
begin
  inherited Create(Value);
end;

constructor TJSONNumber.Create(const Value: Int64);
begin
  inherited Create(IntToStr(Value));
end;

constructor TJSONNumber.Create(const Value: Integer);
begin
  inherited Create(IntToStr(Value));
end;

function TJSONNumber.EstimatedByteSize: Integer;
begin
  Result := FStrBuffer.Length;
end;

function TJSONNumber.ToBytes(const Data: TArray<Byte>; const Idx: Integer): Integer;
var
  Offset: Integer;
  Index: Integer;
  Count: Integer;
  CurrentChar: WideChar;
begin
  Offset := Idx;
  Index := 0;
  Count := FStrBuffer.Length;
  while Index < Count do
  begin
    CurrentChar := FStrBuffer.Chars[IncrAfter(Index)];
    Data[IncrAfter(Offset)] := Ord(CurrentChar);
  end;
  Result := Offset;
end;

function TJSONNumber.ToString: string;
begin
  Result := FStrBuffer.ToString;
end;

function TJSONNumber.Value: string;
var
  BuffStr: string;
begin
  BuffStr := FStrBuffer.ToString;
  if (FStrBuffer.Length > 11) and (not BuffStr.Contains(JSONFormatSettings.DecimalSeparator)) then
    Result := IntToStr(GetAsInt64)
  else
    Result := FloatToStr(JsonToFloat(BuffStr));
end;

function TJSONNumber.Clone: TJSONAncestor;
begin
  Result := TJSONNumber.Create(ToString);
end;

function TJSONNumber.GetAsDouble: Double;
begin
  Result := JsonToFloat(FStrBuffer.ToString);
end;

function TJSONNumber.GetAsInt: Integer;
begin
  Result := StrToInt(FStrBuffer.ToString);
end;

function TJSONNumber.GetAsInt64: Int64;
begin
  Result := StrToInt64(FStrBuffer.ToString);
end;

{ TJSONObject }

class function TJSONObject.HexToDecimal(const Value: Byte): Integer;
begin
  Result := HexDecimalConvert[Value];
end;

class function TJSONObject.ParseJSONValue(const Data: TArray<Byte>; const Offset: Integer; const ALength: Integer; Options: TJSONParseOptions): TJSONValue;
var
  Parent: TJSONArray;
  Answer: TJSONValue;
  Br: TJSONByteReader;
begin
  Parent := TJSONArray.Create;
  Answer := nil;
  Br := TJSONByteReader.Create(Data, Offset, ALength, TJSONParseOption.IsUTF8 in Options);
  try
    ConsumeWhitespaces(Br);
    if (ParseValue(Br, Parent, TJSONParseOption.UseBool in Options) = ALength) and (Parent.Count = 1) then
      Answer := Parent.Pop;
    Result := Answer;
  finally
    Parent.Free;
    Br.Free;
  end;
end;

class function TJSONObject.MakeParseOptions(IsUTF8, UseBool: Boolean): TJSONObject.TJSONParseOptions;
begin
  Result := [];
  if IsUTF8 then Include(Result, TJSONObject.TJSONParseOption.IsUTF8);
  if UseBool then Include(Result, TJSONObject.TJSONParseOption.UseBool);
end;

class function TJSONObject.ParseJSONValue(const Data: TArray<Byte>; const Offset: Integer; Options: TJSONParseOptions): TJSONValue;
begin
  Result := ParseJSONValue(Data, Offset, Length(Data), Options);
end;

class function TJSONObject.ParseJSONValue(const Data: TArray<Byte>; const Offset: Integer; IsUTF8: Boolean): TJSONValue;
begin
  Result := ParseJSONValue(Data, Offset, Length(Data), MakeParseOptions(IsUTF8, False));
end;

class function TJSONObject.ParseJSONValue(const Data: TArray<Byte>; const Offset: Integer; const ALength: Integer;
  IsUTF8: Boolean): TJSONValue;
begin
  Result := ParseJSONValue(Data, Offset, ALength, MakeParseOptions(IsUTF8, False));
end;

class function TJSONObject.ParseJSONValue(const Data: string; UseBool: Boolean): TJSONValue;
begin
  Result := ParseJSONValue(TEncoding.UTF8.GetBytes(Data), 0, MakeParseOptions(True, UseBool));
end;

{$IFNDEF NEXTGEN}
class function TJSONObject.ParseJSONValue(const Data: UTF8String; UseBool: Boolean): TJSONValue;
begin
  Result := ParseJSONValue(BytesOf(Data), 0, MakeParseOptions(True, UseBool));
end;

class function TJSONObject.ParseJSONValueUTF8(const Data: TArray<Byte>; const Offset: Integer;
  const ACount: Integer): TJSONValue;
begin
  Result := ParseJSONValue(Data, Offset, ACount, MakeParseOptions(True, False));
end;

class function TJSONObject.ParseJSONValueUTF8(const Data: TArray<Byte>; const Offset: Integer): TJSONValue;
begin
  Result := ParseJSONValue(Data, Offset, Length(Data), MakeParseOptions(True, False));
end;
{$ENDIF !NEXTGEN}

constructor TJSONObject.Create;
begin
  inherited Create;
  FMembers := TList<TJSONPair>.Create;
end;

constructor TJSONObject.Create(const Pair: TJSONPair);
begin
  Create;
  if Pair <> nil then
    FMembers.Add(Pair);
end;

procedure TJSONObject.SetPairs(const AList: TList<TJSONPair>);
begin
  FMembers.Free;
  FMembers := AList;
end;

function TJSONObject.Size: Integer;
begin
  Result := GetCount;
end;

function TJSONObject.Get(const Index: Integer): TJSONPair;
begin
  Result := Pairs[Index];
end;

function TJSONObject.Get(const Name: string): TJSONPair;
begin
  Result := GetPairByName(Name);
end;

function TJSONObject.GetCount: Integer;
begin
  Result := FMembers.Count;
end;

function TJSONObject.GetPair(const I: Integer): TJSONPair;
begin
  if (I >= 0) and (I < Count) then
    Result := FMembers[I]
  else
    Result := nil;
end;

function TJSONObject.GetPairByName(const PairName: string): TJSONPair;
var
  Candidate: TJSONPair;
  I: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if FMembers[i] = nil then
      exit(nil);
    Candidate := TJSONPair(FMembers[I]);
    if Candidate = nil then
      exit(nil);
    IF CANDIDATE.JSONSTRING = nil then
      exit(nil);
    if (Candidate.JsonString.Value = PairName) then
      Exit(Candidate);
  end;
  Result := nil;
end;

function TJSONObject.GetValue(const Name: string): TJSONValue;
var
  LPair: TJSONPair;
begin
  LPair := GetPairByName(Name);
  if LPair <> nil then
    Result := LPair.JSONValue
  else
    Result := nil;
end;

function TJSONObject.GetEnumerator: TJSONPairEnumerator;
begin
  Result := TJSONPairEnumerator.Create(FMembers);
end;

destructor TJSONObject.Destroy;
var
  Member: TJSONAncestor;
  I: Integer;
begin
  if FMembers <> nil then
  begin
    for i := 0 to FMembers.Count - 1 do
    begin
      Member := TJSONAncestor(FMembers[I]);
      if Member.GetOwned then
        Member.Free;
    end;
    FreeAndNil(FMembers);
  end;
  inherited Destroy;
end;

function TJSONObject.AddPair(const Pair: TJSONPair): TJSONObject;
begin
  if Pair <> nil then
    AddDescendant(Pair);
  Result := Self;
end;

function TJSONObject.AddPair(const Str: TJSONString; const Val: TJSONValue): TJSONObject;
begin
  if (Str <> nil) and (Val <> nil) then
    AddPair(TJSONPair.Create(Str, Val));
  Result := Self;
end;

function TJSONObject.AddPair(const Str: string; const Val: TJSONValue): TJSONObject;
begin
  if (not Str.IsEmpty) and (Val <> nil) then
    AddPair(TJSONPair.Create(Str, Val));
  Result := Self;
end;

function TJSONObject.AddPair(const Str: string; const Val: string): TJSONObject;
begin
  if (not Str.IsEmpty) and (not Val.IsEmpty) then
    AddPair(TJSONPair.Create(Str, Val));
  Result := Self;
end;

function TJSONObject.FindValue(const APath: string): TJSONValue;
begin
  Result := FindJSONValue(Self, APath);
end;

procedure TJSONObject.AddDescendant(const Descendant: TJSONAncestor);
begin
  FMembers.Add(TJSONPair(Descendant));
end;

function TJSONObject.EstimatedByteSize: Integer;
var
  Size: Integer;
  I: Integer;
begin
  Size := 1;
  for i := 0 to FMembers.Count - 1 do
    Size := Size + (TJSONAncestor(FMembers[I])).EstimatedByteSize + 1;
  if Size = 1 then
    Exit(2);
  Result := Size;
end;

function TJSONObject.ToBytes(const Data: TArray<Byte>; const Idx: Integer): Integer;
var
  Offset: Integer;
  Size: Integer;
  I: Integer;
begin
  Offset := Idx;
  Size := FMembers.Count;
  Data[IncrAfter(Offset)] := Ord('{');
  if Size > 0 then
    Offset := (TJSONAncestor(FMembers[0])).ToBytes(Data, Offset);
  for i := 1 to FMembers.Count - 1 do
  begin
    Data[IncrAfter(Offset)] := Ord(',');
    Offset := (TJSONAncestor(FMembers[I])).ToBytes(Data, Offset);
  end;
  Data[IncrAfter(Offset)] := Ord('}');
  Result := Offset;
end;

function TJSONObject.Clone: TJSONAncestor;
var
  Data: TJSONObject;
  I: Integer;
begin
  Data := TJSONObject.Create;
  for I := 0 to FMembers.Count - 1 do
    Data.AddPair(TJSONPair(Pairs[I].Clone));
  Result := Data;
end;

function TJSONObject.Parse(const Data: TArray<Byte>; const Pos: Integer; UseBool: Boolean = False): Integer;
var
  Offset: Integer;
  Count: Integer;
begin
  Count := Length(Data);
  Offset := Parse(Data, Pos, Count, UseBool);
  if Offset = Count then
    Result := Count
  else if Offset < 0 then
    Result := Offset
  else
    Result := -Offset;
end;

function TJSONObject.Parse(const Data: TArray<Byte>; const Pos: Integer; const Count: Integer; UseBool: Boolean = False): Integer;
var
  Br: TJSONByteReader;
begin
  if (Data = nil) or (Pos < 0) or (Pos >= Count) then
    Exit(-1);
  Br := TJSONByteReader.Create(Data, Pos, Count, True);
  try
    Result := Parse(Br, UseBool);
  finally
    Br.Free;
  end;
end;

function TJSONObject.Parse(const Br: TJSONByteReader; UseBool: Boolean): Integer;
var
  SepPos: Integer;
  PairExpected: Boolean;
begin
  ConsumeWhitespaces(Br);
  if Br.Empty then
    Exit(-Br.Offset);
  if Br.PeekByte <> Ord('{') then
    Exit(-Br.Offset);
  Br.ConsumeByte;
  ConsumeWhitespaces(Br);
  if Br.Empty then
    Exit(-Br.Offset);
  PairExpected := False;
  while PairExpected or (Br.PeekByte <> Ord('}')) do
  begin
    SepPos := ParsePair(Br, Self, UseBool);
    if SepPos <= 0 then
      Exit(SepPos);
    ConsumeWhitespaces(Br);
    if Br.Empty then
      Exit(-Br.Offset);
    PairExpected := False;
    if Br.PeekByte = Ord(',') then
    begin
      Br.ConsumeByte;
      ConsumeWhitespaces(Br);
      PairExpected := True;
      if Br.PeekByte = Ord('}') then
        Exit(-Br.Offset);
    end;
  end;
  Br.ConsumeByte;
  ConsumeWhitespaces(Br);
  Result := Br.Offset;
end;

class procedure TJSONObject.ConsumeWhitespaces(const Br: TJSONByteReader);
var
  Current: Byte;
begin
  while not Br.Empty do
  begin
    Current := Br.PeekByte;
    case Current of
      32,
      9,
      10,
      13:
        Br.ConsumeByte;
      else
        Exit;
    end;
  end;
end;

class function TJSONObject.ParseObject(const Br: TJSONByteReader; const Parent: TJSONAncestor; UseBool: Boolean): Integer;
var
  JsonObj: TJSONObject;
begin
  JsonObj := TJSONObject.Create;
  Parent.AddDescendant(JsonObj);
  Result := JsonObj.Parse(Br, UseBool);
end;

class function TJSONObject.ParsePair(const Br: TJSONByteReader; const Parent: TJSONObject; UseBool: Boolean): Integer;
var
  Pair: TJSONPair;
  CommaPos: Integer;
begin
  Pair := TJSONPair.Create;
  Parent.AddDescendant(Pair);
  CommaPos := ParseString(Br, Pair);
  if CommaPos > 0 then
  begin
    ConsumeWhitespaces(Br);
    if Br.Empty then
      Exit(-Br.Offset);
    if Br.PeekByte <> Ord(':') then
      Exit(-Br.Offset);
    Br.ConsumeByte;
    ConsumeWhitespaces(Br);
    CommaPos := ParseValue(Br, Pair, UseBool);
  end;
  Result := CommaPos;
end;

class function TJSONObject.ParseArray(const Br: TJSONByteReader; const Parent: TJSONAncestor; UseBool: Boolean): Integer;
var
  ValueExpected: Boolean;
  JsonArray: TJSONArray;
  Pos: Integer;
begin
  ConsumeWhitespaces(Br);
  if Br.Empty then
    Exit(-Br.Offset);
  if Br.PeekByte <> Ord('[') then
    Exit(-Br.Offset);
  Br.ConsumeByte;
  JsonArray := TJSONArray.Create;
  Parent.AddDescendant(JsonArray);
  ValueExpected := False;
  ConsumeWhitespaces(Br);
  while ValueExpected or (Br.PeekByte <> Ord(']')) do
  begin
    ConsumeWhitespaces(Br);
    Pos := ParseValue(Br, JsonArray, UseBool);
    if Pos <= 0 then
      Exit(Pos);
    ConsumeWhitespaces(Br);
    if Br.Empty then
      Exit(-Br.Offset);
    ValueExpected := False;
    if Br.PeekByte = Ord(',') then
    begin
      Br.ConsumeByte;
      ValueExpected := True;
    end
    else if Br.PeekByte <> Ord(']') then
      Exit(-Br.Offset);
  end;
  Br.ConsumeByte;
  ConsumeWhitespaces(Br);
  Result := Br.Offset;
end;

class function TJSONObject.ParseValue(const Br: TJSONByteReader; const Parent: TJSONAncestor; UseBool: Boolean): Integer;
var
  Pos: Integer;
begin
  Pos := Br.Offset;
  if Br.Empty then
    Exit(-Pos);
  case Br.PeekByte of
    Ord('"'):
      Exit(ParseString(Br, Parent));
    Ord('-'),
    Ord('0'),
    Ord('1'),
    Ord('2'),
    Ord('3'),
    Ord('4'),
    Ord('5'),
    Ord('6'),
    Ord('7'),
    Ord('8'),
    Ord('9'):
      Exit(ParseNumber(Br, Parent));
    Ord('{'):
      Exit(ParseObject(Br, Parent, UseBool));
    Ord('['):
      Exit(ParseArray(Br, Parent, UseBool));
    Ord('t'):
      begin
        if not Br.HasMore(3) then
          Exit(-Pos);
        Br.ConsumeByte;
        if (Br.ConsumeByte <> Ord('r')) or (Br.ConsumeByte <> Ord('u')) or (Br.ConsumeByte <> Ord('e')) then
          Exit(-Pos);
        if UseBool then
          Parent.AddDescendant(TJSONBool.Create(True))
        else
          Parent.AddDescendant(TJSONTrue.Create);
        Exit(Br.Offset);
      end;
    Ord('f'):
      begin
        if not Br.HasMore(4) then
          Exit(-Pos);
        Br.ConsumeByte;
        if (Br.ConsumeByte <> Ord('a')) or (Br.ConsumeByte <> Ord('l')) or (Br.ConsumeByte <> Ord('s')) or (Br.ConsumeByte <> Ord('e')) then
          Exit(-Pos);
        if UseBool then
          Parent.AddDescendant(TJSONBool.Create(False))
        else
          Parent.AddDescendant(TJSONFalse.Create);
        Exit(Br.Offset);
      end;
    Ord('n'):
      begin
        if not Br.HasMore(3) then
          Exit(-Pos);
        Br.ConsumeByte;
        if (Br.ConsumeByte <> Ord('u')) or (Br.ConsumeByte <> Ord('l')) or (Br.ConsumeByte <> Ord('l')) then
          Exit(-Pos);
        Parent.AddDescendant(TJSONNull.Create);
        Exit(Br.Offset);
      end;
  end;
  Result := -Pos;
end;

function TJSONObject.RemovePair(const PairName: string): TJSONPair;
var
  Candidate: TJSONPair;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Candidate := TJSONPair(FMembers[I]);
    if (Candidate.JsonString.Value = PairName) then
    begin
      FMembers.Remove(FMembers[i]);
      Exit(Candidate);
    end;
  end;
  Result := nil;
end;

class function TJSONObject.ParseNumber(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  Nb: TJSONNumber;
  Consume: Boolean;
  Exponent: Boolean;
  OneAdded: Boolean;
begin
  Nb := TJSONNumber.Create;
  Parent.AddDescendant(Nb);
  if Br.PeekByte = Ord('-') then
  begin
    Nb.AddChar('-');
    Br.ConsumeByte;
    if Br.Empty then
      Exit(-1);
  end;
  if Br.PeekByte = Ord('0') then
  begin
    Nb.AddChar('0');
    Br.ConsumeByte;
    if Br.Empty then
      Exit(Br.Offset);
    case Br.PeekByte of
      Ord('0'),
      Ord('1'),
      Ord('2'),
      Ord('3'),
      Ord('4'),
      Ord('5'),
      Ord('6'),
      Ord('7'),
      Ord('8'),
      Ord('9'):
        Exit(-Br.Offset);
    end;
  end;
  Consume := True;
  while Consume do
    case Br.PeekByte of
      Ord('0'),
      Ord('1'),
      Ord('2'),
      Ord('3'),
      Ord('4'),
      Ord('5'),
      Ord('6'),
      Ord('7'),
      Ord('8'),
      Ord('9'):
        begin
          Nb.AddChar(WideChar(Br.ConsumeByte));
          if Br.Empty then
            Exit(Br.Offset);
        end;
      else
        Consume := False;
    end;
  Exponent := False;
  if Br.PeekByte = Ord(JSONFormatSettings.DecimalSeparator) then
  begin
    Nb.AddChar(JSONFormatSettings.DecimalSeparator);
    Br.ConsumeByte;
    if Br.Empty then
      Exit(-Br.Offset);
  end
  else if (Br.PeekByte = Ord('e')) or (Br.PeekByte = Ord('E')) then
  begin
    Nb.AddChar(WideChar(Br.ConsumeByte));
    Exponent := True;
    if Br.Empty then
      Exit(-Br.Offset);
    if (Br.PeekByte = Ord('-')) or (Br.PeekByte = Ord('+')) then
    begin
      Nb.AddChar(WideChar(Br.ConsumeByte));
      if Br.Empty then
        Exit(-Br.Offset);
    end;
  end
  else
    Exit(Br.Offset);
  OneAdded := False;
  Consume := True;
  while Consume do
    case Br.PeekByte of
      Ord('0'),
      Ord('1'),
      Ord('2'),
      Ord('3'),
      Ord('4'),
      Ord('5'),
      Ord('6'),
      Ord('7'),
      Ord('8'),
      Ord('9'):
        begin
          Nb.AddChar(WideChar(Br.ConsumeByte));
          OneAdded := True;
          if Br.Empty then
            Exit(Br.Offset);
        end;
      else
        Consume := False;
    end;
  if not OneAdded then
    Exit(-Br.Offset);
  if not Exponent and ((Br.PeekByte = Ord('e')) or (Br.PeekByte = Ord('E'))) then
  begin
    Nb.AddChar(WideChar(Br.ConsumeByte));
    if Br.Empty then
      Exit(-Br.Offset);
    if (Br.PeekByte = Ord('-')) or (Br.PeekByte = Ord('+')) then
    begin
      Nb.AddChar(WideChar(Br.ConsumeByte));
      if Br.Empty then
        Exit(-Br.Offset);
    end;
    OneAdded := False;
    Consume := True;
    while Consume do
      case Br.PeekByte of
        Ord('0'),
        Ord('1'),
        Ord('2'),
        Ord('3'),
        Ord('4'),
        Ord('5'),
        Ord('6'),
        Ord('7'),
        Ord('8'),
        Ord('9'):
          begin
            Nb.AddChar(WideChar(Br.ConsumeByte));
            OneAdded := True;
            if Br.Empty then
              Exit(Br.Offset);
          end;
        else
          Consume := False;
      end;
    if not OneAdded then
      Exit(-Br.Offset);
  end;
  Result := Br.Offset;
end;

class function TJSONObject.ParseString(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  UnicodeCh: Integer;
  Ch: WideChar;
  Str: TJSONString;
begin
  Ch := ' ';
  if Br.PeekByte <> Ord('"') then
    Exit(-Br.Offset);
  Br.ConsumeByte;
  if Br.Empty then
    Exit(-Br.Offset);
  Str := TJSONString.Create('');
  Parent.AddDescendant(Str);
  while Br.PeekByte <> Ord('"') do
  begin
    case Br.PeekByte of
      Ord('\'):
        begin
          Br.ConsumeByte;
          if Br.Empty then
            Exit(-Br.Offset);
          case Br.PeekByte of
            Ord('"'):
              Ch := '"';
            Ord('\'):
              Ch := '\';
            Ord('/'):
              Ch := '/';
            Ord('b'):
              Ch := #$8;
            Ord('f'):
              Ch := #$c;
            Ord('n'):
              Ch := #$a;
            Ord('r'):
              Ch := #$d;
            Ord('t'):
              Ch := #$9;
            Ord('u'):
              begin
                Br.ConsumeByte;
                if not Br.HasMore(3) then
                  Exit(-Br.Offset);
                UnicodeCh := HexToDecimal(Br.ConsumeByte) shl 12;
                UnicodeCh := UnicodeCh or HexToDecimal(Br.ConsumeByte) shl 8;
                UnicodeCh := UnicodeCh or HexToDecimal(Br.ConsumeByte) shl 4;
                UnicodeCh := UnicodeCh or HexToDecimal(Br.PeekByte);
                Ch := WideChar(UnicodeCh);
              end;
            else
              Exit(-Br.Offset);
          end;
        end;
      else
        Ch := WideChar(Br.PeekByte);
    end;
    Str.AddChar(Ch);
    Br.ConsumeByte;
    if Br.Empty then
      Exit(-Br.Offset);
  end;
  Br.ConsumeByte;
  Result := Br.Offset;
end;

function TJSONObject.ToString: string;
var
  Buf: TStringBuilder;
  Size: Integer;
  I: Integer;
begin
  Size := FMembers.Count;
  Buf := TStringBuilder.Create;
  try
    Buf.Append('{');
    if Size > 0 then
      Buf.Append(FMembers[0].ToString);
    for I := 1 to Size - 1 do
    begin
      Buf.Append(',');
      Buf.Append(FMembers[I].ToString);
    end;
    Buf.Append('}');
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

{ TJSONNull }

procedure TJSONNull.AddDescendant(const Descendant: TJSONAncestor);
begin
//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TJSONNull.AsTValue(ATypeInfo: PTypeInfo; out AValue: TValue): Boolean;
begin
  Result := True;
  case ATypeInfo.Kind of
    tkString, tkLString, tkUString, TkWString:
      AValue := '';
    else
      Result := inherited;
  end;
end;

function TJSONNull.IsNull: Boolean;
begin
  Result := True;
end;

function TJSONNull.EstimatedByteSize: Integer;
begin
  Result := 4;
end;

function TJSONNull.ToBytes(const Data: TArray<Byte>; const Offset: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := Offset;
  Data[IncrAfter(Idx)] := Ord('n');
  Data[IncrAfter(Idx)] := Ord('u');
  Data[IncrAfter(Idx)] := Ord('l');
  Data[IncrAfter(Idx)] := Ord('l');
  Result := Idx;
end;

function TJSONNull.ToString: string;
begin
  Result := NULLString;
end;

function TJSONNull.Value: string;
begin
  Result := NULLString;
end;

function TJSONNull.Clone: TJSONAncestor;
begin
  Result := TJSONNull.Create;
end;

{ TJSONFalse }

constructor TJSONFalse.Create;
begin
  inherited Create(False);
end;

function TJSONFalse.Clone: TJSONAncestor;
begin
  Result := TJSONFalse.Create;
end;

{ TJSONArray }

constructor TJSONArray.Create;
begin
  inherited Create;
  FElements := TList<TJSONValue>.Create;
end;

constructor TJSONArray.Create(const FirstElem: TJSONValue);
begin
  Create;
  AddElement(FirstElem);
end;

constructor TJSONArray.Create(const FirstElem: TJSONValue; const SecondElem: TJSONValue);
begin
  Create;
  AddElement(FirstElem);
  AddElement(SecondElem);
end;

constructor TJSONArray.Create(const FirstElem: string; const SecondElem: string);
begin
  Create;
  AddElement(TJSONString.Create(FirstElem));
  AddElement(TJSONString.Create(SecondElem));
end;

destructor TJSONArray.Destroy;
var
  Element: TJSONAncestor;
  I: Integer;
begin
  if FElements <> nil then
  begin
    for I := 0 to FElements.Count - 1 do
    begin
      Element := TJSONAncestor(FElements[I]);
      if Element.GetOwned then
        Element.Free;
    end;
    FreeAndNil(FElements);
  end;
  inherited Destroy;
end;

procedure TJSONArray.SetElements(const AList: TList<TJSONValue>);
begin
  FElements.Free;
  FElements := AList;
end;

function TJSONArray.Size: Integer;
begin
  Result := GetCount;
end;

function TJSONArray.Get(const Index: Integer): TJSONValue;
begin
  Result := Items[Index];
end;

function TJSONArray.GetCount: Integer;
begin
  if (FElements = nil) or (FElements.Count = 0) then
    Exit(0);
  Result := FElements.Count;
end;

function TJSONArray.GetValue(const Index: Integer): TJSONValue;
begin
  if (Index < 0) or (Index >= Count) then
    Exit(nil);
  Result := TJSONValue(FElements[Index]);
end;

function TJSONArray.FindValue(const APath: string): TJSONValue;
begin
  Result := FindJSONValue(Self, APath);
end;

procedure TJSONArray.AddDescendant(const Descendant: TJSONAncestor);
begin
  FElements.Add(TJSONValue(Descendant));
end;

function TJSONArray.Pop: TJSONValue;
var
  Value: TJSONValue;
begin
  Value := TJSONValue(FElements[0]);
  FElements.Remove(FElements[0]);
  Result := Value;
end;

function TJSONArray.Remove(Index: Integer): TJSONValue;
begin
  Result := GetValue(Index);
  if (Index >= 0) and (Index < Count) then
     FElements.Remove(FElements[Index]);
end;

procedure TJSONArray.AddElement(const Element: TJSONValue);
begin
  if Element <> nil then
    AddDescendant(Element);
end;

function TJSONArray.Add(const Element: string): TJSONArray;
begin
  AddElement(TJSONString.Create(Element));
  Result := Self;
end;

function TJSONArray.Add(const Element: Integer): TJSONArray;
begin
  AddElement(TJSONNumber.Create(Element));
  Result := Self;
end;

function TJSONArray.Add(const Element: Double): TJSONArray;
begin
  AddElement(TJSONNumber.Create(Element));
  Result := Self;
end;

function TJSONArray.Add(const Element: Boolean): TJSONArray;
begin
  if Element then
    AddElement(TJSONTrue.Create)
  else
    AddElement(TJSONFalse.Create);
  Result := Self;
end;

function TJSONArray.Add(const Element: TJSONObject): TJSONArray;
begin
  if Element <> nil then
    AddElement(Element)
  else
    AddElement(TJSONNull.Create);
  Result := Self;
end;

function TJSONArray.Add(const Element: TJSONArray): TJSONArray;
begin
  AddElement(Element);
  Result := Self;
end;

function TJSONArray.EstimatedByteSize: Integer;
var
  Size: Integer;
  I: Integer;
begin
  Size := 1;
  for I := 0 to FElements.Count - 1 do
    Size := Size + (TJSONAncestor(FElements[I])).EstimatedByteSize + 1;
  if Size = 1 then
    Exit(2);
  Result := Size;
end;

function TJSONArray.ToBytes(const Data: TArray<Byte>; const Pos: Integer): Integer;
var
  Offset: Integer;
  Size: Integer;
  I: Integer;
begin
  Offset := Pos;
  Size := FElements.Count;
  Data[IncrAfter(Offset)] := Ord('[');
  if Size > 0 then
    Offset := (TJSONAncestor(FElements[0])).ToBytes(Data, Offset);
  for I := 1 to Size - 1 do
  begin
    Data[IncrAfter(Offset)] := Ord(',');
    Offset := (TJSONAncestor(FElements[I])).ToBytes(Data, Offset);
  end;
  Data[IncrAfter(Offset)] := Ord(']');
  Result := Offset;
end;

function TJSONArray.ToString: string;
var
  Buf: TStringBuilder;
  Size: Integer;
  I: Integer;
begin
  Size := FElements.Count;
  Buf := TStringBuilder.Create;
  try
    Buf.Append('[');
    if Size > 0 then
      Buf.Append(FElements[0].ToString);
    for I := 1 to Size - 1 do
    begin
      Buf.Append(',');
      Buf.Append(FElements[I].ToString);
    end;
    Buf.Append(']');
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

function TJSONArray.Clone: TJSONAncestor;
var
  Data: TJSONArray;
  I: Integer;
begin
  Data := TJSONArray.Create;
  for I := 0 to Count - 1 do
    Data.AddDescendant(Items[I].Clone);
  Result := Data;
end;

function TJSONArray.GetEnumerator: TJSONArrayEnumerator;
begin
  Result := TJSONArrayEnumerator.Create(Self);
end;

{ TList<TObject>Enumerator }

constructor TJSONPairEnumerator.Create(const AList: TList<TJSONPair>);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TJSONPairEnumerator.GetCurrent: TJSONPair;
begin
  Result := TJSONPair(FList[FIndex]);
end;

function TJSONPairEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TJSONArrayEnumerator }

constructor TJSONArrayEnumerator.Create(const AArray: TJSONArray);
begin
  inherited Create;
  FIndex := -1;
  FArray := AArray;
end;

function TJSONArrayEnumerator.GetCurrent: TJSONValue;
begin
  Result := FArray.GetValue(FIndex);
end;

function TJSONArrayEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FArray.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TJSONBool }

procedure TJSONBool.AddDescendant(const Descendant: TJSONAncestor);
begin
//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TJSONBool.AsTValue(ATypeInfo: PTypeInfo; out AValue: TValue): Boolean;
begin
  Result := True;
  case ATypeInfo.Kind of
    tkEnumeration:
      Result := StrToTValue(Value, ATypeInfo, AValue);
    tkInteger, tkInt64, tkFloat:
      if FValue then
        AValue := 1
      else
        AValue := 0;
    tkString, tkLString, tkWString, tkUString:
      AValue := Value;
    else
      Result := inherited;
  end;
end;

function TJSONBool.Clone: TJSONAncestor;
begin
  Result := TJSONBool.Create(FValue);
end;

constructor TJSONBool.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

function TJSONBool.EstimatedByteSize: Integer;
begin
  if FValue then
    Result := Length(TrueBytes)
  else
    Result := Length(FalseBytes);
end;

function TJSONBool.ToBytes(const Data: TArray<Byte>; const Offset: Integer): Integer;
begin
  if FValue then
  begin
    Move(TrueBytes[0], Data[Offset], Length(TrueBytes));
    Result := Offset + Length(TrueBytes);
  end
  else
  begin
    Move(FalseBytes[0], Data[Offset], Length(FalseBytes));
    Result := Offset + Length(FalseBytes);
  end;
end;

function TJSONBool.ToString: string;
begin
  Result := Value;
end;

function TJSONBool.Value: string;
begin
  if FValue then
    Result := TrueString
  else
    Result := FalseString;
end;

{ TJSONPathParser }

constructor TJSONPathParser.Create(const APath: string);
begin
  FPath := APath;
end;

procedure TJSONPathParser.EatWhiteSpaces;
begin
  while not IsEof and FPath.Chars[FPos].IsWhiteSpace do
    Inc(FPos);
end;

function TJSONPathParser.EnsureLength(ALength: Integer): Boolean;
begin
  Result := (FPos + ALength) < Length(FPath);
end;

function TJSONPathParser.GetIsEof: Boolean;
begin
  Result := FPos >= Length(FPath);
end;

function TJSONPathParser.NextToken: TToken;
var
  IsFirstToken: Boolean;
begin
  IsFirstToken := FPos = 0;
  EatWhiteSpaces;
  if IsEof then
    SetToken(TToken.Eof)
  else
  begin
    case FPath.Chars[FPos] of
      '.':
        // Root element cannot start with a dot
        if IsFirstToken then
          RaiseError(SJSONPathUnexpectedRootChar)
        else
          ParseName;
      '[':
        ParseIndexer;
      else
        // In dot notation all names are prefixed by '.', except the root element
        if IsFirstToken then
          ParseName
        else
          RaiseErrorFmt(SJSONPathUnexpectedIndexedChar, [FPath.Chars[FPos]]);
    end;
    Inc(FPos);
  end;
  Result := FToken;
end;

procedure TJSONPathParser.ParseArrayIndex;
var
  LEndPos: Integer;
  LString: string;
  I: Integer;
begin
  LEndPos := FPath.IndexOf(']', FPos);
  if LEndPos < 0 then
    RaiseError(SJSONPathEndedOpenBracket)
  else
  begin
    LString := Trim(FPath.Substring(FPos, LEndPos - FPos));
    FPos := LEndPos - 1;
    if TryStrToInt(LString, I) then
      SetToken(TToken.ArrayIndex, I)
    else
      RaiseErrorFmt(SJSONPathInvalidArrayIndex, [LString])
  end;
end;

procedure TJSONPathParser.ParseQuotedName(AQuote: Char);
var
  LString: string;
begin
  LString := '';
  Inc(FPos);
  while not IsEof do
  begin
    if (FPath.Chars[FPos] = '\') and  EnsureLength(1) and  (FPath.Chars[FPos + 1] = AQuote) then // \"
    begin
      Inc(FPos);
      LString := LString + AQuote
    end
    else if FPath.Chars[FPos] = AQuote then
    begin
      SetToken(TToken.Name, LString);
      Exit;
    end
    else
      LString := LString + FPath.Chars[FPos];
    Inc(FPos);
  end;
  RaiseError(SJSONPathEndedOpenString);
end;

procedure TJSONPathParser.RaiseError(const AMsg: string);
begin
  RaiseErrorFmt(AMsg, []);
end;

procedure TJSONPathParser.RaiseErrorFmt(const AMsg: string; const AParams: array of const);
begin
  SetToken(TToken.Error);
  raise EJSONPathException.Create(Format(AMsg, AParams));
end;

procedure TJSONPathParser.ParseIndexer;
begin
  Inc(FPos); // [
  EatWhiteSpaces;
  if IsEof then
    RaiseError('Path ended with an open bracket');
  case FPath.Chars[FPos] of
    '"',
    '''':
      ParseQuotedName(FPath.Chars[FPos]);
  else
    ParseArrayIndex;
  end;
  Inc(FPos);
  EatWhiteSpaces;
  if FPath.Chars[FPos] <> ']' then
    RaiseErrorFmt(SJSONPathUnexpectedIndexedChar,[FPath.Chars[FPos]]);
end;

procedure TJSONPathParser.ParseName;
var
  LEndPos: Integer;
  LString: string;
begin
  if FPath.Chars[FPos] = '.' then
  begin
    Inc(FPos);
    if IsEof then
    begin
      SetToken(TToken.Error);
      Exit;
    end;
  end;
  LEndPos := FPath.IndexOfAny(['.', '['], FPos);
  if LEndPos < 0 then
  begin
    LString := FPath.Substring(FPos);
    FPos := Length(FPath) - 1;
  end
  else
  begin
    LString := Trim(FPath.Substring(FPos, LEndPos - FPos));
    FPos := LEndPos - 1;
  end;
  if LString = '' then
    RaiseError(SJSONPathDotsEmptyName)
  else
    SetToken(TToken.Name, LString);
end;

procedure TJSONPathParser.SetToken(const AToken: TToken);
var
  P: Pointer;
begin
  SetToken(AToken, P);
end;

procedure TJSONPathParser.SetToken(const AToken: TToken; const AValue);
begin
  FToken := AToken;
  case FToken of
    TToken.Name:
      FTokenName := string(AValue);
    TToken.ArrayIndex:
      FTokenArrayIndex := Integer(AValue);
  end;
end;

initialization
  JSONFormatSettings := TFormatSettings.Invariant;
end.
