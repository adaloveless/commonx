unit DirFile;
//This unit contains the TFileInformation class, it usually compliments the dir.pas
//unit and the TDirectory class, however,  it is sufficiently generic enough to be
//reusable in other situations.

interface
uses sysutils, classes, filefind, helpers.stream, numbers,
{$IFDEF WINDOWS}
    winapi.windows,
{$ENDIF}
  system.ioutils, Debug, memoryfilestream;

type
TAdvancedFileChecksum = packed record
  bytesum: byte;
  bytexor: byte;
  bytecount:int64;
  procedure Calculate(sFile: string);
  class operator equal(a,b: TAdvancedFileChecksum): boolean;
end;

TFileInformation = class(Tobject)
  //The TFileInformation Class is typically contained in the TDirectory class.
  //TFileInformation represents information about a file on a disk.  TDirectory
  //will typically automatically create an instance of TFileInformation to represent
  //files in a folder, or folders within a folder.
  private
    FIsFolder: boolean;
    FUniversalAttributes: integer;
    procedure SetAtributes(const Value: TFileAttributes);
  protected
    FSize: int64;
    FFullName: string;
    FFileDate: TDateTime;
    FAttributes: TFileAttributes;
    bAttributesFetched: boolean;
    function GetName: string;
    procedure SetName(str: string);
    function getTypeDescription: string;
    function getExtension: string;
    function GetPath: string;
    procedure SetPath(str: string);
    function GetDate: TDateTime;
    procedure SetDate(dt: TDateTime);
    procedure SetSize(i: int64);
    function GetSize: int64;
    function GetNamePart: string;
    procedure SetNamePart(str: string);
    procedure SetExtension(str: string);
    function GetAttributes: TFileAttributes;
    function GetfullName: string;
    procedure SetFullName(str: string);
  public
    constructor Create;reintroduce; virtual;
    procedure Invalidate;
    procedure Refresh;
    property Path: string read GetPath write SetPath;
    //The path where the file is located.
    property Attributes: TFileAttributes read GetAttributes write SetAtributes;
    //Attributes of the file, e.g. Read-Only, Hidden, System, ARchive...etc.
    //See OS documentation for more info.  If WRITING to this property, the attributes for the file on disk WILL BE MODIFIED!
    property TypeDescription: string read GetTypeDescription;
    //Retrieves a friendly name for the file type from the operating system.
    //For example, Windows, may return "Application" for files with extension
    //".EXE"
    property Extension: string read GetExtension write SetExtension;
    //Returns the Extension of the file.
    property Name: string read GetName write SetName;
    //Returns the name, excluding the fully qualifying path.
    property NamePart: string read GetNamePart write SetNamePart;
    //Return the name, excluding the fully qualitying path and the Extension.
    property FullName: string read GetFullName write setFullName;
    //Returns the fully qualified name of the file, including path and extension.

    property Size: int64 read GetSize write SetSize;
    //Returns the Size of the file.
    property Date: TDateTime read GetDate write SetDate;
    //Returns the date-time stamp of the file in Pascal datetime format.

    function IsCompressed: boolean;
    property IsFolder: boolean read FIsFolder write FIsFolder;
    procedure LoadFromFile(sFilestring: string);
    procedure RefreshUniversalAttributes;
    property UniversalAttributes: integer read FUniversalAttributes write FUniversalAttributes;
end;
function LastPos(sub,s : string):integer;
function GetFileSize(sFile: string): int64;

implementation
uses
  dir;

function GetFileSize(sFile: string): int64;
var
  fil: TFileInformation;
begin
  result := 0;
  try
    fil := TFileInformation.Create;
    try
      fil.LoadFromFile(sFile);
      result := fil.Size;
    finally
        fil.Free;
    end;
  except
    result := 0;
  end;
end;

{------------------------------------------------------------------------------}
function TfileInformation.GetName: string;
//Getter for the name property.
begin
  result:=ExtractFileName(fullname);
end;
{------------------------------------------------------------------------------}
procedure TfileInformation.SetName(str: string);
//Setter for the name property... does not actually change the file on disk.
begin
  fullname:=path+str;
end;
{------------------------------------------------------------------------------}
function TfileInformation.getTypeDescription: string;
//Getter for the TypeDescription property.
begin
  result:='Not implemented';
end;
procedure TFileInformation.Invalidate;
begin
  bAttributesFetched:= false;

end;

function TFileInformation.IsCompressed: boolean;
begin
  {$IFDEF ALLOW_COMPRESSION}
  result := TFileAttribute.faCompressed in Attributes;
  {$ELSE}
  result := false;
  {$ENDIF}

//  result := (Attributes and FILE_ATTRIBUTE_COMPRESSED) > 0;
end;

procedure TFileInformation.LoadFromFile(sFilestring: string);
var
  r: TSearchRec;
  scope: Tfindscope;
begin
  ResetEasyFind(scope);
  if EasyFindfile(scope, sfileString, 0,0, r) then begin
    Size := r.Size;


    FAttributes := FileFind.IntegertoFileAttributes(r.Attr);

    FullName := sfileString;
    try
      Date := r.TimeStamp;
    except
      Debug.Log(self,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'+r.Name+' has a bad date... '+datetimetostr(r.TimeStamp), '');
      Date := now;
    end;
  end;

  CloseEasyFind(scope, r);


end;

procedure TFileInformation.Refresh;
begin
  INvalidate;
  LoadFromFile(FullName);
end;

procedure TFileInformation.RefreshUniversalAttributes;
var
  attr: integer;
begin
  attr := 0;

  if isFolder then
    attr := attr or UFA_FOLDER
  else begin
{$IFDEF MSWINDOWS}
    if TFileAttribute.faHidden in Attributes then
      attr := attr or UFA_HIDDEN;

    if TFileAttribute.faSystem in Attributes then
      attr := attr or UFA_SYSTEM;
    if TFileAttribute.faCompressed in Attributes then
      attr := attr or UFA_COMPRESSED;

    if TFileAttribute.faArchive in Attributes then
      attr := attr or UFA_ARChIVE;
{$ENDIF}
  end;
end;

{------------------------------------------------------------------------------}
function TfileInformation.getExtension: string;
//Getter for the Extension property.
begin
  result:=Extractfileext(fullname);

end;
{------------------------------------------------------------------------------}
function TfileInformation.GetNamePart: string; {verified}
//Getter for the NamePart property.
var
  BeginPos: integer;
  iTemp: integer;
begin
  beginPOs:=lastpos('\',fullname)+1;
  iTemp := lastpos('.',fullname);
  if iTemp < 1 then iTemp := 9999999;
  result:=Copy(fullname,BeginPos, iTemp-beginpos);

end;
{------------------------------------------------------------------------------}
procedure TfileInformation.SetNamePart(str: string);
//Setter for the namepart property.
begin
  fullname:=Path+str+Extension;
end;
{------------------------------------------------------------------------------}
procedure TfileInformation.SetExtension(str: string);
//Setter for the extension property.
begin
  fullname:=Path+NamePart+'.'+str;
end;
{------------------------------------------------------------------------------}
function TFileInformation.GetAttributes: TFileAttributes;
//Getter for the Attributes property.
begin
{$IFDEF MSWINDOWS}
  result := [TFileAttribute.faOffline];
{$ELSE}
  result := [];
{$ENDIF}
  try
    if not bAttributesFetched then
      result := TFile.GetAttributes(self.fullname);
  except
    on E: exception do begin
      Debug.Log('could not read file attributes from '+self.fullname);
    end;
  end;

//  result:=FileGetAttr(fullname);
end;

{------------------------------------------------------------------------------}
function TFileInformation.GetPath: string;
//Getter for the path property.
begin
  result:=ExtractFilePath(fullname);

end;
{------------------------------------------------------------------------------}
procedure TfileInformation.SetPath(str: string);
//Setter for the path property.
begin
  if not (Copy(str,length(str),1)='\') then str:=str+'\';
  fullname:=str+namepart+'.'+extension;

end;
{------------------------------------------------------------------------------}
function Tfileinformation.GetFullName: string;
//GEtter for the FullName property.
begin
  result:=FFullName;
end;
{------------------------------------------------------------------------------}
procedure TfileInformation.setFullName(str:string);
//Setter for the fullname property.
begin
  fFullName:=str;
end;
{------------------------------------------------------------------------------}
function TfileInformation.GetDate: TDateTime;
//Getter for the date property.
var
  iA: integer;
begin
  if Real(FFileDate) <> 0.0 then
    result := FFileDate
  else try
    iA := FileAge(FullName);
    if iA < 0 then iA:= 0;
    result := FileDateToDateTime(iA);
  except
    result := now;
  end;
end;
{------------------------------------------------------------------------------}
procedure TfileInformation.SetDate(dt: TDateTime);
//Setter for the date property.
begin
  FFileDate := dt;
//  raise Exception.create('not implemented');
end;
{------------------------------------------------------------------------------}
function TfileInformation.GetSize: int64;
//Getter for the size property.
begin
  result := fsize;
end;
{------------------------------------------------------------------------------}
procedure TfileInformation.SetSize(i: int64);
//Setter for the size property.
begin
  FSize := i;
end;
{------------------------------------------------------------------------------}
function LastPos(sub,s : string):integer;
//p: sub: The substring you seek.
//p: s: The string you are searching.
//Helper function which returns the position LAST instance of a substring in a string.  If the substring is not found, returns 0.
label 1;
var
  p: integer;
  t: integer;
begin
  p:=0;

  for t:=((length(s))-length(sub))+1 downto 0 do begin
    if copy(s, t, length(sub))=sub then begin
      p:=t;
      goto 1;
    end;
  end;

1: result:=p;
end;

procedure TfileInformation.SetAtributes(const Value: TFileAttributes);
//Setter for the Attributes property.  This will actually change the attrbutes for the file on disk!
//p: Value: New Attrbutes... see OS documentation for valid attribute flags.
begin

  TFile.SetAttributes(fullname, value);
  Invalidate;
//  FileSetAttr(fullname, value);

end;

constructor TfileInformation.CReate;
begin
  inherited;
  FFileDate := 0.0;
end;

{ TAdvancedFileChecksum }

procedure TAdvancedFileChecksum.Calculate(sFile: string);
var
  fs:TMemoryFileStream;
  b: byte;
  a: array [0..7999] of byte;
  iTotalRead: int64;
  t: integer;
  iJustRead: integer;
begin
  bytesum := 0;
  bytexor := 0;
  bytecount := 0;
  fs := TMemoryFileStream.create(sFile, fmOpenRead+fmShareDenyNone);
  try
    fs.seek(0,soBeginning);
    iTotalRead := 0;
    while fs.Position < fs.Size do begin
      iJustRead := Stream_GuaranteeRead(fs, @a[0],lesserof(8000, fs.Size-fs.Position));
      for t:= 0 to iJustRead-1 do begin
        b := a[t];
        bytesum := (cardinal(bytesum )+ cardinal(b)) mod 255;
        bytexor := bytexor xor b;
        inc(bytecount);
      end;

    end;
  finally
    fs.free;
  end;

end;

class operator TAdvancedFileChecksum.equal(a,
  b: TAdvancedFileChecksum): boolean;
begin
  result := (a.bytesum = b.bytesum) and (a.bytexor = b.bytexor) and (a.bytecount = b.bytecount);

end;

end.
