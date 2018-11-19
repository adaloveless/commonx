unit iniFile;

interface

uses sysutils, classes, stringx, Exceptions;

type
  EIniSectionNotFound = class(EClassException);
  TMothershipIniSection = class;

  TMothershipIniFile = class
    //Digital Tundra's own INI file class does not use the shotty API calls for Windows
    //but instead reads directly from INI files or even directly from Memory.
    //This class became a necessity when we needed to Parse INI files that
    //came across HTTP and never hit the disk.
    //Used mainly by the AICC integraion.  Could use some improvement as it
    //doesn't support default values.
  private
    function GetFileContents: string;
    function GetSections(sName: string): TMotherShipIniSection;
    procedure SetFileContents(const Value: string);
    function GetSectionCount: integer;
    function GetSectionsByIndex(idx: integer): TMotherShipIniSection;
  protected
    FFileContents: TStringList;
    FSections: TList;
    procedure Parse;
    function AddSection(sName: string): TMotherShipIniSection;
    procedure DeleteSection(idx: integer);
  public
    constructor Create; reintroduce; virtual;


    procedure LoadFromFile(sFileName: string);
    procedure ProcessFileContents;        

    property FileContents: string read GetFileContents write SetFileContents;
    //property representing the entire INI file contents.The contents of the file will
    //automatically be parsed when this value is set.
    property Sections[sName: string]: TMotherShipIniSection read GetSections; default;
    //Returns a class instance reference that handles a given section name in the INI.
    property SectionsbyIndex[idx: integer]: TMotherShipIniSection read GetSectionsByIndex;
    //Returns a class instance reference that handles a given section indexed by order in the file.

    property SectionCount: integer read GetSectionCount;
    //Returns the number of sections in the INI file.

    function ReadString(sSection, sKey: string; sDefault: string = ''): string;

  end;

  TMotherShipIniSection = class
  protected
    FNames: TStringList;
    FValues: TStringList;
    FName: string;
    function GetNames(idx: integer): string;

    function GetValues(sName: string): string;
    procedure SetValues(sName: string; const Value: string);
    function GetValueNames(idx: integer): string;
    procedure SetValuesByIndex(idx: integer; const Value: string);
    function GetValueCount: integer;
    function GetValuesByindex(idx: integer): string;
  public
    constructor Create; reintroduce; virtual;
    property Name: string read FName write FName;
    //Name of the section.
    property ValueNames[idx: integer]: string read GetValueNames;
    //Array property of the parameter names under the section, indexed by order in file.
    property Values[sName: string]: string read GetValues write SetValues; default;
    //Array property of the values in the section, indexed by name
    property ValueCount: integer read GetValueCount;
    //Returns the number of parameters in the section.
    property ValuesByIndex[idx: integer]: string read GetValuesByindex write SetValuesByIndex;
    //Array property of all values in the section by index (order in file)

    procedure AddValue(sValueName, sValue: string);
    function HasValue(sValueName: string): boolean;
    function IndexOfValue(sValueName: string): integer;

    function GetString: string;

  end;

implementation

{ TMotherShipIniSection }

//------------------------------------------------------------------------------
procedure TMotherShipIniSection.AddValue(sValueName, sValue: string);
//Adds a value to the section in ... used when parsing, or when generating
//an INI file.
begin
  FNames.add(sValueName);
  FValues.add(sValue);

end;

//------------------------------------------------------------------------------
constructor TMotherShipIniSection.Create;
//standard.
begin
  inherited;
  FNames := TStringList.create;
  FValues := TStringList.create;
end;

//------------------------------------------------------------------------------
function TMotherShipIniSection.GetNames(idx: integer): string;
//Getter for the names property.
begin
  result := FNames[idx];
end;

//------------------------------------------------------------------------------
function TMotherShipIniSection.GetString: string;
//Returns the section as a single string.
var
  t: integer;
begin
  result :='['+self.Name+']'+#13#10;
  for t:= 0 to self.valuecount-1 do begin
    result := result+ FNames[t]+'='+FValues[t]+#13#10;
  end;

end;
//------------------------------------------------------------------------------
function TMotherShipIniSection.GetValueCount: integer;
//Getter for the value count property. 
begin
  result := FValues.count;
end;
//------------------------------------------------------------------------------
function TMotherShipIniSection.GetValueNames(idx: integer): string;
//Getter for the value names property.
begin
  result := FNames[idx];

end;
//------------------------------------------------------------------------------
function TMotherShipIniSection.GetValues(sName: string): string;
//Getter for the values property.
var
  i: integer;
begin
  i := FNames.IndexOf(sName);
  if i=-1 then
    result := ''
  else
    result := FValues[i];

end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
function TMotherShipIniSection.GetValuesByindex(idx: integer): string;
//Getter for the valuesbyIndex property.
begin
  result := FValues[idx];
end;

function TMotherShipIniSection.HasValue(sValueName: string): boolean;
begin
  result := IndexOfValue(sValueName) >= 0;
end;

function TMotherShipIniSection.IndexOfValue(sValueName: string): integer;
begin
  result := FNames.IndexOF(sValueName);

end;

function TMotherShipIniFile.ReadString(sSection, sKey,
  sDefault: string): string;
var
  s: TMothershipINISection;
begin
  try
    s := sections[sSection];
  except
    s := nil;
  end;

  if s=nil then
    result := sDefault
  else
  if s.HasValue(sKey) then
    result := s.Values[sKey]
  else
    result := sDefault;


end;

procedure TMotherShipIniSection.SetValues(sName: string; const Value: string);
//p: sName: the Value name in the section.
//p: Value: The new value.
//Setter for the Values property.
//Sets a value in an INI section.
var
  i: integer;
begin
  i := FNames.IndexOf(sName);
  if i=-1 then
    AddValue(sName, value)
  else
    FValues[i] := value;

end;

{ TMotherShipIniFile }

//------------------------------------------------------------------------------
function TMotherShipIniFile.AddSection(sName: string): TMotherShipIniSection;
//adds a section to the INI file and returns a reference to the section object
begin
  result := TMotherShipIniSection.create;
  result.Name := sName;
  FSections.add(result);
end;
//------------------------------------------------------------------------------
constructor TMotherShipIniFile.Create;
//Standard.
begin
  inherited Create;
  FSections := TList.create;
  FFileContents := TStringList.create;

end;
//------------------------------------------------------------------------------
procedure TMotherShipIniFile.DeleteSection(idx: integer);
//deletes and frees a section from the INI
var
  tempSection: TMotherShipIniSection;
begin
  tempSection := TMotherShipIniSection(FSections[idx]);
  tempSection.free;
  FSections.Delete(idx);
end;
//------------------------------------------------------------------------------
function TMotherShipIniFile.GetFileContents: string;
//Getter for the file contents property.
var
  t: integer;
begin
  result := '';
  for t:= 0 to FSections.count-1 do begin
    result := result+SectionsByIndex[t].GetString+#13#10;
  end;
end;
//------------------------------------------------------------------------------
function TMotherShipIniFile.GetSectionCount: integer;
//Getter for the section count property.
begin
  result := FSections.count;
end;
//------------------------------------------------------------------------------
function TMotherShipIniFile.GetSections(sName: string): TMotherShipIniSection;
//Getter for the sections property. Returns a TMotherShipINIsection class for the
//desired section name.  There is one TMotherShipINIseciton instance for every
//section in the INI file.
var
  t: integer;
  curSect: TMotherShipINISection;
begin
  result := nil;
  for t:=0 to FSections.count-1 do begin
    curSect := TMotherShipIniSection(FSections[t]);
    if lowercase(cursect.name) = lowercase(sName) then begin
      result := cursect;
      break;
    end;
  end;

  if result = nil then
    raise EIniSectionNotFound.create('Section '+sName+' not found');
end;
//------------------------------------------------------------------------------
function TMotherShipIniFile.GetSectionsByIndex(idx: integer): TMotherShipIniSection;
//Getter for the sectionsByIndex property. Returns a TMotherShipINIsection class for the
//desired section index (order in the file).  There is one TMotherShipINIseciton instance for every
//section in the INI file.
begin
  result := TMotherShipIniSection(FSections[idx]);
end;

procedure TMotherShipIniFile.LoadFromFile(sFileName: string);
//p: sFileName: The File to load.
//Loads INI file date from disk.
var
  sl : TStringList;
begin
  sl := TStringList.create;
  try
    sl.LoadFromFile(sFileName);
    self.FileContents := sl.text;
  finally
    sl.free;
  end;
end;

procedure TMotherShipIniFile.Parse;
//Parses the ini file and breaks the file into INI sections with values.
//This is automatically called when the contents of the INI are loaded from
//disk or the FileContents property is set to something from memory.
var
  sLine: string;
  curSection: TMotherShipIniSection;
  t: integer;
  sLeft, sRight: string;
begin
  curSection := self.AddSection('');
  //scan each line of the file
  for t:= 0 to FFileContents.count-1 do begin
    //set the current line variable
    sLine := Trim(FFileContents[t]);
    //if the line is blank or a comment then continue
    if (sLine = '') or (sLine[1]=';') then
      continue
    else
    //if the line starts with [ then treat as a section
    if sLine[1]='[' then begin
      sRight := copy(sLine, 2, pos(']',sLine)-2);
      //add the section name... everything up to the next ]
      curSection := self.AddSection(sRight)
    end else
    //if the line starts with anything else.. and contains a '=' then
    if pos('=', sLine) > -1 then begin
      //separate the name and value
      SplitString(sLine, '=', sLeft, sRight);
      //add as name value paire
      if assigned(curSection) then
        curSection.AddValue(sLeft, sRight);
    end;
  end;
end;

procedure TMothershipIniFile.ProcessFileContents;
var
  sFile, sLeft, sRight: string;
  ini: TMotherShipINIFile;
  sInner: string;
begin
  while SplitString(FFileContents.text,'{{{', sLeft, sRight) do begin
    //split at }}}
    SplitString(sRight, '}}}', sFile, sRight);

    //load file name found in the middle
    ini := TMothershipINIFile.Create;
    try
      ini.LoadFromFile(sFile);
      sInner := ini.FileContents;
    finally
      ini.free;
    end;
    //URL encode the file
    sInner := StringToHex(sInner);

    //put the file back together
    FFileContents.text := sLeft+sInner+sRight;

  end;
end;

procedure TMotherShipIniFile.SetFileContents(const Value: string);
//Setter for the file contents property. The contents of the file will
//automatically be parsed when this value is set.
begin
  FFileContents.text := value;
  self.ProcessFileContents;
  Parse;
end;

procedure TMotherShipIniSection.SetValuesByIndex(idx: integer;
  const Value: string);
//Setter fo rhte values by index property.
begin
  FValues[idx] := value;
end;

end.
