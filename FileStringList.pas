unit FileStringList;
//Contains the TFileStringList class.

interface
uses classes;
type
  TFileStringList = class(TStringList)
  //I really have no idea why anyone would make this.  Its very easy to just
  //call TStringList.LoadFromFile.  But I guess using this class eliminates ONE line of
  //code by allowing you to create the ansistringlist AND load from file with one line in the
  //constructor.
  public
    constructor CreateFromFile(sFile: ansistring);

  end;

implementation

constructor TFileStringList.CreateFromFile(sFile:ansistring);
//This class's constructor is reintroduced to load the ansistring list from the file
//upon construction.
begin
  inherited create;
  self.LoadFromFile(sFile);

end;

end.
