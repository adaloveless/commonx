unit DataDictionary;

interface

uses sharedobject, sysutils, classes;

type
  TDataDefinition = class (TSharedObject)
  private
    FObjectType: ansistring;
    FKey1: ansistring;
    FSubtype: ansistring;
    FTable: ansistring;
    FLink: ansistring;
  public
    property ObjectType: ansistring read FObjectType write FObjectType;
    property Table: ansistring read FTable write FTable;
    property Link: ansistring read FLink write FLink;
    property SubType: ansistring read FSubtype write FSubtype;
    property Key1: ansistring read FKey1 write FKey1;
  end;

  TDataDictionary = class(TSharedObject)
  private
    function GetDefs(sType: ansistring): TDataDefinition;
  protected
    FDefs: TList;
  public
    constructor create; override;
    destructor destroy; override;
    procedure Define(sObjectType: ansistring; sTAble: ansistring; sKEy1: ansistring; sSubType: ansistring; sLink: ansistring);
    property Defs[sType: ansistring]: TDataDefinition read GetDefs;


  end;

var
  dict: TDataDictionary;

implementation

{ TDataDictionary }

constructor TDataDictionary.create;
begin
  inherited;
  FDefs := TList.create;

end;

procedure TDataDictionary.Define(sObjectType, sTAble, sKEy1, sSubType,
  sLink: ansistring);
var
  dd: TDataDefinition;
begin

  dd := TDataDefinition.create;
  dd.ObjectType := sObjectType;
  dd.Table := sTable;
  dd.SubType := sSubType;
  dd.Link := sLink;
  dd.Key1 := sKey1;
  Fdefs.add(dd);

end;

destructor TDataDictionary.destroy;
begin
  FDefs.free;
  inherited;
end;

function TDataDictionary.GetDefs(sType: ansistring): TDataDefinition;
var
  dd: TDataDefinition;
  t: integer;
begin
  result := nil;

  for t:= 0 to FDefs.count-1 do begin
    dd := TDataDefinition(FDefs[t]);
    if lowercase(dd.objecttype) = lowercase(sType) then begin
      result := dd;
      break;
    end;
  end;

end;


initialization
  dict := TDatadictionary.create;

finalization
  dict.free;



end.
