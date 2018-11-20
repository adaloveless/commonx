unit osm_to_sql;
dont use me
interface

uses
  sysutils, commandprocessor, xmltools, systemx, typex, classes, stringx, commandicons;
type
  TCurrentNode = record
    id: int64;
    lat: double;
    lon: double;
    visible: boolean;
    procedure Init;
    function GetSQLValues: string;
  end;
  Tcmd_InjectChangeSet = class(TCommand)
  private
    slTemp: TStringLIst;
    slQueries: TStringLIst;
    slNodeMods, slNodeCreates: TStringlist;
    FxData: TXMLDocument;
    currentNode: TCurrentnode;
    procedure CommitNodes;
    procedure HandleOsmChangeElement(elem: TXMLElement);
    procedure HandleElement(elem: TXMLElement);
    procedure HandleSubElement(sCommand: string; elem: TXMLElement);
    procedure HandleWayElement(sCommand: string; elem: TXMLElement);
    procedure HandleNodeElement(sCommand: string; elem: TXMLElement);
    procedure WriteQuery(sQuery: string);
    procedure GetAttributeToTempIgnoreNulls(elem: TXMLElement;
      sAttribute: string; sdefault: string);
    procedure ClearTemp;
    function GetTempForQuery: string;
    function GetTempForDuplicates: string;
    function GEtResult: string;
  public
    parse: boolean; // flags to indicate which stage we're in
    write: boolean;
    constructor Create;override;
    destructor Destroy;override;

    procedure InitExpense; override;
    procedure DoExecute; override;
    property xData: TXMLDocument read FxData write FxData;
    property result: string read GEtResult;
  end;

implementation

uses
  worldvis;

procedure Tcmd_InjectChangeSet.ClearTemp;
begin
  slTemp.Clear;
end;

procedure Tcmd_InjectChangeSet.CommitNodes;
begin
  if slNodeCReates.count > 0 then begin
    WriteQuery('Insert ignore into nodes values '+unparsestring(',',slNodeCreates));
  end;
  slNodeCreates.clear;
end;

constructor Tcmd_InjectChangeSet.Create;
begin
  inherited;
  slQueries := TStringLIst.Create;
  slNodeMods := TStringlist.create;
  slNodeCreates := TStringlist.create;

end;

destructor Tcmd_InjectChangeSet.Destroy;
begin
  slnodemods.free;
  slnodecreates.free;
  slQueries.free;
  inherited;
end;

procedure Tcmd_InjectChangeSet.DoExecute;
begin
  inherited;
  slTemp := TStringLIst.Create;
  try
    if xData = nil then
      exit;
    // xmld := TXMLDocument.create;
    try
      // xmld.Value := sData;
      if xData.HasElement('osmChange') then
      begin
        HandleOsmChangeElement(xData.Elements['osmChange', 0]);
      end;
    finally
      // xmld.free;
    end;
  finally
    xData.free;
    slTemp.free;
    slTemp := nil;
  end;
end;

procedure Tcmd_InjectChangeSet.GetAttributeToTempIgnoreNulls(elem: TXMLElement;
  sAttribute, sdefault: string);
var
  sPoop: string;
begin
  sPoop := elem.Attributes.GetItemEx(sAttribute, sdefault);
  if sPoop <> '' then
    slTemp.Add(sAttribute + '=' + sPoop);
end;

function Tcmd_InjectChangeSet.GEtResult: string;
begin
  result := slQueries.Text;
end;

function Tcmd_InjectChangeSet.GetTempForDuplicates: string;
var
  t: ni;
  sl,sr: string;
begin
  result := '';
  for t:= 0 to slTemp.count-1 do begin
    if splitstring(slTemp[t], '=', sl,sr) then begin
      if result = '' then
        result := 'on duplicate key update '
      else
        result := result + ',';

      result := result + sl+'=values('+sl+')';

    end;

  end;

end;

function Tcmd_InjectChangeSet.GetTempForQuery: string;
begin
  result := UnParseString(',', slTemp);
end;

procedure Tcmd_InjectChangeSet.HandleElement(elem: TXMLElement);
var
  sCommand: string;
  t: ni;
  inner: TXMLElement;
begin
  // save the outer element name, we will use this to determine db op
  sCommand := elem.ElementName;
  for t := 0 to elem.elementcount - 1 do
  begin
    inner := elem.ElementsByIndex[t];
    if inner.ElementName = 'node' then
      HandleNodeElement(sCommand, inner);
    if inner.ElementName = 'way' then
      HandleWayElement(sCommand, inner);
  end;
end;

procedure Tcmd_InjectChangeSet.HandleNodeElement(sCommand: string;
  elem: TXMLElement);
var
  sID: string;
  wvp: TWorldVisPoint;
begin
  ClearTemp;
  if sCommand = 'delete' then
    WriteQuery('delete from nodes where id=' + elem.Attributes['id'].Value);
  if (sCommand = 'create') then
  begin
    CurrentNode.Init;
    currentnode.lat := elem.attributes.GetItemEx('lat', 0.0);
    currentnode.lon := elem.attributes.GetItemEx('lon', 0.0);
    currentnode.visible := elem.attributes.GetItemEx('visible', true);
    currentnode.id := elem.attributes.GetItemEx('id', 0);

    GetAttributeToTempIgnoreNulls(elem, 'lat', '');
    GetAttributeToTempIgnoreNulls(elem, 'lon', '');
    GetAttributeToTempIgnoreNulls(elem, 'visible', '1');
    GetAttributeToTempIgnoreNulls(elem, 'id', '');
    slNodeCreates.add(currentnode.GetSQLValues);
    //slNodeCReates.add('insert ignore into nodes set ' + GetTempForQuery+','+GetTempForDuplicates);
  end;
  if sCommand = 'modify' then
  begin
    GetAttributeToTempIgnoreNulls(elem, 'lat', '');
    GetAttributeToTempIgnoreNulls(elem, 'lon', '');
    GetAttributeToTempIgnoreNulls(elem, 'visible', '1');
    // GetAttributeToTempIgnoreNulls(elem,'id','');
    sID := elem.Attributes.GetItemEx('id', '0');
    WriteQuery('update ignore nodes set ' + GetTempForQuery +' where id =' + sID);
  end;
  wvp.y := elem.Attributes.GetItemEx('lat', 0.0);
  wvp.x := elem.Attributes.GetItemEx('lon', 0.0);
  wring.Inject(wvp);
end;

procedure Tcmd_InjectChangeSet.HandleOsmChangeElement(elem: TXMLElement);
var
  inner: TXMLElement;
  t: ni;
begin
  STepCount := elem.elementcount;
  for t := 0 to elem.elementcount - 1 do
  begin
    step := t;
    inner := elem.ElementsByIndex[t];
    HandleElement(inner);
  end;
  CommitNodes;
end;

procedure Tcmd_InjectChangeSet.HandleSubElement(sCommand: string;
  elem: TXMLElement);
begin
  if elem.ElementName = 'way' then
    HandleWayElement(sCommand, elem);
  if elem.ElementName = 'node' then
    HandleNodeElement(sCommand, elem);
end;

procedure Tcmd_InjectChangeSet.HandleWayElement(sCommand: string;
  elem: TXMLElement);
var
  slRefs: TStringLIst;
  slTags: TStringLIst;
var
  t: ni;
  iCount: ni;
  sID: string;
  inner: TXMLElement;
  sTemp: string;
  slNotIn: TStringlist;
begin
  ClearTemp;
  slRefs := TStringLIst.Create;
  slTags := TStringLIst.Create;
  slNotIn := tStringlist.create;
  try
    sID := elem.Attributes['id'].Value;
    if (sCommand = 'delete') or (sCommand = 'modify') then
    begin
      WriteQuery('delete from ways where id=' + sID);
{$IFDEF FULL_DELETE}
      WriteQuery('delete from way_tags where id=' + sID);
      WriteQuery('delete from way_nodes where id=' + sID);
{$ENDIF}
    end;
    if (sCommand = 'modify') or (sCommand = 'create') then
    begin
      for t := 0 to elem.GetElementNameCount('nd') - 1 do
      begin
        slRefs.Add('(' + sID + ',' + elem.Elements['nd', t].Attributes['ref']
          .Value + ','+inttostr(t)+')');
        slnotIn.add(elem.Elements['nd', t].Attributes['ref'].value);
      end;
{$IFNDEF FULL_DELETE}
      if slNotIn.count > 0 then
        WriteQuery('delete from way_nodes where id='+sID+' and (!(node_id in ('+unparsestring(',',slNotIn)+')))');
{$ENDIF}
      slNotIn.clear;

      for t := 0 to elem.GetElementNameCount('tag') - 1 do
      begin
        inner := elem.Elements['tag', t];
        slTags.Add('(' + sID + ',' + vartomysqlstorage(inner.Attributes['k']
          .Value) + ',' + vartomysqlstorage(inner.Attributes['v']
          .Value) + ',0)');
        slNotIn.add(vartomysqlstorage(inner.Attributes['k'].value));
      end;
{$IFNDEF FULL_DELETE}
      if slNotIn.count > 0 then
        WriteQuery('delete from way_tags where id='+sID+' and (!(k in ('+unparsestring(',',slNotIn)+')))');
{$ENDIF}
      slNotIn.clear;

      GetAttributeToTempIgnoreNulls(elem, 'id', '0');
      GetAttributeToTempIgnoreNulls(elem, 'visible', '1');
      WriteQuery('insert ignore into ways set ' + GetTempForQuery);

      if slRefs.count > 0 then
        WriteQuery('insert ignore into way_nodes (id, node_id, sortorder) values ' +
          UnParseString(',', slRefs)+' on duplicate key update sortorder = values(sortorder)');
      if slTags.count > 0 then
        WriteQuery('insert ignore into way_tags (id, k,v,version) values ' +
          UnParseString(',', slTags)+' on duplicate key update v= values(v)');
    end;
  finally
    slRefs.free;
    slTags.free;
    slNotIn.free;
    slRefs := nil;
    slTags := nil;
  end;
end;

procedure Tcmd_InjectChangeSet.InitExpense;
begin
  inherited;
  CpuExpense := 0.0;
  icon := @cmd_icon_crunch;
end;

procedure Tcmd_InjectChangeSet.WriteQuery(sQuery: string);
begin
  slQueries.Add(sQuery + CRLF + '--execute--');
end;

{ TCurrentNode }

function TCurrentNode.GetSQLValues: string;
begin
  result := '('+inttostr(id)+','+floattostr(lat)+','+floattostr(lon)+','+inttostr(booltoint(visible))+',0)';
end;

procedure TCurrentNode.Init;
begin
  visible := true;
  lat := 0;
  lon := 0;
  id := 0;
end;

end.
