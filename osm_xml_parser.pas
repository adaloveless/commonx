unit osm_xml_parser;

interface

uses
  typex, xmltools, systemx, classes, stringx, commands_system, sysutils, memoryfilestream, helpers.stream, abstractdb;

type
  TnodeData = record
    id: int64;
    lon,lat: double;
    procedure WriteToStream(s: TStream);
    procedure ReadFromSTream(s: TSTream);
  end;

  PNodeData = ^TnodeData;

  TWayTag = record
    k: string;
    v: string;
    procedure WriteToStream(s: TStream);
    procedure ReadFromSTream(s: TSTream);
  end;
  PWayTAg = ^TWayTAg;

  TWayData = record
    id: int64;
    nodes: array of TNodeData;
    tags: array of TWayTag;
    function IndexOfTag(sTag: string): ni;
    function HasTag(sTag: string): boolean;
    function GetTagValue(sTag: string; sDefault: string = ''): string;
    procedure WriteToStream(s: TStream);
    procedure ReadFromSTream(s: TSTream);
    function AddNode(id: int64): PNodeData;
    procedure AddTag(const k,v: string);
  end;

  PWayDAta = ^TWAyData;

  TXMLDrawWorkingData = record
    nodes: array of TnodeData;
    ways: array of TWayDAta;
    function AddWay(id: int64): PWayData;
    procedure FRomFile(sFile: string);
    procedure FromDataBAse(db: TAbstractDB; w,s,e,n: double);
    procedure FRomBinFile(sBin: string);
    procedure ToBinFile(sBin: string);
    function FindNode(id: int64): PNodeData;
    function FindWay(id: int64): PWAyData;

    procedure WriteToStream(s: TStream);
    procedure ReadFromSTream(s: TSTream);



  end;

implementation

{ TXMLDrawWorkingData }

function TXMLDrawWorkingData.AddWay(id: int64): PWayDAta;
var
  way: TWAyDAta;
begin
  setlength(ways, length(ways)+1);
  way.id := id;
  ways[high(ways)] := way;
  result := @ways[high(ways)];

end;

function TXMLDrawWorkingData.FindNode(id: int64): PNodeData;
var
  t: ni;
begin
  result := nil;
  for t:= low(nodes) to high(nodes) do begin
    if nodes[t].id = id then begin
      result := @nodes[t];
      exit;
    end;
  end;



end;

function TXMLDrawWorkingData.FindWay(id: int64): PWAyData;
var
  t: ni;
begin
  result := nil;
  for t:= low(ways) to high(ways) do begin
    if ways[t].id = id then begin
      result := @ways[t];
      exit;
    end;
  end;



end;

procedure TXMLDrawWorkingData.FRomBinFile(sBin: string);
var
  str: TMemoryFileStream;
begin
  str := TMemoryFileStream.create(sBin, fmOpenRead+fmShareDenyNone);
  try
    str.BufferSize := 256000;
    str.MinimumPrefetchSize := 2000000;
    ReadFromSTream(str);


  finally
    str.free;
  end;

end;



procedure TXMLDrawWorkingData.FromDataBAse(db: TAbstractDB; w, s, e, n: double);
var
  cnodes: TAbstractDBCursor;
  cways: TAbstractDBCursor;
  cwaytags: TAbstractDBCursor;
  t: ni;
  sWAyIn: TStringlist;
  q: string;
  way: PWayData;
  node: PNodeDAta;
  lid: ni;
begin
  sWAyIn:= TSTringlist.Create;
  try
    cnodes := db.ReadQueryDBC('select * from nodes where (lat >= '+floattostr(s)+') and (lat <= '+floattostr(n)+') and (lon >='+floattostr(w)+') and (lon <= '+floattostr(e)+')');
    setlength(nodes, cnodes.CountRecords);
    t := 0;
    while not cnodes.EOF do begin
      nodes[t].id := cnodes['id'];
      nodes[t].lon := cnodes['lon'];
      nodes[t].lat := cnodes['lat'];
      sWAyIn.Add(inttostr(nodes[t].id));
      inc(t);
      cnodes.Next;
    end;
    //------------------------------------------------------
    if length(nodes) > 0 then begin
      q := 'select * from way_nodes join nodes on (way_nodes.node_id=nodes.id) where node_id in ('+unparsestring(',',sWAyin)+') order by way_nodes.id';
      cways := db.readquerydbc(q);
      t := 0;
      sWAyIn.Clear;
      while not cways.EOF do begin
        lid := cways['id'];
        way := findway(lid);
        if way = nil then begin
          way := addWay(lid);
          sWAyIn.add(inttostr(lid));
        end;


        node := way.addnode(cways['node_id']);
        node.lon := cways['lon'];
        node.lat := cways['lat'];
        inc(t);
        cways.Next;
      end;
    end;
    if sWayIn.Count > 0 then begin
      //--------------------------------
      q := 'select * from ways join way_tags on (ways.id=way_tags.id) where ways.id in ('+unparsestring(',',sWAyin)+')';
      cways := db.readquerydbc(q);
      t := 0;
      while not cways.EOF do begin
        lid := cways['id'];
        way := findway(lid);
        if way = nil then
          way := addWay(lid);

        way.AddTag(cways['k'], cways['v']);

        inc(t);
        cways.next;
      end;
    end;


  finally
    sWayIn.free;
  end;


end;

procedure TXMLDrawWorkingData.FRomFile(sFile: string);
var
  xml: TXMLDocument;
  iosmcount: ni;
  t,u,v,w: ni;
  eOSM, eNode,  eWay, eNdRef, eTag: TXMLElement;
  way: PWayData;
begin
  if extractfileext(sFile) = '.bin' then begin
    FromBinFile(sFile);
  end else begin
    xml := TXMLDocument.create;
    try
      try
        xml.LoadFromFile(sFile);
      except
        deletefile(sFile);
        exit;
      end;
      iOsmCount := xml.GetElementNameCount('osm');
      for t:= 0 to iOsmCount-1 do begin
        eOSM := xml.Elements['osm',0];

        //nodes
        setlength(self.nodes, eOSM.GetElementNameCount('node'));
        for u := 0 to high(nodes) do begin
          eNode := eOSM.Elements['node',u];
          nodes[u].id := eNode.Attributes['id'].AsInteger;
          nodes[u].lon := eNode.Attributes['lon'].AsFloat;
          nodes[u].lat := eNode.Attributes['lat'].AsFloat;
        end;

        //ways
        setlength(self.ways, eOSM.GetElementNameCount('way'));
        for u := 0 to high(ways) do begin
          eWAy := eOSM.Elements['way',u];
          way := @ways[u];

          way.id := eWay.Attributes['id'].AsInteger;

          setlength(way.nodes, eWay.GetElementNameCount('nd'));
          //node references from ways
          for v := 0 to high(way.nodes) do begin
            eNdRef := eWay.elements['nd',v];
            way.nodes[v] := self.FindNode(endRef.Attributes['ref'].AsInteger)^;
          end;

          //way tags
          setlength(way.tags, eWay.GetElementNameCount('tag'));
          for v := 0 to high(way.tags)  do begin
            eTag := eWay.elements['tag',v];
            way.tags[v].v := eTag.attributes['v'].Value;
            way.tags[v].k := eTag.attributes['k'].Value;
          end;

        end;

      end;
    finally
      GarbageCollect(xml);
    end;
  end;
end;

procedure TXMLDrawWorkingData.ReadFromSTream(s: TSTream);
var
  t: ni;
  cnt: int64;
begin

  //node count
  Stream_GuaranteeRead(s, @cnt, sizeof(cnt));
  setlength(nodes, cnt);

  //nodes
  for t:= 0 to high(nodes) do
    nodes[t].ReadFromStream(s);

  //way count
  Stream_GuaranteeRead(s, @cnt, sizeof(cnt));
  setlength(ways, cnt);

  //ways
  for t := 0 to high(ways) do
    ways[t].ReadFromStream(s);


end;

procedure TXMLDrawWorkingData.ToBinFile(sBin: string);
var
  str: Tmemoryfilestream;
begin
  str := Tmemoryfilestream.create(sBin, fmCreate);
  try
    WriteToStream(str);
  finally
    str.free;
  end;
end;

procedure TXMLDrawWorkingData.WriteToStream(s: TStream);
var
  t: ni;
  cnt: int64;
begin
  //node count
  setlength(nodes,0);
  cnt := length(nodes);
  Stream_GuaranteeWrite(s, @cnt, sizeof(cnt));


  //nodes
  for t:= 0 to high(nodes) do
    nodes[t].WriteToStream(s);

  //tag count
  cnt := length(ways);
  Stream_GuaranteeWrite(s, @cnt, sizeof(cnt));

  //tags
  for t := 0 to high(ways) do
    ways[t].WriteToStream(s);

end;

{ TWayData }

function TWayData.AddNode(id: int64): PNodeData;
var
  n: TNodedata;
begin
  n.id := id;
  setlength(nodes, length(nodes)+1);
  nodes[high(nodes)] := n;
  result := @nodes[high(nodes)];

end;

procedure TWayData.AddTag(const k, v: string);
begin
  setlength(tags, length(tags)+1);
  tags[high(tags)].k := k;
  tags[high(tags)].v := v;
end;

function TWayData.GetTagValue(sTag, sDefault: string): string;
var
  idx: ni;
begin
  idx := IndexOfTag(sTag);
  if idx < 0 then
    result := sDefault
  else
    result := tags[idx].v;


end;

function TWayData.HasTag(sTag: string): boolean;
begin
  result := IndexOfTag(sTag) > -1;
end;

function TWayData.IndexOfTag(sTag: string): ni;
var
  t: ni;
begin
  sTag := lowercase(sTag);

  result := -1;
  for t:= 0 to high(tags) do begin
    if lowercase(tags[t].k) = sTag then begin
      result := t;
      exit;
    end;
  end;

end;

procedure TWayData.ReadFromSTream(s: TSTream);
var
  t: ni;
  cnt: int64;
begin
  //id
  Stream_GuaranteeRead(s, @id, sizeof(id));

  //node count
  Stream_GuaranteeRead(s, @cnt, sizeof(cnt));
  setlength(nodes, cnt);

  //nodes
  for t:= 0 to high(nodes) do
    nodes[t].ReadFromStream(s);

  //tag count
  Stream_GuaranteeRead(s, @cnt, sizeof(cnt));
  setlength(tags, cnt);

  //tags
  for t := 0 to high(tags) do
    tags[t].ReadFromStream(s);

end;

procedure TWayData.WriteToStream(s: TStream);
var
  t: ni;
  cnt: int64;
begin
  //ID
  Stream_GuaranteeWrite(s, @id, sizeof(id));

  //node count
  cnt := length(nodes);
  Stream_GuaranteeWrite(s, @cnt, sizeof(cnt));


  //nodes
  for t:= 0 to high(nodes) do
    nodes[t].WriteToStream(s);

  //tag count
  cnt := length(tags);
  Stream_GuaranteeWrite(s, @cnt, sizeof(cnt));

  //tags
  for t := 0 to high(tags) do
    tags[t].WriteToStream(s);



end;

{ TnodeData }

procedure TnodeData.ReadFromSTream(s: TSTream);
begin
  Stream_guaranteeread(s, @self, sizeof(self));
end;

procedure TnodeData.WriteToStream(s: TStream);
begin
  Stream_guaranteewrite(s, @self, sizeof(self));
end;

{ TWayTag }

procedure TWayTag.ReadFromSTream(s: TSTream);
var
  i: cardinal;
begin
  stream_guaranteeread(s, @i, sizeof(i));
  setlength(k, i);
  stream_guaranteeread(s, @k[STRZ], i*sizeof(char));

  stream_guaranteeread(s, @i, sizeof(i));
  setlength(v, i);
  stream_guaranteeread(s, @v[STRZ], i*sizeof(char));
end;

procedure TWayTag.WriteToStream(s: TStream);
var
  i: cardinal;
begin
  //key string
  i := length(k);
  stream_guaranteewrite(s, @i, sizeof(i));
  stream_guaranteewrite(s, @k[STRZ], length(k)*sizeof(char));

  //value string
  i := length(v);
  stream_guaranteewrite(s, @i, sizeof(i));
  stream_guaranteewrite(s, @v[STRZ], length(v)*sizeof(char));



end;

end.
