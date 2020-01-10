unit packethelpers;
//TODO 4: WORKOUT HOW TO PROPERTY MARSHALL TOBJECT TYPES
//TODO 3: WORKOUT CReation/Destruction Scheme  * would call "ByPointer" function which uses existing class

interface
{x$DEFINE ALLOW_BYTE_ZIP}
{$DEFINE SAVE_MEMORY}

uses
  consolelock, helpers_stream, debug, system.ioutils, numbers, systemx, typex, packet, sysutils, SqlExpr, DB, StorageEngineTypes, variants, Databasedictionary, dir, dirfile, classes, zip, rdtp_file, betterobject;

type
  TStringArray = array of ansistring;
  bool = boolean;

  TRemoteData = record
    TimeToOut: single;
    Position: single;
    Length: single;
    InPoint: single;
    OutPoint: single;
    Ready: boolean;
    NowPlaying: array[1..255] of char;
    procedure SetNowPlaying(s: string);overload;
    procedure SetNowPlaying(p: PByte; iLength: integer);overload;

  end;

procedure GetTRemoteDataFromPacket(packet: TRDTPPacket; out res: TRemoteData);
procedure WriteTRemoteDataToPacket(packet: TRDTPPacket; cs: TRemoteData);


procedure GetTAdvancedFileChecksumFromPacket(packet: TRDTPPacket; out res: TAdvancedFileCheckSum);
procedure WriteTAdvancedFileChecksumToPacket(packet: TRDTPPacket; cs: TAdvancedFileCheckSum);

procedure GetIntegerFromPacket(packet: TRDTPPacket; out res: integer);
procedure GetInt64FromPacket(packet: TRDTPPacket; out res: int64);
procedure GetBooleanFromPacket(packet: TRDTPPacket; out res: boolean);
procedure GetStringFromPacket(packet: TRDTPPacket; out res: ansistring);overload;
procedure GetStringFromPacket(packet: TRDTPPacket; out res: string);overload;
procedure GetTStringArrayFromPacket(packet: TRDTPPacket; out res: TStringArray);
procedure GetBoolFromPacket(packet: TRDTPPacket; out res: boolean);

procedure GetRealFromPacket(packet: TRDTPPacket; out res: real);
procedure WriteRealToPacket(packet: TRDTPPacket; r: real);
procedure GetNativeFloatFromPacket(packet: TRDTPPacket; out res: NativeFloat);
procedure WriteNativeFloatToPacket(packet: TRDTPPacket; r: NativeFloat);

procedure WriteDoubleToPacket(packet: TRDTPPacket; f: double);
procedure GetDoubleFromPacket(packet: TRDTPPacket; out res: double);

procedure WriteSingleToPacket(packet: TRDTPPacket; f: single);
procedure GetSingleFromPacket(packet: TRDTPPacket; out res: single);

procedure WriteBoolToPacket(packet: TRDTPPacket;  b: boolean);
procedure WriteBooleanToPacket(packet: TRDTPPacket;  b: boolean);
procedure WriteIntegerToPacket(packet: TRDTPPacket;  i: integer);
procedure WriteInt64ToPacket(packet: TRDTPPacket;  i: int64);
procedure WriteStringToPacket(packet: TRDTPPacket;  s: string);
procedure WriteTStringArrayToPacket(packet: TRDTPPacket; var ary: TStringArray);

procedure GetTFileInformationFromPacket(packet: TRDTPPacket; out inf: TFileInformation);
procedure WriteTFileInformationToPacket(packet: TRDTPPacket; inf: TFileInformation);
procedure GetTDirectoryFromPacket(packet: TRDTPPacket; out d: TDirectory);
procedure WriteTDirectoryToPacket(packet: TRDTPPacket; d: TDirectory);

procedure GetTCustomSQLDataSetFromPAcket(packet: TRDTPPacket; out ds: TCustomSQLDAtaSet);
procedure WriteTCustomSQLDataSetToPacket(packet: TRDTPPacket; ds: TCustomSQLDataSet);

procedure GetTSERowSetFromPAcket(packet: TRDTPPacket; out ds: TSERowSet);
procedure WriteTSERowSetToPacket(packet: TRDTPPacket; ds: TSERowSet);

procedure GetTSERowSetArrayFromPAcket(packet: TRDTPPacket; out ds: TSERowSetArray);
procedure WriteTSERowSetArrayToPacket(packet: TRDTPPacket; ds: TSERowSetArray);

procedure WriteTStringListToPacket(packet: TRDTPPacket; slTakesOwnership: TStringList);
procedure GetTStringListfromPacket(packet: TRDTPPacket; out sl: TStringList);

//--dictionary types
procedure GetTFieldTypeFromPacket(packet: TRDTPPacket; out ft: TFieldTYpe);
procedure WriteTFieldTypeToPacket(packet: TRDTPPacket; ft: TFieldTYpe);

procedure GetTDatabaseDictionaryFromPacket(packet: TRDTPPacket; out dd: TDatabaseDictionary);
procedure WriteTDatabaseDictionaryToPacket(packet: TRDTPPacket; dd: TDatabaseDictionary);

procedure GetTTableDefinitionFromPacket(packet: TRDTPPacket; out td: TTableDefinition);
procedure WriteTTableDefinitionToPacket(packet: TRDTPPacket; td: TTableDefinition);

procedure GetTFieldDefinitionFromPacket(packet: TRDTPPacket; out fd: TFieldDefinition);
procedure WriteTFieldDefinitionToPacket(packet: TRDTPPacket; fd: TFieldDefinition);

procedure GetTStorageEngineInfoFromPacket(packet: TRDTPPacket; out se: TStorageEngineInfo);
procedure WriteTStorageEngineInfoToPacket(packet: TRDTPPacket; se: TStorageEngineInfo);

procedure GetTTableRelationFromPacket(packet: TRDTPPAcket; out tr: TTableRelation);
procedure WriteTTableRelationToPacket(packet: TRDTPPacket; tr: TTableRelation);

procedure GetTFileTransferReferenceFromPacket(packet: TRDTPPacket; out fr: TFileTransferReference);
procedure WriteTFileTransferReferenceToPacket(packet: TRDTPPacket; tr: TFileTransferReference);

procedure WriteTStreamToPacket(packet: TRDTPPacket; s: TStream);
procedure GetTStreamfromPacket(packet: TRDTPPacket; out s: TStream);

procedure WriteTDynByteArrayToPacket(packet: TRDTPPacket; ba: TDynByteArray);
procedure GetTDynByteArrayFromPacket(packet: TRDTPPacket; out ba: TDynByteArray);
procedure GetTDynInt64ArrayFromPacket(packet: TRDTPPacket; out ia: TDynInt64Array);
procedure WriteTDynInt64ArrayToPacket(packet: TRDTPPacket; ia: TDynInt64Array);

procedure WriteTDateTimeToPacket(packet: TRDTPPacket; dt: TDateTime);
procedure GetTDateTimeFromPAcket(packet: TRDTPPacket; out dt: TDateTime);

procedure WriteTRemoteFileRecToPacket(packet: TRDTPPacket; fr: TRemoteFileRec);
procedure GetTRemoteFileRecFromPAcket(packet: TRDTPPacket; out fr: TRemoteFileRec);

procedure WriteTRemoteFileArrayToPacket(packet: TRDTPPacket; fr: TRemoteFileArray);
procedure GetTRemoteFileArrayFromPAcket(packet: TRDTPPacket; out fr: TRemoteFileArray);




//^^^^






implementation

procedure GetTFileInformationFromPacket(packet: TRDTPPacket; out inf: TFileInformation);
var
  i: nativeint;
begin
  inf := TFileInformation.create;
  inf.FullName := packet.SeqRead;
  packet.SeqRead;//name is kinda redundant
  i := packet.SeqRead;
  inf.UniversalAttributes := i;


end;

procedure WriteTFileInformationToPacket(packet: TRDTPPacket; inf: TFileInformation);
var
  i: nativeint;
  fa: TFileAttributes;
begin
  packet.AddString(inf.Fullname);
  packet.AddString(inf.Name);

  i := inf.UniversalAttributes;

//  i := 0;
//  MoveMem32(@i, @fa, lesserof(sizeof(fa), sizeof(i)));

  packet.AddInt(i);

end;



procedure GetTFileTransferReferenceFromPacket(packet: TRDTPPacket; out fr: TFileTransferReference);
var
  iLength: int64;
begin
  fr := THolder<TfileTransferReferenceObj>.create;
  fr.o := TfileTransferReferenceObj.create;

  fr.o.filename := packet.SeqRead;
  fr.o.Handle := packet.SeqREad;
  fr.o.ContainsData := packet.seqread;
  fr.o.StartBlock := packet.seqread;
  fr.o.length := packet.SeqRead;
  if fr.o.ContainsData then begin
    fr.o.EOF := packet.seqread;
    fr.o.Buffer := packet.SeqReadBytes(iLength);
    if fr.o.EOF then begin
      fr.o.FileDate := packet.SeqRead;
    end;
  end;
end;


procedure WriteTFileTransferReferenceToPacket(packet: TRDTPPacket; tr: TFileTransferReference);
begin
  try
    packet.addvariant(tr.o.FileName);
    packet.AddLong(tr.o.handle);
    packet.addvariant(tr.o.ContainsData);
    packet.addvariant(tr.o.StartBlock);
    packet.addvariant(tr.o.length);
    if (tr.o.containsdata) then begin
      packet.AddVariant(tr.o.EOF);
      packet.addBytes(tr.o.Buffer, tr.o.Length);
      if tr.o.eof then begin
        packet.AddVariant(tr.o.FileDate);
      end;
    end;
  finally
//    tr.free;
  end;


end;

procedure GetTCustomSQLDataSetFromPAcket(packet: TRDTPPacket; out ds: TCustomSQLDAtaSet);
var
  dt: TFieldType;
  sName: ansistring;
  iFieldCount: integer;
  iRowCount: integer;
  t: integer;
begin
  ds := TCustomSQLDAtaset.Create(nil);

  ds.Active := true;
  //read field count
  iFieldCount := packet.SeqRead;
  for t:= 0 to iFieldCount-1 do begin
    sName := packet.SeqRead;
    dt := TFieldType(integer(packet.Seqread));
    ds.FieldDefs.Add(sName, dt);
  end;

  while not (packet.SeqRead) do begin
    ds.Append;
    for t:= 0 to iFieldCount-1 do begin
      ds.Fields[t].AsVariant := packet.SeqRead;

    end;
    ds.Post;
  end;



end;

procedure WriteTCustomSQLDataSetToPacket(packet: TRDTPPacket; ds: TCustomSQLDataSet);
var
  t: integer;
begin
  //write field count
  //write fields
  //write boolean NOT eof
  //write field values
  //write boolean EOF

  if assigned(ds) then begin
    ds.first;
    packet.AddVariant(ds.FieldCount);
    for t:= 0 to ds.FieldCount-1 do begin
      packet.AddString(ds.FieldDefs[t].Name);
      packet.AddVariant(ord(ds.fielddefs[t].DataType));
    end;

    while not ds.Eof do begin
      packet.addBoolean(true);
      for t:= 0 to ds.FieldCount-1 do begin
        packet.AddVariant(ds.Fields[t].AsVariant);
      end;

      ds.Next;
    end;
  end else
    packet.addvariant(0);

  packet.AddBoolean(false);
end;

procedure GetTStringArrayFromPacket(packet: TRDTPPacket; out res: TStringArray);
var
  iCount: integer;
  t: integer;
begin
  //get number of ansistrings
  iCount := packet.SeqRead;
  //get ansistrings
  SetLength(res, iCount);
  for t:= low(res) to high(res) do begin
    res[t] := packet.seqread;
  end;

end;

procedure GetInt64FromPacket(packet: TRDTPPacket; out res: int64);
begin
  res := packet.seqread;

end;

procedure GetBooleanFromPacket(packet: TRDTPPacket; out res: boolean);
var
  v: variant;
begin
  v := packet.seqread;
//  debug.log(vartostr(v));
  res := v;

end;

procedure GetStringFromPacket(packet: TRDTPPacket; out res: ansistring);
begin
  res := packet.seqread;

end;

procedure GetStringFromPacket(packet: TRDTPPacket; out res: string);
begin
  res := packet.seqread;

end;

procedure GetIntegerFromPacket(packet: TRDTPPacket; out res: integer);
begin
  res := packet.seqread;

end;


procedure WriteBooleanToPacket(packet: TRDTPPacket; b: boolean);
begin
  packet.AddBoolean(b);

end;

procedure WriteIntegerToPacket(packet: TRDTPPacket; i: integer);
begin
  packet.addvariant(i);
end;

procedure WriteInt64ToPacket(packet: TRDTPPacket; i: int64);
begin
  packet.addlonglong(i);
end;

procedure WriteDoubleToPacket(packet: TRDTPPacket; f: double);
begin
  packet.adddouble(f);
end;
procedure WriteSingleToPacket(packet: TRDTPPacket; f: single);
begin
  packet.AddDouble(f);
end;
procedure GetSingleFromPacket(packet: TRDTPPacket; out res: single);
begin
  res := packet.SeqRead;
end;



procedure WriteStringToPacket(packet: TRDTPPacket; s: string);
begin
  packet.addstring(s);
end;

procedure WriteTStringArrayToPacket(packet: TRDTPPacket; var ary: TStringArray);
var
  t: integer;
begin

  packet.AddLong(length(ary));
  for t:= low(ary) to high(ary) do begin
    packet.AddString(ary[t]);
  end;

end;

function DatasetToString(ds: TCustomSQLDAtaSet; destroyit: boolean = false): ansistring;
var
  iCount: integer;
  t: integer;
  s: ansistring;
begin
  s := '';
  for t:= 0 to ds.FieldCount-1 do begin
    s := s+'['+ds.FieldDefs[t].Name+']';
  end;
  s := s+#13#10;


  ds.First;
  while not ds.Eof do begin
    for t:= 0 to ds.FieldCount-1 do begin
      s := s+'['+ds.Fields[t].AsString+']';
    end;
    s := s+#13#10;
    ds.next;
  end;

  result := s;

end;

function RowsetToString(ds: TSERowSet; destroyit: boolean = false): ansistring;
var
  iCount: integer;
  t,u: integer;
  s: ansistring;
begin
  s := '';
  for t:= 0 to ds.FieldCount-1 do begin
    s := s+'['+ds.fields[t].sName+']';
  end;
  s := s+#13#10;

  for u := 0 to ds.RowCount-1 do begin
    for t:= 0 to ds.FieldCount-1 do begin
      s := s+'['+vartostr(ds.Values[t,u])+']';
    end;
    s := s+#13#10;
  end;

  result := s;

end;



procedure GetTSERowSetFromPAcket(packet: TRDTPPacket; out ds: TSERowSet);
var
  dt: TFieldType;
  sName: ansistring;
  iFieldCount: integer;
  iRowCount: integer;
  t: integer;
  iRow: integer;
begin
  ds := TSERowSet.Create;
  //read field count
  ds.BoundToTable := packet.SeqRead;
  iFieldCount := packet.SeqRead;
  ds.SetFieldCount(iFieldCount);
  for t:= 0 to iFieldCount-1 do begin
    sName := packet.SeqRead;
    dt := TFieldType(integer(packet.Seqread));
    ds.fields[t].sName := sName;
    ds.fields[t].vType := dt;
  end;

  while not (packet.SeqRead) do begin
    iRow := ds.AddRow;
    for t:= 0 to iFieldCount-1 do begin
      ds.Values[t,iRow] := packet.SeqRead;
    end;
  end;

  ds.first;



end;
procedure WriteTSERowSetToPacket(packet: TRDTPPacket; ds: TSERowSet);
var
  t,u: integer;
begin
{$IFDEF SAVE_MEMORY}
  LockConsole;
{$ENDIF}
  try
  //bound to table - optional
  packet.AddString(ds.BoundToTable);
  //field defs
  packet.AddVariant(ds.FieldCount);
  for t:= 0 to ds.FieldCount-1 do begin
    packet.AddString(ds.fields[t].sName);
    packet.AddVariant(ord(ds.fields[t].vType));
  end;


  for u := 0 to ds.RowCount-1 do begin
    packet.addBoolean(false); //NOT EOF
    for t:= 0 to ds.FieldCount-1 do begin
      packet.AddVariant(ds.Values[t,u]);
    end;
  end;

  packet.AddBoolean(true);//EOF

  finally
    ds.free;
{$IFDEF SAVE_MEMORY}
    UnlockConsole;
{$ENDIF}
  end;
end;

procedure GetTFieldTypeFromPacket(packet: TRDTPPacket; out ft: TFieldTYpe);
begin
  ft := TFieldType(packet.SeqRead);
end;

procedure WriteTFieldTypeToPacket(packet: TRDTPPacket; ft: TFieldTYpe);
begin
  packet.Addlong(integer(ft));
end;

procedure GetTSERowSetArrayFromPAcket(packet: TRDTPPacket; out ds: TSERowSetArray);
var
  t: integer;
begin
  //read length
  //read array

end;
procedure WriteTSERowSetArrayToPacket(packet: TRDTPPacket; ds: TSERowSetArray);
var
  t: integer;
begin
  //write length
  packet.Addlong(length(ds));
  //write array
  for t:= low(ds) to high(ds) do begin

  end;

end;

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
procedure GetTDatabaseDictionaryFromPacket(packet: TRDTPPacket; out dd: TDatabaseDictionary);
var
  iCount: integer;
  t: integer;
  se: TStorageEngineInfo;
  td: TTableDefinition;
  sContext: ansistring;
begin
  sContext := packet.SeqRead;
  dd := TDatabaseDictionary.create(sContext,'');
//TODO -cunimplemented: unimplemented block
  //read base params
  dd.Config := packet.seqread;

  //read host count
  iCount := packet.seqread;
  //read hosts
  for t:= 0 to iCount-1 do begin
    packethelpers.GetTStorageEngineInfoFRomPacket(packet, se);
    dd.AddHost(se);
  end;
  //dd.LoadHostsFromFile;

  //read table count
  iCount := packet.seqread;
  //read tables
  for t:= 0 to iCount-1 do begin
    packethelpers.GetTTableDefinitionFromPacket(packet, td);
    dd.addtable(td);
  end;

  dd.fetched := true;
end;
//------------------------------------------------------------------------------
procedure WriteTDatabaseDictionaryToPacket(packet: TRDTPPacket; dd: TDatabaseDictionary);
var
  t: integer;
begin
  packet.AddString(dd.Context);
  packet.AddString(dd.Config);

  packet.addvariant(dd.HostCount);
  for t:= 0 to dd.hostcount-1 do begin
    WriteTStorageEngineInfoToPacket(packet, dd.hosts[t]);
  end;

  packet.addvariant(dd.TableCount);
  for t:= 0 to dd.TableCount-1 do begin
    WriteTTableDefinitionToPacket(packet, dd.tables[t]);
  end;



end;
//------------------------------------------------------------------------------
procedure GetTTableDefinitionFromPacket(packet: TRDTPPacket; out td: TTableDefinition);
var
  t: integer;
  iCount: integer;
  fd: TFieldDefinition;
  tr: TTableRelation;
begin
  td := TTableDefinition.create;
  td.Name := packet.SeqRead;
  iCount := packet.seqread;
  for t:= 0 to iCount-1 do begin
    GetTFieldDefinitionFromPacket(packet, fd);
    if fd<> nil then
      td.AddField(fd);
  end;
  //DONE 1: Add Relation Marchalling
  iCount := packet.SeqRead;
  for t:= 0 to iCount-1 do begin
    tr := TTableRelation.create;
    GetTTableRelationFRomPacket(packet, tr);
    td.AddRelation(tr);
  end;
  //TODO 4: Add parts to marshalling

end;
//------------------------------------------------------------------------------
procedure WriteTTableDefinitionToPacket(packet: TRDTPPacket; td: TTableDefinition);
var
  t: integer;
begin
  packet.AddVariant(td.Name);
  packet.AddVariant(td.FieldCount);
  for t:=0 to td.FieldCount-1 do begin
    WriteTFieldDefinitionToPacket(packet, td.Fields[t]);
  end;
  //DONE 1: Add Relation Marshalling
  packet.AddVariant(td.RelationCount);
  for t:= 0 to td.RelationCount-1 do begin
    WriteTTableRelationToPacket(packet, td.Relations[t]);
  end;
  //TODO 5: Add Part Marshalling

end;
//------------------------------------------------------------------------------
procedure GetTFieldDefinitionFromPacket(packet: TRDTPPacket; out fd: TFieldDefinition);
begin
//TODO -cunimplemented: unimplemented block
  fd := TFieldDefinition.create(nil);
  fd.Name := packet.SeqRead;
  fd.FieldType := packet.seqread;

end;
//------------------------------------------------------------------------------
procedure WriteTFieldDefinitionToPacket(packet: TRDTPPacket; fd: TFieldDefinition);
begin
  packet.addvariant(fd.name);
  packet.addvariant(fd.FieldType);
end;
//------------------------------------------------------------------------------
procedure GetTStorageEngineInfoFromPacket(packet: TRDTPPacket; out se: TStorageEngineInfo);
begin
  se := TStorageEngineInfo.create;
  se.ID := packet.seqread;
  se.SE := packet.SeqRead;
  se.DB := packet.SeqRead;
  se.Endpoint := packet.SeqRead;
  se.SystemPower := packet.seqread;
  se.Context := packet.seqread;

end;
//------------------------------------------------------------------------------
procedure WriteTStorageEngineInfoToPacket(packet: TRDTPPacket; se: TStorageEngineInfo);
begin
  packet.AddVariant(se.id);
  packet.AddVariant(se.SE);
  packet.AddVariant(se.DB);
  packet.AddVariant(se.Endpoint);
  packet.AddVariant(se.SystemPower);
  packet.AddVariant(se.Context);

end;

//------------------------------------------------------------------------------
procedure GetTTableRelationFromPacket(packet: TRDTPPAcket; out tr: TTableRelation);
begin
  tr := TTableRelation.create;
  tr.JoinName := packet.SeqRead;
  tr.MAsterTable := packet.SeqRead;
  tr.DetailTable := packet.SeqRead;
  tr.MasterField := packet.SeqRead;
  tr.DetailField := packet.SeqRead;
  tr.Publish := packet.SeqRead;

end;
//------------------------------------------------------------------------------
procedure WriteTTableRelationToPacket(packet: TRDTPPacket; tr: TTableRelation);
begin
  packet.AddVariant(tr.JoinName);
  packet.AddVariant(tr.MasterTable);
  packet.AddVariant(tr.DetailTable);
  packet.AddVariant(tr.MasterField);
  packet.AddVariant(tr.DetailField);
  packet.AddVariant(tr.Publish);

end;
//------------------------------------------------------------------------------
procedure GetTDirectoryFromPacket(packet: TRDTPPacket; out d: TDirectory);
var
  count,t: integer;
  fi: TFileInformation;
begin
  d := TDirectory.CreateFromRemote;
  try
    count := packet.seqread;
    for t:= 0 to count-1 do begin
      GetTFileInformationFromPacket(packet, fi);
      d.AddFileInfo(fi);
    end;

    count := packet.seqread;
    for t:= 0 to count-1 do begin
      GetTFileInformationFromPacket(packet, fi);
      d.AddFolderInfo(fi);
    end;
  except
    d.free;
    d := nil;
    raise;
  end;


end;
//------------------------------------------------------------------------------
procedure WriteTDirectoryToPacket(packet: TRDTPPacket; d: TDirectory);
var
  t: integer;
begin
  packet.AddVariant(d.Filecount);
  for t:= 0 to d.Filecount-1 do begin
    WriteTFileInformationToPacket(packet, d.Files[t]);
  end;

  packet.AddVariant(d.FolderCount);
  for t:= 0 to d.Foldercount-1 do begin
    WriteTFileInformationToPacket(packet, d.Folders[t]);
  end;
end;


procedure GetTAdvancedFileChecksumFromPacket(packet: TRDTPPacket; out res: TAdvancedFileCheckSum);
begin
  res.bytesum := packet.seqread;
  res.bytexor := packet.seqread;
  res.bytecount := packet.seqread;
end;
procedure WriteTAdvancedFileChecksumToPacket(packet: TRDTPPacket; cs: TAdvancedFileCheckSum);
begin
  packet.addshort(cs.bytesum);
  packet.addshort(cs.bytexor);
  packet.addlonglong(cs.bytecount);

end;



procedure GetBoolFromPacket(packet: TRDTPPacket; out res: boolean);
begin
  GetBooleanFromPacket(packet, res);
end;



procedure WriteBoolToPacket(packet: TRDTPPacket;  b: boolean);
begin
  WriteBooleanToPacket(packet, b);
end;

procedure GetRealFromPacket(packet: TRDTPPacket; out res: real);
begin
  res := packet.SeqRead;
end;

procedure WriteRealToPacket(packet: TRDTPPacket; r: real);
begin
  packet.adddouble(r);
end;

procedure GetNativeFloatFromPacket(packet: TRDTPPacket; out res: NativeFloat);
begin
  res := packet.SeqRead;
end;


procedure GetDoubleFromPacket(packet: TRDTPPacket; out res: double);
begin
  res := packet.SeqRead;
end;
procedure WriteNativeFloatToPacket(packet: TRDTPPacket; r: NativeFloat);
begin
  packet.adddouble(r);
end;


procedure WriteTStreamToPacket(packet: TRDTPPacket; s: TStream);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.create;
  try
    if s<> nil then begin
      s.seek(0, soBeginning);
      stream_GuaranteeCopy(s, ms, s.size);
    end;


    //ms.CopyFrom(s, s.size);
    ms.seek(0, soBeginning);
    packet.AddBytes(ms.Memory, ms.Size);

  finally
    if s <> nil then
      s.free;
    ms.free;
  end;

end;

procedure GetTStreamfromPacket(packet: TRDTPPacket; out s: TStream);
var
  ms: TMemoryStream;
  pb: PByte;
  iLength: int64;
  t: integer;
  b: byte;
begin
  ms := TMemoryStream.create;
  pb := packet.SeqReadBytes(ilength);


  stream_GuaranteeWrite(ms, pb, iLength);

  ms.Seek(0,soBeginning);

  s := ms;

  s.seek(0,soBeginning);
  FreeMem(pb);


end;

procedure WriteTStringListToPacket(packet: TRDTPPacket; slTakesOwnership: TStringList);
var
  t: integer;
begin
  packet.Addlong(slTakesOwnerShip.Count);
  for t:= 0 to slTakesOwnership.Count-1 do begin
    packet.AddString(slTakesOwnership[t]);
  end;

end;
procedure GetTStringListfromPacket(packet: TRDTPPacket; out sl: TStringList);
var
  t, count: integer;
begin
  sl := TStringlist.create;
  count := packet.SeqRead;
  for t:= 0 to count-1 do begin
    sl.Add(packet.SeqRead);
  end;

end;


procedure TRemoteData.SetNowPlaying(s: string);
var
  t: integer;
  iend: integer;
begin
  for t:= 1 to 255 do begin
    Self.NowPlaying[t] := #0;
  end;


  iEnd := system.Length(s);
  iEnd := lesserof(iend-1, 254);

  for t:= 1 to iEnd do begin
    nowplaying[t] := s[t];
  end;

end;

procedure GetTRemoteDataFromPacket(packet: TRDTPPacket; out res: TRemoteData);
var
  iPoo: int64;
  p: PByte;
begin
  res.TimeToOut := packet.SeqRead;
  res.Position := packet.SeqRead;
  res.Length := packet.SeqRead;
  res.InPOint := packet.SeqRead;
  res.outpoint := packet.SeqRead;
  res.Ready := packet.SeqRead;

  p := packet.SeqReadBytes(iPoo);
  res.SetNowPlaying(p, iPoo);

  freemem(p);


end;
procedure WriteTRemoteDataToPacket(packet: TRDTPPacket; cs: TRemoteData);
begin
  packet.AddDouble(cs.TimeToOut);
  packet.AddDouble(cs.Position);
  packet.AddDouble(cs.Length);
  packet.AddDouble(cs.InPoint);
  packet.AddDouble(cs.OutPoint);
  packet.AddBoolean(cs.Ready);
  packet.AddBytes(pbyte(@cs.NowPlaying[1]), 255);

end;





procedure TRemoteData.SetNowPlaying(p: PByte; iLength: integer);
var
  t: integer;
begin
  for t:= 1 to 255 do begin
    NowPlaying[t] :=  #0;
  end;

  for t := 0 to iLength do begin
    NowPlaying[t+1] := char(p[t]);
  end;

end;

procedure WriteTDynByteArrayToPacket(packet: TRDTPPacket; ba: TDynByteArray);
var
  ba2: PByte;
  l,l2: ni;
begin
{$IFDEF ALLOW_BYTE_ZIP}
  if ba = nil then begin
    packet.AddInt(-1);
    packet.AddBytes(nil, 0);
  end else
  begin
    l := length(ba);
    ba2 := GetMemory(l);
    try
      l2 := zipram(@ba[0], ba2, l, l);
      //if NOT zipped
      if l2 < 0 then begin
        packet.AddInt(-1);//-1
        packet.AddBytes(@ba[0], l);
      end
      //if ZIPPED
      else begin
        packet.AddInt(l);
        packet.AddBytes(ba2, l2);
      end;
    finally
      FreeMemory(ba2);
    end;
  end;
{$ELSE}
  packet.AddInt(-1);
  packet.AddBytes(@ba[0], length(ba));
{$ENDIF}
end;

procedure GetTDynByteArrayFromPacket(packet: TRDTPPacket; out ba: TDynByteArray);
var
  ba2: TDynByteArray;
  l: int64;
begin
  l := packet.seqread;
  if l < 0 then begin
    ba := packet.seqreadBytes;
  end else begin
    SetLength(ba, l);
    if l > 0 then begin
      ba2 := packet.seqreadBytes;
      setlength(ba, l);
      UnzipRam(@ba2[0], @ba[0], length(ba2), l, nil);
    end;
  end;
end;

procedure WriteTDynInt64ArrayToPacket(packet: TRDTPPacket; ia: TDynInt64Array);
begin
  WriteTDynByteArrayToPacket(packet, DynInt64ArrayToByteArray(ia));
end;

procedure GetTDynInt64ArrayFromPacket(packet: TRDTPPacket; out ia: TDynInt64Array);
var
  ba: TDynByteArray;
begin
  GetTDynByteArrayFromPacket(packet, ba);
  ia := DynByteArrayToInt64Array(ba);
end;

procedure WriteTDateTimeToPacket(packet: TRDTPPacket; dt: TDateTime);
begin
  packet.AddDateTime(dt);
end;

procedure GetTDateTimeFromPAcket(packet: TRDTPPacket; out dt: TDateTime);
begin
  dt := packet.SeqRead;
end;

procedure WriteTRemoteFileRecToPacket(packet: TRDTPPacket; fr: TRemoteFileRec);
begin
  packet.AddString(fr.name);
  packet.AddString(fr.path);
  packet.AddDateTime(fr.date);
  packet.AddInt(fr.attributes);
  packet.AddInt(fr.size);
end;
procedure GetTRemoteFileRecFromPAcket(packet: TRDTPPacket; out fr: TRemoteFileRec);
begin
  fr.name := packet.seqread;
  fr.path := packet.SeqRead;
  fr.date := packet.SeqRead;
  fr.attributes := packet.SeqRead;
  fr.size := packet.SeqRead;
end;

procedure WriteTRemoteFileArrayToPacket(packet: TRDTPPacket; fr: TRemoteFileArray);
var
  t: ni;
begin
  packet.AddInt(length(fr));
  for t:= low(fr) to high(fr) do
    WriteTRemoteFileRecToPacket(packet, fr[t]);

end;


procedure GetTRemoteFileArrayFromPacket(packet: TRDTPPacket; out fr: TRemoteFileArray);
var
  t: ni;
begin
  t := packet.seqread;
  setlength(fr, t);
  for t:= low(fr) to high(fr) do begin
    GetTRemoteFileRecFromPacket(packet, fr[t]);
  end;

end;






end.

