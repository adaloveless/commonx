unit RDTPSystem_Coordinator;
//Todo 5: map of query result (many) does not echo keys sent to list requests

{$INLINE AUTO}
interface

uses RDTPProcessor, sysutils, SQLExpr, variants, ipclientwrapper, windows, simplewinsock, applock, crossplatformtypes, abstractRDTPDataModule, storageenginetypes;


const
  KEY_SESSION = 100;

procedure RDTP_Time(proc: TRDTPProcessor);
procedure RDTP_UpdateQuery(proc: TRDTPProcessor);
procedure RDTP_NoTransUpdateQuery(proc: TRDTPProcessor);
procedure RDTP_FireForgetQuery(proc: TRDTPProcessor);
procedure RDTP_Query(proc: TRDTPProcessor);
procedure RDTP_RecordQuery(proc: TRDTPProcessor);
procedure RDTP_QueryMap(proc: TRDTPProcessor);
procedure RDTP_Login(proc: TRDTPProcessor);
procedure RDTP_Logout(proc: TRDTPProcessor);
procedure RDTP_GetNextID(proc: TRDTPProcessor);
procedure RDTP_SetNextID(proc: TRDTPProcessor);
procedure RDTP_Rollback(proc: TRDTPProcessor);
procedure RDTP_GetReplayLogs(proc: TRDTPProcessor);
procedure RenderQueryResult(proc: TRDTPProcessor; sQuery: ansistring; query: TSERowSet; bExpectmany: boolean);
procedure MapQueryResult(proc: TRDTPProcessor; sQuery: ansistring; query: TSERowSet; iBaseType: integer; iBaseKeys: integer; iSubType: integer; iSubKeys: integer);overload;
procedure MapQueryResult(proc: TRDTPProcessor; sQuery: ansistring; query: TSERowSet; iIgnoreKeys: integer; iBaseType: integer; iBaseKeys: integer; iSubType: integer; iSubKeys: integer);overload;

procedure LockForget;
procedure UnlockForget;

implementation

uses sharedDebugLog, ReplayLog;

var
  sectForget: TCLXCriticalSection;

procedure LockForget;
begin
  EnterCriticalSection(sectForget);
end;
procedure UnlockForget;
begin
  LeaveCriticalSection(sectForget);
end;





procedure RDTP_Time(proc: TRDTPProcessor);
var
  t: integer;
begin

//    response.IsResponse := true;

//  proc.response.AddLong(0);//sessionid
//  proc.response.AddBoolean(true);//result
//  proc.response.AddLong(0);//error
//  proc.response.AddString('');//message

  for t:= 0 to 10 do begin
    proc.SendProgress('Progress test...',t, 10, inttostr(t));
    sleep(200);
  end;
  proc.response.AddObject($1100, 0, 1, 0, 0);
  proc.response.AddString(datetimetostr(now));

end;

procedure RDTP_Query(proc: TRDTPProcessor);
var
  sQuery: ansistring;
  dm: TAbstractRDTPDAtaModule;
  bExpectMany: boolean;
  ds: TSERowSet;
begin
  //ds := TSERowSet.create(nil);
  ds := nil;
  try
    dm := proc.data;
    dm.ConnectRead;

    //sQuery := proc.Request.SeqRead;
    bExpectMany := proc.request.seqread;
    sQuery := proc.Request.SeqRead;
    GLOG.Debug(sQuery);

//    dm.qread.Active := false;
//    dm.qread.SQL.text := sQuery;
    Replaylog.LogReplay(sQuery);


    dm.ExecuteRead(sQuery, ds);
//    dm.reads.Execute(sQuery, nil, @ds);

//    ds := TSERowSet(p);
//    dm.qread.Active := true;

//    AL.Lock; try
    RenderQueryResult(proc, sQuery,ds, bExpectMany);
//    finally AL.Unlock; end;
  finally
    ds.free;
  end;
//  except
//    on E:exception do begin
//      GLOG.Debug(e.message);
//      e.Message := 'exception during query: '+e.message;
//
//      raise e;
//    end;
//  end;

end;
//------------------------------------------------------------------------------
procedure RDTP_RecordQuery(proc: TRDTPProcessor);
var
  sQuery: ansistring;
  dm: TAbstractRDTPDataModule;
  ds: TSERowSet;
begin
  ds := nil;
  dm := proc.Data;
  try
//    sQuery := proc.Request.SeqRead;
    proc.request.seqread; //Expect Many
    sQuery := proc.Request.SeqRead;
//    dm.Connect;

//      dm.qread.active := false;
//      dm.qread.SQL.text := sQuery;
      dm.ExecuteRead(sQuery, ds);
      Replaylog.LogReplay(sQuery);
//      dm.qread.Active := true;
    RenderQueryResult(proc, sQuery, ds, false);

  finally
//    dm.free;
    ds.free;
  end;
end;

//------------------------------------------------------------------------------
procedure RDTP_Login(proc: TRDTPProcessor);
var
  dm: TAbstractRDTPDataModule;
  iSession: integer;
  sUser, sGroup, sPassword: ansistring;
  sQuery: ansistring;
  dsUser: TSERowSet;
  sUserID: ansistring;
  ds: TSERowSet;
  sid: integer;
begin
  ds := nil;
  dsUser := nil;
  dm := proc.Data;
//  Al.Lock;
  try
    //fetch user

    //proc.Request.SeqRead;
    sUser := proc.Request.SeqRead;
    sGroup := proc.Request.SeqRead;
    sPassword := proc.Request.SeqRead;

    //verify password
    if (sGroup = 'shop') then begin
      iSession := dm.GetNextID(KEY_SESSION);
        sQuery := 'INSERT INTO SESSION VALUES ('+inttostr(iSession)+',-9999,0, "", now())';
        dm.ExecuteSystem(sQuery);
        Replaylog.LogReplay(sQuery);
    //non shop
    end else begin
        sQuery := 'SELECT * from USER WHERE (((Phone="'+sUser+'") and ((pin="'+sPassword+'") or (password="'+sPassword+'")))  or (((email="'+sUser+'") and ((pin="'+sPassword+'") or (password="'+sPassword+'")))))';
        GLOG.Debug(sQuery,'login');
        dm.ExecuteRead(sQuery, dsUser);
        if (not assigned(dsUser)) or (dsUser.eof) then begin //!!
          raise Exception.create('INVALID USER');
        end else begin
          dsUser.first;
          sUserID := vartostr(dsUser['userid']);
//          if (sUserID <> '4') and (sUser='6515924867') then begin
//            raise exception.create('user pairity error');
//          end;
        end;

      //get next sessionid
      iSession := dm.GetNextID(KEY_SESSION);
      GLOG.Debug('user '+sUserID+' ('+sUser+','+sPassword+') got session '+inttostr(iSession), 'login');

      //create session
//        if sGroup <> 'lwp' then begin
//          sQuery := 'delete from session where userid='+sUserID;
//          GLOG.Debug('login:'+sQuery);
//          dm.ExecuteDirect(sQuery, dm.sessiondb);
//        end;

        sQuery := 'INSERT INTO SESSION VALUES ('+inttostr(iSession)+','+sUserID+',0, "", now())';
        GLOG.Debug('login:'+sQuery);
        dm.ExecuteSYstem(sQuery);

        sQuery := 'SELECT * FROM SESSION WHERE (SessionID='''+inttostr(iSession)+''')';
        GLOG.Debug('login:'+sQuery);
        dm.ExecuteSystem(sQuery, ds);
        if vartostr(ds['userid']) <> sUserID then
          raise exception.create('user validation failure');

    end;
      sQuery := 'SELECT * FROM SESSION WHERE (SessionID='''+inttostr(iSession)+''')';
      dm.ExecuteSystem(sQuery, ds);

      sid := ds['SessionID'];
      proc.response.AddLong(sid);

      GLOG.Debug('user '+sUserID+' returned session '+inttostr(sid), 'login');
      MapQueryResult(proc, 'session', ds, $110A, 1, 0,0);
//    RenderQueryResult(proc, sQuery, ds, false);

  finally
//    NoNeedDM(dm);
//    AL.Unlock;
    ds.free;
    dsUser.free;
  end;
end;
//------------------------------------------------------------------------------
procedure RDTP_Logout(proc: TRDTPProcessor);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure RDTP_GetNextID(proc: TRDTPProcessor);
var
  dm: TAbstractRDTPDataModule;
  iKey: integer;
  i64: integer;
label
  retry;
begin
  dm := proc.Data;
  //proc.request.seqread; //sessionid
  iKey := proc.Request.SeqRead;

  i64 := dm.GetNextID(iKey);
  proc.Response.AddLongLong(i64);

end;

procedure RDTP_SetNextID(proc: TRDTPProcessor);
var
  dm: TAbstractRDTPDataModule;
  iKey: integer;
  i64: integer;
  iValue: int64;
label
  retry;
begin
  dm := proc.Data;
  proc.request.seqread; //sessionid
  iKey := proc.Request.SeqRead;
  iValue := proc.request.SeqRead;
  i64 := dm.SetNextID(iKey, iValue);
end;

procedure RenderQueryResult(proc: TRDTPProcessor; sQuery: ansistring; query: TSERowSet; bExpectMany: boolean);
var
  t,u: integer;
  v: variant;
  iPos, iCount: integer;
begin
  //write fieldcount
  if query = nil then begin
    proc.Response.AddVariant(0);
    proc.Response.AddLongObject($999, 0, 0, 0, 0);
    exit;
  end;


  //write fieldcount
  proc.Response.AddVariant(query.FieldCount);
  for t:= 0 to query.FieldCount-1 do begin
    proc.Response.AddVariant(query.fields[t].sName);
  end;

  iCount := 0;
  if bExpectMany then begin
    query.First;
    iPos := proc.Response.SeqWritePos;
    proc.Response.AddLongObject($999, 1, 0, 0, 0);
    proc.response.AddString(trim(sQuery));
    while not query.eof do begin
      proc.Response.AddObject($999, 2, query.FieldCount, 0, 0);
      proc.response.AddString(sQuery);
      proc.Response.AddShort(iCount);
      for t:= 0 to query.FieldCount-1 do begin
        if vartype(query.CurRecordFieldsByIdx[t]) = varNull then begin
          proc.Response.AddNull;
        end else begin
          v := query.CurRecordFieldsByIdx[t];
          proc.Response.addvariant(v);
        end;
      end;
      inc(iCount);

      query.Next;
    end;

    //replace initial header at end, because SQL sucks
    proc.Response.UpdateLongObject(iPos, $999, 1, 0, 0, iCount);
  end else begin
    query.First;

    if query.Eof then
      raise ENonFatal.create('Query returned blank result');

    for u := 1 to 1 do begin
      proc.Response.AddObject($999, 1, query.FieldCount, 0, 0);
      proc.response.AddString(sQuery);
      for t:= 0 to query.FieldCount-1 do begin
//        try
          v := query.CurRecordFieldsByIdx[t];
//        except
//          v := null;
//        end;
        proc.Response.addvariant(v);
      end;
//      query.Next;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure MapQueryResult(proc: TRDTPProcessor; sQuery: ansistring; query: TSERowSet; iBaseType: integer; iBaseKeys: integer; iSubType: integer; iSubKeys: integer);overload;
begin
  MapQueryResult(proc, sQuery, query, 0, iBaseType, iBaseKeys, iSubType, iSubKeys);

end;
//------------------------------------------------------------------------------
procedure MapQueryResult(proc: TRDTPProcessor; sQuery: ansistring; query: TSERowSet; iIgnoreKeys: integer; iBaseType: integer; iBaseKeys: integer; iSubType: integer; iSubKeys: integer);
var
  t,u: integer;
  v: variant;
  icountPos: integer;
  sTemp: ansistring;
begin
  GLOG.Debug('Mapping Query Result');
  if iSubType > 0 then begin
    GLOG.Debug('Multi record mode');
    GLOG.Debug('Fields:'+inttostr(query.FieldCount));
    GLOG.Debug('Ignore:'+inttostr(iIgnoreKeys));
    GLOG.Debug('SubKeys:'+inttostr(iSubkeys));
    query.First;

    sTemp := '';
    for u := 0 to query.FieldCount-1 do begin
      sTemp := sTemp+'['+query.Fields[u].sName+']';
    end;
    GLOG.Debug(sTemp);

    iCountPos :=  proc.Response.SeqWritePos;
    proc.Response.AddLongObject(iBaseType, iBaseKeys, 0, 0, 0);
    u := 0;
    if query.Eof then begin
      for t:= iIgnoreKeys+0 to iIgnoreKeys+iBaseKeys-1 do begin
        proc.Response.AddVariant(null);
      end;
    end else begin

      for t:= iIgnoreKeys+0 to iIgnoreKeys+iBaseKeys-1 do begin
        v := query.CurRecordFieldsByIdx[t];
        proc.Response.AddVariant(v);
      end;

      //proc.response.AddString(query.SQL.text);
      while not query.Eof do begin
        proc.Response.AddObject(iSubType, iSubKeys, query.FieldCount-(iSubKeys+iIgnoreKeys), 0, 0);
        for t:= iIgnoreKeys to iIgnoreKeys+iSubKeys-1 do begin
          proc.Response.AddVariant(query.CurRecordFieldsByIdx[t]);
        end;
        for t:= iIgnoreKeys+iSubKeys to query.FieldCount-1 do begin
          proc.Response.AddVariant(query.CurRecordFieldsByIdx[t]);
        end;
        query.Next;
        inc(u);
      end;
    end;

    proc.Response.UpdateLongObject(icountPos, iBaseType, iBaseKeys, 0, 0, u);

  end else begin
    GLOG.Debug('Single record mode');
    GLOG.Debug('Fields:'+inttostr(query.FieldCount));
    GLOG.Debug('Ignore:'+inttostr(iIgnoreKeys));
    GLOG.Debug('SubKeys:'+inttostr(iSubkeys));
    sTemp := '';
    for u := 0 to query.FieldCount-1 do begin
      sTemp := sTemp+'['+query.Fields[u].sname+']';
    end;
    GLOG.Debug(sTemp);

    query.First;
    if query.Eof then
      raise ENonFatal.create('Query returned blank result');
    for u := 1 to 1 do begin
      proc.Response.AddObject(iBaseType, iBaseKeys, query.FieldCount-(iBaseKeys+iIgnoreKeys), 0, 0);
      for t:= iIgnoreKeys+0 to iIgnoreKeys+iBaseKeys-1 do begin
        proc.Response.AddVariant(query.CurRecordFieldsByIdx[t]);
      end;
      for t:= iIgnoreKeys+iBaseKeys to query.FieldCount-1 do begin
        v := query.CurRecordFieldsByIdx[t];
        proc.Response.addvariant(v);
      end;
    end;
  end;
end;




procedure RDTP_UpdateQuery(proc: TRDTPProcessor);
var
  sQuery: ansistring;
  dm: TAbstractRDTPDataModule;
begin
//  try
    dm := proc.data;
    try
      proc.request.seqread;//expect many
      sQuery := proc.Request.SeqRead;
      GLOG.Debug('UPDATE: '+sQuery);
      dm.ConnectWrite;
      dm.ExecuteWrite(sQuery);
      Replaylog.LogReplay(sQuery);


    finally
  //    dm.free;
    end;
//  except
//    on E:exception do begin
//      e.Message := 'Integrity error';
//      raise e;
//    end;
//  end;
end;

procedure RDTP_QueryMap(proc: TRDTPProcessor);
var
  sQuery: ansistring;
  dm: TAbstractRDTPDataModule;
  iIgnoreKeys, iBaseType, iSubType, iBaseKeys, iSubKeys: integer;
  ds: TSERowSet;
begin
//  try
    ds := nil;
    dm := proc.data;
    dm.ConnectRead;
    try
      //sQuery := proc.Request.SeqRead;
      sQuery := proc.Request.SeqRead;
      GLOG.Debug('---------------');
      GLOG.Debug(sQuery);

  //    dm.Connect;

//      dm.qread.Active := false;
//      dm.qread.SQL.text := sQuery;
      dm.ExecuteRead(sQuery, ds);
      Replaylog.LogReplay(sQuery);
//      dm.qread.Active := true;

      iIgnoreKeys := proc.Request.SeqRead;
      iBaseTYpe := proc.Request.SeqRead;
      iBaseKeys := proc.Request.SeqRead;
      iSubType := proc.Request.SeqRead;
      iSubKeys := proc.Request.SeqRead;

  //    AL.Lock; try;
      MapQueryResult(proc, sQuery, ds, iIgnoreKeys, iBaseType, iBasekeys, iSubType, iSubKeys);
  //    finally AL.Unlock; end;

    finally
  //    dm.free;
      ds.free;
    end;
//  except
//    on E:exception do begin
//      e.Message := 'exception during mapped query: '+e.message;
//      raise e;
//    end;
//  end;
end;


procedure RDTP_Rollback(proc: TRDTPProcessor);
var
  dm: TAbstractRDTPDataModule;
begin
  dm := proc.Data;
  try
    dm.Rollback;
  finally
  end;
end;

procedure RDTP_Commit(proc: TRDTPProcessor);
var
  dm: TAbstractRDTPDataModule;
begin
  dm := proc.Data;
  try
    dm.Commit;
  finally
  end;
end;


procedure RDTP_NoTransUpdateQuery(proc: TRDTPProcessor);
var
  sQuery: ansistring;
  dm: TAbstractRDTPDataModule;
begin
//  try
    dm := proc.data;
    try
      sQuery := proc.Request.SeqRead;
      //proc.request.seqread; //expect many
      sQuery := proc.Request.SeqRead;
      GLOG.Debug('No-Trans UPDATE: '+sQuery);
      dm.ExecuteSystem(sQuery);


//        dm.IncWrite;


    finally
  //    dm.free;
    end;
//  except
//    on E:exception do begin
//      e.Message := 'Integrity error';
//      raise e;
//    end;
//  end;
end;


procedure RDTP_GetReplayLogs(proc: TRDTPProcessor);
var
//  dm: TAbstractRDTPDataModule;
//  iKey: integer;
//  i: integer;
  s: ansistring;
  since: TDateTime;
begin
//  dm := proc.Data;
  since := proc.request.seqread;
  s := GetReplaylogs(since);
  proc.Response.AddString(s);


end;

procedure RDTP_FireForgetQuery(proc: TRDTPProcessor);
var
  sQuery: ansistring;
  dm: TAbstractRDTPDataModule;
  t,u, iCount: integer;
  v: variant;
  bSerialize: boolean;
begin
  dm := proc.data;
  try
    bSerialize := proc.request.seqread;
    sQuery := proc.Request.SeqRead;

    GLOG.Debug('Fire Forget UPDATE: '+sQuery);
    if bSerialize then begin
      LockForget;
      try
        dm.ExecuteSystem(sQuery);
      finally
        UnlockForget;
      end;
    end else begin
      dm.ExecuteSystem(sQuery);
    end;
  finally
  end;
end;


procedure oinit;
begin
  RDTPFF.DefineFunc('StorageEngineCoordinator',$1100,RDTP_Time);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$0999,RDTP_Query);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$0998,RDTP_QueryMap);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$0997,RDTP_UpdateQuery);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$0991,RDTP_NoTransUpdateQuery);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$0992,RDTP_FireForgetQuery);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$0990,RDTP_RecordQuery);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$1001,RDTP_Login);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$1004,RDTP_GetNextID);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$1005,RDTP_SetNextID);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$0000,RDTP_Rollback);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$0001,RDTP_Commit);
  RDTPFF.DefineFunc('StorageEngineCoordinator',$0711,RDTP_GetReplayLogs);
  InitializeCriticalSection(sectForget);
end;

procedure ofinal;
begin
  DEleteCriticalSection(sectForget);

end;

initialization
  init.RegisterProcs('BackGroundCommandProcessor', oinit, ofinal);



finalization


end.

