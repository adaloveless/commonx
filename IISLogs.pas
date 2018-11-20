unit IISLogs;

interface

uses
  typex, stringx, classes, betterobject, better_collections, webstring;

//   SAMPLE
//#Software: Microsoft Internet Information Services 8.5
//#Version: 1.0
//#Date: 2016-03-11 19:35:43
//#Fields: date time s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs(User-Agent) cs(Referer) sc-status sc-substatus sc-win32-status time-taken
//2016-03-11 19:35:43 10.0.0.194 GET /appcontent/jamstikPlus/TuningInfo.json p=635933217485172260 443 - 216.228.112.21 Dalvik/2.1.0+(Linux;+U;+Android+6.0;+SHIELD+Tablet+X1+Build/MRA58K) - 401 2 5 109
//2016-03-11 19:35:43 10.0.0.194 GET /appcontent/jamstikPlus/ChordDatabase.json p=635933217485228010 443 - 216.228.112.21 Dalvik/2.1.0+(Linux;+U;+Android+6.0;+SHIELD+Tablet+X1+Build/MRA58K) - 401 2 5 93
//[date]     [time]   [ip]       [method] [file]                                [vars]               [port] [-] [clientip] [useragent] [-] [result] [stat1] [stat2] [stat3]

//[date]
//[time]
//[ip]
//[method]
//[file]
//[vars]
//[port] [-]
//[clientip]
//[useragent] [-]
//[result]
//[stat1]
//[stat2]
//[stat3]
type
  TLogEntry = record
    date: string;
    time: string;
    ip: string;
    method: string;
    filename: string;
    vars: string;
    port: string;
    dash1: string;
    clientip: string;
    useragent: string;
    dash2: string;
    resultcode: string;
    stat1,stat2,stat3: string;
    hits: nativeint;
    procedure ParseFrom(sLine: string);
  end;

  TLogEntryObject = class(TbetterObject)
  public
    data: TLogEntry;
  end;

  TLogList = class(TBetterList<TLogEntryObject>)
  public
    procedure Clear;
    procedure AddFromString(sLine: string);
    procedure AddRolledUp_ByFileName(sLine: string);
    function IndexOfFile(sFile: string): ni;
    procedure AddFromfile(sfile: string);
    procedure SortByHits;
  end;




implementation

{ TLogEntry }

procedure TLogEntry.ParseFrom(sLine: string);
var
  sl: TStringlist;
begin
  sl := nil;
  try
    sl := ParseString(sLine, ' ');
    date := sl[0];
    time := sl[1];
    ip := sl[2];
    method := sl[3];
    filename := sl[4];
    vars := sl[5];
    port := sl[6];
    dash1 := sl[7];
    clientip := sl[8];
    useragent := sl[9];
    dash2 := sl[10];
    resultcode := sl[11];
    stat1 := sl[12];
    stat2 := sl[13];
    stat3 := sl[14];
    hits := 1;


  finally
    sl.free;
  end;

end;

{ TLogList }

procedure TLogList.AddFromfile(sfile: string);
var
  sl: TStringlist;
  t: ni;
begin
  sl := TStringList.create;
  try
    sl.LoadFromFile(sFile);
    for t:= 0 to sl.count-1 do begin
      self.AddRolledUp_ByFileName(sl[t]);
    end;
  finally
    sl.free;
  end;

end;

procedure TLogList.AddFromString(sLine: string);
var
  l: TLogEntryObject;
begin
  if zcopy(sLine, 0, 1) = '#' then
    exit;

  l := TLogEntryObject.create;
  l.data.ParseFrom(sLine);
  add(l);


end;

procedure TLogList.AddRolledUp_ByFileName(sLine: string);
var
  le: TLogEntry;
  leo: TLogEntryObject;
  idx: ni;
begin
  if zcopy(sLine, 0, 1) = '#' then
    exit;

  le.ParseFrom(sLine);
  idx := IndexOfFile(le.filename);
  if idx >= 0 then begin
    inc(items[idx].data.hits);
  end else begin
    leo := TLogEntryObject.create;
    leo.data := le;
    add(leo);
  end;

end;

procedure TLogList.Clear;
var
  l: TLogEntryObject;
begin
  while count > 0 do begin
    l := items[count-1];
    delete(count-1);
    l.free;
    l := nil;
  end;

end;

function TLogList.IndexOfFile(sFile: string): ni;
var
  t: ni;
  leo: TLogEntryObject;
begin
  result := -1;
  for t:= 0 to count-1 do begin
    leo := items[t];
    if leo.data.filename = sFile then begin
      exit(t);
    end;
  end;
end;

procedure TLogList.SortByHits;
  function SortPass: boolean;
  var
    t: ni;
    leo: TLogEntryObject;
  begin
    result := false;
    for t := 0 to count-2 do begin
      if items[t].data.hits < items[t+1].data.hits then begin
        leo := items[t];
        items[t] := items[t+1];
        items[t+1] := leo;
        result := true;
      end;
    end;
  end;
begin
  while sortpass do ;

end;

{ TLogEntryObject }


{ TLogEntryObject }


end.
