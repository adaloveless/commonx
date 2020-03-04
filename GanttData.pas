unit GanttData;

interface


uses
  classes, types,tickcount, typex, stringx, systemx, sysutils, dxtypes, debug, anoncommand, commandprocessor, numbers;

const
  OVERLAP_CHECK_WIDTH = 1000;
type


//  70.166.249.130 - - [13/Feb/2020:06:25:08 +0000]
// "GET /fastcrib2/notificationMessage/ajaxGetNotifications?YII_CSRF_TOKEN=b2b4742f58c3dcc2473c62379b006d36f58448bc&user_id=2274&company_id=44&firstRunOnPage=false&_=1581566878285 HTTP/1.1"
// 200
// 3307
// "https://fastcrib.com/fastcrib2/customerPartCribMap/index?CustomerPartCribMap%5Bid%5D=&CustomerPartCribMap%5Bcu2_location_search%5D=&CustomerPartCribMap%5Bcu3_crib_search%5D=&CustomerPartCribMap%5Bbi1_id_default%5D=&CustomerPartCribMap%5Btotal_bins_search%5D=&CustomerPartCribMap%5Bpa3_customer_part_number_search%5D=&CustomerPartCribMap%5Bpa3_manufacturer_item_number_search%5D=&CustomerPartCribMap%5Bpreferred_supplier_search%5D=&CustomerPartCribMap%5Bpa3_customer_part_description_search%5D=&CustomerPartCribMap%5Bpa4_total_available_quantity_search%5D=&CustomerPartCribMap%5Bpa4_total_available_quantity_search_status%5D=&CustomerPartCribMap%5Bpa4_total_on_hand_quantity_search%5D=&CustomerPartCribMap%5Bpa3_package_quantity_search%5D=&CustomerPartCribMap%5Bcurrency_converted_price%5D=&CustomerPartCribMap%5Bcr1_id_location%5D=&CustomerPartCribMap%5Bpa3_unit_search%5D=&CustomerPartCribMap%5Bpa4_total_min_quantity_search%5D=&CustomerPartCribMap%5Bpa4_total_max_quantity_search%5D=&CustomerPartCribMap%5Bor2_total_order_quantity_search%5D=&CustomerPartCribMap%5Bimage_link_search%5D=&CustomerPartCribMap%5Bcu5_active%5D=&CustomerPartCribMap%5Bpa3_is_kit_template_search%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_1%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_2%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_3%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_4%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_5%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_6%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_7%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_8%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_9%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_10%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_11%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_12%5D=&CustomerPartCribMap%5Bpa3_custom_attribute_13%5D=&CustomerPartCribMap%5Bcu5_consignment%5D=&CustomerPartCribMap_page=1&yt4="
// "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"
//  IP                                     c
//date
//0=url/command
//1=code
//2=time
//3=referer
//4=UserAgent

  TganttRecord = record
    ip: string;
    dateutc: string;
    head: string;
    resultcode: string;
    runtime: ni;
    referer: string;
    date: TDateTime;
    runtimef: TDateTime;
    useragent: string;
    track: ni;
    sz: int64;
    procedure FromLogLine(sLine: string);
    procedure Decode;
    function endtime: TDateTime;
  end;

  PGanttRecord = ^TGanttRecord;

  TGanttData = record
    minx,maxx: TDateTime;
    startday: TDateTime;
    maxtrack: ni;
    recs: TArray<TGanttRecord>;
    selected: PGanttRecord;
    Procedure LoadFromfile(sfile: string);
    function Loaded: boolean;
    procedure Analyze;
    procedure CheckOverlap(idx: ni;width: ni;bReset: boolean; multiplytime: ni);
    procedure CalcMaxTrack;
    procedure SearchPoint(x,y: double);
  end;

  PGanttData = ^TGanttData;





implementation

{ TganttRecord }

procedure TganttRecord.Decode;
begin
  var l,r,day,month,year,h,m,s,ms: string;
  //13/Feb/2020:06:25:08 +0000
  SplitString(dateutc, '-', year, month);
  SplitString(month, '-', month, day);
  SplitString(day, ' ', day, h);
  SplitString(h, ':', h,m);
  SplitString(m, ':', m,s);
  SplitString(s, '.', s,ms);
  SplitString(ms, ' ', ms,r);
  ms := zcopy(ms, 0,4);




  //EncodeDate(strtoint(year),strtoint(month), strtoint(day));


  var time: TDateTime := (strtoint(h)/24)+(strtoint(m)/(24*60))+(strtoint(s)/(24*60*60)+(strtoint(ms)/(24*60*60*1000)));
  var date: TdateTime := EncodeDate(strtoint(year), MonthToInt(month), strtoint(day));
  self.date := date+time-(6/24);

  runtimef := ((runtime / 1000)/(24*60*60));


end;

function TganttRecord.endtime: TDateTime;
begin
  result := date + runtimef;
end;

procedure TganttRecord.FromLogLine(sLine: string);
begin
  var l,r: string;
  sLine := StringReplace(sLine, ' - - ', ' ', []);
  var slh := ParseStringNotInH(sLine,' ', '"');
  ip := slh.o[0];
  dateutc := slh.o[1]+' '+slh.o[2];
  head:= unquote(slh.o[3]);
  resultcode := slh.o[4];
  runtime := strtoint64(slh.o[5]);
  sz := strtoint64(slh.o[6]);
  referer := slh.o[7];
  useragent := slh.o[8];
  decode;

end;

{ TGanttData }

procedure TGanttData.Analyze;
begin
  minx := recs[0].date;
  startday := trunc(minx);
  maxx := (recs[high(recs)].date)+recs[high(recs)].runtimef;

{x$DEFINE NO_CHECK_OVERLAP}
{$IFNDEF NO_CHECK_OVERLAP}
{x$DEFINE NOMTOVERLAP}
{$IFDEF NOMTOVERLAP}
  for var t:= 0 to high(recs) do begin
    if t and $FFF = 0 then
      Debug.Log('Checking overlaps '+t.ToString);
    CheckOverlap(t,OVERLAP_CHECK_WIDTH);
  end;
{$ELSE}
  //ITERATION 1
  begin
    var cx := length(recs);
    var idx: ni := 0;

    var pg: PGanttData := @self;
    var cl := TCommandList<TCommand>.create;
    try
      while cx > 0 do begin
        var group := (length(recs) div GetEnabledCPUCount)+1;
        var ac := InlineIteratorGroupProc(idx, lesserof(group,cx),
          procedure (idx: ni) begin
            pg.CheckOverlap(idx,OVERLAP_CHECK_WIDTH, true, 0);
          end
        );
        cl.Add(ac);
        inc(idx, group);
        dec(cx, group);
      end;

      cl.WaitForAll_DestroyWhileWaiting;

    finally
      cl.free;
    end;
  end;
  //ITERATION 2
  begin
    var cx := length(recs);
    var idx: ni := 0;

    var pg: PGanttData := @self;
    var cl := TCommandList<TCommand>.create;
    try
      while cx > 0 do begin
        var group := (length(recs) div GetEnabledCPUCount)+1;
        var ac := InlineIteratorGroupProc(idx, lesserof(group,cx),
          procedure (idx: ni) begin
            pg.CheckOverlap(idx,OVERLAP_CHECK_WIDTH, false, 1);
          end
        );
        cl.Add(ac);
        inc(idx, group);
        dec(cx, group);
      end;

      cl.WaitForAll_DestroyWhileWaiting;

    finally
      cl.free;
    end;
  end;
{$ENDIF}
{$ENDIF}
  CalcMaxTrack;
end;

procedure TGanttData.CalcMaxTrack;
begin
  maxtrack :=0;
  for var t := 0 to high(recs) do begin
    maxtrack := greaterof(maxtrack,recs[t].track);
  end;
end;

procedure TGanttData.CheckOverlap(idx: ni;width: ni;bReset: boolean; multiplytime: ni);
var
  myrec : PganttRecord;
begin
  myrec := @recs[idx];
  var usewidth := (myrec.runtime*multiplytime)+width;

  var track:ni := 0;
  if not bReset then
    track := myrec.track;
  var bFoundTrack := false;
  while not bFoundTrack do begin
    bFoundTrack := true;
    //make sure that nothing overlaps us on this track..
    //if collision is found, increment track and start over
    for var t := greaterof(0,idx - usewidth) to lesserof(high(recs), idx+usewidth) do begin

      if t = idx then continue;

      var check: PGanttRecord := @recs[t];

      if check.track <> track then
        continue;

      if check.date > myrec.endtime then
        continue;

      if check.endtime < myrec.Date then
        continue;

      bFoundTrack := false;
      inc(track);
      break;
    end;
  end;

  myrec.track := track;


end;

function TGanttData.Loaded: boolean;
begin
  result := length(recs)> 0;
end;

procedure TGanttData.LoadFromfile(sfile: string);
begin
  selected := nil;
  var recs := self.recs;
  var sl := TStringList.create;
  try
    try
      sl.LoadFromFile(sfile);
    except
      on E: exception do begin
        Debug.Log(e.message);
      end;
    end;
    sl.delete(0);
    setlength(recs, sl.count);
    var tmStart := GetTicker;

{$IFNDEF NOMT}
    var cx := sl.count;
    var idx: ni := 0;


    var cl := TCommandList<TCommand>.create;
    try
      while cx > 0 do begin
        var group := (sl.count div GetEnabledCPUCount)+1;
        var ac := InlineIteratorGroupProc(idx, lesserof(group,cx),
          procedure (idx: ni) begin
            recs[idx].FromLogLine(sl[idx]);
          end
        );
        cl.Add(ac);
        inc(idx, group);
        dec(cx, group);
      end;

      cl.WaitForAll_DestroyWhileWaiting;

    finally
      cl.free;
    end;
{$ELSE}

    for var t:= SL.COUNT-2 to sl.count-1 do begin
      recs[t].FromLogLine(sl[t]);
    end;
{$ENDIF}

    self.recs := recs;
    Analyze;
    Debug.Log('Loaded in '+inttostr(gettimesince(tmStart)));


  finally
    sl.free;
  end;

end;

procedure TGanttData.SearchPoint(x,y: double);
begin
  for var t := 0 to high(recs) do begin
    var prec : PGanttRecord := @recs[t];
    if (prec.date < X)
    and (prec.endtime > x)
    and (prec.track = trunc(y)) then begin
      selected := prec;
      exit;
    end;
  end;

end;

end.
