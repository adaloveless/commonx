unit osm_renderer;

interface

uses
  memoryfilestream, easyimage, typex,systemx, advancedgraphics2, colorconversion, classes, osm_xml_parser, graphics, osm, commandprocessor, sysutils;


const
  COLOR_MOTORWAY = clWhite;
  COLOR_PRIMARY = clYEllow;
  COLOR_SECONDARY = clRed;
  COLOR_URBAN = clCYAN;
  COLOR_Water = clBlue;
type
  TMapBox = record
    w,n,e,s: double;
  end;
  TMapBoxFIle = record
    mb: TMapBox;
    fetched: boolean;
    data: TXMLDrawWorkingData;
  end;
  PMapBoxFile = ^TMapBoxFile;
  TOSMRenderer = class(advancedgraphics2.TDrawingBoard)
  protected
    getr: osm.TOSM_Getter;
    files: array of TMapBoxFile;
    procedure DrawWay(pw: PWayData);
    procedure DrawFile(mb: PMapBoxFile;sKey: string; sValue: string);
  public
    watch_command: TCommand;
    procedure AddFile(w,n,e,s: double);
    procedure Init;override;
    destructor Destroy;override;

    procedure DoDraw;override;

  end;


implementation

{ TOSMRenderer }


procedure TOSMRenderer.AddFile(w, n, e, s: double);
begin
  setlength(files, length(files)+1);
  files[high(files)].mb.w := w;
  files[high(files)].mb.n := n;
  files[high(files)].mb.e := e;
  files[high(files)].mb.s := s;

end;

destructor TOSMRenderer.Destroy;
var
  t: ni;
begin


  inherited;
end;

procedure TOSMRenderer.DoDraw;
var
  t: ni;
begin

  inherited;
  Self.New;
  self.clear(clGreen);
  //bounds should be set by user of class

  watch_command.STepCount := high(files);
  for t:= 0 to high(files) do begin
    watch_command.step := t;
    drawfile(@files[t], 'waterway','');
  end;

  for t:= 0 to high(files) do begin
    drawfile(@files[t], 'highway','residential');
  end;
  for t:= 0 to high(files) do begin
    drawfile(@files[t], 'highway','tertiary');
  end;
  for t:= 0 to high(files) do begin
    drawfile(@files[t], 'highway','secondary');
  end;
  for t:= 0 to high(files) do begin
    drawfile(@files[t], 'highway','primary');
  end;
  for t:= 0 to high(files) do begin
    drawfile(@files[t], 'highway','trunk');
  end;
  for t:= 0 to high(files) do begin
    drawfile(@files[t], 'highway','motorway');
  end;


end;

procedure TOSMRenderer.DrawFile(mb: PMapBoxFile; sKey: string; sValue: string);
var
  w,n: ni;
  pw: PWayData;
  sRes: string;
begin
  sREs := 'osm'+floattostr(mb.mb.w)+','+floattostr(mb.mb.n)+','+floattostr(mb.mb.e)+','+floattostr(mb.mb.s);
  try
    if not mb.fetched then begin
      mb.data := getr.GetSnapBinaryData(mb.mb.w,mb.mb.n,mb.mb.e,mb.mb.s, watch_command);
      mb.fetched := true;
    end;


    //to draw shit, have bounds for drawing area set before calling this function
    for w := 0 to high(mb.data.ways) do begin
      pw := @mb.data.ways[w];
      if pw.HasTag(sKEy) then begin
        if (sVAlue = '') or (pw.GetTagValue(sKey) = sValue) then begin
          DrawWay(pw);
        end else begin
          //DrawWay(pw);
        end;
      end;
    end;
  finally
  end;


end;

procedure TOSMRenderer.DrawWay(pw: PWayData);
var
  t: ni;
  c: TColor;
  s: string;
begin
  c := clBlack;
  if pw.HasTag('highway') then begin
    s := lowercase(pw.GetTagValue('highway', 'motorway'));
    if s = 'motorway' then begin
      c := COLOR_MOTORWAY;
    end else
    if s = 'trunk' then begin
      c := COLOR_MOTORWAY;
    end else
    if s = 'primary' then begin
      c := COLOR_PRIMARY;
    end else
    if s = 'secondary' then begin
      c := COLOR_SECONDARY;
    end else
      c := COLOR_URBAN;
  end else begin
    if pw.HasTag('waterway') then begin
      IF pw.GetTagValue('waterway','stream') <> 'stream' then
        c := clBlue;
    end;
  end;

  if c <> clBlack then
  for t:= 1 to high(pw.nodes) do begin
//    self.FatLine(pw.nodes[t-1].lon, 0-pw.nodes[t-1].lat, pw.nodes[t].lon, 0-pw.nodes[t].lat,3,clWhite, true);
//    self.AlphaOp := aoAdd;
    self.fatline(pw.nodes[t-1].lon, 0-pw.nodes[t-1].lat, pw.nodes[t].lon, 0-pw.nodes[t].lat,1, c,true);
  end;

end;

procedure TOSMRenderer.Init;
begin
  inherited;
  setlength(files,0);
  ConstrainProportions := false;
  getr := TOSM_Getter.create;
end;

end.
