unit dmx_timeline;

interface

uses
  generictimeline, classes, stringx, typex, sysutils, graphics, numbers;

const
  TL_COLORS: array of TColor = [clRed, clLime, clBlue, clFuchsia, clYellow, claqua, clGray];
type
  TDMXTimeLine = class(TTimeLine)
  public
    procedure LoadfromScript(s: string);overload;
    procedure LoadFromScript(sl: TStringlist);overload;

  end;

implementation

{ TDMXTimeLine }

procedure TDMXTimeLine.LoadfromScript(s: string);
var
  sl: TStringlist;
begin
  sl := tStringlist.create;
  try
    sl.text := s;
    LoadFromScript(sl);
  finally
    sl.free;
  end;

end;

procedure TDMXTimeLine.LoadfromScript(sl: TStringlist);
var
  slp: TStringlist;
  t: ni;
  sFirstChar: string;
  s: string;
  nl: TTimeLIneLine;
  iLastTime: int64;
  iThisTime: int64;
  lp: TTimeLinePoint;
  max: nativefloat;
begin
  iLAstTime := -1;
  nl := nil;
  lp := nil;
  slp := TStringlist.create;
  self.clearelements;
  max := 1;
  Tempo := 60;
  try
    for t:= 0 to sl.Count-1 do begin
      s := sl[t];

      sFirstChar := zcopy(s,0,1);
      if sFirstChar = '*' then begin
        nl := self.AddLIne;
        nl.BGColor := TL_COLORS[(linecount-1) mod high(TL_COLORS)];
        nl.color := clYellow;
      end else begin
        if nl<>nil then
        begin
          PArseString(s, ',', slp);
          iThisTime := strtoint64(slp[0]);
          if iThisTime <> iLastTime then begin
            lp := self.AddPoint(iThisTime / 1000, LIneCount-1, tlcStartLoop);
            lp.Data := s;
            if (iThisTime/1000)> MAX then begin
              max := iThisTime/1000;
              targetboundx2 := max;
            end;
          end else begin
            if lp<>nil then
              lp.Data := lp.DAta + #13#10+s;
          end;
          iLAstTime := iThisTime;

        end;
      end;


    end;
  finally
    slp.free;
  end;

  targetboundy2 := greaterof(16,linecount);
  targetboundx1 := 0;


end;

end.
