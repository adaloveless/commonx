unit SoundGUIControls;

interface

uses
  soundinterfaces, Classes,StdCtrls,ExtCtrls, Controls, graphics, soundtools, windows, sysutils, betterobject, Generics.Collections.fixed, advancedgraphics, soundtools_easywindows, colorblending;

const
  AA = 4;
  BUTTON_WIDTH = 55;
  BUTTON_MARGIN = 1;


type
  iptr = ^integer;

  TSoundMarker = class(TBetterObject)
  private
    FStart: int64;
    FEnd: int64;
  protected
  public
    property StartPoint: int64 read FStart write FStart;
    property EndPoint: int64 read FEnd write FEnd;
  end;

  TPolarity = (polPos, polNeg, polZero);
  TSoundGUISTate = (ssBase, ssZoomIn);
  TSoundEditor = class (TPanel)
  private
    FViewStart: int64;
    FViewend: int64;
    FData: pointer;
    FSelectEnd: int64;
    FSelectStart: int64;
    FMouseIsDown: boolean;
    FNoPaint: boolean;
    FSampleStart: int64;
    FSampleend: int64;
    FLoopStart: int64;
    FLoopend: int64;
    FStream: TMemoryStream;
    FMarkers: TList<TSoundMarker>;
    FSampleRate: int64;

    procedure SetViewEnd(Value: int64);
    procedure SetViewStart(Value: int64);
    procedure SetData(const Value: pointer);
    function GetDataLength: int64;
    function GetSampleLength: int64;
    procedure SetState(const Value: TSoundGUIState);
    procedure SetSelectEnd(const Value: int64);
    procedure DrawWave;
    procedure SetNoPaint(const Value: boolean);
    procedure SetLoopEnd(const Value: int64);
    procedure SetLoopStart(const Value: int64);
    procedure SetSampleEnd(const Value: int64);
    procedure SetSampleStart(const Value: int64);
    procedure SetSelectStart(const Value: int64);
    function GetStream: TMemoryStream;
  public
    scrollbar: TScrollBar;
    Fstate: TSoundGUIState;
    btnZoomIn: TButton;
    btnZoomOut: TButton;
    btnMark: TButton;
    btnUnMark: TButton;
    btnUnSelect: TButton;
    btnTweakInLeft: TButton;
    btnTweakInRight: TButton;
    btnTweakOutLeft: TButton;
    btnTweakOutRight: TButton;
    btnTweakLoopInLeft: TButton;
    btnTweakLoopInRight: TButton;
    btnTweakLoopOutLeft: TButton;
    btnTweakLoopOutRight: TButton;

    btnPLay: TButton;
    lblDebug: TLabel;
    RefreshingScrollbar: boolean;
    FSamples: array of smallint;

    constructor create(aowner: TComponent); reintroduce;virtual;
    destructor Destroy;override;
    procedure Paint;override;
    property Data: pointer read FData write SetData;
    property Stream: TMemoryStream read GetStream;
    procedure LoadFromFile(sBoogerFile: ansistring);
    procedure SaveToFile(sBoogerFile: ansistring);
    procedure SaveLoop(sBoogerFile: ansistring);

    property DataLength: int64 read GetDataLength;
    property SAmpleLength: int64 read GetSampleLength;
    property SampleRAte: int64 read FSampleRate write FSampleRate;

    procedure PlayClick(sender: TObject);
    procedure ZoomInClick(sender: TObject);
    procedure ZoomOutClick(sender: TObject);
    procedure MarkClick(sender: TObject);
    procedure UnMarkClick(sender: TObject);
    procedure UnSelectClick(sender: TObject);
    procedure TweakInLeftClick(sender: TObject);
    procedure TweakInRightClick(sender: TObject);
    procedure TweakInLoopLeftClick(sender: TObject);
    procedure TweakInLoopRightClick(sender: TObject);
    procedure TweakOutLeftClick(sender: TObject);
    procedure TweakOutRightClick(sender: TObject);
    procedure TweakOutLoopLeftClick(sender: TObject);
    procedure TweakOutLoopRightClick(sender: TObject);


    procedure ScrollBarChange(sender: TObject);

  published
    property ViewStart: int64 read FViewStart write SetViewStart;
    property ViewEnd:   int64 read FViewend write SetViewEnd;
    property SampleStart: int64 read FSampleStart write SetSampleStart;
    property SampleEnd:   int64 read FSampleend write SetSampleEnd;
    property LoopStart: int64 read FLoopStart write SetLoopStart;
    property LoopEnd:   int64 read FLoopend write SetLoopEnd;

    procedure Box(x1,y1,x2,y2: integer; color: TColor);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    function GetSampleOnDisplay(x: real): integer;

    property SelectStart: int64 read FSelectStart write SetSelectStart;
    property SelectEnd: int64 read FSelectEnd write SetSelectEnd;

    property MouseIsDown: boolean read FMouseIsDown;
    property State: TSoundGUIState read FState write SetState;
    procedure Debug(sMessage: ansistring);
    function VIEW_HEIGHT: integer;
    function BUTTON_TOP: integer;
    procedure ZoomTo(iStart, iEnd: int64);
    procedure REfreshScrollbar;
    procedure Resize; override;
    property NoPaint: boolean read FNoPaint write SetNoPaint;

    function SearchForZeroCrossing(iStart: int64; bLeft, bRight: boolean): int64;
    procedure UnMark;

    procedure PlayScrub(iStart: integer; iLength: integer = (44100 div 2));
    procedure PlayLoop(iRepeats: integer = 2);


  end;



implementation

uses EasyImage, Geometry, systemx;

{ TSoundEditor }

procedure TSoundEditor.Box(x1, y1, x2, y2: integer; color: TColor);
begin
  canvas.brush.Color := color;
  canvas.pen.color := color;
  canvas.FillRect(Rect(x1,y1,x2+1,y2+1));

end;

function TSoundEditor.BUTTON_TOP: integer;
begin
  result := VIEW_HEIGHT+10;
end;

constructor TSoundEditor.create(aowner: TComponent);
begin
  inherited;
  FMarkers:= TList<TSoundMarker>.create();

  FullRepaint := false;

  doublebuffered := true;
  SelectSTart := -1;
  scrollbar := TScrollBar.create(self);
  scrollbar.Parent := self;
  scrollbar.Align := alBottom;
  scrollbar.OnChange := self.ScrollbarChange;

  //zoomin button
  btnZoomIn := TButton.create(self);
  with btnZoomIn do begin
    left := 10;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := '&Zoom In';
    OnClick := self.ZoomInClick;
    anchors :=  [akLeft,akBottom];
  end;

  //zoomout button
  btnZoomOut := TButton.create(self);
  with btnZoomOut do begin
    left := btnZoomIn.left+btnZoomIn.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := 'Zoom &Out';
    OnClick := self.ZoomOutClick;
    anchors :=  [akLeft,akBottom];
  end;
  //mark spot
  btnMark := TButton.create(self);
  with btnMark do begin
    left := btnZoomOut.left+btnZoomOut.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := '&Mark';
    anchors :=  [akLeft,akBottom];
    OnClick := self.MarkClick;
  end;

  //unmark
  btnUnMark := TButton.create(self);
  with btnUnMark do begin
    left := btnMark.left+btnMark.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := '&UnMark';
    anchors :=  [akLeft,akBottom];
    OnClick := self.UnMarkClick;
  end;

  //unselect
  btnUnSelect := TButton.create(self);
  with btnUnSelect do begin
    left := btnUnMark.left+btnUnMark.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := '&UnSelect';
    anchors :=  [akLeft,akBottom];
    OnClick := self.UnSelectClick;
  end;


  //TweakInLeft
  btnTweakInLeft := TButton.create(self);
  with btnTweakInLeft do begin
    left := btnUnSelect.left+btnUnSelect.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := 'In<';
    anchors :=  [akLeft,akBottom];
    OnClick := self.TweakInLeftClick;
  end;

  //TweakInRight
  btnTweakInRight := TButton.create(self);
  with btnTweakInRight do begin
    left := btnTweakInLeft.left+btnTweakInLeft.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := 'In>';
    anchors :=  [akLeft,akBottom];
    OnClick := self.TweakInRightClick;
  end;

  //TweakLoopInLeft
  btnTweakLoopInLeft := TButton.create(self);
  with btnTweakLoopInLeft do begin
    left := btnTweakInRight.left+btnTweakInRight.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := 'LoopIn<';
    anchors :=  [akLeft,akBottom];
    OnClick := self.TweakInLoopLeftClick;
  end;

  //TweakLoopInRight
  btnTweakLoopInRight := TButton.create(self);
  with btnTweakLoopInRight do begin
    left := btnTweakLoopInLeft.left+btnTweakLoopInLeft.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := 'LoopIn>';
    anchors :=  [akLeft,akBottom];
    OnClick := self.TweakInLoopRightClick;
  end;

  //TweakLoopoutLeft
  btnTweakLoopOutLeft := TButton.create(self);
  with btnTweakLoopOutLeft do begin
    left := btnTweakLoopInRight.left+btnTweakLoopInRight.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := 'LoopOut<';
    anchors :=  [akLeft,akBottom];
    OnClick := self.TweakOutLoopLeftClick;
  end;

  //TweakLoopoutRight
  btnTweakLoopOutRight := TButton.create(self);
  with btnTweakLoopOutRight do begin
    left := btnTweakLoopOutLeft.left+btnTweakLoopOutLeft.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := 'LoopOut>';
    anchors :=  [akLeft,akBottom];
    OnClick := self.TweakOutLoopRightClick;
  end;

  //TweakoutLeft
  btnTweakOutLeft := TButton.create(self);
  with btnTweakOutLeft do begin
    left := btnTweakLoopOutRight.left+btnTweakLoopOutRight.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := 'Out<';
    anchors :=  [akLeft,akBottom];
    OnClick := self.TweakOutLeftClick;
  end;

  //TweakoutRight
  btnTweakOutRight := TButton.create(self);
  with btnTweakOutRight do begin
    left := btnTweakOutLeft.left+btnTweakOutLeft.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := 'Out>';
    anchors :=  [akLeft,akBottom];
    OnClick := self.TweakOutRightClick;
  end;

  //play loop
  btnPlay := TButton.create(self);
  with btnPlay do begin
    left := btnTweakOutRight.left+btnTweakOutRight.width+BUTTON_MARGIN;
    width := BUTTON_WIDTH;
    top := BUTTON_TOP;
    parent := self;
    caption := '&Play';
    OnClick := self.PlayClick;
    anchors :=  [akLeft,akBottom];
  end;

  lblDebug := TLabel.create(self);
  lblDebug.parent := self;
  lblDebug.top := VIEW_HEIGHT;
  lblDebug.left := btnPlay.left+btnPlay.width+BUTTON_MARGIN;
  lblDebug.anchors :=  [akLeft,akBottom];


end;

procedure TSoundEditor.Debug(sMessage: ansistring);
begin
  lblDebug.caption := sMessage;
end;

destructor TSoundEditor.destroy;
begin
  while FMarkers.count > 0 do begin
    FMarkers[FMarkers.count-1].free;
    FMarkers.delete(FMarkers.count-1);
  end;
  inherited;
end;

function TSoundEditor.GetDataLength: int64;
begin
  if FData = nil then
    result := 0
  else
    result := iptr(FData)^;

  if ViewEnd > result then
    ViewEnd := result;
  if ViewStart > result then
    ViewStart := result;

end;

function TSoundEditor.GetSampleLength: int64;
begin
  result := (GetDataLength) div 2;
end;

function TSoundEditor.GetSampleOnDisplay(x: real): integer;
begin
  if x>SampleLength then
    x := SampleLength;
  result := round((ViewEnd-ViewStart)*(x/width))+ViewStart;
end;

procedure TSoundEditor.MarkClick(sender: TObject);
begin
  if SampleStart = -1 then begin
    SampleStart := SelectStart;
    SampleEnd := SelectEnd;
  end;

  Loopstart := SelectStart;
  LoopEnd := SelectEnd;


end;

procedure TSoundEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin

  inherited;
  case button of
    mbLeft: if y < VIEW_HEIGHT then begin
              self.SelectStart := GetSampleOnDisplay(x);

              FMouseIsDown := true;
            end;
    mbRight:if y < VIEW_HEIGHT then begin
              self.PlayScrub(GetSampleOnDisplay(x));
            end;
  end;

end;

procedure TSoundEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FMouseIsDown then begin
    SelectEnd := GetSampleOnDisplay(x);
    //PlayScrub(GetSampleOnDisplay(x));
  end;

  if y < self.VIEW_HEIGHT then begin
    if ssRight in shift then begin
      PlayScrub(GetSampleOnDisplay(x));
    end;

  end;


end;

procedure TSoundEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin

//  windows.beep(800,10);
  if FMouseIsDown then
    self.SelectEnd := GetSampleOnDisplay(x);
  FMouseIsDown := false;

  inherited;
end;

procedure SortREals(var r1,r2,r3,r4: real);
var
  a: array[0..3] of real;
  t: integer;
  bSorted: boolean;
begin
  a[0] := abs(r1);
  a[1] := abs(r2);
  a[2] := abs(r3);
  a[3] := abs(r4);

  bSorted := false;
  while not bSorted do begin
    bSorted := true;
    for t:= 0 to 2 do begin
      if a[t] < a[t+1] then begin
        r1 := a[t];
        a[t+1] := a[t];
        a[t] := r1;
        bSorted := false;
      end;
    end;
  end;

  r1 := a[0];
  r2 := a[1];
  r3 := a[2];
  r4 := a[3];



end;

procedure TSoundEditor.DrawWave;
var
  t: integer;
  r: real;
  amp: real;
  val: smallint;
  x: integer;
  amp1,amp2,amp3,amp4: real;
  c1,c2,c3,c4: TColor;
  hlColor: Tcolor;
  bhighLight: boolean;
  bg: TColor;
  endtext: integer;
begin
  inherited;
  if NoPAint Then Exit;

//  Box(0,0,width-1, 100, clTeal);

  if FData = nil then
    exit;

  if FViewEnd = FViewStart then
    FViewEnd := FViewStart+1;

  bg := Colorblend(clTeal, clBlack,0.5);
  c1 := ColorBlend(bg, clWhite, 0.20);
  c2 := ColorBlend(bg, clWhite, 0.40);
  c3 := ColorBlend(bg, clWhite, 0.60);
  c4 := ColorBlend(bg, clWhite, 0.80);
  for t:=0 to width-1 do begin
    x := GetSampleOnDisplay(t);
    amp1 := FSamples[GetSampleOnDisplay(t)]/32767;
    amp2 := FSamples[GetSampleOnDisplay(t+0.25)]/32767;
    amp3 := FSamples[GetSampleOnDisplay(t+0.50)]/32767;
    amp4 := FSamples[GetSampleOnDisplay(t+0.75)]/32767;
    sortreals(amp1,amp2,amp3,amp4);

    hlColor := clBlack;
    bHighLight := false;
    if (SelectStart > -1) and (x>=SelectStart) and (x<=SelectEnd) then begin
      bHighLight := true;
      if hlColor = clBlack then
        hlcolor := clWhite
      else
        hlColor := ColorBlend(hlColor, clWhite, 0.5);
    end;
    if (SampleStart > -1) and (x>=SampleStart) and (x<=SampleEnd) then begin
      bHighLight := true;
      bHighLight := true;
      if hlColor = clBlack then
        hlcolor := clGreen
      else
        hlColor := ColorBlend(hlColor, clGreen, 0.5);
    end;
    if (LoopStart > -1) and (x>=LoopStart) and (x<=LoopEnd) then begin
      bHighLight := true;
      bHighLight := true;
      if hlColor = clBlack then
        hlcolor := clYellow
      else
        hlColor := ColorBlend(hlColor, clYellow, 0.5);
    end;




    if bHighLight then begin
      Box(t,0,t, VIEW_HEIGHT, ColorBlend(bg, hlColor,0.5));
      box(t,(VIEW_HEIGHT div 2)-round(amp1*(VIEW_HEIGHT div 2)),t,(VIEW_HEIGHT div 2)+round(amp1*(VIEW_HEIGHT div 2)), ColorBlend(c1, hlColor, 0.5));
      box(t,(VIEW_HEIGHT div 2)-round(amp2*(VIEW_HEIGHT div 2)),t,(VIEW_HEIGHT div 2)+round(amp2*(VIEW_HEIGHT div 2)), ColorBlend(c2, hlColor, 0.5));
      box(t,(VIEW_HEIGHT div 2)-round(amp3*(VIEW_HEIGHT div 2)),t,(VIEW_HEIGHT div 2)+round(amp3*(VIEW_HEIGHT div 2)), ColorBlend(c3, hlColor, 0.5));
      box(t,(VIEW_HEIGHT div 2)-round(amp4*(VIEW_HEIGHT div 2)),t,(VIEW_HEIGHT div 2)+round(amp4*(VIEW_HEIGHT div 2)), ColorBlend(c4, hlColor, 0.5));

    end else begin
      Box(t,0,t, VIEW_HEIGHT, bg);
      box(t,(VIEW_HEIGHT div 2)-round(amp1*(VIEW_HEIGHT div 2)),t,(VIEW_HEIGHT div 2)+round(amp1*(VIEW_HEIGHT div 2)), c1);
      box(t,(VIEW_HEIGHT div 2)-round(amp2*(VIEW_HEIGHT div 2)),t,(VIEW_HEIGHT div 2)+round(amp2*(VIEW_HEIGHT div 2)), c2);
      box(t,(VIEW_HEIGHT div 2)-round(amp3*(VIEW_HEIGHT div 2)),t,(VIEW_HEIGHT div 2)+round(amp3*(VIEW_HEIGHT div 2)), c3);
      box(t,(VIEW_HEIGHT div 2)-round(amp4*(VIEW_HEIGHT div 2)),t,(VIEW_HEIGHT div 2)+round(amp4*(VIEW_HEIGHT div 2)), c4);

    end;

  end;

  endtext := -1;
  for t:=0 to width-1 do begin
    if t < endtext then begin
      continue;
    end;
    x := GetSampleOnDisplay(t);
    endtext := t+self.Canvas.TextWidth('|'+inttostr(x)+'   ');
    self.Canvas.pen.Mode := pmXor;
    self.Canvas.TextOut(t, VIEW_HEIGHT-self.canvas.TextHeight('0'),'|'+inttostr(x)+'   ');

  end;


end;

procedure TSoundEditor.PlayClick(sender: TObject);
begin
  if LoopStart > -1 then begin
    self.PlayLoop;
  end else
  if SelectStart > -1 then begin
    SoundTools_easywindows.PlayRawSoundMono(PAnsiChar(self.Data)+4+(SelectStart*2), (SelectEnd-SelectStart)*2);
  end else
    SoundTools_easywindows.PlayRawSoundMono(PAnsiChar(self.Data)+4, DataLength);

end;

procedure TSoundEditor.REfreshScrollbar;
begin
  RefreshingScrollbar := true;
  try
    self.scrollbar.Min := 0;
    if self.scrollbar.Position > SampleLength then begin
      scrollbar.position := 0;
      self.scrollbar.PageSize := 1;
    end;
    self.scrollbar.Position := ViewStart;
    self.scrollbar.PageSize := ViewEnd-ViewStart;
    self.scrollbar.MAx := SampleLength;

  finally
    RefreshingScrollbar := false;
  end;

end;

procedure TSoundEditor.Resize;
begin
  inherited;
  DrawWave;
end;

procedure TSoundEditor.ScrollBarChange(sender: TObject);
var
  ps: int64;
begin
  if not RefreshingScrollbar then begin
    ps := scrollbar.PageSize;
    self.ViewStart := scrollbar.Position;
    self.ViewEnd := ViewStart+ps;
  end;
end;

procedure TSoundEditor.SetData(const Value: pointer);
var
  p2: pointer;
begin
  FData := Value;
  p2 := pointer(PAnsiChar(FData)+4);
  FSamples := p2 ;

//  FSamples := @PAnsiChar(PAnsiChar(@PAnsiChar(FData)[0])+SOUND_IDX_DATA)[0];
  FViewEnd := SampleLength;
  FViewStart := 0;
//  UnMark;
  ReFreshScrollbar;
  Invalidate;
end;

procedure TSoundEditor.SetSelectEnd(const Value: int64);
begin
  FSelectEnd := Value;
  Debug(inttostr(selectstart)+'-'+inttostr(selectend));
  DrawWave;
end;

procedure TSoundEditor.SetState(const Value: TSoundGUIState);
begin
  FState := Value;
  case FState of
    ssZoomIn: begin
      btnZoomIn.Enabled := false;
      invalidate;
    end;
  else
    btnZoomIn.Enabled := true;
  end;
end;

procedure TSoundEditor.SetViewEnd(Value: int64);
begin
  if Value > SampleLength-1 then
    Value := SampleLength-1;
  if value = FViewEnd then
    exit;

  FViewEnd := Value;
  RefreshScrollbar;
  DrawWave;
end;

procedure TSoundEditor.SetViewStart(Value: int64);
begin
  if value < 0 then
  value := 0;
  if value = FViewStart then
    exit;

  FViewStart := Value;
  RefreshScrollbar;
  DrawWave;
end;

function TSoundEditor.VIEW_HEIGHT: integer;
begin
  result := height - (btnZoomIn.Height + 30);

end;

procedure TSoundEditor.ZoomInClick(sender: TObject);
begin
  if SelectStart > -1 then
    ZoomTo(SelectStart, SelectEnd)
  else
    ZoomTo(0, SampleLength);




end;

procedure TSoundEditor.ZoomOutClick(sender: TObject);
var
  viewwidth: int64;
  viewcenter: int64;
begin
  viewwidth := ViewEnd-ViewStart;
  viewcenter := ViewStart+(viewwidth div 2);
  ZoomTo(viewCenter-(viewwidth), viewcenter+(viewwidth));

end;

procedure TSoundEditor.ZoomTo(iStart, iEnd: int64);
const
  steps = 750;
var
  t: integer;
  iStartStart, iEndStart: integer;
  tm1, tm2: cardinal;
begin
  iStartStart := ViewStart;
  iEndStart := ViewEnd;

  tm1 := GetTickCount;
  while tm2-tm1 <> steps do begin
    tm2 := GEtTickCount;
    if tm2 < tm1 then tm1 := tm2;
    if tm2-tm1 > steps then
      tm2 := tm1+steps;
    t := tm2-tm1;


    NoPaint := true;
    ViewStart := Round(Geometry.Interpolate(t, steps, iStartStart, iStart));
    ViewEnd := Round(Geometry.Interpolate(t, steps, iEndStart, iEnd));
    NoPaint := false;

  end;

  ViewStart := iStart;
  ViewEnd := iEnd;

end;

procedure TSoundEditor.Paint;
begin
  inherited;
  DrawWave;
end;

procedure TSoundEditor.SetNoPaint(const Value: boolean);
begin
  if (FNoPaint <> Value)
  and (not value) then begin
    FNoPaint := Value;
    DrawWave;
  end else
    FNoPaint := Value;

end;

procedure TSoundEditor.SetLoopEnd(const Value: int64);
begin
  FLoopend := Value;
  DrawWave;
end;

procedure TSoundEditor.SetLoopStart(const Value: int64);
begin
  FLoopStart := Value;
  DrawWave;
end;

procedure TSoundEditor.SetSampleEnd(const Value: int64);
begin
  FSampleend := Value;
  DrawWave;
end;

procedure TSoundEditor.SetSampleStart(const Value: int64);
begin
  FSampleStart := Value;
  DrawWave;
end;

procedure TSoundEditor.UnMarkClick(sender: TObject);
begin
  UnMark;
end;

procedure TSoundEditor.TweakInLeftClick(sender: TObject);
begin
  SampleStart := self.SearchForZeroCrossing(SampleStart, true, false);
end;

procedure TSoundEditor.TweakInLoopLeftClick(sender: TObject);
begin
  LoopStart := self.SearchForZeroCrossing(LoopStart, true, false);
end;

procedure TSoundEditor.TweakInLoopRightClick(sender: TObject);
begin
  LoopStart := self.SearchForZeroCrossing(LoopStart, false, true);
end;

procedure TSoundEditor.TweakInRightClick(sender: TObject);
begin
  SampleStart := self.SearchForZeroCrossing(SampleStart, false, true);
end;


function TSoundEditor.SearchForZeroCrossing(iStart: int64; bLeft,
  bRight: boolean): int64;
var
  i: integer;
  amp, ampL, ampR: smallint;
  bNeg: boolean;
  iPos: int64;
  iPos2: int64;
  bZeroFound: boolean;
begin
  //record the initial amp
  amp := FSamples[iStart];
  //flip the polarity if negative
  if amp < 0 then begin
    amp := 0-amp;
    bNeg := true;
  end else bNeg := false;


  bZeroFound := false;
  if bLeft xor bRight then
    i := 1
  else
    i := 0;
  //while not zero found
  while not bZeroFound do begin

    //if look left
    if bLeft then begin
      //get left sample
      iPos := iStart - i;
      iPos2 := iPos;
      if iPos < 0 then begin
        iPos := 0;
        if bLeft xor bRight then break;
      end;

      ampL := FSamples[iPos];
      //flip the polarity if original sample was negative
      if bNeg then ampL := 0-ampL;

      //if the original sample + this sample < the original sample
      //or this sample is 0 then its a zero crossing
      if (ampL <= 0) then begin
        bZeroFound := true;
        result := iPos;
        break;
      end;
    end;

    //if look right
    if bRight then begin
      //get left sample
      iPos := iStart+i;

      if iPOs > SampleLength-1 then BEGIN
        iPos := SampleLength-1;
        if bLeft xor bRight then break;
      END;
      ampR := FSamples[iStart+i];
      //flip the polarity if original sample was negative
      if bNeg then ampR := 0-ampR;

      //if the original sample + this sample < the original sample
      //or this sample is 0 then its a zero crossing
      if (ampR <= 0) then begin
        bZeroFound := true;
        result := iPos;
        break;
      end;
    end;


    //increment offset
    inc(i);
    if bLeft and bright then
    if (iPos2 = 0) and (iPos = SampleLength-1) then begin
      result := 0;
      break;
    end;

  end;

end;

procedure TSoundEditor.UnMark;
begin
// SelectStart := -1;
  SampleStart := -1;
  LoopStart := -1;

end;

procedure TSoundEditor.TweakOutLeftClick(sender: TObject);
begin
  SampleEnd := self.SearchForZeroCrossing(SampleEnd, true, false);
end;

procedure TSoundEditor.TweakOutLoopLeftClick(sender: TObject);
begin
  LoopEnd := self.SearchForZeroCrossing(LoopEnd, true, false);
end;

procedure TSoundEditor.TweakOutLoopRightClick(sender: TObject);
begin
  LoopEnd := self.SearchForZeroCrossing(LoopEnd, false,true);
end;

procedure TSoundEditor.TweakOutRightClick(sender: TObject);
begin
  SampleEnd := self.SearchForZeroCrossing(SampleEnd, false,true);
end;

procedure TSoundEditor.UnSelectClick(sender: TObject);
begin
  SelectStart := -1;
  DrawWave;
end;

procedure TSoundEditor.SetSelectStart(const Value: int64);
begin
  FSelectStart := Value;
  DrawWave;
end;

procedure TSoundEditor.PlayScrub(iStart, iLength: integer);
begin
  if iStart+iLength > SampleLength then
    iLength := SampleLength-iStart;

  Soundtools_easywindows.PlayRawSoundMono(@Fsamples[iStart], iLength, SampleRate);

end;

procedure TSoundEditor.PlayLoop(iRepeats: integer);
var
  temp: array of smallint;
  idx: integer;
  off: integer;
  u,t: integer;
  a,b,c: smallint;
  p: real;
  sample_blend: integer;
  procedure ArrayWrite(amp: smallint);
  begin
    if length(temp) < idx+1 then
      setlength(temp, idx+1);
    temp[idx] := amp;
    inc(idx);
  end;
begin
  idx := 0;

  //calc length
  //SetLength(temp, (SampleEnd-SampleStart+1)-((iRepeats-1)*(LoopEnd-LoopStart+1)));
  SetLength(temp, 0);
  sample_blend := (LoopEnd-LoopStart) div 2;
  if sample_blend > 1500 then
    sample_blend := 1500;

  //loop from sample start to loop start fading in volume
  for t:= SampleStart to LoopStart-1 do begin
    a := FSamples[t];
    a := round(Interpolate(t, 0, a, sampleStart, LoopStart-1));
    ArrayWrite(a);
  end;

  //loop through repeated section
  for u:= 1 to iRepeats do begin
    //begin -- first X samples
    if u = 1 then begin
      for t:= LoopStart to LoopStart+sample_blend-1 do begin
        a := FSamples[t];
        ArrayWrite(a);
      end;
    end;

    //always the middle
    for t:= LoopStart+sample_blend to (Loopend-sample_blend)-1 do begin
      a := FSamples[t];
      ArrayWrite(a);
    end;

    //all but last blend back to beginning
    if (u < iRepeats) then begin
      for t:= Loopend-sample_blend to LoopEnd do begin
        off := t-(LoopEnd-Sample_blend);
        a := FSamples[t];
        b := FSamples[LoopStart+off];
        p := off/sample_blend;
        p := p;
        c := round(Interpolate(p, 1, a,b));
        ArrayWrite(c);
      end;
    end
    //last play tail unfaded
    else begin
      for t:= Loopend-sample_blend to LoopEnd do begin
        a := FSamples[t];
        ArrayWrite(a);
      end;
    end;
  end;

  //loop from loop end to sample end fading OUT volume
  for t:= Loopend+1 to SampleEnd-1 do begin
    a := FSamples[t];
    a := round(Interpolate(t, a, 0, LoopEnd, SampleEnd-1));
    ArrayWrite(a);
  end;


//  if idx <> length(temp) then
//    raise exception.create('IDX='+inttostr(idx-1)+' length(temp)='+inttostr(length(temp)));
  PlayRawSoundmono(@temp[0], length(temp)*2, SampleRAte);



end;

function TSoundEditor.GetStream: TMemoryStream;
begin
  result := FStream;
end;

procedure TSoundEditor.LoadFromFile(sBoogerFile: ansistring);
begin
  NoPaint := true;
  try

    if FData <> nil then begin
      fSamples := nil;
      FreeMem(FData);
      FData := nil;
      //FSamples := Nil;
      FSampleStart := -1;
      FSampleEnd := -1;
      FloopStart := -1;
      FLoopend := -1;
      FViewStart := 0;
      FViewend := 0;
    end;

    FStream := SoundTools.LoadSoundIntoMemory(sBoogerFile);
    try
      GetMem(self.FData, integer(FStream.Size));
      MoveMem32(FData, FStream.Memory, FStream.Size);
      FSampleStart := iptr(PAnsiChar(@PAnsiChar(FData)[0])+SOUND_IDX_CUE1)^;
      FSampleEnd := iptr(PAnsiChar(@PAnsiChar(FData)[0])+SOUND_IDX_CUE2)^;
      FLoopStart := iptr(PAnsiChar(@PAnsiChar(FData)[0])+SOUND_IDX_Loop1)^;
      FLoopEnd := iptr(PAnsiChar(@PAnsiChar(FData)[0])+SOUND_IDX_Loop2)^;
      FViewStart := 0;
      FViewEnd := SampleLength-1;
      self.Data := self.FData;

    finally
      FStream.Free;
    end;
  finally
    NoPaint := false;
  end;

end;

procedure TSoundEditor.SaveLoop(sBoogerFile: ansistring);
begin
  soundtools.SaveSoundData(sBoogerFile, @FSamples[SampleStart], 2*(SampleEnd-SampleStart+1), 0,SampleEnd-SampleStart+1, LoopStart-SampleStart, LoopEnd-SampleStart);
end;

procedure TSoundEditor.SaveToFile(sBoogerFile: ansistring);
begin
  soundtools.SaveSoundData(sBoogerFile, @FSamples[0], DAtaLength, SampleStart, SampleEnd, LoopStart, LoopEnd);

end;

end.
