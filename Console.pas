unit Console;

interface

uses Windows, Classes, StdCtrls, Controls, Graphics, Messages, Math, ClipBrd;

const
  // Constants defining the default look of the console
  CONSOLE_DEFAULT_BACKGROUND = clBlack;
  CONSOLE_DEFAULT_FOREGROUND = clWhite;
  CONSOLE_DEFAULT_FONTNAME = 'Courier New';
  CONSOLE_DEFAULT_FONTSIZE = 10;
  CONSOLE_DEFAULT_FONTSTYLE = [fsBold];

type
  // Forward
  TCustomConsole = class;

  // Events
  TBootEvent = procedure(Sender: TCustomConsole; var ABootFinished: boolean) of object;
  TShutDownEvent = procedure(Sender: TCustomConsole) of object;
  TCommandExecuteEvent = procedure(Sender: TCustomConsole; ACommand: string;
     var ACommandFinished: boolean) of object;
  TGetPromptEvent = procedure(Sender: TCustomConsole; var APrompt: string;
     var ADefaultText: string; var ADefaultCaretPos: Integer) of object;
  TCommandKeyPressEvent = procedure(Sender: TCustomConsole; var AKey: Char;
     var ATerminateCommand: boolean) of object;
  TPromptKeyPressEvent = procedure(Sender: TCustomConsole; var AKey: Char) of object;


  // TConsoleCaretType
  TConsoleCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  // TCustomConsoleLine: Represents a single line
  PConsoleLine = ^TCustomConsoleLine;
  TCustomConsoleLine = record
    IsPromptLine: boolean;        // True, if the line is a prompt line
    Prompt: string;               // Contains the prompt string of the line, if IsPromptLine is True
    Text: string;                 // If IsPromptLine is True, Text contains the typed command string, else it containes the whole line string
  end;

  // TConsoleLines
  TConsoleLines = class(TObject)
  private
    fOwner: TCustomConsole;
    fLines: TList;
    function GetLines(Index: Integer): PConsoleLine;
    procedure SetLines(Index: Integer; const Value: PConsoleLine);
    function GetCount: Integer;
    function GetCurrLine: PConsoleLine;
    procedure SetCurrLine(const Value: PConsoleLine);
    function GetWrappedLines(Index: Integer): string;
    function GetWrappedLineCount: Integer;
    function GetWrapWidth: Integer;
  protected
    function AddLine(Force: boolean = False): PConsoleLine;
    procedure Writeln(ALine: string);
    procedure Write(AText: string);
  public
    constructor Create(AOwner: TCustomConsole);
    destructor Destroy; override;

    procedure Clear;
    function IsEmptyLine(ALine: PConsoleLine): boolean;
    function GetFullLineText(ALine: Integer): string;
    function LogicalToWrappedLineIndex(ALine: Integer): Integer;

    property CurrLine: PConsoleLine read GetCurrLine write SetCurrLine;
    property Lines[Index: Integer]: PConsoleLine read GetLines write SetLines; default;
    property Count: Integer read GetCount;
    property WrappedLines[Index: Integer]: string read GetWrappedLines;
    property WrappedLineCount: Integer read GetWrappedLineCount;
    property WrapWidth: Integer read GetWrapWidth;
  end;

  // TCustomConsole
  TCustomConsole = class(TCustomControl)
  private
    fPrompt: boolean;
    fCaretX: Integer;
    fLines: TConsoleLines;
    fCaretOffset: TPoint;
    fPaintLock: Integer;
    fScrollBars: TScrollStyle;
    fMouseWheelAccumulator: integer;
    fInsertMode: boolean;
    fOverwriteCaret: TConsoleCaretType;
    fInsertCaret: TConsoleCaretType;
    FActive: boolean;
    FOnBoot: TBootEvent;
    FOnCommandExecute: TCommandExecuteEvent;
    FOnShutDown: TShutDownEvent;
    FOnGetPrompt: TGetPromptEvent;
    FBorderSize: Integer;
    FExtraLineSpacing: Integer;
    FAutoUseInsertMode: boolean;
    FOnCommandKeyPress: TCommandKeyPressEvent;
    FMinLeftCaret: Integer;
    FOnPromptKeyPress: TPromptKeyPressEvent;
    function GetCanPaste: Boolean;
    function GetFont: TFont;
    procedure SetCaretX(Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure UpdateScrollBars;
    function CaretXYPix: TPoint;
    function GetAcceptInput: boolean;
    function GetTopLine: Integer;
    function GetTextHeight: Integer;
    function RowColumnToPixels(rowcol: TPoint): TPoint;
    function LogicalToPhysicalPos(p: TPoint): TPoint;
    procedure SetInsertCaret(const Value: TConsoleCaretType);
    procedure SetInsertMode(const Value: boolean);
    procedure SetOverwriteCaret(const Value: TConsoleCaretType);
    function GetCharWidth: Integer;
    function GetLinesInWindow: Integer;
    procedure SetActive(const Value: boolean);
    function GetLines: TConsoleLines;
    procedure SetLines(const Value: TConsoleLines);
    procedure SetOnBoot(const Value: TBootEvent);
    procedure SetOnCommandExecute(const Value: TCommandExecuteEvent);
    procedure SetOnShutDown(const Value: TShutDownEvent);
    procedure SetOnGetPrompt(const Value: TGetPromptEvent);
    function GetPrompt: boolean;
    procedure SetPrompt(const Value: boolean);
    function GetCurrLine: PConsoleLine;
    procedure SetCurrLine(const Value: PConsoleLine);
    procedure SetBorderSize(const Value: Integer);
    procedure SetExtraLineSpacing(const Value: Integer);
    procedure SetAutoUseInsertMode(const Value: boolean);
    procedure SetOnCommandKeyPress(const Value: TCommandKeyPressEvent);
    procedure SetMinLeftCaret(const Value: Integer);
    procedure SetOnPromptKeyPress(const Value: TPromptKeyPressEvent);
  protected
    // Status Flags
    sfLinesChanging: boolean;

    // Windows Events
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;

    // More methods
    procedure InvalidateRect(const aRect: TRect; aErase: boolean);
    procedure DecPaintLock;
    procedure IncPaintLock;
    procedure InitializeCaret;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure PaintTextLines(AClip: TRect; FirstLine, CurrLine: integer); virtual;
    procedure InvalidateLine(Line: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
    procedure HideCaret;
    procedure ShowCaret;
    procedure DoOnPrompt(var APrompt: string; var DefaultText: string;
      var DefaultCaretPos: Integer);
    procedure KeyCommandHandler(AKey: Char);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Boot methods
    procedure Boot;
    procedure Shutdown;

    // Paintlock methods
    procedure BeginUpdate;
    procedure EndUpdate;

    // Various methods
    procedure UpdateCaret;
    procedure WndProc(var Msg: TMessage); override;
    procedure PasteFromClipboard;

    // Output methods
    procedure BeginExternalOutput;
    procedure EndExternalOutput;
    procedure Writeln(ALine: string = '');
    procedure Write(AText: string; AUpdateMinLeftCaret: boolean = False);
    procedure Clear;

    // Active: To work with TCustomConsole, Active must be set to True
    property Active: boolean read FActive write SetActive;
    // AcceptInput: If False, key events are not regognized
    property AcceptInput: boolean read GetAcceptInput;
    // Automatically changes to Insert mode when a new prompt is activated
    property AutoUseInsertMode: boolean read FAutoUseInsertMode write SetAutoUseInsertMode;
    // CharWidth: Returns the width of a single char
    property CharWidth: Integer read GetCharWidth;
    // CaretX: CaretX stores the caret position in the current line
    property CaretX: Integer read fCaretX write SetCaretX;
    // Font: Wrapped to Canvas.Font
    property Font: TFont read GetFont write SetFont;
    // Lines: Provides access to the lines
    property Lines: TConsoleLines read GetLines write SetLines;
    // InsertMode: Switch between Insert and Overwrite mode
    property InsertMode: boolean read FInsertMode write SetInsertMode;
    // Insert Caret + Overwrite Caret
    property InsertCaret: TConsoleCaretType read FInsertCaret write SetInsertCaret;
    property OverwriteCaret: TConsoleCaretType read FOverwriteCaret write SetOverwriteCaret;
    // PaintLock: PaintLock Counter; Can be modified by BeginUpdate and EndUpdate
    property PaintLock: Integer read fPaintLock;
    // CurrLine: Easy access to the current line
    property CurrLine: PConsoleLine read GetCurrLine write SetCurrLine;
    // TextHeight: Returns the high of a single text line
    property TextHeight: Integer read GetTextHeight;
    // TopLine: Stores the index of the first line visible
    property TopLine: Integer read GetTopLine;
    // LinesInWindow: Returns the number of visible lines
    property LinesInWindow: Integer read GetLinesInWindow;
    // Prompt: Returns True, if currently in prompting mode
    property Prompt: boolean read GetPrompt write SetPrompt;
    // BorderSize: Specifies the border size in pixels
    property BorderSize: Integer read FBorderSize write SetBorderSize;
    // ExtraLineSpacing: Makes it possible to add change the space between the single lines
    property ExtraLineSpacing: Integer read FExtraLineSpacing write SetExtraLineSpacing;
    // MinLeftCaret: Only available in command mode
    property MinLeftCaret: Integer read FMinLeftCaret write SetMinLeftCaret;
    // Events
    property OnBoot: TBootEvent read FOnBoot write SetOnBoot;
    property OnShutDown: TShutDownEvent read FOnShutDown write SetOnShutDown;
    property OnCommandExecute: TCommandExecuteEvent read FOnCommandExecute write SetOnCommandExecute;
    property OnGetPrompt: TGetPromptEvent read FOnGetPrompt write SetOnGetPrompt;
    property OnCommandKeyPress: TCommandKeyPressEvent read FOnCommandKeyPress write SetOnCommandKeyPress;
    property OnPromptKeyPress: TPromptKeyPressEvent read FOnPromptKeyPress write SetOnPromptKeyPress;
  end;

  TConsole = class(TCustomConsole)
  published
    // TCustomConsole properties and events
    property AutoUseInsertMode;
    property InsertMode;
    property InsertCaret;
    property OverwriteCaret;
    property BorderSize;
    property ExtraLineSpacing;
    property OnBoot;
    property OnShutDown;
    property OnCommandExecute;
    property OnGetPrompt;
    property OnCommandKeyPress;
    property OnPromptKeyPress;
    // inherited properties
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property Ctl3D;
    property ParentCtl3D;
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property Width;
    // inherited events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TCommandParser = class
  private
    FParamList: TStringList;
    FCommand: string;
    function GetParameters(Index: Integer): string;
    function GetParamCount: Integer;
  public
    constructor Create(ACommand: string); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure ParseCommand(ACommand: string);

    property Command: string read FCommand;
    property Parameters[Index: Integer]: string read GetParameters;
    property ParamCount: Integer read GetParamCount;
  end;


procedure ClearScreen;

procedure Register;

implementation

procedure Register;
Begin
  RegisterComponents('elsdoerfer.net', [TConsole]);
end;

{ TCustomConsole }

// IncPaintLock: prevents TCustomConsole from painting itself until EndUpdate is called
procedure TCustomConsole.BeginUpdate;
begin
  IncPaintLock;
end;

// CaretXYPix: Returns the caret position in pixels
function TCustomConsole.CaretXYPix: TPoint;
var p: TPoint;
begin
  // If CurrLine is nil, then exit
  if CurrLine = nil then exit;

  // Y Caret position is always the last (logical) line. However, this line may
  // be wrapped. In this case it's possible that the carets phyiscal position is
  // above the last line.
  p.Y := fLines.WrappedLineCount -
    (
      ((Length(CurrLine.Prompt + CurrLine.Text)) div (fLines.WrapWidth)) -
      ((fCaretX + Length(CurrLine.Prompt) - 1) div (fLines.WrapWidth))
    );

  // X Caret position is definied by fCaretX. Again, the line may be wrapped and
  // the caret phyiscal position differs from it's logical one.
  p.X := (fCaretX + Length(CurrLine.Prompt)) mod (fLines.WrapWidth);
  if p.X = 0 then p.X := fLines.WrapWidth;

  // Convert value to pixel and return
  Result := RowColumnToPixels(p);
end;

// Clear: Delete all lines
procedure TCustomConsole.Clear;
begin
  fLines.Clear;
  Invalidate;
end;

// Create: Constructor
constructor TCustomConsole.Create(AOwner: TComponent);
begin
  // Inherited
  inherited;
  ControlStyle := ControlStyle + [csOpaque];

  // Defaults settings
  fActive := False;
  fPrompt := False;
  fInsertMode := True;
  fBorderSize := 3;
  fExtraLineSpacing := 0;
  FAutoUseInsertMode := True;

  // Font
  Font.Name := CONSOLE_DEFAULT_FONTNAME;
  Font.Style := CONSOLE_DEFAULT_FONTSTYLE;
  Font.Size := CONSOLE_DEFAULT_FONTSIZE;
  Font.Color := CONSOLE_DEFAULT_FOREGROUND;

  // Background Color
  Color := CONSOLE_DEFAULT_BACKGROUND;

  // Default carets
  InsertCaret := ctHorizontalLine;
  OverwriteCaret := ctHalfBlock;

  // Create Lines Object
  fLines := TConsoleLines.Create(Self);
end;

// DecPaintLock: Decrements the paint lock counter
procedure TCustomConsole.DecPaintLock;
begin
  Dec(fPaintLock);
  if fPaintLock = 0 then Invalidate;
end;

// Destroy: Destructor
destructor TCustomConsole.Destroy;
begin
  // Free Lines Object
  fLines.Free;

  // Inherited
  inherited;
end;

// LogicalToPhysicalPos: Takes a logical caret position (based on line index 0)
// and makes a physical caret position out of it (based on first visible line)
function TCustomConsole.LogicalToPhysicalPos(p: TPoint): TPoint;
var s: string;
    i, L: integer;
    x: integer;
begin
  if p.Y - 1     s := fLines.WrappedLines[p.Y - 1];
    l := Length(s);
    x := 0;
    for i := 1 to p.x - 1 do begin
        inc(x);
    end;
    p.x := x + 1;
  end;
  Result := p;
end;

// RowColumnToPixels: Calculates the pixels for a certain caret position
function TCustomConsole.RowColumnToPixels(RowCol: TPoint): TPoint;
var P: TPoint;
    i: integer;
    lText: string;
begin
  P := LogicalToPhysicalPos(RowCol);
  Result.X := BorderSize;
  if ((RowCol.Y - 1)     for i := 1 to (P.X - 1) do  Begin
       lText := fLines.WrappedLines[RowCol.Y - 1];
       if lText  '' then Result.X := Result.X + Canvas.TextWidth(lText[1]);
    end;
  Result.Y := (RowCol.Y - TopLine) * TextHeight;
end;

// EndUpdate: See DecPaintLock
procedure TCustomConsole.EndUpdate;
begin
  DecPaintLock;
end;

// GetCanPaste: Returns True if text from the clipboard can be pasted
function TCustomConsole.GetCanPaste: Boolean;
begin
  Result := not AcceptInput and (Clipboard.HasFormat(CF_TEXT));
end;

// GetFont
function TCustomConsole.GetFont: TFont;
begin
  Result := Canvas.Font
end;

// GetAcceptInput: Only True, if fActive is also True
function TCustomConsole.GetAcceptInput: boolean;
begin
  Result := Active;
end;

// GetTextHeight: Calculates the height of a line
function TCustomConsole.GetTextHeight: Integer;
begin
  Result := Canvas.TextHeight('Ay%') + ExtraLineSpacing;
end;

// GetTopLine: Returns the physical line displayed on the very top of the control
function TCustomConsole.GetTopLine: Integer;
begin
  Result := Max(fLines.GetWrappedLineCount - LinesInWindow + 2, 1);
end;

// HideCaret
procedure TCustomConsole.HideCaret;
begin
  Windows.HideCaret(Handle);
end;

// IncPaintLock: Increments the Paint Lock Counter
procedure TCustomConsole.IncPaintLock;
begin
  Inc(fPaintLock);
end;

// InitializeCaret: Creates a caret object
procedure TCustomConsole.InitializeCaret;
var ct: TConsoleCaretType;
    cw, ch: integer;
begin
  // Caret type depends on keyboard mode
  if InsertMode then ct := fInsertCaret
  else ct := fOverwriteCaret;

  // Set values depending on caret type
  case ct of
    ctHorizontalLine:
      begin
        cw := CharWidth;
        ch := 2;
        fCaretOffset := Point(0, TextHeight - ExtraLineSpacing - 2);
      end;
    ctHalfBlock:
      begin
        cw := CharWidth;
        ch := (TextHeight - 2) div 2;
        fCaretOffset := Point(0, ch);
      end;
    ctBlock:
      begin
        cw := CharWidth;
        ch := TextHeight - 2;
        FCaretOffset := Point(0, 0);
      end;
    else begin    // Vertical Line
      cw := 2;
      ch := TextHeight - 2;
      FCaretOffset := Point(0, 0);
    end;
  end;

  // Create Caret
  CreateCaret(Handle, 0, cw, ch);

  // Show it
  UpdateCaret;
end;

// InvalidateLine: Calculates the rect of a certain line and uses InvalidateRect
// to repaint it (reduces flickering)
procedure TCustomConsole.InvalidateLine(Line: integer);
var rcInval: TRect;
begin
  if Visible and (Line = TopLine) and (Line      (Line   begin
    rcInval := Rect(0, TextHeight * (Line - TopLine), ClientWidth, 0);
    rcInval.Bottom := rcInval.Top + TextHeight;
    InvalidateRect(rcInval, False);
  end;
end;

// InvalidateLines: Calculates the rect of a certain line range und uses
// InvalidateRect to repaint it
procedure TCustomConsole.InvalidateLines(FirstLine, LastLine: integer);
var rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := ClientRect;
      InvalidateRect(rcInval, false);
    end else begin
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);

      if (LastLine = FirstLine) then begin
        rcInval := Rect(0, TextHeight * (FirstLine - TopLine),
                        ClientWidth, TextHeight * (LastLine - TopLine + 1));
        InvalidateRect(rcInval, false);
      end;
    end;
end;

// InvalidateRect
procedure TCustomConsole.InvalidateRect(const aRect: TRect; aErase: boolean);
begin
  Windows.InvalidateRect(Handle, @aRect, aErase);
end;

// KeyDown
procedure TCustomConsole.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_TAB, VK_END,
             VK_HOME, VK_INSERT: KeyCommandHandler(Chr(Key));
  end;
end;

// KeyPress
procedure TCustomConsole.KeyPress(var Key: Char);
begin
  inherited;
  KeyCommandHandler(Key);
end;

// Loaded
procedure TCustomConsole.Loaded;
begin
  inherited;
  UpdateScrollBars;
  Invalidate;
end;

// Paint
procedure TCustomConsole.Paint;
var rcClip, rcDraw: TRect;
    nL1, nL2: Integer;
begin
  // Only paint if Paint Lock Counter is 0
  if fPaintLock  0 then exit;

  // Get the invalidated rect
  rcClip := Canvas.ClipRect;

  // Calculate Line Range
  nL1 := Max(TopLine + ((rcClip.Top) div TextHeight), TopLine) - 1;
  nL2 := Min(TopLine + ((rcClip.Bottom + TextHeight - 1) div TextHeight), fLines.WrappedLineCount) - 1;

  // Now paint everything while the caret is hidden
  HideCaret;
  try
    rcDraw := rcClip;
    rcDraw.Left := 0;
    rcDraw.Right := ClientWidth;
    PaintTextLines(rcDraw, nL1, nL2);
  finally
    UpdateCaret;
  end;
end;

// PaintTextLines
procedure TCustomConsole.PaintTextLines(AClip: TRect; FirstLine, CurrLine: integer);
var rcLine, rcToken: TRect;

  function ColumnToXValue(Col: integer): integer;
  begin
    Result := Pred(Col) * CharWidth;
  end;

  procedure PaintLines;
  var nLine: integer;
      sLine: string;
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Bottom := (FirstLine - TopLine + 1) * TextHeight;

    // Now loop through all the lines
    for nLine := FirstLine to CurrLine do begin
      // Assign line
      sLine := Lines.WrappedLines[nLine];

      // Update the rcLine rect to this line
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, TextHeight);

      // We will need this later to fill the non-text area
      rcToken := rcLine;

      // Paint
      if not Canvas.HandleAllocated then exit;
      Brush.Color := Color;
      Brush.Style := bsSolid;
      Canvas.FillRect(Rect(0, rcLine.Top, BorderSize, rcLine.Bottom));
      Canvas.TextOut(BorderSize, rcLine.Top, sLine);
      Canvas.FillRect(Rect(BorderSize + Canvas.TextWidth(sLine), rcLine.Top, rcLine.Right, rcLine.Bottom));
    end;
  end;

begin
  // Without this painting does not work until the first time FillRect is called - no clue where the problem is
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0, 0, ClientWidth, 0));

  try
    PaintLines;
  finally
  end;

  // If there is anything visible below the last line, then fill this as well
  rcToken := AClip;
  rcToken.Top := (CurrLine + 1 - TopLine + 1) * TextHeight;
  if (rcToken.Top   begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(rcToken);
  end;
end;

// SetCaretX
procedure TCustomConsole.SetCaretX(Value: Integer);
begin
  if (Value   else if (Value   else if (CurrLine  nil) and (Value  length(CurrLine.Text) + 1) then
    Value := length(CurrLine.Text) + 1;

  if Value  fCaretX then Begin
    fCaretX := Value;
    UpdateCaret;
  end;
end;

// SetFont
procedure TCustomConsole.SetFont(const Value: TFont);
begin
  Canvas.Font.Assign(Value);
end;

procedure TCustomConsole.SetScrollBars(const Value: TScrollStyle);
begin
  { TODO : add scrollbar support }
end;

procedure TCustomConsole.ShowCaret;
begin
  Windows.ShowCaret(Handle);
end;

procedure TCustomConsole.SizeOrFontChanged(bFont: boolean);
begin
  Invalidate;
end;

procedure TCustomConsole.UpdateCaret;
var CX, CY: Integer;
    iClientRect: TRect;
begin
  CX := CaretXYPix.X + FCaretOffset.X;
  CY := CaretXYPix.Y + FCaretOffset.Y + 1;
  iClientRect := GetClientRect;
  if (CX = iClientRect.Left) and (CX      (CY = iClientRect.Top) and (CY      (AcceptInput) and (fActive) then
  begin
    SetCaretPos(CX, CY);
    ShowCaret;
  end else begin
    HideCaret;
    SetCaretPos(CX, CY);
  end;
end;

procedure TCustomConsole.UpdateScrollBars;
begin
  { TODO : add scrollbar support }
end;

procedure TCustomConsole.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TCustomConsole.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or
                DLGC_WANTTAB or DLGC_WANTALLKEYS;
end;

procedure TCustomConsole.WMHScroll(var Msg: TWMScroll);
begin
{ TODO : add scrolling support }
end;

procedure TCustomConsole.WMKillFocus(var Msg: TWMKillFocus);
begin
  HideCaret;
  Windows.DestroyCaret;
end;

procedure TCustomConsole.WMMouseWheel(var Msg: TMessage);
begin
  { TODO : add scrolling support }
end;

procedure TCustomConsole.WMPaste(var Message: TMessage);
begin
  if AcceptInput then PasteFromClipboard;
  Message.Result := ord(True);
end;

procedure TCustomConsole.WMSetFocus(var Msg: TWMSetFocus);
begin
  InitializeCaret;
end;

procedure TCustomConsole.WMSize(var Msg: TWMSize);
begin
  SizeOrFontChanged(False);
end;

procedure TCustomConsole.WMVScroll(var Msg: TWMScroll);
begin
  { TODO : add scrolling support }
end;

procedure TCustomConsole.WndProc(var Msg: TMessage);
begin
  inherited;
end;

procedure TCustomConsole.SetInsertCaret(const Value: TConsoleCaretType);
begin
  FInsertCaret := Value;
end;

procedure TCustomConsole.SetInsertMode(const Value: boolean);
begin
  if fInsertMode  Value then Begin
    fInsertMode := Value;
    InitializeCaret;
  end;
end;

procedure TCustomConsole.SetOverwriteCaret(const Value: TConsoleCaretType);
begin
  FOverwriteCaret := Value;
end;

function TCustomConsole.GetCharWidth: Integer;
begin
  Result := Canvas.TextWidth('a');
end;

function TCustomConsole.GetLinesInWindow: Integer;
begin
  Result := ClientHeight div TextHeight;
end;

// Set Active: Use this to boot or to shutdown the console
procedure TCustomConsole.SetActive(const Value: boolean);
var BootFinished: boolean;
begin
  if (Value  fActive) then Begin
    case Value of
      True:
        begin
          // Clear current lines
          Clear;

          // Add a first, empty line
          fLines.AddLine;

          // Call onBoot Event
          BootFinished := True;
          if Assigned(FOnBoot) then FOnBoot(Self, BootFinished);
          if BootFinished then Prompt := True;

          InitializeCaret;
        end;

      False:
        begin
          if Assigned(FOnShutDown) then FOnShutDown(Self);
        end;
    end;
    fActive := Value;
  end;
end;

procedure TCustomConsole.PasteFromClipboard;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

function TCustomConsole.GetLines: TConsoleLines;
begin
  Result := fLines;
end;

procedure TCustomConsole.SetLines(const Value: TConsoleLines);
begin
  fLines := Value;
end;

procedure TCustomConsole.Writeln(ALine: string);
begin
  if not Prompt then Begin
    fLines.Writeln(ALine);
    fCaretX := length(ALine) + 1;
    Invalidate;
  end;
end;

procedure TCustomConsole.Boot;
begin
  Active := True;
end;

procedure TCustomConsole.Shutdown;
begin
  Active := False;
end;

procedure TCustomConsole.SetOnBoot(const Value: TBootEvent);
begin
  FOnBoot := Value;
end;

procedure TCustomConsole.SetOnCommandExecute(const Value: TCommandExecuteEvent);
begin
  FOnCommandExecute := Value;
end;

procedure TCustomConsole.SetOnShutDown(const Value: TShutDownEvent);
begin
  FOnShutDown := Value;
end;

procedure TCustomConsole.SetOnGetPrompt(const Value: TGetPromptEvent);
begin
  FOnGetPrompt := Value;
end;

procedure TCustomConsole.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  Windows.SetFocus(Handle);
end;

function TCustomConsole.GetPrompt: boolean;
begin
  Result := fPrompt;
end;

procedure TCustomConsole.SetPrompt(const Value: boolean);
begin
  if Value  fPrompt then Begin
    fPrompt := Value;

    if Value = True then Begin
      CaretX := 1;
      MinLeftCaret := 1;
      with fLines.AddLine^ do Begin
        IsPromptLine := True;
        DoOnPrompt(Prompt, Text, fCaretX);
        CaretX := fCaretX;    // just for valid range checking
      end;
      if AutoUseInsertMode then InsertMode := True;
      ShowCaret;
      Invalidate;
    end else Begin
      fLines.AddLine;
    end;  // end else
  end; // end if
end;

procedure TCustomConsole.Write(AText: string; AUpdateMinLeftCaret: boolean = False);
begin
  if not Prompt then Begin
    fLines.Write(AText);
    CaretX := length(CurrLine.Text) + 1;
    if (AUpdateMinLeftCaret) then MinLeftCaret := length(CurrLine.Text) + 1;
  end;
end;

function TCustomConsole.GetCurrLine: PConsoleLine;
begin
  Result := fLines.CurrLine;
end;

procedure TCustomConsole.SetCurrLine(const Value: PConsoleLine);
begin
  fLines.CurrLine := Value;
  Invalidate;
end;

{ TConsoleLines }

function TConsoleLines.AddLine(Force: boolean = False): PConsoleLine;
begin
  if (IsEmptyLine(CurrLine)) and (not Force) then Result := CurrLine
  else Begin
    New(Result);
    Result.IsPromptLine := False;
    Result.Prompt := '';
    Result.Text := '';
    fLines.Add(Result);
  end;
end;

procedure TConsoleLines.Clear;
var i: integer;
begin
  try
    for i := 0 to fLines.Count -1 do Dispose(fLines[i]);
  finally
    fLines.Clear;
  end;
end;

constructor TConsoleLines.Create(AOwner: TCustomConsole);
begin
  fLines := TList.Create;
  fOwner := AOwner;
end;

destructor TConsoleLines.Destroy;
begin
  try
    Clear;
  finally
    fLines.Free;
  end;

  inherited;
end;

function TConsoleLines.GetCount: Integer;
begin
  Result := fLines.Count;
end;

function TConsoleLines.GetFullLineText(ALine: Integer): string;
begin
  Result := Lines[ALine].Prompt + Lines[ALine].Text;
end;

function TConsoleLines.GetCurrLine: PConsoleLine;
begin
  if fLines.Count = 0 then Result := nil
  else Result := fLines[fLines.Count - 1];
end;

function TConsoleLines.GetLines(Index: Integer): PConsoleLine;
begin
  Result := fLines[Index];
end;

procedure TConsoleLines.SetCurrLine(const Value: PConsoleLine);
begin
  fLines[fLines.Count - 1] := Value;
end;

procedure TConsoleLines.SetLines(Index: Integer;
  const Value: PConsoleLine);
begin
  fLines[Index] := Value;
end;

procedure TConsoleLines.Write(AText: string);
begin
  CurrLine.Text := CurrLine.Text + AText;
end;

procedure TConsoleLines.Writeln(ALine: string);
begin
  CurrLine.Text := CurrLine.Text + ALine;
  AddLine(True);
end;

procedure TCustomConsole.SetBorderSize(const Value: Integer);
begin
  FBorderSize := Value;
end;

procedure TCustomConsole.SetExtraLineSpacing(const Value: Integer);
begin
  FExtraLineSpacing := Value;
end;

procedure TCustomConsole.BeginExternalOutput;
begin
  Prompt := False;
end;

procedure TCustomConsole.EndExternalOutput;
begin
  Prompt := True;
end;

function TConsoleLines.IsEmptyLine(ALine: PConsoleLine): boolean;
begin
  Result := (ALine  nil) and (not ALine.IsPromptLine) and (ALine.Text = '');
end;

function TConsoleLines.GetWrappedLines(Index: Integer): string;
var i, j, c: integer;
    FullLine: string;
begin
  c := 0;
  Result := '';
  for i := 0 to fLines.Count - 1 do Begin
     Inc(c, (Length(Lines[i].Prompt + Lines[i].Text) div WrapWidth) + 1);
     if c  Index then Begin
       Dec(c, (Length(Lines[i].Prompt + Lines[i].Text) div WrapWidth) + 1);
       FullLine := Lines[i].Prompt + Lines[i].Text;
       j := c;
       while j          Delete(FullLine, 1, WrapWidth);
         inc(j);
       end; // end while
       Result := Copy(FullLine, 1, WrapWidth);
       break;
     end;  // end if
  end;  // end for
end;

function TConsoleLines.GetWrappedLineCount: Integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to fLines.Count - 1 do
    Result := Result + ((Length(Lines[i].Prompt + Lines[i].Text)) div WrapWidth ) + 1;
end;

function TConsoleLines.GetWrapWidth: Integer;
begin
  Result := (TCustomConsole(fOwner).ClientWidth -
             TCustomConsole(fOwner).BorderSize * 2) div TCustomConsole(fOwner).CharWidth;
end;

function TConsoleLines.LogicalToWrappedLineIndex(ALine: Integer): Integer;
begin
  Result := 0;
end;

procedure TCustomConsole.DoOnPrompt(var APrompt, DefaultText: string;
  var DefaultCaretPos: Integer);
begin
  if Assigned(fOnGetPrompt) then
    fOnGetPrompt(Self, APrompt, DefaultText, DefaultCaretPos)
  else Begin
    APrompt := ' ';
  end;
end;

procedure TCustomConsole.KeyCommandHandler(AKey: Char);
var CommandFinished: boolean;
    ATerminate: boolean;
begin
  // If we are not in prompt mode, filter keys by user event
  ATerminate := False;
  if not (Prompt) and (Assigned(fOnCommandKeyPress)) then
    fOnCommandKeyPress(Self, AKey, ATerminate)
  else if (Assigned(fOnPromptKeyPress)) then fOnPromptKeyPress(Self, AKey);

  // Case Key
  case Ord(AKey) of
    VK_RETURN:
      Begin
        if Prompt then Begin  // prompt mode
          BeginExternalOutput;
          CommandFinished := True;
          if Assigned(fOnCommandExecute) then
            fOnCommandExecute(Self, fLines[fLines.Count - 2].Text, CommandFinished);
          if CommandFinished and fActive then EndExternalOutput;
          Invalidate;
        end;
      end;

    VK_BACK:
      Begin
        if (Prompt) or (CaretX  MinLeftCaret) then Begin
          if (CaretX  length(CurrLine.Text)) then
             Delete(CurrLine.Text, Length(CurrLine.Text), 1)
          else Delete(CurrLine.Text, CaretX - 1, 1);
          CaretX := CaretX - 1;
          Invalidate;
        end;
      end;

    VK_LEFT: CaretX := CaretX - 1;
    VK_RIGHT: CaretX := CaretX + 1;
    VK_END: CaretX := length(CurrLine.Text) + 1;
    VK_HOME: CaretX := 1;

    VK_INSERT:
      Begin
        InsertMode := not InsertMode;
        InitializeCaret;
      end;

    VK_UP, VK_DOWN, VK_TAB:
      Begin
        // Should Be Handled By An Event { TODO : todo }
      end;

    else if (AKey  #0) then // Insert char
      Begin
        if (CaretX  length(CurrLine.Text)) then CurrLine.Text := CurrLine.Text + AKey
        else if InsertMode then Insert(AKey, CurrLine.Text, CaretX)      // Insert Mode
        else Begin                                                       // Overwrite Mode
          Delete(CurrLine.Text, CaretX, 1);
          Insert(AKey, CurrLine.Text, CaretX);
        end;

        inc(fCaretX);
        Invalidate;
      end;
  end;

  if (ATerminate) then EndExternalOutput;
end;

procedure TCustomConsole.SetAutoUseInsertMode(const Value: boolean);
begin
  FAutoUseInsertMode := Value;
end;

procedure TCustomConsole.SetOnCommandKeyPress(
  const Value: TCommandKeyPressEvent);
begin
  FOnCommandKeyPress := Value;
end;

{ TCommandParser }

constructor TCommandParser.Create(ACommand: string);
begin
  Create;
  ParseCommand(ACommand);
end;

constructor TCommandParser.Create;
begin
  FParamList := TStringList.Create;
end;

destructor TCommandParser.Destroy;
begin
  FParamList.Free;
  inherited;
end;

function TCommandParser.GetParamCount: Integer;
begin
  Result := FParamList.Count;
end;

function TCommandParser.GetParameters(Index: Integer): string;
begin
  if Index = 0 then Result := Command
  else Result := FParamList[Index - 1];
end;

procedure TCommandParser.ParseCommand(ACommand: string);
var t: string;
    Apost: boolean;
    i: integer;

    procedure StoreSubString(AString: string);
    Begin
      if (AString  '') then
        // Store string (first substring as command, the others as parameters)
        if (FCommand = '') then FCommand := AString
        else FParamList.Add(AString);
    end;
begin
  // Reset
  FParamList.Clear;
  FCommand := '';
  Apost := False;

  // Parse
  i := 1;
  while (i     case ACommand[i] of
      ' ': if not APost then Begin
             StoreSubString(t);
             t := '';
           end else t := t + ACommand[i];

      '"': Apost := not Apost;

      else t := t + ACommand[i];
    end;
    inc(i);
  end;
  StoreSubString(t);
end;

procedure TCustomConsole.SetMinLeftCaret(const Value: Integer);
begin
  FMinLeftCaret := Value;
  if CaretX  FMinLeftCaret then CaretX := FMinLeftCaret;
end;

procedure TCustomConsole.SetOnPromptKeyPress(
  const Value: TPromptKeyPressEvent);
begin
  FOnPromptKeyPress := Value;
end;


end.
