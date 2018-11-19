unit BetterListView;

interface
uses
  Windows
  , Classes
  , Controls
  , Messages
  , ComCtrls
  , CommCtrl
  ;

type
  TColumnEvent = procedure (ASender: TObject; const AColIndex: Integer) of object;
  TColumnSizeEvent = procedure (ASender: TObject; const AColIndex, AColWidth: Integer) of object;
  
  TBetterListView=class(TListView)
  private
    FOnColumnBeginResize: TColumnEvent;
    FOnColumnResizing: TColumnSizeEvent;
    FOnColumnResized: TColumnEvent;
    procedure HandleWMNotify(var AMsg: TWMNotify); message WM_NOTIFY;
  protected
    procedure DoColumnResized(const AColIndex: Integer);
    procedure DoColumnBeginResize(const AColIndex: Integer);
    procedure DoColumnResizing(const AColIndex, AWidth: Integer);
  published
    // event that will be raised when a column starts resizing. The column's index
    // will be indicated in AColIndex parameter
    property OnColumnBeginResize: TColumnEvent read FOnColumnBeginResize write FOnColumnBeginResize;
    // event that will be raised when a column is resizing. The column's index
    // will be indicated in AColIndex parameter, and the current width indicated
    // in AColWidth parameter
    property OnColumnResizing   : TColumnSizeEvent read FOnColumnResizing write FOnColumnResizing;
    // event that will be raised when a column just finished resizing. The
    // column's index will be indicated in AColIndex parameter
    property OnColumnResized    : TColumnEvent read FOnColumnResized write FOnColumnResized;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CodeCall', [TBetterListView]);
end;

{ TBetterListView }

procedure TBetterListView.DoColumnBeginResize(const AColIndex: Integer);
begin
  if Assigned(FOnColumnBeginResize) then
    FOnColumnBeginResize(Self, AColIndex);
end;

procedure TBetterListView.DoColumnResized(const AColIndex: Integer);
begin
  if Assigned(FOnColumnResized) then
    FOnColumnResized(Self, AColIndex);
end;

procedure TBetterListView.DoColumnResizing(const AColIndex, AWidth: Integer);
begin
  Columns[AcolIndex].Width := awidth;
  if Assigned(FOnColumnResizing) then
    FOnColumnResizing(Self, AColIndex, AWidth);
end;

procedure TBetterListView.HandleWMNotify(var AMsg: TWMNotify);
var
  vColWidth: Integer;
begin
  inherited;
  case PHDNotify(AMsg.NMHdr)^.Hdr.Code of
  
    HDN_ENDTRACKA, HDN_ENDTRACKW:
      DoColumnResized(PHDNotify(AMsg.NMHdr)^.Item);

    HDN_BEGINTRACKA, HDN_BEGINTRACKW:
      DoColumnBeginResize(PHDNotify(AMsg.NMHdr)^.Item);

    HDN_TRACKA, HDN_TRACKW, -320:
      begin
        vColWidth := -1;
        if (PHDNotify(AMsg.NMHdr)^.PItem<>nil)
          and (PHDNotify(AMsg.NMHdr)^.PItem^.Mask and HDI_WIDTH <> 0)
        then
         vColWidth := PHDNotify(AMsg.NMHdr)^.PItem^.cxy;

        DoColumnResizing(PHDNotify(AMsg.NMHdr)^.Item, vColWidth);
      end;

  end;
end;

end.
 