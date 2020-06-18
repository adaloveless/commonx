object frmProgress: TfrmProgress
  Left = 215
  Top = 215
  Caption = 'Progress'
  ClientHeight = 509
  ClientWidth = 879
  Color = clWindow
  Ctl3D = False
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  GlassFrame.SheetOfGlass = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = frmBaseActivate
  OnClose = frmBaseClose
  OnCreate = frmBaseCreate
  OnDblClick = frmBaseDblClick
  OnDestroy = frmBaseDestroy
  DesignSize = (
    879
    509)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl: TLabel
    Left = 8
    Top = 8
    Width = 863
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = #25628#32034#32034#24341'1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PB: TProgressBar
    Left = 8
    Top = 35
    Width = 863
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    DoubleBuffered = False
    ParentDoubleBuffered = False
    Position = 100
    BarColor = clMaroon
    Step = 1
    TabOrder = 0
  end
  object TimerWatchCommand: TTimer
    Enabled = False
    Interval = 25
    OnTimer = TimerWatchCommandTimer
    Left = 192
  end
  object TimerWatchQueue: TTimer
    Enabled = False
    Interval = 25
    OnTimer = TimerWatchQueueTimer
    Left = 312
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 1
    OnTimer = Timer1Timer
    Left = 432
    Top = 272
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 448
    Top = 72
  end
end
