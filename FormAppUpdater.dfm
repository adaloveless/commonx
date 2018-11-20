object frmUpdateProgress: TfrmUpdateProgress
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'frmUpdateProgress'
  ClientHeight = 421
  ClientWidth = 651
  Color = 63
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  DesignSize = (
    651
    421)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 105
    Width = 635
    Height = 52
    Alignment = taCenter
    Anchors = [akLeft, akRight]
    AutoSize = False
    Caption = 'Label1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -43
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitTop = 14
    ExplicitWidth = 619
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 201
    Width = 635
    Height = 11
    Anchors = [akLeft, akRight]
    BarColor = clLime
    BackgroundColor = clBlack
    TabOrder = 0
  end
  object ActivityIndicator1: TActivityIndicator
    Left = 304
    Top = 256
    Anchors = []
    Animate = True
    IndicatorColor = aicWhite
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 12
    OnTimer = Timer1Timer
    Left = 560
    Top = 224
  end
end
