object frmSplash: TfrmSplash
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Loading... Please Wait...'
  ClientHeight = 331
  ClientWidth = 570
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  GlassFrame.SheetOfGlass = True
  OldCreateOrder = False
  OnClose = frmBaseClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  Right = 569
  Bottom = 330
  PixelsPerInch = 96
  TextHeight = 13
  object gi: TImage
    Left = 232
    Top = 112
    Width = 105
    Height = 105
  end
  object pb: TProgressBar
    Left = 0
    Top = 314
    Width = 570
    Height = 17
    Align = alBottom
    TabOrder = 0
    Visible = False
  end
  object Timer1: TTimer
    Interval = 10
    Left = 272
    Top = 152
  end
end
