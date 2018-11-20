object frmVideo: TfrmVideo
  Left = 0
  Top = 0
  AlphaBlend = True
  Caption = 'Video'
  ClientHeight = 606
  ClientWidth = 992
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = frmBaseActivate
  OnCreate = frmBaseCreate
  OnDestroy = frmBaseDestroy
  OnResize = frmBaseResize
  PixelsPerInch = 96
  TextHeight = 13
  object wmp: TWindowsMediaPlayer
    Left = 12
    Top = 13
    Width = 245
    Height = 240
    Cursor = crIBeam
    TabOrder = 0
    ControlData = {
      000300000800000000000500000000000000F03F030000000000050000000000
      0000000008000200000000000300010000000B00FFFF0300000000000B00FFFF
      08000200000000000300320000000B00000008000A000000660075006C006C00
      00000B0000000B0000000B00FFFF0B00FFFF0B00000008000200000000000800
      020000000000080002000000000008000200000000000B00000052190000CE18
      0000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 376
    Width = 992
    Height = 230
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 1
    object Label1: TLabel
      Left = 1
      Top = 209
      Width = 49
      Height = 20
      Align = alBottom
      Caption = 'Label1'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = '@Adobe Gothic Std B'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Chart1: TChart
      Left = 1
      Top = 1
      Width = 990
      Height = 167
      Legend.Visible = False
      Title.Text.Strings = (
        'TChart')
      BottomAxis.Automatic = False
      BottomAxis.AutomaticMaximum = False
      BottomAxis.Maximum = 22.000000000000000000
      Chart3DPercent = 1
      View3D = False
      View3DOptions.Zoom = 134
      Align = alClient
      TabOrder = 0
      ColorPaletteIndex = 13
      object Series1: TLineSeries
        Emboss.Clip = True
        Emboss.Color = 13224393
        Emboss.HorizSize = 3
        Emboss.SmoothBlur = 15
        Emboss.VertSize = 3
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        LinePen.Width = 3
        Pointer.Brush.Gradient.EndColor = 10708548
        Pointer.Gradient.EndColor = 10708548
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.Visible = False
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
      object Series2: TLineSeries
        Marks.Arrow.Visible = True
        Marks.Callout.Brush.Color = clBlack
        Marks.Callout.Arrow.Visible = True
        Marks.Visible = False
        ClickableLine = False
        Dark3D = False
        LinePen.Width = 3
        Pointer.Brush.Gradient.EndColor = 3513587
        Pointer.Gradient.EndColor = 3513587
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        Pointer.Visible = False
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 168
      Width = 990
      Height = 41
      Align = alBottom
      Caption = 'Panel2'
      TabOrder = 1
      object Button1: TButton
        Left = 11
        Top = 10
        Width = 75
        Height = 25
        Caption = 'Reload'
        TabOrder = 0
        OnClick = Button1Click
      end
    end
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 512
    Top = 304
  end
end
