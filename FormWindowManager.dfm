object frmWindowManager: TfrmWindowManager
  Left = 0
  Top = 0
  Align = alLeft
  Caption = 'frmWindowManager'
  ClientHeight = 361
  ClientWidth = 153
  Color = clBtnFace
  Constraints.MaxWidth = 169
  Constraints.MinHeight = 400
  Constraints.MinWidth = 169
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = menuWinMan
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = frmBaseCreate
  OnDestroy = frmBaseDestroy
  Right = 168
  Bottom = 419
  DesignSize = (
    153
    361)
  PixelsPerInch = 96
  TextHeight = 13
  object panWindows: TFrameHostPanel
    Left = 8
    Top = 136
    Width = 137
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'panWindows'
    TabOrder = 0
  end
  object tmRearrange: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmRearrangeTimer
    Left = 48
    Top = 40
  end
  object menuWinMan: TMainMenu
    Left = 88
    Top = 104
    object Monitor1: TMenuItem
      Caption = '&View'
      object Monitor2: TMenuItem
        Caption = '&Monitor'
        OnClick = Monitor2Click
        object N11: TMenuItem
          Caption = '&1'
          OnClick = N51Click
        end
        object N21: TMenuItem
          Tag = 1
          Caption = '&2'
          OnClick = N51Click
        end
        object N31: TMenuItem
          Tag = 2
          Caption = '&3'
          OnClick = N51Click
        end
        object N41: TMenuItem
          Tag = 3
          Caption = '&4'
          OnClick = N51Click
        end
        object N51: TMenuItem
          Tag = 4
          Caption = '&5'
          OnClick = N51Click
        end
      end
    end
  end
end
