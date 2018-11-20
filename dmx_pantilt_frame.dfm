object DMXPanTiltFrame: TDMXPanTiltFrame
  Left = 0
  Top = 0
  Width = 896
  Height = 506
  TabOrder = 0
  object Button1: TButton
    Left = 804
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object Button2: TButton
    Left = 804
    Top = 175
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
  end
  object panstuff: TPanel
    Left = 440
    Top = 0
    Width = 456
    Height = 506
    Caption = 'panstuff'
    TabOrder = 2
    object lbpoints: TListBox
      Left = 1
      Top = 98
      Width = 454
      Height = 407
      Align = alClient
      ItemHeight = 13
      PopupMenu = popCoordSelect
      TabOrder = 0
      OnKeyDown = lbpointsKeyDown
      OnKeyPress = lbpointsKeyPress
      OnKeyUp = lbpointsKeyUp
    end
    object lblights: TListBox
      Left = 1
      Top = 1
      Width = 454
      Height = 97
      Align = alTop
      ItemHeight = 13
      TabOrder = 1
      OnClick = lblightsClick
    end
  end
  object popCoordSelect: TPopupMenu
    Left = 792
    Top = 216
    object Delete1: TMenuItem
      Caption = '&Delete'
      OnClick = Delete1Click
    end
    object Defalt1: TMenuItem
      Caption = 'De&fault'
      OnClick = Defalt1Click
    end
  end
  object Timer1: TTimer
    Interval = 500
    Left = 768
    Top = 352
  end
end
