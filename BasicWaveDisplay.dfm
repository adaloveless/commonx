object FrameBasicWaveDisplay: TFrameBasicWaveDisplay
  Left = 0
  Top = 0
  Width = 879
  Height = 655
  DoubleBuffered = True
  ParentDoubleBuffered = False
  TabOrder = 0
  OnResize = FrameResize
  object scrollbar: TScrollBar
    Left = 0
    Top = 638
    Width = 879
    Height = 17
    Align = alBottom
    DoubleBuffered = False
    PageSize = 0
    ParentDoubleBuffered = False
    TabOrder = 0
    OnChange = scrollbarChange
  end
  object Draw: TTimer
    Interval = 24
    OnTimer = DrawTimer
    Left = 448
    Top = 256
  end
  object popWave: TPopupMenu
    OnPopup = popWavePopup
    Left = 360
    Top = 392
    object Dele1: TMenuItem
      Caption = '&Delete Point'
      OnClick = Dele1Click
    end
    object Cancel1: TMenuItem
      Caption = '&Cancel'
    end
  end
end
