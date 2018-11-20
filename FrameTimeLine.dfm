object FramTimeLine: TFramTimeLine
  Left = 0
  Top = 0
  Width = 1039
  Height = 484
  TabOrder = 0
  OnResize = FrameResize
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1039
    Height = 65
    Align = alTop
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 160
      Height = 63
      Align = alLeft
      TabOrder = 0
      object Button1: TButton
        Left = 6
        Top = 18
        Width = 73
        Height = 25
        Caption = 'Prev Beat'
        TabOrder = 0
        TabStop = False
        OnClick = Button1Click
      end
      object Panel3: TPanel
        Left = 1
        Top = 1
        Width = 158
        Height = 16
        Align = alTop
        Caption = 'Scrub Controls'
        TabOrder = 1
      end
      object Button2: TButton
        Left = 83
        Top = 18
        Width = 73
        Height = 25
        Caption = 'Next Beat'
        TabOrder = 2
        TabStop = False
        OnClick = Button2Click
      end
    end
    object Panel4: TPanel
      Left = 281
      Top = 1
      Width = 154
      Height = 63
      Align = alLeft
      TabOrder = 1
      object Panel5: TPanel
        Left = 1
        Top = 1
        Width = 152
        Height = 16
        Align = alTop
        Caption = 'Zoom'
        TabOrder = 0
      end
      object Button3: TButton
        Left = 0
        Top = 19
        Width = 75
        Height = 25
        Caption = '&In'
        TabOrder = 1
        TabStop = False
        OnClick = Button3Click
      end
      object Button5: TButton
        Left = 75
        Top = 19
        Width = 75
        Height = 25
        Caption = '&Out'
        TabOrder = 2
        TabStop = False
        OnClick = Button5Click
      end
    end
    object Panel6: TPanel
      Left = 161
      Top = 1
      Width = 120
      Height = 63
      Align = alLeft
      TabOrder = 2
      object Panel7: TPanel
        Left = 1
        Top = 1
        Width = 118
        Height = 16
        Align = alTop
        Caption = 'File'
        TabOrder = 0
      end
      object btnOpen: TButton
        Left = 2
        Top = 18
        Width = 75
        Height = 25
        Caption = '&Open'
        TabOrder = 1
        TabStop = False
        OnClick = btnOpenClick
      end
    end
    object edGridSnap: TEdit
      Left = 441
      Top = 9
      Width = 121
      Height = 21
      TabOrder = 3
      Text = '0.25'
    end
    object Button4: TButton
      Left = 584
      Top = 7
      Width = 129
      Height = 25
      Caption = 'Find orphaned Points'
      TabOrder = 4
      TabStop = False
      OnClick = Button4Click
    end
    object Button6: TButton
      Left = 719
      Top = 7
      Width = 170
      Height = 25
      Caption = 'Conclude with all notes off'
      TabOrder = 5
      TabStop = False
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 895
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Quantize'
      TabOrder = 6
      OnClick = Button7Click
    end
    object btnDelbankset: TButton
      Left = 584
      Top = 36
      Width = 185
      Height = 25
      Caption = '&Delete most recent bankset change'
      TabOrder = 7
      OnClick = btnDelbanksetClick
    end
    object Button8: TButton
      Left = 459
      Top = 36
      Width = 40
      Height = 25
      Caption = 'BS1'
      TabOrder = 8
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 505
      Top = 36
      Width = 40
      Height = 25
      Caption = 'Bs2'
      TabOrder = 9
      OnClick = Button9Click
    end
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 456
    Top = 120
  end
  object openfile: TFileOpenDialog
    DefaultExtension = '.perf'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Performance Files'
        FileMask = '*.perf'
      end>
    Options = []
    Left = 544
    Top = 120
  end
end
