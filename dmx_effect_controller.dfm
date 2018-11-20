object DMXEffectController: TDMXEffectController
  Left = 0
  Top = 0
  Width = 1021
  Height = 445
  TabOrder = 0
  object lbEffectList: TListBox
    Left = 0
    Top = 0
    Width = 201
    Height = 392
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = lbEffectListDblClick
  end
  object lbEffectStack: TListBox
    Left = 201
    Top = 0
    Width = 184
    Height = 392
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbEffectStackClick
  end
  object lbGroupList: TListBox
    Left = 385
    Top = 0
    Width = 121
    Height = 392
    Align = alLeft
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 2
    OnClick = lbGroupListClick
  end
  object Panel1: TPanel
    Left = 506
    Top = 0
    Width = 515
    Height = 392
    Align = alClient
    TabOrder = 3
  end
  object Panel2: TPanel
    Left = 0
    Top = 392
    Width = 1021
    Height = 53
    Align = alBottom
    TabOrder = 4
    object Button1: TButton
      Left = 16
      Top = 16
      Width = 75
      Height = 25
      Caption = 'ReFresh'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object tmControllerCheck: TTimer
    Interval = 100
    OnTimer = tmControllerCheckTimer
    Left = 496
    Top = 208
  end
end
