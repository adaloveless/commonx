inherited frmTransientGenerator: TfrmTransientGenerator
  Width = 134
  Anchors = [akLeft, akBottom]
  ExplicitWidth = 134
  object Panel3: TPanel
    Left = 0
    Top = 201
    Width = 134
    Height = 41
    Align = alTop
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      134
      41)
    object btnGo: TButton
      Left = 37
      Top = 6
      Width = 84
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Go'
      TabOrder = 0
      OnClick = btnGoClick
    end
  end
  object panstuff: TPanel
    Left = 0
    Top = 0
    Width = 134
    Height = 201
    Align = alTop
    BevelEdges = []
    BevelOuter = bvNone
    TabOrder = 1
  end
end
