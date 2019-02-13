object FramFlexiMath: TFramFlexiMath
  Left = 0
  Top = 0
  Width = 151
  Height = 102
  TabOrder = 0
  object tv: TTreeView
    Left = 0
    Top = 25
    Width = 151
    Height = 77
    Align = alClient
    Indent = 19
    TabOrder = 0
    Items.NodeData = {
      0301000000200000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      000000000001013200}
    ExplicitWidth = 201
    ExplicitHeight = 92
  end
  object panTitle: TPanel
    Left = 0
    Top = 0
    Width = 151
    Height = 25
    Align = alTop
    Caption = 'panTitle'
    TabOrder = 1
    OnMouseDown = panTitleMouseDown
    OnMouseMove = panTitleMouseMove
    OnMouseUp = panTitleMouseUp
    ExplicitWidth = 201
  end
end
