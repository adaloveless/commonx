object frmPropertiesBare: TfrmPropertiesBare
  Left = 0
  Top = 0
  Caption = 'frmPropertiesBare'
  ClientHeight = 331
  ClientWidth = 545
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  GlassFrame.Enabled = True
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    545
    331)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBack: TPanel
    Left = 0
    Top = 0
    Width = 545
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Locked = True
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 380
    Top = 301
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 465
    Top = 301
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
