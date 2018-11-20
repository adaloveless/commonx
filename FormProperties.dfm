object frmProperties: TfrmProperties
  Left = 0
  Top = 0
  Caption = 'Properties'
  ClientHeight = 532
  ClientWidth = 852
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  GlassFrame.Enabled = True
  GlassFrame.Bottom = 40
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = frmBaseCreate
  Right = 867
  Bottom = 569
  NewMode = False
  DesignSize = (
    852
    532)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 684
    Top = 499
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 769
    Top = 499
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object panBody: TPanel
    Left = 0
    Top = 3
    Width = 852
    Height = 490
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Locked = True
    TabOrder = 2
  end
end
