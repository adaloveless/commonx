object frmPropertyDialog: TfrmPropertyDialog
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
  OnCreate = frmBaseCreate
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
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 769
    Top = 499
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    TabOrder = 1
  end
  object panBody: TFrameHostPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 493
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'panBody'
    Locked = True
    TabOrder = 2
  end
end
