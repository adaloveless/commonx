object frmBase: TfrmBase
  Left = 0
  Top = 0
  Caption = 'Deploy'
  ClientHeight = 581
  ClientWidth = 726
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object tmAfterFirstActivation: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmAfterFirstActivationTimer
    Left = 64
    Top = 16
  end
  object tmDelayedFormSave: TTimer
    Enabled = False
    Interval = 4000
    OnTimer = tmDelayedFormSaveTimer
    Left = 176
    Top = 16
  end
end
