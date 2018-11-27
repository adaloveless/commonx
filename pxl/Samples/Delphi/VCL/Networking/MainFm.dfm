object MainForm: TMainForm
  Left = 249
  Top = 123
  Caption = 'PXL Networking Example'
  ClientHeight = 380
  ClientWidth = 462
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Pitch = fpVariable
  Font.Style = []
  Font.Quality = fqDraft
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 17
  object IncomingGroupBox: TGroupBox
    Left = 0
    Top = 0
    Width = 462
    Height = 229
    Align = alClient
    Caption = ' Incoming Messages '
    TabOrder = 0
    ExplicitTop = 40
    object IncomingMemo: TMemo
      Left = 2
      Top = 19
      Width = 458
      Height = 208
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Consolas'
      Font.Pitch = fpFixed
      Font.Style = []
      Font.Quality = fqDraft
      ParentFont = False
      TabOrder = 0
      ExplicitLeft = 104
      ExplicitTop = 72
      ExplicitWidth = 203
      ExplicitHeight = 91
    end
  end
  object SendGroupBox: TGroupBox
    Left = 0
    Top = 229
    Width = 462
    Height = 128
    Align = alBottom
    Caption = ' Send Message '
    TabOrder = 1
    DesignSize = (
      462
      128)
    object HostLabel: TLabel
      Left = 48
      Top = 23
      Width = 99
      Height = 17
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Destination Host:'
      Color = clBtnFace
      ParentColor = False
    end
    object PortLabel: TLabel
      Left = 51
      Top = 58
      Width = 96
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Destination Port:'
      Color = clBtnFace
      ParentColor = False
    end
    object PortLabel1: TLabel
      Left = 67
      Top = 92
      Width = 56
      Height = 17
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Message:'
      Color = clBtnFace
      ParentColor = False
    end
    object DestHostEdit: TEdit
      Left = 153
      Top = 20
      Width = 160
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 0
      Text = '127.0.0.1'
    end
    object DestPortEdit: TEdit
      Left = 153
      Top = 55
      Width = 160
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 1
      Text = '7500'
    end
    object TextEdit: TEdit
      Left = 129
      Top = 90
      Width = 184
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 2
      Text = 'Hello world there!'
    end
    object SendButton: TButton
      Left = 319
      Top = 89
      Width = 75
      Height = 25
      Caption = 'Send'
      TabOrder = 3
      OnClick = SendButtonClick
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 357
    Width = 462
    Height = 23
    Panels = <
      item
        Text = 'Local IP: Unknown'
        Width = 200
      end
      item
        Text = 'Local Port: Unknown'
        Width = 50
      end>
  end
  object SysTimer: TTimer
    Interval = 100
    OnTimer = SysTimerTimer
    Left = 40
    Top = 48
  end
end
