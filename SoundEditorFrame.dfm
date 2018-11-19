object frmSoundEditor: TfrmSoundEditor
  Left = 0
  Top = 0
  Width = 879
  Height = 655
  DoubleBuffered = True
  ParentDoubleBuffered = False
  TabOrder = 0
  OnResize = FrameResize
  OnupdateState = frmFrameBaseupdateState
  object pbWave: TBackBufferedControl
    Left = 0
    Top = 0
    Width = 879
    Height = 557
    Align = alClient
    PopupMenu = popWave
    OnDblClick = pbWaveDblClick
    OnMouseDown = HookMouseDown
    OnMouseMove = HookMouseMove
    OnMouseUp = HookMouseUp
    OnDraw = pbWavePaint
  end
  object panButtons: TPanel
    Left = 0
    Top = 574
    Width = 879
    Height = 81
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      879
      81)
    object Label1: TLabel
      Left = 10
      Top = 9
      Width = 65
      Height = 13
      Caption = 'Layer to Edit:'
    end
    object btZoomOut: TButton
      Left = 582
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Zoom Ou&t'
      TabOrder = 0
      TabStop = False
      OnClick = ZoomOutClick
    end
    object btDeselect: TButton
      Left = 663
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&DeSelect'
      TabOrder = 1
      TabStop = False
      OnClick = UnSelectClick
    end
    object btZoomIn: TButton
      Left = 501
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Zoom In'
      TabOrder = 2
      TabStop = False
      OnClick = ZoomInClick
    end
    object btPlay: TButton
      Left = 501
      Top = 37
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Play'
      TabOrder = 3
      TabStop = False
      OnClick = btPlayClick
    end
    object Button2: TButton
      Left = 582
      Top = 38
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Zoom &Way Out'
      TabOrder = 4
      TabStop = False
      OnClick = btZoomOutClick
    end
    object btnSaveMarkers: TButton
      Left = 663
      Top = 37
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Update Marker'
      TabOrder = 5
      TabStop = False
      OnClick = btnSaveMarkersClick
    end
    object Button3: TButton
      Left = 744
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Find Near'
      TabOrder = 6
      OnClick = Button3Click
    end
    object Panel1: TPanel
      Left = 992
      Top = 6
      Width = 297
      Height = 69
      Color = clRed
      ParentBackground = False
      TabOrder = 7
      Visible = False
      object btMark: TButton
        Left = 7
        Top = 6
        Width = 75
        Height = 25
        Caption = '&Mark'
        TabOrder = 0
        TabStop = False
        OnClick = MarkClick
      end
      object btUnMark: TButton
        Left = 7
        Top = 37
        Width = 75
        Height = 25
        Caption = '&UnMark'
        TabOrder = 1
        TabStop = False
        OnClick = UnMarkClick
      end
      object btInLeft: TButton
        Left = 93
        Top = 8
        Width = 42
        Height = 25
        Caption = '&In <'
        TabOrder = 2
        TabStop = False
        OnClick = TweakInLeftClick
      end
      object btOutLeft: TButton
        Left = 93
        Top = 38
        Width = 42
        Height = 25
        Caption = 'O&ut <'
        TabOrder = 3
        TabStop = False
        OnClick = TweakOutLeftClick
      end
      object btOutRight: TButton
        Left = 141
        Top = 38
        Width = 42
        Height = 25
        Caption = '&Out >'
        TabOrder = 4
        TabStop = False
        OnClick = TweakOutRightClick
      end
      object btInRight: TButton
        Left = 141
        Top = 7
        Width = 42
        Height = 25
        Caption = 'I&n >'
        TabOrder = 5
        TabStop = False
        OnClick = TweakInRightClick
      end
      object btLoopLEft: TButton
        Left = 190
        Top = 6
        Width = 45
        Height = 25
        Caption = '&Loop <'
        TabOrder = 6
        OnClick = TweakInLoopLeftClick
      end
      object Button1: TButton
        Left = 190
        Top = 38
        Width = 45
        Height = 25
        Caption = '&Loop <'
        TabOrder = 7
        TabStop = False
        OnClick = TweakOutLeftClick
      end
      object btLoopOutRight: TButton
        Left = 239
        Top = 38
        Width = 42
        Height = 25
        Caption = 'Loo&p >'
        TabOrder = 8
        TabStop = False
        OnClick = TweakOutLoopRightClick
      end
      object btLoopRight: TButton
        Left = 239
        Top = 6
        Width = 42
        Height = 25
        Caption = 'Loo&p >'
        TabOrder = 9
        TabStop = False
        OnClick = TweakInLoopRightClick
      end
    end
    object ComboBox1: TComboBox
      Left = 10
      Top = 28
      Width = 98
      Height = 21
      TabOrder = 8
      Text = 'Main'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Main'
        '0'
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8'
        '9'
        '10'
        '11'
        '12'
        '13'
        '14'
        '15'
        '16'
        '17'
        '18'
        '19'
        '20'
        '21'
        '22'
        '23'
        '24'
        '25'
        '26'
        '27'
        '28'
        '29'
        '30'
        '31'
        '32'
        '33'
        '34'
        '35'
        '36'
        '37'
        '38'
        '39'
        '40'
        '41'
        '42'
        '43'
        '44'
        '45'
        '46'
        '47'
        '48'
        '49'
        '50')
    end
    object btnSaveCurves: TButton
      Left = 10
      Top = 55
      Width = 75
      Height = 25
      Caption = '&Save Curves'
      TabOrder = 9
      OnClick = btnSaveCurvesClick
    end
    object btnDelete: TButton
      Left = 390
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 10
      OnClick = btnDeleteClick
    end
    object rbMarks: TRadioButton
      Left = 81
      Top = 8
      Width = 49
      Height = 17
      Caption = 'Marks'
      Checked = True
      TabOrder = 11
      TabStop = True
      OnClick = rbMarksClick
    end
    object rbCurves: TRadioButton
      Left = 136
      Top = 8
      Width = 65
      Height = 17
      Caption = 'Curves'
      TabOrder = 12
      OnClick = rbCurvesClick
    end
    object Button4: TButton
      Left = 390
      Top = 37
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Select &All'
      TabOrder = 13
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 744
      Top = 38
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&TurbData'
      TabOrder = 14
      Visible = False
      OnClick = Button5Click
    end
    object edAxies: TComboBoxEx
      Left = 118
      Top = 28
      Width = 179
      Height = 22
      ItemsEx = <
        item
          Caption = 'Axis 0'
        end
        item
          Caption = '1'
        end
        item
          Caption = '2'
        end
        item
          Caption = '3'
        end
        item
          Caption = '4'
        end
        item
          Caption = '5'
        end
        item
          Caption = '6'
        end
        item
          Caption = '7'
        end
        item
          Caption = '8'
        end
        item
          Caption = '9'
        end
        item
          Caption = '10'
        end
        item
          Caption = '11'
        end>
      TabOrder = 15
      Text = 'Axis 0'
      OnChange = edAxiesChange
    end
  end
  object scrollbar: TScrollBar
    Left = 0
    Top = 557
    Width = 879
    Height = 17
    Align = alBottom
    DoubleBuffered = False
    PageSize = 0
    ParentDoubleBuffered = False
    TabOrder = 1
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
