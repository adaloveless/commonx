inherited frmTestSampleViewer: TfrmTestSampleViewer
  Width = 1019
  Height = 550
  ExplicitWidth = 1019
  ExplicitHeight = 550
  inherited pbWave: TPaintBox
    Width = 1019
    Height = 471
    ExplicitTop = 3
    ExplicitWidth = 1019
    ExplicitHeight = 471
  end
  inherited panButtons: TPanel
    Top = 471
    Width = 1019
    ExplicitTop = 471
    ExplicitWidth = 1019
    inherited btMark: TButton
      Left = 335
      ExplicitLeft = 335
    end
    inherited btUnMark: TButton
      Left = 335
      ExplicitLeft = 335
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 6
      Width = 97
      Height = 17
      Caption = 'Layer 1'
      TabOrder = 17
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 22
      Width = 97
      Height = 17
      Caption = 'Layer 2'
      TabOrder = 18
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Layer 3'
      TabOrder = 19
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 112
      Top = 6
      Width = 97
      Height = 17
      Caption = 'Layer 4'
      TabOrder = 20
      OnClick = CheckBox4Click
    end
    object CheckBox5: TCheckBox
      Left = 112
      Top = 22
      Width = 97
      Height = 17
      Caption = 'Layer 5'
      TabOrder = 21
      OnClick = CheckBox5Click
    end
    object CheckBox6: TCheckBox
      Left = 112
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Layer 6'
      TabOrder = 22
      OnClick = CheckBox6Click
    end
  end
end
