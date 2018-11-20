inherited frmWizardFixed: TfrmWizardFixed
  Caption = 'frmWizardFixed'
  OnCreate = frmBaseCreate
  OnDestroy = frmBaseDestroy
  ExplicitTop = -19
  ExplicitWidth = 879
  ExplicitHeight = 580
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TButton
    Caption = '&Finish'
    ModalResult = 1
  end
  inherited btnCancel: TButton
    ModalResult = 2
  end
  inherited panBody: TFrameHostPanel
    object pcWizard: TPageControl
      Left = 1
      Top = 1
      Width = 860
      Height = 492
      Align = alClient
      Style = tsFlatButtons
      TabOrder = 0
      OnChange = pcWizardChange
    end
  end
  object btnNext: TButton
    Left = 614
    Top = 509
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Next'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 3
    OnClick = btnNextClick
  end
  object btnPrevious: TButton
    Left = 533
    Top = 509
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Previous'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 4
    OnClick = btnPreviousClick
  end
end
