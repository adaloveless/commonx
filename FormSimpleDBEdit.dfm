inherited frmSimpleDBEdit: TfrmSimpleDBEdit
  Caption = 'frmSimpleDBEdit'
  ClientHeight = 174
  ClientWidth = 319
  OnConnectToData = frmDataAwareConnectToData
  ExplicitWidth = 335
  ExplicitHeight = 210
  DesignSize = (
    319
    174)
  PixelsPerInch = 96
  TextHeight = 13
  GlassFrame.Bottom = 40
  GlassFrame.Enabled = True
  inherited btnOK: TButton
    Left = 151
    Top = 141
    ExplicitLeft = 151
    ExplicitTop = 141
  end
  inherited btnCancel: TButton
    Left = 236
    Top = 141
    ExplicitLeft = 236
    ExplicitTop = 141
  end
  inherited pnlBack: TPanel
    Top = 0
    Width = 319
    Height = 132
    ExplicitTop = 0
    ExplicitWidth = 319
    ExplicitHeight = 132
  end
  object tblMain: TSQLTable
    NumericMapping = True
    MaxBlobSize = -1
    TableName = 'category'
  end
  object dsMain: TDataSource
    DataSet = tblMain
    Left = 32
  end
end
