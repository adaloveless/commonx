object dmRDTPMultiServer: TdmRDTPMultiServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 1120
  Width = 1420
  object tcp: TTcpServer
    LocalPort = '876'
    Left = 176
    Top = 96
  end
end
