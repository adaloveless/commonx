object dmRDTPMultiServer: TdmRDTPMultiServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 273
  Width = 348
  object tcp: TTcpServer
    LocalPort = '876'
    OnAccept = tcpAccept
    Left = 176
    Top = 96
  end
end
