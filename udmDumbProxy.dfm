object DataModule1: TDataModule1
  OldCreateOrder = False
  Height = 150
  Width = 215
  object TcpServer1: TTcpServer
    LocalPort = '4321'
    OnAccept = TcpServer1Accept
    Left = 88
    Top = 56
  end
end
