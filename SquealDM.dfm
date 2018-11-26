object Squeal: TSqueal
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 141
  Width = 167
  object tcp: TBetterTCPServer
    Active = True
    LocalPort = '234'
    OnAccept = tcpAccept
    Left = 16
    Top = 16
  end
end
