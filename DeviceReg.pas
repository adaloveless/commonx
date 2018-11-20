unit DeviceReg;

interface

uses
  sysutils,
  system.types,
  System.Devices;

implementation


initialization


  TDeviceinfo.AddDevice(TDeviceinfo.TDeviceClass.Tablet, 'Moto360',
    // The Moto360 is 320x290 phyiscal and 240x218 logical with 213 PPI
    TSize.Create(320, 290), TSize.Create(240, 218),
    TOSVersion.TPlatform.pfAndroid, 213,
    True);

end.
