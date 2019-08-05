unit FormReg;

interface
{$I DelphiDefs.inc}
procedure Register;

implementation

uses sysutils, system.types, System.Devices,
  DesignIntf,
  FrameBaseVCL, FormBase, FrameBaseFMX, FormDataAware, DesignEditors, classes, dialogs, FormWizard, FrameWizardPage, FormProperties, FormPropertyDialog;

procedure Register;
begin
//  showmessage('stfu');
  RegisterNoIcon([TfrmBase, TfrmDataAware]);
  RegisterCustomModule(TfrmBase, TCustomModule);
  RegisterCustomModule(TfrmFrameBase, TCustomModule);
  RegisterCustomModule(TfrmDataAware, TCustomModule);
  RegisterCustomModule(TfrmProperties, TCustomModule);
  RegisterCustomModule(TfrmPropertyDialog, TCustomModule);
  RegisterCustomModule(TfrmWizard, TCustomModule);
  RegisterCustomModule(TfrmWizardPage, TCustomModule);

//  TDeviceinfo.AddDevice(TDeviceinfo.TDeviceClass.Tablet, 'Moto360x',
    // The Moto360 is 320x290 phyiscal and 240x218 logical with 213 PPI
//    TSize.Create(320, 290), TSize.Create(240, 218),
//    TOSVersion.TPlatform.pfAndroid, 213,
//    True);


end;

end.
