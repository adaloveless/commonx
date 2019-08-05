{****************************************************}
{                                                    }
{ firemonkey-external-file-viewer                    }
{                                                    }
{ Copyright (C) 2018 Code Partners Pty Ltd           }
{                                                    }
{ http://www.code-partners.com                       }
{                                                    }
{****************************************************}
{                                                    }
{ This Source Code Form is subject to the terms of   }
{ the Mozilla Public License, v. 2.0. If a copy of   }
{ the MPL was not distributed with this file, You    }
{ can obtain one at                                  }
{                                                    }
{ http://mozilla.org/MPL/2.0/                        }
{                                                    }
{****************************************************}
unit UOpenURLUtils;

interface

uses
  System.SysUtils, System.Classes
  {$IFDEF ANDROID}
    , Androidapi.JNI.GraphicsContentViewText,
    Androidapi.JNI.App,
    Androidapi.JNIBridge,
    Androidapi.JNI.JavaTypes,
    Androidapi.Helpers,
    Androidapi.JNI.Net,
    Androidapi.JNI.Os,
    Androidapi.IOUtils
  {$ENDIF}

  {$IFDEF IOS}
    , Macapi.Helpers,
    iOSAPI.Foundation,
    iOSAPI.Helpers,
    iOSAPI.UIKit,
    iOSAPI.CoreGraphics,
    FMX.Helpers.iOS,
    FMX.Platform.iOS,
    Macapi.ObjectiveC,
    Macapi.ObjCRuntime
  {$ENDIF}
  ;

procedure OpenURL(URL: string);

implementation

{$IF DEFINED(ANDROID)}

procedure OpenURL(URL: string);
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(TJnet_Uri.JavaClass.parse(StringToJString(URL)));
  TAndroidHelper.Activity.startActivity(Intent);
end;

{$ELSEIF DEFINED(IOS)}

procedure OpenURL(URL: string);
var
  u: NSURL;
begin
  u := TNSUrl.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(URL)));
  TiOSHelper.SharedApplication.openURL(u);
end;

{$ELSE}

procedure OpenURL(URL: string);
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

{$ENDIF}

end.
