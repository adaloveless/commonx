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
unit UExternalFileViewer.Android;

interface

{$IFDEF ANDROID}

uses
  System.SysUtils, System.Classes, FMX.Forms, UExternalFileViewer, System.IOUtils,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util,
  Androidapi.Helpers,
  Androidapi.JNI.Environment,
  Androidapi.JNI.Webkit,
  FMX.Helpers.Android;

type
  TAndroidExternalFileViewer = class (TExternalFileViewer)
  private
    function GetMimeType(Uri: Jnet_Uri): JString;
  public
    procedure OpenFile(Path: string); override;
    procedure OpenURL(URL: string); override;
  end;

{$ENDIF}

implementation

{$IFDEF ANDROID}

uses
  FMX.Dialogs;

{ TAndroidExternalFileViewer }

procedure TAndroidExternalFileViewer.OpenFile(Path: string);
var
  &OriginalFile, PublicDirectoryFile, PublicFile: JFile;
  PublicDirectoryPath, PublicPath: string;

  Uri: Jnet_Uri;
  Intent: JIntent;
begin
  inherited;

  if Path.StartsWith(System.IOUtils.TPath.GetDocumentsPath()) then begin
    &OriginalFile := TJFile.JavaClass.init(StringToJString(Path));

    PublicDirectoryPath := (JStringToString(TJEnvironment.JavaClass.getExternalStorageDirectory.getAbsolutePath) + '/Documents');
    PublicPath := PublicDirectoryPath + PathDelim + ExtractFileName(Path);    PublicDirectoryFile := TJFile.JavaClass.init(StringToJString(PublicDirectoryPath));    if (not PublicDirectoryFile.exists) then begin      PublicDirectoryFile.mkdir;    end;    if (not FileExists(PublicPath)) then      TFile.Copy(JStringToString(&OriginalFile.getAbsolutePath), PublicPath);
    PublicFile := TJFile.JavaClass.init(StringToJString(PublicPath));
  end
  else begin
    PublicFile := TJFile.JavaClass.init(StringToJString(Path));
  end;

  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
  Uri := TJnet_Uri.JavaClass.fromFile(PublicFile);
//  Intent.setDataAndType(Uri, StringToJString('application/pdf'));
  Intent.setDataAndType(Uri, self.GetMimeType(Uri));
  Intent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NO_HISTORY);

  TAndroidHelper.Activity.startActivity(Intent);
end;

procedure TAndroidExternalFileViewer.OpenURL(URL: string);
var
  Intent: JIntent;
  Uri: Jnet_Uri;
begin
  inherited;

  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);

//  Intent.setDataAndType(TJnet_Uri.JavaClass.parse(StringToJString(URL)), StringToJString('application/pdf'));

  Uri := TJnet_Uri.JavaClass.parse(StringToJString(URL));
  Intent.setDataAndType(Uri, self.GetMimeType(Uri));

  Intent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NO_HISTORY);

  TAndroidHelper.Activity.startActivity(Intent);
end;

function TAndroidExternalFileViewer.GetMimeType(Uri: Jnet_Uri): JString;
var
  MimeType: JString;
  ContentResolver: JContentResolver;
  FileExtension: JString;
begin
  // https://stackoverflow.com/a/31691791/2899073

  MimeType := nil;
  if (Uri.getScheme.equals(TJContentResolver.JavaClass.SCHEME_CONTENT)) then begin
    ContentResolver := TAndroidHelper.Context.getContentResolver();
    MimeType := ContentResolver.getType(uri);
  end
  else begin
    FileExtension := TJMimeTypeMap.JavaClass.getFileExtensionFromUrl(uri.toString());

    MimeType := TJMimeTypeMap.JavaClass.getSingleton().getMimeTypeFromExtension(
      fileExtension.toLowerCase()
    );
  end;

  Result := MimeType;
end;

{$ENDIF}

end.
