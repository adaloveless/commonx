{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Environment;

interface

{$IFDEF ANDROID}
uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type
// ===== Forward declarations =====

  JEnvironment = interface;//android.os.Environment

// ===== Interface declarations =====

  JEnvironmentClass = interface(JObjectClass)
    ['{847171A2-7B65-4251-9BD3-E0BC89DE31FD}']
    {class} function _GetDIRECTORY_ALARMS: JString;
    {class} procedure _SetDIRECTORY_ALARMS(Value: JString);
    {class} function _GetDIRECTORY_DCIM: JString;
    {class} procedure _SetDIRECTORY_DCIM(Value: JString);
    {class} function _GetDIRECTORY_DOCUMENTS: JString;
    {class} procedure _SetDIRECTORY_DOCUMENTS(Value: JString);
    {class} function _GetDIRECTORY_DOWNLOADS: JString;
    {class} procedure _SetDIRECTORY_DOWNLOADS(Value: JString);
    {class} function _GetDIRECTORY_MOVIES: JString;
    {class} procedure _SetDIRECTORY_MOVIES(Value: JString);
    {class} function _GetDIRECTORY_MUSIC: JString;
    {class} procedure _SetDIRECTORY_MUSIC(Value: JString);
    {class} function _GetDIRECTORY_NOTIFICATIONS: JString;
    {class} procedure _SetDIRECTORY_NOTIFICATIONS(Value: JString);
    {class} function _GetDIRECTORY_PICTURES: JString;
    {class} procedure _SetDIRECTORY_PICTURES(Value: JString);
    {class} function _GetDIRECTORY_PODCASTS: JString;
    {class} procedure _SetDIRECTORY_PODCASTS(Value: JString);
    {class} function _GetDIRECTORY_RINGTONES: JString;
    {class} procedure _SetDIRECTORY_RINGTONES(Value: JString);
    {class} function _GetMEDIA_BAD_REMOVAL: JString;
    {class} function _GetMEDIA_CHECKING: JString;
    {class} function _GetMEDIA_MOUNTED: JString;
    {class} function _GetMEDIA_MOUNTED_READ_ONLY: JString;
    {class} function _GetMEDIA_NOFS: JString;
    {class} function _GetMEDIA_REMOVED: JString;
    {class} function _GetMEDIA_SHARED: JString;
    {class} function _GetMEDIA_UNKNOWN: JString;
    {class} function _GetMEDIA_UNMOUNTABLE: JString;
    {class} function _GetMEDIA_UNMOUNTED: JString;
    {class} function init: JEnvironment; cdecl;
    {class} function getDataDirectory: JFile; cdecl;
    {class} function getDownloadCacheDirectory: JFile; cdecl;
    {class} function getExternalStorageDirectory: JFile; cdecl;
    {class} function getExternalStoragePublicDirectory(type_: JString): JFile; cdecl;
    {class} function getExternalStorageState: JString; cdecl; overload;
    // !!! this method added manually (Android 5.0+)
    {class} function getExternalStorageState(&File: JFile): JString; cdecl; overload;
    {class} function getRootDirectory: JFile; cdecl;
    {class} function getStorageState(path: JFile): JString; cdecl;
    {class} function isExternalStorageEmulated: Boolean; cdecl;
    {class} function isExternalStorageRemovable: Boolean; cdecl;
    {class} property DIRECTORY_ALARMS: JString read _GetDIRECTORY_ALARMS write _SetDIRECTORY_ALARMS;
    {class} property DIRECTORY_DCIM: JString read _GetDIRECTORY_DCIM write _SetDIRECTORY_DCIM;
    {class} property DIRECTORY_DOCUMENTS: JString read _GetDIRECTORY_DOCUMENTS write _SetDIRECTORY_DOCUMENTS;
    {class} property DIRECTORY_DOWNLOADS: JString read _GetDIRECTORY_DOWNLOADS write _SetDIRECTORY_DOWNLOADS;
    {class} property DIRECTORY_MOVIES: JString read _GetDIRECTORY_MOVIES write _SetDIRECTORY_MOVIES;
    {class} property DIRECTORY_MUSIC: JString read _GetDIRECTORY_MUSIC write _SetDIRECTORY_MUSIC;
    {class} property DIRECTORY_NOTIFICATIONS: JString read _GetDIRECTORY_NOTIFICATIONS write _SetDIRECTORY_NOTIFICATIONS;
    {class} property DIRECTORY_PICTURES: JString read _GetDIRECTORY_PICTURES write _SetDIRECTORY_PICTURES;
    {class} property DIRECTORY_PODCASTS: JString read _GetDIRECTORY_PODCASTS write _SetDIRECTORY_PODCASTS;
    {class} property DIRECTORY_RINGTONES: JString read _GetDIRECTORY_RINGTONES write _SetDIRECTORY_RINGTONES;
    {class} property MEDIA_BAD_REMOVAL: JString read _GetMEDIA_BAD_REMOVAL;
    {class} property MEDIA_CHECKING: JString read _GetMEDIA_CHECKING;
    {class} property MEDIA_MOUNTED: JString read _GetMEDIA_MOUNTED;
    {class} property MEDIA_MOUNTED_READ_ONLY: JString read _GetMEDIA_MOUNTED_READ_ONLY;
    {class} property MEDIA_NOFS: JString read _GetMEDIA_NOFS;
    {class} property MEDIA_REMOVED: JString read _GetMEDIA_REMOVED;
    {class} property MEDIA_SHARED: JString read _GetMEDIA_SHARED;
    {class} property MEDIA_UNKNOWN: JString read _GetMEDIA_UNKNOWN;
    {class} property MEDIA_UNMOUNTABLE: JString read _GetMEDIA_UNMOUNTABLE;
    {class} property MEDIA_UNMOUNTED: JString read _GetMEDIA_UNMOUNTED;
  end;

  [JavaSignature('android/os/Environment')]
  JEnvironment = interface(JObject)
    ['{8A8591BC-BC01-4338-91D8-2671DAB231F8}']
  end;
  TJEnvironment = class(TJavaGenericImport<JEnvironmentClass, JEnvironment>) end;

{$ENDIF}

implementation

{$IFDEF ANDROID}

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.Environment.JEnvironment', TypeInfo(Androidapi.JNI.Environment.JEnvironment));
end;

{$ENDIF}

initialization
  {$IFDEF ANDROID}
    RegisterTypes;
  {$ENDIF}
end.


