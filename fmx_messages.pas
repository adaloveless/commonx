unit fmx_messages;
{$DEFINE INCLUDED}
{$IFDEF ANDROID}
//    {$I FMX_messages_android.pas}
interface
implementation
end.

{$ENDIF}
{$IFDEF MSWINDOWS}
    {$I FMX_messages_windows.pas}
{$ENDIF}
{$IFDEF IOS}
//    {$I FMX_messages_android.pas}
interface
implementation
end.

{$ENDIF}

{$IFDEF OSX}
//    {$I FMX_messages_android.pas}
interface
implementation
end.

{$ENDIF}

