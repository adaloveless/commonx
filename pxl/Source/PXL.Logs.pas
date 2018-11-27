unit PXL.Logs;
{
  This file is part of Asphyre Framework, also known as Pascal eXtended Library (PXL).
  Copyright (c) 2000 - 2015  Yuriy Kotsarenko

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.
}
interface

{$INCLUDE PXL.Config.inc}

{$IF DEFINED(MSWINDOWS) AND NOT DEFINED(PXL_CONSOLE)}
  {$DEFINE PXL_LOG_TO_FILE}
{$ENDIF}

uses
  PXL.TypeDef;

type
  { Type of log information to be displayed. }
  TLogType = (
    { Information is treated depending on default settings for each of platforms.  }
    Default,

    { Information is treated as a hint. }
    Hint,

    { Information is treated as a warning with minimal severity. }
    Warning,

    { Information is treated as a severe error. }
    Error);

{ Sends information text to logging console (the location and context of which depends on platform). }
procedure LogText(const Text: StdString; const LogType: TLogType = TLogType.Default; const Tag: StdString = '');

implementation

uses
{$IFDEF ANDROID}
  {$IFDEF FPC}Android.Log,{$ELSE}Androidapi.Log,{$ENDIF}
{$ENDIF}

  SysUtils;

{$IFDEF PXL_LOG_TO_FILE}
var
  LogFile: TextFile;

  ExecFile: StdString = '-';
  ExecDate: StdString = '--';
  ExecPath: StdString = '';

  LogFileName: StdString = 'file-xxxx-xx-xx-xx.log';
  LogFileHour: Integer = -1;
  LogFileDay : Integer = -1;

function IntToStr2(const Value: Integer): StdString;
begin
  Result := IntToStr(Value);
  if Length(Result) < 2 then
    Result := '0' + Result;
end;

function GetLogFileName: StdString;
var
  Timestamp: TDateTime;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  Timestamp := Now;

  DecodeDate(Timestamp, Year, Month, Day);
  DecodeTime(Timestamp, Hour, Min, Sec, MSec);

  Result := ExecPath + ExecFile + '-' + IntToStr(Year) + '-' + IntToStr2(Month) + '-' + IntToStr2(Day) + '-' +
    IntToStr2(Hour) + '.log';
end;

function StartupFile(const FileName: StdString): Boolean;
begin
  AssignFile(LogFile, FileName);

  ReWrite(LogFile);
  if IOResult <> 0 then
  begin
    CloseFile(LogFile);
    Exit(False);
  end;

  WriteLn(LogFile, 'Executed name: ' + ExecPath + ExecFile);
  WriteLn(LogFile, 'Executed date: ' + ExecDate);
  WriteLn(LogFile, '-------------------- ENTRY -----------------------');

  Result := IOResult = 0;
  CloseFile(LogFile);
end;

function ValidateLogFileName: Boolean;
var
  Timestamp: TDateTime;
  Day, Hour, Min, Sec, MSec: Word;
begin
  Timestamp := Now;

  DecodeTime(Timestamp, Hour, Min, Sec, MSec);
  Day := Trunc(Timestamp);

  if (LogFileHour = -1) or (LogFileHour <> Hour) or (LogFileDay = -1) or (LogFileDay <> Day) then
  begin
    LogFileHour := Hour;
    LogFileDay := Day;
    LogFileName := GetLogFileName;

    if not FileExists(LogFileName) then
      Result := StartupFile(LogFileName)
    else
      Result := True;
  end
  else
    Result:= True;
end;
{$ENDIF}

procedure LogText(const Text: StdString; const LogType: TLogType; const Tag: StdString);

{$IFDEF PXL_LOG_TO_FILE}
var
  Hour, Min, Sec, MSec: Word;
{$ENDIF}

{$IFDEF ANDROID}
const
  DefaultTag = 'PXL';
var
  Priority: {$IFDEF FPC}LongInt{$ELSE}android_LogPriority{$ENDIF};
  {$IFNDEF FPC}M: TMarshaller;{$ENDIF}
{$ENDIF}

begin
{$IFDEF PXL_LOG_TO_FILE}
  if not ValidateLogFileName then
    Exit;

  AssignFile(LogFile, LogFileName);
  Append(LogFile);
  if IOResult <> 0 then
  begin
    CloseFile(LogFile);
    Exit;
  end;

  DecodeTime(Time, Hour, Min, Sec, MSec);
  WriteLn(LogFile, '[' + IntToStr2(Min) + '] ' + Text);

  CloseFile(LogFile);
{$ENDIF}

{$IFDEF ANDROID}
  case LogType of
    TLogType.Hint:
      Priority := ANDROID_LOG_INFO;

    TLogType.Warning:
      Priority := ANDROID_LOG_WARN;

    TLogType.Error:
      Priority := ANDROID_LOG_ERROR;

    else
      Priority := ANDROID_LOG_DEBUG;
  end;

{$IFDEF FPC}
  if Length(Tag) < 1 then
    __android_log_write(Priority, DefaultTag, PAnsiChar(Text))
  else
    __android_log_write(Priority, PAnsiChar(Tag), PAnsiChar(Text));
{$ELSE}
  if Length(Tag) < 1 then
    __android_log_write(Priority, DefaultTag, M.AsAnsi(Text).ToPointer)
  else
    __android_log_write(Priority, M.AsAnsi(Tag).ToPointer, M.AsAnsi(Text).ToPointer);
{$ENDIF}

{$ENDIF}

{$IFDEF PXL_CONSOLE}
  WriteLn(Text);
{$ENDIF}
end;

initialization
{$IFDEF PXL_LOG_TO_FILE}
  ExecFile := ExtractFileName(ParamStr(0));
  ExecPath := ExtractFilePath(ParamStr(0));
  ExecDate := DateTimeToStr(Now);
{$ENDIF}

end.

