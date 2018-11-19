unit FileFind;
{$I DelphiDefs.inc}
interface

uses
  Classes, SysUtils, systemx, typex,
{$IFDEF WINDOWS}
    winapi.windows,
{$ELSE}
{$IFDEF ANDROID}
    Androidapi.IOUtils,
{$ENDIF}
    ios.stringx.iosansi,
{$ENDIF}
    system.ioutils;

Type
  TFindScope = record
    First: boolean;
  end;


procedure ResetEasyFind(out scope: TFindScope);

{read below -- EasyFindFile}

function  EasyFindFile(var scope: TFindScope; strFileSpec: string; AttrResult,
          AttrMask: Integer;  var SearchRec: TsearchRec): boolean;
{allows scanning files in a WHILE loop  (make sure you call ResetEasyFind FIRST,
and call FindClose(rec: TSearchRec) in the end). Also makes up for serious
inadequacies in the FindFile routine, by using an Attribute mask... use the mask
to specify which attributes you CARE about, while you use the AttrResult to
specify the required values of those attributes.}

procedure CloseEasyFind(var scope: TFindScope; var SearchRec: TsearchRec);
function GetFileSize(sFile: string): int64;
function IntegerToFileAttributes(const Attributes: Integer): TFileAttributes;



implementation

{------------------------------------------------------------------------------}
function LastPos(sub,s : string):integer;
label 1;
var
  p: integer;
  t: integer;
begin
  p:=0;

  for t:=((length(s))-length(sub))+1 downto 0 do begin
    if copy(s, t, length(sub))=sub then begin
      p:=t;
      goto 1;
    end;
  end;

1: result:=p;
end;
procedure FreeListContents(list: TList);
var
  t: integer;
begin
  for t := list.count-1 downto 0 do begin
{$IFDEF LOGFILES}
		WriteDayfileLine('ROUTINES MEM -- Free ' + TFileInformation(list[t]).fullname);
{$ENDIF}
    TObject(list[t]).free;
{		TFileInformation(list[t]).free; 		JAB: does not work }
    list.delete(t);
  end;
end;

procedure ResetEasyFind(out scope: TFindScope);
begin
  scope.First := true;
end;

{------------------------------------------------------------------------------}
{Use this area to docuement techniques for getting files outta Easy find file....
it can be tricky... even though easyfind file is easier than traditional
findfirst, findnext stuff.}
{To get all files... excluding directories
AttrResult = 0, AttrMask = faDirectory+faVolumeID}


procedure CloseEasyFind(var scope: TFindScope; var SearchRec: TsearchRec);
begin
{$IFDEF WINDOWS}
  FindClose(searchRec.FindHandle);
{$ELSE}
  FindClose(searchRec);
{$ENDIF}
end;

function EasyFindFile(var scope: TFindScope; strFileSpec: string; AttrResult, AttrMask: Integer;
  var SearchRec: TsearchRec): boolean;
        {To use EasyFindFile, you must specify which attributes should be used}
        {and what their required states should be determining whether or not a}
        {file will show up through the find.}

        {In AttrMask... specify which attributes you care about}
        {In AttrResult... specify the required values for each attribute}

        {***********E X A M P L E :* *********}
        {     AttrMask =    faDirectory + faVolumeID + faHidden + faReadOnly}
        {   AttrResult =  faHidden}
        {Returns files entries that are NOT folders, are NOT volumeIDs, are}
        {NOT ReadOnly, but ARE Hidden... but it doesn't care whether or not the}
        {file is Archive or System (because faArchive and faSystem were never}

        {specified in the AttrMask)}

        {See the definition for FindFirst for a list of valid attribute values}
var
  found, eofind: boolean;
  path: string;
  fileAttr: integer;
begin
  found:=false;
  eofind:=false;
  path:=slash(extractfilepath(strFileSpec));
  while not found and not eofind do
    if scope.First then
      if FindFirst(strFileSpec, faAnyFile{$IFDEF GE_XE4}and (not faVolumeID){$ENDIF}, searchRec) = 0 then begin
        scope.first:=false;
        fileAttr:=searchRec.Attr;//fileGetAttr(path+searchrec.name);
        if (fileAttr and AttrMask) = (AttrResult and AttrMask) then
          found:= true
        else
          found:=false;
      end else
        eofind:=true {//If nothing was found... end find;}
      {//end;}
    else
      if FindNext(searchRec) = 0 then begin
        fileAttr:=searchRec.Attr;//fileGetAttr(path+searchrec.name);
        if (fileAttr and AttrMask) = (AttrResult and AttrMask) then
          found:= true
        else
          found:=false
      end else
        eofind:=true;
      {//endif}
    {//end if ;}
  {//end while;}

  result:=found;
end;

{------------------------------------------------------------------------------}
function AdjustPath(path: string): string;
var
  sTemp:string;
  slash: string;
begin
  if path = '' then begin
    result := '';
    exit;
  end;
  sTemp := path;

  {$IFDEF CLX}
  slash := '/';
  {$ELSE}
  slash := '\';
  {$ENDIF}



  if sTemp[length(sTemp)]=slash then
    result:=sTemp
  else
    result:=sTemp+slash;
end;

function GetFileSize(sFile: string): int64;
var
  sr: TSearchRec;
  scope: TFindScope;
begin
  result := -1;
  ResetEasyFind(scope);
  try

    if EasyFindFile(scope, sFile, 0,0, sr) then begin
      result := sr.Size
    end;

  finally
    systemx.FindClose(sr);
  end;


end;


function IntegerToFileAttributes(
  const Attributes: Integer): TFileAttributes;
{$IFDEF MSWINDOWS}
begin
  Result := [];

  if Attributes and FILE_ATTRIBUTE_READONLY <> 0 then
    Include(Result, TFileAttribute.faReadOnly);
  if Attributes and FILE_ATTRIBUTE_HIDDEN <> 0 then
    Include(Result, TFileAttribute.faHidden);
  if Attributes and FILE_ATTRIBUTE_SYSTEM <> 0 then
    Include(Result, TFileAttribute.faSystem);
  if Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
    Include(Result, TFileAttribute.faDirectory);
  if Attributes and FILE_ATTRIBUTE_ARCHIVE <> 0 then
    Include(Result, TFileAttribute.faArchive);
  if Attributes and FILE_ATTRIBUTE_DEVICE <> 0 then
    Include(Result, TFileAttribute.faSymLink);
  if Attributes and FILE_ATTRIBUTE_NORMAL <> 0 then
    Include(Result, TFileAttribute.faNormal);
  if Attributes and FILE_ATTRIBUTE_TEMPORARY <> 0 then
    Include(Result, TFileAttribute.faTemporary);
  if Attributes and FILE_ATTRIBUTE_SPARSE_FILE <> 0 then
    Include(Result, TFileAttribute.faSparseFile);
  if Attributes and FILE_ATTRIBUTE_COMPRESSED <> 0 then
    Include(Result, TFileAttribute.faCompressed);
  if Attributes and FILE_ATTRIBUTE_OFFLINE <> 0 then
    Include(Result, TFileAttribute.faOffline);
  if Attributes and FILE_ATTRIBUTE_NOT_CONTENT_INDEXED <> 0 then
    Include(Result, TFileAttribute.faNotContentIndexed);
  if Attributes and FILE_ATTRIBUTE_ENCRYPTED <> 0 then
    Include(Result, TFileAttribute.faEncrypted);
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  Result := [];

  raise ECritical.create('unimplemented for posix');

{  if Attributes and S_IFIFO <> 0 then
    Include(Result, TFileAttribute.faNamedPipe);
  if Attributes and S_IFCHR <> 0 then
    Include(Result, TFileAttribute.faCharacterDevice);
  if Attributes and S_IFDIR <> 0 then
    Include(Result, TFileAttribute.faDirectory);
  if Attributes and S_IFBLK <> 0 then
    Include(Result, TFileAttribute.faBlockDevice);
  if Attributes and S_IFREG <> 0 then
    Include(Result, TFileAttribute.faNormal);
  if Attributes and S_IFLNK <> 0 then
    Include(Result, TFileAttribute.faSymLink);
  if Attributes and S_IFSOCK <> 0 then
    Include(Result, TFileAttribute.faSocket);
  if Attributes and S_IFWHT <> 0 then
    Include(Result, TFileAttribute.faWhiteout);
  if Attributes and S_IRUSR <> 0 then
    Include(Result, TFileAttribute.faOwnerRead);
  if Attributes and S_IWUSR <> 0 then
    Include(Result, TFileAttribute.faOwnerWrite);
  if Attributes and S_IXUSR <> 0 then
    Include(Result, TFileAttribute.faOwnerExecute);
  if Attributes and S_IRGRP <> 0 then
    Include(Result, TFileAttribute.faGroupRead);
  if Attributes and S_IWGRP <> 0 then
    Include(Result, TFileAttribute.faGroupWrite);
  if Attributes and S_IXGRP <> 0 then
    Include(Result, TFileAttribute.faGroupExecute);
  if Attributes and S_IROTH <> 0 then
    Include(Result, TFileAttribute.faOthersRead);
  if Attributes and S_IWOTH <> 0 then
    Include(Result, TFileAttribute.faOthersWrite);
  if Attributes and S_IXOTH <> 0 then
    Include(Result, TFileAttribute.faOthersExecute);
  if Attributes and S_ISUID <> 0 then
    Include(Result, TFileAttribute.faUserIDExecution);
  if Attributes and S_ISGID <> 0 then
    Include(Result, TFileAttribute.faGroupIDExecution);
  if Attributes and S_ISVTX <> 0 then
    Include(Result, TFileAttribute.faStickyBit);}
end;
{$ENDIF}



end.

