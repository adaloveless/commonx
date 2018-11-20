unit ut_ios_native_stuff;

interface

uses
  typex, classes, sysutils, unittest, systemx, ioutils, iosapi.foundation, stringx;


type
  TUT_IOS_NativeStuff = class(TUnitTest)
  public
    procedure DoExecute;override;
    function Test1: string;
    function Test2: string;
    function Test3: string;
    function Test4: string;
    function Test5: string;
    function Test6: string;
    function Test7: string;
    function Test8: string;
    function Test9: string;
    function Test10: string;

  end;

implementation





{ TUT_IOS_NativeStuff }

procedure TUT_IOS_NativeStuff.DoExecute;
var
  sl: TStringlist;
begin
  inherited;
  sl := TStringlist.create;
  try
    case Variation of
      1: begin
        result := Test1();
      end;
      2: begin
        result := Test2();
      end;
      3: begin
        result := Test3();
      end;
      4: begin
        result := Test4();
      end;
      5: begin
        result := Test5();
      end;
      6: begin
        result := Test6();
      end;
      7: begin
        result := Test7();
      end;
      8: begin
        result := Test8();
      end;
      9: begin
        result := Test9();
      end;
      10: begin
        result := Test10();
      end;
    end;

  finally
    sl.Free;
    sl := nil;
  end;

end;

function TUT_IOS_NativeStuff.Test1: string;
var
  fileManager: iosapi.foundation.NSFileManager;
  error: NSError;
  sPath: string;
  sSource, sTarget: string;
  sl1, sl2: tStringlist;
begin
  VariationName := 'Copy files using NSfileManager and report contents of target.';
  //setup file names
  sSource := GetTempPath+'helloWorld.txt';
  sTarget := GetTempPAth+'poop.txt';

  //create some string lists
  sl1 := TStringlist.Create;
  sl2 := tStringlist.Create;

  //put some text in a file and save it as source
  sl1.Text := 'Hello World';
  sl1.SaveToFile(sSource);

  //create the file manager
  fileManager := TNSFileManager.Create;

  error := TNSError.Create;
  if fileManager.fileExistsAtPath(nsstr(sTarget)) then
    fileManager.removeItemAtPath(nsstr(sTarget), error);

  if error.code <> 0 then
    raise Exception.Create('NSError code:'+inttostr(error.code));


  fileManager.copyItemAtPath(nsstr(sSource), nsstr(sTarget), error);
  sl2.LoadFromFile(starget);

  result := sl2.Text;
end;

function TUT_IOS_NativeStuff.Test10: string;
begin
  result := 'no execution';
end;

function TUT_IOS_NativeStuff.Test2: string;
var
  fm: NSFileManager;
  err: NSError;
  sSource: string;
  sTarget: string;
begin
  sSource := GetTempPath+'helloWorld.txt';
  sTarget := GetTempPAth+'poop.txt';

  VAriationName := 'Delete files from previous step. (Expect FALSE FALSE)';
  fm := TNSFileManager.Create;
  err := TNSError.create;

  if fm.fileExistsAtPath(nsstr(sSource)) then
    fm.removeItemAtPath(nsstr(sSource),err);

  if fm.fileExistsAtPath(nsstr(sTarget)) then
    fm.removeItemAtPath(nsstr(sTarget),err);

  result := booltostr(fm.fileExistsAtPath(nsstr(sSource)))+' '+booltostr(fm.fileExistsAtPath(nsstr(sSource)));
end;

function TUT_IOS_NativeStuff.Test3: string;
var
  sPath: string;
  sSource, sTarget: string;
  sl1, sl2: tStringlist;
begin
  VariationName := 'Perform Variation 1 using systemx calls';
  //setup file names
  sSource := GetTempPath+'helloWorld.txt';
  sTarget := GetTempPAth+'poop.txt';

  //create some string lists
  sl1 := TStringlist.Create;
  sl2 := tStringlist.Create;

  //put some text in a file and save it as source
  sl1.Text := 'Hello World';
  sl1.SaveToFile(sSource);

  if fileexists(sTarget) then begin
    deletefile(sTarget);
  end;

  copyfile(sSource, sTarget);


  sl2.LoadFromFile(starget);

  result := sl2.Text;
end;


function TUT_IOS_NativeStuff.Test4: string;
var
  sSource: string;
  sTarget: string;
begin
  sSource := GetTempPath+'helloWorld.txt';
  sTarget := GetTempPAth+'poop.txt';

  VAriationName := 'Perform variation 2 using systemx calls (Expect FALSE FALSE)';
  if fileExists(sSource) then
    deletefile(sSource);

  if fileExists(sTarget) then
    deletefile(sTarget);

  result := stringx.booltostr(fileexists(sSource))+' '+booltostr(fileExists(sSource));
end;

function TUT_IOS_NativeStuff.Test5: string;
begin
  result := 'no execution';
end;

function TUT_IOS_NativeStuff.Test6: string;
begin
  result := 'no execution';
end;

function TUT_IOS_NativeStuff.Test7: string;
begin
  result := 'no execution';
end;

function TUT_IOS_NativeStuff.Test8: string;
begin
  result := 'no execution';
end;

function TUT_IOS_NativeStuff.Test9: string;
begin
  result := 'no execution';
end;

initialization
  UTF.RegisterClass(TUT_IOS_NativeStuff);

end.
