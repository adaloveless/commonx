unit RDTPBernBabyBernServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPBernBabyBernRQs.txt}
{END}
interface
uses
  fastbitmap, helpers.stream, stringx, typex, systemx, imagemagick, rdtpprocessor, orderlyinit, rdtpserverlist, RDTPBernBabyBernServer, packethelpers, dir, dirfile, classes, sysutils, svnclient, consolelock, types, regionlock;
type
  TRDTPBernBabyBernServer = class(TRDTPBernBabyBernServerBase)
  private
  protected
  public
{INTERFACE_START}
    function RQ_ListFiles(filespec:string):string;overload;override;
    function RQ_GetFile(filename:string):TStream;overload;override;
    function RQ_PutFile(filename:string; bytes:TStream):boolean;overload;override;
    procedure RQ_PutSpriteSheet(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream);overload;override;
    function RQ_GetSpriteSheet(filename:string; out bytes0:TStream; out bytes1:TStream; out bytes2:TStream):boolean;overload;override;
    procedure RQ_PutSpriteSheet2(filename:string; bytes0:TStream; bytes1:TStream; bytes2:TStream; refmap:TStream);overload;override;
    function RQ_GetSpriteSheet2(filename:string; out bytes0:TStream; out bytes1:TStream; out bytes2:TStream; out refmap:TStream):boolean;overload;override;

{INTERFACE_END}
    function GraphicsPath: string;
    function TryGetRegionLock(sUser: string; sDevice: string; sFile: string; leaseTimeInMinutes: ni; region: TRect; out bCollidingLock: string): boolean;
    procedure ReleaseAllLocksForUser(sUser: string; sDevice: string);
    function NeedRegionLocker: TRegionLocker;
    procedure NoNeedRegionLocker(rl: TRegionLocker);
    function RegionLockerFileName: string;
  end;

implementation



procedure oinit;
begin
  RDTPServers.RegisterRDTPProcessor('BernBabyBern', TRDTPBernBabyBernServer);
end;

procedure ofinal;
begin
  //noimp
end;

{ TRDTPBernBabyBernServer }

function TRDTPBernBabyBernServer.GraphicsPath: string;
begin
  result := dllpath+'graphics\';
end;

function TRDTPBernBabyBernServer.NeedRegionLocker: TRegionLocker;
begin
  result := TRegionLocker.create;
  if fileexists(self.RegionLockerFileName) then
    result.LoadFromFile(RegionLockerFileName);


end;

procedure TRDTPBernBabyBernServer.NoNeedRegionLocker(rl: TRegionLocker);
begin
  rl.PersistToFile(RegionLockerFileName);
  rl.ClearLocks;
  rl.Free;


end;

function TRDTPBernBabyBernServer.RegionLockerFileName: string;
begin
  result := GetTempPath+'bern_graphics.locks';
end;

procedure TRDTPBernBabyBernServer.ReleaseAllLocksForUser(sUser,
  sDevice: string);
var
  rl: TRegionLocker;
  l: TRegionLock;
begin
  LockConsole;
  try
    rl := NeedRegionLocker;
    try
      rl.ReleaseLocksForUser(sUser, sDevice);

    finally
      NoNeedRegionLocker(rl);
    end;
  finally
    UnlockConsole;
  end;

end;

function TRDTPBernBabyBernServer.RQ_GetFile(filename: string): TStream;
var
  fs: TFileStream;
begin
  result := TMemoryStream.create;
  fs := TFileStream.create(dllpath+'graphics\'+filename, fmOpenRead+fmShareDenyNone);
  try
    result.CopyFrom(fs, fs.size);
  finally
    fs.free;
  end;
end;

function TRDTPBernBabyBernServer.RQ_GetSpriteSheet(filename: string;
  out bytes0, bytes1, bytes2: TStream): boolean;
var
  fs0, fs1, fs2: TFileStream;
  a: TDynStringArray;
  t: ni;
  fbm: TFastBitmap;
begin
  inherited;
  LockConsole;
  try

  svnclient.svnUpdate(dllpath+'\graphics');

  result := true;
  filename := changefileext(filename, '.psd');
  a := imagemagick.PSDtoPNGList(graphicspath+filename, 'layers.png');

  if length(a) > 4 then begin
    raise ECritical.create('this is not a sprite sheet');
  end;

  for t := 1 to high(a) do begin
    fbm := TFastBitmap.create;
    try
      //fbm.EnableAlpha := true;
      fbm.LoadFromFile(a[t]);
      a[t] := changefileext(a[t],'.pgf');
      fbm.SaveToProprietaryFile(a[t]);
    finally
      fbm.Free;
    end;
  end;

  //create missing layers
  for t := high(a)+1 to 3 do begin
    setlength(a,4);
    fbm := TFastBitmap.create;
    try
      //fbm.EnableAlpha := true;
      fbm.LoadFromFile(a[0]);
      fbm.Canvas.Clear(0);
      a[t] := changefileext(a[0],'.blank'+inttostr(t)+'.pgf');
      fbm.SaveToProprietaryFile(a[t]);
    finally
      fbm.Free;
    end;
  end;

  fs0 := TFileStream.create(a[1], fmOpenRead+fmShareDenyNone);
  fs1 := TFileStream.create(a[2], fmOpenRead+fmShareDenyNone);
  fs2 := TFileStream.create(a[3], fmOpenRead+fmShareDenyNone);
  bytes0 := fs0;
  bytes1 := fs1;
  bytes2 := fs2;
  finally
    unlockconsole;
  end;


end;

function TRDTPBernBabyBernServer.RQ_GetSpriteSheet2(filename: string;
  out bytes0, bytes1, bytes2, refmap: TStream): boolean;
var
  fs0, fs1, fs2, fsr: TFileStream;
  a: TDynStringArray;
  t: ni;
  fbm: TFastBitmap;
  outref: string;
begin
  inherited;
  refmap := nil;
  LockConsole;
  try

    svnclient.svnUpdate(dllpath+'\graphics');

    result := true;
    filename := changefileext(filename, '.psd');
    a := imagemagick.PSDtoPNGList(graphicspath+filename, 'layers.png');

    if length(a) > 4 then begin
      raise ECritical.create('this is not a sprite sheet');
    end;

    for t := 1 to high(a) do begin
      fbm := TFastBitmap.create;
      try
        //fbm.EnableAlpha := true;
        fbm.LoadFromFile(a[t]);
        a[t] := changefileext(a[t],'.pgf');
        fbm.SaveToProprietaryFile(a[t]);
      finally
        fbm.Free;
      end;
    end;

    //create missing layers
    for t := high(a)+1 to 3 do
    begin
      setlength(a,4);
      fbm := TFastBitmap.create;
      try
        //fbm.EnableAlpha := true;
        fbm.LoadFromFile(a[0]);
        fbm.Canvas.Clear(0);
        a[t] := changefileext(a[0],'.blank'+inttostr(t)+'.pgf');
        fbm.SaveToProprietaryFile(a[t]);
      finally
        fbm.Free;
      end;
    end;

    fs0 := TFileStream.create(a[1], fmOpenRead+fmShareDenyNone);
    fs1 := TFileStream.create(a[2], fmOpenRead+fmShareDenyNone);
    fs2 := TFileStream.create(a[3], fmOpenRead+fmShareDenyNone);
    bytes0 := fs0;
    bytes1 := fs1;
    bytes2 := fs2;


    filename := changefileext(graphicspath+filename, '.reflection.psd');
    if fileexists(filename) then
    begin
      outref := changefileext(gettemppath+extractfilename(filename), '.png');
      imagemagick.PSDtoPNG(filename, outref);

      if fileexists(outref) then begin
        fbm := TFastBitmap.create;
        try
          fbm.LoadFromFile(outref);
          outref := changefileext(outref, '.pgf');
          fbm.SaveToProprietaryFile(outref);
        finally
          fbm.free;
          fbm := nil;
        end;

        if fileexists(outref) then begin
          fsr := TFileStream.create(outref, fmOpenREad+fmShareDenyNone);
          refmap := fsr;
        end;
      end;
    end;

  finally
    unlockconsole;
  end;
end;

function TRDTPBernBabyBernServer.RQ_ListFiles(filespec:string):string;
var
  dir: Tdirectory;
  f: TFileInformation;
  sl: TStringList;
begin
  lockconsole;
  try

    svnclient.svnupdate(dllpath+'graphics');
    dir := TDirectory.create(dllpath+'graphics\', filespec, 0,0,false, true, false);
    try
      sl := TStringlist.create;
      try
        while dir.GetNextFile(f) do
        begin
          sl.add(f.Name);
        end;
        result := sl.text;
      finally
        sl.free;
      end;
    finally
      dir.detachandfree;
      dir := nil;
    end;
  finally
    UnlockConsole;
  end;
end;


function TRDTPBernBabyBernServer.RQ_PutFile(filename: string;
  bytes: TStream): boolean;
var
  fs: TFileStream;
  sFullName: string;
begin
  LockConsole;
  try
    sFullName := dllpath+'graphics\'+filename;
    fs := TFileStream.create(sFullName, fmCreate);
    try
      fs.CopyFrom(bytes, bytes.size);
      bytes.free;
      bytes := nil;
    finally
      fs.free;
    end;

    svnclient.SvnAdd(sFullName);
    svnclient.SvnCommit(sFullName, 'auto check-in');
  finally
    UnlockConsole;
  end;
end;

procedure TRDTPBernBabyBernServer.RQ_PutSpriteSheet(filename: string; bytes0,
  bytes1, bytes2: TStream);
var
  s0,s1,s2: string;
  fs0, fs1,fs2: TFileStream;
  a: TDynStringArray;
  fbm: TFastBitmap;
begin
  inherited;
  LockConsole;
  try
    try
      filename := changefileext(filename, '.psd');
      ForceDirectories(GetTempPathForThread);
      s0 := GetTempPathForThread+'incoming-0.pgf';
      s1 := GetTempPathForThread+'incoming-1.pgf';
      s2 := GetTempPathForThread+'incoming-2.pgf';

      fs0 := TFileStream.create(s0, fmCreate);
      fs1 := TFileStream.create(s1, fmCreate);
      fs2 := TFileStream.create(s2, fmCreate);


      Stream_GuaranteeCopy(bytes0, fs0, bytes0.Size);
      Stream_GuaranteeCopy(bytes1, fs1, bytes1.Size);
      Stream_GuaranteeCopy(bytes2, fs2, bytes2.Size);

      setlength(a,3);
    finally
      bytes0.free;
      bytes1.Free;
      bytes2.free;
      fs0.Free;
      fs1.free;
      fs2.free;
    end;

    fbm := TFastBitmap.create;
    try
      fbm.LoadFromProprietaryFile(s0);
      s0 := changefileext(s0, '.png');
      fbm.SaveToFile(s0);
      fbm.LoadFromProprietaryFile(s1);
      s1 := changefileext(s1, '.png');
      fbm.SaveToFile(s1);
      fbm.LoadFromProprietaryFile(s2);
      s2 := changefileext(s2, '.png');
      fbm.SaveToFile(s2);
    finally
      fbm.free;
    end;

    a[0] := s0;
    a[1] := s1;
    a[2] := s2;


    imagemagick.PNGListToPSD(a, graphicspath+filename);
    EmptyPath(GetTempPathForThread);
    RemoveDir(GetTempPAthForThread);

    svnclient.SvnAdd(dllpath+'\graphics\*.*');
    svnclient.SvnCommit(dllpath+'\graphics\*.*', 'auto check-in');
  finally
    UnlockConsole;
  end;
end;

procedure TRDTPBernBabyBernServer.RQ_PutSpriteSheet2(filename: string; bytes0,
  bytes1, bytes2, refmap: TStream);
var
  s0,s1,s2: string;
  fs0, fs1,fs2, fsr: TFileStream;
  a: TDynStringArray;
  fbm: TFastBitmap;
begin
  inherited;
  LockConsole;
  try
    try
      filename := changefileext(filename, '.psd');
      ForceDirectories(GetTempPathForThread);
      s0 := GetTempPathForThread+'incoming-0.pgf';
      s1 := GetTempPathForThread+'incoming-1.pgf';
      s2 := GetTempPathForThread+'incoming-2.pgf';


      fs0 := TFileStream.create(s0, fmCreate);
      fs1 := TFileStream.create(s1, fmCreate);
      fs2 := TFileStream.create(s2, fmCreate);

      Stream_GuaranteeCopy(bytes0, fs0, bytes0.Size);
      Stream_GuaranteeCopy(bytes1, fs1, bytes1.Size);
      Stream_GuaranteeCopy(bytes2, fs2, bytes2.Size);

      setlength(a,3);
    finally
      bytes0.free;
      bytes1.Free;
      bytes2.free;
      fs0.Free;
      fs1.free;
      fs2.free;
    end;

    fbm := TFastBitmap.create;
    try
      fbm.LoadFromProprietaryFile(s0);
      s0 := changefileext(s0, '.png');
      fbm.SaveToFile(s0);
      fbm.LoadFromProprietaryFile(s1);
      s1 := changefileext(s1, '.png');
      fbm.SaveToFile(s1);
      fbm.LoadFromProprietaryFile(s2);
      s2 := changefileext(s2, '.png');
      fbm.SaveToFile(s2);
    finally
      fbm.free;
    end;


    a[0] := s0;
    a[1] := s1;
    a[2] := s2;

    imagemagick.PNGListToPSD(a, graphicspath+filename);


    //determine ref file name that we'll save the stream to
    s0 := GetTempPathForThread+'ref.pgf';
    fsr := TFileStream.create(s0, fmCreate);
    try
      //copy refmap to filestream
      stream_GuaranteeCopy(refmap, fsr, refmap.size);
    finally
      fsr.free;
      refmap.free;
      fsr := nil;
      refmap := nil;
    end;

    //convert pgf to PNG for ref map
    fbm := TFastBitmap.create;
    try
      fbm.LoadFromProprietaryFile(s0);
      s0 := changefileext(s0, '.png');
      fbm.SaveToFile(s0);
    finally
      fbm.free;
    end;

    imagemagick.PNGToPSD(s0, graphicspath+changefileext(filename,'.reflection.psd'));


    EmptyPath(GetTempPathForThread);
    RemoveDir(GetTempPAthForThread);


    svnclient.SvnAdd(dllpath+'\graphics\*.*');
    svnclient.SvnCommit(dllpath+'\graphics\*.*', 'auto check-in');

  finally
    UnlockConsole;
  end;

end;

function TRDTPBernBabyBernServer.TryGetRegionLock(sUser, sDevice, sFile: string;
  leaseTimeInMinutes: ni; region: TRect; out bCollidingLock: string): boolean;
var
  rl: TRegionLocker;
  l: TRegionLock;
begin
  LockConsole;
  try
    rl := NeedRegionLocker;
    try
      l := TRegionLock.create;
      l.user := sUser;
      l.filename := sFile;
      l.device := sDevice;
      l.expires := now+(leasetimeInMinutes/(60*24));
      result := rl.TryGet(l, 0);
      if not result then begin
        l.Free;//only need to free if the lock failed to be acquired
        l := nil;
      end;

    finally
      NoNeedRegionLocker(rl);
    end;
  finally
    UnlockConsole;
  end;

end;

initialization
  init.RegisterProcs('RDTPBernBabyBernServerImplib', oinit, ofinal, 'RDTPServerList');
end.
