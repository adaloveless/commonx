unit ffmpeg_tools;
{$DEFINE USE_MFS}

interface

uses
  ExternalMemoryStream, helpers.stream,numbers,search, soundconversions_commandline,
  debug, tickcount, soundinterfaces, commandicons, managedthread, sharedobject, commandprocessor, soundtools, classes, betterobject, easyimage, dir, dirfile, windows, systemx, typex, stringx, exe, sysutils, MultiBufferMemoryFileStream, vcl.imaging.jpeg;

const
  extract_image_ext = 'jpg';

type
  TFFProbe = record
    w,h: single;
    procedure Init;
  end;
  TBleedingEdge = (beAncient, beStable, be4kEra);
  TJVIDFormat = (jvfJPG, jvfDDS, jvfCRN);
  TJVIDMainHeader = packed record
    fourcc: array [0..3] of ansichar;
    version: cardinal;
    display_width: cardinal;
    display_height: cardinal;
    internal_width: cardinal;
    internal_height: cardinal;
    numberofframes: cardinal;
    framerate: cardinal;
    indexoffset: int64;
    format: TJVIDFormat;
    procedure Init;
  end;
  TJVIDHeader = packed record
    FrameNumber: cardinal;
    PrevFrameSize: cardinal;
    FrameSize: cardinal;
    CheckSum: cardinal;
    procedure CalculateChecksum;
    function VerifyChecksum: boolean;
  end;

  Tcmd_VideoToDmxVideo = class(TCommand)
  private
    FOutputFile: string;
    FInputfile: string;
    function GetOutputAudioFile: string;
  public
    procedure Init;override;
    class function Go(sINput: string; sOutput: string): Tcmd_VideoToDmxVideo;
    procedure DoExecute;override;
    procedure InitExpense;override;
    property InputFile: string read FInputfile write FInputFile;
    property OutputFile: string read FOutputFile write FOutputFile;
    property OutputAudioFile: string read GetOutputAudioFile;
  end;

  Tcmd_VideoToJVID = class(TCommand)
  private
    FOutputFile: string;
    FInputfile: string;
    function GetOutputAudioFile: string;
  public
    procedure Init;override;
    class function Go(sINput: string; sOutput: string): Tcmd_VideoToJVID;
    procedure DoExecute;override;
    procedure InitExpense;override;
    property InputFile: string read FInputfile write FInputFile;
    property OutputFile: string read FOutputFile write FOutputFile;
    property OutputAudioFile: string read GetOutputAudioFile;
  end;


  Tint64Array = array[0..0] of int64;
  PInt64Array = ^Tint64Array;

  TJVIdReader = class;//forward

  Tcmd_NextFrame = class (TCommand)
  private
    FReader: TJVIDReader;
  public
    property Reader: TJVIDReader read FReader write Freader;
    procedure DoExecute;override;
  end;

{$IFDEF USE_MFS}
  TJVIDFileStream = TMultiBufferMemoryFileSTream;
{$ELSE}
  TJVIDFileStream = TFileStream;
{$ENDIF}

  TJVIdReader = class(TSharedOBject)
  protected
    stream: TJVIDFileStream;
    procedure ReadCurrentFrame;
    procedure ReadCurrentFrameHeader;
    procedure ReadCurrentFrameBuffer;
  public

    framememory: Pbyte;
    mainheader: TJVIDMainHeader;
    header: TJVIDHeader;
    framenumber: nativeint;
    framememorylength: nativeint;
    index: PInt64Array;
    cmdNextFRame: Tcmd_NextFrame;
    thrPrefetch: TExternalEventThread;
    PrefetchRequestFlag: nativeint;
    PrefetchAckFlag: nativeint;

    procedure Init;override;
    destructor Destroy;override;
    procedure OnNewVideo;virtual;

    procedure LoadFromFile(sFile: string);
    procedure NextFrame(iCount: integer = 1);
    procedure PrevFrame(iCount: integer = 1);
    procedure GotoFrame(iNumber: nativeint);
    procedure GotoTime(rTime: nativefloat);
    procedure Cleanup;
    procedure CleanupMain;
    function FramePOinterAt: nativeint;
    function FileName: string;
    function GetStreamFromFrameMemory: TExternalMemoryStream;
    function TimeToFrameNumber(rTime: nativefloat): nativeint;
    procedure COmpleteCommand;
    procedure PrefetchNextFrame;
    procedure StartPrefetchThread;
    procedure StopPrefetchThread;

    procedure PrefetchThread_OnExecute(sender: TExternalEventThread);
  end;

  Tcmd_XBOXtranscode = class(Tcmd_RunExe)
  private
    Finfile: string;
    Foutfile: string;
  protected
    procedure Init;override;
    procedure InitExpense;override;
    procedure BuildCommandLine;override;
  public
    property InFile: string read Finfile write finfile;
    property outfile: string read Foutfile write foutfile;
  end;

  Tcmd_Mobiletranscode = class(Tcmd_RunExe)
  private
    Finfile: string;
    Foutfile: string;
  protected
    procedure InitExpense;override;
    procedure BuildCommandLine;override;
  public
    property InFile: string read Finfile write finfile;
    property outfile: string read Foutfile write foutfile;
  end;

  TFFMPEG_Action = (ffExtractFrame,ffExtractFrames, ffToDmxVideo, ffToMp3, ffToChromecastLQ,ffToChromecastHQ, ffToChromecastOQ,ffToOculus,ffProbe);

  Tcmd_FFMPEG = class(Tcmd_RunExe)
  private
    FW: nativeint;
    Fh: nativeint;
    Foutfile: string;
    FAction: TFFMPeg_Action;
    Finfile: string;
    Foutdir: string;
    Findir: string;
    FBleedingEdge: TBleedingEdge;
    FAlternate: boolean;
    FOverWriteOutput: boolean;
    FTotalFrames: ni;
    FDurationInMS: int64;
    procedure SEtOutputFile(const Value: string);
  protected

    procedure InitExpense;override;
    procedure DoExecute;override;
    procedure PreProcess;override;
    procedure PostProcess;override;
    procedure BuildCommandLine;override;
    procedure CC(ccStatus: TConsoleCaptureStatus; sData: string);override;
   public

    procedure Init; override;
    constructor Create; override;
    property ACtion: TFFMPeg_Action read FAction write FAction;
    procedure AfterConstruction; override;

    property InputFile: string read Finfile write finfile;
    property outputdirectory: string read Foutdir write foutdir;
    property InputDirectory: string read Findir write findir;
    property outputFile: string read Foutfile write SEtOutputFile;
    property Width: nativeint read FW write Fw;
    property Height: nativeint read Fh write Fh;
    property BleedingEdge: TBleedingEdge read FBleedingEdge write FBleedingEdge;
    property OverwriteOutput: boolean read FOverWriteOutput write FOverwriteOutput;


  end;


function ffMpegProbeStr(sFile: string; bSynchronous: boolean = true): string;
function ffMpegProbe(sFile: string): TFFProbe;


procedure VideoToDmxVideo(sInputFile: string; sOutputFile: string; cmd: TCommand = nil);
procedure VideoToJVid(sInputFile: string; sOutputFile: string; cmd: TCommand = nil; fmt: TJVIDFormat = jvfJPG);
procedure CreateJVIDFromFrames_JPG(LengthInSeconds: nativefloat; sInputDir: string; sOutFile: string; sINputFilter: string = '*.jpg'; cmd: TCommand = nil);
procedure CreateJVIDFromFrames_Crunch(LengthInSeconds: nativefloat; sInputDir: string; sOutFile: string; sINputFilter: string = '*.jpg'; CrunchFmt: TJVIDFormat = jvfCRN; sCrunchQuality: nativeint = 128; cmd: TCommand = nil);

//procedure REbuildVideoFrames(sInputDir: string; sOutputFile: string);
procedure REbuildVideoFrames(sInputDir: string; sOutputFile: string; w,h,fps: integer);
procedure REbuildVideoFrames_DateOrder(sInputDir: string; sOutputFile: string; w,h,fps: nativeint; ext: string = 'jpg');
procedure REbuildVideoFrames_DateOrder_AggSubfolder(sInputDir: string; sOutputFile: string; w,h,fps: nativeint; ext: string = 'jpg');

procedure ExtractVideoFrames(sInputFile: string; sOutputDir: string; w: nativeint = 0; h: nativeint = 0);
function BeginExtractVideoFrames(sInputFile: string; sOutputDir: string; w: nativeint = 0; h: nativeint = 0): Tcmd_FFMPEG;
procedure EndExtractVideoFrames(cexe: Tcmd_FFMPEG);
procedure ConformVideoFiles(sOutputDir: string; bPad: boolean; bDeleteDuplicates: boolean);
function BeginTranscodeForXBOX(sFile: string; sOutFile:string): Tcmd_XBOXtranscode;
procedure EndTranscodeForXbox(c: Tcmd_XBOXtranscode);
//d:\source\DMX_O_Matic\ffmpeg.exe -i "h:\archives\tv\Workaholics\Episode 1 - Piss & Shit.mkv" -f mp4 -vcodec mpeg4 -b 2000000 -acodec libfaac -ac 2 -ab 128000 i:\tv\test.mp4


function ffmpeg_cpus: ni;
function ffmpeg: string;
//function ffmpeg_ex(sVariation: string = '_bleeding_edge_sept'):string;
function ffmpeg_ex(sVariation: string = '_4k'):string;
implementation

uses
  servervideoparser, tools;

function ffmpeg_cpus: ni;
begin
  result := greaterof(1,lesserof(GetNumberOfProcessors,16));//ffmpeg doesn't really scale past 16 cpus
end;

function ffmpeg: string;
begin
  result := FindTool('ffmpeg.exe');
//  result := adjustpath(extractfilepath(dllname))+'ffmpeg.exe';
end;

function ffmpeg_ex(sVariation: string):string;
begin
  result := FindTool('ffmpeg'+svariation+'.exe');
//  result := adjustpath(extractfilepath(dllname))+'ffmpeg.exe';
end;



procedure REbuildVideoFrames(sInputDir: string; sOutputFile: string; w,h,fps: integer);
var
  dir: TDirectory;
  fil: TfileInformation;
  sNewName: string;
  sLeft: string;
  sRight: string;
  sPArams: string;

begin
//ffmpeg -f image2 -i c:working.%d.jpg -b 1000000 -s 720x480 -aspect 16:9 -f mpeg2video o:output.avi



  dir := TDirectory.create(sInputDir, '*.'+extract_image_ext, 0,0, true);
  try
    while dir.getnextfile(fil) do begin
      sNewName := fil.Name;
      SplitString(sNewName, '.', sLeft, sRight);
      sNewName := sLeft+'.';
      SplitString(sRight,'.', sLeft, sRight);
      sNewName := slash(fil.Path)+sNewName+ inttostr(strtoint(sLeft))+'.'+sRight;
      debug.log('ren: '+fil.fullname+' to '+sNewName);
      RenameFile(fil.fullname, sNewName);
    end;

  finally
    dir.free;
  end;

  debug.log('RUN: '+ffmpeg);
  sParams := '-y -f image2 -r '+inttostr(fps)+' -i "'+sInputDir+'working.%d.jpg" -b 1000000 -s '+inttostr(w)+'x'+inttostr(h)+' -aspect 16:9 -r '+inttostr(fps)+' "'+sOutputFile+'"';
  debug.log(sParams);
  exe.RunProgramAndWait(ffmpeg, sParams, '');

end;

procedure REbuildVideoFrames_DateOrder_AggSubfolder(sInputDir: string; sOutputFile: string; w,h,fps: nativeint; ext: string = 'jpg');
var
  f,dir: TDirectory;
  fil: TfileInformation;
  sNewName: string;
  sLeft: string;
  sRight: string;
  sPArams: string;
  t,u: integer;
  sTempFOlder: string;
  idx: nativeint;
begin
//ffmpeg -f image2 -i c:working.%d.jpg -b 1000000 -s 720x480 -aspect 16:9 -f mpeg2video o:output.avi
  idx := 0;
  //sTempFolder := adjustpath(sInputdir)+'temp\';
  sTempFolder := 'c:\temp\video\';
  DeleteFolder(sTempFolder);
  forcedirectories(sTempFolder);
  f := TDirectory.create(sINputDir, '*.*', 0,0,true, true, false);
  try
    for u := 0 to f.foldercount-1 do begin
      dir := TDirectory.Create(f.Folders[u].FullName, '*.*', 0,0);
      try
        dir.SortByDate;
        if lowercase(dir.path) <> lowercase(sTempFolder) then
        for t:= 0 to dir.Filecount-1 do begin
          fil := dir.Files[t];
          if fil.Size = 0 then begin
            DeleteFile(fil.FullName);
          end else begin
            CopyFile(pchar(fil.FullName), pchar(sTempFolder+inttostr(idx)+'.jpg'), false);
            inc(idx);
          end;
        end;
      finally
        dir.Free;
      end;

    end;


  finally
    f.Free;
  end;

  Debug.log('RUN: '+ffmpeg_ex());
  //took out aspect 16:9
  if (w>0) and (h>0) then begin
    sParams := '-y -f image2 -r '+inttostr(fps)+' -i "'+sTempFolder+'%d.jpg" -b 1000000 -s '+inttostr(w)+'x'+inttostr(h)+' -r '+inttostr(fps)+' "'+sOutputFile+'"';
  end else begin
    sParams := '-y -f image2 -r '+inttostr(fps)+' -i "'+sTempFolder+'%d.jpg" -b 1000000 -r '+inttostr(fps)+' "'+sOutputFile+'"';
  end;

  debug.log(sParams);
  exe.RunProgramAndWait(ffmpeg_ex(), sParams, '',true );
  DeleteFolder(sTempFolder);
end;

procedure REbuildVideoFrames_DateOrder(sInputDir: string; sOutputFile: string; w,h,fps: nativeint; ext: string = 'jpg');
var
  dir: TDirectory;
  fil: TfileInformation;
  sNewName: string;
  sLeft: string;
  sRight: string;
  sPArams: string;
  t: integer;
  sTempFOlder: string;
begin
//ffmpeg -f image2 -i c:working.%d.jpg -b 1000000 -s 720x480 -aspect 16:9 -f mpeg2video o:output.avi

  sTempFolder := slash(sInputdir)+'temp\';
  DeleteFolder(sTempFolder);
  forcedirectories(sTempFolder);
  dir := TDirectory.create(sInputDir, '*.'+ext, 0,0, true);
  dir.SortByDate;
  try
    for t:= 0 to dir.Filecount-1 do begin
      fil := dir.Files[t];
      if (w = 0) or (h = 0) then
        GetImageFileDimensions(dir.Files[t].FullName, w,h);
      if fil.Size = 0 then begin
        DeleteFile(fil.FullName);
      end else begin
        CopyFile(pchar(fil.FullName), pchar(sTempFolder+'temp'+inttostr(t)+'.jpg'), false);
      end;
    end;




//      sNewName := fil.Name;
//      SplitString(sNewName, '.', sLeft, sRight);
//      sNewName := sLeft+'.';
//      SplitString(sRight,'.', sLeft, sRight);
//      sNewName := adjustpath(fil.Path)+sNewName+ inttostr(strtoint(sLeft))+'.'+sRight;
//      debug.log('ren: '+fil.fullname+' to '+sNewName);
//      RenameFile(fil.fullname, sNewName);


  finally
    dir.free;
  end;



  debug.log('RUN: '+ffmpeg_ex());
  //took out aspect 16:9
  if (w>0) and (h>0) then begin
    sParams := '-y -f image2 -r '+inttostr(fps)+' -i "'+sTempFolder+'temp%d.jpg" -b 1000000 -s '+inttostr(w)+'x'+inttostr(h)+' -r '+inttostr(fps)+' "'+sOutputFile+'"';
  end else begin
    sParams := '-y -f image2 -r '+inttostr(fps)+' -i "'+sTempFolder+'temp%d.jpg" -b 1000000 -r '+inttostr(fps)+' "'+sOutputFile+'"';
  end;

  debug.log(sParams);
  exe.RunProgramAndWait(ffmpeg, sParams, '',true );
  DeleteFolder(sTempFolder);
end;

type
  TJVIDBS = class(TBetterObject)
  private

  public

    jpg: TJpegIMage;
    jpgf: TStream;
    ms: TMemoryStream;
    q: nativeint;
    HighSize: nativeint;
    LowSize: nativeint;
    function BSFunc(test: int64): nativeint;
    procedure Search(testfirst: nativeint);
    procedure Init;override;
  end;

function TJVIDBS.BSFunc(test: int64): nativeint;
begin
  if test > 100 then begin
    result := 1;
    exit;
  end;
  if test < 1 then begin
    result := -1;
    exit;
  end;

  q := test;

  ms.Free;
  ms := nil;
  ms := Tmemorystream.Create;
  if (jpgf.Size < HighSize) then begin// THIS IS A SPECIAL CASE
                                      // THERE IS NO SENSE in recompressing
                                      //if the native input is already small
                                      //enough.  It'll just make it slower
                                      //and look like shit.
    jpgf.Seek(0,soBeginning);
    ms.CopyFrom(jpgf, jpgf.Size);
    result := 0;
    exit;
  end else begin
    jpgf.Seek(0,soBeginning);
    jpg.LoadFromStream(jpgf);
    jpg.DIBNeeded;
    jpg.dibneeded;
    jpg.CompressionQuality := q;
    jpg.Smoothing := true;
    jpg.Compress;
    jpg.SaveToStream(ms);
    //Debug('q='+inttostr(q)+' size='+inttostr(ms.Size));
  end;

  if q < 10 then
    result := 0
  else
  if (ms.size > LowSize) and (ms.size < HighSize) then begin
    result := 0;
  end else
  if ms.size <= LowSize then
    result := -1
  else
    result := 1;
end;

procedure CreateJVIDFromFrames_JPG(LengthInSeconds: nativefloat; sInputDir: string; sOutFile: string; sINputFilter: string = '*.jpg'; cmd: TCommand = nil);
type
  TlocalStream = TFileStream;
var
  dir: TDirectory;
  sOut: TlocalStream;
  fil: TFileInformation;
  jpgf: TlocalStream;
  jpg: TJpegImage;
  wh: TJpegImage;
  mainhead: TJVIDMainHeader;
  head: TJVIDHeader;
  prevsize: cardinal;
  t: cardinal;
  a: Pint64array;
  bs: TJVIDBS;
  ms: TMemoryStream;
  q: nativeint;
begin
  prevsize := 0;
  t := 0;
  q := 100;
  dir := TDirectory.Create(sInputDir, sInputFilter, 0,0,false, true,false);
  try
    sOut := TlocalStream.Create(sOutFile, fmCreate);
    try
      sOut.Seek(0,0);
      mainhead.Init;
      mainhead.framerate := round(dir.Filecount / lengthinseconds);
      //determine width and height
      wh := TJpegImage.Create;
      try
        dir.filecount;
        wh.LoadFromFile(dir.Files[0].FullName);
        mainhead.display_width := wh.Width;
        mainhead.display_height := wh.Height;
        mainhead.internal_width := wh.Width;
        mainhead.internal_height := wh.Height;


//        mainhead.internal_width := 1 shl (HighOrderBit(mainhead.display_width)+1);
//        mainhead.internal_height := 1 shl (HighOrderBit(mainhead.display_height)+1);
      finally
        wh.Free;
      end;
      mainhead.numberofframes := dir.Filecount;
      stream_guaranteewrite(sOut, @mainhead, sizeof(mainhead));

      a := GetMemory(sizeof(int64) * dir.filecount);
      if (mainhead.internal_width <> mainhead.display_width) and (mainhead.internal_height <> mainhead.display_height) then
        ResizeImages(sINputDir, '*.jpg', mainhead.internal_width, mainhead.internal_height);



      while dir.GetNextFile(fil) do begin
        jpgf := TlocalStream.Create(fil.FullName, fmOpenRead+fmShareDenyWrite);
        try
          jpg := TJPEGImage.Create;
          try
            ms := nil;
            try
              bs := TJVIDBS.Create;
              bs.ms := ms;
              bs.jpgf := jpgf;
              bs.jpg := jpg;
              bs.Search(q);
              q := bs.q;

              if assigned(cmd) then begin
                cmd.StepCount := dir.Filecount;
                cmd.Step := t;
              end;

              ms := bs.ms;

              ms.Seek(0,soBeginning);

              head.FrameNumber := t;
              head.FrameSize := ms.Size;
              head.PrevFrameSize := prevsize;
              head.CalculateChecksum;
              a[t] := sOut.position;
              Stream_guaranteewrite(sOut, @head, sizeof(head));
              sOut.CopyFrom(ms, ms.Size);

              prevsize := head.FrameSize;
              inc(t);
            finally
              ms.Free;
              ms := nil;
            end;
          finally
            jpg.Free;
          end;
        finally
          jpgf.Free;
        end;
      end;
      mainhead.indexoffset := sOut.Position;
      Stream_guaranteewrite(sOut, @a[0], sizeof(int64) * dir.filecount);
      //rewrite the main header to refresh the indexoffset
      sOut.Seek(0,soBeginning);
      stream_guaranteewrite(sOut, @mainhead, sizeof(mainhead));

    finally
      sOut.Free;
    end;
  finally
    dir.Free;
  end;

end;


procedure CreateJVIDFromFrames_Crunch(LengthInSeconds: nativefloat; sInputDir: string; sOutFile: string; sINputFilter: string = '*.jpg'; CrunchFmt: TJVIDFormat = jvfCRN; sCrunchQuality: nativeint = 128; cmd: TCommand = nil);
var
  dir: TDirectory;
  sOut: TMultibufferMemoryFileStream;
  fil: TFileInformation;
  crunchf: TMultibufferMemoryFileStream;
  sCrunchFile: string;
  wh: TJpegImage;
  mainhead: TJVIDMainHeader;
  head: TJVIDHeader;
  prevsize: cardinal;
  t: cardinal;
  a: Pint64array;
  bs: TJVIDBS;
  q: nativeint;
  sCrunchExt: string;
  c: Tcmd_RunExe;
begin
  if CrunchFmt = jvfDDS then
    sCrunchExt := 'dds';
  if CrunchFmt = jvfCRN then
    sCrunchExt := 'crn';


  prevsize := 0;
  t := 0;
  q := 100;

  c := Tcmd_RunExe.create;
  c.Prog := findtool('crunch_x64.exe');
  c.Params := '/mipMode none /quiet /fileformat '+sCrunchExt+' /quality '+inttostr(sCrunchQuality)+' /outsamedir -file "'+slash(sInputDir)+'*.jpg'+'"';
  c.Hide := true;
//  c.CPUExpense := GEtNumberOfProcessors;
//  c.ConsoleRedirect := false;
  c.Resources.SetResourceUsage('crunch', 1.0);
  c.Start;
  c.WaitFor;
  c.Free;
  c := nil;

  dir := TDirectory.Create(sInputDir, '*.'+sCrunchExt, 0,0,false, true,false);
  try
    sOut := TMultiBufferMemoryFileStream.Create(sOutFile, fmCreate);
    try
      sOut.Seek(0,0);
      mainhead.Init;
      mainhead.framerate := round(dir.Filecount / lengthinseconds);
      mainhead.format := CrunchFmt;
      //determine width and height
      wh := TJpegImage.Create;
      try
        dir.filecount;
        wh.LoadFromFile(changefileext(dir.Files[0].FullName,'.jpg'));
        mainhead.display_width := wh.Width;
        mainhead.display_height := wh.Height;
      finally
        wh.Free;
      end;
      mainhead.numberofframes := dir.Filecount;
      stream_guaranteewrite(sOut, @mainhead, sizeof(mainhead));

      a := GetMemory(sizeof(int64) * dir.filecount);

      while dir.GetNextFile(fil) do begin
        sCrunchFile := fil.FullName;

        crunchf := TMultibufferMemoryFileStream.Create(sCrunchFile, fmOpenRead+fmShareDenyWrite);
        try

          if assigned(cmd) then begin
            cmd.StepCount := dir.Filecount;
            cmd.Step := t;
          end;


          crunchf.Seek(0,soBeginning);

          head.FrameNumber := t;
          head.FrameSize := crunchf.Size;
          head.PrevFrameSize := prevsize;
          head.CalculateChecksum;
          a[t] := sOut.position;
          Stream_guaranteewrite(sOut, @head, sizeof(head));
          sOut.CopyFrom(crunchf, crunchf.Size);

          prevsize := head.FrameSize;
          inc(t);
        finally
          crunchf.Free;
        end;
      end;
      mainhead.indexoffset := sOut.Position;
      Stream_guaranteewrite(sOut, @a[0], sizeof(int64) * dir.filecount);
      //rewrite the main header to refresh the indexoffset
      sOut.Seek(0,soBeginning);
      stream_guaranteewrite(sOut, @mainhead, sizeof(mainhead));

    finally
      sOut.Free;
    end;
  finally
    dir.Free;
  end;

end;

procedure ConformVideoFiles(sOutputDir: string; bPad: boolean; bDeleteDuplicates: boolean);
var
  soutputFile: string;
  t: integer;
  dir: TDirectory;
  fil: TFileInformation;
  sLeft, sRight: string;
  sNewName: string;
begin

  forcedirectories(sOutputDir);
  sOutputFile := slash(sOutputDir)+'working.%d.'+extract_image_ext;
  dir := TDirectory.create(sOutputDir, '*.'+extract_image_ext, 0,0, true);
  try
    while dir.getnextfile(fil) do begin
      sNewName := fil.Name;
      SplitString(sNewName, '.', sLeft, sRight);
      sNewName := sLeft+'.';
      SplitString(sRight,'.', sLeft, sRight);
      if bPAd then begin
        sNewName := slash(fil.Path)+sNewName+ padstring(sLeft,'0', 15)+'.'+sRight;
      end else begin
        sNewName := slash(fil.Path)+sNewName+ inttostr(strtoint(sLeft))+'.'+sRight;
      end;
      if (sNewName <> fil.fullname) then begin
        if bDeleteDuplicates and FileExists(sNewName) then begin
          DeleteFile(fil.FullName);
        end else begin
          RenameFile_Verify(fil.fullname, sNewName);
        end;
      end;
    end;

  finally
    dir.free;
  end;


end;

function BeginExtractVideoFrames(sInputFile: string; sOutputDir: string; w: nativeint = 0; h: nativeint = 0): Tcmd_FFMPEG;
var
  soutputFile: string;
  t: integer;
  dir: TDirectory;
  fil: TFileInformation;
  sLeft, sRight: string;
  sNewName: string;
  sDir: string;
  cexe: Tcmd_FFMPEG;
  sDim: string;
begin




//  exe.RunProgramAndWait(ffmpeg, '-i "'+sInputFile+'" -y -f image2 -b 100000 -s '+sDim+' "'+sOutputFIle+'"', extractfilepath(ffmpeg), true);

  dir := TDirectory.create(sOutputDir, '*.'+extract_image_ext,0,0,false);
  try
    for t:= 0 to dir.Filecount-1 do begin
      DeleteFile(dir.files[t].fullname);
    end;
  finally
    dir.free;
  end;



  cexe := Tcmd_FFMPEG.Create;
  cexe.BleedingEdge := beStable;
  cexe.WorkingDir := extractfilepath(ffmpeg);
  cexe.OutputDIrectory :=   sOutputDir;
  cexe.InputFile := sInputFile;
  cexe.Action := ffExtractFrames;
  cexe.width := w;
  cexe.height := h;
  cexe.Hide := true;
  cexe.Start;

  result := cexe;

end;

procedure EndExtractVideoFrames(cexe: Tcmd_FFMPEG);
var
  dir: TDirectory;
  t: nativeint;
  fil: TFileInformation;
  sNewName: string;
  sLeft, sRight: string;
begin

  cexe.WaitFor;


  //pad all the frame numbers

  dir := TDirectory.create(cexe.outputdirectory, '*.'+extract_image_ext, 0,0, true);
  try
    while dir.getnextfile(fil) do begin
      sNewName := fil.Name;
      SplitString(sNewName, '.', sLeft, sRight);
      sNewName := sLeft+'.';
      SplitString(sRight,'.', sLeft, sRight);
      sNewName := slash(fil.Path)+sNewName+ padstring(sLeft,'0', 5)+'.'+sRight;
      RenameFile(fil.fullname, sNewName);
    end;

  finally
    dir.free;
  end;

  cexe.free;
  cexe := nil;


end;
procedure ExtractVideoFrames(sInputFile: string; sOutputDir: string; w: nativeint = 0; h: nativeint = 0);
begin
  EndExtractVideoFrames(beginExtractVideoFrames(sInputfile, soutputDir,  w,h));
end;


{ Tcmd_XBOXtranscode }

procedure Tcmd_XBOXtranscode.BuildCommandLine;
begin
  inherited;
  //d:\source\DMX_O_Matic\ffmpeg.exe -i "h:\archives\tv\Workaholics\Episode 1 - Piss & Shit.mkv" -f mp4 -vcodec mpeg4 -b 2000000 -acodec libfaac -ac 2 -ab 128000 i:\tv\test.mp4
  Prog := ffmpeg;
  Params := '-y -i "'+finfile+'" -f mp4 -vcodec mpeg4 -b 2000000 -acodec libfaac -ac 2 -ab 128000 "'+foutfile+'"';
  Hide := true;

  //GLOG.LogToDisk(Prog+' '+Params);
end;
procedure Tcmd_XBOXtranscode.Init;
begin
  inherited;
  Icon := @CMD_ICON_REEL;
end;

procedure Tcmd_XBOXtranscode.InitExpense;
begin
  inherited;
  CPUExpense := 1.0;
end;

function BeginTranscodeForXBOX(sFile: string; sOutFile:string): Tcmd_XBOXtranscode;
begin
  result := Tcmd_XBOXtranscode.Create;
  result.infile := sfile;
  result.outfile := sOutfile;
  result.start;

end;
procedure EndTranscodeForXbox(c: Tcmd_XBOXtranscode);
begin
  c.WaitFor;
  c.Free;
  c := nil;
end;



{ Tcmd_FFMPEG }


{ Tcmd_FFMPEG }

procedure Tcmd_FFMPEG.AfterConstruction;
begin
  inherited;
  outputFile := '';//sets resource usage
end;

procedure Tcmd_FFMPEG.BuildCommandLine;
var
  sDim: string;
  br: string;
  p: TFFProbe;
  sThreads: string;
begin
  inherited;
  Name := extractFileName(outputfile);
  ConsoleRedirect := true;
  Hide := true;

  if lowercase(extractfileext(InputFile)) = '.webm' then
    BleedingEdge := beSTable;

  case bleedingedge of
    beAncient: prog := ffmpeg;
    beStable: prog := ffmpeg_ex();
    be4kEra: prog := ffmpeg_ex('_4k');
  end;

  p.init;
  if action <> ffProbe then
    p := ffMpegProbe(InputFile);

  if (Width > 0) and (Height > 0) then
    sDim := ' -s '+inttostr(Width)+'x'+inttostr(height);



  sThreads := ' ';//' -threads '+ffmpeg_cpus.tostring+' ';

  case Action of
    ffProbe: begin
      Params := '-i "'+InputFile+'"';
      CPuExpense := 0.0;
    end;
    ffExtractFrame: begin
      if BleedingEdge <> beAncient then begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -ss 00:01:00.000 -y -r 30 -f image2 -q:a 0 -q:v 0 -b 100000'+sDim+' -vframes 1 "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -y -f image2 -sameq '+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffExtractFrames: begin
      OutputFile := slash(OutputDirectory)+'working.%d.'+extract_image_ext;
      if BleedingEdge <> beAncient then begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -y -r 30 -f image2 -q:a 0 -q:v 0 -b 100000'+sDim+' "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -y -f image2 -sameq '+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffToDMXVideo: begin
//      OutputFile := slash(OutputDirectory)+'working.%d.'+extract_image_ext;
      if BleedingEdge <> beAncient then begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -q:a 0 -q:v 0 -r 30 '+sDim+' "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffToChromeCastOQ: begin
      //D:\source\Pascal\64\MovieMaker\..\..\tools\ffmpeg_4k.exe -re
      //-i "\\192.168.33.3\d$\media\_movies\Red Sparrow\Red.Sparrow.2018.2160p.BluRay.x264.8bit.SDR.DTS-HD.MA.TrueHD.7.1.Atmos-SWTYBLZ.mkv"
      //-c:v libx265 -preset veryslow -vf "scale=3840:2160" -b:v 12M -sc_threshold 0
      //"\\192.168.33.3\d$\media\_movies\Red Sparrow\oq\oq.Red.Sparrow.2018.2160p.BluRay.x264.8bit.SDR.DTS-HD.MA.TrueHD.7.1.Atmos-SWTYBLZ_2.mkv.mp4"
      if (p.w > 1920) then begin
        if outputfile <> '' then
          forcedirectories(extractfilepath(outputfile));

        if FAlternate then changefileext(OutputFile, '.webm');
        if BleedingEdge <> beAncient then begin
          Params := '-y'+sThreads+'-re -i "'+InputFile+'" -c:v libx265 -tune fastdecode -vf "scale=3840:2160" -b:v 10M -sc_threshold 0 "'+OutputFIle+'"';
        end else begin
  //        Params := '-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
        end;
      end;
    end;
    ffToChromeCastHQ: begin
      if outputfile <> '' then
        forcedirectories(extractfilepath(outputfile));

      if (sDim = '') and (p.w > 1920) then begin
        sDim := '-s 1920x'+inttostr(round(1920*(p.h/p.w)));
      end;
      if FAlternate then changefileext(OutputFile, '.webm');
      if BleedingEdge <> beAncient then begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -q:a 0 -q:v 0 -acodec mp3 '+sDim+' "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffToOculus: begin
      if FAlternate then changefileext(OutputFile, '.webm');
      if BleedingEdge <> beAncient then begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -q:a 0 -q:v 0 -s 3840x1080 -acodec mp3 '+sDim+' "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffToChromeCastLQ: begin
      if outputfile <> '' then
        forcedirectories(extractfilepath(outputfile));

      if (sDim = '') and (p.w > 1920) then begin
        sDim := '-s 1920x'+inttostr(round(1920*(p.h/p.w)));
      end;
      if FAlternate then changefileext(OutputFile, '.webm');
      if BleedingEdge <> beAncient then begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -c:v libx264 -crf 20 -maxrate 300k -acodec mp3 -bufsize 1834k '+sDim+' "'+OutputFIle+'"';
      end else begin
        Params := '-y'+sThreads+'-i "'+InputFile+'" -sameq -b 100000'+sDim+' "'+OutputFIle+'"';
      end;
    end;
    ffToMp3: begin
      if lowercase(extractfileext(INputFile)) = '.flv' then begin
        br := '128k -ar 44100'
      end else begin
        br := '256k'
      end;

      //if bUseAsyncFlag then
      Params := '-y'+sThreads+'-vsync 1 -async 1 -ab '+br+' -i "' + INputFile + '" "' + OutputFile + '"'
      //else
      //  result.Params := '-y -vsync 1 -ab '+br+' -i "' + sFile + '" "' + sOutFile + '"';

    end;
  end;

end;




procedure Tcmd_FFMPEG.CC(ccStatus: TConsoleCaptureStatus; sData: string);
var
  sLeft: string;
  sRight: string;
  timeh: IHolder<TStringList>;

  f: single;
  t: ni;
  function DurationToMS(s: string): int64;
  begin
    result := 0;
        timeh := ParseStringH(sLeft, ':');
        if timeh.o.count = 3 then begin
          f := strtofloat(timeh.o[2]);
          result := round(f*1000);
          result := result + (strtoint(timeh.o[1])*60*1000);
          result := result + (strtoint(timeh.o[0])*60*60*1000);
        end;
  end;
begin
  inherited CC(ccStatus, sData);
  if (FTotalFrames = 0) and (FDurationInMS=0) then begin
    if SplitString(sData, 'NUMBER_OF_FRAMES:', sLeft, sRight) then begin
      If SplitString(sright, #13, sLeft, sright) then begin
        sLEft := trim(sLeft);
        if IsInteger(sLeft) then
          FTotalFrames := strtoint(sLeft);
          StepCount := FtotalFrames;
      end;
    end;
    if SplitString(sData, 'Duration: ', sLeft, sRight) then begin
      If SplitString(sright, ',', sLeft, sright) then begin
        sLEft := trim(sLeft);

        try
          FDurationInMS :=  DurationToMS(sLeft);
          StepCount := FDurationInMS;
        except
        end;

      end;
    end;
  end;

  if FTotalFrames > 0 then begin
    if SplitString(sData, 'frame=', sLeft, sRight) then begin
      If SplitString(sright, ' fps', sLeft, sright) then begin
        sLeft := trim(sLeft);
        if IsInteger(sLeft) then
          Step := strtoint(trim(sLeft));

      end;
    end;
  end;

  if FdurationInMS > 0 then begin
    if SplitString(sData, 'time=', sLeft, sRight) then begin
      If SplitString(sright, ' bitrate', sLeft, sright) then begin
        sLeft := trim(sLeft);
        try
          Step := DurationToMS(sLeft);
        except
        end;

      end;
    end;
  end;



end;

constructor Tcmd_FFMPEG.Create;
begin
  inherited;

end;

procedure Tcmd_FFMPEG.DoExecute;
begin
  inherited;
//  if dir.GetFileSize(OutputFile) = 0 then begin
//    deletefile(outputfile);
//    FAlternate := true;
//    inherited;
//  end;


end;

procedure Tcmd_FFMPEG.Init;
begin
  inherited;

end;

procedure Tcmd_FFMPEG.InitExpense;
begin
  inherited;
  Timeout := (60000*60)*6;//6-hour timeout
  Icon := @CMD_ICON_REEL;

end;


procedure Tcmd_FFMPEG.PostProcess;
var
  dir: TDirectory;
  t: nativeint;
  fil: TFileInformation;
  sNewName: string;
  sLeft, sRight: string;
begin

   Status := 'PostProcesss';
//  dir := TDirectory.create(OutputDirectory, '*.'+extract_image_ext,0,0,false);
//  try
//    for t:= 0 to dir.Filecount-1 do begin
//      DeleteFile(dir.files[t].fullname);
//    end;
//  finally
//    dir.free;
//  end;

  //pad all the frame numbers

  dir := TDirectory.create(OutputDirectory, '*.'+extract_image_ext, 0,0, true);
  try
    while dir.getnextfile(fil) do begin
      sNewName := fil.Name;
      SplitString(sNewName, '.', sLeft, sRight);
      sNewName := sLeft+'.';
      SplitString(sRight,'.', sLeft, sRight);
      sNewName := slash(fil.Path)+sNewName+ padstring(sLeft,'0', 5)+'.'+sRight;
      RenameFile_Verify(fil.fullname, sNewName);
    end;

  finally
    dir.free;
  end;

  inherited;
end;

procedure Tcmd_FFMPEG.PreProcess;
begin
  inherited;
  Status := 'PreProcess';

  case ACtion of
    //when extracting frames, delete old files first
    ffExtractFrames: begin


      DeleteFolder(OutputDirectory);


      while not directoryexists(OutputDirectory) DO begin
        Status := 'not DirectoryExists '+OutputDirectory;
        deletefile(unslash(OutputDirectory));
        if pos(':', OutputDirectory) > 0 then begin
          forcedirectories(OutputDirectory);
        end else begin
          forcedirectories(dllpath+OutputDirectory);
        end;

        //mkdir(sDir);
      end;
    end;
  end;

  if fileexists(OutputFile) then begin
    if not OverwriteOutput then
      Cancel;
  end;

end;

procedure Tcmd_FFMPEG.SEtOutputFile(const Value: string);
begin
  Resources.SetResourceUsage('ffmpeg '+Foutfile, 0.0);
  Foutfile := Value;
  Resources.SetResourceUsage('ffmpeg '+Foutfile, 1.0);
end;

{ Tcmd_Mobiletranscode }

procedure Tcmd_Mobiletranscode.BuildCommandLine;
begin
  inherited;
  //d:\source\DMX_O_Matic\ffmpeg.exe -i "h:\archives\tv\Workaholics\Episode 1 - Piss & Shit.mkv" -f mp4 -vcodec mpeg4 -b 2000000 -acodec libfaac -ac 2 -ab 128000 i:\tv\test.mp4
  Prog := ffmpeg;
//  Params := '-y -i "'+finfile+'" -f mp4 -vcodec mpeg2 -s 850x480 -b 2000000 -acodec libfaac -ac 2 -ab 128000 "'+foutfile+'"';
  Params := '-y -i "'+finfile+'" -s 850x480 -ac 2 "'+foutfile+'"';
  Hide := true;

  Debug.Log(Prog+' '+Params);

end;

procedure Tcmd_Mobiletranscode.InitExpense;
begin
  inherited;
  CPUExpense := 1.0;
  Icon := @CMD_ICON_REEL;
end;

{ TJVIDHeader }

procedure TJVIDHeader.CalculateChecksum;
begin
  CheckSum := FrameSize xor PrevFrameSize xor FrameNumber;
end;

function TJVIDHeader.VerifyChecksum: boolean;
begin
  result := Checksum = (FrameSize xor PrevFrameSize xor FrameNumber);
end;

{ TJVIdReader }

procedure TJVIdReader.Cleanup;
begin
  if framememory <> nil then begin
    FreeMemory(framememory);
    FrameMemory := nil;
  end;

end;

procedure TJVIdReader.CleanupMain;
begin
  if index <> nil then
    FreeMemory(index);
  index := nil;

end;

procedure TJVIdReader.COmpleteCommand;
begin
  while not PrefetchAckFlag = 1 do
    sleep(0);

//  if assigned(cmdNextFRame) then begin
//    cmdNExtFrame.waitfor;
//    cmdNExtFrame.free;
//    cmdNExtFrame := nil
//  end;

end;

destructor TJVIdReader.Destroy;
begin
  StopPrefetchThread;
  COmpleteCommand;
  Cleanup;

  stream.Free;
  stream := nil;

  inherited;
end;

function TJVIdReader.FileName: string;
begin
  if stream = nil then
    result := ''
  else
    result := stream.FileName;
end;

function TJVIdReader.FramePOinterAt: nativeint;
begin
  result := FrameNumber + 1;
end;

function TJVIdReader.GetStreamFromFrameMemory: TExternalMemoryStream;
begin
  result := TExternalMemoryStream.Create;
  result.SetExternalPointer(framememory, framememorylength);
//  stream_guaranteewrite(result, framememory, framememorylength);
//  result.Seek(0,soBeginning);



end;

procedure TJVIdReader.GotoFrame(iNumber: nativeint);
var
  tm,tm2: cardinal;
begin
  try
    if iNumber = framenumber then
      exit;

    if iNumber >= nativeint(self.mainheader.numberofframes) then
      exit;


    if stream = nil then
      exit;

    if iNumber >= nativeint(mainheader.numberofframes) then
      exit;
    if iNumber < 0 then
      exit;


    tm := GetTicker;
    try
      stream.seek(index[iNumber], soBeginning);
      ReadCurrentFrame;
    finally
      tm2 := GEtTimeSince(tm);
      if tm2 > (1000/30) then begin
        Debug.log('Frame grab time: '+inttostr(tm2));
      end;
    end;
  finally
    //stream.BeginFlushAndPrefetch(random(5000000));
  end;

//  if iNumber < FrameNumber then begin
//    PrevFrame(FrameNumber-iNumber);
//  end else begin
//    NExtFrame(iNumber-FrameNumber);
//  end;

end;

procedure TJVIdReader.GotoTime(rTime: nativefloat);
var
  i: nativeint;
begin
  i := round(rTime * mainheader.framerate);
  GotoFrame(i);


end;

procedure TJVIdReader.Init;
begin
  inherited;
  StartPrefetchThread;
end;

procedure TJVIdReader.LoadFromFile(sFile: string);
var
  iPos: int64;
begin
  CleanupMain;
  Cleanup;

  FrameNumber := -1;
  if not fileexists(sFile) then
    exit;

  stream := TJVIDFileStream.Create(sfile, fmOpenRead+fmShareDenyWrite);
//  stream.buffersize := sizeof(header);
{$IFDEF USE_MFS}
  stream.buffersize := 100000000;
  stream.OptimizeForBinarySearch;
{$ENDIF}
  stream.Seek(0,0);
  Stream_GuaranteeRead(stream, @mainheader, sizeof(mainheader));
  index := GetMemory(mainheader.numberofframes* sizeof(int64));
  stream.Seek(mainheader.indexoffset, soBeginning);
  Stream_GuaranteeRead(stream, @index[0], sizeof(int64)*mainheader.numberofframes);
  OnNewVideo;
  GotoFrame(0);

end;

procedure TJVIdReader.NextFrame(iCount: integer = 1);
begin
  if FramePOinterAt >= nativeint(mainheader.numberofframes) then
    exit;

{$IFDEF USE_MFS}
  if iCount > 300 then
    stream.buffersize := sizeof(header)
  else
    stream.BufferSize := 1000000;
{$ENDIF}

  while iCount > 1 do begin
    ReadCurrentFrameHeader;
    stream.Seek(header.FrameSize, soCurrent);
    inc(framenumber);
    dec(iCount);
  end;

  ReadCurrentFrame;
  inc(framenumber);
  //ReadCurrentFrame;

end;

procedure TJVIdReader.OnNewVideo;
begin

//  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TJVIdReader.PrefetchNextFrame;
begin
  PrefetchAckFlag := 0;
//  PrefetchRequestFlag := 1;
//  cmdNextFRame := Tcmd_NextFrame.Create;
//  cmdNextFrame.Reader := self;
//  cmdNextFrame.Start;
end;

procedure TJVIdReader.PrefetchThread_OnExecute(sender: TExternalEventThread);
begin
  if PrefetchRequestFlag = 1 then begin
    ///sender.SetMM('Pro Audio');
    NExtFrame;
    PrefetchRequestFlag := 0;
    PrefetchAckFlag := 1;
  end;

end;

procedure TJVIdReader.PrevFrame(iCount: integer = 1);
var
  i: int64;
begin
  //first lets rewind to the beginning of the current frame
  i := (header.FrameSize+sizeof(header));
  i := 0-i;
  stream.Seek(i, soCurrent);

  while iCount > 0 do begin
    i := (header.PrevFrameSize+sizeof(header));
    i := 0-i;
    stream.Seek(i, soCurrent);
    ReadCurrentFrameHeader;
    dec(framenumber);
    dec(iCount);

    if iCount > 0 then
      stream.Seek(0-sizeof(header), soCurrent);



  end;

  ReadCurrentFrameBuffer;
end;

procedure TJVIdReader.ReadCurrentFrame;
begin
  ReadCurrentFrameHeader;
  ReadCurrentFrameBuffer;
end;

procedure TJVIdReader.ReadCurrentFrameBuffer;
begin
  Cleanup;
  framememory := GetMemory(header.FrameSize);
  framememorylength := header.FrameSize;
  Stream_GuaranteeRead(stream, @framememory[0], header.FrameSize);
end;

procedure TJVIdReader.ReadCurrentFrameHeader;
begin
  Stream_GuaranteeREad(stream, @header, sizeof(header));
end;

procedure TJVIdReader.StartPrefetchThread;
begin
  thrPrefetch := TPM.NeedThread<TExternalEventThread>(nil);
  //thrPrefetch.Loop := true;
  thrPRefetch.OnExecute := self.PrefetchThread_OnExecute;

  thrPrefetch.Start;
  thrPrefetch.safeResume;
end;

procedure TJVIdReader.StopPrefetchThread;
begin
  thrPrefetch.WaitForFinish;
  TPM.NoNeedThread(thrPrefetch);
  thrPrefetch := nil;

end;

function TJVIdReader.TimeToFrameNumber(rTime: nativefloat): nativeint;
begin
  result := round(rTime * mainheader.framerate);
end;

{ TJVIDMainHeader }

procedure TJVIDMainHeader.Init;
begin
  fourcc[0] := 'J';
  fourcc[1] := 'V';
  fourcc[2] := 'I';
  fourcc[3] := 'D';
  version := 1;
end;

procedure VideoToJVid(sInputFile: string; sOutputFile: string; cmd: TCommand = nil; fmt: TJVIDFormat = jvfJPG);
var
  sFile, sIn, sTemp2, sTemp, sOut, smp3, sboog: string;
  mainhead: TJVIDMainHeader;
  bh: TBoogerHeader;
  ss: TSoundStream;
  iLen: nativefloat;
  c: Tcmd_RunExe;
begin
  iLen := 0;
  if assigned(cmd) then cmd.Stepcount := 5;
  sFile := sInputFile;
  sIn := sInputFile;
  sTemp2 := GetTempPath+'videoproc\'+inttostr(GetCurrentThreadID())+'\';
  forcedirectories(sTemp2);
  copyfile(pchar(sIn), pchar(sTemp2+extractfilename(sIn)), false);
  sIn := pchar(sTemp2+extractfilename(sIn));
  sTemp := GetTempPath+'videoproc\'+inttostr(GetCurrentThreadID())+'\frames\';
  forcedirectories(sTemp);
  try
    sOut := sOutputFile;
    //1---------------------------------------------------------
    if assigned(cmd) then cmd.Step := 1;
    sMp3 := sIn+'.mp3';
    c := BeginVideoToMp3(sIn, false, sMp3);
    ffmpeg_tools.ExtractVideoFrames(sIn, sTemp);
  //  ffmpeg_tools.ConformVideoFiles(sTemp, true, true);
    //2---------------------------------------------------------
    if assigned(cmd) then cmd.Step := 2;
    mainhead.Init;

    EndVideoToMp3(c);
    //3---------------------------------------------------------
    if assigned(cmd) then cmd.Step := 3;
    ss := TSoundStream.Create(sMp3, fmOpenRead+fmShareDenyWrite);
    try
      if ss.samplerate = 0 then
        raise Exception.Create('could not determine sample rate of mp3:'+sMp3);
      iLen := ss.SampleCount / ss.samplerate;
    finally
      ss.Free;
    end;


    case fmt of
      jvfJPG: ffmpeg_tools.CReateJVidFromFrames_JPG(iLen, sTemp, sOut, '*.jpg', cmd);
      jvfCRN: ffmpeg_tools.CReateJVidFromFrames_Crunch(iLen, sTemp, sOut, '*.jpg', fmt, 128, cmd);
      jvfDDS: ffmpeg_tools.CReateJVidFromFrames_Crunch(iLen, sTemp, sOut, '*.jpg', fmt, 255, cmd);
    end;

    //5---------------------------------------------------------
    if assigned(cmd) then cmd.Step := 5;

    copyfile(pchar(sMp3), pchar(changefileext(sOut, '.mp3')), false);
  finally
    DeleteFolder(sTemp2);
  end;
end;


procedure VideoToDmxVideo(sInputFile: string; sOutputFile: string; cmd: TCommand = nil);
var
  sFile, sIn, sTemp2, sTurb, sTemp, sHue, sOut, smp3, sboog: string;
  sFinalhue, sFinalturb, sfinalmp3, sFinalmp4: string;
  sLckFile: string;
  bNeedsTurb, bNeedsHue, bNeedsMp3, bNeedsmp4, bNeedsFrames: boolean;
  mainhead: TJVIDMainHeader;
  bh: TBoogerHeader;
  ss: TSoundStream;
  iLen: nativefloat;
  c: Tcmd_RunExe;
  c2: TCmd_FFmpeg;
  c3: Tcmd_RenderHuestogram;
  turb: TCmd_GEnerateTurbulenceData;
  fsLck: TFileStream;
  dtLck: TDateTime;
begin
  c3 := nil;
  c2 := nil;
  fsLck := nil;
  try
    try
      sFinalHue := sOutputFile+'.hue';
      sFinalTurb := sOutputFile+'.turb';
      sFinalMp3 := sOutputFile+'.mp3';
      sFinalMp4 := sOutputFile+'.mp4';
      sLckFile := sOutputFile+'.lck';
      bNeedsMp3 := not fileexists(sFinalMp3);
      bNeedsMp4 := not fileexists(sFinalMp4);
      bNeedsTurb := not fileexists(sFinalTurb);
      bNeedsHue := not fileexists(sFinalHue);
      if bNeedsHue then bNeedsMp3 := true;
      bNeedsFRames := bNeedsHue;

      if not (bNeedsMp3 or bNeedsMp4 or bNeedsHue or bNeedsTurb or bNeedsFrames)
      then
        exit;

      //get lock
      try

        if fileexists(sLckFile) then begin
          dtLck := FileAge(sLckFile);
          deletefile(sLckFile);

          if FileExists(sFinalHue) and (FileAge(sFinalHue) >= dtLck) then deletefile(sFinalHue);
          if FileExists(sFinalTurb) and (FileAge(sFinalTurb) >= dtLck) then deletefile(sFinalTurb);
          if FileExists(sFinalMp3) and (FileAge(sFinalMp3) >= dtLck) then deletefile(sFinalMp3);
          if FileExists(sFinalMp4) and (FileAge(sFinalMp4) >= dtLck) then deletefile(sFinalMp4);


        end;


        if fileexists(sLckFile) then
          exit;
        fsLck := TFileStream.create(sLckFile, fmCReate);
      except
        fsLck.free;
        exit;
      end;







      if assigned(cmd) then cmd.Stepcount := 5;
      sFile := sInputFile;
      sIn := sInputFile;

      sTemp2 := GetTempPath+'videoproc\'+inttostr(GetCurrentThreadID())+'\';
      forcedirectories(sTemp2);
      copyfile(pchar(sIn), pchar(sTemp2+extractfilename(sIn)), false);
      if fileexists(sFinalMp3) then
        copyfile(pchar(sFinalMp3), pchar(sMp3), false);

      sIn := pchar(sTemp2+extractfilename(sIn));
      sMp3 := sIn+'.mp3';

      if bNeedsFRames then begin
        sTemp := GetTempPath+'videoproc\'+inttostr(GetCurrentThreadID())+'\frames\';
        forcedirectories(sTemp);
      end;
      try
        sOut := sOutputFile;

        //1---------------------------------------------------------
        if assigned(cmd) then cmd.Step := 1;


        if bNeedsMp4 then begin
          //start command to convert video to mp4
          sMp3 := sIn+'.mp3';
          sTurb := sIn+'.turb';
          c2 := Tcmd_FFMPEG.Create;
          c2.FAction := TFFMPEG_Action.ffToDMXVideo;
          c2.InputFile := sIn;
          c2.outputFile := sOutputFile+'.mp4';
          c2.Start;
        end;
        try


          //ffmpeg_tools.ExtractVideoFrames(sIn, sTemp);
        //  ffmpeg_tools.ConformVideoFiles(sTemp, true, true);
          //2---------------------------------------------------------
          if assigned(cmd) then cmd.Step := 2;



          if bNeedsMp3 then begin
            //! Mp3 must be made before huestogram can be built
            c := BeginVideoToMp3(sIn, false, sMp3);
            //Status := 'Waiting for mp3 to finish extraction.';
            EndVideoToMp3(c);
          end;

          if bNeedsMp3 then
            copyfile(pchar(sMp3), pchar(sFinalMp3), false);


          turb := nil;
          if bNeedsTurb then begin
            turb := Tcmd_GenerateTurbulenceData.Create;
            turb.FileName := sFinalMp3;
            turb.Rebuild := true;
            turb.Start;
          end;
          try

            //LockConsole;
            if bNeedsHue then begin
              c3 := servervideoparser.Tcmd_RenderHuestogram.create;
              c3.TempDir := sTEmp;
              c3.FileName := sIn;
              c3.OutputFileName := sOut;
            end;
            try
              if bNeedsHue then begin
                if assigned(c3) then
                  c3.Start;
              end;
              try
              finally
                if bNeedsHue Then begin
                  if assigned(c3) then
                    c3.waitfor;
                end;
              end;
            finally
              if bNeedsHue then begin
                if assigned(c3) then begin
                  c3.Free;
                  c3 := nil;
                end;
              end;
            end;
          finally
            if bNeedsTurb then begin
              turb.WaitFor;
              turb.Free;
              turb := nil;
            end;
          end;
          //UnlockConsole;
        finally
          if bNeedsMp4 then begin
            if assigned(c2) then begin
              c2.WaitFor;
              c2.Free;
              c2 := nil;
            end;
          end;
        end;


        //5---------------------------------------------------------
        if assigned(cmd) then cmd.Step := 5;

        if bNeedsTurb then
          copyfile(pchar(sTurb), pchar(sOutputFile+'.turb'), false);
      finally
        DeleteFolder(sTemp2);
      end;
    except
      on e:exception do begin
        Debug.Log(e.classname+' threw exception: '+e.classname+' '+e.message)
      end;
    end;
  finally
    fsLck.free;
    fslck := nil;

     if fileexists(sLckFile) then
      deletefile(sLckFile);


  end;


end;


{ Tcmd_VideoToJVID }

procedure Tcmd_VideoToJVID.DoExecute;
begin
  inherited;
  VideoToJVid(InputFile, OutputFile, self);

end;

function Tcmd_VideoToJVID.GetOutputAudioFile: string;
begin
  result := changefileext(OutputFile, '.mp3');
end;

class function Tcmd_VideoToJVID.Go(sINput, sOutput: string): Tcmd_VideoToJVID;
begin
  result := Tcmd_VideoToJVID.Create;

  result.InputFile := sInput;
  result.OutputFile := sOutput;
  result.Start;
end;

procedure Tcmd_VideoToJVID.Init;
begin
  inherited;
  Icon := @CMD_ICON_REEL;
end;

procedure Tcmd_VideoToJVID.InitExpense;
begin
  inherited;
  CPUExpense := 0;
  resources.SetResourceUsage('ffmpeg', lesserof(1.0,2/(GetnumberofProcessors)));
end;

procedure TJVIDBS.Init;
begin
  inherited;
  HighSize := 80000;
  LowSize := 3000;
end;

procedure TJVIDBS.Search(testfirst: nativeint);
begin
  BSFunc(testfirst);
  if (ms.size >= HighSize) or (ms.size <= LowSize) then begin
    raise ECritical.create('deprecated');
//    BinarySearch(BSFunc);
  end;
end;

{ Tcmd_NextFrame }

procedure Tcmd_NextFrame.DoExecute;
begin
  inherited;
  if Reader.PrefetchRequestFlag = 1 then begin
    reader.NextFrame;
    Reader.PrefetchRequestFlag := 0;
    Reader.PrefetchAckFlag := 1;
  end;


end;

{ Tcmd_VideoToDmxVideo }

procedure Tcmd_VideoToDmxVideo.DoExecute;
begin
  inherited;
  //LockConsole;
  self.FRaiseExceptions := false;
  Status := ExtractFileName(InputFile);
  VideoToDmxVideo(InputFile, OutputFile, self);


  //UnlockConsole;
end;

function Tcmd_VideoToDmxVideo.GetOutputAudioFile: string;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

class function Tcmd_VideoToDmxVideo.Go(sINput,
  sOutput: string): Tcmd_VideoToDmxVideo;
begin
  result := Tcmd_VideoToDmxVideo.Create;

  result.InputFile := sInput;
  result.OutputFile := sOutput;
  result.Start;
end;

procedure Tcmd_VideoToDmxVideo.Init;
begin
  inherited;
  Icon := @CMD_ICON_REEL;
end;

procedure Tcmd_VideoToDmxVideo.InitExpense;
begin
  inherited;
  CPUExpense := 0;
  resources.SetResourceUsage('ffmpeg', lesserof(1.0,1/(lesserof(16,GetnumberofProcessors))));
end;


function ffMpegProbe(sFile: string): TFFProbe;
var
  s, sl, sr: string;
  h: IHolder<TStringList>;
  t: ni;
begin
  result.init;
  s := ffMpegProbeStr(sFile);
  if SplitString(s, 'Stream #0:0: Video: ', sl, sr) then begin
    sr := stringreplace(sr, CRLF, LF, [rfReplaceAll]);
    h := ParseStringh(sr, LF);
    if h.o.Count > 0 then begin
      h := ParseStringh(h.o[0], ',');
      for t:= 0 to h.o.count-1 do begin
        s := h.o[t];
        s := Trim(s);

        if SplitString(s, 'x', sl,sr) then begin
          if IsInteger(sl) and IsInteger(sr) then begin
            result.w := strtofloat(sl);
            result.h := strtofloat(sr);
          end;
        end;
      end;

    end;
  end;

end;

function ffMpegProbeStr(sFile: string; bSynchronous: boolean = true): string;
var
  c: Tcmd_FFMPEG;
begin
  c := Tcmd_FFMPEG.Create;
  try
    c.bleedingedge := be4kEra;
    c.InputFile := sFile;
    c.ACtion := ffProbe;
    c.CaptureConsoleoutput := true;
    c.CPUExpense := 0;
    c.resources.SetResourceUsage('ffmpeg_probe', 1.0);
    if bSynchronous then
      c.Execute
    else begin
      c.Start;
      c.WaitFor;
    end;
    result := c.Consoleoutput;
  finally
    c.Free;
  end;

end;

{ TFFProbe }

procedure TFFProbe.Init;
begin
  w := 0;
  h := 0;

end;

end.
