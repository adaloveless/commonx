unit FormAppUpdater;

//what we need here....
//a params file should be setup to determine where and what we're looking for
//the version file should be the same as the params file but with a different extension
//this allows for updates to an updater app, initiated from the main or a tertiary app

//zx_BLE_Programmer checks AppUpdater.updateparams and AppUpdater.ver
//if out of date then
//Run Update scripts from back-end
//Launch AppUpdater.exe ZX_Ble_Programmer.updateparams


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, commandprocessor, fileserviceclientex, ApplicationParams, stringx, typex, systemx,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.WinXCtrls, colorblending, tickcount, exe, namevaluepair;

type
  TWorker = class(TCommand)
  private
    FCheckOnly: boolean;
    function GetUpdateAvailable: boolean;
  protected
    localver, remoteVer: ni;
    workingver: ni;
    host, endpoint, prog, run_when_done, run_when_done_params: string;
    beta: boolean;
    verfile: string;
    paramsfile: string;
    procedure ExecuteUpdateScriptLine(sLIne: string);
  public
    cli: TFileServiceClientEx;
    procedure Init; override;
    property UpdateAVailable: boolean read GetUpdateAvailable;
    property CheckOnly: boolean read FCheckOnly write FCheckOnly;
    procedure DoExecute; override;
  end;

  TfrmUpdateProgress = class(TForm)
    Timer1: TTimer;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    ActivityIndicator1: TActivityIndicator;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    c: TWorker;
    procedure CheckForUpdatesAndUpdate(paramsfile: string);
    function CheckForUpdates(paramsfile: string): boolean;
  end;

procedure CheckForUpdatesAndUpdate(paramsfile: string);
function CheckForUpdates(paramsfile: string; bShow: boolean): boolean;

procedure UpdateAndLaunchUpdaterApp(paramsfile: string);

var
  frmUpdateProgress: TfrmUpdateProgress;

implementation

{$R *.dfm}

uses debug;

{ TWorker }
procedure UpdateAndLaunchUpdaterApp(paramsfile: string);
begin
  checkForUpdatesAndUpdate(paramsfile);
end;


procedure CheckForUpdatesAndUpdate(paramsfile: string);
begin
  if frmUpdateProgress = nil then begin
    application.CreateForm(TfrmUpdateProgress, frmUpdateProgress);
  end;

  frmUpdateProgress.CheckForUpdatesAndUpdate(paramsfile);
  frmUpdateProgress.showModal;

end;

function CheckForUpdates(paramsfile: string; bShow: boolean): boolean;
begin
  if frmUpdateProgress = nil then begin
    application.CreateForm(TfrmUpdateProgress, frmUpdateProgress);
  end;

  result := frmUpdateProgress.CheckForUpdates(paramsfile);

end;



procedure TWorker.DoExecute;
var
  sl: TStringlist;
  t: ni;
  nvpl: TNameValuePairList;
begin
  inherited;
  if IsRelativePath(paramsfile) then
    paramsfile := dllpath+paramsfile;

  Status := '检查更新 Checking for updates...';

  Debug.Log(self, 'Begin Update Stuff');

  nvpl :=  TNameValuePairLIst.create;
  try
    nvpl.loadFRomFile(paramsfile);
    Debug.Log('Params File '+paramsfile+#13#10+nvpl.ToString);
    host := nvpl.GetItemEx('update_host', '127.0.0.1');
    endpoint := nvpl.GetItemEx('update_endpoint', '420');
    beta := nvpl.GetItemEx('update_beta', false);
    prog := nvpl.getitemex('update_prog', 'something');
    run_when_done := dllpath+nvpl.getitemex('update_run', prog);
    run_when_done_params := dllpath+nvpl.getitemex('update_run_params', '');

  finally
    nvpl.free;
  end;

  verfile := changefileext(paramsfile,'.ver');
  Debug.Log(self, 'verfile='+verfile);


  //connect to remote server
  cli := nil;
  try
    Debug.Log(self, 'About to create TFileServiceClientEx for '+host+' , '+endpoint);
    cli := TFileServiceClientEx.create(host, endpoint);

    //check local version file
    if fileexists(verfile) then begin
      try
        localver := strtoint(trim(LoadFileAsString(verfile)));
      except
        Debug.Log(self, 'failed to get localver');
        localver := 0;
      end;
    end;

    Debug.Log(self, 'LocalVer='+inttostr(localver));

    //get remote version
    remotever := cli.GetUpgradeVersion(prog, beta);
    Debug.Log(self, 'RemoteVer='+inttostr(REmotever));

    if CheckOnly then exit;

    //if upgrade available
    if localver < remotever then begin
      Status := '更新文件 Updating files...';
      //get update script
      sl := TStringlist.create;
      try
        Debug.Log(self, 'Getting update script');
        sl.text := cli.GetUpgradeScript(prog, localver, remotever);
        Debug.Log(self, 'Update Script is:'+NEWLINE+sl.text);

        //for each line of script
        workingver := localver+1;
        StepCount := sl.Count;
        for t:= 0 to sl.Count-1 do begin
          //execute script line
          Step := t;
          Debug.Log(self, sl[t]);
          ExecuteUpdateScriptLine(sl[t]);
          Debug.Log(self, 'Ok');
          localver := workingver;
        end;

        //save localversion
        localver := remotever;
        Debug.Log('saving local version '+inttostr(localver)+' to '+verfile);
        SaveStringAsFile(verfile, INTTOSTR(localver));
        Debug.Log(self, 'Ok');



      finally
        sl.free;
      end;
    end;

  finally
    Debug.Log(self, 'Freeing Client');
    cli.free;
    Debug.Log(self, 'Client Freed');
    if not CheckOnly then begin
      Debug.Log(self, 'Run Target Program '+run_when_done+' '+run_when_done_params);
      exe.RunProgram(run_when_done, run_when_done_params, extractfilepath(run_when_done), false, false, false);
    end;
  end;

  Debug.Log(self, 'End of Worker');
end;

procedure TWorker.ExecuteUpdateScriptLine(sLIne: string);
var
  sl,sr: string;
  upgradePath: string;
begin


  splitstring(sLine, ' ', sl, sr);

  if zcopy(sl, 0,3) = '>>>' then begin
    SaveStringAsFile(verfile, INTTOSTR(localver));
    splitstring(sl, '>>>', sl,sr);
    splitstring(sr,'<<<',sl,sr);
    workingver := strtoint(sl);
    localver := workingver;
  end else begin
    if comparetext(sl,'get')=0 then begin
      upgradePath := cli.GetUpgradePath(prog);
      if zpos(lowercase(prog), lowercase(upgradepath)) < 0 then
        upgradePath := upgradePath + prog+'\';//bug fix for some server version
      cli.GetFileEx(upgradepath+inttostr(workingver)+'\'+sr, dllpath+sr, @subprogress);
    end else
    if comparetext(sl,'del')=0 then begin
      if fileexists(dllpath+sr) then
        deletefile(dllpath+sr);
      if fileexists(dllpath+sr) then
        raise ECRitical.create('Failed to delete file: '+dllpath+sr);
    end else
    if comparetext(sl,'run')=0 then begin
      exe.RunProgram(dllpath+sr, '', dllpath, false, false, false);
    end else
    if comparetext(sl,'kill')=0 then begin
      sleep(8000);
      exe.RunProgramAndWait(getsystemdir+'taskkill.exe', '/IM "'+sr+'" /F', DLLPath, true, false);
    end;


  end;



end;

function TWorker.GetUpdateAvailable: boolean;
begin
  result := remoteVer > localver;
end;

procedure TWorker.Init;
begin
  inherited;
  paramsfile := ApplicationParams.GetApplicationParamsfileName;
end;


function TfrmUpdateProgress.CheckForUpdates(paramsfile: string): boolean;
begin
  c := TWorker.create;
  c.RaiseExceptions := false;
  if paramsfile <> '' then
    c.paramsfile := paramsfile;
  c.CheckOnly := true;
  c.Start;


  try
    c.WAitFor;
  except
    on E: Exception do begin
      showmessage(e.Message);
    end;
  end;
  timer1.enabled := true;

  Debug.Log(paramsfile);
  Debug.Log('RemoteVersion='+inttostr(c.remoteVer)+' localversion='+inttostr(c.localver));
  result := c.UpdateAVailable;






end;

procedure TfrmUpdateProgress.CheckForUpdatesAndUpdate(paramsfile: string);
begin

  c := TWorker.create;
  if paramsfile <> '' then
    c.paramsfile := paramsfile;
  c.Start;
  timer1.enabled := true;



end;

procedure TfrmUpdateProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action := caFree;

end;

procedure TfrmUpdateProgress.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if assigned(c) then
    CanClose := false;

end;

procedure TfrmUpdateProgress.FormDestroy(Sender: TObject);
begin
  if assigned(c) then begin
    c.waitfor;
    c.free;
    c := nil;
  end;
  frmUpdateProgress := nil;
end;

procedure TfrmUpdateProgress.Timer1Timer(Sender: TObject);
begin
  if assigned(c) then begin
    if c.IsComplete then begin
      c.free;
      c := nil;
      close;
    end else begin
      label1.Caption := c.Status;
      label1.Font.Color := colorblend(clBlack, clWhite, 0.5+((((sin(getticker/500)) + 1) /2)/2));
      //color := colorblend(clNavy, clBlack, 0.5+((((sin(getticker/666)) + 1) /2)/2));
      progressbar1.Max := 1000;
      progressbar1.Position := round(c.PercentComplete*1000);
    end;

  end else begin

  end;
end;

end.
