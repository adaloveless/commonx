unit fmx_Progress;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, typex,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, managedthread, commandprocessor,
  FMX.Controls.Presentation;

type
  TfmxProgress = class(TForm)
    tmModalTest: TTimer;
    moveme: TPanel;
    panFill: TPanel;
    panProgTop: TPanel;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    aniind: TAniIndicator;
    procedure tmModalTestTimer(Sender: TObject);
  private
    FStatus: string;
    watch_cp: TCommandProcessor;
    watch_mt: TManagedThread;
    watch_t: TThread;
    watch_c: TCommand;


    procedure SetStatus(const Value: string);
    procedure SetMax(const Value: nativefloat);
    function GetMax: nativefloat;
    function GetValue: nativefloat;
    procedure SetValue(const Value: nativefloat);
    procedure IdiotProc(ModalResult: TModalResult);
    { Private declarations }

  public

    { Public declarations }
    property Status: string read FStatus write SetStatus;
    property Max: nativefloat read GetMax write SetMax;
    property Value: nativefloat read GEtValue write SEtValue;

    function ClassicShowModal: TModalResult;
    procedure Watch(t: TThread);overload;
    procedure Watch(mt: TManagedThread);overload;
    procedure Watch(cp: TCommandProcessor);overload;
    procedure Watch(c: TCommand);overload;
    procedure Progress(sStatus: string; step, stepcount: ni);overload;
    procedure Progress(step,stepcount: ni);overload;
  end;

var
  fmxProgress: TfmxProgress;

implementation

{$R *.fmx}

{ TfmxProgress }

function TfmxProgress.ClassicShowModal: TModalResult;
begin
  ShowModal(IdiotProc);
  result := ModalResult;
end;

function TfmxProgress.GetMax: nativefloat;
begin
  result := ProgressBar1.Max;
end;

function TfmxProgress.getValue: nativefloat;
begin
  result := ProgressBar1.Value;
end;

procedure TfmxProgress.IdiotProc(ModalResult: TModalResult);
begin
  self.ModalResult := ModalResult;
  hide;
end;

procedure TfmxProgress.Progress(sStatus: string; step, stepcount: ni);
begin
  self.Label1.Text := sStatus;
  ProgressBar1.Min := 0;
  progressbar1.Max := stepcount;
  progressbar1.Value := step;


end;

procedure TfmxProgress.Progress(step, stepcount: ni);
begin
  ProgressBar1.Min := 0;
  progressbar1.Max := stepcount;
  progressbar1.Value := step;

end;

procedure TfmxProgress.SetMax(const Value: nativefloat);
begin
  self.ProgressBar1.Max := value;
end;

procedure TfmxProgress.SEtValue(const Value: nativefloat);
begin
  ProgressBar1.Value:= value;
end;

procedure TfmxProgress.SetStatus(const Value: string);
begin
  FStatus := Value;
  label1.Text := value;
end;

procedure TfmxProgress.tmModalTestTimer(Sender: TObject);
begin
  if assigned(watch_c) then begin
    Status := watch_c.Status;
    Max := watch_c.StepCount;
    self.Value := watch_c.Step;
    if watch_c.IsComplete then
      modalresult := mrOk;
  end;

  if assigned(watch_cp) then begin
    Status := 'Processing Commands...';
    Max := 1;
    self.Value := watch_cp.PercentComplete;
    if watch_cp.IsComplete then
      modalresult := mrOk;
  end;

  if assigned(watch_mt) then begin
    Status := 'Waiting for thread to complete...';
    Max := 1;
    self.Value := 1;
    if watch_mt.IsFinished then
      modalresult := mrOk;
  end;

  if assigned(watch_t) then begin
    Status := 'Waiting for thread to complete...';
    Max := 1;
    self.Value := 1;
    if watch_t.Finished then
      modalresult := mrOk;
  end;


end;

procedure TfmxProgress.Watch(t: TThread);
begin
  watch_t := t;
  ClassicShowModal;
  watch_t := nil;
end;

procedure TfmxProgress.Watch(c: TCommand);
begin
  watch_c := c;
  ClassicShowModal;
  watch_c := nil;


end;

procedure TfmxProgress.Watch(mt: TManagedThread);
begin
  watch_mt := mt;
  ClassicShowModal;
  watch_mt := nil;
end;

procedure TfmxProgress.Watch(cp: TCommandProcessor);
begin
  watch_cp:= cp;
  ClassicShowModal;
  watch_cp:= nil;
end;

end.

