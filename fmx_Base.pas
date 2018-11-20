unit fmx_Base;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, typex, commandprocessor,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, generics.collections, fmx_Progress;

type
  TfmxBase = class(TForm)
    tmCommandWatch: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmCommandWatchTimer(Sender: TObject);
  private
    FWaitingOnModalForm: boolean;
    FWaitingOn: TForm;
    prog: TfmxProgress;
    cmdWAiting: TCommand;
    FReenableList: TList<TComponent>;
    procedure SetWaitingOn(const Value: TForm);
    procedure SetWaitingOnModalForm(const Value: boolean);
    { Private declarations }
  protected
    UpdatingState: boolean;
  public
    { Public declarations }
    property WaitingOnModalForm: boolean read FWaitingOnModalForm write SetWaitingOnModalForm;
    property WaitingOn: TForm read FWaitingOn write SetWaitingOn;
    function CheckForModalReturn: TModalResult;
    function WaitForModalResult: TModalResult;
    function WaitOnModalForm(frm: TfmxBase): TModalResult;
    procedure UpdateState;
    procedure DoUpdateState;virtual;
    procedure DisableAllControls;
    procedure ReenableControls;
    procedure WaitOnCommand(c: TCommand);
    procedure CommandComplete(c: TCommand);
    procedure OnCommandComplete(c: TCommand);virtual;



  end;

var
  fmxBase: TfmxBase;

implementation

{$R *.fmx}

{ TfmxBase }

function TfmxBase.CheckForModalReturn: TModalResult;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TfmxBase.CommandComplete(c: TCommand);
begin
  ReenableControls;
  cmdWaiting := nil;
  prog.moveme.parent := prog;
  prog.Free;
  prog := nil;

  //do stuff?
  OnCommandComplete(c);
  //do more stuff?
end;

procedure TfmxBase.DisableAllControls;
var
  c: TComponent;
  cc: TControl;
  t: ni;
begin
  for t:= 0 to ComponentCount-1 do begin
    c := components[t];
    if c is TControl then begin
      cc := c as TControl;
      if cc.Enabled then begin
        FReenableList.add(cc);
        cc.Enabled := false;
      end;
    end;
  end;

end;

procedure TfmxBase.DoUpdateState;
begin
  //
end;

procedure TfmxBase.FormCreate(Sender: TObject);
begin
  FReenableList := TList<TComponent>.create;
end;

procedure TfmxBase.FormDestroy(Sender: TObject);
begin
  FReenableList.free;
  FReenableList := nil;
  //
end;

procedure TfmxBase.OnCommandComplete(c: TCommand);
begin
  //
end;

procedure TfmxBase.ReenableControls;
var
  t: ni;
  cc: TControl;
  c: TComponent;
begin
  for t:= FReenableList.count-1 downto 0 do begin
    c := FReenableList[t];
    if c is TControl then begin
      cc := FReenableList[t] as TControl;
      cc.Enabled := true;
      FREenableList.Delete(t);
    end;
  end;

end;

procedure TfmxBase.SetWaitingOn(const Value: TForm);
begin
  FWaitingOn := Value;
end;

procedure TfmxBase.SetWaitingOnModalForm(const Value: boolean);
begin
  FWaitingOnModalForm := Value;
end;

procedure TfmxBase.tmCommandWatchTimer(Sender: TObject);
begin
  if assigned(cmdWaiting) then begin
    if cmdWaiting.IsComplete then begin
      CommandComplete(cmdWaiting);
    end else begin
      prog.Progress(cmdWaiting.Status, cmdWaiting.Step, cmdWaiting.StepCount);
    end;
  end;
end;

procedure TfmxBase.UpdateState;
begin
  DoUpdateState;
  //
end;

function TfmxBase.WaitForModalResult: TModalResult;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TfmxBase.WaitOnCommand(c: TCommand);
begin
  DisableAllControls;
  prog := TfmxProgress.create(self);
  cmdWaiting := c;
  //prog.Show;
  prog.moveme.parent := self;
  prog.Progress(c.Status, c.Step, c.StepCount);



end;

function TfmxBase.WaitOnModalForm(frm: TfmxBase): TModalResult;
begin

  raise Exception.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

end.
