program Hasteroids;

uses
  Forms,
  MainFm in 'MainFm.pas' {MainForm},
  StartFm in 'StartFm.pas' {StartForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
