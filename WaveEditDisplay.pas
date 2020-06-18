unit WaveEditDisplay;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SoundEditorFrame, Vcl.Menus, soundtools, applicationparams,
  Vcl.ExtCtrls, Vcl.StdCtrls, GlassControls, advancedgraphics, Vcl.ComCtrls, namevaluepair, numbers,
  graphicwincontrol, SoundDevice_PortAudio, BasicWaveDisplay, typex;

type
  TfrmSoundFrameBase = class(TFrameBasicWaveDisplay)
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure cbDevListEnter(Sender: TObject);
    procedure cbDevListChange(Sender: TObject);
  private
    { Private declarations }
    FParams: TNameValuePairList;
  public
    { Public declarations }
    sFile, sMovieFile: string;
    sAudioFile: string;
    constructor Create(aowner: TComponent); reintroduce; virtual;
    procedure DoLoad;
    procedure UpdateDeviceList;
    procedure DeviceChanged;
  end;

var
  frmSoundFrameBase: TfrmSoundFrameBase;

implementation

{$R *.dfm}

{ TfrmInherited }

procedure TfrmSoundFrameBase.Button6Click(Sender: TObject);
begin
  inherited;
  FParams['In'].Value := inttostr(round(self.SelectStart / 44100)*1000);
  SampleStart := self.SelectStart;

  FParams.SaveToFile;
end;

procedure TfrmSoundFrameBase.Button7Click(Sender: TObject);
begin
  inherited;
  FParams['Out'].Value := inttostr(round(self.SelectEnd / 44100)*1000);
  SampleEnd := self.SelectEnd;
  FParams.SaveToFile;
end;

procedure TfrmSoundFrameBase.cbDevListChange(Sender: TObject);
begin
  inherited;
  DeviceChanged;
end;

procedure TfrmSoundFrameBase.cbDevListEnter(Sender: TObject);
begin
  inherited;
  UpdateDeviceList;
end;

constructor TfrmSoundFrameBase.Create(aowner: TComponent);
var

  bAlt: boolean;
  ap: TAppParams;
begin
  inherited CReate(aowner);

  FParams := TNameValuePairList.Create;
  FParams.AutoAdd := true;


  sFile := Paramstr(1);

  FPArams.LoadFromFile(sFile);




  bAlt := FParams.GetItemEx('UseAlternateAudioExtraction', true);

  sMovieFile := FParams.GetItemEx('FileName','');

  sAudioFile := changefileext(sFile,'.mp3');
  if not fileexists(sAudioFile) then begin
    sAudioFile := changefileext(sFile,'.boog');
    if not fileexists(sAudioFile) then
      sAudioFile := changefileext(sFile, '.wav');

  end;


  UpdateDeviceList;

  ap := NeedAppParams;
  try

  finally
    NoNeedAppParams(ap);
  end;



end;

procedure TfrmSoundFrameBase.DeviceChanged;
begin

  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TfrmSoundFrameBase.DoLoad;
begin
  self.LoadFromFile(sAudioFile);
  LoopStart := -1;
  LoopEnd := -1;
  SampleStart := round(44100*(FPArams.GetItemEx('In', 0)/1000));
  Sampleend := round(44100*(FPArams.GetItemEx('Out', self.SAmpleLength)/1000));
end;

procedure TfrmSoundFrameBase.UpdateDeviceList;
var
  sl: TStringlist;

begin
  exit;
//  cbDevList.items.Clear;
//  sl := nil;
//  try
//    sl := GetAudioDeviceList;
//    cbDevList.Items.Text := sl.Text;
//  finally
//    sl.Free;
//  end;
end;

end.
