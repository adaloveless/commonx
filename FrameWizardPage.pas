unit FrameWizardPage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FrameBaseVcl;

type
  TfrmWizardPage = class(TfrmFrameBase)
  private
    FOnPageActivate: TNotifyEvent;
    FOnPageFinish: TNotifyEvent;
    FOnPagePrevious: TNotifyEvent;
    { Private declarations }
  public
    { Public declarations }
  published
    property OnPageActivate: TNotifyEvent read FOnPageActivate write FOnPageActivate;
    property OnPageFinish: TNotifyEvent read FOnPageFinish write FOnPageFinish;
    property OnPagePrevious: TNotifyEvent read FOnPagePrevious write FOnPagePrevious;
  end;

  TWizardFrameClass = class of TfrmWizardPage;


implementation

{$R *.dfm}

end.
