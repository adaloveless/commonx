unit FrameInsertable;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase;

type
  TfrmInsertableFrame = class(TfrmFrameBase)
  private
    { Private declarations }
  public
    { Public declarations }

  end;

  InsertableFrameClass = class of TfrmInsertableFrame;


var
  frmInsertableFrame: TfrmInsertableFrame;

implementation

{$R *.dfm}

{ TfrmInsertableFrame }


end.
