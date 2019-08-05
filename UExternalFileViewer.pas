{****************************************************}
{                                                    }
{ firemonkey-external-file-viewer                    }
{                                                    }
{ Copyright (C) 2018 Code Partners Pty Ltd           }
{                                                    }
{ http://www.code-partners.com                       }
{                                                    }
{****************************************************}
{                                                    }
{ This Source Code Form is subject to the terms of   }
{ the Mozilla Public License, v. 2.0. If a copy of   }
{ the MPL was not distributed with this file, You    }
{ can obtain one at                                  }
{                                                    }
{ http://mozilla.org/MPL/2.0/                        }
{                                                    }
{****************************************************}
unit UExternalFileViewer;

interface

uses
  System.SysUtils, System.Classes, FMX.Forms;

type
  TExternalFileViewer = class (TComponent)
  protected
    FForm: TForm;

    constructor Create(AOwner: TComponent; AForm: TForm); reintroduce; virtual;
  public
    class function Factory(AOwner: TComponent; AForm: TForm): TExternalFileViewer;

    procedure OpenFile(Path: string); virtual; abstract;
    procedure OpenURL(URL: string); virtual; abstract;
  end;

  TDummyExternalFileViewer = class (TExternalFileViewer)
  public
    procedure OpenFile(Path: string); override;
    procedure OpenURL(URL: string); override;
  end;

implementation

  {$IF DEFINED(ANDROID)}
    uses
      UExternalFileViewer.Android;
  {$ELSEIF DEFINED(IOS)}
    uses
      UExternalFileViewer.iOS;
  {$ENDIF}

{ TExternalFileViewer }

constructor TExternalFileViewer.Create(AOwner: TComponent; AForm: TForm);
begin
  inherited Create(AOwner);
  self.FForm := AForm;
end;

class function TExternalFileViewer.Factory(AOwner: TComponent;
  AForm: TForm): TExternalFileViewer;
begin
  {$IF DEFINED(ANDROID)}
    Result := TAndroidExternalFileViewer.Create(AOwner, AForm);
  {$ELSEIF DEFINED(IOS)}
    Result := TiOSExternalFileViewer.Create(AOwner, AForm);
  {$ELSE}
    Result := TDummyExternalFileViewer.Create(AOwner, AForm);
  {$ENDIF}
end;

{ TDummyExternalFileViewer }

procedure TDummyExternalFileViewer.OpenFile(Path: string);
begin

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

procedure TDummyExternalFileViewer.OpenURL(URL: string);
begin

//  raise ECritical.create('unimplemented');
//TODO -cunimplemented: unimplemented block
end;

end.
