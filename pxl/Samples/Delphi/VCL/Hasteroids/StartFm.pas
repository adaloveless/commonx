unit StartFm;
{
  This file is part of Asphyre Framework, also known as Pascal eXtended Library (PXL).
  Copyright (c) 2000 - 2015  Yuriy Kotsarenko

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
  Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
  details.
}
interface

{$INCLUDE PXL.Config.inc}

{ Special note: this code was ported multiple times from earliest framework releases predating Asphyre. }

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, PXL.TypeDef, PXL.Providers;

type
  TStartForm = class(TForm)
    LogoImage: TImage;
    TopBevel: TBevel;
    NameGroup: TGroupBox;
    NameEdit: TEdit;
    ConfigGroup: TGroupBox;
    PlayButton: TBitBtn;
    CloseButton: TBitBtn;
    VSyncBox: TCheckBox;
    ProviderBox: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetVSync: Boolean;
    function GetPlayerName: UniString;
  public
    { Public declarations }
    function CreateProvider: TGraphicsDeviceProvider;

    property VSync: Boolean read GetVSync;
    property PlayerName: UniString read GetPlayerName;
  end;

var
  StartForm: TStartForm;

implementation
{$R *.dfm}

uses
  {$IFDEF CPUX86}PXL.Providers.DX7,{$ENDIF} PXL.Providers.DX9, PXL.Providers.DX11, PXL.Providers.GL;

procedure TStartForm.FormCreate(Sender: TObject);
begin
  ProviderBox.Items.Add('DirectX 11');
  ProviderBox.Items.Add('DirectX 9');
  ProviderBox.Items.Add('OpenGL');
  {$IFDEF CPUX86}
    ProviderBox.Items.Add('DirectX 7');
  {$ENDIF}
  ProviderBox.ItemIndex := 0;
end;

function TStartForm.GetVSync: Boolean;
begin
  Result := VSyncBox.Checked;
end;

function TStartForm.GetPlayerName: UniString;
begin
  Result := NameEdit.Text;
end;

function TStartForm.CreateProvider: TGraphicsDeviceProvider;
begin
  case ProviderBox.ItemIndex of
    0: Result := TDX11Provider.Create(nil);
    1: Result := TDX9Provider.Create(nil);
    2: Result := TGLProvider.Create(nil);
  {$IFDEF CPUX86}
    3: Result := TDX7Provider.Create(nil);
  {$ENDIF}
  end;
end;

end.
