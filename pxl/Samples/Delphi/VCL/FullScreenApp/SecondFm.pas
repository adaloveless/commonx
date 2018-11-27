unit SecondFm;
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

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TSecondForm = class(TForm)
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SecondForm: TSecondForm;

implementation

{$R *.dfm}

uses
  PXL.Types, MainFm;

procedure TSecondForm.FormResize(Sender: TObject);
begin
  if MainForm <> nil then
  begin
    MainForm.SecondarySize := Point2px(ClientWidth, ClientHeight);

    if MainForm.EngineDevice <> nil then
      MainForm.EngineDevice.Resize(1, MainForm.SecondarySize);
  end;
end;

procedure TSecondForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TSecondForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (MainForm <> nil) and MainForm.Visible then
    MainForm.Close;
end;

end.
