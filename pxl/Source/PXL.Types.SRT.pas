unit PXL.Types.SRT;
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

uses
  PXL.Types, PXL.Surfaces, PXL.Devices;

type
  TSRTDeviceContextWriter = class abstract(TCustomDeviceContextWriter)
  private
    procedure SetSurface(const Value: TConceptualPixelSurface); virtual; abstract;
    procedure SetSurfaceSize(const Value: TPoint2px); virtual; abstract;
  public
    property Surface: TConceptualPixelSurface write SetSurface;
    property SurfaceSize: TPoint2px write SetSurfaceSize;
  end;

  TSRTDeviceContext = class(TCustomDeviceContext)
  private type
    TWriter = class(TSRTDeviceContextWriter)
    protected
      procedure SetSurface(const Value: TConceptualPixelSurface); override;
      procedure SetSurfaceSize(const Value: TPoint2px); override;
    end;
  private
    FSurface: TConceptualPixelSurface;
    FSurfaceSize: TPoint2px;
  public
    constructor Create(const ADevice: TCustomDevice; out AWriter: TSRTDeviceContextWriter);

    property Surface: TConceptualPixelSurface read FSurface;
    property SurfaceSize: TPoint2px read FSurfaceSize;
  end;

implementation

{$REGION 'TSRTDeviceContext.TWriter'}

procedure TSRTDeviceContext.TWriter.SetSurface(const Value: TConceptualPixelSurface);
begin
  TSRTDeviceContext(Context).FSurface := Value;
end;

procedure TSRTDeviceContext.TWriter.SetSurfaceSize(const Value: TPoint2px);
begin
  TSRTDeviceContext(Context).FSurfaceSize := Value;
end;

{$ENDREGION}
{$REGION 'TSRTDeviceContext'}

constructor TSRTDeviceContext.Create(const ADevice: TCustomDevice; out AWriter: TSRTDeviceContextWriter);
begin
  inherited Create(ADevice);

  AWriter := TWriter.Create(Self);
end;

{$ENDREGION}

end.
