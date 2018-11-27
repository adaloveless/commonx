unit PXL.Shaders.DX11;
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
  Windows, PXL.Windows.D3D11, PXL.TypeDef, PXL.Types, PXL.Types.DX11;

type
  PDX11BufferVariable = ^TDX11BufferVariable;
  TDX11BufferVariable = record
    VariableName: StdString;
    ByteAddress: Integer;
    SizeInBytes: Integer;
  end;

  TDX11BufferVariables = class
  private
    Data: array of TDX11BufferVariable;
    DataDirty: Boolean;

    procedure DataListSwap(const Index1, Index2: Integer);
    function DataListCompare(const Item1, Item2: TDX11BufferVariable): Integer;
    function DataListSplit(const Start, Stop: Integer): Integer;
    procedure DataListSort(const Start, Stop: Integer);
    procedure UpdateDataDirty;
    function IndexOf(const Name: StdString): Integer;
    function GetVariable(const Name: StdString): PDX11BufferVariable;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Declare(const Name: StdString; const ByteAddress, SizeInBytes: Integer);
    procedure Clear;

    property Variable[const Name: StdString]: PDX11BufferVariable read GetVariable; default;
  end;

  TDX11ConstantBufferType = (Unknown, Vertex, Pixel);

  TDX11ConstantBuffer = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX11DeviceContext;
    FVariables: TDX11BufferVariables;

    FName: StdString;
    FInitialized: Boolean;

    FBufferType: TDX11ConstantBufferType;
    FBufferSize: Integer;

    FSystemBuffer: Pointer;
    FVideoBuffer: ID3D11Buffer;

    FConstantIndex: Integer;

    function UpdateVariable(const Variable: TDX11BufferVariable; const Content: Pointer; const ByteOffset,
      ByteCount: Integer): Boolean;

    function SetBasicVariable(const VariableName: StdString; const Content: Pointer; const ContentSize,
      SubIndex: Integer): Boolean;
  public
    constructor Create(const AContext: TDX11DeviceContext; const AName: StdString);
    destructor Destroy; override;

    function Initialize: Boolean;
    procedure Finalize;

    function Update: Boolean;
    function Bind: Boolean;

    function SetInt(const VariableName: StdString; const Value: LongInt; const SubIndex: Integer = 0): Boolean;
    function SetUInt(const VariableName: StdString; const Value: LongWord; const SubIndex: Integer = 0): Boolean;
    function SetFloat(const VariableName: StdString; const Value: Single; const SubIndex: Integer = 0): Boolean;
    function SetPoint2(const VariableName: StdString; const Value: TPoint2; const SubIndex: Integer = 0): Boolean;
    function SetVector3(const VariableName: StdString; const Value: TVector3; const SubIndex: Integer = 0): Boolean;
    function SetVector4(const VariableName: StdString; const Value: TVector4; const SubIndex: Integer = 0): Boolean;
    function SetMatrix4(const VariableName: StdString; const Value: TMatrix4; const SubIndex: Integer = 0): Boolean;

    property Context: TDX11DeviceContext read FContext;

    property Name: StdString read FName;
    property Initialized: Boolean read FInitialized;

    property BufferType: TDX11ConstantBufferType read FBufferType write FBufferType;
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property SystemBuffer: Pointer read FSystemBuffer;
    property VideoBuffer: ID3D11Buffer read FVideoBuffer;
    property ConstantIndex: Integer read FConstantIndex write FConstantIndex;

    property Variables: TDX11BufferVariables read FVariables;
  end;

  TDX11ShaderEffect = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FContext: TDX11DeviceContext;

    ConstantBuffers: array of TDX11ConstantBuffer;
    ConstantBuffersDirty: Boolean;

    FInitialized: Boolean;

    FInputLayout: ID3D11InputLayout;
    FVertexShader: ID3D11VertexShader;
    FPixelShader: ID3D11PixelShader;

    VertexLayoutDesc: array of D3D11_INPUT_ELEMENT_DESC;

    BinaryVS: Pointer;
    BinaryVSLength: Integer;

    BinaryPS: Pointer;
    BinaryPSLength: Integer;

    procedure ConstantBufferSwap(const Index1, Index2: Integer);
    function ConstantBufferCompare(const Item1, Item2: TDX11ConstantBuffer): Integer;
    function ConstantBufferSplit(const Start, Stop: Integer): Integer;
    procedure ConstantBufferSort(const Start, Stop: Integer);
    procedure OrderConstantBuffers;

    function IndexOfConstantBuffer(const Name: StdString): Integer;
    function GetConstantBuffer(const Name: StdString): TDX11ConstantBuffer;
  public
    constructor Create(const AContext: TDX11DeviceContext);
    destructor Destroy; override;

    procedure RemoveAllConstantBuffers;

    function AddConstantBuffer(const Name: StdString; const BufferType: TDX11ConstantBufferType;
      const BufferSize: Integer; const ConstantIndex: Integer = 0): TDX11ConstantBuffer;

    function UpdateBindAllBuffers: Boolean;

    procedure SetVertexLayout(const Content: PD3D11_INPUT_ELEMENT_DESC; const ElementCount: Integer);

    procedure SetShaderCodes(const AVertexShader: Pointer; const VertexShaderLength: Integer;
      const APixelShader: Pointer; const PixelShaderLength: Integer);

    function Initialize: Boolean;
    procedure Finalize;

    function Activate: Boolean;
    procedure Deactivate;

    property Context: TDX11DeviceContext read FContext;

    property Initialized: Boolean read FInitialized;

    property InputLayout: ID3D11InputLayout read FInputLayout;
    property VertexShader: ID3D11VertexShader read FVertexShader;
    property PixelShader: ID3D11PixelShader read FPixelShader;

    property ConstantBuffer[const Name: StdString]: TDX11ConstantBuffer read GetConstantBuffer; default;
  end;

implementation

uses
  SysUtils, Math;

{$REGION 'TDX11BufferVariables'}

constructor TDX11BufferVariables.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
  DataDirty := False;
end;

destructor TDX11BufferVariables.Destroy;
begin
  try
    Clear;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

procedure TDX11BufferVariables.DataListSwap(const Index1, Index2: Integer);
var
  Temp: TDX11BufferVariable;
begin
  Temp := Data[Index1];
  Data[Index1] := Data[Index2];
  Data[Index2] := Temp;
end;

function TDX11BufferVariables.DataListCompare(const Item1, Item2: TDX11BufferVariable): Integer;
begin
  Result := CompareText(Item1.VariableName, Item2.VariableName);
end;

function TDX11BufferVariables.DataListSplit(const Start, Stop: Integer): Integer;
var
  Left, Right: Integer;
  Pivot: TDX11BufferVariable;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := Data[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (DataListCompare(Data[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (DataListCompare(Data[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      DataListSwap(Left, Right);
  end;

  DataListSwap(Start, Right);
  Result := Right;
end;

procedure TDX11BufferVariables.DataListSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := DataListSplit(Start, Stop);
    DataListSort(Start, SplitPt - 1);
    DataListSort(SplitPt + 1, Stop);
  end;
end;

procedure TDX11BufferVariables.UpdateDataDirty;
begin
  if Length(Data) > 1 then
    DatalistSort(0, Length(Data) - 1);

  DataDirty := False;
end;

function TDX11BufferVariables.IndexOf(const Name: StdString): Integer;
var
  Left, Right, Pivot, Res: Integer;
begin
  if DataDirty then
    UpdateDataDirty;

  Left := 0;
  Right := Length(Data) - 1;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;
    Res := CompareText(Data[Pivot].VariableName, Name);

    if Res = 0 then
      Exit(Pivot);

    if Res > 0 then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

procedure TDX11BufferVariables.Clear;
begin
  SetLength(Data, 0);
  DataDirty := False;
end;

function TDX11BufferVariables.GetVariable(const Name: StdString): PDX11BufferVariable;
var
  Index: Integer;
begin
  Index := IndexOf(Name);

  if Index <> -1 then
    Result := @Data[Index]
  else
    Result := nil;
end;

procedure TDX11BufferVariables.Declare(const Name: StdString; const ByteAddress, SizeInBytes: Integer);
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  if Index = -1 then
  begin
    Index := Length(Data);
    SetLength(Data, Index + 1);
    DataDirty := True;
  end;

  Data[Index].VariableName := Name;
  Data[Index].ByteAddress := ByteAddress;
  Data[Index].SizeInBytes := SizeInBytes;
end;

{$ENDREGION}
{$REGION 'TDX11ConstantBuffer'}

constructor TDX11ConstantBuffer.Create(const AContext: TDX11DeviceContext; const AName: StdString);
begin
  inherited Create;

  FVariables := TDX11BufferVariables.Create;

  FContext := AContext;
  FName := AName;

  Increment_PXL_ClassInstances;
end;

destructor TDX11ConstantBuffer.Destroy;
begin
  try
    if FInitialized then
      Finalize;

    FVariables.Free;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TDX11ConstantBuffer.Initialize: Boolean;
var
  Desc: D3D11_BUFFER_DESC;
begin
  if (FContext = nil) or (FContext.Device = nil) or FInitialized or (FBufferSize < 1) then
    Exit(False);

  FillChar(Desc, SizeOf(D3D11_BUFFER_DESC), 0);

  Desc.ByteWidth := FBufferSize;
  Desc.Usage := D3D11_USAGE_DYNAMIC;
  Desc.BindFlags := Cardinal(D3D11_BIND_CONSTANT_BUFFER);
  Desc.CPUAccessFlags := Cardinal(D3D11_CPU_ACCESS_WRITE);

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateBuffer(Desc, nil, @FVideoBuffer)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  FSystemBuffer := AllocMem(FBufferSize);
  FInitialized := True;
  Result := True;
end;

procedure TDX11ConstantBuffer.Finalize;
begin
  if not FInitialized then
    Exit;

  FreeMemAndNil(FSystemBuffer);
  FVideoBuffer := nil;
  FInitialized := False;
end;

function TDX11ConstantBuffer.Update: Boolean;
var
  Mapped: D3D11_MAPPED_SUBRESOURCE;
begin
  if (not FInitialized) or (FContext = nil) or (FContext.Context = nil) or (FSystemBuffer = nil) or
    (FVideoBuffer = nil) then
    Exit(False);

  PushClearFPUState;
  try
    if Failed(FContext.Context.Map(FVideoBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, Mapped)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  Move(FSystemBuffer^, Mapped.Data^, FBufferSize);

  PushClearFPUState;
  try
    FContext.Context.Unmap(FVideoBuffer, 0);
  finally
    PopFPUState;
  end;

  Result := True;
end;

function TDX11ConstantBuffer.Bind: Boolean;
begin
  if (not FInitialized) or (FConstantIndex < 0) or (FBufferType = TDX11ConstantBufferType.Unknown) or
    (FContext = nil) or (FContext.Context = nil) or (FVideoBuffer = nil) then
    Exit(False);

  PushClearFPUState;
  try
    case FBufferType of
      TDX11ConstantBufferType.Vertex:
        FContext.Context.VSSetConstantBuffers(FConstantIndex, 1, @FVideoBuffer);

      TDX11ConstantBufferType.Pixel:
        FContext.Context.PSSetConstantBuffers(FConstantIndex, 1, @FVideoBuffer);
    end;
  finally
    PopFPUState;
  end;

  Result := True;
end;

function TDX11ConstantBuffer.UpdateVariable(const Variable: TDX11BufferVariable; const Content: Pointer;
  const ByteOffset, ByteCount: Integer): Boolean;
var
  MinBytes: Integer;
  WritePtr: Pointer;
begin
  if (Content = nil) or ((ByteOffset > 0) and (Variable.SizeInBytes <= ByteOffset)) then
    Exit(False);

  MinBytes := Min(ByteCount, Variable.SizeInBytes - ByteOffset);
  if MinBytes < 1 then
    Exit(False);

  WritePtr := Pointer(PtrInt(FSystemBuffer) + Variable.ByteAddress + ByteOffset);
  Move(Content^, WritePtr^, MinBytes);

  Result := True;
end;

function TDX11ConstantBuffer.SetBasicVariable(const VariableName: StdString; const Content: Pointer; const ContentSize,
  SubIndex: Integer): Boolean;
var
  Variable: PDX11BufferVariable;
begin
  Variable := FVariables[VariableName];
  if Variable = nil then
    Exit(False);

  Result := UpdateVariable(Variable^, Content, SubIndex * ContentSize, ContentSize);
end;

function TDX11ConstantBuffer.SetInt(const VariableName: StdString; const Value: LongInt;
  const SubIndex: Integer): Boolean;
begin
  Result := SetBasicVariable(VariableName, @Value, SizeOf(LongInt), SubIndex);
end;

function TDX11ConstantBuffer.SetUInt(const VariableName: StdString; const Value: LongWord;
  const SubIndex: Integer): Boolean;
begin
  Result := SetBasicVariable(VariableName, @Value, SizeOf(LongWord), SubIndex);
end;

function TDX11ConstantBuffer.SetFloat(const VariableName: StdString; const Value: Single;
  const SubIndex: Integer): Boolean;
begin
  Result := SetBasicVariable(VariableName, @Value, SizeOf(Single), SubIndex);
end;

function TDX11ConstantBuffer.SetPoint2(const VariableName: StdString; const Value: TPoint2;
  const SubIndex: Integer): Boolean;
begin
  Result := SetBasicVariable(VariableName, @Value, SizeOf(TPoint2), SubIndex);
end;

function TDX11ConstantBuffer.SetVector3(const VariableName: StdString; const Value: TVector3;
  const SubIndex: Integer): Boolean;
begin
  Result := SetBasicVariable(VariableName, @Value, SizeOf(TVector3), SubIndex);
end;

function TDX11ConstantBuffer.SetVector4(const VariableName: StdString; const Value: TVector4;
  const SubIndex: Integer): Boolean;
begin
  Result := SetBasicVariable(VariableName, @Value, SizeOf(TVector4), SubIndex);
end;

function TDX11ConstantBuffer.SetMatrix4(const VariableName: StdString; const Value: TMatrix4;
  const SubIndex: Integer): Boolean;
var
  Temp: TMatrix4;
begin
  Temp := TransposeMtx4(Value);
  Result := SetBasicVariable(VariableName, @Temp, SizeOf(TMatrix4), SubIndex);
end;

{$ENDREGION}
{$REGION 'TDX11ShaderEffect'}

constructor TDX11ShaderEffect.Create(const AContext: TDX11DeviceContext);
begin
  inherited Create;

  FContext := AContext;

  Increment_PXL_ClassInstances;
end;

destructor TDX11ShaderEffect.Destroy;
begin
  try
    RemoveAllConstantBuffers;

    if FInitialized then
      Finalize;

    if BinaryPS <> nil then
    begin
      FreeMemAndNil(BinaryPS);
      BinaryPSLength := 0;
    end;

    if BinaryVS <> nil then
    begin
      FreeMemAndNil(BinaryVS);
      BinaryVSLength := 0;
    end;

    FContext := nil;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

procedure TDX11ShaderEffect.RemoveAllConstantBuffers;
var
  I: Integer;
begin
  for I := Length(ConstantBuffers) - 1 downto 0 do
    ConstantBuffers[I].Free;

  SetLength(ConstantBuffers, 0);
  ConstantBuffersDirty := False;
end;

procedure TDX11ShaderEffect.ConstantBufferSwap(const Index1, Index2: Integer);
var
  Temp: TDX11ConstantBuffer;
begin
  Temp := ConstantBuffers[Index1];
  ConstantBuffers[Index1] := ConstantBuffers[Index2];
  ConstantBuffers[Index2] := Temp;
end;

function TDX11ShaderEffect.ConstantBufferCompare(const Item1, Item2: TDX11ConstantBuffer): Integer;
begin
  Result := CompareText(Item1.Name, Item2.Name);
end;

function TDX11ShaderEffect.ConstantBufferSplit(const Start, Stop: Integer): Integer;
var
  Left, Right: Integer;
  Pivot: TDX11ConstantBuffer;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := ConstantBuffers[Start];

  while Left <= Right do
  begin
    while (Left <= Stop) and (ConstantBufferCompare(ConstantBuffers[Left], Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (ConstantBufferCompare(ConstantBuffers[Right], Pivot) >= 0) do
      Dec(Right);

    if Left < Right then
      ConstantBufferSwap(Left, Right);
  end;

  ConstantBufferSwap(Start, Right);
  Result := Right;
end;

procedure TDX11ShaderEffect.ConstantBufferSort(const Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if Start < Stop then
  begin
    SplitPt := ConstantBufferSplit(Start, Stop);
    ConstantBufferSort(Start, SplitPt - 1);
    ConstantBufferSort(SplitPt + 1, Stop);
  end;
end;

procedure TDX11ShaderEffect.OrderConstantBuffers;
begin
  if Length(ConstantBuffers) > 1 then
    ConstantBufferSort(0, Length(ConstantBuffers) - 1);

  ConstantBuffersDirty := False;
end;

function TDX11ShaderEffect.IndexOfConstantBuffer(const Name: StdString): Integer;
var
  Left, Right, Pivot, Res: Integer;
begin
  if ConstantBuffersDirty then
    OrderConstantBuffers;

  Left := 0;
  Right := Length(ConstantBuffers) - 1;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;
    Res := CompareText(ConstantBuffers[Pivot].Name, Name);

    if Res = 0 then
      Exit(Pivot);

    if Res > 0 then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

function TDX11ShaderEffect.GetConstantBuffer(const Name: StdString): TDX11ConstantBuffer;
var
  Index: Integer;
begin
  Index := IndexOfConstantBuffer(Name);

  if Index <> -1 then
    Result := ConstantBuffers[Index]
  else
    Result := nil;
end;

function TDX11ShaderEffect.AddConstantBuffer(const Name: StdString; const BufferType: TDX11ConstantBufferType;
  const BufferSize, ConstantIndex: Integer): TDX11ConstantBuffer;
var
  Index: Integer;
begin
  if Length(Name) < 1 then
    Exit(nil);

  Index := IndexOfConstantBuffer(Name);
  if Index <> -1 then
    Exit(ConstantBuffers[Index]);

  Result := TDX11ConstantBuffer.Create(FContext, Name);
  Result.BufferType := BufferType;
  Result.BufferSize := BufferSize;
  Result.ConstantIndex := ConstantIndex;

  if not Result.Initialize then
  begin
    FreeAndNil(Result);
    Exit;
  end;

  Index := Length(ConstantBuffers);
  SetLength(ConstantBuffers, Index + 1);

  ConstantBuffers[Index] := Result;
  ConstantBuffersDirty := True;
end;

function TDX11ShaderEffect.UpdateBindAllBuffers: Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to Length(ConstantBuffers) - 1 do
    if (ConstantBuffers[I] <> nil) and ConstantBuffers[I].Initialized then
    begin
      Result := ConstantBuffers[I].Update;
      if not Result then
        Break;

      Result := ConstantBuffers[I].Bind;
      if not Result then
        Break;
    end;
end;

procedure TDX11ShaderEffect.SetVertexLayout(const Content: PD3D11_INPUT_ELEMENT_DESC; const ElementCount: Integer);
var
  I: Integer;
  Source: PD3D11_INPUT_ELEMENT_DESC;
begin
  Source := Content;

  SetLength(VertexLayoutDesc, ElementCount);

  for I := 0 to Length(VertexLayoutDesc) - 1 do
  begin
    Move(Source^, VertexLayoutDesc[I], SizeOf(D3D11_INPUT_ELEMENT_DESC));
    Inc(Source);
  end;
end;

procedure TDX11ShaderEffect.SetShaderCodes(const AVertexShader: Pointer; const VertexShaderLength: Integer;
  const APixelShader: Pointer; const PixelShaderLength: Integer);
begin
  if (AVertexShader <> nil) and (VertexShaderLength > 0) then
  begin
    BinaryVSLength := VertexShaderLength;

    ReallocMem(BinaryVS, BinaryVSLength);
    Move(AVertexShader^, BinaryVS^, BinaryVSLength);
  end
  else if BinaryVS <> nil then
  begin
    FreeMemAndNil(BinaryVS);
    BinaryVSLength := 0;
  end;

  if (APixelShader <> nil) and (PixelShaderLength > 0) then
  begin
    BinaryPSLength := PixelShaderLength;

    ReallocMem(BinaryPS, BinaryPSLength);
    Move(APixelShader^, BinaryPS^, BinaryPSLength);
  end
  else if BinaryPS <> nil then
  begin
    FreeMemAndNil(BinaryPS);
    BinaryPSLength := 0;
  end;
end;

function TDX11ShaderEffect.Initialize: Boolean;
var
  hr: HResult;
begin
  if FInitialized or (FContext = nil) or (FContext.Device = nil) or (Length(VertexLayoutDesc) < 1) or
    (BinaryVS = nil) or (BinaryPS = nil) then
    Exit(False);

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateVertexShader(BinaryVS, BinaryVSLength, nil, @FVertexShader)) then
      Exit(False);
  finally
    PopFPUState;
  end;

  PushClearFPUState;
  try
    if Failed(FContext.Device.CreateInputLayout(@VertexLayoutDesc[0], Length(VertexLayoutDesc), BinaryVS,
      BinaryVSLength, @FInputLayout)) then
    begin
      FVertexShader := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  PushClearFPUState;
  try
    hr := FContext.Device.CreatePixelShader(BinaryPS, BinaryPSLength, nil, @FPixelShader);
    if Failed(hr) then
    begin
      FInputLayout := nil;
      FVertexShader := nil;
      Exit(False);
    end;
  finally
    PopFPUState;
  end;

  FInitialized := True;
  Result := True;
end;

procedure TDX11ShaderEffect.Finalize;
begin
  if not FInitialized then
    Exit;

  FPixelShader := nil;
  FInputLayout := nil;
  FVertexShader := nil;
  FInitialized := False;
end;

function TDX11ShaderEffect.Activate: Boolean;
begin
  if (not FInitialized) or (FContext = nil) or (FContext.Context = nil)  then
    Exit(False);

  PushClearFPUState;
  try
    FContext.Context.IASetInputLayout(FInputLayout);
    FContext.Context.VSSetShader(FVertexShader, nil, 0);
    FContext.Context.PSSetShader(FPixelShader, nil, 0);
  finally
    PopFPUState;
  end;

  Result := True;
end;

procedure TDX11ShaderEffect.Deactivate;
begin
  if (not FInitialized) or (FContext = nil) or (FContext.Context = nil)  then
    Exit;

  PushClearFPUState;
  try
    FContext.Context.PSSetShader(nil, nil, 0);
    FContext.Context.VSSetShader(nil, nil, 0);
    FContext.Context.IASetInputLayout(nil);
  finally
    PopFPUState;
  end;
end;

{$ENDREGION}

end.
