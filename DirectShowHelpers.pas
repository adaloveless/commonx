unit DirectShowHelpers;

interface

uses
  directshow9, activex, windows, generics.collections.fixed, interfacelist, sysutils;

type
  TFilterList = class(TinterfaceList)
  public
    constructor Create(Graph: IGraphBuilder); reintroduce; virtual;

  end;

function AddFilter(pGraph: IGraphBuilder; GUID: TGUID; Name: WideString;
  out Filter: IBasefilter): HResult;

function GetUnconnectedPin(pFilter: IBasefilter; PinDir: TPinDirection;
  out ppPin: IPin): HResult;overload;

function GetUnconnectedPin(pFilter: IBaseFilter; PinDir: TPinDirection): IPin;overload;

function GetConnectedPin(pFilter: IBasefilter; PinDir: TPinDirection;
  out ppPin: IPin): HResult;overload;

function GetConnectedPin(pFilter: IBaseFilter; PinDir: TPinDirection): IPin;overload;


function ConnectToSound(pGraph: IGraphBuilder;  AudioFilter: IBasefilter): HResult;
procedure ConnectGraphPins(pGraph: IGraphBuilder; pOut, pIn: IPin);
function COMAssert(hr: HRESULT): boolean;
function GetfilterFromGraph(pGraph: IGRaphBuilder; classid: TGUID): IBaseFilter;

implementation

// ------------------------------------------------------------------------------
function GetfilterFromGraph(pGraph: IGRaphBuilder; classid: TGUID): IBasefilter;
var
  poop: IEnumfilters;
  fil: IBaseFilter;
  cmp: TGuid;
begin
  result := nil;
  pGraph.EnumFilters(poop);

  while Succeeded(poop.Next(1, fil, nil)) do begin
    if fil = nil then exit;
    if COMAssert(fil.GetClassID(cmp)) then begin

      if CompareMem(@cmp, @classid, sizeof(cmp)) then begin
        result := fil;
        exit;
      end;
    end;
  end;


end;



function COMAssert(hr: HRESULT): boolean;
begin
  result := hr = S_OK ;

  if not result then begin
    raise Exception.Create('COM Assert Failure:'+inttostr(hr));
  end;
end;


procedure ConnectGraphPins(pGraph: IGraphBuilder; pOut, pIn: IPin);
begin
  if pin = nil then
    raise Exception.create('Input pin is nil');

  if pout = nil then
    raise Exception.create('Output pin is nil');

  if not Succeeded(pGraph.Connect(pOut, pIn)) then begin
    raise Exception.create('Shit blew up');
  end;

end;

function AddFilter(pGraph: IGraphBuilder; GUID: TGUID; Name: WideString;
  out Filter: IBasefilter): HResult;
var
  hr: HResult;
begin
  Result := S_FALSE;
  if not Assigned(pGraph) then
    exit;

  hr := CoCreateInstance(GUID, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter,
    Filter);

  if SUCCEEDED(hr) then begin
    hr := pGraph.AddFilter(Filter, PWideChar(Name));
    if Failed(hr) then
      Filter._Release;
  end;
  Result := hr;
end;


function GetConnectedPin(pFilter: IBaseFilter; PinDir: TPinDirection): IPin;overload;
begin
  if not succeeded(GetConnectedPin(pfilter, PinDir, result)) then begin
    result := nil;
  end;
end;


function GetUnconnectedPin(pFilter: IBaseFilter; PinDir: TPinDirection): IPin;overload;
begin
  if not succeeded(GetunconnectedPin(pfilter, PinDir, result)) then begin
    result := nil;
  end;
end;

function GetUnconnectedPin(pFilter: IBasefilter; PinDir: TPinDirection;
  out ppPin: IPin): HResult;
var
  pEnum: IEnumPins;
  hr: HResult;
  pPin, pTmp: IPin;
  ThisPinDir: PIN_DIRECTION;
  found: boolean;
begin
  found := false;
  ppPin := nil;
  pEnum := nil;
  pPin := nil;
  hr := pFilter.EnumPins(pEnum);
  if Failed(hr) then begin
    Result := hr;
    exit;
  end;
  while COMAssert(pEnum.Next(1, pPin, NIL)) do begin
    pPin.QueryDirection(ThisPinDir);
    if (ThisPinDir = PinDir) then begin
      pTmp := nil;
      hr := pPin.ConnectedTo(pTmp);
      if SUCCEEDED(hr) then // Already connected, not the pin we want.
        pTmp := nil
      else // Unconnected, this is the pin we want.
        begin
        found := true;
        ppPin := pPin;
        break;
      end;
    end;
    pPin := nil;
  end;
  pEnum := nil;
  if found then
    Result := S_OK
  else
    Result := E_FAIL;
end;

//------------------------------------------------------------------------------
function GetConnectedPin(pFilter: IBasefilter; PinDir: TPinDirection;
  out ppPin: IPin): HResult;
var
  pEnum: IEnumPins;
  hr: HResult;
  pPin, pTmp: IPin;
  ThisPinDir: PIN_DIRECTION;
  found: boolean;
begin
  found := false;
  ppPin := nil;
  pEnum := nil;
  pPin := nil;
  hr := pFilter.EnumPins(pEnum);
  if Failed(hr) then begin
    Result := hr;
    exit;
  end;
  while COMAssert(pEnum.Next(1, pPin, NIL)) do begin
    pPin.QueryDirection(ThisPinDir);
    if (ThisPinDir = PinDir) then begin
      pTmp := nil;
      hr := pPin.ConnectedTo(pTmp);
      if SUCCEEDED(hr) then // Already connected, not the pin we want.
        pTmp := nil
      else // Unconnected, this is the pin we want.
        begin
        found := true;
        ppPin := pPin;
        break;
      end;
    end;
    pPin := nil;
  end;
  pEnum := nil;
  if found then
    Result := S_OK
  else
    Result := E_FAIL;
end;


// ------------------------------------------------------------------------------
function ConnectToSound(pGraph: IGraphBuilder;
  AudioFilter: IBasefilter): HResult;
var
  FL: TFilterList;
// PL : TPinList;
  hr: HResult;
  i: integer;
  iP, oP: IPin;
  pEnum: IEnumPins;
  ThisPinDir: PIN_DIRECTION;
  pTmp: IPin;
begin
  hr := GetUnconnectedPin(AudioFilter, PINDIR_INPUT, iP);
  if Failed(hr) then begin
    Result := hr;
    exit;
  end;
  FL := TFilterList.Create(pGraph);
  try
    for i := 0 to FL.Count - 1 do
      if FL[i] <> AudioFilter then begin
        hr := IBaseFilter(FL[i]).EnumPins(pEnum);
        if windows.SUCCEEDED(hr) then begin
          while pEnum.Next(1, oP, NIL) = S_OK do begin
            oP.QueryDirection(ThisPinDir);
            if (ThisPinDir = PinDir_OUTPUT) then begin
              pTmp := nil;
              hr := oP.ConnectedTo(pTmp);
              if SUCCEEDED(hr) then // Already connected, not the pin we want.
                pTmp := nil
              else // Unconnected, this is the pin we want.
                begin
                hr := pGraph.Connect(oP, iP);
                if windows.SUCCEEDED(hr) then begin
                  FL.Free;
                  Result := hr;
                  exit;
                end
                else
                  oP := nil;
              end;
            end;
          end;
        end;
      end;
  finally
    FL.Free;
  end;
  Result := -1;
end;

{ TFilterList }

constructor TFilterList.Create(Graph: IGraphBuilder);
var
  en: IEnumfilters;
  fil: IBasefilter;
  iFetched: longint;
begin
  Graph.EnumFilters(en);

  repeat
    if not SUCCEEDED(en.Next(1, fil, @iFetched)) then
      break;

    if iFetched = 0 then
      break;

    self.Add(fil);

  until false;

end;

end.
