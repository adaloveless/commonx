unit RDTPLightShowServerImplib;
{GEN}
{TYPE IMPLIB}
{RQFILE RDTPLightShowRQs.txt}
{END}
interface
uses RDTPLightShowServer, classes, systemx, sharedobject, windows, messages, packethelpers, sysutils, stringx;

type
  TLightshowServerSharedData = class(TSharedObject)
  public
    FileSync: TStringlist;
    QueueSync: TStringlist;
    ShitToQueue: string;
    Remove: boolean;
    Msg: boolean;
    procedure Init;override;
    destructor Destroy;override;
    procedure ClearMessage;
    procedure WaitForMessage;
  end;

  TLightShowServer = class(TLightShowServerBase)
  private
  protected
  public
    constructor Create;override;
    destructor Destroy;override;
{INTERFACE_START}
    function RQ_GetFiles():TStringList;overload;override;
    function RQ_GetQueue():TStringList;overload;override;
    procedure RQ_QueueItem(sName:string);overload;override;
    procedure RQ_NextItem();overload;override;
    procedure RQ_Pause();overload;override;
    procedure RQ_Rewind();overload;override;
    function RQ_GetRemoteData():TRemoteData;overload;override;
    procedure RQ_UnQueueItem(iIndex:integer);overload;override;
    procedure RQ_QueueMoveUp(iIndex:integer);overload;override;
    procedure RQ_QueueMoveDown(iIndex:integer);overload;override;

{INTERFACE_END}
  end;

var
  lsd: TLightshowServersharedData;
implementation
{ TLightShowServer }

uses
  FormPlaybackTest;

constructor TLightShowServer.Create;
begin
  inherited;
end;

destructor TLightShowServer.Destroy;
begin
  inherited;
end;

function TLightShowServer.RQ_GetFiles: TStringList;
var
  slTemp: TStringlist;
begin

  result := TStringlist.create;
  lsd.Lock;
  try
    result := CopyStringList(lsd.FileSync);
  finally
    lsd.Unlock;
  end;
end;

function TLightShowServer.RQ_GetQueue: TStringList;
var
  slTemp: TStringlist;
begin

  result := TStringlist.create;
  lsd.Lock;
  try
    result := CopyStringList(lsd.QueueSync);
  finally
    lsd.Unlock;
  end;
end;

function TLightShowServer.RQ_GetRemoteData: TRemoteData;
begin
  result := frmDmx.RemoteData;
end;

procedure TLightShowServer.RQ_NextItem;
begin
  inherited;
    PostMessage(frmDmx.Handle, WM_QUEUE_NEXT, 0,0);
end;

procedure TLightShowServer.RQ_Pause;
begin
  inherited;

end;

procedure TLightShowServer.RQ_QueueItem(sName: string);
begin
  inherited;
  lsd.Lock;
  try
    lsd.ShitToQueue := sName;
    lsd.msg := true;
    lsd.Remove := false;
    PostMessage(frmDmx.Handle, WM_REMOTE_MSG, 0,0);

  finally
    lsd.Unlock;
  end;

  lsd.WaitForMessage;
end;

procedure TLightShowServer.RQ_QueueMoveDown(iIndex: integer);
begin
  inherited;
  PostMessage(frmDmx.Handle, WM_QUEUE_DOWN, iIndex, 0);

end;

procedure TLightShowServer.RQ_QueueMoveUp(iIndex: integer);
begin
  inherited;
  PostMessage(frmDmx.Handle, WM_QUEUE_UP, iIndex, 0);
end;

procedure TLightShowServer.RQ_Rewind;
begin
  inherited;

end;

procedure TLightShowServer.RQ_UnQueueItem(iIndex: integer);
begin
  inherited;
  lsd.Lock;
  try
    lsd.ShitToQueue := inttostr(iIndex);
    lsd.Remove := true;
    lsd.msg := true;
    PostMessage(frmDmx.Handle, WM_REMOTE_MSG, 0,0);

  finally
    lsd.Unlock;
  end;

  lsd.WaitForMessage;

end;

{ TLightshowServerSharedData }

procedure TLightshowServerSharedData.ClearMessage;
begin
  Msg := false;
end;

destructor TLightshowServerSharedData.Destroy;
begin
  fileSync.Free;
  QueueSync.free;
  inherited;
end;

procedure TLightshowServerSharedData.Init;
begin
  inherited;
  fileSync := TStringlist.Create;
  QueueSync := TStringlist.Create;

end;

procedure TLightshowServerSharedData.WaitForMessage;
begin
  while msg do begin
    sleep(30);
  end;
end;

initialization
  lsd := TLightshowServerSharedData.create;

finalization
  lsd.Free;
  lsd := nil;


end.
