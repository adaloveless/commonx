unit NamedCommandList;

interface

uses
  tickcount, betterobject, sharedobject, managedthread, commandprocessor,orderlyinit,
  sysutils, stringx, generics.collections.fixed;

type
  TNamedCommandList = class(TsharedObject)
  protected
    list: TList<TCommand>;
  private
    FCommandHoldtime: cardinal;
  protected
    procedure Init;override;
    destructor Destroy;override;
    procedure ClearCommands;
    function IndexOf(sKey: string): nativeint;
    function Findcommand(sKey: string): TCommand;
  public
    function NeedCommand<T: TCommand>(sKey: string): T;
    procedure ProvideCommand(c: TCommand);
    procedure NoNeedCommand(sKey: string);
    property CommandHoldTime: cardinal read FCommandHoldtime write FCommandHoldtime;
    procedure CombStaleCommands;
  end;

var
  GNC: TNamedCommandList;

implementation

{ TNamedCommandList }

procedure TNamedCommandList.ClearCommands;
begin
  Lock;
  try
    while list.count > 0 do begin
      list[0].free;
      list.delete(0);
    end;
  finally
    Unlock;
  end;

end;

procedure TNamedCommandList.CombStaleCommands;
var
  t: integer;
begin
  lock;
  try
    for t:= list.Count-1 downto 0 do begin
      if list[t].IsComplete then begin
        if gettimesince(list[t].TimeOfCompletion) > CommandHoldTime then begin
          if list[t].RefCount <=0 then begin
            list[t].Free;
            list.Delete(t);
          end;
        end;
      end;
    end;
  finally
    Unlock;
  end;


end;

destructor TNamedCommandList.Destroy;
begin
  ClearCommands;
  list.free;
  inherited;
end;

function TNamedCommandList.Findcommand(sKey: string): TCommand;
var
  i: nativeint;
begin
  result := nil;
  lock;
  try
    i := IndexOf(sKey);
    if i>=0 then
      result := list[i];

  finally
    Unlock;
  end;
end;

function TNamedCommandList.IndexOf(sKey: string): nativeint;
var
  t: integer;
begin
  result := -1;
  Lock;
  try
    for t:= 0 to list.Count-1 do begin
      if list[t].Name = sKey then begin
        result := t;
        break;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TNamedCommandList.Init;
begin
  inherited;
  list := TList<TCommand>.create;

end;


function TNamedCommandList.NeedCommand<T>(sKey: string): T;
var
  i: nativeint;
begin
  Lock;
  try
    i := IndexOf(sKey);
    if i < 0 then
      result := T.create
    else begin
      result := T(list[i]);
      result.addRef;
    end;


    CombStaleCommands;

  finally
    Unlock;
  end;


end;


procedure TNamedCommandList.NoNeedCommand(sKey: string);
var
  c: TCommand;
begin
  Lock;
  try
    c := FindCommand(sKey);
    if c <> nil then begin
      c.releaseRef;
      CombStalecommands;
    end;
  finally
    Unlock;
  end;
end;

procedure TNamedCommandList.ProvideCommand(c: TCommand);
begin
  Lock;
  try
    c.AddRef;
    list.Add(c);
    CombStaleCommands;
  finally
    Unlock;
  end;
end;

procedure oinit;
begin
  GNC := TNamedCommandList.Create;

end;

procedure ofinal;
begin
  GNC.Free;
  GNC := nil;


end;

initialization
  init.RegisterProcs('NamedCommandList', oinit, ofinal);

finalization



end.
