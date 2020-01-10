unit NamedCommandList;
{$DEFINE USE_HOLDER}

interface

uses
  tickcount, betterobject, sharedobject, managedthread, commandprocessor,orderlyinit,
  sysutils, stringx, generics.collections.fixed;

type
  TNamedCommandList = class(TsharedObject)
  protected
{$IFDEF USE_HOLDER}
    list: TList<IHolder<TCommand>>;
{$ELSE}
    list: TList<TCommand>;
{$ENDIF}
  private
    FCommandHoldtime: cardinal;
  protected
    procedure Init;override;
    destructor Destroy;override;
    procedure ClearCommands;
    function IndexOf(sKey: string): nativeint;
    function Findcommand(sKey: string): TCommand;
  public
    function NeedCommand<T: TCommand>(sKey: string): IHolder<TCommand>;
    procedure ProvideCommand(c: IHolder<TCommand>);
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
{$IFnDEF USE_HOLDER}
      list[0].free;
{$ENDIF}
      list.delete(0);
    end;
  finally
    Unlock;
  end;

end;

procedure TNamedCommandList.CombStaleCommands;
var
  t: integer;
  obj: TCommand;
begin
  lock;
  try
    for t:= list.Count-1 downto 0 do begin
{$IFDEF USE_HOLDER}
      obj := list[t].o;
{$ELSE}
      obj := list[t];
{$ENDIF}
      if list[t].o.IsComplete then begin
        if gettimesince(obj.TimeOfCompletion) > CommandHoldTime then begin
{$IFnDEF USE_HOLDER}
          if list[t].RefCount <=0 then begin
            obj.ReleaseRef;
            list.Delete(t);
          end;
{$ELSE}
        list.delete(t);
{$ENDIF}
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
      result := list[i].o;

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
      if list[t].o.Name = sKey then begin
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
  list := TList<IHolder<TCommand>>.create;

end;


function TNamedCommandList.NeedCommand<T>(sKey: string): IHolder<TCommand>;
var
  i: nativeint;
begin
  Lock;
  try
    i := IndexOf(sKey);
    if i < 0 then begin
      result := THolder<TCommand>.create;
      result.o := T.create;
    end else begin
      result := IHolder<TCommand>(list[i]);
{$IFnDEF USE_HOLDER}
      result.addRef;
{$ENDIF}
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

procedure TNamedCommandList.ProvideCommand(c: IHolder<TCommand>);
begin
  Lock;
  try
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
  init.RegisterProcs('NamedCommandList', oinit, ofinal, 'CommandProcessor,ManagedThread');

finalization



end.
