unit helpers.JSON;

interface


uses
  better_JSON;

type
  TJSONHelper = class
  public
    class function TryGetValue<T>(jo: TJSONObject; sPath: string; out val: T): boolean;overload;
    class function TryGetValue<T>(jo: TJSONValue; sPath: string; out val: T): boolean;overload;
  end;



implementation

{ TJSONHelper }

class function TJSONHelper.TryGetValue<T>(jo: TJSONObject; sPath: string; out val: T): boolean;
var
  pair: TJSONPair;
begin
  result := false;
  if jo.Get(spath)<>nil then begin
    result := jo.TryGetValue<T>(sPath, val);
  end;
end;

class function TJSONHelper.TryGetValue<T>(jo: TJSONValue; sPath: string;
  out val: T): boolean;
begin
  result := false;
  if jo = nil then
    exit;

  result := jo.TryGetValue<T>(sPath, val);



end;

end.
