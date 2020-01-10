unit CommandListView;


interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ComCtrls;

type
  TCommandListView = class(TListView)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Digital Tundra', [TCommandListView]);
end;

end.
