unit UnicodeTextFileAdapter;

interface

uses
  typex, stringx, systemx, classes, betterobject, generics.collections;

type
  TUCTextFile = record
    filename: string;
    line: ni;
    h: IHolder<TStringList>;
    procedure AssignFile(sFile: string);
    procedure Reset;
    procedure Close;
    function ReadLn(out s: string): boolean;
    function eof: boolean;
  end;

  UCTextFile = TUCTextFile;


procedure UCAssignFile(out tf: TUCTextFile; sFileName: string);
procedure UCReset(var tf: TUCTextFile);
procedure UCCloseFile(var tf: TUCTextFile);
function UCReadLn(var tf: TUCTextFile; out s: string): boolean;
function UCEOF(var tf: TUCTextFile): boolean;







implementation

{ TUCTextFile }

procedure TUCTextFile.AssignFile(sFile: string);
begin
  h := THolder<TStringList>.create;
  h.o := TStringlist.create;
  h.o.loadfromfile(sFile);
end;

procedure TUCTextFile.Close;
begin
  h := nil;

end;

function TUCTextFile.eof: boolean;
begin
  result := line >= h.o.count;
end;

function TUCTextFile.ReadLn(out s: string): boolean;
begin
  if eof then
    exit(false);

  s := h.o[line];
  inc(line);
  result := true;


end;

procedure TUCTextFile.Reset;
begin
  line := 0;
end;

procedure UCAssignFile(out tf: TUCTextFile; sFileName: string);
begin
  tf.AssignFile(sFileName);
end;

procedure UCReset(var tf: TUCTextFile);
begin
  tf.Reset;
end;
procedure UCCloseFile(var tf: TUCTextFile);
begin
  tf.Close();
end;

function UCReadLn(var tf: TUCTextFile; out s: string): boolean;
begin
  result := tf.REadLn(s);
end;


function UCEOF(var tf: TUCTextFile): boolean;
begin
  result := tf.eof;
end;


end.

