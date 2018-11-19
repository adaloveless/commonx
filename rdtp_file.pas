unit rdtp_file;

interface

uses
  classes, typex, types;

type
  TRemoteFileRec = record
    name: string;
    attributes: integer;
    date: TDateTime;
    path: string;
    size: int64;
  end;

  TRemoteFileArray = TArray<TRemoteFileRec>;


implementation

end.
