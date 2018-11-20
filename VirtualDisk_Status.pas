unit VirtualDisk_Status;

interface

uses
  typex, systemx;

type
  TVirtualDiskStatus = packed record
  private
    function GetFileName: string;
    function GetStatus: string;
    procedure SetFileNAme(const Value: string);
    procedure SetStatus(const Value: string);
  public
    operational: boolean;
    MaxDriveSpan: byte;
    MinDriveSpan: byte;
    _filename: array[0..511] of char;
    _status: array[0..511] of char;
    property FileName: string read GetFileName write SetFileNAme;
    property Status: string read GetStatus write SetStatus;

  end;

  PVirtualDiskStatus = ^TVirtualDiskStatus;

  TVirtualDiskStatusList = array of TVirtualDiskStatus;

implementation


{ TVirtualDiskStatus }

function TVirtualDiskStatus.GetFileName: string;
begin
  result := _filename;
end;

function TVirtualDiskStatus.GetStatus: string;
begin
  result := _status;
end;

procedure TVirtualDiskStatus.SetFileNAme(const Value: string);
begin
  fillmem(pointer(@_filename[0]), sizeof(_filename), 0);
  movemem32(pbyte(@_filename[0]), pbyte(@value[STRZ]), length(value)*sizeof(char));
end;

procedure TVirtualDiskStatus.SetStatus(const Value: string);
begin
  fillmem(pbyte(@_status[0]), sizeof(_status), 0);
  movemem32(pbyte(@_status[0]), pbyte(@value[STRZ]), length(value)*sizeof(char));
end;

end.
