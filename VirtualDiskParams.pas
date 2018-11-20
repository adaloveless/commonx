unit VirtualDiskParams;

interface

type
  TNewDiskParams = record
    Name: string;
    SourceArchive: string;
    TargetArchive: string;
    SourceArchiveHost: string;
    TargetArchiveHost: string;
    SourceArchivePinId: int64;
    MaxRaidSpan: int64;
    LazySourceFetch: boolean;
    LocalRedundancy: boolean;
    DiskSize: int64;
  end;


implementation

end.
