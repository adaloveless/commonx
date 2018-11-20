unit PacketHelpers_VirtualDisk;

interface

uses
  virtualdisk_advanced, packet, systemx, typex, virtualdisk_status, virtualdiskparams;

procedure GetTVirtualDiskPayloadConfigurationFromPacket(packet: TRDTPPacket; out td: TVirtualDiskPayloadConfiguration);

procedure WriteTVirtualDiskPayloadConfigurationToPacket(packet: TRDTPPacket; td: TVirtualDiskPayloadConfiguration);

procedure GetPVirtualDiskPayloadConfigurationFromPacket(packet: TRDTPPacket; out td: PVirtualDiskPayloadConfiguration);

procedure WritePVirtualDiskPayloadConfigurationToPacket(packet: TRDTPPacket; td: PVirtualDiskPayloadConfiguration);

procedure WriteTVirtualDiskStatusListToPacket(packet: TRDTPPacket; td: TVirtualDiskStatusList);
procedure GetTVirtualDiskStatusListFromPacket(packet: TRDTPPacket; out td: TVirtualDiskStatusList);

procedure WritePVirtualDiskStatusToPacket(packet: TRDTPPacket; td: PVirtualDiskStatus);
procedure GetPVirtualDiskStatusFromPacket(packet: TRDTPPacket; out td: TVirtualDiskStatus);

procedure WriteTNewDiskParamsToPacket(packet: TRDTPPacket; params: TNewDiskPArams);
procedure GetTNewDiskParamsFromPacket(packet: TRDTPPacket; out params: TNewDiskPArams);







implementation


procedure GetPVirtualDiskPayloadConfigurationFromPacket(packet: TRDTPPacket; out td: PVirtualDiskPayloadConfiguration);
var
  i: int64;
  p: pointer;
  ptd: pointer;
begin
  p := nil;
  try
    p := packet.SeqReadBytes(i);
    //allocate FULL structure
    ptd := GetMemory(sizeof(TVirtualDiskPayloadConfiguration));
    //zero out full struction
    fillmem(ptd, sizeof(TVirtualDiskPayloadConfiguration), 0);
    //move partial structure to full structure
    systemx.MoveMem32(ptd, p, i);
    //free the partial structure
    freememory(p);
  finally
    td := ptd;
  end;

end;


procedure GetTVirtualDiskPayloadConfigurationFromPacket(packet: TRDTPPacket; out td: TVirtualDiskPayloadConfiguration);
var
  i: int64;
  p: pointer;
begin
  p := nil;
  try
    fillmem(Pbyte(@td), sizeof(td), 0);
    p := packet.SeqReadBytes(i);
    systemx.MoveMem32(@td, p, i);
  finally
    if p <> nil then
      FreeMem(p);
  end;

end;
procedure WriteTVirtualDiskPayloadConfigurationToPacket(packet: TRDTPPacket; td: TVirtualDiskPayloadConfiguration);
begin
  packet.AddBytes(Pbyte(@td), td.GetMarhshalSize);
end;

procedure WritePVirtualDiskPayloadConfigurationToPacket(packet: TRDTPPacket; td: PVirtualDiskPayloadConfiguration);
begin
  packet.AddBytes(PByte(td), td.GetMarhshalSize);
end;

procedure WriteTVirtualDiskStatusListToPacket(packet: TRDTPPacket; td: TVirtualDiskStatusList);
var
  t: ni;
begin
  packet.AddInt(length(td));
  for t:= 0 to length(td)-1 do begin
    WritePVirtualDiskStatusToPacket(packet, @td[t]);
  end;
end;

procedure GetTVirtualDiskStatusListFromPacket(packet: TRDTPPacket; out td: TVirtualDiskStatusList);
var
  iCount: ni;
  t: ni;
  pv: TVirtualDiskStatus;
begin
  iCount := packet.SeqRead;
  setlength(td, iCount);
  for t:= 0 to iCount-1 do begin
    GetPVirtualDiskStatusFromPacket(packet, pv);
    td[t] := pv;
  end;


end;

procedure WritePVirtualDiskStatusToPacket(packet: TRDTPPacket; td: PVirtualDiskStatus);
var
  tv: TVirtualDiskStatus;
begin
  packet.AddBytes(PByte(td), sizeof(TVirtualDiskStatus));
end;

procedure GetPVirtualDiskStatusFromPacket(packet: TRDTPPacket; out td: TVirtualDiskStatus);
var
//  tv: TVirtualDiskStatus;
  p: pbyte;
  icount: int64;
begin
  p := nil;
  p := packet.SeqReadBytes(iCount);
  if iCount <> sizeof(TVirtualDiskStatus) then begin
    raise Ecritical.create('Read bytes from packet didn''t match size of TVirtualDiskStatus');
  end;

  movemem32(@td, p, sizeof(TVirtualDiskStatus));

  if p <> nil then
    freememory(p);




end;

procedure WriteTNewDiskParamsToPacket(packet: TRDTPPacket; params: TNewDiskPArams);
begin
  packet.AddString(params.Name);
  packet.AddString(params.SourceArchive);
  packet.AddString(params.TargetArchive);
  packet.AddString(params.SourceArchiveHost);
  packet.AddString(params.TargetArchiveHost);
  packet.AddInt(params.SourceArchivePinId);
  packet.AddInt(params.MaxRaidSpan);
  packet.AddBoolean(params.LazySourceFetch);
  packet.addboolean(params.LocalRedundancy);
  packet.AddInt(params.DiskSize);
end;

procedure GetTNewDiskParamsFromPacket(packet: TRDTPPacket; out params: TNewDiskPArams);
begin
  params.Name := packet.seqread;
  params.SourceArchive := packet.seqread;
  params.TargetArchive := packet.seqread;
  params.SourceArchiveHost := packet.seqread;
  params.TargetArchiveHost := packet.seqread;
  params.SourceArchivePinId := packet.seqread;
  params.MaxRaidSpan := packet.seqread;
  params.LazySourceFetch := packet.seqread;
  params.LocalRedundancy := packet.seqread;
  params.DiskSize := packet.seqread;
end;






end.
