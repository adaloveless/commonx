unit BrainScanWindowsGUI;

interface

uses GUIHelpers, windows, controls, comctrls, bigbrainultra, sysutils;

procedure MemList(lv: TListView; lvRef: TListView);


implementation


procedure MemList(lv: TListView; lvRef: TListView);
var
  t: integer;
  bm: TBlockManager;
  li: TlistItem;
  id: cardinal;
  u: nativeint;
begin
  SyncListView(lv, ManMan.ManagerCount,8);//sync once to get in the ballpark

  manman.Lock;
  try
    SyncListView(lv, ManMan.ManagerCount, 8);//sync again under lock. should be already in the ballpark

    for t:= 0 to ManMan.managercount-1 do begin

      bm := manman.Managers[t];
      if bm is TThreadBlockManager then
        id := TThreadBlockManager(bm).LastOwnedThreadID
      else
        id := 0;

      li := lv.items[t];
      if bm.TryLock then
      try
        li.caption := inttostr(id);
        li.subitems[0] := inttostr(bm.USedBytes);
        li.subitems[1] := inttostr(bm.FreeBytes);
        li.subitems[2] := inttostr(bm.AllocBytes);
        li.subitems[3] := inttostr(bm.WasteBytes);
        li.subitems[4] := inttostr(bm.BlockCount);
        if bm.BlockCount> 0 then begin
          li.subitems[5] := inttostr(bm.AllocBytes div bm.BlockCount);
        end else begin
          li.subitems[5] := '0';
        end;

        li.SubItems[6] := '';
        li.SubItems[7] := '';
        if lvRef <> nil then
        for u := 0 to lvRef.items.count- 1do begin
          if lvRef.items[u].caption = li.caption then begin
            li.SubItems[6] := lvRef.items[u].subitems[0];
            li.SubItems[7] := lvRef.items[u].subitems[1];
            li.ImageIndex := lvRef.items[u].ImageIndex;
          end;
        end;





      finally
        bm.Unlock;
      end;
    end;

  finally
    manman.Unlock;
  end;

end;


end.
