unit guihelpers_fmx;

interface


uses
  FMX.ListView, FMX.ListView.Appearances, FMX.ListView.Types, typex;


function listview_GetSelectedAppearance(lv: TListView): TlistViewItem;





implementation

function listview_GetSelectedAppearance(lv: TListView): TlistViewItem;
begin
  result := lv.selected as TListViewItem;




end;

end.
