unit FormDataAware;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FormBase, ExtCtrls, StdCtrls, FMTBcd, DB, SqlExpr, datalink, componentdatahelper, mysqlstoragestring;



type
  TfrmDataAware = class(TfrmBase)
  private
    FOnConnectToDAta: TNotifyEvent;
    FNewMode: boolean;
    FOnSaveData: TNotifyEvent;
    FOnCancelData: TNotifyEvent;
    FOnDisconnectFromData: TNotifyEvent;
    { Private declarations }
    FDataLinks: array of TDataLink;
    FDataLinkCount: integer;
    FOnLoadData: TNotifyEvent;
    function GetDataLinkCount: integer;
    function GetDataLinks(idx: integer): PDataLink;

  public
    { Public declarations }
    constructor create(AOwner: TComponent);override;
    function ShowForNew: TModalResult;virtual;
    function ShowForEdit: TModalResult;virtual;
    procedure DoConnectToData;virtual;

    procedure DoSaveData;virtual;
    procedure DoCancelData;virtual;
    procedure DoDisConnectFromData;virtual;
    procedure DoLoadData;virtual;
    property DataLinks[idx: integer]: PDataLink read GetDataLinks;
    procedure AddDataLink(varType: integer; ds: TDataSet; sTableName: ansistring; sField: ansistring; obj: TComponent);
    property DataLinkCount: integer read GetDataLinkCount;
    procedure LoadDataLinks;
    procedure SaveDataLinks;
    procedure WipeDataLinks;
    function GenerateSaveQueries: TStringlist;


  published
    property NewMode: boolean read FNewMode write FNewMode;
    property OnConnectToData: TNotifyEvent read FOnConnectToDAta write FOnConnectToDAta;
    property OnLoadData: TNotifyEvent read FOnLoadData write FOnLoadData;
    property OnSaveData: TNotifyEvent read FOnSaveData write FOnSaveData;
    property OnCancelData: TNotifyEvent read FOnCancelData write FOnCancelData;
    property OnDisconnectFromData: TNotifyEvent read FOnDisconnectFromData write FOnDisconnectFromData;

  end;



var
  frmDataAware: TfrmDataAware;

implementation

{$R *.dfm}


uses ProgressForm;





constructor TfrmDataAware.create(AOwner: TComponent);
begin
  inherited;

end;

procedure TfrmDataAware.DoCancelData;
begin
  beginProgress;
  try
    ShowProgress('Please wait...', 0, 1,0);
    if Assigned(OnCancelDAta) then
      OnCancelDAta(self);
  finally
    EndProgress;
  end;

  DoDisconnectFromData;

end;

procedure TfrmDataAware.DoConnectToDAta;
begin
  BeginProgress;
  try
    ShowProgress('Please wait...', 0, 1,0);
    if Assigned(OnConnectToDAta) then
      OnConnectToDAta(self);
  finally
    endProgress;
  end;
end;

procedure TfrmDataAware.DoDisConnectFromData;
begin
  BeginProgress;
  try
    ShowProgress('Please wait...', 0, 1,0);
    if Assigned(OnDisconnectFromData) then
      OnDisconnectFromData(self);
  finally
    endProgress;
  end;
end;

procedure TfrmDataAware.DoLoadData;
begin
  BeginProgress;
  try
    ShowProgress('Please wait...', 0, 1,0);
    LoadDataLinks;
    if Assigned(OnLoadDAta) then
      OnLoadDAta(self);
  finally
    endProgress;
  end;

  DoDisconnectFromData;

end;

procedure TfrmDataAware.DoSaveData;
begin
  BeginProgress;
  try
    ShowProgress('Please wait...', 0, 1,0);
    SaveDataLinks;
    if Assigned(OnSaveDAta) then
      OnSaveDAta(self);
  finally
    endProgress;
  end;

  DoDisconnectFromData;

end;

function TfrmDataAware.ShowForEdit: TModalResult;
begin
  NewMode := false;
  DoLoadData;
  result := ShowModal;
end;

function TfrmDataAware.ShowForNew: TModalResult;
begin
  NewMode := true;
  DoLoadData;
  result := ShowModal;
end;


procedure TfrmDataAware.WipeDataLinks;
var
  t: integer;
  pd: PDataLInk;
begin
  for t := 0 to DataLInkCount-1 do begin
    pd := datalinks[t];
    SetValueToComponent(pd.Component, '');

  end;

end;

procedure TfrmDataAware.AddDataLink(varType: integer; ds: TDataSet; sTableName: ansistring; sField: ansistring;
  obj: TComponent);
var
  pd: PDatalink;
begin
  SetLength(FDataLinks, length(FDataLInks)+1);
  pd := @FDataLinks[length(Fdatalinks)-1];
  pd.VarType:= varType;
  pd.TableName := sTableName;
  pd.DataSet := ds;
  pd.FieldName := sField;
  pd.Component := obj;
end;


function TfrmDataAware.GenerateSaveQueries: TStringlist;
var
  t,u,v: integer;
  s: ansistring;
  sFields: ansistring;
  slTables: TStringlist;
  pd: PDataLInk;
begin
  result := TStringList.create;

  slTables := TStringlist.create;
  slTables.Duplicates := dupIgnore;
  try
    //make list of all tables
    for t:= 0 to DataLinkCount-1 do begin
      pd := datalinks[u];
      if slTables.IndexOf(lowercase(pd.TableName)) < 0 then
        slTables.add(lowercase(pd.TableName));
    end;

    //for each table
    for t := 0 to slTables.count-1 do begin
      //generate a delete query
      for u := 0 to datalinkcount-1 do begin
        pd := datalinks[u];
        if lowercase(pd.tablename) = lowercase(slTables[t]) then begin
          s := 'delete from '+slTables[t]+' where '+pd.FieldName+'='+GetStorageString(pd.vartype, GetValueFromComponent(pd.Component));
          break;
        end;

      end;
      result.add(s);

      //generate an insert query
      sFields := '';
      s := 'insert into '+slTables[t]+' set ';
      for u := 0 to datalinkcount-1 do begin
        pd := datalinks[u];
        if lowercase(pd.tablename) = lowercase(slTables[t]) then begin
          if sFields <> '' then
            sFields := sFields+',';

          sFields := sFields+pd.FieldName+'='+GetStorageString(pd.vartype, GetValueFromComponent(pd.Component));
        end;
      end;
      result.add(s+sFields);



    end;

  finally
    slTables.free;
  end;
end;


function TfrmDataAware.GetDataLinkCount: integer;
begin
  result := length(FDataLinks);
end;

function TfrmDataAware.GetDataLinks(idx: integer): PDataLink;
begin
  result := @FDatalinks[idx];
end;

procedure TfrmDataAware.LoadDataLinks;
var
  t: integer;
  pd: PDataLink;
begin
  for t := 0 to datalinkcount-1 do begin
    pd := datalinks[t];

    SetValueToComponent(pd.Component, pd.DataSet.FieldByName(pd.FieldName).AsString );
//    := GetValueFromComponent(pd.Component);
  end;

end;

procedure TfrmDataAware.SaveDataLinks;
var
  t: integer;
  pd: PDataLink;
begin
  for t := 0 to datalinkcount-1 do begin
    pd := datalinks[t];

    pd.DataSet.FieldByName(pd.FieldName).AsString := GetValueFromComponent(pd.Component);
  end;

end;

end.
