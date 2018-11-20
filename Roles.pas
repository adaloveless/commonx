unit Roles;

interface

uses
  templates, RequestInfo, Dataobject, MTDTInterface, variants, sysutils, webresource;

procedure WRQ_Roles(rqInfo: TrequestInfo);
procedure WRQ_Role_Add(rqInfo: TRequestInfo);
procedure WRQ_Role_Edit(rqInfo: TRequestInfo);
procedure WRQ_POST_Client(rqInfo: TRequestInfo);
procedure WRQ_Client_Delete(rqInfo: TrequestINfo);



implementation

//------------------------------------------------------------------------------
procedure WRQ_Roles(rqInfo: TrequestInfo);
begin
  rqINfo.response.objectpool['roles'] := Query(rqInfo,'SELECT * from Role', true);

  LoadWebResourceAndMergeWithBestTemplate(rqInfo, 'roles.html','');

end;
//------------------------------------------------------------------------------
procedure WRQ_Role_Add(rqInfo: TRequestInfo);
begin
//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
procedure WRQ_Role_Edit(rqInfo: TRequestInfo);
begin
//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
procedure WRQ_POST_Client(rqInfo: TRequestInfo);
begin
//TODO -cunimplemented: unimplemented block
end;
//------------------------------------------------------------------------------
procedure WRQ_Client_Delete(rqInfo: TrequestINfo);
begin
//TODO -cunimplemented: unimplemented block
end;




end.
