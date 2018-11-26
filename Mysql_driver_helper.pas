unit Mysql_driver_helper;
//For use with Mysql 5.1.xx

interface


function GetVendorLibForPlatform: string;
function GetDBXLibForPlatForm: string;
function GetDBXDriverFunction: string;


implementation

function GetDBXDriverFunction: string;
begin
  result := 'getSQLDriverMySQL'
end;


function GetDBXLibForPlatForm: string;
begin
  result := 'dbxmys.dll';
  exit;
  {$IF (CompilerVersion>=25.0) and (CompilerVersion<26.0)}
    {$IFDEF CPUX86}
      result := 'dbxmys.dll';
    {$ELSE}
      result := 'dbxmys_xe4_64.dll';
    {$ENDIF}
  {$ENDIF}

  {$IF (CompilerVersion>=24.0) and (CompilerVersion<25.0)}
    {$IFDEF CPUX86}
      //result := 'dbxmys_xe3_32.dll';
      result := 'dbxmys_xe3_32.dll';
    {$ELSE}
      result := 'dbxmys_xe3_64.dll';
    {$ENDIF}

  {$ENDIF}

end;

function GetVendorLibForPlatform: string;
begin
  result := 'libmysql.dll';
  exit;
  {$IF (CompilerVersion>=25.0) and (CompilerVersion<26.0)}
    {$IFDEF CPUX86}
      result := 'libmysql.dll';
    {$ELSE}
      result := 'libmysql.dll';
    {$ENDIF}
  {$ENDIF}

  {$IF (CompilerVersion>=24.0) and (CompilerVersion<25.0)}
    {$IFDEF CPUX86}
      result := 'libmysql.50.dll';
    {$ELSE}
      result := 'libmysql.50.dll';
    {$ENDIF}

  {$ENDIF}

end;

end.
