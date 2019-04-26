unit RDTP_DataBaseConnection;

interface

{$IFNDEF NO_UNIDAC}
uses
  uni, //NOTE, I Highly recommend UniDAC for database connections
       //.. this is a COMMERCIAL product that is quite expensive but quite good...
       //If you don't want to use UniDac:
       //DEFINE NO_UNIDAC to use the Delphi-bundled stuff
       //but have fun troubleshooting your MYSQL connections with
       //that UTTER GARBAGE
  UNIDACRDTPDataModule;

type
  TBestDatabaseDM = TUniDacRDTPDataModule;
{$ELSE}

uses
  MYSQLRDTPDataModule;

type
  TBestDatabaseDM = TMYSQLRDTPDataModule;

{$ENDIF}










implementation

end.
