unit AdoConnHelper;

(*
 * Class helper for TAdoConnection (works only with Microsoft SQL Server)
 * by Daniel Marschall, ViaThinkSoft <www.viathinksoft.com>
 *
 * Revision: 23 September 2024
 * License: Apache 2.0
 *
 * Latest version here:
 * https://github.com/danielmarschall/delphiutils/blob/master/_Common/AdoConnHelper.pas
 *)

interface

uses
  DB, ADODB, Variants, Classes, SysUtils;

type
  TDatabaseFileType = (ftRows, ftLog);

type
  TAdoConnectionHelperForSqlServer = class helper for TADOConnection
  private
    function GetConnectionID: TGUID;
    function GetDatabaseName: string;
    function GetDbOwnerSid: string;
    function GetSqlServerMac: string;
    function SqlServerBestProvider: string;
  public
    function GetTable(const SQL: string; ATimeout: integer=-1): TADODataSet;
    function GetScalar(const SQL: string; ATimeout: integer=-1): Variant;
    function Any(const SQL: string; ATimeout: integer=-1): boolean;
    procedure ExecSQL(const SQL: string; ATimeout: integer=-1);
    procedure ExecSQLList(List: TStrings; ATimeout: integer=-1; AContinueOnError: boolean=false);

    procedure ConnectConnStr(const AConnectionString: string; AConnectionTimeout: integer=-1);
    procedure ConnectPwdAuth(const aServerName: string; const aDatabaseName: string; const aUserName: string; const aPassword: string; AConnectionTimeout: integer=-1);
    procedure ConnectNtAuth(const aServerName: string; const aDatabaseName: string; AConnectionTimeout: integer=-1);
    procedure Disconnect;

    class function SQLStringEscape(const str: string): string; static;
    class function SQLObjectNameEscape(const str: string; const schema: string='dbo'): string; static;
    class function SQLFieldNameEscape(const str: string): string; static;
    class function SQLDatabaseNameEscape(const str: string): string; static;

    procedure GetPrimaryKeyNames(const TableName: string; outsl: TStrings); // TODO: add argument aSchemaName

    property ConnectionID: TGUID read GetConnectionID;
    property DatabaseName: string read GetDatabaseName;
    property DbOwnerSid: string read GetDbOwnerSid;
    property SqlServerMac: string read GetSqlServerMac;

    function InsertAndReturnID(const query: string): integer;

    procedure DropTableOrView(const aTableName: string; const schema: string='dbo');
    procedure DropDatabase(const aDatabaseName: string);
    procedure DropColumn(const aTableName: string; const aColumnName: string; const schema: string='dbo');

    function FieldCount(const aTableName: string): integer; // TODO: add argument aSchemaName
    function IndexCount(const aTableName: string): integer; // TODO: add argument aSchemaName

    function TableExists(const aTableName: string; const aSchemaName: string='dbo'): boolean;
    function ViewExists(const aViewName: string; const aSchemaName: string='dbo'): boolean;
    function IndexExists(const aTableName: string; const aIndexName: string): boolean; // TODO: add argument aSchemaName
    function ColumnExists(const aTableName: string; const aColumnName: string): boolean; // TODO: add argument aSchemaName

    procedure ShrinkDatabase(const Datenbankname: string; typ: TDatabaseFileType);
    function SupportsBackupCompression: boolean;
  end;

implementation

uses
  Windows, StrUtils, Registry;

{ TAdoConnectionHelperForSqlServer }

function TAdoConnectionHelperForSqlServer.GetConnectionID: TGUID;
var
  s : string;
begin
  s := GetScalar('select connection_id from sys.dm_exec_connections ec ' +
                 'left join sys.dm_exec_sessions se on ec.session_id = se.session_id ' +
                 'where se.session_id = @@SPID');
  result := StringToGUID(s);
end;

procedure TAdoConnectionHelperForSqlServer.GetPrimaryKeyNames(const TableName: string; outsl: TStrings);
var
  ds: TADODataSet;
begin
  ds := TADODataSet.Create(nil);
  try
    OpenSchema(siPrimaryKeys, Unassigned, EmptyParam, ds);
    while not ds.Eof do
    begin
      if ds.FieldByName('TABLE_NAME').AsWideString = TableName then
      begin
        outsl.Add(ds.FieldByName('COLUMN_NAME').AsWideString);
      end;
      ds.Next;
    end;
    ds.Close;
  finally
    ds.Free;
  end;
end;

function TAdoConnectionHelperForSqlServer.GetScalar(const SQL: string; ATimeout: integer=-1): Variant;
var
  ds: TADODataSet;
begin
  ds := GetTable(SQL, ATimeout);
  try
    result := ds.Fields[0].AsVariant;
  finally
    ds.Free;
  end;
end;

function TAdoConnectionHelperForSqlServer.GetTable(const SQL: string; ATimeout: integer=-1): TADODataSet;
begin
  result := TADODataSet.Create(nil);
  result.Connection := Self;
  result.EnableBCD := false;
  result.ParamCheck := false;
  result.CommandType := cmdText;
  result.CommandText := SQL;
  result.DisableControls;
  if ATimeout <> -1 then
    result.CommandTimeout := ATimeout
  else
    result.CommandTimeout := self.CommandTimeout;
  result.Active := true;
end;

function TAdoConnectionHelperForSqlServer.Any(const SQL: string;
  ATimeout: integer): boolean;
var
  q: TAdoDataset;
begin
  q := GetTable(sql);
  try
    result := not q.EOF;
  finally
    q.Free;
  end;
end;

function TAdoConnectionHelperForSqlServer.ColumnExists(const aTableName: string; const aColumnName: string): boolean;
begin
  result := GetScalar('select count (*) from sys.columns where Name = N'+SQLStringEscape(aColumnName)+' and Object_ID = Object_ID(N'+SQLStringEscape(aTableName)+')') > 0;
end;

function TAdoConnectionHelperForSqlServer.IndexCount(const aTableName: string): integer;
begin
  result := GetScalar('select max (ik.indid) from sysindexkeys ik left join sysindexes ind on ind.id = ik.id and ind.indid = ik.indid where ik.id = (select id from sysobjects ' +
                      'where name = N'+SqlStringEscape(aTableName)+') and ind.status < 10000000');
end;

function TAdoConnectionHelperForSqlServer.IndexExists(const aTableName,
  aIndexName: string): boolean;
begin
  result := GetScalar('select count (*) ' +
                      'from sys.indexes ' +
                      'where name = N'+SqlStringEscape(aIndexName)+' and ' +
                      'Object_ID = Object_ID(N'+SqlStringEscape(aTableName)+')').AsInteger > 0;
end;

function TAdoConnectionHelperForSqlServer.InsertAndReturnID(const query: string): integer;
resourcestring
  LNG_NO_AUTOINC = 'Cannot find AutoIncrement value';
var
  q1: TADODataSet;
begin
  BeginTrans;
  try
    ExecSql(query);

    // Note: @@IDENTITY is only local and does not work with linked servers. Also, it might get disrupted by triggers.
    // q1 := GetTable('select @@IDENTITY as INSERT_ID;');
    q1 := GetTable('select SCOPE_IDENTITY() as INSERT_ID;');
    try
      if (q1.RecordCount = 0) or (q1.Fields[0{INSERT_ID}].AsInteger = 0) then
      begin
        //result := -1;
        raise EADOError.Create(LNG_NO_AUTOINC);
      end;

      result := q1.Fields[0{INSERT_ID}].AsInteger;
    finally
      q1.Free;
    end;
  finally
    CommitTrans;
  end;
end;

procedure TAdoConnectionHelperForSqlServer.ConnectConnStr(const AConnectionString: string; AConnectionTimeout: integer=-1);
begin
  Disconnect;
  ConnectionString := AConnectionString;
  if AConnectionTimeout <> -1 then ConnectionTimeout := AConnectionTimeout;
  Connected := true;
end;

function TAdoConnectionHelperForSqlServer.GetDatabaseName: string;
begin
  // Note: This also works if you have selected a different database using "use"
  result := DefaultDatabase;

  // Alternatively:
  // result := GetScalar('select db_name()');
end;

function TAdoConnectionHelperForSqlServer.GetDbOwnerSid: string;
begin
  // Attention: "sa" user has SID 0x01
  result := GetScalar('select CONVERT([varchar](100), owner_sid, 1) ' +
                      'from sys.databases ' +
                      'where name = N'+SqlStringEscape(DatabaseName)).AsWideString;
end;

procedure TAdoConnectionHelperForSqlServer.Disconnect;
begin
  Connected := false;
end;

procedure TAdoConnectionHelperForSqlServer.DropColumn(const aTableName,
  aColumnName: string; const schema: string='dbo');
begin
  if ColumnExists(aTableName, aColumnName) then
  begin
    ExecSQL('alter table ' + SQLObjectNameEscape(aTableName, schema) + ' drop column ' + SQLFieldNameEscape(aColumnName));
  end;
end;

procedure TAdoConnectionHelperForSqlServer.DropDatabase(const aDatabaseName: string);
begin
  if aDatabaseName = GetDatabaseName then
  begin
    try
      ExecSQL('use master');
    except
    end;
  end;
  ExecSql('drop database ' + SQLDatabaseNameEscape(aDatabaseName));
end;

procedure TAdoConnectionHelperForSqlServer.DropTableOrView(const aTableName: string; const schema: string='dbo');
begin
  if ViewExists(aTableName, schema) then
  begin
    ExecSql('drop view ' + SQLObjectNameEscape(aTableName, schema));
  end;
  if TableExists(aTableName, schema) then
  begin
    ExecSql('drop table ' + SQLObjectNameEscape(aTableName, schema));
  end;
end;

procedure TAdoConnectionHelperForSqlServer.ExecSQL(const SQL: string; ATimeout: integer=-1);
var
  cmd: TADOCommand;
begin
  cmd := TADOCommand.Create(nil);
  try
    cmd.Connection := Self;
    cmd.ParamCheck := false;
    cmd.CommandType := cmdText;
    cmd.CommandText := SQL;
    if ATimeOut <> -1 then
      cmd.CommandTimeout := ATimeout
    else
      cmd.CommandTimeout := self.CommandTimeout;
    cmd.Execute;
  finally
    cmd.Free;
  end;
end;

procedure TAdoConnectionHelperForSqlServer.ExecSQLList(List: TStrings; ATimeout: integer=-1; AContinueOnError: boolean=false);
var
  s: string;
begin
  for s in List do
  begin
    try
      ExecSQL(s, ATimeout);
    except
      if not AContinueOnError then
        raise;
    end;
  end;
end;

function TAdoConnectionHelperForSqlServer.FieldCount(const aTableName: string): integer;
begin
  result := GetScalar('select count(*) ' +
                      'from syscolumns ' +
                      'where id = (select id from sysobjects where name = N'+SQLStringEscape(aTableName)+')');
end;

class function TAdoConnectionHelperForSqlServer.SQLStringEscape(const str: string): string;
begin
  result := str;

  // Escape SQL-Argument
  (*
  result := StringReplace(result, '\', '\\', [rfReplaceAll]);
  result := StringReplace(result, '_', '\_', [rfReplaceAll]);
  result := StringReplace(result, '%', '\%', [rfReplaceAll]);
  result := StringReplace(result, '[', '\[', [rfReplaceAll]);
  result := StringReplace(result, '''', '\''', [rfReplaceAll]);
  *)

  // DM 29.02.2016 I don't get it...
  // 'xxx\'xxx' is allowed, but 'xxx\'xxx\'xxx' not
  // but 'xxx''xxx''xxx' works.
  result := StringReplace(result, '''', '''''', [rfReplaceAll]);

  // Prevent that SQL server thinks that it is a parameter object.
  // But we only need it if the query has ParamCheck=true.
  (*
  result := StringReplace(result, ':', '::', [rfReplaceAll]);
  *)

  result := 'N''' + result + '''';
end;

function TAdoConnectionHelperForSqlServer.SupportsBackupCompression: boolean;
var
  SqlEdition: string;
begin
  SqlEdition := GetScalar('select SERVERPROPERTY(''Edition'')');
  // ContainsText is important, because the Edition might also be called "Enterprise Evaluation"
  result := ContainsText(SqlEdition, 'Standard') or ContainsText(SqlEdition, 'Enterprise') or ContainsText(SqlEdition, 'Developer');
end;

class function TAdoConnectionHelperForSqlServer.SQLObjectNameEscape(const str: string; const schema: string='dbo'): string;
begin
  result := '[' + schema + '].[' + str + ']';
end;

threadvar
  _SqlServerProvider_Cache: string;

function TAdoConnectionHelperForSqlServer.SqlServerBestProvider: string;
var
  reg: TRegistry;
begin
  if _SqlServerProvider_Cache <> '' then
  begin
    result := _SqlServerProvider_Cache;
    exit;
  end;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;

    // More information about the SQL Server providers:
    // https://learn.microsoft.com/en-us/sql/connect/oledb/oledb-driver-for-sql-server?view=sql-server-ver16

    if reg.KeyExists('CLSID\{EE5DE99A-4453-4C96-861C-F8832A7F59FE}') then
    begin
      result := 'MSOLEDBSQL19'; // Generation 3, Version 19+
      _SqlServerProvider_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{5A23DE84-1D7B-4A16-8DED-B29C09CB648D}') then
    begin
      result := 'MSOLEDBSQL'; // Generation 3
      _SqlServerProvider_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{397C2819-8272-4532-AD3A-FB5E43BEAA39}') then
    begin
      result := 'SQLNCLI11'; // Generation 2
      _SqlServerProvider_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{0C7FF16C-38E3-11d0-97AB-00C04FC2AD98}') then
    begin
      result := 'SQLOLEDB'; // Generation 1
      _SqlServerProvider_Cache := result;
      exit;
    end;
  finally
    FreeAndNil(reg);
  end;

  result := 'SQLOLEDB'; // Fallback (should not happen)
  _SqlServerProvider_Cache := result;
end;

procedure TAdoConnectionHelperForSqlServer.ConnectPwdAuth(const aServerName: string;
  const aDatabaseName: string; const aUserName: string;
  const aPassword: string; AConnectionTimeout: integer);
var
  sqlConnStr: string;
  provider: string;
begin
  provider := SqlServerBestProvider;

  sqlConnStr := 'Provider='+provider+';';
  sqlConnStr := sqlConnStr + 'Application Name='+ExtractFileName(ParamStr(0))+';';

  sqlConnStr := sqlConnStr + 'User ID='+aUserName+';Password='+aPassword+';Persist Security Info=True;';

  if provider = 'MSOLEDBSQL19' then
    sqlConnStr := sqlConnStr + 'Use Encryption for Data=Optional;';

  sqlConnStr := sqlConnStr + 'Initial Catalog=' + aDatabaseName + ';';
  sqlConnStr := sqlConnStr + 'Data Source=' + aServerName;

  ConnectConnStr(sqlConnStr, AConnectionTimeout);
end;

procedure TAdoConnectionHelperForSqlServer.ConnectNtAuth(const aServerName: string;
  const aDatabaseName: string; AConnectionTimeout: integer);
var
  sqlConnStr: string;
  provider: string;
begin
  provider := SqlServerBestProvider;

  sqlConnStr := 'Provider='+provider+';';
  sqlConnStr := sqlConnStr + 'Application Name='+ExtractFileName(ParamStr(0))+';';

  sqlConnStr := sqlConnStr + 'Integrated Security=SSPI;Persist Security Info=False;';

  if provider = 'MSOLEDBSQL19' then
    sqlConnStr := sqlConnStr + 'Use Encryption for Data=Optional;';

  sqlConnStr := sqlConnStr + 'Initial Catalog=' + aDatabaseName + ';';
  sqlConnStr := sqlConnStr + 'Data Source=' + aServerName;

  ConnectConnStr(sqlConnStr, AConnectionTimeout);
end;

class function TAdoConnectionHelperForSqlServer.SQLFieldNameEscape(const str: string): string;
begin
  result := '[' + str + ']';
end;

class function TAdoConnectionHelperForSqlServer.SQLDatabaseNameEscape(const str: string): string;
begin
  result := '[' + str + ']';
end;

function TAdoConnectionHelperForSqlServer.GetSqlServerMac: string;
begin
  ExecSQL('IF object_id(''SeqId'', ''P'') IS NOT NULL DROP PROCEDURE [dbo].[SeqId];');

  ExecSQL('CREATE PROCEDURE SeqId ' +
          'AS ' +
          'begin ' +
          '  declare @t table ' +
          '    ( ' +
          '    i uniqueidentifier default newsequentialid(), ' +
          '    m as cast(i as char(36)) ' +
          '    ) ' +
          ' ' +
          '    insert into @t default values; ' +
          ' ' +
          '    select m FROM @t ' +
          'end;');
  try
    result := Copy(GetScalar('exec SeqId').AsWideString,25,12);
  finally
    ExecSQL('DROP PROCEDURE [dbo].[SeqId];');
  end;
end;

procedure TAdoConnectionHelperForSqlServer.ShrinkDatabase(const Datenbankname: string; typ: TDatabaseFileType);
var
  q: TAdoDataset;
  sTyp: string;
begin
  case typ of
    ftRows: styp := 'ROWS';
    ftLog:  styp := 'LOG';
  end;
  q := GetTable('SELECT f.name as LogicalName, ' +
                '       f.physical_name AS PhysicalName, ' +
                '       f.type_desc TypeofFile ' +
                'FROM sys.master_files f ' +
                'INNER JOIN sys.databases d ON d.database_id = f.database_id ' +
                'where d.name = N'+SQLStringEscape(Datenbankname)+' and ' +
                '      f.type_desc = N'+SQLStringEscape(styp));
  try
    while not q.Eof do
    begin
      ExecSQL('DBCC SHRINKFILE (N'+SQLStringEscape(q.Fields[0].AsWideString)+' , 0)', 3600);
      q.Next;
    end;
  finally
    q.Free;
  end;
end;

function TAdoConnectionHelperForSqlServer.TableExists(const aTableName: string; const aSchemaName: string='dbo'): boolean;
begin
  if Copy(aTableName, 1, 1) = '#' then
  begin
    // Temporary table
    result := GetScalar('select case when OBJECT_ID(N'+SQLStringEscape('tempdb..'+aTableName)+') is not null then ''1'' else ''0'' end') > 0;
  end
  else
  begin
    // Physical table
    // result := GetScalar('select count (*) from sysobjects where name = ' + aTableName.toSQLString) > 0;
    result := GetScalar('SELECT count(*) ' +
                        'FROM INFORMATION_SCHEMA.TABLES ' +
                        'WHERE TABLE_CATALOG = N'+SQLStringEscape(DataBaseName)+' AND ' +
                        '      TABLE_SCHEMA = N'+SQLStringEscape(aSchemaName)+' AND ' +
                        '      TABLE_NAME = N'+SQLStringEscape(aTableName)) > 0;
  end;
end;

function TAdoConnectionHelperForSqlServer.ViewExists(const aViewName: string; const aSchemaName: string='dbo'): boolean;
begin
  // Note: sys.views only shows views from the current database. No need to filter for DataBaseName
  result := GetScalar('select count(*) ' +
                      'from sys.views v ' +
                      'left join sys.schemas s on s.schema_id = v.schema_id ' +
                      'where s.name = N'+SQLStringEscape(aSchemaName)+' and ' +
                      '      v.name = N'+SQLStringEscape(aViewName)+' and ' +
                      '      v.type = ''V''') > 0;
end;

end.
