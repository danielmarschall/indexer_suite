unit AdoConnHelper;

(*
 * Class helper for TAdoConnection
 * by Daniel Marschall, ViaThinkSoft <www.viathinksoft.com>
 * Revision: 28 Aug 2018
 *)

interface

uses
  DB, ADODB, Variants, Classes, SysUtils;

type
  TAdoConnectionHelper = class helper for TADOConnection
  private
    function GetConnectionID: TGUID;
  public
    // Attention: Some of the functions here (e.g. ConnectionID) require SQL Server
    function GetTable(SQL: string; ATimeout: integer=-1): TADODataSet;
    function GetScalar(SQL: string; ATimeout: integer=-1): Variant;
    procedure ExecSQL(SQL: string; ATimeout: integer=-1);
    procedure ExecSQLList(List: TStrings; ATimeout: integer=-1);
    procedure Connect(AConnectionString: string; ATimeout: integer=-1);
    procedure Disconnect;
    class function SQLStringEscape(str: string): string; static;
    class function SQLObjectNameEscape(str: string): string; static;
    procedure GetPrimaryKeyNames(TableName: string; outsl: TStrings);
    property ConnectionID: TGUID read GetConnectionID;
    function InsertAndReturnID(query: string): integer;
    procedure DropTable(aTableName: string);
    function FieldCount(aTableName: string): integer;
    function IndexCount(aTableName: string): integer;
    function TableExist(aTableName: string): boolean;
    function ViewExist(aViewName: string): boolean;
    function ColumnExists(aTableName, aColumnName: string): boolean;
    procedure GetTableNames(List: TStrings; SystemTables: Boolean=false);
    //property TransactionLevel: integer read FTransactionLevel;
  end;

implementation

{ TAdoConnectionHelper }

function TAdoConnectionHelper.GetConnectionID: TGUID;
var
  s : string;
begin
  s := GetScalar('select connection_id from sys.dm_exec_connections ec ' +
                 'left join sys.dm_exec_sessions se on ec.session_id = se.session_id ' +
                 'where se.session_id = @@SPID');
  result := StringToGUID(s);
end;

procedure TAdoConnectionHelper.GetPrimaryKeyNames(TableName: string; outsl: TStrings);
var
  ds: TADODataSet;
begin
  ds := TADODataSet.Create(nil);
  try
    OpenSchema(siPrimaryKeys, Unassigned, EmptyParam, ds);
    while not ds.Eof do
    begin
      if ds.FieldByName('TABLE_NAME').AsString = TableName then
      begin
        outsl.Add(ds.FieldByName('COLUMN_NAME').AsString);
      end;
      ds.Next;
    end;
    ds.Close;
  finally
    ds.Free;
  end;
end;

function TAdoConnectionHelper.GetScalar(SQL: string; ATimeout: integer=-1): Variant;
var
  ds: TADODataSet;
begin
  ds := GetTable(SQL, ATimeout);
  result := ds.Fields[0].AsVariant;
  ds.Free;
end;

function TAdoConnectionHelper.GetTable(SQL: string; ATimeout: integer=-1): TADODataSet;
begin
  result := TADODataSet.Create(nil);
  result.Connection := Self;
  result.EnableBCD := false;
  result.ParamCheck := false;
  result.CommandType := cmdText;
  result.CommandText := SQL;
  result.DisableControls;
  if ATimeout <> -1 then result.CommandTimeout := ATimeout;
  result.Active := true;
end;

function TAdoConnectionHelper.ColumnExists(aTableName, aColumnName: string): boolean;
begin
  result := GetScalar('select count (*) from sys.columns where Name = '+SQLStringEscape(aColumnName)+' and Object_ID = Object_ID('+SQLStringEscape(aTableName)+')') > 0;
end;

procedure TAdoConnectionHelper.GetTableNames(List: TStrings;
  SystemTables: Boolean);
begin

end;

function TAdoConnectionHelper.IndexCount(aTableName: string): integer;
begin
  result := GetScalar('select max (ik.indid) from sysindexkeys ik left join sysindexes ind on ind.id = ik.id and ind.indid = ik.indid where ik.id = (select id from sysobjects ' +
                      'where name = ''' + aTableName + ''') and ind.status < 10000000');
end;

function TAdoConnectionHelper.InsertAndReturnID(query: string): integer;
resourcestring
  LNG_NO_AUTOINC = 'Cannot find AutoIncrement value';
var
  q1: TADODataSet;
begin
  BeginTrans;
  try
    ExecSql(query); // Execute(query);

    // Hinweis: Das geht nur lokal, nicht über linked servers
    // TODO: Man sollte lieber SCOPE_IDENTITY() verwenden
    q1 := GetTable('select @@IDENTITY as INSERT_ID;');
    try
      if (q1.RecordCount = 0) or (q1.{FieldByName('INSERT_ID')}Fields[0].AsInteger = 0) then
      begin
        //result := -1;
        raise EADOError.Create(LNG_NO_AUTOINC);
      end;

      result := q1.{FieldByName('INSERT_ID')}Fields[0].AsInteger;
    finally
      FreeAndNil(q1);
    end;
  finally
    CommitTrans;
  end;
end;

procedure TAdoConnectionHelper.Connect(AConnectionString: string; ATimeout: integer=-1);
begin
  Disconnect;
  ConnectionString := AConnectionString;
  if ATimeout <> -1 then ConnectionTimeout := ATimeout;
  Connected := true;
end;

procedure TAdoConnectionHelper.Disconnect;
begin
  Connected := false;
end;

procedure TAdoConnectionHelper.DropTable(aTableName: string);
begin
  if ViewExist(aTableName) then
  begin
    ExecSql('drop view ' + SQLObjectNameEscape(aTableName));
  end;
  if TableExist(aTableName) then
  begin
    ExecSql('drop table ' + SQLObjectNameEscape(aTableName));
  end;
end;

procedure TAdoConnectionHelper.ExecSQL(SQL: string; ATimeout: integer);
var
  cmd: TADOCommand;
begin
  cmd := TADOCommand.Create(nil);
  try
    cmd.Connection := Self;
    cmd.ParamCheck := false;
    cmd.CommandType := cmdText;
    cmd.CommandText := SQL;
    if ATimeOut <> -1 then cmd.CommandTimeout := ATimeout;
    cmd.Execute;
  finally
    cmd.Free;
  end;
end;

procedure TAdoConnectionHelper.ExecSQLList(List: TStrings; ATimeout: integer);
var
  s: string;
begin
  for s in List do
  begin
    ExecSQL(s);
  end;
end;

function TAdoConnectionHelper.FieldCount(aTableName: string): integer;
begin
  result := GetScalar('select count (*) from syscolumns where id = (select id from sysobjects where name = ''' + aTableName + ''') ');
end;

class function TAdoConnectionHelper.SQLStringEscape(str: string): string;
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

  // DM 29.02.2016 Irgendwie versteh ich das nicht...
  // 'xxx\'xxx' ist erlaubt, aber 'xxx\'xxx\'xxx' nicht
  // aber 'xxx''xxx''xxx' geht.
  result := StringReplace(result, '''', '''''', [rfReplaceAll]);

  // Verhindern, dass SQL Server denkt, es sei ein Parameterobjekt
  // Brauchen wir nur, wenn die abfrage ParamCheck=true hat.
  // Wir haben aber in hl_Datenbank.pas das immer auf false.
  // result := StringReplace(result, ':', '::', [rfReplaceAll]);

  {$IFDEF UNICODE}
  result := 'N''' + result + '''';
  {$ELSE}
  result := '''' + result + '''';
  {$ENDIF}
end;

function TAdoConnectionHelper.TableExist(aTableName: string): boolean;
begin
  if Copy(aTableName, 1, 1) = '#' then
  begin
    // TempTable
    result := GetScalar('select case when OBJECT_ID(''tempdb..'+aTableName+''') is not null then ''1'' else ''0'' end') > 0;
  end
  else
  begin
    // Physikalische Tabelle (in Schema dbo)
    // result := GetScalar('select count (*) from sysobjects where name = ' + aTableName.toSQLString) > 0;
    result := GetScalar('SELECT count(*) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_CATALOG = N'''+DefaultDatabase+''' AND TABLE_SCHEMA = N''dbo'' AND TABLE_NAME = N'''+aTableName+'''') > 0;
  end;
end;

function TAdoConnectionHelper.ViewExist(aViewName: string): boolean;
begin
  result := GetScalar('select count(*) FROM sys.views where name = '+SQLStringEscape(aViewName)) > 0;
end;

class function TAdoConnectionHelper.SQLObjectNameEscape(str: string): string;
begin
  result := '[dbo].[' + str + ']';
end;

end.
