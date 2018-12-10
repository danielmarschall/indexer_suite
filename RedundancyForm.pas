unit RedundancyForm;

// TODO: man soll einstellen können, dass er redundanzen nur innerhalb eines datenträgers (= root node) findet

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, AdoDb,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Gauges;

type
  TfrmRedundancy = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Gauge1: TGauge;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    procedure Mode2Rec(StartDir: string; const FileMask: string;
      var cntRedundant: integer; var cntUnique: integer);
  protected
    StopRequest: Boolean;
    procedure EnableDisableControls(v: Boolean);
    function TableName: string;
    function conn: TAdoConnection;
  end;

implementation

{$R *.dfm}

uses
  DB, AdoConnHelper, IdHashMessageDigest, idHash, MainForm, IniFiles;

function MD5File(const FileName: string): string;
var
  IdMD5: TIdHashMessageDigest5;
  FS: TFileStream;
begin
  IdMD5 := TIdHashMessageDigest5.Create;
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
{$IFDEF UNICODE} // I actually do not know at which version of Delphi/Indy, this has changed.
    Result := IdMD5.HashStreamAsHex(FS);
{$ELSE}
    Result := IdMD5.AsHex(IdMD5.HashValue(FS));
{$ENDIF}
  finally
    FS.Free;
    IdMD5.Free;
  end;
end;

procedure TfrmRedundancy.Mode2Rec(StartDir: string; const FileMask: string;
  var cntRedundant: integer; var cntUnique: integer);

  procedure CheckFile(aFilename: string);
  var
    md5: string;
  begin
    try
      Label4.Caption := aFilename;
      md5 := MD5File(aFilename);
      if conn.GetScalar('select count(*) from ' + TableName +
        ' where md5hash = ' + conn.SQLStringEscape(md5)) = 0 then
      begin
        Memo1.Lines.Add(aFilename);
        Inc(cntUnique);
      end
      else
      begin
        Inc(cntRedundant);
      end;
    except
      on E: Exception do
      begin
        Memo1.Lines.Add(Format('Error: Cannot process %s : %s',
          [aFilename, E.Message]))
      end;
    end;
  end;

var
  SR: TSearchRec;
  DirList: TStrings;
  IsFound: Boolean;
  i: integer;
begin
  StartDir := IncludeTrailingPathDelimiter(StartDir);

  i := 0;
  IsFound := FindFirst(StartDir + FileMask, faAnyFile - faDirectory, SR) = 0;
  try
    while IsFound do
    begin
      Inc(i);
      Application.ProcessMessages;
      if Application.Terminated or StopRequest then
        Abort;

      CheckFile(StartDir + SR.Name);
      IsFound := FindNext(SR) = 0;
    end;
  finally
    FindClose(SR);
  end;

  // Build a list of subdirectories
  DirList := TStringList.Create;
  try
    IsFound := FindFirst(StartDir + '*', faDirectory, SR) = 0;
    try
      while IsFound do
      begin
        if (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          Application.ProcessMessages;
          if Application.Terminated or StopRequest then
            Abort;

          DirList.Add(StartDir + SR.Name);
        end;
        IsFound := FindNext(SR) = 0;
      end;
    finally
      FindClose(SR);
    end;

    // Scan the list of subdirectories
    for i := 0 to DirList.Count - 1 do
    begin
      try
        Mode2Rec(DirList[i], FileMask, cntRedundant, cntUnique);
      except
        on E: Exception do
        begin
          if E is EAbort then
            Abort;
          Memo1.Lines.Add('Unexpected error at directory ' + DirList[i] + ': ' +
            E.Message);
        end;
      end;
    end;
  finally
    DirList.Free;
  end;
end;

function TfrmRedundancy.TableName: string;
begin
  result := frmMain.TableName;
end;

procedure TfrmRedundancy.Button1Click(Sender: TObject);
var
  q: TADODataSet;
  fMD5: TField;
  fFilename: TField;
  dirMask: string;
  cntRedundant: integer;
  cntUnique: integer;
begin
  EnableDisableControls(False);
  if ComboBox1.ItemIndex = 1 then
    Gauge1.Visible := False;
  Memo1.Lines.Clear;
  try
{$REGION 'Mode 1'}
    if ComboBox1.ItemIndex = 0 then
    begin
      dirMask := IncludeTrailingPathDelimiter(Edit1.Text) + '%';
      q := conn.GetTable
        ('select filename, md5hash from '+TableName+' where filename like ' +
        conn.SQLStringEscape(dirMask) + ' order by filename');
      try
        Gauge1.MinValue := 0;
        Gauge1.MaxValue := q.RecordCount;
        Gauge1.Progress := 0;
        cntRedundant := 0;
        cntUnique := 0;
        fMD5 := q.FieldByName('md5hash');
        fFilename := q.FieldByName('filename');
        while not q.Eof do
        begin
          if conn.GetScalar('select count(*) from '+TableName+' where md5hash = ' +
            conn.SQLStringEscape(fMD5.AsString) + ' and filename not like ' +
            conn.SQLStringEscape(dirMask)) = 0 then
          begin
            Memo1.Lines.Add(fFilename.AsString);
            Inc(cntUnique);
          end
          else
          begin
            Inc(cntRedundant);
          end;
          Gauge1.Progress := Gauge1.Progress + 1;
          Application.ProcessMessages;
          if Application.Terminated then
            Abort;
          q.Next;
        end;
      finally
        q.Free;
      end;
    end;
{$ENDREGION}
{$REGION 'Mode 2'}
    if ComboBox1.ItemIndex = 1 then
    begin
      cntRedundant := 0;
      cntUnique := 0;
      Mode2Rec(Edit1.Text, '*', cntRedundant, cntUnique);
    end;
{$ENDREGION}
    if (cntRedundant = 0) and (cntUnique = 0) then
      raise Exception.Create('No files found. Is the string correct?')
    else
      ShowMessageFmt('Done. %d files are redundant. %d are unique.',
        [cntRedundant, cntUnique]);

    if ComboBox1.ItemIndex = 0 then
    begin
      ShowMessage
        ('Attention: Only check 1 directory at a time, then delete redundant files, then re-index and only then continue with checking the redundancy of the any other directory.');
    end;
  finally
    EnableDisableControls(True);
    Gauge1.Progress := 0;
    Gauge1.Visible := True;
    Label4.Caption := '';
  end;
end;

function TfrmRedundancy.conn: TAdoConnection;
begin
  Result := frmMain.AdoConnection1;
end;

procedure TfrmRedundancy.EnableDisableControls(v: Boolean);
begin
  Edit1.Enabled := v;
  Button1.Enabled := v;
  ComboBox1.Enabled := v;
end;

procedure TfrmRedundancy.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmRedundancy.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  StopRequest := True;
end;

procedure TfrmRedundancy.FormShow(Sender: TObject);
var
  ini: TMemIniFile;
begin
  ini := frmMain.ini;
  Edit1.Text := ini.ReadString('RedundancyFinder', 'DefaultDir', '');
  ComboBox1.ItemIndex := ini.ReadInteger('RedundancyFinder', 'DefaultMode', 1)-1;
end;

end.
