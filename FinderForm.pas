unit FinderForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AdoDb;

type
  TfrmFinder = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    Button1: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    procedure EnableDisableControls(v: boolean);
  protected
    StopRequest: boolean;
  public
    function conn: TAdoConnection;
    function TableName: string;
  end;

implementation

{$R *.dfm}

uses
  MainForm, AdoConnHelper, IniFiles;

procedure TfrmFinder.Button1Click(Sender: TObject);
var
  q: TADODataSet;
  cntFound: integer;
begin
  memo1.Lines.Clear;
  EnableDisableControls(false);
  try
    cntFound := 0;
    q := conn.GetTable('select filename from '+TableName+' where filename like '+conn.SQLStringEscape('%'+edit1.Text+'%')+' order by filename');
    while not q.Eof do
    begin
      memo1.Lines.Add(q.Fields[0].AsString);
      Inc(cntFound);
      if StopRequest then Abort;
      q.Next;
    end;
    ShowMessageFmt('Done. Found %d files', [cntFound]);
  finally
    EnableDisableControls(true);
  end;
end;

function TfrmFinder.conn: TAdoConnection;
begin
  result := frmMain.ADOConnection1;
end;

procedure TfrmFinder.EnableDisableControls(v: boolean);
begin
  Memo1.Enabled := v;
  Button1.Enabled := v;
  Edit1.Enabled := v;
end;

procedure TfrmFinder.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmFinder.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  StopRequest := true;
end;

procedure TfrmFinder.FormShow(Sender: TObject);
var
  ini: TMemIniFile;
begin
  ini := frmMain.ini;
  Edit1.Text := ini.ReadString('Finder', 'DefaultDir', '');
end;

function TfrmFinder.TableName: string;
begin
  result := frmMain.TableName;
end;

end.
