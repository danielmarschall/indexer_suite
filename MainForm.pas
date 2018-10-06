unit MainForm;

// TODO: viele funktionen: (ggf auch per kontextmenü im explorer)
//       - öffnen der datei, wenn datenträger online ist
//       - anzeigen von eigenschaften
//       - schauen ob es die dateiprüfsumme noch woanders gibt
//       - welche dateien in A und welche in B?
//       - Alle Fehler zeigen
//       - Statistik (Anzahl Dateien etc)

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Data.DB,
  Data.Win.ADODB, IniFiles;

type
  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    OfflineExplorer1: TMenuItem;
    RedundancyVerifier1: TMenuItem;
    IndexCreator1: TMenuItem;
    ADOConnection1: TADOConnection;
    Finder1: TMenuItem;
    procedure OfflineExplorer1Click(Sender: TObject);
    procedure RedundancyVerifier1Click(Sender: TObject);
    procedure IndexCreator1Click(Sender: TObject);
    procedure Finder1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTableName: string;
  public
    ini: TMemIniFile;
    property TableName: string read FTableName;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  ExplorerForm, RedundancyForm, IndexCreatorForm, FinderForm;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  iniFilename: string;
begin
  iniFilename := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'VtsFileIndexerSuite.ini';
  if FileExists(iniFilename) then
  begin
    ini := TMemIniFile.Create(iniFilename);

    FTableName := ini.ReadString('IndexerSuite', 'TableName', 'files');
    Caption := Caption + Format(' [%s]', [FTableName]);

    ADOConnection1.ConnectionString := ini.ReadString('IndexerSuite', 'ConnectionString', '');
    if ADOConnection1.ConnectionString = '' then
    begin
      ShowMessage('Please define a ConnectionString in the INI file.');
      Close;
      Exit;
    end;
    try
      ADOConnection1.Connected := true;
    except
      on E: Exception do
      begin
        ShowMessage('Cannot connect to the database: ' + E.Message);
        Close;
        Exit;
      end;
    end;
  end
  else
  begin
    ShowMessageFmt('%s not found', [iniFilename]);
    Close;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ini);
end;

procedure TfrmMain.IndexCreator1Click(Sender: TObject);
begin
  TfrmIndexCreator.Create(self);
end;

procedure TfrmMain.OfflineExplorer1Click(Sender: TObject);
begin
  TfrmExplorer.Create(Self);
end;

procedure TfrmMain.Finder1Click(Sender: TObject);
begin
  TfrmFinder.Create(Self);
end;

procedure TfrmMain.RedundancyVerifier1Click(Sender: TObject);
begin
  TfrmRedundancy.Create(Self);
end;

end.
