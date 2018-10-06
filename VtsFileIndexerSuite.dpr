program VtsFileIndexerSuite;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ExplorerForm in 'ExplorerForm.pas' {frmExplorer},
  RedundancyForm in 'RedundancyForm.pas' {frmRedundancy},
  IndexCreatorForm in 'IndexCreatorForm.pas' {frmIndexCreator},
  FinderForm in 'FinderForm.pas' {frmFinder};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
