unit IndexCreatorForm;

// TODO: vor einem fehler bitte vorher einen löschvorgang durchführen --> geht nicht?
// TODO: berücksichtigen, wenn datei gesperrt. etc, fehler anschauen
// TODO: warum sind in der db mehr einträge als dateien auf der festplatte sind?!
// TODO: Möglichkeit geben, Dateien und Verzeichnisse auszuschließen
// TODO: should we include flags (readonly, invisible, compressed, encrypted)?
// TODO: search+replace tool, wenn man große verschiebungen vorgenommen hat
// update top (100000) files set filename = replace(filename, '\\?\Volume{560e8251-2b6a-4ab7-82fc-d03df4d93538}\', 'EHDD:\') where filename like '%\\?\%';
// TODO: anzeige, wie viele stunden der prozess schon läuft
// TODO: multithreading
// TODO: diverse tools schreiben, die die datenbank nutzen, z.b. ein tool, das prüft, ob ein verzeichnis vollständig redundant ist
// TODO: Beim Lauf F:\nas\data wurden 1312 Fehler gefunden, aber nicht geloggt! ?! Eine exception im exception handler?!
// => nochmal durchlaufen lassen
// TODO: "Laufwerk" EHDD: soll man auch eingeben dürfen (das ist z.b. wichtig, wenn man Querverknüpfung vom Explorer verwendet)
// TODO: validate modus auch ohne prüfsummencheck. nur gucken ob die dateien existieren

{$DEFINE VIATHINKSOFT}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, DB, ADODB, Menus;

const
  modusUpdate = 0;
  modusRecreate = 1;
  modusValidation = 2;

type
  TfrmIndexCreator = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    LabeledEdit2: TLabeledEdit;
    Button2: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PopupMenu1: TPopupMenu;
    Copyuniquepathtoclipboard1: TMenuItem;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Memo1: TMemo;
    Button4: TButton;
    Label14: TLabel;
    cbNoDelete: TCheckBox;
    Memo2: TMemo;
    cbVerboseLogs: TCheckBox;
    cbSimulate: TCheckBox;
    rgModus: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Copyuniquepathtoclipboard1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure rgModusClick(Sender: TObject);
  private
    StopRequest: boolean;
    sumsize: int64;
    sumfiles: int64;
    sumfiles_new: int64;
    sumfiles_updated: int64;
    sumfiles_error: int64;
    sumfiles_deleted: int64;
    sumfiles_integrityfail: int64;
    function TableName: string;
    function conn: TAdoConnection;
    procedure Rec(StartDir: string; const FileMask: string);
    procedure CheckFile(const originalFileName, uniqueFilename: string);
    procedure EnableDisableControls(enabled: boolean);
    procedure IndexDrive(initialdir: string);
    procedure RedrawStats;
    procedure DeleteVanishedFiles(mask: string = '');
    class function DriveGuid(const Letter: char): string; static;
    class function uniqueFilename(const filename: string): string; static;
    class function VtsSpecial(const filename: string): string; static;
    procedure DeleteAllFiles(mask: string = '');
  end;

implementation

{$R *.dfm}

uses
  FileCtrl, DateUtils, inifiles, IdHashMessageDigest, idHash, Math, Clipbrd,
  StrUtils, AdoConnHelper, MainForm;

const
  Win32ImportSuffix = {$IFDEF Unicode}'W'{$ELSE}'A'{$ENDIF};

function GetVolumeNameForVolumeMountPointA(lpszVolumeMountPoint: PAnsiChar;
  lpszVolumeName: PAnsiChar; cchBufferLength: DWORD): BOOL; stdcall;
  external 'kernel32.dll';
function GetVolumeNameForVolumeMountPointW(lpszVolumeMountPoint: PWideChar;
  lpszVolumeName: PWideChar; cchBufferLength: DWORD): BOOL; stdcall;
  external 'kernel32.dll';
function GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint: PChar;
  lpszVolumeName: PChar; cchBufferLength: DWORD): BOOL; stdcall;
  external 'kernel32.dll' name 'GetVolumeNameForVolumeMountPoint' +
  Win32ImportSuffix;

const
  ERROR_FIELD_SIZE = 200;
{$IFDEF VIATHINKSOFT}
  // Example of multiple drives merging to one Index
  // Find out via "mountvol" command
  GUID_EHDD_A = '\\?\Volume{31e044b1-28dc-11e6-9bae-d067e54bf736}\';
  GUID_EHDD_B = '\\?\Volume{560e8251-2b6a-4ab7-82fc-d03df4d93538}\';
  GUID_EHDD_R = '\\?\Volume{9d53ea3c-175c-4a8f-a7b4-7b9e6b765e58}\';
{$ENDIF}

function MD5File(const filename: string): string;
var
  IdMD5: TIdHashMessageDigest5;
  FS: TFileStream;
begin
  IdMD5 := TIdHashMessageDigest5.Create;
  FS := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
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

function FileMTime_UTC(const filename: string): TDateTime;
var
  fad: TWin32FileAttributeData;
  systime: SYSTEMTIME;
begin
  if not GetFileAttributesEx(PChar(filename), GetFileExInfoStandard, @fad) then
    RaiseLastOSError;

  FileTimeToSystemTime(fad.ftLastWriteTime, systime);

  Result := SystemTimeToDateTime(systime);
end;

function FileCTime_UTC(const filename: string): TDateTime;
var
  fad: TWin32FileAttributeData;
  systime: SYSTEMTIME;
begin
  if not GetFileAttributesEx(PChar(filename), GetFileExInfoStandard, @fad) then
    RaiseLastOSError;

  FileTimeToSystemTime(fad.ftCreationTime, systime);

  Result := SystemTimeToDateTime(systime);
end;

function GetFileSize(const AFileName: String): int64;
var
  lFindData: TWin32FindData;
  lHandle: Cardinal;
begin
  // https://www.delphipraxis.net/24331-dateigroesse-einer-beliebigen-datei-ermitteln.html
  lHandle := FindFirstFile(PChar(AFileName), lFindData);
  if (lHandle <> INVALID_HANDLE_VALUE) then
  begin
    Result := lFindData.nFileSizeLow;
    PCardinal(Cardinal(@Result) + SizeOf(Cardinal))^ := lFindData.nFileSizeHigh;
    Windows.FindClose(lHandle);
  end
  else
    Result := 0;
end;

function IntToStr2(i: int64): string;
begin
  // https://www.delphipraxis.net/150464-integer-mit-tausender-trennzeichen-ausgeben.html
  Result := Format('%.0n', [i / 1]);
end;

function ConvertBytes(Bytes: int64): string;
const
  Description: Array [0 .. 8] of string = ('Bytes', 'KB', 'MB', 'GB', 'TB',
    'PB', 'EB', 'ZB', 'YB');
var
  i: Integer;
begin
  // https://stackoverflow.com/questions/30548940/correct-way-to-convert-size-in-bytes-to-kb-mb-gb-delphi
  i := 0;

  while Bytes > Power(1024, i + 1) do
    Inc(i);

  Result := FormatFloat('###0.##', Bytes / IntPower(1024, i)) + ' ' +
    Description[i];
end;

var
  DriveGuidCache: TStringList = nil;

class function TfrmIndexCreator.DriveGuid(const Letter: char): string;
var
  Buffer: array [0 .. 49] of char;
begin
  if not Assigned(DriveGuidCache) then
    DriveGuidCache := TStringList.Create;

  Result := DriveGuidCache.Values[Letter];
  if Result = '' then
  begin
    Win32Check(GetVolumeNameForVolumeMountPoint(PChar(Letter + ':\'), Buffer,
      Length(Buffer)));
    Result := Buffer;
    DriveGuidCache.Values[Letter] := Result;
  end;
end;

class function TfrmIndexCreator.uniqueFilename(const filename: string): string;
var
  guid: string;
begin
  if Length(filename) < 2 then
    exit;
  if filename[2] = ':' then
  begin
    guid := DriveGuid(filename[1]);

    Result := guid + Copy(filename, 4, Length(filename) - 3);

    // result := LowerCase(result);
  end
  else
    Result := filename; // z.B. UNC-Pfad
end;

class function TfrmIndexCreator.VtsSpecial(const filename: string): string;
begin
  Result := filename;
{$IFDEF VIATHINKSOFT}
  Result := StringReplace(Result, GUID_EHDD_A, 'EHDD:\', []);
  Result := StringReplace(Result, GUID_EHDD_B, 'EHDD:\', []);
  Result := StringReplace(Result, GUID_EHDD_R, 'EHDD:\', []);
{$ENDIF}
end;

function SpecialCompare(a, b: TDateTime): boolean; // true = same timestamp
begin
  if SecondsBetween(a,b) < 2 then exit(true); // equal

  if SecondsBetween(a,b) > 7200 then exit(false);

  // Minute and Second equal, and difference is < 2h: fair enough, seems to be a DST issue
  if copy(TimeToStr(a),4,5) = copy(TimeToStr(b),4,5) then exit(true);

  result := false;
end;

procedure TfrmIndexCreator.CheckFile(const originalFileName,
  uniqueFilename: string);

  function DateTimeToSQL(dt: TDateTime): string;
  begin
    if dt = -1 then
      Result := 'NULL'
    else
      Result := conn.SQLStringEscape(DateTimetoStr(dt));
  end;

type
  TExistResult = (erDoesNotExist, erHadError, erChanged, erUnchanged);

var
  lastCheckedMd5: string;

  function Exists(const filename: string; size: int64;
    const modified: TDateTime): TExistResult;
  var
    q: TADODataSet;
  begin
    q := conn.GetTable('select error, size, modified, md5hash from ' + TableName
      + ' where filename = ' + conn.SQLStringEscape
      (VtsSpecial(uniqueFilename)));
    try
      if q.RecordCount = 0 then
        Result := erDoesNotExist
      else if not q.Fields[0].IsNull then
        Result := erHadError
      else if (q.Fields[1].AsString <> IntToStr(size)) or // we are combining strings because of int64
        not SpecialCompare(q.Fields[2].AsDateTime, modified) then
      begin
        Result := erChanged
      end
      else
        Result := erUnchanged;
      lastCheckedMd5 := q.Fields[3].AsString;
    finally
      FreeAndNil(q);
    end;
  end;

var
  created, modified: TDateTime;
  size: int64;
  md5: string;
begin
  Label1.Caption := MinimizeName(originalFileName, Label1.Canvas, Label1.Width);
  Application.ProcessMessages;

  try
    if FileExists(uniqueFilename) then
      created := FileCTime_UTC(uniqueFilename)
    else
      created := -1;

    if FileExists(uniqueFilename) then
      modified := FileMTime_UTC(uniqueFilename)
    else
      modified := -1;

    size := GetFileSize(uniqueFilename);
    Inc(sumsize, size);
    Inc(sumfiles);

    if rgModus.ItemIndex = modusRecreate then
    begin
      md5 := MD5File(uniqueFilename);
      if not cbSimulate.Checked then
      begin
        conn.ExecSQL('INSERT INTO ' + TableName +
          ' (filename, size, created, modified, md5hash, error) values (' +
          conn.SQLStringEscape(VtsSpecial(uniqueFilename)) + ', ' +
          IntToStr(size) + ', ' + DateTimeToSQL(created) +
          ', ' + DateTimeToSQL(modified) + ', ' +
          conn.SQLStringEscape(LowerCase(md5)) + ', NULL);');
      end;
      if cbVerboseLogs.Checked then
        Memo2.Lines.Add('New: ' + uniqueFilename);
      Inc(sumfiles_new);
    end
    else
    begin
      case Exists(uniqueFilename, size, modified) of
        erDoesNotExist: // File does not exist or has a different hash
          begin
            if rgModus.ItemIndex <> modusValidation then
              md5 := MD5File(uniqueFilename);
            if not cbSimulate.Checked and (rgModus.ItemIndex <> modusValidation)
            then
            begin
              conn.ExecSQL('INSERT INTO ' + TableName +
                ' (filename, size, created, modified, md5hash, error) values ('
                + conn.SQLStringEscape(VtsSpecial(uniqueFilename)) + ', ' +
                IntToStr(size) + ', ' +
                DateTimeToSQL(created) + ', ' +
                DateTimeToSQL(modified) + ', ' +
                conn.SQLStringEscape(LowerCase(md5)) + ', NULL);');
            end;
            if cbVerboseLogs.Checked then
              Memo2.Lines.Add('New: ' + uniqueFilename);
            Inc(sumfiles_new);
          end;
        erHadError, erChanged:
          begin
            if rgModus.ItemIndex <> modusValidation then
              md5 := MD5File(uniqueFilename);
            if not cbSimulate.Checked and (rgModus.ItemIndex <> modusValidation)
            then
            begin
              conn.ExecSQL('UPDATE ' + TableName + ' SET size = ' +
                IntToStr(size) + ', created = ' +
                DateTimeToSQL(created) + ', modified = ' +
                DateTimeToSQL(modified) + ', md5hash = ' +
                conn.SQLStringEscape(LowerCase(md5)) +
                ', error = NULL WHERE filename = ' + conn.SQLStringEscape
                (VtsSpecial(uniqueFilename)) + ';');
            end;
            if cbVerboseLogs.Checked then
              Memo2.Lines.Add('Updated: ' + uniqueFilename);
            Inc(sumfiles_updated);
          end;
        erUnchanged: // Date/Time+Size has not changed
          begin
            {$REGION 'Update it to correct wrong UTC/DST datasets...'}
            conn.ExecSQL('UPDATE ' + TableName + ' SET size = ' +
              IntToStr(size) + ', created = ' +
              DateTimeToSQL(created) + ', modified = ' +
              DateTimeToSQL(modified) +
              ', error = NULL WHERE filename = ' + conn.SQLStringEscape
              (VtsSpecial(uniqueFilename)) + ';');
            {$ENDREGION}

            if rgModus.ItemIndex = modusValidation then
            begin
              md5 := MD5File(uniqueFilename);
              if not SameText(md5, lastCheckedMd5) then
              begin
                Memo2.Lines.Add
                  ('!!! HASH HAS CHANGED WHILE DATETIME+SIZE IS THE SAME: ' +
                  uniqueFilename + ' (' + lastCheckedMd5 + ' became ' +
                  md5 + ')');
                Memo2.Color := clRed;
                Inc(sumfiles_integrityfail);
              end;
            end;
          end;
      end;
    end;
  except
    on E: Exception do
    begin
      if E is EAbort then
        Abort;
      // if AdoConnection1.InTransaction then AdoConnection1.RollbackTrans;
      // AdoConnection1.BeginTrans;
      try
        if not cbSimulate.Checked and (rgModus.ItemIndex <> modusValidation)
        then
        begin
          conn.ExecSQL('DELETE FROM ' + TableName + ' WHERE filename = ' +
            conn.SQLStringEscape(VtsSpecial(uniqueFilename)) + ';');
          conn.ExecSQL('INSERT INTO ' + TableName +
            ' (filename, size, created, modified, md5hash, error) values (' +
            conn.SQLStringEscape(VtsSpecial(uniqueFilename)) +
            ', NULL, NULL, NULL, NULL, ' + conn.SQLStringEscape(Copy(E.Message,
            1, ERROR_FIELD_SIZE)) + ');');
          Memo2.Lines.Add('Error (logged): ' + E.Message + ' at file ' +
            VtsSpecial(uniqueFilename));
        end
        else
        begin
          Memo2.Lines.Add('Error: ' + E.Message + ' at file ' +
            VtsSpecial(uniqueFilename));
        end;
        // AdoConnection1.CommitTrans;
      except
        // AdoConnection1.RollbackTrans;
        Memo2.Lines.Add('Cannot write error into file database! ' + E.Message +
          ' at file ' + VtsSpecial(uniqueFilename));
      end;
      Inc(sumfiles_error);
    end;
  end;

  RedrawStats;
  Application.ProcessMessages;
end;

function TfrmIndexCreator.conn: TAdoConnection;
begin
  Result := frmMain.AdoConnection1;
end;

procedure TfrmIndexCreator.RedrawStats;
begin
  Label5.Caption := ConvertBytes(sumsize);
  Label6.Caption := IntToStr2(sumfiles);
  Label7.Caption := IntToStr2(sumfiles_new);
  Label9.Caption := IntToStr2(sumfiles_updated);
  Label11.Caption := IntToStr2(sumfiles_error);
  Label12.Caption := IntToStr2(sumfiles_deleted);
  // LabelXX.Caption := IntToStr2(sumfiles_integrityfail);
end;

procedure TfrmIndexCreator.Copyuniquepathtoclipboard1Click(Sender: TObject);
var
  s: string;
begin
  s := uniqueFilename(LabeledEdit2.Text);
  Clipboard.AsText := s;
{$IFDEF VIATHINKSOFT}
  if VtsSpecial(s) <> s then
  begin
    s := s + #13#10 + VtsSpecial(s);
  end;
{$ENDIF}
  ShowMessageFmt('Copied to clipboard:' + #13#10#13#10 + '%s', [s]);
end;

procedure TfrmIndexCreator.rgModusClick(Sender: TObject);
begin
  cbSimulate.enabled := rgModus.ItemIndex <> modusValidation;
  cbNoDelete.enabled := rgModus.ItemIndex <> modusValidation;
end;

function TfrmIndexCreator.TableName: string;
begin
  Result := frmMain.TableName;
end;

procedure TfrmIndexCreator.Rec(StartDir: string; const FileMask: string);
var
  SR: TSearchRec;
  DirList: TStrings;
  IsFound: boolean;
  i: Integer;
  UniqueStartDir: string;
begin
  StartDir := IncludeTrailingPathDelimiter(StartDir);

  i := 0;
  conn.BeginTrans;
  IsFound := FindFirst(StartDir + FileMask, faAnyFile - faDirectory, SR) = 0;
  try
    while IsFound do
    begin
      Inc(i);
      if i mod 1000 = 0 then // Only for performance
      begin
        conn.CommitTrans;
        conn.BeginTrans;
      end;
      Application.ProcessMessages;
      if Application.Terminated or StopRequest then
        Abort;

      if UniqueStartDir = '' then
        UniqueStartDir := uniqueFilename(StartDir);
      CheckFile(StartDir + SR.Name, UniqueStartDir + SR.Name);
      IsFound := FindNext(SR) = 0;
    end;
  finally
    FindClose(SR);
    conn.CommitTrans;
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
        Rec(DirList[i], FileMask);
      except
        on E: Exception do
        begin
          if E is EAbort then
            Abort;
          Memo2.Lines.Add('Unexpected error at directory ' + DirList[i] + ': ' +
            E.Message);
        end;
      end;
    end;
  finally
    DirList.Free;
  end;
end;

procedure TfrmIndexCreator.DeleteAllFiles(mask: string = '');
begin
  sumfiles_deleted := conn.GetScalar('select count(*) as cnt from ' + TableName
    + ' where filename like ' + conn.SQLStringEscape(VtsSpecial(mask)));
  RedrawStats;

  if not cbSimulate.Checked and (rgModus.ItemIndex <> modusValidation) then
  begin
    if (mask = '') or (mask = '%') then
      conn.ExecSQL('delete from ' + TableName)
    else
      conn.ExecSQL('delete from ' + TableName + ' where filename like ' +
        conn.SQLStringEscape(VtsSpecial(mask)));
  end;
end;

procedure TfrmIndexCreator.DeleteVanishedFiles(mask: string = '');

{$IFDEF VIATHINKSOFT}
var
  cacheAconnected: boolean;
  cacheBconnected: boolean;
  cacheRconnected: boolean;
{$ENDIF}
  function AllowFileCheck(AFileName: string): boolean;
  var
    guid: string;
  begin
    Result := false;
{$IFDEF VIATHINKSOFT}
    if StartsText('EHDD:\', AFileName) then
    begin
      if not cacheAconnected and SysUtils.DirectoryExists(GUID_EHDD_A) then
      begin
        cacheAconnected := true;
      end;
      if not cacheBconnected and SysUtils.DirectoryExists(GUID_EHDD_B) then
      begin
        cacheBconnected := true;
      end;
      if not cacheRconnected and SysUtils.DirectoryExists(GUID_EHDD_R) then
      begin
        cacheRconnected := true;
      end;
      Result := cacheAconnected or cacheBconnected or cacheRconnected;
    end
    else
{$ENDIF}
      if StartsText('\\?\Volume', AFileName) then
      begin
        guid := Copy(AFileName, 1, 49);
        if EndsText('\', guid) then // should always happen
        begin
          // TODO: cache this result somehow, so that DirectoryExists() does not need to be called all the time
          if SysUtils.DirectoryExists(guid) then // is drive connected/existing?
          begin
            Result := true;
          end;
        end;
      end
      else
      begin
        // TODO: Einen Code für Netzlaufwerke machen: Wir dürfen nur Dateien löschen,
        // wenn das Netzlaufwerk wirklich da ist.
      end;
  end;

  function FileDoesExist(AFileName: string): boolean;
  begin
{$IFDEF VIATHINKSOFT}
    if StartsText('EHDD:\', AFileName) then
    begin
      // Attention: AllowFileCheck must be called to initialize cacheAconnected and cacheBconnected

      if cacheAconnected and FileExists(StringReplace(AFileName, 'EHDD:\',
        GUID_EHDD_A, [])) then
        exit(true);

      if cacheBconnected and FileExists(StringReplace(AFileName, 'EHDD:\',
        GUID_EHDD_B, [])) then
        exit(true);

      if cacheBconnected and FileExists(StringReplace(AFileName, 'EHDD:\',
        GUID_EHDD_R, [])) then
        exit(true);

      exit(false);
    end;
{$ENDIF}
    exit(FileExists(AFileName));
  end;

var
  filename: string;
  q: TADODataSet;
  fFileName: TField;
  i: int64;
begin
  if mask <> '' then
    q := conn.GetTable('select filename from ' + TableName +
      ' where filename like ' + conn.SQLStringEscape(VtsSpecial(mask)))
  else
    q := conn.GetTable('select filename from ' + TableName);
  try
    i := 0;
    fFileName := q.FieldByName('filename');
    while not q.Eof do
    begin
      filename := fFileName.AsString;

      if AllowFileCheck(filename) and not FileDoesExist(filename) then
      begin
        if not cbSimulate.Checked and (rgModus.ItemIndex <> modusValidation)
        then
        begin
          conn.ExecSQL('delete from ' + TableName + ' where filename = ' +
            conn.SQLStringEscape(filename));
        end;
        Inc(sumfiles_deleted);
        if cbVerboseLogs.Checked then
          Memo2.Lines.Add('Deleted: ' + filename);
        RedrawStats;
      end;

      Inc(i);
      if i mod 100 = 0 then
      begin
        Label1.Caption := MinimizeName(filename, Label1.Canvas, Label1.Width);
        Application.ProcessMessages;
        if Application.Terminated or StopRequest then
          Abort;
      end;

      q.Next;
    end;
  finally
    FreeAndNil(q);
  end;
end;

procedure TfrmIndexCreator.IndexDrive(initialdir: string);
begin
  if not cbNoDelete.Checked and not cbSimulate.Checked and
    (rgModus.ItemIndex <> modusValidation) then
  begin
    if rgModus.ItemIndex = modusRecreate then
    begin
      DeleteAllFiles(uniqueFilename(IncludeTrailingPathDelimiter
        (initialdir)) + '%');
    end
    else
    begin
      DeleteVanishedFiles
        (uniqueFilename(IncludeTrailingPathDelimiter(initialdir)) + '%');
    end;
  end;

  Rec(IncludeTrailingPathDelimiter(initialdir), '*');
end;

procedure TfrmIndexCreator.Button1Click(Sender: TObject);
begin
  sumsize := 0;
  sumfiles := 0;
  sumfiles_new := 0;
  sumfiles_updated := 0;
  sumfiles_error := 0;
  sumfiles_deleted := 0;
  sumfiles_integrityfail := 0;

  Label1.Caption := 'Please wait...';
  Label5.Caption := '0';
  Label6.Caption := '0';
  Label7.Caption := '0';
  Label9.Caption := '0';
  Label11.Caption := '0';
  Label12.Caption := '0';
  Application.ProcessMessages;

  EnableDisableControls(false);
  try
    if not SysUtils.DirectoryExists(LabeledEdit2.Text) then
    begin
      raise Exception.CreateFmt('Directory %s not found.', [LabeledEdit2.Text]);
    end;

    IndexDrive(LabeledEdit2.Text);

    (*
      if not Application.Terminated or StopRequest then
      begin
      ShowMessage('Finished');
      end;
    *)
  finally
    if not StopRequest then EnableDisableControls(true);
  end;

  if not StopRequest then
  begin
    Beep;
    Label1.Caption := 'Done.';
    Application.ProcessMessages;
  end;
end;

procedure TfrmIndexCreator.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopRequest := true;
  Action := caFree; // TODO: müssen wir warten bis der prozess angehalten ist?
end;

procedure TfrmIndexCreator.FormShow(Sender: TObject);
var
  ini: TMemIniFile;
begin
  ini := frmMain.ini;
  rgModus.ItemIndex := ini.ReadInteger('IndexCreator', 'DefaultMode', modusUpdate);
  cbNoDelete.Checked := ini.ReadBool('IndexCreator', 'DefaultCheckVanished', false);
  cbVerboseLogs.Checked := ini.ReadBool('IndexCreator', 'DefaultVerboseLogs', false);
  cbSimulate.Checked := ini.ReadBool('IndexCreator', 'DefaultSimulate', false);
  LabeledEdit2.Text := ini.ReadString('IndexCreator', 'DefaultDir', 'C:\');
end;

procedure TfrmIndexCreator.Button2Click(Sender: TObject);
begin
  StopRequest := true;
  Close;
end;

procedure TfrmIndexCreator.Button4Click(Sender: TObject);
var
  i: Integer;
  s: string;
begin
  sumsize := 0;
  sumfiles := 0;
  sumfiles_new := 0;
  sumfiles_updated := 0;
  sumfiles_error := 0;
  sumfiles_deleted := 0;

  Label1.Caption := 'Please wait...';
  Label5.Caption := '0';
  Label6.Caption := '0';
  Label7.Caption := '0';
  Label9.Caption := '0';
  Label11.Caption := '0';
  Label12.Caption := '0';
  Application.ProcessMessages;

  EnableDisableControls(false);
  try
    // if fileexists('tmp') then memo1.lines.LoadFromFile('tmp');
    for i := Memo1.Lines.Count - 1 downto 0 do
    begin
      s := Memo1.Lines.strings[i];
      if Trim(s) <> '' then
      begin
        LabeledEdit2.Text := s;

        if not SysUtils.DirectoryExists(LabeledEdit2.Text) then
        begin
          raise Exception.CreateFmt('Directory %s not found.',
            [LabeledEdit2.Text]);
        end;

        IndexDrive(LabeledEdit2.Text);
      end;
      Memo1.Lines.Delete(i);
      // memo1.lines.SaveToFile('tmp');
    end;

    (*
      if not Application.Terminated or StopRequest then
      begin
      ShowMessage('Finished');
      end;
    *)
  finally
    EnableDisableControls(true);
  end;

  Beep;
  Label1.Caption := 'Done.';
  Application.ProcessMessages;
end;

procedure TfrmIndexCreator.EnableDisableControls(enabled: boolean);
begin
  rgModus.enabled := enabled;
  cbNoDelete.enabled := enabled and (rgModus.ItemIndex <> modusValidation);
  cbVerboseLogs.enabled := enabled;
  cbSimulate.enabled := enabled and (rgModus.ItemIndex <> modusValidation);
  Button1.enabled := enabled;
  LabeledEdit2.enabled := enabled;
  Memo1.enabled := enabled;
  Button4.enabled := enabled;
end;

end.
