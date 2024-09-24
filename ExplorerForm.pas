unit ExplorerForm;

// TODO: Detailansicht: - Skalieren der Spalten
//                      - Mehr Eigenschaften zeigen, die in der SQL-Datenbank sind

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  System.ImageList, Vcl.ImgList, Data.DB, Data.Win.ADODB, Vcl.Samples.Gauges,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.Menus;

type
  TfrmExplorer = class(TForm)
    ListView1: TListView;
    Button2: TButton;
    ImageListSmall: TImageList;
    ImageListLarge: TImageList;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    Checkifdirisredundant1: TMenuItem;
    ReIndexthisitem1: TMenuItem;
    procedure Button2Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure Checkifdirisredundant1Click(Sender: TObject);
    procedure ReIndexthisitem1Click(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FCurrentFolder: string;
    FCurrentDepth: integer;
    procedure SetCurrentFolder(const Value: string);
    procedure SetCurrentDepth(const Value: integer);
    property CurrentFolder: string read FCurrentFolder write SetCurrentFolder;
    property CurrentDepth: integer read FCurrentDepth write SetCurrentDepth;
  protected
    procedure FillListView(sl: TStrings);
  public
    procedure OpenFolder(const folder: string);
    procedure ListDevices;
    procedure PrevFolder;
    function conn: TAdoConnection;
    function TableName: string;
  end;

implementation

{$R *.dfm}

uses
  ShellAPI, System.Types, AdoConnHelper, RedundancyForm, IndexCreatorForm, MainForm,
  IniFiles;

function AddTransparentIconToImageList(ImageList: TImageList; Icon: TIcon; DoGreyscale: boolean=False): integer;
  // http://www.delphipages.com/forum/showthread.php?t=183999

  function RealIconSize(H: HIcon): TPoint;
  // http://www.delphipages.com/forum/showthread.php?t=183999
  var
    IconInfo: TIconInfo;
    bmpmask: TBitmap;
  begin
    result := Point(0, 0);

    if H <> 0 then
    begin
      bmpmask := TBitmap.Create;
      try
        IconInfo.fIcon := true;
        try
          GetIconInfo(H, IconInfo);
          bmpmask.Handle := IconInfo.hbmMask;
          bmpmask.Dormant; //lets us free the resource without 'losing' the bitmap
        finally
          DeleteObject(IconInfo.hbmMask);
          DeleteObject(IconInfo.hbmColor)
        end;
        result := Point(bmpmask.Width, bmpmask.Height);
      finally
        bmpmask.Free;
      end;
    end;
  end;

  function ToGray(PixelColor: Longint): Longint;
  var
    Red, Green, Blue, Gray: Byte;
  begin
    Red    := PixelColor;
    Green  := PixelColor shr 8;
    Blue   := PixelColor shr 16;
    Gray   := Round(0.299 * Red + 0.587 * Green + 0.114 * Blue);
    result := Gray + Gray shl 8 + Gray shl 16;
  end;

var
  buffer, mask: TBitmap;
  p: TPoint;
  x, y: integer;
begin
  // result := ImageList.AddIcon(ico);
  // --> In Delphi 6, Icons with half-transparency have a black border (e.g. in ListView)

  p := RealIconSize(icon.handle);

  buffer := TBitmap.Create;
  mask := TBitmap.Create;
  try
    buffer.PixelFormat := pf24bit;
    mask.PixelFormat := pf24bit;

    buffer.Width := p.X;
    buffer.Height := p.Y;
    buffer.Canvas.Draw(0, 0, icon);
    buffer.Transparent := true;
    buffer.TransparentColor := buffer.Canvas.Pixels[0,0];

    if (ImageList.Width <> p.X) or (ImageList.Height <> p.Y) then
    begin
      ImageList.Width := p.X;
      ImageList.Height := p.Y;
    end;

    // create a mask for the icon.
    mask.Assign(buffer);
    mask.Canvas.Brush.Color := buffer.Canvas.Pixels[0, buffer.Height -1];
    mask.Monochrome := true;

    if DoGreyscale then
    begin
      for x := 0 to buffer.Width - 1 do
      begin
        for y := 0 to buffer.Height - 1 do
        begin
          buffer.Canvas.Pixels[x, y] := ToGray(buffer.Canvas.Pixels[x, y]);
        end;
      end;
    end;

    result := ImageList.Add(buffer, mask);
  finally
    mask.Free;
    buffer.Free;
  end;
end;

procedure TfrmExplorer.Button2Click(Sender: TObject);
begin
  // TODO: Refreshen wegen Anzeigefehler
  ListView1.ViewStyle := TViewStyle((Ord(ListView1.ViewStyle)+1) mod (Ord(High(TViewStyle))+1));
end;

procedure TfrmExplorer.Checkifdirisredundant1Click(Sender: TObject);
begin
  if ListView1.ItemIndex = -1 then exit;

  with TfrmRedundancy.Create(Owner) do
  begin
    Edit1.Text := CurrentFolder + ListView1.Selected.Caption;
  end;
end;

function TfrmExplorer.conn: TAdoConnection;
begin
  result := frmMain.ADOConnection1;
end;

procedure TfrmExplorer.FillListView(sl: TStrings);
var
  s: string;
  i: Integer;
  Icon: TIcon;
  Extention : string;
  FileInfo : SHFILEINFO;
  attr: Cardinal;
begin
  ListView1.Clear;
  Icon := TIcon.Create;
  ImageListSmall.Clear;
  ImageListSmall.Width := 16;
  ImageListSmall.Height := 16;
  ImageListLarge.Clear;
  ImageListLarge.Width := 32;
  ImageListLarge.Height := 32;
  for i := 0 to sl.Count-1 do
  begin
    s := sl.Strings[i];
    with ListView1.Items.Add do
    begin
      Extention := '*' + ExtractFileExt(s);

      if Pos('\', s) = 0 then
      begin
        attr := FILE_ATTRIBUTE_NORMAL;
        Caption := s;
        Data := Pointer(0);
      end
      else
      begin
        attr := FILE_ATTRIBUTE_DIRECTORY;
        Caption := Copy(s, 1, Length(s)-1); // remove trailing "\"
        Data := Pointer(1);
      end;

      {$REGION 'File extention name'}
      SHGetFileInfo(PChar(Extention),
                    attr,
                    FileInfo,
                    SizeOf(FileInfo),
                    SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES
                    );
      SubItems.Add(FileInfo.szTypeName);
      {$ENDREGION}

      {$REGION 'Small icon'}
      SHGetFileInfo(PChar(Extention),
                    attr,
                    FileInfo,
                    SizeOf(FileInfo),
                    SHGFI_ICON or SHGFI_SMALLICON or
                    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES
                    );
      Icon.Handle := FileInfo.hIcon;
      AddTransparentIconToImageList(ImageListSmall, Icon, false); // ImageListSmall.AddIcon(Icon);
      {$ENDREGION}

      {$REGION 'Large icon'}
      SHGetFileInfo(PChar(Extention),
                    attr,
                    FileInfo,
                    SizeOf(FileInfo),
                    SHGFI_ICON or SHGFI_LARGEICON or
                    SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES
                    );
      Icon.Handle := FileInfo.hIcon;
      AddTransparentIconToImageList(ImageListLarge, Icon, false); // ImageListLarge.AddIcon(Icon);
      {$ENDREGION}

      ImageIndex := i;
    end;
  end;
  Icon.Free;
end;

procedure TfrmExplorer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmExplorer.FormShow(Sender: TObject);
//var
//  ini: TMemIniFile;
begin
  //ini := frmMain.ini;
  // Edit1.Text := ini.ReadString('Explorer', 'DefaultDir', '');
end;

procedure TfrmExplorer.ListDevices;
var
  q: TADODataSet;
  sl: TStringList;
begin
  label1.Caption := '*** PLEASE WAIT ***';
  Application.ProcessMessages;

  sl := TStringList.Create;

  q := conn.GetTable('select distinct left(filename,charindex('':\'',filename,0)) from '+TableName+' where filename not like ''\\%'';');
  while not q.Eof do
  begin
    sl.Add(q.Fields[0].AsString+'\'); // e.g. "C:" or "EHDD:" for ViaThinkSoft
    q.Next;
  end;
  q.Free;

  q := conn.GetTable('select distinct left(filename,charindex(''\'',filename,3)-1) from '+TableName+' where filename like ''\\%'' and filename not like ''\\?\%'';');
  while not q.Eof do
  begin
    sl.Add(q.Fields[0].AsString+'\'); // e.g. "\\server1"
    q.Next;
  end;
  q.Free;

  q := conn.GetTable('select distinct left(filename,charindex(''\'',filename,5)-1) from '+TableName+' where filename like ''\\?\%'';');
  while not q.Eof do
  begin
    sl.Add(q.Fields[0].AsString+'\'); // e.g. "\\?\Volume{560e8251-2b6a-4ab7-82fc-d03df4d93538}"
    q.Next;
  end;
  q.Free;

  FillListView(sl);
  CurrentFolder := '';
  CurrentDepth := 0;
end;

procedure TfrmExplorer.ListView1DblClick(Sender: TObject);
begin
  if ListView1.ItemIndex = -1 then exit;
  if ListView1.Selected.Data = Pointer(0) then
  begin
    // Ist eine Datei
    ShowMessageFmt('Filename: %s', [ListView1.Selected.Caption]);
  end;
  if ListView1.Selected.Data = Pointer(1) then
  begin
    // Ist ein Verzeichnis
    OpenFolder(CurrentFolder + ListView1.Selected.Caption + '\');
  end;
end;

procedure TfrmExplorer.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_BACK then
  begin
    if SpeedButton1.Enabled then SpeedButton1.Click;
    Key := 0;
  end;
  if Key = VK_RETURN then
  begin
    ListView1DblClick(ListView1);
    Key := 0;
  end;
end;

procedure TfrmExplorer.OpenFolder(const folder: string);
var
  sl: TStringList;
  q: TADODataSet;
  p: Integer;
  DirName: string;
  locFolder: string;
  relfilepath: string;
  folders: TStringList;
begin
  label1.Caption := '*** PLEASE WAIT ***';
  Application.ProcessMessages;

  sl := TStringList.Create;
  folders := TStringList.Create;
  try
    //q := conn.GetTable('select filename from '+TableName+' where filename like '+conn.SQLStringEscape(Folder+'%'));
    q := conn.GetTable('select distinct left(filename,charindex(''\'',filename+''\'','+inttostr(Length(Folder)+1)+')) from '+TableName+' where filename like '+conn.SQLStringEscape(Folder+'%'));

    //gauge1.MaxValue := q.RecordCount;
    //Gauge1.Progress := 0;
    while not q.EOF do
    begin
      relfilepath := q.Fields[0].AsString;
      Delete(relfilepath, 1, Length(Folder));
      p := Pos('\', relfilepath);
      if p > 0 then
      begin
        // Ist ein Verzeichnis
        DirName := Copy(relfilepath, 1, p);
        if folders.IndexOf(DirName) = -1 then folders.Add(DirName);
      end
      else
      begin
        sl.Add(ExtractFileName(relfilepath));
      end;
      q.Next;
      //Gauge1.Progress := Gauge1.Progress + 1;
    end;
    q.Free;
    for locFolder in folders do
    begin
      sl.Add(locFolder);
    end;
    FillListView(sl);
    CurrentFolder := folder;
    CurrentDepth := CurrentDepth + 1;
  finally
    sl.Free;
    folders.Free;
  end;
end;

procedure TfrmExplorer.PrevFolder;
begin
  label1.Caption := '*** PLEASE WAIT ***';
  Application.ProcessMessages;

  if CurrentDepth = 1 then
  begin
    ListDevices;
  end
  else
  begin
    OpenFolder(IncludeTrailingPathDelimiter(ExtractFileDir(Copy(CurrentFolder,1,Length(CurrentFolder)-1))));
    CurrentDepth := CurrentDepth - 2;
  end;
end;

procedure TfrmExplorer.ReIndexthisitem1Click(Sender: TObject);
begin
  if ListView1.ItemIndex = -1 then exit;

  with TfrmIndexCreator.Create(Owner) do
  begin
    LabeledEdit2.Text := CurrentFolder + ListView1.Selected.Caption;
  end;
end;

procedure TfrmExplorer.SetCurrentDepth(const Value: integer);
begin
  SpeedButton1.Visible := Value > 0;
  FCurrentDepth := Value;
end;

procedure TfrmExplorer.SetCurrentFolder(const Value: string);
begin
  Label1.Caption := Value;
  FCurrentFolder := Value;
end;

procedure TfrmExplorer.SpeedButton1Click(Sender: TObject);
begin
  PrevFolder;
end;

function TfrmExplorer.TableName: string;
begin
  result := frmMain.TableName;
end;

procedure TfrmExplorer.Timer1Timer(Sender: TObject);
begin
  timer1.Enabled := false;
  ListDevices;
end;

end.
