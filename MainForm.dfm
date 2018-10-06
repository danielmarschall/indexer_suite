object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ViaThinkSoft Offline File Indexer Suite'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 112
    Top = 88
    object IndexCreator1: TMenuItem
      Caption = 'Index Creator'
      OnClick = IndexCreator1Click
    end
    object OfflineExplorer1: TMenuItem
      Caption = 'Offline Explorer'
      OnClick = OfflineExplorer1Click
    end
    object RedundancyVerifier1: TMenuItem
      Caption = 'Redundancy Verifier'
      OnClick = RedundancyVerifier1Click
    end
    object Finder1: TMenuItem
      Caption = 'Finder'
      OnClick = Finder1Click
    end
  end
  object ADOConnection1: TADOConnection
    Left = 264
    Top = 88
  end
end
