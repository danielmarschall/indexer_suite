object frmExplorer: TfrmExplorer
  Left = 0
  Top = 0
  Caption = 'Offline Explorer'
  ClientHeight = 370
  ClientWidth = 787
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    787
    370)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object SpeedButton1: TSpeedButton
    Left = 11
    Top = 4
    Width = 23
    Height = 22
    Caption = '..'
    OnClick = SpeedButton1Click
  end
  object ListView1: TListView
    Left = 8
    Top = 32
    Width = 767
    Height = 330
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Filename'
        Width = 300
      end
      item
        Caption = 'Type'
      end>
    LargeImages = ImageListLarge
    PopupMenu = PopupMenu1
    SmallImages = ImageListSmall
    TabOrder = 0
    OnDblClick = ListView1DblClick
    OnKeyDown = ListView1KeyDown
  end
  object Button2: TButton
    Left = 694
    Top = 8
    Width = 81
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'View'
    TabOrder = 1
    OnClick = Button2Click
  end
  object ImageListSmall: TImageList
    Left = 32
    Top = 32
  end
  object ImageListLarge: TImageList
    Left = 104
    Top = 32
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 456
    Top = 264
  end
  object PopupMenu1: TPopupMenu
    Left = 416
    Top = 152
    object Checkifdirisredundant1: TMenuItem
      Caption = 'Check if item is redundant'
      OnClick = Checkifdirisredundant1Click
    end
    object ReIndexthisitem1: TMenuItem
      Caption = 'Re-Index this item'
      OnClick = ReIndexthisitem1Click
    end
  end
end
