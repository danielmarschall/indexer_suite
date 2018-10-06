object frmIndexCreator: TfrmIndexCreator
  Left = 0
  Top = 0
  Caption = 'ViaThinkSoft Directory Hasher'
  ClientHeight = 611
  ClientWidth = 877
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    877
    611)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 279
    Width = 827
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Ready'
    ExplicitWidth = 465
  end
  object Label2: TLabel
    Left = 24
    Top = 298
    Width = 113
    Height = 13
    Caption = 'Size of processed data:'
  end
  object Label3: TLabel
    Left = 24
    Top = 317
    Width = 111
    Height = 13
    Caption = 'Sum of processed files:'
  end
  object Label4: TLabel
    Left = 24
    Top = 336
    Width = 47
    Height = 13
    Caption = 'New files:'
  end
  object Label5: TLabel
    Left = 160
    Top = 297
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label6: TLabel
    Left = 160
    Top = 316
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label7: TLabel
    Left = 160
    Top = 335
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label8: TLabel
    Left = 24
    Top = 355
    Width = 67
    Height = 13
    Caption = 'Updated files:'
  end
  object Label9: TLabel
    Left = 160
    Top = 355
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label10: TLabel
    Left = 24
    Top = 374
    Width = 33
    Height = 13
    Caption = 'Errors:'
  end
  object Label11: TLabel
    Left = 160
    Top = 374
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label12: TLabel
    Left = 160
    Top = 393
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label13: TLabel
    Left = 24
    Top = 393
    Width = 41
    Height = 13
    Caption = 'Deleted:'
  end
  object Label14: TLabel
    Left = 584
    Top = 8
    Width = 53
    Height = 13
    Caption = 'Process list'
  end
  object Button1: TButton
    Left = 320
    Top = 185
    Width = 169
    Height = 33
    Caption = 'Start'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object LabeledEdit2: TLabeledEdit
    Left = 24
    Top = 56
    Width = 273
    Height = 21
    EditLabel.Width = 113
    EditLabel.Height = 13
    EditLabel.Caption = 'Drive / Label to analyze'
    PopupMenu = PopupMenu1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 320
    Top = 224
    Width = 169
    Height = 33
    Caption = 'Exit'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 584
    Top = 25
    Width = 283
    Height = 193
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object Button4: TButton
    Left = 778
    Top = 224
    Width = 89
    Height = 33
    Anchors = [akTop, akRight]
    Caption = 'Go'
    TabOrder = 4
    OnClick = Button4Click
  end
  object cbNoDelete: TCheckBox
    Left = 320
    Top = 128
    Width = 241
    Height = 17
    Caption = 'Disable deleting/truncating of vanished data'
    TabOrder = 5
  end
  object Memo2: TMemo
    Left = 24
    Top = 440
    Width = 843
    Height = 156
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Log messages')
    ScrollBars = ssBoth
    TabOrder = 6
    WordWrap = False
  end
  object cbVerboseLogs: TCheckBox
    Left = 772
    Top = 417
    Width = 97
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Verbose logging'
    TabOrder = 7
  end
  object cbSimulate: TCheckBox
    Left = 320
    Top = 151
    Width = 97
    Height = 17
    Caption = 'Simulate'
    TabOrder = 8
  end
  object rgModus: TRadioGroup
    Left = 320
    Top = 9
    Width = 185
    Height = 105
    Caption = 'Mode'
    ItemIndex = 0
    Items.Strings = (
      'Update index'
      'Completely re-create index'
      'Verify checksums')
    TabOrder = 9
    OnClick = rgModusClick
  end
  object PopupMenu1: TPopupMenu
    Left = 272
    Top = 80
    object Copyuniquepathtoclipboard1: TMenuItem
      Caption = 'Copy unique path to clipboard'
      OnClick = Copyuniquepathtoclipboard1Click
    end
  end
end
