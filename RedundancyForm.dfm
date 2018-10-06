object frmRedundancy: TfrmRedundancy
  Left = 0
  Top = 0
  Caption = 'Redundant Directory Verifier'
  ClientHeight = 528
  ClientWidth = 613
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
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    613
    528)
  PixelsPerInch = 96
  TextHeight = 13
  object Gauge1: TGauge
    Left = 8
    Top = 481
    Width = 591
    Height = 33
    Anchors = [akLeft, akRight, akBottom]
    Progress = 0
    ExplicitTop = 456
    ExplicitWidth = 544
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 196
    Height = 13
    Caption = 'Check if following directory is redundant:'
  end
  object Label2: TLabel
    Left = 8
    Top = 85
    Width = 61
    Height = 13
    Caption = 'Unique Files:'
  end
  object Label3: TLabel
    Left = 8
    Top = 462
    Width = 46
    Height = 13
    Caption = 'Progress:'
  end
  object Label4: TLabel
    Left = 72
    Top = 462
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 462
    Top = 51
    Width = 137
    Height = 33
    Anchors = [akTop, akRight]
    Caption = 'Check'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 104
    Width = 592
    Height = 353
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 592
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 51
    Width = 329
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 3
    Text = 'Mode 1: Check if entry in index table is unique'
    Items.Strings = (
      'Mode 1: Check if entry in index table is unique'
      
        'Mode 2: Check if local folder has files which are not in the ind' +
        'ex')
  end
end
