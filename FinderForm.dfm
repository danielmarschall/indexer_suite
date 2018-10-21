object frmFinder: TfrmFinder
  Left = 0
  Top = 0
  Caption = 'Finder'
  ClientHeight = 411
  ClientWidth = 712
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
    712
    411)
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 606
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 696
    Height = 364
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Results')
    TabOrder = 1
  end
  object Button1: TButton
    Left = 629
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Search'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
end
