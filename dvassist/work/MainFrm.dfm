object Main: TMain
  Left = 355
  Top = 314
  Width = 206
  Height = 145
  Caption = 'Global HotKey Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 26
    Height = 13
    Caption = 'ALT_'
  end
  object Edit1: TEdit
    Left = 56
    Top = 16
    Width = 121
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 0
    Text = 'A'
  end
  object Button1: TButton
    Left = 56
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Set Hotkey'
    TabOrder = 1
    OnClick = Button1Click
  end
end
