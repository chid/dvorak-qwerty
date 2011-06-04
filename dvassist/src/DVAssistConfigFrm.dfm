object DVAssistConfigForm: TDVAssistConfigForm
  Left = 700
  Top = 473
  BorderStyle = bsDialog
  Caption = 'Configure'
  ClientHeight = 168
  ClientWidth = 281
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 287
  DefaultMonitor = dmPrimary
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object pnlClient: TPanel
    Left = 0
    Top = 0
    Width = 281
    Height = 127
    Align = alClient
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 20
      Width = 105
      Height = 13
      AutoSize = False
      Caption = 'Toggle Shortcut Key:'
    end
    object lblBeware: TLabel
      Left = 16
      Top = 48
      Width = 241
      Height = 41
      AutoSize = False
      Caption = 
        'Note: use of letters A-Z and punctuation is not recommended as t' +
        'hese change location when the layout is toggled.'
      WordWrap = True
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 127
    Width = 281
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      281
      41)
    object btnCancel: TButton
      Left = 198
      Top = 8
      Width = 75
      Height = 25
      Action = actCancel
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
    object btnOK: TButton
      Left = 110
      Top = 8
      Width = 75
      Height = 25
      Action = actOK
      Anchors = [akTop, akRight]
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    Left = 136
    Top = 88
    object actOK: TAction
      Caption = 'OK'
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
  end
end
