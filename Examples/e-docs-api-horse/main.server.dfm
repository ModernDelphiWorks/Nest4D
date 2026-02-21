object FormServer: TFormServer
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'VCL'
  ClientHeight = 137
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    364
    137)
  TextHeight = 13
  object LabelStatus: TLabel
    Left = 129
    Top = 90
    Width = 107
    Height = 25
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Caption = 'LabelStatus'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 364
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 362
    object Label1: TLabel
      Left = 16
      Top = 17
      Width = 63
      Height = 13
      Caption = 'Port number:'
    end
    object PortNumberEdit: TEdit
      Left = 85
      Top = 14
      Width = 82
      Height = 21
      TabOrder = 0
      Text = '9000'
    end
  end
end
