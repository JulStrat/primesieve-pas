object frmPrimeSieve: TfrmPrimeSieve
  Left = 0
  Top = 0
  Caption = 'PrimeSieve'
  ClientHeight = 419
  ClientWidth = 679
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object memOutput: TMemo
    Left = 0
    Top = 112
    Width = 679
    Height = 280
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object btnSieve: TButton
    Left = 256
    Top = 24
    Width = 137
    Height = 60
    Caption = 'Sieve'
    TabOrder = 2
    OnClick = btnSieveClick
  end
  object edStart: TEdit
    Left = 48
    Top = 24
    Width = 202
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object edStop: TEdit
    Left = 48
    Top = 60
    Width = 202
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object stbResults: TStatusBar
    Left = 0
    Top = 392
    Width = 679
    Height = 27
    Panels = <
      item
        Width = 50
      end>
  end
end
