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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object lblStart: TLabel
    Left = 12
    Top = 31
    Width = 28
    Height = 16
    Alignment = taRightJustify
    Caption = 'Start'
  end
  object lblStop: TLabel
    Left = 14
    Top = 65
    Width = 26
    Height = 16
    Alignment = taRightJustify
    Caption = 'Stop'
  end
  object lblSieveSize: TLabel
    Left = 428
    Top = 26
    Width = 87
    Height = 16
    Alignment = taRightJustify
    Caption = 'Sieve Size (KB)'
  end
  object lblThreads: TLabel
    Left = 468
    Top = 57
    Width = 47
    Height = 16
    Alignment = taRightJustify
    Caption = 'Threads'
  end
  object memOutput: TMemo
    Left = 0
    Top = 104
    Width = 679
    Height = 288
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object btnSieve: TButton
    Left = 278
    Top = 21
    Width = 137
    Height = 60
    Caption = 'Sieve'
    TabOrder = 2
    OnClick = btnSieveClick
  end
  object edtStart: TEdit
    Left = 60
    Top = 23
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
  object edtStop: TEdit
    Left = 60
    Top = 57
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
  object lbxSieveSize: TListBox
    Left = 533
    Top = 23
    Width = 121
    Height = 24
    Items.Strings = (
      '16'
      '32'
      '64'
      '128'
      '256'
      '512'
      '1024'
      '2048'
      '4096')
    TabOrder = 3
  end
  object lbxThreads: TListBox
    Left = 533
    Top = 57
    Width = 121
    Height = 24
    Items.Strings = (
      '1'
      '2'
      '4'
      '8')
    TabOrder = 4
  end
end
