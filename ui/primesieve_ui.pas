unit primesieve_ui;
{$POINTERMATH ON}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  primesieve;

type
  TfrmPrimeSieve = class(TForm)
    memOutput: TMemo;
    btnSieve: TButton;
    edStart: TEdit;
    edStop: TEdit;
    stbResults: TStatusBar;
    procedure btnSieveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrimeSieve: TfrmPrimeSieve;

implementation

{$R *.dfm}

procedure TfrmPrimeSieve.btnSieveClick(Sender: TObject);
var
  start, stop: UInt64;
  primes: PUInt64;
  i, psize: NativeUInt;
  plist: TStringList;

begin
  plist := TStringList.Create;
  start := StrToUInt64(edStart.Text);
  stop := StrToUInt64(edStop.Text);
  memOutput.Clear;
  stbResults.Panels[0].Text := '';

  primes := primesieve_generate_primes(start, stop, psize, UINT64_PRIMES);
  for i := 0 to psize - 1 do
    plist.Add(primes[i].ToString());
  memOutput.Text := plist.Text;

  plist.Free;
  primesieve_free(primes);
  self.stbResults.Panels[0].Text := psize.ToString();
end;

end.
