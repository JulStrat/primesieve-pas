unit primesieve_ui;
{$POINTERMATH ON}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  primesieve;

type
  TfrmPrimeSieve = class(TForm)
    memOutput: TMemo;
    btnSieve: TButton;
    edtStart: TEdit;
    edtStop: TEdit;
    stbResults: TStatusBar;
    lbxSieveSize: TListBox;
    lbxThreads: TListBox;
    lblStart: TLabel;
    lblStop: TLabel;
    lblSieveSize: TLabel;
    lblThreads: TLabel;
    procedure btnSieveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    numThreads: Integer;
    sieveSize: Integer;
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
  start := StrToUInt64(edtStart.Text);
  stop := StrToUInt64(edtStop.Text);
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

procedure TfrmPrimeSieve.FormCreate(Sender: TObject);
var
  idx: Integer;

begin
  sieveSize := primesieve_get_sieve_size();
  numThreads := primesieve_get_num_threads();

  idx := lbxSieveSize.Items.IndexOf(sieveSize.ToString());
  if idx > -1 then
    self.lbxSieveSize.ItemIndex := idx;

  idx := self.lbxThreads.Items.IndexOf(numThreads.ToString());
  if idx > -1 then
    self.lbxThreads.ItemIndex := idx;
end;

end.
