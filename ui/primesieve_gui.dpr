program primesieve_gui;

uses
  Vcl.Forms,
  primesieve_ui in 'primesieve_ui.pas' {frmPrimeSieve},
  primesieve in '..\primesieve.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPrimeSieve, frmPrimeSieve);
  Application.Run;
end.
