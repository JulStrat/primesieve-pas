(** @example nth_prime.pas
 *  Pascal program that finds the nth prime. *)
program nth_prime;
{$ifdef FPC}
{$mode Delphi}
{$endif}

uses SysUtils, primesieve;

var
  n: Int64;
  prime: UInt64;

begin
  n := 1000;
  load_libprimesieve();
  if ParamCount > 0 then
    TryStrToInt64(ParamStr(1), n);
  prime := primesieve_nth_prime(n, 0);
  WriteLn(Format('%dth prime = %d', [n, prime]));
  unload_libprimesieve();
end.
