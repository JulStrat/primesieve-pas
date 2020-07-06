(** @example count_primes.pas
 *  Pascal program that shows how to count primes. *)

program count_primes;
{$ifdef FPC}
{$mode Delphi}
{$endif}

uses SysUtils, primesieve;

var
  count: UInt64;

begin
  load_libprimesieve();
  count := primesieve_count_primes(0, 1000);
  WriteLn(Format('Primes below 1000 = %d', [count]));
  unload_libprimesieve();
end.
