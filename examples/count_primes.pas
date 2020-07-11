(** @example count_primes.pas
 *  Pascal program that shows how to count primes. *)

program count_primes;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIR}

{$APPTYPE CONSOLE}

uses SysUtils, primesieve;

var
  count: UInt64;

begin
  count := primesieve_count_primes(0, 1000);
  WriteLn(Format('Primes below 1000 = %d', [count]));
end.
