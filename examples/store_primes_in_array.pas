(*  @example store_primes_in_array.pas
 *  Store primes in a array. *)
program store_primes_in_array;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}
{$POINTERMATH ON}

uses primesieve;

var 
  start, stop, n: UInt64;
  i, size: NativeUInt;
  primes: PInteger;

begin
  start := 0;
  stop := 1000;

  (* store the primes below 1000 *)
  primes := primesieve_generate_primes(start, stop, size, INT_PRIMES);

  for i := 0 to size-1 do
    WriteLn(primes[i]);

  primesieve_free(primes);
  n := 1000;

  (* store the first 1000 primes *)
  primes := primesieve_generate_n_primes(n, start, INT_PRIMES);

  for i := 0 to n-1 do
    WriteLn(primes[i]);

  primesieve_free(primes);
end.