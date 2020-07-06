[![Build Status](https://travis-ci.org/JulStrat/primesieve-pas.png?branch=master)](https://travis-ci.org/JulStrat/primesieve-pas)

# primesieve-pas

Pascal bindings for [PrimeSieve](https://github.com/kimwalisch/primesieve) library.

# Requirements

Install ```primesieve``` library.

- Ubuntu - ```sudo apt install libprimesieve-dev```.
- MacOS - ```brew install primesieve```.
- Windows - you can use [libprimesieve.dll](https://github.com/JulStrat/primesieve-pas/tree/loadlib/libprimesieve).
- Or [build](https://github.com/kimwalisch/primesieve/blob/master/BUILD.md) library from source.

# Usage
```
(** @example store_primes_in_array.pas
 *  Store primes in a array. *)
program store_primes_in_array;
{$ifdef FPC}
{$mode Delphi}
{$endif}
uses primesieve;

var 
  start, stop, n: UInt64;
  i, size: NativeUInt;
  primes: PInteger;

begin
  start := 0;
  stop := 1000;

  load_libprimesieve();
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
  
  unload_libprimesieve();
end.
```