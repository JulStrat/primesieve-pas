[![Build Status](https://travis-ci.org/JulStrat/primesieve-pas.png?branch=master)](https://travis-ci.org/JulStrat/primesieve-pas)

# primesieve-pas

Pascal bindings for [PrimeSieve](https://github.com/kimwalisch/primesieve) library.

## Pascal API documentation

https://julstrat.github.io/primesieve-pas/

Documentation generated with [PasDoc](https://github.com/pasdoc/pasdoc).

## Requirements

Install ```primesieve``` library.

- Ubuntu - ```sudo apt install libprimesieve-dev```.
- MacOS - ```brew install primesieve```.
- Windows - you can use [libprimesieve.dll](https://github.com/JulStrat/primesieve-pas/tree/loadlib/libprimesieve) built with MinGW.
- Or [build](https://github.com/kimwalisch/primesieve/blob/master/BUILD.md) library from source.

## Usage

```
program store_primes_in_array;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}
{$POINTERMATH ON}

uses primesieve;

type
  PInt32 = ^Int32;

var 
  start, stop, n: UInt64;
  i, size: NativeUInt;
  primes: PInt32;

begin
  start := 0;
  stop := 1000;

  (* store the primes below 1000 *)
  primes := primesieve_generate_primes(start, stop, size, INT32_PRIMES);

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
```

## Building example

Free Pascal compiler - 
```
fpc -B -O3 -CX -XX examples/store_primes_in_array.pas
```

Embarcadero Delphi compiler - 
```
dcc64 -B -O+ -NSSystem examples/store_primes_in_array.pas
...
dcc64 -B -O+ -NSSystem examples/printlets.pas
dcc64 -B -O+ -NSSystem examples/countlets.pas
```

Run - 
```
$ time ./examples/printlets.exe 6 0 1000000
(7, 11, 13, 17, 19, 23)
(97, 101, 103, 107, 109, 113)
(16057, 16061, 16063, 16067, 16069, 16073)
(19417, 19421, 19423, 19427, 19429, 19433)
(43777, 43781, 43783, 43787, 43789, 43793)

real    0m0.038s
user    0m0.000s
sys     0m0.031s

$ time examples/countlets.exe 6 0 1000000000
317

real    0m0.184s
user    0m0.000s
sys     0m0.031s
```

## Links

### General information about prime numbers

- [Prime number](https://en.wikipedia.org/wiki/Prime_number)
- [Twin prime](https://en.wikipedia.org/wiki/Twin_prime)
- [Prime triplet](https://en.wikipedia.org/wiki/Prime_triplet)
- [Prime quadruplet](https://en.wikipedia.org/wiki/Prime_quadruplet)

### Prime sieve

- [Segmented sieve of Eratosthenes](https://github.com/kimwalisch/primesieve/wiki/Segmented-sieve-of-Eratosthenes), by Kim Walisch
- [PrimeSieve links](https://github.com/kimwalisch/primesieve/wiki/Links)
- [The Genuine Sieve of Eratosthenes](https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf)
