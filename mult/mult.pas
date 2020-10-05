{
  Mult - Multiplicative number theory functions.@br
  Copyright (C) 2020 I. Kakoulidis, <ioulianos.kakoulidis@hotmail.com>@br
  https://github.com/JulStrat/primesieve-pas

  primesieve - library for fast prime number generation.@br
  Copyright (C) 2019 Kim Walisch, <kim.walisch@gmail.com>@br
  https://github.com/kimwalisch/primesieve  
  
  primesieve-pas - FPC/Delphi API for primesieve library.@br
  Copyright (C) 2020 I. Kakoulidis, <ioulianos.kakoulidis@hotmail.com>@br
  https://github.com/JulStrat/primesieve-pas
  
  This file is distributed under the BSD 2-Clause License.
}
unit mult;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

{$INLINE ON}
interface

uses primesieve;

(*
  Euler totient function Phi(n): count numbers <= n and prime to n.
  OEIS: https://oeis.org/A000010
*)
function Phi(n: UInt64): UInt64;

(*
  Möbius (or Moebius) function mu(n).
  mu(1) = 1; mu(n) = (-1)^k if n is the product of k different primes; otherwise mu(n) = 0.
  OEIS: https://oeis.org/A008683
*)
function Mu(n: UInt64): integer;

(*
  tau(n), the number of divisors of n.
  OEIS: https://oeis.org/A000005
*)
function Tau(n: UInt64): UInt64;

(*
  Rad(n), product of the distinct prime factors of n.
  OEIS: https://oeis.org/A007947
*)
function Rad(n: UInt64): UInt64;

implementation

var
  it: primesieve_iterator;

function Phi(n: UInt64): UInt64;
var
  p: UInt64;

begin
  primesieve_skipto(it, 0, primesieve_get_max_stop());
  p := primesieve_next_prime(it);
  Result := n;

  while p * p <= n do
  begin
    if n mod p = 0 then
    begin 
      Result := Result div p * (p-1);
      repeat
        n := n div p;
      until n mod p <> 0;
    end;  
    p := primesieve_next_prime(it);
  end;
  if n > 1 then
    Result := Result div n * (n-1);
end;

function Mu(n: UInt64): integer;
var
  p: UInt64;

begin
  primesieve_skipto(it, 0, primesieve_get_max_stop());
  p := primesieve_next_prime(it);
  Result := 1;

  while p * p <= n do
  begin
    if n mod (p*p) = 0 then
      Exit(0);
    if n mod p = 0 then
    begin
      Result := -Result;
      n := n div p;
    end;
    p := primesieve_next_prime(it);
  end;
  if n > 1 then
    Result := -Result;
end;

function Tau(n: UInt64): UInt64;
var
  p: UInt64;
  exp: UInt64;

begin
  primesieve_skipto(it, 0, primesieve_get_max_stop());
  p := primesieve_next_prime(it);
  Result := 1;

  while p * p <= n do
  begin
    exp := 0;
    while n mod p = 0 do
    begin  	
      Inc(exp);	
      n := n div p;
    end;
    Result := (exp+1) * Result;
    p := primesieve_next_prime(it);
  end;
  if n > 1 then
    Result := 2 * Result;
end;

function Rad(n: UInt64): UInt64;
var
  p: UInt64;

begin
  primesieve_skipto(it, 0, primesieve_get_max_stop());
  p := primesieve_next_prime(it);
  Result := 1;

  while p * p <= n do
  begin
    if n mod p = 0 then
    begin 
      Result := Result * p;
      repeat
        n := n div p;
      until n mod p <> 0;
    end;  
    p := primesieve_next_prime(it);
  end;
  if n > 1 then
    Result := Result * n;
end;

initialization

  primesieve_init(it);

finalization

  primesieve_free_iterator(it);

end.
