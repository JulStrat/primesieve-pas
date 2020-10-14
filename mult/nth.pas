{
  Nth - Number theory algorithms for unsigned 64bit integers.@br
  Copyright (C) 2020 I. Kakoulidis, <ioulianos.kakoulidis@hotmail.com>@br
  https://github.com/JulStrat/primesieve-pas

  This file is distributed under the BSD 2-Clause License.
}
unit nth;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

{$INLINE ON}
interface

(*
  Miller–Rabin deterministic primality test.

  @returns(@true if @italic(n) is prime number, @false otherwise)
*)
function MillerRabin(n: UInt64): boolean;

(*
  Pollard Rho (Greek ρ letter) factorization algorithm.
  https://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm
*)
function PollardRho(n: UInt64; x: UInt64 = 2; c: UInt64 = 1): UInt64;

implementation

uses modar;

function MillerRabin(n: UInt64): boolean;
var
  a, d, x: UInt64;
  i, r: integer;
  witness: boolean;

begin
  case n of
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37: Exit(True);
  end;
  if ((n and 1) = 0) or (n = 1) then Exit(False);
  d := n - 1;
  r := 0;

  while (d and 1) = 0 do
  begin
    Inc(r);
    d := d shr 1;
  end;

  for a in [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37] do 
  begin
    x := PowMod(a, d, n);
    if (x = 1) or (x = n - 1) then continue;
    witness := false;

    for i := 1 to r - 1 do
    begin
      x := MulMod(x, x, n);
      if x = n - 1 then
      begin
        witness := true;
        break;
      end;
    end;

    if witness = false then Exit(false);
  end;
  Result := true;
end;

function PollardRho(n: UInt64; x: UInt64 = 2; c: UInt64 = 1): UInt64;
var
  y, d: UInt64;

begin
  y := x;
  d := 1;

  while d = 1 do
  begin
    x := AddMod(MulMod(x, x, n), c, n);
    y := AddMod(MulMod(y, y, n), c, n);
    y := AddMod(MulMod(y, y, n), c, n);
    if y >= x then d := GCD(n, y - x)
    else d := GCD(n, x - y);
  end;
  Result := d;
end;

end.
