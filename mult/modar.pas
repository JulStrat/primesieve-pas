{
  Modar - Modular arithmetic for unsigned 64bit integers.@br
  Copyright (C) 2020 I. Kakoulidis, <ioulianos.kakoulidis@hotmail.com>@br
  https://github.com/JulStrat/primesieve-pas

  This file is distributed under the BSD 2-Clause License.
}
unit modar;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

interface

function AddMod(a, b: UInt64; m: UInt64): UInt64;
function MulMod(a, b: UInt64; m: UInt64): UInt64;
function PowMod(b, e: UInt64; m: UInt64): UInt64;

implementation

function AddMod(a, b: UInt64; m: UInt64): UInt64;
begin
  if a >= m then a := a mod m;
  if b >= m then b := b mod m;

  if b >= m - a then
    Result := b - (m - a)
  else
    Result := b + a;
end;

function MulMod(a, b: UInt64; m: UInt64): UInt64;
begin
  if a >= m then a := a mod m;
  if b >= m then b := b mod m;
  Result := 0;

  while a > 0 do
  begin
    if a and 1 = 1 then Result := AddMod(Result, b, m);
    a := a shr 1;
    b := AddMod(b, b, m);
  end;
end;

function PowMod(b, e: UInt64; m: UInt64): UInt64;
begin
  if b >= m then b := b mod m;
  Result := 1;

  while e > 0 do
  begin
    if e and 1 = 1 then Result := MulMod(Result, b, m);
    e := e shr 1;
    b := MulMod(b, b, m);
  end;
end;

end.
