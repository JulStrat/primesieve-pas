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

function MillerRabin(n: UInt64): boolean;

implementation

uses modar;

function MillerRabin(n: UInt64): boolean;
var
  d, x: UInt64;
  i, r: integer;
  witness: boolean;
  a: UInt64;

begin
  if n in [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37] then Exit(True);
  if (n and 1 = 0) or (n = 1) then Exit(False);
  d := n - 1;
  r := 0;

  while (d > 0) and (d and 1 = 0) do
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

end.
