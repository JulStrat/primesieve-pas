program se_t;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses nth;

var
  i: integer;
  primes: TPrimes;

function IsPerfSquare(n: UInt64): boolean;
var
  t: UInt64;

begin
  t := Trunc(Sqrt(n));
  if t * t = n then Result := true
  else Result := false;
end;

function factorize(n: UInt64): UInt64;
var
  p: UInt64;
  i: UInt64;
begin
  i := 0;
  while i < Length(primes) do 
  begin
    p := primes[i];
    if p * p * p > n then break;
    while (n mod p) = 0 do
      n := n div p;
    Inc(i);
  end;
  if n > 1 then
  begin
    if IsPerfSquare(n) then Exit(Trunc(Sqrt(n)));
    if MillerRabin(n) then Exit(n);
    Exit(n);
  end;
  Exit(n)
end;

begin
  primes := SieveEratosthenes(LOW_PRIME_BOUND);
  WriteLn(Length(primes));

  WriteLn(factorize(3*2497349));

  SetLength(primes, 0);
end.