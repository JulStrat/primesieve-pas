program se_t;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses nth;

var
  primes: TPrimes;
  start, n, p: UInt64;

function IsPerfSquare(n: UInt64): UInt64;
var
  t: UInt64;

begin
  t := Trunc(Sqrt(n));
  if t * t = n then Result := t
  else Result := n;
end;

function GetFactor(n: UInt64): UInt64;
var
  x, f: UInt64;
begin
  x := 2;
  while x <= 5 do
  begin
    f := PollardRho(n, x, 1);
    if f <> n then Exit(f);
    Inc(x);
  end;
  Result := n;
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
    p := IsPerfSquare(n);
    if p < n then Exit(p);

    if MillerRabin(n) then Exit(n);

    if n <= HIGH_PRIME_BOUND then
    begin
      while i < Length(primes) do
      begin
        p := primes[i];
        if (n mod p) = 0 then Exit(p);
        Inc(i);
      end;
    end
    else
    begin
      p := GetFactor(n);

      if p < n then Exit(p)
      else Exit(0);
    end;
  end;
  Exit(n);
end;

begin
  primes := SieveEratosthenes(LOW_PRIME_BOUND);

  start := 1000000000000000000;
  n := start;

  WriteLn(LOW_PRIME_BOUND);
  WriteLn(HIGH_PRIME_BOUND);

  while n <= start + 10000 do
  begin
    p := factorize(n);
    if p = 0 then
    begin
      WriteLn(n, ' Failed.');
      Halt(1);
    end;
    Inc(n);
  end;

  SetLength(primes, 0);
end.