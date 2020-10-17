program se_t;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses nth;

var
  i: integer;
  primes: TPrimes;

begin
  primes := SieveEratosthenes(LOW_PRIME_BOUND);
  for i := 0 to Length(primes) - 1 do
    WriteLn(i + 1, ' ', primes[i]);
  SetLength(primes, 0);
end.