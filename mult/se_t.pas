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
  WriteLn(Length(primes));
  SetLength(primes, 0);
end.