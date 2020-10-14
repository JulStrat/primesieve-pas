program miller_rabin_t;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses nth, primesieve;

var
  n: UInt64;
  it: primesieve_iterator;
  prime: UInt64;

begin
  primesieve_init(it);
  prime := primesieve_next_prime(it);
  n := 0;

  while n <= 1000000 do
  begin
    if n = prime then
    begin
      if not MillerRabin(n) then Halt(1);
      prime := primesieve_next_prime(it);
    end
    else
      if MillerRabin(n) then Halt(1);
    Inc(n);
  end;

  n := 1000000000000000;
  primesieve_skipto(it, 1000000000000000, 1000000001000000);
  prime := primesieve_next_prime(it);

  while n <= 1000000001000000 do
  begin
    if n = prime then
    begin
      if not MillerRabin(n) then Halt(1);
      prime := primesieve_next_prime(it);
    end
    else
      if MillerRabin(n) then Halt(1);
    Inc(n);
  end;

  n := 1000000000000000000;
  primesieve_skipto(it, 1000000000000000000, 1000000000001000000);
  prime := primesieve_next_prime(it);

  while n <= 1000000000001000000 do
  begin
    if n = prime then
    begin
      if not MillerRabin(n) then Halt(1);
      prime := primesieve_next_prime(it);
    end
    else
      if MillerRabin(n) then Halt(1);
    Inc(n);
  end;

  primesieve_free_iterator(it);
end.