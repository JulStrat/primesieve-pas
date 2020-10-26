program pollard_rho_t;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses nth, prng, primesieve;

var
  start, n, f: UInt64;
  it: primesieve_iterator;
  prime: UInt64;

function GetFactor(n: UInt64): UInt64;
var
  x, f: UInt64;
begin
  x := 2;
  while x <= 5 do
  begin
    f := PollardRho(n, 1 + RandRangeU64(n - 2), 1 + RandRangeU64(n - 2));
    if f <> n then Exit(f);
    Inc(x);
  end;
  Result := n;
end;

begin
  primesieve_init(it);

  start := 1000;
  while start <= 1000000000000000000 do
  begin
    WriteLn('Start - ', start);
    WriteLn('--------', start);
    primesieve_skipto(it, start, start + 1000000);
    prime := primesieve_next_prime(it);
    n := start + 1;
    while n <= start + 1000000 do
    begin
      if n = prime then
        prime := primesieve_next_prime(it)
      else
      begin
        if (n and 1) = 1 then f := GetFactor(n);
        if (f = n) then WriteLn(n, ' fails.');
      end;
      Inc(n);
    end;
    start := start * 1000;
  end;

  primesieve_free_iterator(it);
end.