program pollard_rho_t;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses nth, primesieve;

var
  start, n, f: UInt64;
  it: primesieve_iterator;
  prime: UInt64;

function GetFactor(n: UInt64): UInt64;
var
  x, f: UInt64;
begin
  for x := 2 to 5 do
  begin
    f := PollardRho(n, x, 1);
    if f <> n then Exit(f);
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