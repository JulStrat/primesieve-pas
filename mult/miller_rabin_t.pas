program miller_rabin_t;
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
  primesieve_free_iterator(it);
end.