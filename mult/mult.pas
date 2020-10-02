{
  Multiplicative functions.
}
unit mult;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

{$INLINE ON}
interface

uses primesieve;

(*
  Euler totient function Phi(n): count numbers <= n and prime to n.
  OEIS: https://oeis.org/A000010
*)
function Phi(x: UInt64): UInt64;

implementation

var
  it: primesieve_iterator;

function Phi(x: UInt64): UInt64;
var
  p: UInt64;

begin
  primesieve_skipto(it, 0, primesieve_get_max_stop());
  p := primesieve_next_prime(it);
  Result := x;

  while p * p <= x do
  begin
    if x mod p = 0 then
      Result := Result div p * (p-1);
    while x mod p = 0 do
      x := x div p;
    p := primesieve_next_prime(it);
  end;
  if x > 1 then
    Result := Result div x * (x-1);
end;

initialization

  primesieve_init(it);

finalization

  primesieve_free_iterator(it);

end.
