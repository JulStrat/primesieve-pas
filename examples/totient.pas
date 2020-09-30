(*  @example totient.pas
 *  Prints Euler's totient function for given range.
 *  https://oeis.org/A000010 *)

program totient;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

{$INLINE ON}
{$APPTYPE CONSOLE}

uses primesieve, SysUtils;

function phi(x: UInt64; it: primesieve_iterator): UInt64;
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

var
  it: primesieve_iterator;
  start, stop, x: UInt64;

begin
  if ParamCount <> 2 then
  begin
    WriteLn('Usage: totient start stop');
    Halt(1);
  end;

  start := UInt64.Parse(ParamStr(1));
  stop := UInt64.Parse(ParamStr(2));
  x := start;
  primesieve_init(it);
  while x <= stop do
  begin
    WriteLn(x, ' ', phi(x, it));
    Inc(x);
  end;
  primesieve_free_iterator(it);
end.
