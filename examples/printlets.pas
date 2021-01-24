(*  Pascal program that shows how to print primes, twins, ...  *)

program printlets;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

uses SysUtils, primesieve;

var
  start, stop: UInt64;

begin
  if ParamCount <> 3 then
  begin
    WriteLn('Usage: printlets lets start stop');
    WriteLn('Where lets - 1..6');
    Halt(1);
  end;

  UInt64.TryParse(ParamStr(2), start);
  UInt64.TryParse(ParamStr(3), stop);

  case Integer.Parse(ParamStr(1)) of
    1: primesieve_print_primes(start, stop);
    2: primesieve_print_twins(start, stop);
    3: primesieve_print_triplets(start, stop);
    4: primesieve_print_quadruplets(start, stop);
    5: primesieve_print_quintuplets(start, stop);
    6: primesieve_print_sextuplets(start, stop);
  end;
end.
