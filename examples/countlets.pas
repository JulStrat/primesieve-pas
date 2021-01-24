(*  @example countlets.pas
 *  Pascal program that shows how to count primes, twins, ... *)

program countlets;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

uses SysUtils, primesieve;

var
  start, stop, num: UInt64;

begin
  if ParamCount <> 3 then
  begin
    WriteLn('Usage: countlets lets start stop');
    WriteLn('Where lets - 1..6');
    Halt(1);
  end;

  UInt64.TryParse(ParamStr(2), start);
  UInt64.TryParse(ParamStr(3), stop);

  case Integer.Parse(ParamStr(1)) of
    1: num := primesieve_count_primes(start, stop);
    2: num := primesieve_count_twins(start, stop);
    3: num := primesieve_count_triplets(start, stop);
    4: num := primesieve_count_quadruplets(start, stop);
    5: num := primesieve_count_quintuplets(start, stop);
    6: num := primesieve_count_sextuplets(start, stop);
  end;
  WriteLn(num);
end.
