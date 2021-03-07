(*
  Pascal program that shows how to print primes, twins, ..., sextuplets
  using primesieve_iterator and tuplets_iterator.
*)

program printlets_it;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

uses SysUtils, primesieve;

var
  start, stop, prime: UInt64;
  primes_it: primesieve_iterator;
  tuplets_it: tuplets_iterator;
  t: Integer;

begin
  if ParamCount <> 3 then
  begin
    WriteLn('Usage: printlets_it lets start stop');
    WriteLn('Where lets - 1..6');
    Halt(1);
  end;

  Integer.TryParse(ParamStr(1), t);
  UInt64.TryParse(ParamStr(2), start);
  UInt64.TryParse(ParamStr(3), stop);

  if start > 0 then Dec(start);
  if t = 1 then
  begin
    primesieve_init(primes_it);
    primesieve_skipto(primes_it, start, primesieve_get_max_stop());
  end
  else
  begin
    tuplets_init(tuplets_it);
    tuplets_skipto(tuplets_it, start);
  end;

  case t of
    1:
      begin
        prime := primesieve_next_prime(primes_it);
        while prime <= stop do
        begin
          WriteLn(prime);
          prime := primesieve_next_prime(primes_it);
        end;
      end;
    2:
      begin
        prime := tuplets_next_twin(tuplets_it);
        while tuplets_it.tail[0] <= stop do
        begin
          WriteLn(Format('(%d, %d)', [tuplets_it.tail[1], 
            tuplets_it.tail[0]]));
          prime := tuplets_next_twin(tuplets_it);
        end;
      end;
    3:
      begin
        prime := tuplets_next_triplet(tuplets_it);
        while tuplets_it.tail[0] <= stop do
        begin
          WriteLn(Format('(%d, %d, %d)',
            [tuplets_it.tail[2], tuplets_it.tail[1], tuplets_it.tail[0]]));
          prime := tuplets_next_triplet(tuplets_it);
        end;
      end;
    4:
      begin
        prime := tuplets_next_quadruplet(tuplets_it);
        while tuplets_it.tail[0] <= stop do
        begin
          WriteLn(Format('(%d, %d, %d, %d)',
            [tuplets_it.tail[3], tuplets_it.tail[2], tuplets_it.tail[1],
            tuplets_it.tail[0]]));
          prime := tuplets_next_quadruplet(tuplets_it);
        end;
      end;
    5:
      begin
        prime := tuplets_next_quintuplet(tuplets_it);
        while tuplets_it.tail[0] <= stop do
        begin
          WriteLn(Format('(%d, %d, %d, %d, %d)',
            [tuplets_it.tail[4], tuplets_it.tail[3], tuplets_it.tail[2],
            tuplets_it.tail[1], tuplets_it.tail[0]]));
          prime := tuplets_next_quintuplet(tuplets_it);
        end;
      end;
    6:
      begin
        prime := tuplets_next_sextuplet(tuplets_it);
        while tuplets_it.tail[0] <= stop do
        begin
          WriteLn(Format('(%d, %d, %d, %d, %d, %d)',
            [tuplets_it.tail[5], tuplets_it.tail[4], tuplets_it.tail[3],
            tuplets_it.tail[2], tuplets_it.tail[1], tuplets_it.tail[0]]));
          prime := tuplets_next_sextuplet(tuplets_it);
        end;
      end;
  end;

  if t = 1 then
    primesieve_free_iterator(primes_it)
  else
    tuplets_free(tuplets_it);

end.
