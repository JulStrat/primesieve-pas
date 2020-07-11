(**
 * @example prev_prime.pas
 * Iterate backwards over primes using primesieve_iterator.
 * 
 * Note that primesieve_next_prime() runs up to 2x faster and uses
 * only half as much memory as primesieve_prev_prime(). Hence if
 * it is possible to write the same algorithm using either
 * primesieve_prev_prime() or primesieve_next_prime() then it is
 * preferable to use primesieve_next_prime().
 *)

program prev_prime;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
{$INLINE ON}

{$APPTYPE CONSOLE}

uses primesieve;

var
  it: primesieve_iterator;
  prime: UInt64;

begin
  primesieve_init(it);

  (* primesieve_skipto(it, start_number, stop_hint) *)
  primesieve_skipto(it, 2000, 1000);
  (* iterate over primes from 2000 to 1000 *)
  prime := primesieve_prev_prime(it);
  
  while prime >= 1000 do
  begin
    WriteLn(prime);
    prime := primesieve_prev_prime(it);
  end;
  
  primesieve_free_iterator(it);
end.
