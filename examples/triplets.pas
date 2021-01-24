(* Prints prime triplets up to 1000000 using tuplets_iterator *)
program triplets;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

{$INLINE ON}
{$APPTYPE CONSOLE}

uses SysUtils, primesieve;

var
  it: tuplets_iterator;
  prime: UInt64;

begin
  tuplets_init(it);
  prime := tuplets_next_triplet(it);
  while it.tail[0] <= 1000000 do
  begin
    WriteLn(Format('(%d, %d, %d)', [it.tail[2], it.tail[1], it.tail[0]]));
    prime := tuplets_next_triplet(it);
  end;
  tuplets_free(it);
end.
