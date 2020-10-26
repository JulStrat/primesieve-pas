program prng_t;
{$IF Defined(FPC)}{$MODE Delphi}{$ENDIF}

uses prng, SysUtils;


const
  check: UInt64 = $FFFFFFFFFFFFFFFF shr 8;

var
  r: TSplitMix64;
  x: UInt64;
  i, c: integer;

begin
  r.Init(UInt64(GetTickCount()) * UInt64(GetTickCount()));

  for i := 0 to 1000000000 do
  begin
    x := r.Next();
    if x < check then Inc(c);
  end;
  
  WriteLn(c, ' values < ', check);
  
  for i := 0 to 20 do
    WriteLn(RandRangeU64(UInt64($FFFFFFFFFFFFFFFF)));

end.