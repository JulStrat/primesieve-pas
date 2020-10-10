program nth_t;
uses nth;

var
  i: integer;

begin
  for i := 0 to 1000 do
    WriteLn(i, ' - ', MillerRabin(i));

  WriteLn(1000000007, ' - ', MillerRabin(1000000007));
  WriteLn(1000000009, ' - ', MillerRabin(1000000009));
  WriteLn(1000000011, ' - ', MillerRabin(1000000011));  
  WriteLn(1000000021, ' - ', MillerRabin(1000000021));    
end.