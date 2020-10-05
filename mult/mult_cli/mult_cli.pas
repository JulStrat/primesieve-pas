program mult_cli;
{$IF Defined(FPC)}
{$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

uses mult, SysUtils;

var
  start, stop: UInt64;
  fun: LongInt;

begin
  if ParamCount <> 3 then
  begin
    WriteLn('Usage: mult-cli <OEIS sequence> start stop');
    Halt(1);
  end;

  fun := StrToInt(ParamStr(1));
  start := StrToQWord(ParamStr(2));
  stop := StrToQWord(ParamStr(3));

  case fun of
    10: while start <= stop do begin WriteLn(start, ' ', Phi(start)); Inc(start); end;
    8683: while start <= stop do begin WriteLn(start, ' ', Mu(start)); Inc(start); end;
    5: while start <= stop do begin WriteLn(start, ' ', Tau(start)); Inc(start); end;
    7947: while start <= stop do begin WriteLn(start, ' ', Rad(start)); Inc(start); end;
  end;

end.
