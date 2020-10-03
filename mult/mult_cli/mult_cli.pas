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
    10: while start <= stop do begin WriteLn(start, ' ', phi(start)); Inc(start); end;
  end;

end.
