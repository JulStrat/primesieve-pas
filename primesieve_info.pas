program primesieve_info;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$APPTYPE Console}

uses SysUtils, primesieve;

begin
  WriteLn('PrimeSieve information');
  WriteLn('----------------------');  
  WriteLn(Format('Library version: %s', [primesieve_version()]));
  WriteLn(Format('API version: %s', [_PRIMESIEVE_VERSION]));
  WriteLn(Format('_PRIMESIEVE_ERROR constant: %x', [_PRIMESIEVE_ERROR]));  
  WriteLn(Format('Largest valid stop number: %x', [primesieve_get_max_stop()]));
  WriteLn(Format('Current sieve size in KiB: %d', [primesieve_get_sieve_size()]));  
  WriteLn(Format('Current number of threads: %d', [primesieve_get_num_threads()]));    
end.
