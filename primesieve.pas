unit primesieve;
{$ifdef FPC}
{$mode Delphi}
{$endif}

{$IFDEF USE_ABI6}
{$MESSAGE Warn 'Using ABI6'}
{$ENDIF}

interface

const

  _PRIMESIEVE_VERSION = '7.5';
  _PRIMESIEVE_VERSION_MAJOR = 7;
  _PRIMESIEVE_VERSION_MINOR = 5;

  _PRIMESIEVE_PAS_VERSION = '0.1';

  (**
   * primesieve functions return PRIMESIEVE_ERROR
   * (UINT64_MAX) if any error occurs.
   *)  
  _PRIMESIEVE_ERROR = not UInt64(0);

  (** Generate primes of short type *)
  SHORT_PRIMES = 0;
  (** Generate primes of unsigned short type *)
  USHORT_PRIMES = 1;
  (** Generate primes of int type *)
  INT_PRIMES = 2;
  (** Generate primes of unsigned int type *)
  UINT_PRIMES = 3;
  (** Generate primes of long type *)
  LONG_PRIMES = 4;
  (** Generate primes of unsigned long type *)
  ULONG_PRIMES = 5;
  (** Generate primes of long long type *)
  LONGLONG_PRIMES = 6;
  (** Generate primes of unsigned long long type *)
  ULONGLONG_PRIMES = 7;
  (** Generate primes of int16_t type *)
  INT16_PRIMES = 8;
  (** Generate primes of uint16_t type *)
  UINT16_PRIMES = 9;
  (** Generate primes of int32_t type *)
  INT32_PRIMES = 10;
  (** Generate primes of uint32_t type *)
  UINT32_PRIMES = 11;
  (** Generate primes of int64_t type *)
  INT64_PRIMES = 12;
  (** Generate primes of uint64_t type *)
  UINT64_PRIMES = 13;

type
  
  {$IFDEF USE_ABI6}
  primesieve_iterator = record
    i_: NativeUInt;
    last_idx_: NativeUInt;
    primes_: PUInt64;
    primes_pimpl_: PUInt64;
    start_: UInt64;
    stop_: UInt64;
    stop_hint_: UInt64;
    tiny_cache_size_: UInt64;
    is_error_: integer;
  end;
  {$ELSE}  
  primesieve_iterator = record
    i: NativeUInt;
    last_idx: NativeUInt;
    start: UInt64;
    stop: UInt64;
    stop_hint: UInt64;
    dist: UInt64;
    primes: PUInt64;
    vector: Pointer;
    primeGenerator: Pointer;
    is_error: integer;
  end; 
  {$ENDIF}  

var
(**
 * Get an array with the primes inside the interval [start, stop].
 * @param size  The size of the returned primes array.
 * @param type  The type of the primes to generate, e.g. INT_PRIMES.
 *)
  primesieve_generate_primes: function(start: UInt64; stop: UInt64; size: PNativeUInt; &type: Integer): Pointer; cdecl;

(**
 * Get an array with the first n primes >= start.
 * @param type  The type of the primes to generate, e.g. INT_PRIMES.
 *)
  primesieve_generate_n_primes: function(n: UInt64; start: UInt64; &type: Integer): Pointer; cdecl;

(**
 * Find the nth prime.
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *
 * Note that each call to primesieve_nth_prime(n, start) incurs an
 * initialization overhead of O(sqrt(start)) even if n is tiny.
 * Hence it is not a good idea to use primesieve_nth_prime()
 * repeatedly in a loop to get the next (or previous) prime. For
 * this use case it is better to use a primesieve::iterator which
 * needs to be initialized only once.
 *
 * @param n  if n = 0 finds the 1st prime >= start, <br/>
 *           if n > 0 finds the nth prime > start, <br/>
 *           if n < 0 finds the nth prime < start (backwards).
 *)
  primesieve_nth_prime: function(n: Int64; start: UInt64): UInt64; cdecl;

(**
 * Count the primes within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *
 * Note that each call to primesieve_count_primes() incurs an
 * initialization overhead of O(sqrt(stop)) even if the interval
 * [start, stop] is tiny. Hence if you have written an algorithm
 * that makes many calls to primesieve_count_primes() it may be
 * preferable to use a primesieve::iterator which needs to be
 * initialized only once.
 *)
  primesieve_count_primes: function(start: UInt64; stop: UInt64): UInt64; cdecl;

(**
 * Count the twin primes within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *)
  primesieve_count_twins: function(start: UInt64; stop: UInt64): UInt64; cdecl;

(**
 * Count the prime triplets within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *)
  primesieve_count_triplets: function(start: UInt64; stop: UInt64): UInt64; cdecl;

(**
 * Count the prime quadruplets within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *)
  primesieve_count_quadruplets: function(start: UInt64; stop: UInt64): UInt64; cdecl;

(**
 * Count the prime quintuplets within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *)
  primesieve_count_quintuplets: function(start: UInt64; stop: UInt64): UInt64; cdecl;

(**
 * Count the prime sextuplets within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *)
  primesieve_count_sextuplets: function(start: UInt64; stop: UInt64): UInt64; cdecl;

(**
 * Print the primes within the interval [start, stop]
 * to the standard output.
 *)
  primesieve_print_primes: procedure(start: UInt64; stop: UInt64); cdecl;

(**
 * Print the twin primes within the interval [start, stop]
 * to the standard output.
 *)
  primesieve_print_twins: procedure(start: UInt64; stop: UInt64); cdecl;

(**
 * Print the prime triplets within the interval [start, stop]
 * to the standard output.
 *)
  primesieve_print_triplets: procedure(start: UInt64; stop: UInt64); cdecl;

(**
 * Print the prime quadruplets within the interval [start, stop]
 * to the standard output.
 *)
  primesieve_print_quadruplets: procedure(start: UInt64; stop: UInt64); cdecl;

(**
 * Print the prime quintuplets within the interval [start, stop]
 * to the standard output.
 *)
  primesieve_print_quintuplets: procedure(start: UInt64; stop: UInt64); cdecl;

(**
 * Print the prime sextuplets within the interval [start, stop]
 * to the standard output.
 *)
  primesieve_print_sextuplets: procedure(start: UInt64; stop: UInt64); cdecl;

(**
 * Returns the largest valid stop number for primesieve.
 * @return 2^64-1 (UINT64_MAX).
 *)
  primesieve_get_max_stop: function(): UInt64; cdecl;

(** Get the current set sieve size in KiB *)
  primesieve_get_sieve_size: function(): Integer; cdecl;

(** Get the current set number of threads *)
  primesieve_get_num_threads: function(): Integer; cdecl;

(**
 * Set the sieve size in KiB (kibibyte).
 * The best sieving performance is achieved with a sieve size
 * of your CPU's L1 or L2 cache size (per core).
 * @pre sieve_size >= 8 && <= 4096.
 *)
  primesieve_set_sieve_size: procedure(sieve_size: Integer); cdecl;

(**
 * Set the number of threads for use in
 * primesieve_count_*() and primesieve_nth_prime().
 * By default all CPU cores are used.
 *)
  primesieve_set_num_threads: procedure(num_threads: Integer); cdecl;

(**
 * Deallocate a primes array created using the
 * primesieve_generate_primes() or primesieve_generate_n_primes()
 * functions.
 *)
  primesieve_free: procedure(primes: Pointer); cdecl;

(** Get the primesieve version number, in the form “i.j” *)
  primesieve_version: function(): PAnsiChar; cdecl;



(** Initialize the primesieve iterator before first using it *)
  primesieve_init: procedure(var it: primesieve_iterator); cdecl;

(** Free all memory *)
  primesieve_free_iterator: procedure(var it: primesieve_iterator); cdecl;

(**
 * Reset the primesieve iterator to start.
 * @param start      Generate primes > start (or < start).
 * @param stop_hint  Stop number optimization hint. E.g. if you want
 *                   to generate the primes below 1000 use
 *                   stop_hint = 1000, if you don't know use
 *                   primesieve_get_max_stop().
 *)
  primesieve_skipto: procedure(var it: primesieve_iterator; start: UInt64;
    stop_hint: UInt64); cdecl;

(**
 * Get the next prime.
 * Returns UINT64_MAX if next prime > 2^64.
 *)
  function primesieve_next_prime(var it: primesieve_iterator): UInt64; inline;

(**
 * Get the previous prime.
 * primesieve_prev_prime(n) returns 0 for n <= 2.
 * Note that primesieve_next_prime() runs up to 2x faster than
 * primesieve_prev_prime(). Hence if the same algorithm can be written
 * using either primesieve_prev_prime() or primesieve_next_prime()
 * it is preferable to use primesieve_next_prime().
 *)
  function primesieve_prev_prime(var it: primesieve_iterator): UInt64; inline;

implementation

uses SysUtils{$ifdef FPC}, dynlibs{$endif};

const
{$IF Defined(Linux)}
  {$MESSAGE HINT 'Linux platform'}
  LIB_FNPFX = '';
  LIB_PRIMESIEVE = 'libprimesieve.so';
{$ELSEIF Defined(Darwin)}
  {$MESSAGE HINT 'Darwin platform'}
  LIB_FNPFX = '';
  LIB_PRIMESIEVE = 'libprimesieve.dylib';
{$ELSEIF Defined(Windows)}
  {$MESSAGE HINT 'Windows platform'}
  LIB_FNPFX = '';
  LIB_PRIMESIEVE = 'libprimesieve.dll';
{$ELSE}
  {$MESSAGE Fatal 'Unsupported platform'}
{$ENDIF}

var
  (** Internal use *)
  primesieve_generate_next_primes: procedure(var it: primesieve_iterator); cdecl;

  (** Internal use *)
  primesieve_generate_prev_primes: procedure(var it: primesieve_iterator); cdecl;

function primesieve_next_prime(var it: primesieve_iterator): UInt64; inline;
begin
  {$IFDEF USE_ABI6}
  if it.i_ = it.last_idx_ then  
    primesieve_generate_next_primes(it)
  else
    Inc(it.i_);
  Result := it.primes_[it.i_];  
  {$ELSE}
  if it.i = it.last_idx then  
    primesieve_generate_next_primes(it)
  else
    Inc(it.i);
  Result := it.primes[it.i];  
  {$ENDIF}  
end;

function primesieve_prev_prime(var it: primesieve_iterator): UInt64; inline;
begin
  {$IFDEF USE_ABI6}
  if it.i_ = 0 then  
    primesieve_generate_prev_primes(it)
  else
    Dec(it.i_);
  Result := it.primes_[it.i_];    
  {$ELSE}
  if it.i = 0 then  
    primesieve_generate_prev_primes(it)
  else
    Dec(it.i);  
  Result := it.primes[it.i];    
  {$ENDIF}
end;

var
  libHandle: TLibHandle;

procedure GetAddr(var procAddr: Pointer; procName: string; assertCheck: Boolean=True);
begin
  procAddr := GetProcAddress(libHandle, procName);
  if assertCheck then
    Assert(procAddr <> nil,
      Format('%s procedure not found in "%s"!', [procName, LIB_PRIMESIEVE]));
end;

initialization
  libHandle := LoadLibrary(LIB_PRIMESIEVE);
  if libHandle = NilHandle then
    Halt(1);

  GetAddr(@primesieve_generate_primes, 
    LIB_FNPFX + 'primesieve_generate_primes');
  GetAddr(@primesieve_generate_n_primes, 
    LIB_FNPFX + 'primesieve_generate_n_primes');
  GetAddr(@primesieve_nth_prime, 
    LIB_FNPFX + 'primesieve_nth_prime');
  GetAddr(@primesieve_count_primes, 
    LIB_FNPFX + 'primesieve_count_primes');
  GetAddr(@primesieve_count_twins, 
    LIB_FNPFX + 'primesieve_count_twins');
  GetAddr(@primesieve_count_triplets, 
    LIB_FNPFX + 'primesieve_count_triplets');
  GetAddr(@primesieve_count_quadruplets, 
    LIB_FNPFX + 'primesieve_count_quadruplets');
  GetAddr(@primesieve_count_quintuplets, 
    LIB_FNPFX + 'primesieve_count_quintuplets');
  GetAddr(@primesieve_count_sextuplets, 
    LIB_FNPFX + 'primesieve_count_sextuplets');
  GetAddr(@primesieve_print_primes, 
    LIB_FNPFX + 'primesieve_print_primes');
  GetAddr(@primesieve_print_twins, 
    LIB_FNPFX + 'primesieve_print_twins');
  GetAddr(@primesieve_print_triplets, 
    LIB_FNPFX + 'primesieve_print_triplets');
  GetAddr(@primesieve_print_quadruplets, 
    LIB_FNPFX + 'primesieve_print_quadruplets');
  GetAddr(@primesieve_print_quintuplets, 
    LIB_FNPFX + 'primesieve_print_quintuplets');
  GetAddr(@primesieve_print_sextuplets, 
    LIB_FNPFX + 'primesieve_print_sextuplets');
  GetAddr(@primesieve_get_max_stop, 
    LIB_FNPFX + 'primesieve_get_max_stop');
  GetAddr(@primesieve_get_sieve_size, 
    LIB_FNPFX + 'primesieve_get_sieve_size');
  GetAddr(@primesieve_get_num_threads, 
    LIB_FNPFX + 'primesieve_get_num_threads');
  GetAddr(@primesieve_set_sieve_size, 
    LIB_FNPFX + 'primesieve_set_sieve_size');
  GetAddr(@primesieve_set_num_threads, 
    LIB_FNPFX + 'primesieve_set_num_threads');
  GetAddr(@primesieve_free, 
    LIB_FNPFX + 'primesieve_free');
  GetAddr(@primesieve_version, 
    LIB_FNPFX + 'primesieve_version');

  GetAddr(@primesieve_init, 
    LIB_FNPFX + 'primesieve_init');
  GetAddr(@primesieve_free_iterator, 
    LIB_FNPFX + 'primesieve_free_iterator');
  GetAddr(@primesieve_skipto, 
    LIB_FNPFX + 'primesieve_skipto');

  GetAddr(@primesieve_generate_next_primes, 
    LIB_FNPFX + 'primesieve_generate_next_primes');
  GetAddr(@primesieve_generate_prev_primes, 
    LIB_FNPFX + 'primesieve_generate_prev_primes');

finalization
  FreeLibrary(libHandle);
end.
