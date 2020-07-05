unit primesieve;
{$ifdef FPC}
{$mode Delphi}
{$endif}

{$IFDEF USE_ABI6}
{$MESSAGE HINT 'Using ABI6'}
{$ENDIF}
// Debug
// {$DEFINE USE_ABI6}

interface

const
  {$IF Defined(Linux)}
  {$MESSAGE HINT 'LINUX platform'}
  LIB_FNPFX = '';
  LIB_PRIMESIEVE = 'primesieve';
  {$ELSEIF Defined(Darwin)}
  {$MESSAGE HINT 'DARWIN platform'}
  LIB_FNPFX = '';
  LIB_PRIMESIEVE = 'libprimesieve.dylib';
  {$linklib libprimesieve.dylib}
  {$ELSE}
    {$MESSAGE Fatal 'Unsupported platform'}
  {$ENDIF}

  _PRIMESIEVE_VERSION = '7.5';
  _PRIMESIEVE_VERSION_MAJOR = 7;
  _PRIMESIEVE_VERSION_MINOR = 5;

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

(**
 * Get an array with the primes inside the interval [start, stop].
 * @param size  The size of the returned primes array.
 * @param type  The type of the primes to generate, e.g. INT_PRIMES.
 *)
function primesieve_generate_primes(start: UInt64; stop: UInt64; size: PNativeUInt; &type: Integer): Pointer; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_generate_primes';

(**
 * Get an array with the first n primes >= start.
 * @param type  The type of the primes to generate, e.g. INT_PRIMES.
 *)
function primesieve_generate_n_primes(n: UInt64; start: UInt64; &type: Integer): Pointer; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_generate_n_primes';

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
function primesieve_nth_prime(n: Int64; start: UInt64): UInt64; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_nth_prime';

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
function primesieve_count_primes(start: UInt64; stop: UInt64): UInt64; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_primes';

(**
 * Count the twin primes within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *)
function primesieve_count_twins(start: UInt64; stop: UInt64): UInt64; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_twins';

(**
 * Count the prime triplets within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *)
function primesieve_count_triplets(start: UInt64; stop: UInt64): UInt64; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_triplets';

(**
 * Count the prime quadruplets within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *)
function primesieve_count_quadruplets(start: UInt64; stop: UInt64): UInt64; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_quadruplets';

(**
 * Count the prime quintuplets within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *)
function primesieve_count_quintuplets(start: UInt64; stop: UInt64): UInt64; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_quintuplets';

(**
 * Count the prime sextuplets within the interval [start, stop].
 * By default all CPU cores are used, use
 * primesieve_set_num_threads(int threads) to change the
 * number of threads.
 *)
function primesieve_count_sextuplets(start: UInt64; stop: UInt64): UInt64; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_sextuplets';

(**
 * Print the primes within the interval [start, stop]
 * to the standard output.
 *)
procedure primesieve_print_primes(start: UInt64; stop: UInt64); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_primes';

(**
 * Print the twin primes within the interval [start, stop]
 * to the standard output.
 *)
procedure primesieve_print_twins(start: UInt64; stop: UInt64); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_twins';

(**
 * Print the prime triplets within the interval [start, stop]
 * to the standard output.
 *)
procedure primesieve_print_triplets(start: UInt64; stop: UInt64); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_triplets';

(**
 * Print the prime quadruplets within the interval [start, stop]
 * to the standard output.
 *)
procedure primesieve_print_quadruplets(start: UInt64; stop: UInt64); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_quadruplets';

(**
 * Print the prime quintuplets within the interval [start, stop]
 * to the standard output.
 *)
procedure primesieve_print_quintuplets(start: UInt64; stop: UInt64); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_quintuplets';

(**
 * Print the prime sextuplets within the interval [start, stop]
 * to the standard output.
 *)
procedure primesieve_print_sextuplets(start: UInt64; stop: UInt64); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_sextuplets';

(**
 * Returns the largest valid stop number for primesieve.
 * @return 2^64-1 (UINT64_MAX).
 *)
function primesieve_get_max_stop(): UInt64; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_get_max_stop';

(** Get the current set sieve size in KiB *)
function primesieve_get_sieve_size(): Integer; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_get_sieve_size';

(** Get the current set number of threads *)
function primesieve_get_num_threads(): Integer; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_get_num_threads';

(**
 * Set the sieve size in KiB (kibibyte).
 * The best sieving performance is achieved with a sieve size
 * of your CPU's L1 or L2 cache size (per core).
 * @pre sieve_size >= 8 && <= 4096.
 *)
procedure primesieve_set_sieve_size(sieve_size: Integer); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_set_sieve_size';

(**
 * Set the number of threads for use in
 * primesieve_count_*() and primesieve_nth_prime().
 * By default all CPU cores are used.
 *)
procedure primesieve_set_num_threads(num_threads: Integer); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_set_num_threads';

(**
 * Deallocate a primes array created using the
 * primesieve_generate_primes() or primesieve_generate_n_primes()
 * functions.
 *)
procedure primesieve_free(primes: Pointer); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_free';

(** Get the primesieve version number, in the form “i.j” *)
function primesieve_version(): PAnsiChar; cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_version';


(** Initialize the primesieve iterator before first using it *)
procedure primesieve_init(var it: primesieve_iterator); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_init';

(** Free all memory *)
procedure primesieve_free_iterator(var it: primesieve_iterator); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_free_iterator';

(**
 * Reset the primesieve iterator to start.
 * @param start      Generate primes > start (or < start).
 * @param stop_hint  Stop number optimization hint. E.g. if you want
 *                   to generate the primes below 1000 use
 *                   stop_hint = 1000, if you don't know use
 *                   primesieve_get_max_stop().
 *)
procedure primesieve_skipto(var it: primesieve_iterator; start: UInt64;
  stop_hint: UInt64); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_skipto';

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

(** Internal use *)
procedure primesieve_generate_next_primes(var it: primesieve_iterator); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_generate_next_primes';

(** Internal use *)
procedure primesieve_generate_prev_primes(var it: primesieve_iterator); cdecl;
  external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_generate_prev_primes';

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

end.
