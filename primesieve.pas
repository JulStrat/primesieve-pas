{
  Pascal bindings for primesieve library.

  primesieve - library for fast prime number generation.@br
  Copyright (C) 2010 - 2021 Kim Walisch, <kim.walisch@gmail.com>@br
  https://github.com/kimwalisch/primesieve  

  primesieve-pas - FPC/Delphi API for primesieve library.@br
  Copyright (C) 2020 - 2021 I. Kakoulidis, <ioulianos.kakoulidis@hotmail.com>@br
  https://github.com/JulStrat/primesieve-pas

  This file is distributed under the BSD 2-Clause License.
}

unit primesieve;
{$IF Defined(FPC)}
{$MODE Delphi}
{$PACKRECORDS C}
{$ENDIF}

{$IF Defined(USE_ABI6)}
{$MESSAGE HINT 'Using ABI6'}
{$ENDIF}

interface

type
  PUInt64 = ^UInt64;
  PInt64 = ^Int64;

const
  {$IF Defined(Darwin)}
    {$MESSAGE HINT 'Darwin platform'}
    (* @exclude *)    
    LIB_FNPFX = '';
    (* @exclude *)    
    LIB_PRIMESIEVE = 'primesieve';
    {$linklib primesieve}
  {$ELSEIF Defined(Unix)}
    {$MESSAGE HINT 'Unix platform'}
    (* @exclude *)    
    LIB_FNPFX = '';
    (* @exclude *)    
    LIB_PRIMESIEVE = 'primesieve';
  {$ELSEIF Defined(MSWindows)}
    {$MESSAGE HINT 'Windows platform'}
    (* @exclude *)    
    LIB_FNPFX = '';
    (* @exclude *)    
    LIB_PRIMESIEVE = 'primesieve';
  {$ELSE}
    {$MESSAGE Fatal 'Unsupported platform'}
  {$ENDIF}
  
{$REGION 'primesieve.h'}

const
  _PRIMESIEVE_VERSION = '7.7';
  _PRIMESIEVE_VERSION_MAJOR = 7;
  _PRIMESIEVE_VERSION_MINOR = 7;

  (* Pascal API version *)
  _PRIMESIEVE_PAS_VERSION = '0.5.1';
  _PRIMESIEVE_PAS_VERSION_MAJOR = 0;
  _PRIMESIEVE_PAS_VERSION_MINOR = 5;
  _PRIMESIEVE_PAS_VERSION_PATCH = 1;

  (*
    primesieve functions return @italic(PRIMESIEVE_ERROR 
    (UINT64_MAX)) if any error occurs.
  *)  
  _PRIMESIEVE_ERROR = not UInt64(0);

{ Platform dependent types }
const
  (* Generate primes of short type *)
  (* @exclude *)  
  SHORT_PRIMES = 0;

  (* Generate primes of unsigned short type *)
  (* @exclude *)    
  USHORT_PRIMES = 1;

  (* Generate primes of int type *)
  (* @exclude *)    
  INT_PRIMES = 2;

  (* Generate primes of unsigned int type *)
  (* @exclude *)    
  UINT_PRIMES = 3;

  (* Generate primes of long type *)
  (* @exclude *)    
  LONG_PRIMES = 4;

  (* Generate primes of unsigned long type *)
  (* @exclude *)    
  ULONG_PRIMES = 5;

  (* Generate primes of long long type *)
  (* @exclude *)    
  LONGLONG_PRIMES = 6;

  (* Generate primes of unsigned long long type *)
  (* @exclude *)    
  ULONGLONG_PRIMES = 7;

{ Platform independent types }
const  
  (* Generate primes of @italic(Int16 (c int16_t)) type *)
  INT16_PRIMES = 8;
  (* Generate primes of @italic(UInt16 (c uint16_t)) type *)
  UINT16_PRIMES = 9;
  (* Generate primes of @italic(Int32 (c int32_t)) type *)
  INT32_PRIMES = 10;
  (* Generate primes of @italic(UInt32 (c uint32_t)) type *)
  UINT32_PRIMES = 11;
  (* Generate primes of @italic(Int64 (c int64_t)) type *)
  INT64_PRIMES = 12;
  (* Generate primes of @italic(UInt64 (c uint64_t)) type *)
  UINT64_PRIMES = 13;

(*
  Get an array with the primes inside the interval @italic([start, stop]).
  
  @param(size The size of the returned primes array.)
  @param(ptype The type of the primes to generate, e.g. @link(INT_PRIMES32).)
*)
function primesieve_generate_primes(
  start, stop: UInt64;
  var size: NativeUInt;
  ptype: Integer ): Pointer;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_generate_primes';

(*
  Get an array with the first @italic(n primes >= start).
  
  @param(ptype The type of the primes to generate,
    e.g. @link(INT_PRIMES32).)
*)
function primesieve_generate_n_primes(
  n: UInt64;
  start: UInt64;
  ptype: Integer ): Pointer;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_generate_n_primes';

(*
  Find the nth prime.
  By default all CPU cores are used, use
  @link(primesieve_set_num_threads) to change the number of threads.
 
  Note that each call to @link(primesieve_nth_prime) incurs an
  initialization overhead of @italic(O(sqrt(start))) even if @italic(n) is tiny.
  Hence it is not a good idea to use @link(primesieve_nth_prime)
  repeatedly in a loop to get the next (or previous) prime. For
  this use case it is better to use a @link(primesieve_iterator) which
  needs to be initialized only once.
 
  @param(n if @italic(n = 0) finds the @italic(1st prime >= start),@br
           if @italic(n > 0) finds the @italic(nth prime > start),@br
           if @italic(n < 0) finds the @italic(nth prime < start) (backwards).)
*)
function primesieve_nth_prime(
  n: Int64;
  start: UInt64 ): UInt64;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_nth_prime';

(*
  Count the primes within the interval @italic([start, stop]).
  By default all CPU cores are used, use
  @link(primesieve_set_num_threads) to change the number of threads.
 
  Note that each call to @link(primesieve_count_primes) incurs an
  initialization overhead of @italic(O(sqrt(stop))) even if the interval
  @italic([start, stop]) is tiny. Hence if you have written an algorithm
  that makes many calls to @link(primesieve_count_primes) it may be
  preferable to use a @link(primesieve_iterator) which needs to be
  initialized only once.
*)
function primesieve_count_primes(
  start, stop: UInt64 ): UInt64;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_primes';

(*
  Count the twin primes within the interval @italic([start, stop]).
  
  By default all CPU cores are used, use
  @link(primesieve_set_num_threads) to change the number of threads.
*)
function primesieve_count_twins(
  start, stop: UInt64 ): UInt64;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_twins';

(*
  Count the prime triplets within the interval @italic([start, stop]).
  
  By default all CPU cores are used, use
  @link(primesieve_set_num_threads) to change the number of threads.
*)
function primesieve_count_triplets(
  start, stop: UInt64 ): UInt64;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_triplets';

(*
  Count the prime quadruplets within the interval @italic([start, stop]).
  
  By default all CPU cores are used, use
  @link(primesieve_set_num_threads) to change the number of threads.
*)
function primesieve_count_quadruplets(
  start, stop: UInt64 ): UInt64;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_quadruplets';

(*
  Count the prime quintuplets within the interval @italic([start, stop]).
  
  By default all CPU cores are used, use
  @link(primesieve_set_num_threads) to change the number of threads.
*)
function primesieve_count_quintuplets(
  start, stop: UInt64 ): UInt64;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_quintuplets';

(*
  Count the prime sextuplets within the interval @italic([start, stop]).
  
  By default all CPU cores are used, use
  @link(primesieve_set_num_threads) to change the number of threads.
*)
function primesieve_count_sextuplets(
  start, stop: UInt64 ): UInt64;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_count_sextuplets';

(*
  Print the primes within the interval @italic([start, stop])
  to the standard output.
*)
procedure primesieve_print_primes(
  start, stop: UInt64 );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_primes';

(*
  Print the twin primes within the interval @italic([start, stop])
  to the standard output.
*)
procedure primesieve_print_twins(
  start, stop: UInt64 );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_twins';

(*
  Print the prime triplets within the interval @italic([start, stop])
  to the standard output.
*)
procedure primesieve_print_triplets(
  start, stop: UInt64 );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_triplets';

(*
  Print the prime quadruplets within the interval @italic([start, stop])
  to the standard output.
*)
procedure primesieve_print_quadruplets(
  start, stop: UInt64 );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_quadruplets';

(*
  Print the prime quintuplets within the interval @italic([start, stop])
  to the standard output.
*)
procedure primesieve_print_quintuplets(
  start, stop: UInt64 );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_quintuplets';

(*
  Print the prime sextuplets within the interval @italic([start, stop])
  to the standard output.
*)
procedure primesieve_print_sextuplets(
  start, stop: UInt64 );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_print_sextuplets';

(*
  Returns the largest valid stop number for primesieve.
  
  @italic(2^64-1 (UINT64_MAX))
*)
function primesieve_get_max_stop(): UInt64;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_get_max_stop';

(* Get the current set sieve size in KiB. *)
function primesieve_get_sieve_size(): Integer;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_get_sieve_size';

(* Get the current set number of threads. *)
function primesieve_get_num_threads(): Integer;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_get_num_threads';

(*
  Set the sieve size in KiB (kibibyte).
  The best sieving performance is achieved with a sieve size
  of your CPU's L1 or L2 cache size (per core).
  @italic(sieve_size >= 16 and <= 4096)
*)
procedure primesieve_set_sieve_size(
  sieve_size: Integer );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_set_sieve_size';

(*
  Set the number of threads for use in
  @italic(primesieve_count_*()) and @link(primesieve_nth_prime).
  By default all CPU cores are used.
*)
procedure primesieve_set_num_threads(
  num_threads: Integer );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_set_num_threads';

(*
  Deallocate a primes array created using the
  @link(primesieve_generate_primes) or @link(primesieve_generate_n_primes)
  functions.
*)
procedure primesieve_free(
  primes: Pointer );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_free';

(* Get the primesieve version number, in the form “i.j” *)
function primesieve_version(): PAnsiChar;
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_version';

{$REGION 'iterator.h'}
(*
  @link(primesieve_iterator) allows to easily iterate over primes
  both forwards and backwards. Generating the first prime
  has a complexity of @italic(O(r log log r)) operations with
  @italic(r = n^0.5), after that any additional prime is generated in
  amortized @italic(O(log n log log n)) operations. The memory usage
  is about @italic(PrimePi(n^0.5) * 8) bytes.

  The @italic(primesieve_iterator.pas)
  example shows how to use @link(primesieve_iterator).
  If any error occurs @link(primesieve_next_prime) and 
  @link(primesieve_prev_prime) return @link(_PRIMESIEVE_ERROR).
  Furthermore @italic(primesieve_iterator.is_error) is initialized
  to @italic(0) and set to @italic(1) if any error occurs.
*)
type 
  {$IF Defined(USE_ABI6)}
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

(* Initialize the primesieve iterator before first using it. *)
procedure primesieve_init(
  var it: primesieve_iterator );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_init';

(* Free all iterator memory. *)
procedure primesieve_free_iterator(
  var it: primesieve_iterator );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_free_iterator';

(*
  Reset the primesieve iterator to start.
  
  @param(start Generate @italic(primes > start (or < start)))
  
  @param(stop_hint Stop number optimization hint. E.g. if you want
         to generate the primes below @italic(1000) use @italic(stop_hint = 1000), 
         if you don't know use @link(primesieve_get_max_stop))
*)
procedure primesieve_skipto(
  var it: primesieve_iterator;
  start, stop_hint: UInt64 );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_skipto';

(*
  Get the next prime.
  
  Returns @italic(UINT64_MAX) if next prime @italic(prime > 2^64).
*)
function primesieve_next_prime(
  var it: primesieve_iterator ): UInt64; inline;

(*
  Get the previous prime.
  
  @link(primesieve_prev_prime) returns @italic(0) for @italic(n <= 2).
  Note that @link(primesieve_next_prime) runs up to 2x faster than
  @link(primesieve_prev_prime). Hence if the same algorithm can be written
  using either @link(primesieve_prev_prime) or @link(primesieve_next_prime)
  it is preferable to use @link(primesieve_next_prime).
*)
function primesieve_prev_prime(
  var it: primesieve_iterator ): UInt64; inline;

(*
  @exclude Internal use.
  Delphi -  E2441 Inline function declared 
  in interface section must not use local symbol.
*)
procedure primesieve_generate_next_primes(
  var it: primesieve_iterator );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_generate_next_primes';

(*
  @exclude Internal use.
  Delphi -  E2441 Inline function declared 
  in interface section must not use local symbol.
*)
procedure primesieve_generate_prev_primes(
  var it: primesieve_iterator );
  cdecl; external LIB_PRIMESIEVE name LIB_FNPFX + 'primesieve_generate_prev_primes';

{$ENDREGION}

{$ENDREGION}

{$REGION 'tuplets_iterator'}

(*
  @link(tuplets_iterator) allows to iterate over prime tuplets.
  Functions @italic(tuplets_next_twin, tuplets_next_triplet, 
  ..., tuplets_next_sextuplet) can be called in any order.
  Each successful call to tuplets_next_* stores prime tuplet sequence  
  into array @italic(tuplets_iterator.tail) and returns last element of tuplet.
  
  The @italic(printlets_it.pas) example shows how to use @link(tuplets_iterator).
  If any error occurs tuplets_next_* functions return @link(_PRIMESIEVE_ERROR).
  Furthermore @italic(tuplets_iterator.iterator.is_error) is initialized
  to @italic(0) and set to @italic(1) if any error occurs.
*)
type
  tuplets_iterator = record
    tail: array[0..5] of UInt64;
    iterator: primesieve_iterator;
  end;  

(* Initialize the prime tuplets iterator before first using it. *)
procedure tuplets_init(
  var it: tuplets_iterator );

(* Free all prime tuplets iterator memory. *)
procedure tuplets_free(
  var it: tuplets_iterator );

(* Reset the prime tuplets iterator to start. *)
procedure tuplets_skipto(
  var it: tuplets_iterator;
  start: UInt64 );

(*
  Get next sequence of two primes of the form 
  (p, p+2)
*)
function tuplets_next_twin(
  var it: tuplets_iterator ): UInt64;

(*
  Get next sequence of three primes of the form 
  (p, p + 2, p + 6) or 
  (p, p + 4, p + 6)
*)
function tuplets_next_triplet(
  var it: tuplets_iterator ): UInt64;

(*
  Get next sequence of four primes of the form
  (p, p+2, p+6, p+8)
*)
function tuplets_next_quadruplet(
  var it: tuplets_iterator ): UInt64;

(*
  Get next sequence of five primes of the form 
  (p, p+2, p+6, p+8,  p+12) or 
  (p, p+4, p+6, p+10, p+12)
*)
function tuplets_next_quintuplet(
  var it: tuplets_iterator ): UInt64;

(*
  Get next sequence of six primes of the form 
  (p, p+4, p+6, p+10, p+12, p+16)
*)
function tuplets_next_sextuplet(
  var it: tuplets_iterator ): UInt64;

{$ENDREGION}

implementation

{$POINTERMATH ON}

function primesieve_next_prime(var it: primesieve_iterator): UInt64;
begin
  {$IF Defined(USE_ABI6)}
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

function primesieve_prev_prime(var it: primesieve_iterator): UInt64;
begin
  {$IF Defined(USE_ABI6)}
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

procedure tuplets_init(var it: tuplets_iterator);
begin
  with it do
  begin
    tail[0] := 0;  tail[1] := 0;  tail[2] := 0;  
    tail[3] := 0;  tail[4] := 0;  tail[5] := 0;
  end;
  primesieve_init(it.iterator);
end;

procedure tuplets_free(var it: tuplets_iterator);
begin
  with it do
  begin
    tail[0] := 0;  tail[1] := 0;  tail[2] := 0;  
    tail[3] := 0;  tail[4] := 0;  tail[5] := 0;
  end;
  primesieve_free_iterator(it.iterator);
end;

procedure tuplets_skipto(var it: tuplets_iterator; start: UInt64);
begin
  with it do
  begin
    tail[0] := 0;  tail[1] := 0;  tail[2] := 0;  
    tail[3] := 0;  tail[4] := 0;  tail[5] := 0;
  end;
  primesieve_skipto(it.iterator, start, primesieve_get_max_stop());
end;

function tuplets_next_twin(var it: tuplets_iterator): UInt64;
var
  prime: UInt64;
begin
  Result := _PRIMESIEVE_ERROR;
  while True do
  begin
    prime := primesieve_next_prime(it.iterator);
    it.tail[5] := it.tail[4]; it.tail[4] := it.tail[3];
    it.tail[3] := it.tail[2]; it.tail[2] := it.tail[1];
    it.tail[1] := it.tail[0]; it.tail[0] := prime;
    if prime = _PRIMESIEVE_ERROR then break;

    if it.tail[0] - it.tail[1] = 2 then
      if it.tail[1] > 0 then
        Exit(it.tail[1]);    
  end;
end;

function tuplets_next_triplet(var it: tuplets_iterator): UInt64;
var
  prime: UInt64;
begin
  Result := _PRIMESIEVE_ERROR;
  while True do
  begin
    prime := primesieve_next_prime(it.iterator);
    it.tail[5] := it.tail[4]; it.tail[4] := it.tail[3];
    it.tail[3] := it.tail[2]; it.tail[2] := it.tail[1];
    it.tail[1] := it.tail[0]; it.tail[0] := prime;
    if prime = _PRIMESIEVE_ERROR then break;

    if it.tail[0] - it.tail[2] = 6 then
      Exit(it.tail[2]);    
  end;
end;

function tuplets_next_quadruplet(var it: tuplets_iterator): UInt64;
var
  prime: UInt64;
begin
  Result := _PRIMESIEVE_ERROR;
  while True do
  begin
    prime := primesieve_next_prime(it.iterator);
    it.tail[5] := it.tail[4]; it.tail[4] := it.tail[3];
    it.tail[3] := it.tail[2]; it.tail[2] := it.tail[1];
    it.tail[1] := it.tail[0]; it.tail[0] := prime;
    if prime = _PRIMESIEVE_ERROR then break;
    
    if it.tail[0] - it.tail[3] = 8 then
      if prime > 11 then
        Exit(it.tail[3]);    
  end;
end;

function tuplets_next_quintuplet(var it: tuplets_iterator): UInt64;
var
  prime: UInt64;
begin
  Result := _PRIMESIEVE_ERROR;
  while True do
  begin
    prime := primesieve_next_prime(it.iterator);
    it.tail[5] := it.tail[4]; it.tail[4] := it.tail[3];
    it.tail[3] := it.tail[2]; it.tail[2] := it.tail[1];
    it.tail[1] := it.tail[0]; it.tail[0] := prime;
    if prime = _PRIMESIEVE_ERROR then break;	

    if it.tail[0] - it.tail[4] = 12 then
      Exit(it.tail[4]);    
  end;
end;

function tuplets_next_sextuplet(var it: tuplets_iterator): UInt64;
var
  prime: UInt64;
begin
  Result := _PRIMESIEVE_ERROR;
  while True do
  begin
    prime := primesieve_next_prime(it.iterator);
    it.tail[5] := it.tail[4]; it.tail[4] := it.tail[3];
    it.tail[3] := it.tail[2]; it.tail[2] := it.tail[1];
    it.tail[1] := it.tail[0]; it.tail[0] := prime;
    if prime = _PRIMESIEVE_ERROR then break;

    if it.tail[0] - it.tail[5] = 16 then
      Exit(it.tail[5]);    
  end;
end;

end.
