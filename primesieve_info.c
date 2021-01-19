#include <inttypes.h>
#include <stdio.h>
#include <primesieve.h>

int main() {
  printf("PrimeSieve information\n");
  printf("----------------------\n");
  printf("Library version: %s\n", primesieve_version());
  printf("API version: %s\n", PRIMESIEVE_VERSION);
  printf("PRIMESIEVE_ERROR constant: %" PRIX64 "\n", PRIMESIEVE_ERROR);
  printf("Largest valid stop number: %" PRIX64 "\n", primesieve_get_max_stop());
  printf("Current sieve size in KiB: %d\n", primesieve_get_sieve_size());
  printf("Current number of threads: %d\n", primesieve_get_num_threads());
  printf("SizeOf primesieve_iterator record: %ld\n", sizeof(primesieve_iterator));
  return 0;
}
