#include <stdio.h>
#include <stdlib.h>
#include "zlib.h"           /* inflateBackInit(), inflateBack(), */
#include <time.h>
#include "x86.h"

//>gcc -o tst  x86.c crc32.c zutil.c  test.c -DHAS_PCLMUL -march=native; ./tst
// Conversion required 0.314882 seconds.
// CRC= 4062621372
//>gcc -o tst  x86.c crc32.c zutil.c  test.c -march=native; ./tst
// Conversion required 3.668909 seconds.
// CRC= 4062621372
//crc >gcc -o tst  x86.c crc32.c zutil.c  test.c -DHAS_PCLMUL -march=native; ./tst
// Conversion required 0.316549 seconds.
// CRC= 4062621372
//>gcc -O3 -o tst  x86.c crc32.c zutil.c  test.c -march=native; ./tst
// Conversion required 2.069432 seconds.
// CRC= 4062621372
//>gcc -O3 -o tst  x86.c crc32.c zutil.c  test.c -DHAS_PCLMUL -march=native; ./tst
// Conversion required 0.089894 seconds.
// CRC= 4062621372

int main() {
	x86_check_features();
	if (x86_cpu_enable_simd)
		printf("SIMD can be enabled\n");
	else
		printf("SIMD is not supported\n");
	const int len = 1073741824;
	srand(127);
	unsigned char *buf = malloc (len);
	for (int i = 0; i < len; i++)
  		buf[i] = rand ();
  	unsigned long crc = 0;
  	clock_t start = clock();
  	unsigned char *bufUnaligned = buf + 1;
  	crc = crc32(crc, bufUnaligned, len-2);
  	printf ("Conversion required %f seconds.\n",((float)(clock()-start))/CLOCKS_PER_SEC);
   
  	printf("CRC= %lu\n", crc);
	
  	free(buf);
	return 0;
}