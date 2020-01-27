# simd_crc validating Chromium's SIMD CRC

## Introduction

This project validates that the BSD-licensed CRC code from the Chromium browser can be used to accelerate GZ compression, offering similar performance to the GPL-licensed code.

The [Cyclic Redundancy Check](https://en.wikipedia.org/wiki/Cyclic_redundancy_check) is a popular method to determine if a computer file has been corrupted. Of relevance here, it is used for the popular [GZip format](https://en.wikipedia.org/wiki/Gzip). Popular tools like the program gz spend a lot of time calculating the CRC. This is particularly problematic for the program [pigz](https://github.com/madler/pigz) which runs other compression stages in parallel, but must compute the CRC in serial. Therefore, CRC can become a [rate limiting factor](https://en.wikipedia.org/wiki/Amdahl%27s_law).

The [CloudFlare zlib](https://github.com/cloudflare/zlib) uses several methods to accelerate zlib, which is the compression library used by pigz (as well as many tools in my field of neuroimaging where the Gzip NIfTI format is popular). One of the tricks Cloudflare uses is accelerated CRC using the [PCLMULQDQ
Instruction](https://www.intel.com/content/dam/www/public/us/en/documents/white-papers/fast-crc-computation-generic-polynomials-pclmulqdq-paper.pdf) introduced to x86-64 computers in 2009. This [single instruction, multiple data](https://en.wikipedia.org/wiki/SIMD) method dramatically accelerates CRC calculation. Unfortunately, the Cloudflare zlib computes the CRC using code (originally from the Linux kernel) that is protected by the [GPL](https://en.wikipedia.org/wiki/GNU_General_Public_License) license. This limits this enhanced version of the zlib to situations that allow GPL code. On the other hand, the [zlib-ng](https://github.com/zlib-ng/zlib-ng) project attempts to accelerate zlib, but restricts itself to non-GPL code. Therefore, while zlib-ng is more attractive for many projects, it is not as fast as CloudFlare, because it does not accelerate the CRC calculation. 

This tiny project compares the conventional CRC calculation with the GPL-licensed SIMD solution as well as the BSD-licensed SIMD solution of the [Chrome](https://en.wikipedia.org/wiki/Chromium_(web_browser) web browser. Each provides the same answer, but the SIMD solutions are much faster.

Compile this program with `HAS_PCLMUL` for the BSD-licensed SIMD solution. Compile with both `HAS_PCLMUL` and `HAS_GPL` for the GPL-licensed SIMD solution (which also requires you to compile the gpl object file). Compile with neither option for the conventional solution.

Here is an example of three runs of the program, once without using SIMD, once with the BSD-licensed SIMD and once with the GPL SIMD. Note the two SIMD solutions provide similar performance.

```
>gcc -std=c11 -O3 -o tst  crc32.c zutil.c  test.c -mpclmul -lm -march=native; ./tst
Conversion required 2.138226 seconds.
CRC= 1214662048
>gcc -std=c11 -O3 -o tst  crc32.c zutil.c  test.c -DHAS_PCLMUL -mpclmul -lm -march=native; ./tst
Conversion required 0.112599 seconds.
CRC= 1214662048
>gcc  -O3 -c crc32-pclmul_asm.S -o gpl.o
>gcc -std=c11 -O3 -o tst  crc32.c zutil.c  test.c -DHAS_PCLMUL -DHAS_GPL gpl.o -mpclmul -lm -march=native; ./tst
Conversion required 0.120295 seconds.
CRC= 1214662048
```



