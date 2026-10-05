/* Stata's default random-number generator (c(rng_current) == "mt64").
 *
 * Stata 17's mt64 is the reference 64-bit Mersenne Twister MT19937-64
 * (Matsumoto & Nishimura, 2004) seeded with init_genrand64(seed), and
 * runiform() is genrand64_real3(), a double on the open interval (0, 1).
 * Verified against Stata 17 draws (tests/testthat/golden/rng_mt64.csv) and
 * the Python oracle qa/tools/mt64_reference.py. tabtools uses it to
 * reproduce the 2,000-row Shapiro-Wilk subsample drawn after `set seed 12345`
 * in _tabtools_common.ado:376-393.
 *
 * The state is local to each call, so R's own RNG (.Random.seed) is never
 * touched.
 *
 * Derived from the reference implementation mt19937-64.c, whose licence
 * follows (also in inst/COPYRIGHTS):
 *
 *   Copyright (C) 2004, Makoto Matsumoto and Takuji Nishimura,
 *   All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *     1. Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer.
 *
 *     2. Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 *
 *     3. The names of its contributors may not be used to endorse or promote
 *        products derived from this software without specific prior written
 *        permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include <stdint.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>

#define MT64_NN 312
#define MT64_MM 156
#define MT64_MATRIX_A UINT64_C(0xB5026F5AA96619E9)
#define MT64_UM UINT64_C(0xFFFFFFFF80000000) /* most significant 33 bits */
#define MT64_LM UINT64_C(0x7FFFFFFF)         /* least significant 31 bits */

typedef struct {
    uint64_t mt[MT64_NN];
    int mti;
} mt64_state;

static void mt64_init(mt64_state *s, uint64_t seed)
{
    s->mt[0] = seed;
    for (int i = 1; i < MT64_NN; i++) {
        s->mt[i] = UINT64_C(6364136223846793005) *
            (s->mt[i - 1] ^ (s->mt[i - 1] >> 62)) + (uint64_t) i;
    }
    s->mti = MT64_NN;
}

static uint64_t mt64_int64(mt64_state *s)
{
    static const uint64_t mag01[2] = {UINT64_C(0), MT64_MATRIX_A};
    uint64_t x;
    int i;

    if (s->mti >= MT64_NN) {
        for (i = 0; i < MT64_NN - MT64_MM; i++) {
            x = (s->mt[i] & MT64_UM) | (s->mt[i + 1] & MT64_LM);
            s->mt[i] = s->mt[i + MT64_MM] ^ (x >> 1) ^ mag01[(int) (x & UINT64_C(1))];
        }
        for (; i < MT64_NN - 1; i++) {
            x = (s->mt[i] & MT64_UM) | (s->mt[i + 1] & MT64_LM);
            s->mt[i] = s->mt[i + (MT64_MM - MT64_NN)] ^ (x >> 1) ^ mag01[(int) (x & UINT64_C(1))];
        }
        x = (s->mt[MT64_NN - 1] & MT64_UM) | (s->mt[0] & MT64_LM);
        s->mt[MT64_NN - 1] = s->mt[MT64_MM - 1] ^ (x >> 1) ^ mag01[(int) (x & UINT64_C(1))];
        s->mti = 0;
    }

    x = s->mt[s->mti++];
    x ^= (x >> 29) & UINT64_C(0x5555555555555555);
    x ^= (x << 17) & UINT64_C(0x71D67FFFEDA60000);
    x ^= (x << 37) & UINT64_C(0xFFF7EEE000000000);
    x ^= (x >> 43);
    return x;
}

/* genrand64_real3: ((x >> 12) + 0.5) / 2^52, on (0, 1) */
static double mt64_real3(mt64_state *s)
{
    return ((double) (mt64_int64(s) >> 12) + 0.5) * (1.0 / 4503599627370496.0);
}

SEXP tt_stata_runiform_c(SEXP n_, SEXP seed_)
{
    double n = asReal(n_);
    double seed = asReal(seed_);
    if (!R_FINITE(n) || n < 0 || n != floor(n) || n > (double) R_XLEN_T_MAX) {
        error("'n' must be a non-negative whole number within R's vector length limit");
    }
    if (!R_FINITE(seed) || seed < 0 || seed > 2147483647.0 || seed != (double) (int64_t) seed) {
        error("'seed' must be a whole number between 0 and 2147483647");
    }
    R_xlen_t len = (R_xlen_t) n;
    mt64_state *s = (mt64_state *) R_alloc(1, sizeof(mt64_state));
    mt64_init(s, (uint64_t) seed);
    SEXP out = PROTECT(allocVector(REALSXP, len));
    double *u = REAL(out);
    for (R_xlen_t i = 0; i < len; i++) u[i] = mt64_real3(s);
    UNPROTECT(1);
    return out;
}
