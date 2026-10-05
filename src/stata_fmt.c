/* Exact, platform-independent printf("%.df") and printf("%.de").
 *
 * Stata's string(x, "%w.df") rounds the exact binary value of x, with exact
 * ties to even, as glibc's printf does (golden/stata_fmt.csv: 0.45 is
 * 0.4500000000000000111... and prints "0.5"; 0.5 prints "0", 1.5 "2").
 * R's sprintf() on Windows does not: it printed 0.45 as "0.4" and the exact
 * tie 97907712.5 as "97907713" (CI, windows-latest, R 4.5). So the display
 * text never goes through the C library's floating-point formatting.
 *
 * A finite double is m * 2^e with m a 53-bit integer. Its decimal expansion
 * is finite: m * 2^e for e >= 0, or m * 5^-e / 10^-e for e < 0. Both are
 * formed exactly in multi-word integer arithmetic (at most 2,547 bits, for
 * the smallest subnormal), converted to decimal digits, and rounded in
 * decimal: up when the first dropped digit is above 5, or 5 followed by a
 * nonzero digit; to even when it is exactly 5 followed by zeros. No
 * floating-point arithmetic is involved, so compiler flags, x87 precision
 * and the C runtime cannot change the result.
 *
 * The output matches C99 printf: a '-' for any value with the sign bit set
 * ("-0.0" for -0.04, as glibc), an exponent of at least two digits, and the
 * '#' flag keeping the point of a zero-decimal mantissa ("2.e+200").
 */
#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

#define TT_LIMBS 88      /* 2,547 bits need 80 32-bit words */
#define TT_MAXDIG 800    /* 767 significant digits at most */

typedef struct {
    uint32_t w[TT_LIMBS];
    int n;               /* words in use; w[n - 1] != 0 unless n == 0 */
} tt_big;

static void big_set_u64(tt_big *b, uint64_t v)
{
    b->n = 0;
    while (v) {
        b->w[b->n++] = (uint32_t) (v & 0xffffffffu);
        v >>= 32;
    }
}

static void big_mul_small(tt_big *b, uint32_t k)
{
    uint64_t carry = 0;
    for (int i = 0; i < b->n; i++) {
        uint64_t t = (uint64_t) b->w[i] * k + carry;
        b->w[i] = (uint32_t) (t & 0xffffffffu);
        carry = t >> 32;
    }
    if (carry) b->w[b->n++] = (uint32_t) carry;
}

static void big_shl(tt_big *b, int bits)
{
    if (b->n == 0 || bits == 0) return;
    int words = bits / 32, r = bits % 32;
    if (r) {
        uint32_t carry = 0;
        for (int i = 0; i < b->n; i++) {
            uint32_t v = b->w[i];
            b->w[i] = (v << r) | carry;
            carry = v >> (32 - r);
        }
        if (carry) b->w[b->n++] = carry;
    }
    if (words) {
        for (int i = b->n - 1; i >= 0; i--) b->w[i + words] = b->w[i];
        for (int i = 0; i < words; i++) b->w[i] = 0;
        b->n += words;
    }
}

/* Decimal digits of b (destroyed), most significant first, no leading
 * zeros; returns the digit count (0 for b == 0). */
static int big_to_dec(tt_big *b, char *out)
{
    char rev[TT_MAXDIG + 16];
    int nd = 0;
    while (b->n > 0) {
        uint64_t rem = 0;
        for (int i = b->n - 1; i >= 0; i--) {
            uint64_t cur = (rem << 32) | b->w[i];
            b->w[i] = (uint32_t) (cur / 1000000000u);
            rem = cur % 1000000000u;
        }
        while (b->n > 0 && b->w[b->n - 1] == 0) b->n--;
        for (int k = 0; k < 9; k++) {
            rev[nd++] = (char) ('0' + rem % 10);
            rem /= 10;
        }
    }
    while (nd > 0 && rev[nd - 1] == '0') nd--;
    for (int i = 0; i < nd; i++) out[i] = rev[nd - 1 - i];
    out[nd] = '\0';
    return nd;
}

/* |x| = D * 10^-s exactly, D the returned digit string (length *n, no
 * leading zeros; "" for zero). */
static void exact_decimal(double a, char *D, int *n, int *s)
{
    if (a == 0) {
        D[0] = '\0';
        *n = 0;
        *s = 0;
        return;
    }
    int e2;
    double f = frexp(a, &e2);                 /* a = f * 2^e2, f in [0.5, 1) */
    uint64_t m = (uint64_t) ldexp(f, 53);     /* exact: f has <= 53 bits */
    int e = e2 - 53;
    while ((m & 1u) == 0 && e < 0) {
        m >>= 1;
        e++;
    }
    tt_big b;
    big_set_u64(&b, m);
    if (e >= 0) {
        big_shl(&b, e);
        *s = 0;
    } else {
        int k = -e;
        while (k >= 13) {
            big_mul_small(&b, 1220703125u);  /* 5^13 */
            k -= 13;
        }
        uint32_t p = 1;
        while (k-- > 0) p *= 5u;
        big_mul_small(&b, p);
        *s = -e;
    }
    *n = big_to_dec(&b, D);
}

/* Round the digit string D (length n) to its first K digits (K >= 0), half
 * to even on exact ties. Writes the kept digits to Q (padded with zeros when
 * K > n; "" when K == 0 and the value rounds down) and returns their count,
 * which is K + 1 when rounding carried into a new leading digit. */
static int round_digits_rule(const char *D, int n, int K, char *Q, int halfup)
{
    if (K >= n) {
        memcpy(Q, D, (size_t) n);
        memset(Q + n, '0', (size_t) (K - n));
        Q[K] = '\0';
        return K;
    }
    memcpy(Q, D, (size_t) K);
    Q[K] = '\0';
    int up = 0;
    char next = D[K];
    if (next > '5' || (halfup && next == '5')) up = 1;
    else if (next == '5') {
        int rest = 0;
        for (int i = K + 1; i < n; i++) if (D[i] != '0') { rest = 1; break; }
        if (rest) up = 1;
        else up = K > 0 && ((D[K - 1] - '0') & 1);
    }
    if (!up) return K;
    int i = K - 1;
    while (i >= 0 && Q[i] == '9') Q[i--] = '0';
    if (i >= 0) {
        Q[i]++;
        return K;
    }
    memmove(Q + 1, Q, (size_t) K + 1);
    Q[0] = '1';
    return K + 1;
}

static int round_digits(const char *D, int n, int K, char *Q)
{
    return round_digits_rule(D, n, K, Q, 0);
}

/* The exact digits D (n of them, s decimals) rounded to 19 significant
 * digits (Stata's 18-decimal mantissa), exact ties to even; D and n, s are
 * updated in place. */
static void to_19_digits(char *D, int *n, int *s, char *Q)
{
    if (*n <= 19) return;
    int q = round_digits(D, *n, 19, Q);
    *s -= *n - 19;
    if (q > 19) {          /* carried into a new leading digit: 10...0 */
        Q[19] = '\0';
        (*s)--;
    }
    memcpy(D, Q, 20);
    *n = 19;
}

static void fmt_fixed(double x, int d, int alt, int stata, char *out, char *D, char *Q)
{
    int n, s;
    exact_decimal(fabs(x), D, &n, &s);
    if (stata) to_19_digits(D, &n, &s, Q);
    int K = n - s + d, q;
    if (n == 0 || K < 0) {
        Q[0] = '\0';
        q = 0;
    } else {
        q = round_digits_rule(D, n, K, Q, stata);
    }
    char *o = out;
    if (signbit(x)) *o++ = '-';
    /* Q holds round(|x| * 10^d) without leading zeros; pad to d + 1. */
    int pad = (q < d + 1) ? d + 1 - q : 0;
    int total = q + pad, ip = total - d;
    for (int i = 0; i < total; i++) {
        if (i == ip) *o++ = '.';
        *o++ = (i < pad) ? '0' : Q[i - pad];
    }
    if (d == 0 && alt) *o++ = '.';
    *o = '\0';
}

static void fmt_exp(double x, int d, int alt, int stata, char *out, char *D, char *Q)
{
    int n, s, E, q;
    exact_decimal(fabs(x), D, &n, &s);
    if (n == 0) {
        memset(Q, '0', (size_t) d + 1);
        Q[d + 1] = '\0';
        E = 0;
    } else {
        E = n - s - 1;
        if (stata && d < 18) {
            /* Stata's e-notation: the mantissa at 18 decimals (exact ties
             * to even), then rounded half up (away from zero) to d. */
            int s19 = s, n19 = n;
            to_19_digits(D, &n19, &s19, Q);
            E = n19 - s19 - 1;
            q = round_digits_rule(D, n19, d + 1, Q, 1);
        } else {
            q = round_digits(D, n, d + 1, Q);
        }
        if (q > d + 1) {
            E++;
            Q[d + 1] = '\0';
        }
    }
    char *o = out;
    if (signbit(x)) *o++ = '-';
    *o++ = Q[0];
    if (d > 0 || alt) *o++ = '.';
    for (int i = 1; i <= d; i++) *o++ = Q[i];
    *o++ = 'e';
    *o++ = E < 0 ? '-' : '+';
    int ae = E < 0 ? -E : E;
    char eb[8];
    int ne = 0;
    do {
        eb[ne++] = (char) ('0' + ae % 10);
        ae /= 10;
    } while (ae);
    if (ne < 2) eb[ne++] = '0';
    while (ne) *o++ = eb[--ne];
    *o = '\0';
}

/* x: doubles; d: decimals (recycled); mode: 0 = "%.df", 1 = "%.de",
 * 2 = "%.de" and 3 = "%.df" with Stata's %g / e-notation rounding (the
 * 18-decimal mantissa, then half up);
 * alt: the '#' flag. Non-finite values give NA (the caller formats them). */
SEXP tt_fmt_c(SEXP x_, SEXP d_, SEXP mode_, SEXP alt_)
{
    if (TYPEOF(x_) != REALSXP) error("'x' must be a double vector");
    if (TYPEOF(d_) != INTSXP || XLENGTH(d_) < 1) error("'d' must be an integer vector");
    R_xlen_t n = XLENGTH(x_), nd = XLENGTH(d_);
    int mode = asInteger(mode_), alt = asLogical(alt_) == TRUE;
    const double *x = REAL(x_);
    const int *d = INTEGER(d_);
    SEXP out = PROTECT(allocVector(STRSXP, n));
    char D[TT_MAXDIG + 16];
    for (R_xlen_t i = 0; i < n; i++) {
        int di = d[i % nd];
        if (!R_FINITE(x[i]) || di == NA_INTEGER) {
            SET_STRING_ELT(out, i, NA_STRING);
            continue;
        }
        if (di < 0 || di > 400) error("'d' must be between 0 and 400");
        const void *vmax = vmaxget();
        size_t len = TT_MAXDIG + (size_t) di + 32;
        char *Q = R_alloc(len, 1), *buf = R_alloc(len, 1);
        if (mode == 1 || mode == 2) fmt_exp(x[i], di, alt, mode == 2, buf, D, Q);
        else fmt_fixed(x[i], di, alt, mode == 3, buf, D, Q);
        SET_STRING_ELT(out, i, mkChar(buf));
        vmaxset(vmax);
    }
    UNPROTECT(1);
    return out;
}
