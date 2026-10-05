"""Reference MT19937-64 reproducing Stata's default RNG (c(rng_current) == "mt64").

Verified 2026-09-25 against Stata 17: after `set seed 12345`, runiform()
equals genrand64_real3 below for all of the first 4,500 draws (to 18 decimal
places), and the 2,000-row subsample picked by `sort u _n` + `in 1/2000`
(_tabtools_common.ado:375-394) matched with zero membership mismatches.
This is the oracle for src/stata_mt64.c (plan task 2.4b).

Usage: python3 mt64_reference.py SEED N  -> prints N draws, one per line, %.18f
"""
import sys

MASK = (1 << 64) - 1
NN, MM = 312, 156
MATRIX_A = 0xB5026F5AA96619E9
UM, LM = 0xFFFFFFFF80000000, 0x7FFFFFFF


class MT64:
    def __init__(self, seed):
        self.mt = [0] * NN
        self.mt[0] = seed & MASK
        for i in range(1, NN):
            prev = self.mt[i - 1]
            self.mt[i] = (6364136223846793005 * (prev ^ (prev >> 62)) + i) & MASK
        self.mti = NN

    def genrand64_int64(self):
        if self.mti >= NN:
            for i in range(NN):
                x = (self.mt[i] & UM) | (self.mt[(i + 1) % NN] & LM)
                self.mt[i] = self.mt[(i + MM) % NN] ^ (x >> 1) ^ (MATRIX_A if x & 1 else 0)
            self.mti = 0
        x = self.mt[self.mti]
        self.mti += 1
        x ^= (x >> 29) & 0x5555555555555555
        x ^= (x << 17) & 0x71D67FFFEDA60000 & MASK
        x ^= (x << 37) & 0xFFF7EEE000000000 & MASK
        x ^= x >> 43
        return x

    def runiform(self):
        """genrand64_real3: uniform on the open interval (0, 1)."""
        return ((self.genrand64_int64() >> 12) + 0.5) / 4503599627370496.0


if __name__ == "__main__":
    seed, n = int(sys.argv[1]), int(sys.argv[2])
    g = MT64(seed)
    for _ in range(n):
        print("%.18f" % g.runiform())
