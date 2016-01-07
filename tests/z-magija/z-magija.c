#include <stdio.h>
#define BSZ 32

typedef struct _chunk {
    char state;
    long sstate;
    long sub[BSZ];
} chunk;

long readint()
{
    char c;
    long i = 0;
    while ((c = getc(stdin)) > 40) {
        i = i * 10 + c - 48;
    }
    return i;
}
int main()
{
    long n, m, k, i, j, bn, lim, fr, to, frx, fry, tox, toy, chs, chc;
    char c, sbn, sn, rnx, rny;
    chunk* ch;
    n = readint();
    k = readint();
    chunk cc[n / BSZ / BSZ + 1];
    for (i = n / BSZ / BSZ + 1; --i > -1 && (ch = cc + i);) {
        ch->state = ch->sstate = 0;
        for (j = BSZ; --j > -1;)
            ch->sub[j] = 0;
    }
    while (--k > -1)
    {
        while ((c = getc(stdin)) < 40);
        if (c == 49) {
            fr = readint();
            fr = readint() - 1;
            frx = fr / BSZ;
            fry = frx / BSZ;
            to = readint();
            tox = to / BSZ;
            toy = tox / BSZ;
            rnx = fr % BSZ == 0 && frx % BSZ == 0;
            rny = to % BSZ == 0 && tox % BSZ == 0;
            while (fr < to) {
                bn = fr / BSZ / BSZ;
                ch = cc + bn;
                if ((fry < bn || (rnx && fry == bn)) && (toy > bn || (rny && toy == bn))) {
                    ch->state = !ch->state;
                    fr += BSZ * BSZ;
                } else {
                    chs = 0;
                    while (fr < to && fr / BSZ / BSZ < bn + 1) {
                        sbn = fr / BSZ % BSZ;
                        lim = bn * BSZ + sbn;
                        if (frx < lim && tox > lim) {
                            chs |= 1 << sbn;
                            fr += BSZ;
                        } else {
                            chc = 1 << fr % BSZ % BSZ;
                            while (++fr < to && fr / BSZ < lim + 1) {
                                chc = (chc << 1) | chc;
                            }
                            ch->sub[sbn] ^= chc;
                        }
                    }
                    ch->sstate ^= chs;
                }
            }
        }
        else {
            n = readint();
            n = readint() - 1;
            bn = n / BSZ / BSZ;
            sbn = n / BSZ % BSZ;
            putc((cc[bn].state + ((cc[bn].sstate & 1 << sbn) != 0)
                        + ((cc[bn].sub[sbn] & 1 << n % BSZ % BSZ) != 0))
                    % 2 + 48, stdout);
            putc(10, stdout);
        }
    }
    return 0;
}
