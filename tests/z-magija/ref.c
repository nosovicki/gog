#include <stdio.h>
#include <stdlib.h>

#define MaxN 200002
long n, k, s [MaxN];

long sumiraj (long x)
{
    long pom;

    pom = 0;
    while (x > 0)
    {
        pom = pom + s [x];
        x = x - (x & (x ^ (x - 1)));
    }
    return pom;
}

void com1 (long a, long b)
{
    while (a<= n)
    {
        s [a] = s [a] + 1;
        a = a + (a & (a ^ (a - 1)));
    }
    b++;
    while (b <=n )
    {
        s [b] = s [b] - 1;
        b = b + (b & (b ^ (b - 1)));
    }
}

int main ()
{
    long i, c, a, b;

    scanf ("%ld %ld", &n, &k);
    for (i = 0; i <= n; i++)
        s [i] = 0;
    for (i = 1; i <= k; i++)
    {
        scanf ("%ld", &c);
        if (c == 1)
        {
            scanf ("%ld %ld", &a, &b);
            com1 (a, b);
        }
        else
        {
            scanf ("%ld", &a);
            if (sumiraj(a) % 2 == 0)
                printf ("0\n");
            else
                printf ("1\n");
        }
    }
    return 0;
}
