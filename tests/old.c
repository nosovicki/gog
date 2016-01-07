#include <stdio.h>
int rotate(char cc[])
{
    int i, fr = 0, to = 0;
    scanf("%d %d %d\n", &i, &fr, &to);
    for (i = fr - 1; i < to; i++) {
        cc[i] = !cc[i];
    }
}
int tell(char cc[])
{
    int i, n;
    scanf("%d %d\n", &i, &n);
    printf("%d\n", cc[n - 1]);
}
int main()
{
    int n, k, i;
    scanf("%d %d\n", &n, &k);
    char cc[n], c;
    for (i = 0; i < n; i++)
        cc[i] = 0;
    for (i = 0; i < k; i++) {
        c = getc(stdin);
        ungetc(c, stdin);
        if (c == 49)
            rotate(cc);
        else
            tell(cc);
    }
    return 0;
}
