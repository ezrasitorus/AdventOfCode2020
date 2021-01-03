#include <stdio.h>

static int length = 30000000;
static int memo[30000000];

int setAndGet(int memo[], int last, int pos){
    int diff = memo[last] == -1 ? 0 : pos - memo[last];
    memo[last] = pos;
    return diff;
}

int main(void){

    for (int i = 0; i < length; i++)
    {
        memo[i] = -1;
    }

    memo[14] = 1;
    memo[8] = 2;
    memo[16] = 3;
    memo[0] = 4;
    memo[1] = 5;
    memo[17] = 6;

    int last = 0;

    for (int i = 7; i < length; i++)
    {
        last = setAndGet(memo, last, i);
    }

    printf("%d\n", last);
    return 0;
}