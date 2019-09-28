#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    int n = open("/dev/null", O_RDWR);
    dup2(n, 1);
    execvp(argv[1], &argv[1]);   
    return 0;
}
