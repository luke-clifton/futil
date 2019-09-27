#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
	while(*(++argv)) write(1, *argv, strlen(*argv));
	return 0;
}
