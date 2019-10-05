#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
	unsigned int usecs = atoi(argv[1]);
	usleep(usecs);
	execvp(argv[2], &argv[2]);
	perror("delay");
	exit(1);
}
