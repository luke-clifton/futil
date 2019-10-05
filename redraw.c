#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


int main(int argc, char *argv[])
{
	printf("\033[2J\033[H");
	fflush(stdout);
	if (argv[1])
	{
		execvp(argv[1], argv + 1);
		perror("redraw");
		exit(1);
	}
	exit(0);
}
