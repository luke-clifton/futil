#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Run the continuation N times

int main(int argc, char *argv[])
{
	int n = atoi(argv[1]);
	for(;n;n--)
	{
		switch (fork())
		{
			case -1:
				perror("iterate:fork()");
				exit(1);
			case 0:
				execvp(argv[2], &argv[2]);
			default:
				if (wait(NULL) == -1)
				{
					perror("iterate:wait()");
					exit(1);
				}
		}
	}
}
