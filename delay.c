#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <math.h>
#include <sys/wait.h>

int main(int argc, char *argv[])
{
	double secs = strtod(argv[1], NULL);
	unsigned int usecs = round(abs(secs * 1000000));
	
	if (secs >= 0)
	{
		usleep(usecs);
		if (argv[2])
		{
			execvp(argv[2], &argv[2]);
			perror("delay");
			exit(1);
		}
		exit(0);
	}

	int stat;
	switch (fork())
	{
		case -1:
			perror("delay");
			exit(1);
		case 0:
			execvp(argv[2], &argv[2]);
			perror("delay");
			exit(1);
		default:
			wait(&stat);
			if (WIFEXITED(stat))
			{
				usleep(usecs);
				exit(WEXITSTATUS(stat));
			}
			exit(1);
	}
}
