#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <math.h>
#include <sys/wait.h>
#include <signal.h>

// Sleep for n seconds before executing a command.
//
// If n is negative, execute the command first, then sleep for abs(n)
// seconds.

int main(int argc, char *argv[])
{
	double secs = strtod(argv[1], NULL);
	unsigned int usecs = round(fabs(secs * 1000000));
	
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
	if (!argv[2])
	{
		fprintf(stderr, "delay: called with negative argument requires a command to execute");
		exit(1);
	}
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
			if (WTERMSIG(stat))
			{
				if (0 > raise(WTERMSIG(stat)))
				{
					perror("delay");
					exit(1);
				}
			}
			exit(1);
	}
}
