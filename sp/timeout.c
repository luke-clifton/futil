#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include "futil.h"

// String -> [String] -> [String]

int main(int argc, char *argv[])
{
	if (argc < 3)
	{
		fprintf(stderr, "timeoutx requires at least two argument\n");
		exit(1);
	}
	unsigned long d = strtoul(argv[1], NULL, 10);
	int pipes[2];
	if (-1 == pipe(pipes))
	{
		die_perror("timeout");
	}
	pid_t ppid = getpid();

	switch (fork())
	{
		case -1:
			die_perror("timeout: fork:");
			break;
		case 0:
			// child
			close(pipes[1]);
			fprintf(stderr, "Starting delay: %ld\n", d);
			fd_set fds;
			FD_ZERO(&fds);
			FD_SET(pipes[0], &fds);
			struct timeval t = (struct timeval){d,0};
			if (select(pipes[0]+1, &fds, NULL, NULL, &t))
			{
				exit(0);
			}
			else
			{
				fprintf(stderr, "Timeout reached, terminating...\n");
				kill(ppid,SIGTERM);
				exit(0);
			}
			break;
		default:
			close(pipes[0]);
			if (argv[2])
			{
				execvp(argv[2], &argv[2]);
				perror("cons");
				exit(1);
			}
	}
}

