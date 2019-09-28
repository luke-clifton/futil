#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
	int stat;
	int fds[2];
	if (-1 == pipe(fds))
	{
	    perror("filter");
	    exit(1);
	}
	switch (fork())
	{
		case -1:
			perror("filter");
			exit(1);
		case 0:
			close(fds[1]);
			dup2(fds[0], 0);
			execvp(argv[2], &argv[2]);
		default:
			close(fds[0]);
			write(fds[1], argv[1], strlen(argv[1]));
			close(fds[1]);
			if (wait(&stat) == -1)
			{
				perror("filter");
				exit(1);
			}
			if (WIFEXITED(stat))
			{
				return WEXITSTATUS(stat);
			} else
			{
			    fprintf(stderr, "Process exit weird\n");
			    exit(1);
			}

	}
}
