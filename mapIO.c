#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// For each null terminated string read in from stdin, execute
// the provided function passing the string as stdin.
//
// same as 
//
//   map lambda a arg a cmd

int main(int argc, char *argv[])
{
	char *linep = NULL;
	size_t s;
	int stat;
	int fds[2];
	while (-1 != getdelim(&linep, &s, 0, stdin))
	{
		if (pipe(fds))
		{
			perror("mapIO.pipe");
			exit(1);
		}
		switch (fork())
		{
			case -1:
				perror("map:fork()");
				exit(1);
			case 0:
				close(fds[1]);
				dup2(fds[0], 0);
				execvp(argv[1], &argv[1]);
			default:
				close(fds[0]);
				write(fds[1], linep, strlen(linep));
				close(fds[1]);
				if (wait(&stat) == -1)
				{
					perror("map:wait()");
					exit(1);
				}
				if (WIFEXITED(stat))
				{
					char zero = 0;
					write(1,&zero,1);
				}
		}
	}
}
