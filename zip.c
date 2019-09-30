#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// For each null terminated string read in from stdin, execute
// the provided function with that argument.

int main(int argc, char *argv[])
{
	if (argc < 2)
	{
		fprintf(stderr, "zip requires at least one argument");
		exit(1);
	}
	char *linep = NULL;
	size_t s;
	char *linep2 = NULL;
	size_t s2;
	int stat;
	int n = open("/dev/null", O_RDWR);
	int fds[2];
	FILE *handle;
	if (-1 == pipe(fds))
	{
	    perror("zip");
	    exit(1);
	}
	switch (fork())
	{
		case -1:
			perror("zip.fork()");
			exit(1);
		case 0:
			close(fds[0]);
			close(0);
			dup2(n, 0);
			dup2(fds[1], 1);
			close(fds[1]);
			execvp(argv[1], &argv[1]);
		default:
			close(fds[1]);
			if (!(handle = fdopen(fds[0], "r")))
			{
				perror("zip.fdopen()");
				exit(1);
			}
			while (  (getdelim(&linep, &s, 0, stdin) > 0)
			      && (getdelim(&linep2, &s2, 0, handle) > 0)
			      )
			{
				fputs(linep, stdout);
				putchar(0);
				fputs(linep2, stdout);
				putchar(0);
			}
			fclose(stdin);
			fclose(handle);
			if (wait(&stat))
			{
				if (WIFEXITED(stat))
				{
					return WEXITSTATUS(stat);
				}
			}
	}
}
