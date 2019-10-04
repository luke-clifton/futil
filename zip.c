#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// Interleave two lists, stopping as soon as one of the lists
// is empty. Use can use this in conjunction with functions
// which read multiple values, such as mapTuple. One list
// is read from stdin, the other is read from exec'ing the
// argument and reading it's stdout.

int main(int argc, char *argv[])
{
	if (argc < 2)
	{
		fprintf(stderr, "zip requires at least one argument");
		exit(1);
	}
	char *linep = NULL;
	char *linep2 = NULL;
	int stat;
	int n = open("/dev/null", O_RDWR);
	int fds[2];
	FILE *handle;
	if (-1 == pipe(fds))
	{
	    perror("zip");
	    exit(1);
	}

	ctx cin = (ctx) { .out = stdout, .in = stdin };
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
			ctx cpi = (ctx) { .in = handle };
			while (  (linep = read_item(&cin))
			      && (linep2 = read_item(&cpi))
			      )
			{
				write_item(&cin, linep);
				write_item(&cin, linep2);
			}
			fclose(handle);
			fclose(stdin);
			if (wait(&stat))
			{
				if (WIFEXITED(stat))
				{
					return WEXITSTATUS(stat);
				}
			}
	}
}
