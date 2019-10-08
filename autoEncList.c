#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>
#include <sys/wait.h>

// Construct an array out of a variable number of command line arguments.

char **command(char **argv)
{
	while (*argv)
	{
		if (!strcmp(*argv, ">>="))
			return argv + 1;
		argv++;
	}
	return NULL;
}

int main(int argc, char *argv[])
{
	char **cmd = command(argv);
	int fds[2];
	FILE *h;
	if (cmd)
	{
		if (pipe(fds))
		{
			perror("autoEncList");
			exit(1);
		}
		switch (fork())
		{
			case -1:
				perror("autoEncList");
				exit(1);
			case 0:
				close(fds[1]);
				dup2(fds[0], 0);
				execvp(*cmd, cmd);
				perror("autoEncList");
				exit(1);
			default:
				close(fds[0]);
				h = fdopen(fds[1], "w");
				if (!h)
				{
					perror("autoEncList");
					exit(1);
				}
				break;
		}
	}
	else
	{
		h = stdout;
	}
	argv++;
	while (*argv && strcmp(*argv, ">>="))
	{
		if (*argv && **argv == ':')
		{
			fprintf(h, "%c", 0);
			argv++;
		}
		
		while(*argv && strcmp(*argv, ":") && strcmp(*argv, ">>="))
		{
			fprintf(h, "%s\1\1", *argv++);
		}
	}
	fprintf(h, "%c", 0);
	fclose(h);
	wait(NULL);
}
