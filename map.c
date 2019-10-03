#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// For each null terminated string read in from stdin, execute
// the provided function with that argument.

int main(int argc, char *argv[])
{
	for (int i = 0; i < argc; i++)
	{
		argv[i] = argv[i+1];
	}
	char *linep = NULL;
	size_t s;
	int stat;
	int tail = 0;
	while (-1 != getdelim(&linep, &s, 0, stdin))
	{
		char zero = 0;
		if (tail) write(1,&zero,1);
		tail = 1;
		switch (fork())
		{
			case -1:
				perror("map:fork()");
				exit(1);
			case 0:
				argv[argc-1] = linep;
				if (-1 == execvp(argv[0], &argv[0]))
				{
					perror("main.execvp");
					exit(1);
				}
			default:
				if (wait(&stat) == -1)
				{
					perror("map:wait()");
					exit(1);
				}
		}
	}
}
