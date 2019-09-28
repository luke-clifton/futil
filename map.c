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
	while (-1 != getdelim(&linep, &s, 0, stdin))
	{
		switch (fork())
		{
			case -1:
				perror("map:fork()");
				exit(1);
			case 0:
				argv[argc-1] = linep;
				execvp(argv[0], &argv[0]);
			default:
				if (wait(&stat) == -1)
				{
					perror("forever");
					exit(1);
				}
				if (WIFEXITED(stat))
				{
					if (WEXITSTATUS(stat))
					{
						exit(1);
					}
				}
		}
	}
}
