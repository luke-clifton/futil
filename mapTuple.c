#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// For each null terminated string read in from stdin, execute
// the provided function with that argument.

int main(int argc, char *argv[])
{
	char *newargv[argc+3];
	newargv[argc+2] = NULL;
	for (int i = 1; i < argc; i++)
	{
		newargv[i] = argv[i];
	}
	char *linep1 = NULL;
	size_t s1;

	char *linep2 = NULL;
	size_t s2;
	int stat;
	while (  (-1 != getdelim(&linep1, &s1, 0, stdin))
	      && (-1 != getdelim(&linep2, &s2, 0, stdin))
	      )
	{
		switch (fork())
		{
			case -1:
				perror("zipWith:fork()");
				exit(1);
			case 0:
				newargv[argc] = linep1;
				newargv[argc+1] = linep2;
				execvp(newargv[1], &newargv[1]);
			default:
				if (wait(&stat) == -1)
				{
					perror("zipWith:wait()");
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
