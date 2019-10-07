#include <stdio.h>
#include <limits.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// Unlike xargs, we intentionally do not break up the argument list to fit
// into ARG_MAX or anything like that. The whole point of this is to turn
// the input into a single command.

// A largish number. This limit is simply here to terminate the program
// when we suspect it was going to fail to execvp anyway. Portable scripts
// will probably want to keep it small anyway.

#define ARG_SIZE_LIMIT 10000000

int main(int argc, char *argv[])
{
	size_t argsize = 0;
	ctx c = (ctx){.in = stdin, .out=stdout, .err="arg"};
	int size = argc + 20;
	char **args = malloc(size * sizeof(char *));
	if (!args)
	{
		exit(1);
	}

	int i = 0;
	char **a = &argv[1];
	while(*a)
	{
		args[i] = *a;
		a++;
		i++;
	}
	for(;;i++)
	{
		if (i >= size - 1)
		{
			size += size;
			if (!(args = realloc(args, size * sizeof(char*))))
			{
				perror("arg");
				exit(1);
			}
		}
		size_t s = 0;
		args[i] = NULL;
		if (!(args[i] = read_item_into(&c, &args[i], &s)))
			break;
		argsize += strlen(args[i]);
		if (argsize > ARG_SIZE_LIMIT)
		{
			fprintf(stderr, "%s: Argument list too long", argv[0]);
			exit(1);
		}
	}
	if (*args)
	{
		execvp(*args, args);
		perror("arg");
		exit(1);
	}
	exit(0);
}
