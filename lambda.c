#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Arguments before the first ':' argument are variable names. Any occurence
// of those names in the command portion of the arguments are replaced with
// argument from the argument portion of the arguments.
//
//   lambda [vars..] : [cmd..] [args..]
//
// The number of vars and args must be the same.
//
//   lambda a : cmd x a y a z r
//
// would be re-written to
// 
//   cmd x r y r z
//
// and
//
//   lambda a b : cmd x a b a 1 2
//
// to
//
//   cmd x 1 2 1
//
// Useful for when you need to re-arrange arguments to work with commands
// like map.
//
// Note that nested lambdas are a bit confusing as the bind from the outside
// in.
//
//   lambda x : lambda y : cmd x y a b
//
// becomes
//
//   cmd b a
//
// which might be the opposite of what you expected.

int main(int argc, char *argv[])
{
	char **c = argv + 1;
	int nvars = 0;
	while (*c && strcmp(*c, ":"))
	{
		c++;
		nvars++;
	}

	if (argc < 2 + nvars + nvars)
	{
		fprintf(stderr, "usage: %s [names..] : [cmd..] [args (match names)]\n", argv[0]);
		exit(0);
	}

	if (!*c)
	{
		fprintf(stderr, "missing ':'");
		exit(1);
	}

	char *args[argc];
	char **k = args;
	char **s = c++;
	while (*c && c <= (&argv[argc] - s + argv))
	{
		*k = NULL;
		for (char **a = argv + 1; a != s; a++)
		{
			if (!strcmp(*c,*a))
			{
				*k = *(&argv[argc] - s + a);
				break;
			}
		}
		if (!*k)
		{
			*k = *c;
		}
		k++;
		c++;
	}
	*k = NULL;
	k = args;
	if (execvp(args[0], args) == -1)
	{
		perror("lambda.execvp");
		exit(1);
	}
	return 0;
}
