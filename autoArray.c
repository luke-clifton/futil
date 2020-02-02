
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// Construct multiple arrays out of a variable number of command line arguments.
// Each array is split by a `:` argument.

int count(char **argv)
{
	int count = 0;
	while (*argv && (**argv != ':'))
	{
		argv++;
		count++;
	}
	return count;
}

int main(int argc, char *argv[])
{
	ctx c = (ctx){.out = stdout};
	argv++;
	while (*argv)
	{
		if (*argv && **argv == ':') argv++;
		printf("%d%c", count(argv), 0);
		while(*argv && (**argv != ':')) write_item(&c, *argv++);
	}
}
