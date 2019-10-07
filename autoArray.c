
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// Construct an array out of a variable number of command line arguments.

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
		printf("%d%c", count(argv), 0);
		while(*argv && (**argv != ':')) write_item(&c, *argv++);
		if (*argv && **argv == ':') argv++;
	}
}
