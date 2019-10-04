#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// Version of zip that works with files (useful with process substitution!)
//
// zip <(enumFromTo 1) <(cat README.md | lines) | mapTuple printf '%3d| %s'

int main(int argc, char *argv[])
{
	if (argc != 3)
	{
		fprintf(stderr, "zip requires exactly two arguments");
		exit(1);
	}
	char *linep = NULL;
	char *linep2 = NULL;
	ctx out = (ctx){.out = stdout};
	FILE *h1 = fopen(argv[1], "r");
	FILE *h2 = fopen(argv[2], "r");
	ctx c1 = (ctx){.in = h1};
	ctx c2 = (ctx){.in = h2};
	while (  (linep = read_item(&c1))
	      && (linep2 = read_item(&c2))
	      )
	{
		write_item(&out, linep);
		write_item(&out, linep2);
	}
	fclose(h1);
	fclose(h2);
}
