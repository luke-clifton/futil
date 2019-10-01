#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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
	size_t s;
	char *linep2 = NULL;
	size_t s2;
	int stat;
	FILE *h1 = fopen(argv[1], "r");
	FILE *h2 = fopen(argv[2], "r");
	while (  (getdelim(&linep, &s, 0, h1) > 0)
	      && (getdelim(&linep2, &s2, 0, h2) > 0)
	      )
	{
		fputs(linep, stdout);
		putchar(0);
		fputs(linep2, stdout);
		putchar(0);
	}
	fclose(h1);
	fclose(h2);
}
