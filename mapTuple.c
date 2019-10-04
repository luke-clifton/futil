#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

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
	char *linep2 = NULL;
	char *buf = NULL;
	size_t s = 0;
	ctx c = (ctx){.in = stdin, .out = stdout, .err = argv[0]};
	while (  (linep1 = read_item(&c))
	      && (linep2 = read_item_into(&c, &buf, &s))
	      )
	{
		newargv[argc] = linep1;
		newargv[argc+1] = linep2;
		write_item_proc(&c, &newargv[1], NULL, 0);
	}
}
