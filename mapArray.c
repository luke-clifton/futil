#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// For each null terminated string read in from stdin, execute
// the provided function with that argument.

int main(int argc, char *argv[])
{
	int len;
	char **items;
	ctx c = (ctx){.in = stdin, .out = stdout, .err = argv[0]};
	while (  (items = read_array(&c, &len))
	      )
	{
		char *newargv[argc + len];
		char **ix = newargv;
		for (int i = 1; i < argc; i++)
		{
			newargv[i-1] = argv[i];
			ix++;
		}
		while (*items) *ix++ = *items++;
		*ix = NULL;
		write_item_proc(&c, newargv, NULL, 0);
	}
}
