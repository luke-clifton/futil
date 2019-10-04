#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// For each null terminated string read in from stdin, execute
// the provided function with that argument.

int main(int argc, char *argv[])
{
	ctx c = (ctx){
		.in = stdin,
		.out = stdout,
		.err = argv[0],
		};

	for (int i = 0; i < argc; i++)
	{
		argv[i] = argv[i+1];
	}
	char *linep = NULL;
	while ((linep = read_item(&c)))
	{
		argv[argc-1] = linep;
		write_item_proc(&c, &argv[0]);
	}
}
