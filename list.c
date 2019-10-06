#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// Construct an array out of a variable number of command line arguments.

int main(int argc, char *argv[])
{
	ctx c = (ctx){.out = stdout};
	while(*(++argv)) write_item(&c, *argv);
}
