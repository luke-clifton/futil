#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Construct an array out of a variable number of command line arguments.

int main(int argc, char *argv[])
{
	while(*(++argv)) write(1, *argv, strlen(*argv) + 1);
}
