#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// String -> [String] -> [String]

int main(int argc, char *argv[])
{
	if (argc < 3)
	{
		fprintf(stderr, "cons requires at least two arguments\n");
		exit(1);
	}
	write(1, argv[1], strlen(argv[1]) + 1);
	execvp(argv[2], &argv[2]);
}
