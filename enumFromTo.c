#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// For each null terminated string read in from stdin, execute
// the provided function with that argument.

// KNOWN BUGS: overflow is an issue. Not sure on desired semantics yet.

int main(int argc, char *argv[])
{
	if (argc < 2 || argc > 4)
	{
		fprintf(stderr, "usage: %s start [end] [step]", argv[0]);
		exit(1);
	}

	int from = atoi(argv[1]);
	int to   = argc > 2 ? atoi(argv[2]) : INT_MAX;
	int step = argc > 3 ? atoi(argv[3]) : 1;
	while (step > 0 ? from <= to : from >= to)
	{
		printf("%d%c", from, 0);
		from += step;
	}
}
