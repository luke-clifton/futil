#include "futil.h"
#include <limits.h>

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "enum",
		.output = stdout
	};


	long start = 0;
	long stop  = LONG_MAX;
	long step  = 1;

	if (argc > 1)
	{
		start = atol(argv[1]);
	}

	if (argc > 2)
	{
		stop = atol(argv[2]);
	}

	if (argc == 4)
	{
		step = atol(argv[3]);
	}

	for (long i = start; i < stop; i += step)
	{
		char buf[BUFSIZ];
		int len = snprintf(buf, sizeof(buf), "%ld", i);
		futil_write(&prog, len + 1, buf);
	}

	futil_shutdown(&prog);
}

