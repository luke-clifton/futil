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
		if (argv[1][0] != '_')
			start = atol(argv[1]);
	}

	if (argc > 2)
	{
		if (argv[2][0] != '_')
			stop = atol(argv[2]);
	}

	if (argc == 4)
	{
		if (argv[3][0] != '_')
			step = atol(argv[3]);
	}

	for (long i = start; ((step > 0) && (i < stop)) || ((step < 0) && (i > stop)) || (step == 0); i += step)
	{
		char buf[BUFSIZ];
		int len = snprintf(buf, sizeof(buf), "%ld", i);
		futil_write(&prog, len + 1, buf);
		if (step > 0)
		{
			if (i > (LONG_MAX - step))
			{
				break;
			}
		}
		else
		{
			if (i < (LONG_MIN - step))
			{
				break;
			}
		}
	}

	futil_shutdown(&prog);
}

