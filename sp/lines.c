#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "lines",
		.output = stdout
	};

	int input = 0;

	if (argc > 1)
	{
		futil_spawn(&prog, &input, &argv[1]);
	}
	
	ssize_t r;
	char buf[BUFSIZ];
	while ((r = futil_read(&prog, sizeof(buf), buf, input)))
	{
		char *x;
		while ((x = memchr(buf, '\n', r)))
		{
			*x = 0;
		}
		futil_write(&prog, r, buf);
	}
	if (input) close(input);
	futil_shutdown(&prog);
}
