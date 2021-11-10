#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "unlines",
		.output = stdout
	};

	int input = 0;

	if (argc > 1)
	{
		futil_spawn(&prog, &input, &argv[1]);
	}
	
	ssize_t r;
	char buf[BUFSIZ];
	bool terminated = true;
	while ((r = futil_read(&prog, sizeof(buf), buf, input)))
	{
		char *x;
		while ((x = memchr(buf, 0, r)))
		{
			*x = '\n';
		}
		terminated = buf[r-1] == '\n';
		futil_write(&prog, r, buf);
	}
	if (!terminated)
	{
		futil_write(&prog, 1, "\n");
	}
	if (input) close(input);
	futil_shutdown(&prog);
}
