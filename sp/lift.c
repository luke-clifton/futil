#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "lift",
		.output = stdout
	};


	int input = 0;
	if (argc > 1)
	{
		futil_spawn(&prog, &input, &argv[1]);
	}

	int r;
	char buf[BUFSIZ];
	bool terminated = false;
	while ((r = futil_read(&prog, sizeof(buf), buf, input)))
	{
		terminated = true;
		if (memchr(buf, 0, r))
		{
			futil_die(&prog, "did not expect a '0' byte in the input");
		}
		futil_write(&prog, r, buf);
	}

	if (!terminated)
	{
		futil_write(&prog, 1, "\0");
	}

	if (input)
	{
		close(input);
	}

	futil_shutdown(&prog);
}
