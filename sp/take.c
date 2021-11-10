#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "take",
		.output = stdout
	};

	if (argc < 2)
	{
		futil_die(&prog, "requires at least one argument");
	}
	char buf[BUFSIZ];
	int count = atoi(argv[1]);
	int cur = 0;

	int input = 0;
	if (argc > 2)
	{
		futil_spawn(&prog, &input, &argv[2]);
	}

	while ((cur >= 0) && (count > 0))
	{
		cur = futil_forward_object(&prog, sizeof(buf), buf, cur, input); 
		if (cur > -2)
			futil_write(&prog, 1, "\0");
		count--;
	}
	if (input) close(input);
	futil_shutdown(&prog);
}

