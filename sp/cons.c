#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "cons",
		.output = stdout
	};

	if (argc < 2)
	{
		futil_die(&prog, "requires at least one argument");
	}
	futil_write(&prog, strlen(argv[1]) + 1, argv[1]);
	if (argv[2])
	{
		futil_exec(&prog, &argv[2]);
	}
	ssize_t r;
	char buf[BUFSIZ];
	while ((r = futil_read(&prog, sizeof(buf), buf, 0)))
	{
		futil_write(&prog, r, buf);
	}
	futil_shutdown(&prog);
}
