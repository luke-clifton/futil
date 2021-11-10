#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "fix",
		.output = stdout
	};
	int fds[2];
	futil_spawn2(&prog, fds, &argv[1]);
	ssize_t r;
	char buf[BUFSIZ];
	while ((r = futil_read(&prog, sizeof(buf), buf, fds[0])))
	{
		if (-1 == write(fds[1], buf, r))
		{
			futil_die_errno(&prog);
		}
		futil_write(&prog, r, buf);
	}
	futil_shutdown(&prog);
}
