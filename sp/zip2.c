#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "zip2",
		.output = stdout
	};

	if (argc < 2) futil_die(&prog, "missing arguments");

	int fds[2];
	futil_spawn2(&prog, fds, &argv[1]);

	close(fds[1]);

	char buf1[BUFSIZ] = {0};
	int cur1 = 0;

	char buf2[BUFSIZ] = {0};
	int cur2 = 0;

	for(;;)
	{
		if (0 == cur1)
		{
			if (0 == (cur1 = futil_read(&prog, sizeof(buf1), buf1, 0)))
			{
				break;
			}
		}

		if (0 == cur2)
		{
			if (0 == (cur2 = futil_read(&prog, sizeof(buf2), buf2, fds[0])))
			{
				break;
			}
		}

		cur1 = futil_forward_object(&prog, sizeof(buf1), buf1, cur1, 0);
		futil_write(&prog, 1, "\0");
		cur2 = futil_forward_object(&prog, sizeof(buf2), buf2, cur2, fds[0]);
		futil_write(&prog, 1, "\0");

		if ((cur1 < 0) || (cur2 < 0)) break;
	}

	futil_shutdown(&prog);
}
