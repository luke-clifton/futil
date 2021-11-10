#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "flatmaps",
		.output = stdout
	};

	char **endargs = argv;

	char buf[BUFSIZ];
	int cur = 0;

	while (*endargs && strcmp(*endargs, ":")) endargs++;

	if (! *endargs)
	{
		futil_die(&prog, "expected a ':' in the argument list");
	}

	char **command = endargs + 1;
	int nargs = endargs - argv - 1;
	int ncmd  = argc - nargs - 2;

	if (!ncmd) futil_die(&prog, "need at least one argument after the ':'");

	char **args = calloc(nargs, sizeof(*args));

	for(;;)
	{
		for (int i = 0; i < nargs; i++)
		{
			cur = futil_slurp_object(&prog, sizeof(buf), buf, cur, 0, &args[i]);
			if (cur == -1)
			{
				if (i > 0)
				{
					futil_die(&prog, "not enough objects");
				}
				futil_shutdown(&prog);
				exit(0);
			}
		}

		char **newv = calloc(ncmd + 1, sizeof(*newv));

		for(int i = 0; i < ncmd; i++)
		{
			newv[i] = command[i];
			for (int j = 0; j < nargs; j++)
			{
				if (! strcmp(command[i], argv[1 + j]))
				{
					newv[i] = args[j];
					break;
				}
			}
		}
		int fds[2];
		pid_t pid = futil_spawn2(&prog, fds, newv);
		close(fds[1]);
		char buf2[BUFSIZ];
		int r;
		bool terminated = true;
		while ((r = futil_read(&prog, sizeof(buf2), buf2, fds[0])))
		{
			futil_write(&prog, r, buf2);
			terminated = buf2[r-1] == 0;
		}
		if (!terminated)
		{
			futil_write(&prog, 1, "\0");
		}
		close(fds[0]);
		futil_wait(&prog, pid);
		for (int i = 0; i < nargs; i++)
		{
			free(args[i]);
		}
	}


	futil_shutdown(&prog);
}



