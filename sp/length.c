#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "length",
		.output = stdout
	};

	int input = 0;
	pid_t pid = 0;

	if (argc > 1)
	{
		pid = futil_spawn(&prog, &input, &argv[1]);
	}
	
	ssize_t r;
	char buf[BUFSIZ];
	ssize_t count = 0;
	bool terminated = true;
	while ((r = futil_read(&prog, sizeof(buf), buf, input)))
	{
		terminated = 0 == buf[r - 1];
		char *s = buf;
		char *e = &buf[r];
		while ((s = memchr(s, 0, e - s)))
		{
			count++;
			s++;
		}
	}
	if (!terminated)
	{
		count++;
	}
	r = snprintf(buf, sizeof(buf), "%ld", count);
	futil_write(&prog, r, buf);

	if (input) close(input);
	futil_shutdown(&prog);
	if (pid)
	{
		waitpid(pid, NULL, 0);
	}
}

