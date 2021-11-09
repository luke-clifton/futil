#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "concat",
		.output = stdout
	};

	char buf[BUFSIZ];
	int cur = 0;

	int input = 0;
	int pid = 0;
	if (argc > 1)
	{
		pid = futil_spawn(&prog, &input, &argv[1]);
	}

	while (cur >= 0)
	{
		cur = futil_forward_object(&prog, sizeof(buf), buf, cur, input); 
	}
	if (input) close(input);
	futil_shutdown(&prog);
	if (pid)
	{
		int status;
		waitpid(pid, &status, 0);
		if (WIFEXITED(status))
		{
			return WEXITSTATUS(status);
		}
		if (WIFSIGNALED(status))
		{
			kill(getpid(), WTERMSIG(status));
		}
	}
}


