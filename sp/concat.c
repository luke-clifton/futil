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
	if (argc > 1)
	{
		futil_spawn(&prog, &input, &argv[1]);
	}

	while (cur >= 0)
	{
		cur = futil_forward_object(&prog, sizeof(buf), buf, cur, input); 
	}
	if (input) close(input);
	futil_shutdown(&prog);
}


