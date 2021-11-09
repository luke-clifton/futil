#include "futil.h"
#include <stdlib.h>
#include <sys/wait.h>
#include <string.h>

// String -> [String] -> [String]

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "list",
		.output = stdout
	};

	argv++;
	while (*argv)
	{
		futil_write(&prog, strlen(argv[0]) + 1, argv[0]);
		argv++;
	}
	futil_shutdown(&prog);
}


