#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Takes a directory and a continuation which runs in the
// given directory.
//
//   withCurrentDirectory / ls
//
// Would list all the files in /, but would not change the
// working directory of the current process.

int main(int argc, char *argv[])
{
	if (argc < 3)
	{
		fprintf(stderr, "withCurrentDirectory requires at least two arguments");
		exit(1);
	}
	if (chdir(argv[1]))
	{
		perror("withCurrentDirectory.chdir");
		exit(1);
	}
	if (execvp(argv[2], &(argv[2])) == -1)
	{
		perror(argv[2]);
		exit(1);
	}
}
