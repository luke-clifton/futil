#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// First argument to `lambda` names the variable. All occurences of that
// variable in argument list get replaced with the last argument, and
// then that array is exec'ed.
//
//   lambda a cmd x a y a z r
//
// would be re-written to
// 
//   cmd x r y r z
//
// Useful for when you need to re-arrange arguments to work with commands
// like concatMap.

int main(int argc, char *argv[])
{
	char *var = argv[1];
	if (argc < 2)
	{
		fprintf(stderr, "Lambda requires at least two arguments");
		exit(1);
	}
	for (int i = 2; i < (argc - 1); i++)
	{
		if (!strcmp(var,argv[i]))
			argv[i] = argv[argc-1];
	}
	argv[argc-1] = NULL;
	if (execvp(argv[2], &(argv[2])) == -1)
	{
		perror(argv[2]);
		exit(1);
	}
	return 0;
}
