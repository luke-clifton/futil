#include <stdio.h>
#include <limits.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// Slurp in an entire list, and the output it prefixed by it's length.

int main(int argc, char *argv[])
{
	ctx c = (ctx){.in = stdin, .out=stdout, .err="arg"};
	int size = argc + 20;
	char **args = malloc(size * sizeof(char *));
	if (!args)
	{
		exit(1);
	}

	int i = 0;
	char **a = &argv[1];
	while(*a)
	{
		args[i] = *a;
		a++;
		i++;
	}
	for(;;i++)
	{
		if (i >= size - 1)
		{
			size += size;
			if (!(args = realloc(args, size * sizeof(char*))))
			{
				perror("arg");
				exit(1);
			}
		}
		size_t s = 0;
		args[i] = NULL;
		if (!(args[i] = read_item_into(&c, &args[i], &s)))
			break;
	}
	printf("%d%c", i, 0);
	while (*args)
	{
		write_item(&c, *args);
		args++;
	}
}
