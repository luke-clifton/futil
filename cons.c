#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// String -> [String] -> [String]

int main(int argc, char *argv[])
{
	if (argc < 2)
	{
		fprintf(stderr, "cons requires at least one argument\n");
		exit(1);
	}
	write(1, argv[1], strlen(argv[1]) + 1);
	if (argv[2])
	{
		execvp(argv[2], &argv[2]);
		perror("cons");
		exit(1);
	}

	char buf[BUFSIZ];
	size_t len;
	while ((len = fread(buf, 1, sizeof(buf), stdin)))
	{
		if (len > fwrite(buf, 1, len, stdout))
		{
			perror("cons");
			exit(1);
		}
	}
	if (ferror(stdin))
	{
		perror("cons");
		exit(1);
	}
}
