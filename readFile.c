#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

// Like cat, but without the options.

int main(int argc, char *argv[])
{
	char buf[BUFSIZ];
	while (*(++argv))
	{
		FILE *handle = fopen(*argv, "r");
		if (!handle)
		{
			perror("readFile.fopen");
			exit(1);
		}
		int len;
		while ((len = fread(buf, 1, sizeof(buf), handle)))
		{
			if (len > fwrite(buf, 1, len, stdout))
			{
				perror("readFile");
				exit(1);
			}
		}
		if (ferror(handle))
		{
			perror("readFile.fread");
			exit(1);
		}
	}
}
