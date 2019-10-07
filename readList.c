#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>

// Like cat, but without the options, and will ensure that the output is
// '\0' terminated.
//
// See also readFile

int main(int argc, char *argv[])
{
	char buf[BUFSIZ];
	int terminated = 1;
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
			terminated = !buf[len - 1];
		}
		if (ferror(handle))
		{
			perror("readFile.fread");
			exit(1);
		}
		if (!terminated) fputc(0, stdout);
	}
}
