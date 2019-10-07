#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/errno.h>

// Like cat, but without the options and will error out if the file
// contains a \0 byte.
//
// See also readList
//
// TODO: Should this error out, or should it encode the file?

int main(int argc, char *argv[])
{
	char buf[BUFSIZ];
	while (*(++argv))
	{
		FILE *handle = fopen(*argv, "r");
		if (!handle)
		{
			fprintf(stderr, "readFile: %s: %s\n", *argv, strerror(errno));
			exit(1);
		}
		int len;
		while ((len = fread(buf, 1, sizeof(buf), handle)))
		{
			if (memchr(buf, 0, len))
			{
				fprintf(stderr, "readFile: %s contains a 0 byte. Terminating.", *argv);
				exit(1);
			}
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
