#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <skalibs/iobuffer.h>
#include <skalibs/djbunix.h>
#include <skalibs/strerr.h>

// Like cat, but without the options.

int main(int argc, char *argv[])
{
	iobuffer buf;
	char *prog = argv[0];
	while (*(++argv))
	{
		int fd = open2(*argv, O_RDONLY);
		if (fd == -1)
		{
			perror("readFile.open");
			exit(1);
		}
		if (!iobuffer_init(&buf, fd, 1))
		{
			fprintf(stderr, "Failed to init\n");
			exit(1);
		}
		int len;
		while ((len = iobuffer_fill(&buf)) > 0) iobuffer_flush(&buf);
		if (len == -1)
		{
			strerr_warn4sys(prog, ": ", *argv, ": ");
		}
	}
}
