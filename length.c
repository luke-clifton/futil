#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

int main(int argc, char *argv[])
{
	int i;

	if (argc == 1)
	{
		ctx c = (ctx){.in = stdin, .out = stdout};
		for(i = 0; read_item(&c); i++);
		printf("%d", i);
	}
	else
	{
		int fds[2];
		if (pipe(fds))
		{
			perror("lines.pipe");
			exit(0);
		}
		switch (fork())
		{
			case -1:
				perror("lines.fork");
				exit(1);
			case 0:
				close(fds[0]);
				dup2(fds[1],1);
				execvp(argv[1], &argv[1]);
				perror("length");
				exit(1);
			default:
				close(fds[1]);
				FILE *h = fdopen(fds[0], "r");
				ctx c = (ctx){.in = h, .out = stdout};
				for(i = 0; read_item(&c); i++);
				printf("%d", i);
				wait(NULL);
		}
	}
}
