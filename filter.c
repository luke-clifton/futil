#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

int main(int argc, char *argv[])
{
	int n = open("/dev/null", O_RDWR);
	ctx c = (ctx){
		.in = stdin,
		.out = stdout,
		.err = argv[0],
		};

	char *linep;
	while ((linep = read_item(&c)))
	{
		int stat;
		int fds[2];
		if (-1 == pipe(fds))
		{
		    perror("filter");
		    exit(1);
		}
		switch (fork())
		{
			case -1:
				perror("filter");
				exit(1);
			case 0:
				close(fds[1]);
				dup2(n, 1);
				dup2(fds[0], 0);
				execvp(argv[1], &argv[1]);
				perror("filter");
				return 0;
			default:
				close(fds[0]);
				write(fds[1], linep, strlen(linep));
				close(fds[1]);
				if (wait(&stat) == -1)
				{
					perror("filter");
					exit(1);
				}
				if (WIFEXITED(stat))
				{
					if (!WEXITSTATUS(stat))
					{
						write_item(&c, linep);
					}
				} else
				{
				    fprintf(stderr, "Process exit weird\n");
				    exit(1);
				}

		}
	}
}
