#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
	char *linep = NULL;
	size_t s;
	ssize_t len;
	int n = open("/dev/null", O_RDWR);
	while (-1 != (len = getdelim(&linep, &s, 0, stdin)))
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
			default:
				close(fds[0]);
				write(fds[1], linep, len - 1);
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
					write(1, linep, len);
				    }
				} else
				{
				    fprintf(stderr, "Process exit weird\n");
				    exit(1);
				}

		}
	}
}
