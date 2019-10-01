#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// With no arguments, split stdin into an array of lines.
// Otherwise, exec the argument, and split the output of
// that function into lines.

void go(int fd)
{
	char buf[BUFSIZ];
	ssize_t len;
	char *pre = buf;
	char *cur = buf;
	char zero = 0;
	while ((len = read(fd, buf, sizeof(buf))))
	{
		if (len == -1)
		{
			perror("concat.read");
			exit(1);
		}
		while (cur < &buf[len])
		{
			cur = memchr(pre, 0, buf + len - pre);
			if (cur)
			{
				write(1,pre, cur - pre);
				pre = cur + 1;
			}
			else
			{
				break;
			}
		}
		if (!cur)
		{
			write(1,pre, buf + len - pre);
			pre = buf;
		}
	}
}

int main(int argc, char *argv[])
{
	if (argc == 1)
	{
		go(0);
	}
	else
	{
		int fds[2];
		if (pipe(fds))
		{
			perror("concat.pipe");
			exit(0);
		}
		switch (fork())
		{
			case -1:
				perror("concat.fork");
				exit(1);
			case 0:
				close(fds[0]);
				dup2(fds[1],1);
				execvp(argv[1], &argv[1]);
			default:
				close(fds[1]);
				go(fds[0]);
				wait(NULL);
		}
	}
}
