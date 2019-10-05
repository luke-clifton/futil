#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// With no arguments, split stdin into an array of lines.
// Otherwise, exec the argument, and split the output of
// that function into lines.

void go(int count, int fd)
{
	char buf[BUFSIZ];
	ssize_t len;
	char *pre = buf;
	char *cur = buf;
	char zero = 0;
	while (count > 0 && (len = read(fd, buf, sizeof(buf))))
	{
		if (len == -1)
		{
			perror("take");
			exit(1);
		}
		while (cur < &buf[len])
		{
			cur = memchr(pre, '\0', buf + len - pre);
			if (cur)
			{
				write(1,pre, cur - pre);
				write(1,&zero,1);
				if (--count < 1) return;
				pre = cur + 1;
			}
			else
			{
				break;
			}
		}
		if (!cur)
		{
			int l = buf + len - pre;
			if (l)
			{
				write(1,pre,l);
			}
			pre = buf;
		}
	}
}

int main(int argc, char *argv[])
{
	int count = atoi(argv[1]);
	if (argc == 2)
	{
		go(count, 0);
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
				execvp(argv[2], &argv[2]);
				perror("take");
				exit(1);
			default:
				close(fds[1]);
				go(count, fds[0]);
				close(fds[0]);
				wait(NULL);
		}
	}
}
