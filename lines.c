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
	int new = 0;
	while ((len = read(fd, buf, sizeof(buf))))
	{
		if (len == -1)
		{
			perror("words");
			exit(1);
		}
		while (cur < &buf[len])
		{
			cur = memchr(pre, '\n', buf + len - pre);
			if (cur)
			{
				if (new) write(1,&zero,1);
				write(1,pre, cur - pre);
				new = 1;
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
				if (new) write(1,&zero,1);
				write(1,pre,l);
				new = 0;
			}
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
			default:
				close(fds[1]);
				go(fds[0]);
				wait(NULL);
		}
	}
}
