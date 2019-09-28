#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Split stdin into an array of lines.
int main(int argc, char *argv[])
{
	char buf[BUFSIZ];
	ssize_t len;
	char *pre = buf;
	char *cur = buf;
	char zero = 0;
	while ((len = read(0, buf, sizeof(buf))))
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
				write(1,pre, cur - pre);
				write(1,&zero,1);
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
		}
	}
	return 0;
}
