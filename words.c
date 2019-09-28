#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
	char buf[BUFSIZ];
	ssize_t len;
	char *cur = buf;
	char zero = 0;
	while ((len = read(0, buf, sizeof(buf))))
	{
		if (len == -1)
		{
			perror("words");
			exit(1);
		}
		for (ssize_t i = 0; i < len; i++)
		{
			if (cur && isspace(buf[i]))
			{
				write(1,cur, &buf[i] - cur);
				write(1,&zero, 1);
				cur = NULL;
				continue;
			}
			if (!cur && !isspace(buf[i]))
			{
				cur = &buf[i];
				continue;
			}
		}
		if (cur)
		{
			write(1,cur,&buf[sizeof(buf)] - cur);
			cur = buf;
		}
	}
}
