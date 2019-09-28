#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
	size_t len;
	char buf[BUFSIZ];
	while ((len = read(0, buf, sizeof(buf))))
	{
		if (len == -1)
		{
			perror("cons");
			exit(1);
		}
		if (write(1, buf, len) == -1)
		{
			perror("cons");
			exit(1);
		}
	}
	return 0;
}

