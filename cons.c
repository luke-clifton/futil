#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// String -> [String] -> [String]

int main(int argc, char *argv[])
{
	while(*(++argv)) write(1, *argv, strlen(*argv));
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
