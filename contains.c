#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
	if (argc != 3)
	{
		fprintf(stderr, "contains requires two arguments");
		exit(2);
	}
	if (strstr(argv[1], argv[2]))
	{
		return 0;
	}
	return 1;
}
