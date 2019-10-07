#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// String -> [String] -> [String]

int main(int argc, char *argv[])
{
	
	ctx c = (ctx){.out = stdout};
	printf("%d%c", argc - 1, 0);
	while(*(++argv)) write_item(&c, *argv);

	char buf[BUFSIZ];
	size_t len;
	while ((len = fread(buf, 1, sizeof(buf), stdin)))
	{
		if (len > fwrite(buf, 1, len, stdout))
		{
			perror("cons");
			exit(1);
		}
	}
	if (ferror(stdin))
	{
		perror("cons");
		exit(1);
	}
}
