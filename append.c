#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
	size_t l;
	char buf[BUFSIZ];
	int terminated = 1;
	while (!feof(stdin) && (l = fread(buf, 1, sizeof(buf), stdin)))
	{
		fwrite(buf, 1, l, stdout);
		terminated = !buf[l-1];
	}
	if (!terminated) fputc(0, stdout);
	
	fflush(stdout);
	execvp(argv[1], argv+1);
	perror("append");
	exit(1);
}
