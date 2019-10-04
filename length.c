#include <stdio.h>
#include <futil.h>

int main(int argc, char *argv[])
{
	int i;
	ctx c = (ctx){.in = stdin, .out = stdout};
	for(i = 0; read_item(&c); i++);
	char buf[40] = {0};
	printf("%d", i);
	return 0;
}
