#include <stdio.h>
#include <stdlib.h>
#include <futil.h>

void go(ctx *c)
{
	int count = 0;
	int alloced = 20;
	char **items = malloc(alloced * sizeof(char *));
	if (!items)
	{
		perror("cycle");
		exit(1);
	}
	for(;;count++)
	{
		if (count >= alloced - 1)
		{
			alloced += alloced;
			items = realloc(items, alloced * sizeof(char *));
			if (!items)
			{
				perror("cycle");
				exit(1);
			}
		}
		size_t s = 0;
		items[count] = NULL;
		if (!(items[count] = read_item_into(c, &items[count], &s)))
		{
			break;
		}
		write_item(c, items[count]);
	}
	count--;
	fprintf(stderr, "%d -- \n", count);
	for(int i = 0;;)
	{
		write_item(c, items[i]);
		i = i < count ? i + 1 : 0;
	}
}

int main(int argc, char *argv[])
{
	ctx c = (ctx){.in = stdin, .out = stdout, .err = argv[0]};
	go(&c);
}
