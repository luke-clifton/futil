#include <stdio.h>
#include <futil.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "futil.h"

/* Write an item out. This writes an entire item out at a time,
 * and is thus unsuitable for streaming
 */
int write_item(ctx *c, char *item)
{
	if (c->tail) fputc(0, c->out);
	c->tail = 1;
	if (fputs(item, c->out) > 0)
	{
		return 1;
	}
	else
	{
		perror("write_item");
		exit(1);
	}
}

/* Read an item. This reads the whole item into memory. Not suitable for
 * streaming.
 */
char *read_item(ctx *c)
{
	int len = getdelim(&(c->linep), &(c->linesize), 0, c->in);
	if (len >= 0)
	{
		return c->linep;
	}
	if (ferror(c->in))
	{
		perror(c->err);
		exit(1);
	}
	return NULL;
}


/* Stream an item from a sub-process. */
void write_item_proc(ctx *c, char **args)
{
	int stat;
	if (c->tail) fputc(0, c->out);
	c->tail = 1;
	fflush(c->out);
	switch (fork())
	{
		case -1:
			perror(c->err);
			exit(1);
		case 0:
			dup2(fileno(c->out), 1);
			dup2(open("/dev/null", O_RDONLY), 0);
			execvp(*args, args);
			perror("main.execvp");
			exit(1);
		default:
			if (wait(&stat) == -1)
			{
				perror(c->err);
				exit(1);
			}
	}
}

