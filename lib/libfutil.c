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
	if (fputs(item, c->out) >= 0)
	{
		fputc(0, c->out);
		return 1;
	}
	else
	{
		perror("write_item");
		exit(1);
	}
}


char *read_item_into(ctx *c, char **linep, size_t *s)
{
	int len = getdelim(linep, s, 0, c->in);
	if (len >= 0)
	{
		return *linep;
	}
	if (ferror(c->in))
	{
		perror(c->err);
		exit(1);
	}
	return NULL;
}

/* Read an item. This reads the whole item into memory. Not suitable for
 * streaming. Uses an internal buffer in the ctx argument. Use read_item_into
 * if you need to have read multiple items at once.
 */
char *read_item(ctx *c)
{
	return read_item_into(c, &(c->linep), &(c->linesize));
}


/* Stream an item from a sub-process. */
void write_item_proc(ctx *c, char **args, char *input, int len)
{
	int stat;
	fflush(c->out);
	int fds[2];
	if (input && pipe(fds))
	{
		perror(c->err);
		exit(1);
	}

	if (!input)
	{
		fds[0] = open("/dev/null", O_RDONLY);
	}

	switch (fork())
	{
		case -1:
			perror(c->err);
			exit(1);
		case 0:
			if (input) close(fds[1]);
			dup2(fileno(c->out), 1);
			dup2(fds[0]        , 0);
			execvp(*args, args);
			perror("main.execvp");
			exit(1);
		default:
			close(fds[0]);
			if (input)
			{
				write(fds[1], input, len);
				close(fds[1]);
			}
			if (wait(&stat) == -1)
			{
				perror(c->err);
				exit(1);
			}
			fputc(0, c->out);
	}
}

char **read_array(ctx *c, int *len)
{
	char *length_str = read_item(c);
	if (!length_str) return NULL;
	int length = atoi(length_str);
	char **items = calloc(length+1, sizeof(*items));
	memset(items, 0, (length + 1) * sizeof(*items));
	for(int i = 0; i < length; i++)
	{
		size_t s = 0;
		if (!read_item_into(c, &(items[i]), &s))
		{
			for (int j = 0; j < i; j++)
			{
				free(items[j]);
			}
			free(items);
			return NULL;
		}
	}
	if (len) *len = length;
	return items;
}

void free_array(char **items)
{
	char **i = items;
	while (*i)
	{
		free(*i);
	}
	free(items);
}
