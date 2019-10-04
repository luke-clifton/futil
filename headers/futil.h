#ifndef FUTIL_H
#define FUTIL_H

typedef struct ctx
{
	FILE *in;
	FILE *out;
	int tail;
	char *err;
	char *linep;
	size_t linesize;
	int okToEnd;
} ctx;

typedef struct item
{
	char *bytes;
	size_t size;
} item;

int write_item(ctx *c, char *item);
char *read_item(ctx *c);
char *read_item_into(ctx *c, char **linep, size_t *s);
void write_item_proc(ctx *c, char **args, char *input, int len);

#endif /* FUTIL_H */
