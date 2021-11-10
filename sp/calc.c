#include "futil.h"

struct list_t {
	char * val;
	struct list_t *next;
};

struct list_t * list_push(struct list_t * list, char * s)
{
	struct list_t * new = malloc(sizeof(struct list_t));
	if (!new) return NULL;

	new->next = list;
	new->val = s;
	return new;
}

struct list_t * list_pop(struct prog_t * prog, struct list_t * list, char ** s)
{
	static int cur = 0;
	static char buf[BUFSIZ];

	if (!list)
	{
		cur = futil_slurp_object(prog, sizeof(buf), buf, cur, 0, s);
		if (cur == -1)
			*s = NULL;
		return NULL;
	}

	*s = list->val;
	struct list_t * r = list->next;
	free(list);
	return r;
}

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "calc",
		.output = stdout
	};

	struct list_t * input = NULL;

	char stack[1000][100];
	int top=0;

	char **p = argv;

	while (*++p)
	{
		if (!strcmp(*p, "+"))
		{
			top--; if (top < 0) futil_die(&prog, "not enough values on the stack");
			int a = atoi(stack[top]);
			top--; if (top < 0) futil_die(&prog, "not enough values on the stack");
			int b = atoi(stack[top]);
			snprintf(stack[top], sizeof(stack[top]), "%d", a + b); top++;
		}
		else if (!strcmp(*p, "read") || !strcmp(*p, "."))
		{
			char * s;
			input = list_pop(&prog, input, &s);
			if (! s) futil_die(&prog, "read: not enough input");
			snprintf(stack[top], sizeof(stack[top]), "%s", s); top++;
			free(s);
		}
		else if (!strcmp(*p, "peek"))
		{
			char * s;
			input = list_pop(&prog, input, &s);
			if (! s) futil_die(&prog, "peek: not enough input");
			snprintf(stack[top], sizeof(stack[top]), "%s", s); top++;
			input = list_push(input, s);
			if (!input) futil_die(&prog, "failed to push");
		}
		else if (!strcmp(*p, "reset"))
		{
			top = 0;
			p = argv;
		}
		else if (!strcmp(*p, "p"))
		{
			futil_write(&prog, strlen(stack[top - 1])+1, stack[top-1]);
		}
		else if (!strcmp(*p, "P"))
		{
			futil_write(&prog, strlen(stack[top - 1])+1, stack[top-1]); top--;
		}
		else
		{
			snprintf(stack[top], sizeof(stack[top]), "%s", *p); top++;
		}

	}

	futil_shutdown(&prog);
}


