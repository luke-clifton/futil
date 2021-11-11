#include "futil.h"
#include <ctype.h>
#include <limits.h>

// TODO: This needs to be wide character aware and support unicode.

struct cell_t {
	long  width;
	char * object;
	int  cur;
	char just; // l - left, r - right, c - center
	char *prefix;
	char *postfix;
};

void parse_cell(struct prog_t *prog, struct cell_t * cell, char * fmt)
{
	cell->width = -1;
	cell->object = NULL;
	cell->cur = 0;
	cell->just = 'l';
	cell->prefix = NULL;
	cell->postfix = NULL;

	for (char * c = fmt; *c;)
	{
		switch (*c)
		{
			case 'l':
			case 'r':
			case 'c':
				cell->just = *c++;
				break;
			case 'w':
				cell->width = strtol(c+1,&c,10);
				break;
			default:
				futil_die(prog, "unknown character in table format");
		}
	}
}

int string_width(const char * c)
{
	int width = 0;
	for(;;)
	{
		switch (*c)
		{
			case '\n':
				return width;
			case 0:
				return width;
			case '\t':
				width += 4;
				break;
			default:
				if (isprint(*c))
				{
					width += 1;
				}
		}
		c++;
	}
}

int cell_write_char(struct prog_t * prog, char c, long width)
{
	switch (c)
	{
		case '\n':
			return -1;
		case 0:
			return -2;
		case '\t':
			futil_write(prog, width < 4 ? width : 4, "    ");
			return (width < 4 ? width : 4);
		default:
			if (isprint(c))
			{
				futil_write(prog, 1, &c);
				return 1;
			}
			else
			{
				futil_write(prog, 1, ".");
				return 1;
			}
	}
}

void cell_write_line(struct prog_t * prog, struct cell_t * cell)
{
	long width = cell->width - 1;
	if (cell->object == NULL)
	{
		while (width--)
		{
			futil_write(prog, 1, " ");
		}
		futil_write(prog, strlen("│"), "│");
		return;
	}
	
	if (cell->just == 'r')
	{
		int sw = string_width(&(cell->object[cell->cur]));
		while (sw < width) {
			futil_write(prog, 1, " ");
			width--;
		}
	}
	if (cell->just == 'c')
	{
		int sw = string_width(&(cell->object[cell->cur]));
		int x = (width - sw) / 2;
		while (x > 0) {
			futil_write(prog, 1, " ");
			width--;
			x--;
		}
	}

	long add = 0;
	while ((width > 0) || (string_width(&cell->object[cell->cur]) == 0)) 
	{
		add = cell_write_char(prog, cell->object[cell->cur], width);
		cell->cur += 1;
		if (add == -2)
		{
			free(cell->object);
			cell->object = NULL;
			cell->cur = 0;
			break;
		}
		else if (add == -1)
		{
			if (cell->object[cell->cur] == 0)
			{
				free(cell->object);
				cell->object = NULL;
				cell->cur = 0;
			}
			break;
		}
		else
		{
			width -= add;
		}
	}

	while (width--)
		futil_write(prog, 1, " ");

	futil_write(prog, strlen("│"), "│");
}

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "table",
		.output = stdout
	};

	char * cols = getenv("COLUMNS");
	long width = atoi(cols ? cols : "80");
	width = width ? width : 80;
	width -= 1;

	struct cell_t * cells = calloc(argc, sizeof(*cells));
	long auto_width = 0;
	for (int i = 0; i < argc - 1; i++)
	{
		parse_cell(&prog, &cells[i], argv[i+1]);
		if (cells[i].width > 0)
		{
			if (cells[i].width == 1)
				cells[i].width++;
			width -= cells[i].width;
		}
		else
			auto_width -= cells[i].width;
	}
	if (argc == 1)
	{
		parse_cell(&prog, cells, "");
		auto_width -= cells->width;
		argc = 2;
	}

	long actual_width = 0;
	for(;;)
	{
		int smallest = -1;
		long smallestval = LONG_MAX;
		for (int i = 0; i < argc - 1; i++)
		{
			if ((cells[i].width < 0) && abs(cells[i].width) < smallestval)
			{
				smallestval = abs(cells[i].width);
				smallest = i;
			}
		}
		if (smallest == -1)
		{
			break;
		}
		long aw = cells[smallest].width;
		cells[smallest].width = abs(aw) * width / auto_width;
		if (cells[smallest].width < 2)
		{
			cells[smallest].width = 2;
		}
		width -= cells[smallest].width;
		auto_width += aw;
	}

	int cur = 0;
	char buf[BUFSIZ];
	bool top = true;

	for(;;)
	{
		bool allnull = true;
		for(int i = 0; i < argc - 1; i++)
		{
			cur = futil_slurp_object(&prog, sizeof(buf), buf, cur, 0, &(cells[i].object));
			if (cur < 0)
			{
				free(cells[i].object);
				cells[i].object = NULL;
			}
			if (cells[i].object)
				allnull = false;
		}
		if (allnull && !top) 
		{
			for (int i = 0; i < argc - 1; i++)
			{
				long width = cells[i].width - 1;
				if (!i)
					futil_write(&prog, sizeof("\u2514"), "\u2514");
				while (width--)
					futil_write(&prog, sizeof("\u2500"), "\u2500");
				if (i == argc - 2)
					futil_write(&prog, sizeof("\u2518"), "\u2518");
				else
					futil_write(&prog, sizeof("\u2534"), "\u2534");

			}
			futil_write(&prog, 1, "\n");
			break;
		} else if (!top)
		{
			for (int i = 0; i < argc - 1; i++)
			{
				long width = cells[i].width - 1;
				if (!i)
					futil_write(&prog, sizeof("\u251C"), "\u251C");
				while (width--)
					futil_write(&prog, sizeof("\u2500"), "\u2500");
				if (i == argc - 2)
					futil_write(&prog, sizeof("\u2524"), "\u2524");
				else
					futil_write(&prog, sizeof("\u253C"), "\u253C");

			}
			futil_write(&prog, 1, "\n");
		} else
		{
			for (int i = 0; i < argc - 1; i++)
			{
				long width = cells[i].width - 1;
				if (!i)
					futil_write(&prog, sizeof("\u250C"), "\u250C");
				while (width--)
					futil_write(&prog, sizeof("\u2500"), "\u2500");
				if (i == argc - 2)
					futil_write(&prog, sizeof("\u2510"), "\u2510");
				else
					futil_write(&prog, sizeof("\u252C"), "\u252C");

			}
			futil_write(&prog, 1, "\n");
		}
		top = false;
		for(;;)
		{
			bool allnull = true;
			for(int i = 0; i < argc - 1; i++)
			{
				if (!i)
					futil_write(&prog, strlen("│"), "│");
				cell_write_line(&prog, &cells[i]);
				if (cells[i].object)
					allnull = false;
			}
			futil_write(&prog, 1, "\n");
			if (allnull)
			{
				break;
			}
		}
	}


}
