#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "format",
		.output = stdout
	};

	int input = 0;
	pid_t pid = 0;

	if (argc < 1)
	{
		futil_die(&prog, "need a format argument");
	}

	if (argc > 2)
	{
		pid = futil_spawn(&prog, &input, &argv[2]);
	}
	
	ssize_t r;
	char buf[BUFSIZ];
	int cur = 0;
	int lengthspec = -1;
	char *end;
	int sized;
	for (;;)
	{
		char *ix = argv[1];
		while (*ix)
		{
			switch (*ix)
			{
				case '%':
					ix++;
					switch (*ix)
					{
						case 0:
							futil_die(&prog, "expected another character after the '%'");
							break;
						case '%':
							futil_write(&prog, 1, ix);
							ix++;
							break;
						case '0':
						case '1':
						case '2':
						case '3':
						case '4':
						case '5':
						case '6':
						case '7':
						case '8':
						case '9':
							lengthspec = strtol(ix, &end, 10);
							ix = end;
						case 's':
							if (cur < 0)
							{
								goto nomore;
							}
							cur = futil_forward_object_sized(&prog, sizeof(buf), buf, cur, input, &sized);
							if (cur == -2)
							{
								goto nomore;
							}
							while (lengthspec > sized)
							{
								futil_write(&prog, 1, " ");
								sized++;
							}
							lengthspec = 0;
							ix++;
							break;
						default:
							futil_die(&prog, "unknown format directive");
					}
					break;
				case '\\':
					ix++;
					switch (*ix)
					{
						case 0:
							futil_die(&prog, "expected another character after the '\\'");
							break;
						case '0':
							futil_write(&prog, 1, "\0");
							ix++;
							break;
						case 't':
							futil_write(&prog, 1, "\t");
							ix++;
							break;
						case 'n':
							futil_write(&prog, 1, "\n");
							ix++;
							break;
						default:
							futil_die(&prog, "unknown escape sequence");
					}
					break;
				default:
					futil_write(&prog, 1, ix);
					ix++;
			}
		}
	}

nomore:
	if (input) close(input);
	futil_shutdown(&prog);
}


