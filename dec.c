#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

void go(FILE *out)
{
	char buf[BUFSIZ];
	size_t s;
	int esc = 0;
	while ((s = fread(buf, 1, sizeof(buf), stdin)))
	{
		for (int i = 0; i < s; i++)
		{
			char c = buf[i];
			switch (c)
			{
				case 0:
					fprintf(stderr, "dec: invalid format (found NUL byte)\n");
					exit(1);
					break;
				case 1:
					if (esc)
					{
						fputc(0, out);
						esc = 0;
					}
					else
					{
						esc = 1;
					}
					break;
				default:
					if (esc && c == 2)
					{
						fputc(1, out);
						esc = 0;
					}
					else
					{
						fputc(c, out);
					}
					break;
			}
		}
	}
}

int main(int argc, char *argv[])
{
	if (argv[1])
	{
		int fds[2];
		if (pipe(fds))
		{
			perror("enc");
			exit(1);
		}
		int stat;
		switch (fork())
		{
			case -1:
				perror("enc");
				exit(1);
			case 0:
				close(fds[1]);
				dup2(fds[0], 0);
				execvp(argv[1], argv + 1);
				perror("env");
				exit(1);
			default:
				close(fds[0]);
				FILE *h = fdopen(fds[1], "w");
				go(h);
				fclose(h);
				wait(&stat);
				if (WIFEXITED(stat))
				{
					exit(WEXITSTATUS(stat));
				}
				if (WTERMSIG(stat))
				{
					if (0 > raise(WTERMSIG(stat)))
					{
						perror("delay");
						exit(1);
					}
				}
		}
	}
	else
	{
		go(stdout);
	}
}
