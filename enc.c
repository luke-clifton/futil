#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

void go(FILE *in)
{
	char buf[BUFSIZ];
	size_t s;
	while ((s = fread(buf, 1, sizeof(buf), in)))
	{
		for (int i = 0; i < s; i++)
		{
			char c = buf[i];
			switch (c)
			{
				case 0:
					fputs("\1\1",stdout);
					break;
				case 1:
					fputs("\1\2", stdout);
					break;
				default:
					fputc(c, stdout);
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
		switch (fork())
		{
			case -1:
				perror("enc");
				exit(1);
			case 0:
				close(fds[0]);
				dup2(fds[1], 1);
				execvp(argv[1], argv + 1);
				perror("env");
				exit(1);
			default:
				close(fds[1]);
				FILE *h = fdopen(fds[0], "r");
				go(h);
				wait(NULL);
		}
	}
	else
	{
		go(stdin);
	}
}
