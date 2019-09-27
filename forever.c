#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Run the continuation forever, concatonating all the outputs.
//
// IO [String] -> IO [String]

int main(int argc, char *argv[])
{
	for(;;)
	{
		switch (fork())
		{
			case -1:
				perror("forever");
				exit(1);
			case 0:
				execvp(argv[1], &argv[1]);
				break;
			default:
				if (wait(NULL) == -1)
				{
					perror("forever");
					exit(1);
				}
		}
	}
}
