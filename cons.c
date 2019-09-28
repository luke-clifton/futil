#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// String -> [String] -> [String]

// We need a String -> IO [String] -> IO [String]

int main(int argc, char *argv[])
{
	write(1, argv[1], strlen(argv[1]) + 1);
	execvp(argv[2], &argv[2]);
}
