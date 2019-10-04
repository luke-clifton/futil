#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <futil.h>

// For each null terminated string read in from stdin, execute
// the provided function passing the string as stdin.
//
// same as 
//
//   map lambda a arg a cmd

int main(int argc, char *argv[])
{
	char *linep = NULL;
	ctx c = (ctx) {
		.in = stdin,
		.out = stdout,
		.err = "mapIO"
		};
	while ((linep = read_item(&c)))
	{
		write_item_proc(&c, &argv[1], linep, strlen(linep));
	}
}
