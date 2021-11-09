#include "futil.h"

int main(int argc, char *argv[])
{
	struct prog_t prog = (struct prog_t){
		.name = "flatmap",
		.output = stdout
	};
	int fds[2];
	ssize_t cur;
	char buf[BUFSIZ];
	char buf2[BUFSIZ];
	while ((cur > 0) || ((cur == 0) && (cur = futil_read(&prog, sizeof(buf), buf, 0))))
	{
		bool terminated = true;
		pid_t pid = futil_spawn2(&prog, fds, &argv[1]);
		while ((cur > 0) || ((cur == 0) && (cur = futil_read(&prog, sizeof(buf), buf, 0))))
		{
			fd_set fdsr, fdsw;
			FD_ZERO(&fdsr);
			FD_SET(fds[0], &fdsr);
			FD_ZERO(&fdsw);
			FD_SET(fds[1], &fdsw);
			int ready = select((fds[0] > fds[1] ? fds[0] : fds[1]) + 1, &fdsr, &fdsw, NULL, NULL);
			if (FD_ISSET(fds[0], &fdsr))
			{
				ssize_t r = futil_read(&prog, sizeof(buf2), buf2, fds[0]);
				futil_write(&prog, r, buf2);
				terminated = buf2[r-1] == 0;
			}
			if (FD_ISSET(fds[1], &fdsw))
			{
				char *nil = memchr(buf, 0, cur);
				int nlen = nil ? nil - buf : cur;
				if (-1 == write(fds[1], buf, nlen))
				{
					futil_die_errno(&prog);
				}
				if (nil)
				{
					memmove(buf, nil + 1, sizeof(buf) - (nil - buf) - 1);
					cur = cur - nlen - 1;
					break;
				}
			}
		}
		ssize_t r;
		close(fds[1]);
		while ((r = futil_read(&prog, sizeof(buf2), buf2, fds[0])))
		{
			futil_write(&prog, r, buf2);
			terminated = buf2[r-1] == 0;
		}
		if (!terminated)
		{
			futil_write(&prog, 1, "\0");
		}
		close(fds[0]);
		waitpid(pid, NULL, 0);
	}
	futil_shutdown(&prog);
}

