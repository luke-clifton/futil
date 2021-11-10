#include <stdbool.h>
#include <sys/select.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "futil.h"
#include <errno.h>
#include <sys/wait.h>
#include <ctype.h>

// A futil program should
//  a) flush it's buffers if reading from input would ever block (for N seconds);
//  b) flush it's buffers if the chunk read from the input didn't result in any further
//     writes.
//  c) otherwise buffer up output for efficient processing.
//
//  d) die the same way it's child did.
//  e) die immediately if a child dies with a non-0 exit code or was killed by a signal

struct prog_t *main_prog = NULL;

void handler(int sig)
{
	pid_t *p = main_prog->children;
	while (p < (main_prog->children + sizeof(main_prog->children)/sizeof(main_prog->children[0])))
	{
		if (*p)
		{
			kill(*p,sig);
		}
		p++;
	}
}

void register_handler(struct prog_t *prog, bool on)
{
	if (on && main_prog) return;
	main_prog = prog;

	struct sigaction sa = (struct sigaction) {
		.sa_handler = on ? handler : SIG_DFL,
	};
	// TODO: Determine a better set of signals
	sigaction(SIGTERM, &sa, NULL);
	sigaction(SIGUSR1, &sa, NULL);
	sigaction(SIGUSR2, &sa, NULL);
	sigaction(SIGBUS, &sa, NULL);
	sigaction(SIGALRM, &sa, NULL);
	sigaction(SIGHUP, &sa, NULL);
	sigaction(SIGINT, &sa, NULL);
	sigaction(SIGWINCH, &sa, NULL);
}

void futil_write(struct prog_t *prog, size_t n, char buf[n])
{
	prog->changed = true;
	if (n != fwrite(buf, 1, n, prog->output))
	{
		futil_die_errno(prog);
	}
}

void futil_flush(struct prog_t *prog)
{
	prog->flush_count++;
	prog->changed = false;
	if (0 != fflush(prog->output))
	{
		futil_die_errno(prog);
	}
}

ssize_t futil_read(struct prog_t *prog, size_t n, char buf[n], int fd)
{
	fd_set fds;
	FD_ZERO(&fds);
	FD_SET(fd, &fds);
	prog->read_count++;

	suseconds_t us = 0;
	if (getenv("FUTIL_READTIME"))
	{
		us = atoi(getenv("FUTIL_READTIME"));
	}

	struct timeval timeout = (struct timeval){.tv_sec = 0, .tv_usec = us};

	int ready = select(fd + 1, &fds, NULL, NULL, &timeout);

	if (0 == ready) {
		futil_flush(prog);
	}

	ssize_t r;
readagain:
	r = read(fd, buf, n);
	if (-1 == r)
	{
		if (errno == EINTR) goto readagain;
		futil_die_errno(prog);
	}
	return r;
}

void futil_exec(struct prog_t *prog, char *argv[])
{
	futil_shutdown(prog);
	execvp(argv[0], argv);
	futil_die_errno(prog);
}

pid_t futil_spawn2(struct prog_t *prog, int fds[2], char *argv[])
{
	register_handler(prog, true);
	int childin[2];
	int childout[2];

	if (-1 == pipe(childin))
	{
		futil_die_errno(prog);
	}

	if (-1 == pipe(childout))
	{
		futil_die_errno(prog);
	}

	sigset_t ss;
	sigset_t restore;
	if (-1 == sigfillset(&ss))
	{
		futil_die_errno(prog);
	}
	if (-1 == sigprocmask(SIG_SETMASK, &ss, &restore))
	{
		futil_die_errno(prog);
	}
	pid_t pid = fork();
	
	if (-1 == pid)
	{
		futil_die_errno(prog);
	}

	if (0 == pid)
	{
		// In child
		if (-1 == sigprocmask(SIG_SETMASK, &restore, NULL))
		{
			futil_die_errno(prog);
		}
		close(childin[1]);
		close(childout[0]);
		dup2(childin[0], 0);
		dup2(childout[1], 1);
		execvp(argv[0], argv);
		futil_die_errno(prog);
	}
	pid_t *save = prog->children;
	while ((save < (prog->children + sizeof(prog->children)/sizeof(prog->children[0]))) && *save) save++;
	if (save == (prog->children + sizeof(prog->children)/sizeof(prog->children[0])))
		futil_die(prog, "too many children");
	*save = pid;

	if (-1 == sigprocmask(SIG_SETMASK, &restore, NULL))
	{
		futil_die_errno(prog);
	}

	close(childin[0]);
	close(childout[1]);
	fds[0] = childout[0];
	fds[1] = childin[1];
	return pid;
}

pid_t futil_spawn(struct prog_t *prog, int fds[1], char *argv[])
{
	register_handler(prog, true);
	int childout[2];

	if (-1 == pipe(childout))
	{
		futil_die_errno(prog);
	}

	sigset_t ss;
	sigset_t restore;
	if (-1 == sigfillset(&ss))
	{
		futil_die_errno(prog);
	}
	if (-1 == sigprocmask(SIG_SETMASK, &ss, &restore))
	{
		futil_die_errno(prog);
	}

	pid_t pid = fork();
	
	if (-1 == pid)
	{
		futil_die_errno(prog);
	}

	if (0 == pid)
	{
		// In child
		if (-1 == sigprocmask(SIG_SETMASK, &restore, NULL))
		{
			futil_die_errno(prog);
		}
		close(childout[0]);
		dup2(childout[1], 1);
		execvp(argv[0], argv);
		futil_die_errno(prog);
	}
	pid_t *save = prog->children;
	while ((save < (prog->children + sizeof(prog->children)/sizeof(prog->children[0]))) && *save) save++;
	if (save == (prog->children + sizeof(prog->children)/sizeof(prog->children[0])))
		futil_die(prog, "too many children");
	*save = pid;

	if (-1 == sigprocmask(SIG_SETMASK, &restore, NULL))
	{
		futil_die_errno(prog);
	}

	close(childout[1]);
	fds[0] = childout[0];
	return pid;
}

void futil_die_errno(struct prog_t *prog)
{
	futil_die(prog, strerror(errno));
}

void futil_die(struct prog_t *prog, const char *message)
{
	fprintf(stderr, "%s: %s\n", prog->name, message);
	futil_shutdown(prog);
	exit(111);
}

ssize_t futil_forward_object(struct prog_t *prog, size_t n, char buf[n], ssize_t cur, int fd)
{
	return futil_forward_object_sized(prog, n, buf, cur, fd, NULL);
}

ssize_t futil_forward_object_sized(struct prog_t *prog, size_t n, char buf[n], ssize_t cur, int fd, int *written)
{
	bool sent = false;
	if (written) *written = 0;
	do
	{
		char *nil = memchr(buf, 0, cur);
		if (nil)
		{
			if (written) (*written) += nil - buf;
			futil_write(prog, nil - buf, buf);
			memmove(buf, nil + 1, n - (nil - buf) - 1);
			return (cur - (nil - buf) - 1);
		}
		else
		{
			if (written) (*written) += cur;
			futil_write(prog, cur, buf);
			sent = cur != 0;
		}
	}
	while ((cur = futil_read(prog, n, buf, fd)));
	if (sent)
	{
		return -1;
	}
	else
	{
		return -2;
	}
}

void futil_wait(struct prog_t *prog, pid_t pid)
{
	int status;
	waitpid(pid, &status, 0);
	pid_t *p = prog->children;
	while ((p < (prog->children + sizeof(prog->children)/sizeof(prog->children[0]))) && (*p != pid)) p++;
	*p = 0;
	if (WIFEXITED(status))
	{
		if (WEXITSTATUS(status))
		{
			futil_shutdown(prog);
			exit(WEXITSTATUS(status));
		}
	}
	if (WIFSIGNALED(status))
	{
		futil_shutdown(prog);
		register_handler(prog, false);
		kill(getpid(), WTERMSIG(status));
		sleep(60);
		futil_die(prog, "Expected to be killed by a signal");
	}
}

void futil_shutdown(struct prog_t *prog)
{
	futil_flush(prog);
	pid_t *p = prog->children;
	while (p < (prog->children + sizeof(prog->children) / sizeof(prog->children[0])))
	{
		if (*p) futil_wait(prog, *p);
		p++;
	}
	if (getenv("FUTIL_DEBUG")) {
		fprintf(stderr, "%s: DEBUG: flush count %d\n", prog->name, prog->flush_count);
		fprintf(stderr, "%s: DEBUG: read count %d\n", prog->name, prog->read_count);
	}
}

void debug_buf(size_t n, char buf[n])
{
	for (int i = 0; i < n; i++)
	{
		if (isprint(buf[i]))
			fprintf(stderr, "%c", buf[i]);
		else
			fprintf(stderr, ".");
	}
	fprintf(stderr, "---\n");
}

int futil_slurp_object(struct prog_t *prog, size_t n, char buf[n], int cur, int fd, char **out)
{
	if (! prog->arg_max)
	{
		prog->arg_max = sysconf(_SC_ARG_MAX);
	}

	*out = calloc(prog->arg_max, 1);
	if (! *out)
		futil_die(prog, "out of memory");

	char *curfil = *out;
	char *end    = &(*out)[prog->arg_max];
	for(;;)
	{
		if (cur == 0)
		{
			cur = futil_read(prog, n, buf, fd);
			if (cur == 0)
			{
				return (-1);
			}
		}

		char *nil = memchr(buf, 0, cur);
		int copysize = cur;
		if (nil)
		{
			copysize = nil - buf + 1;
		}
		if ((curfil + copysize) >= end)
		{
			futil_die(prog, "object too large to slurp");
		}
		memcpy(curfil, buf, copysize);
		if (nil)
		{
			memmove(buf, buf + copysize, cur - copysize);
			return (cur - copysize);
		}
		cur = 0;
		curfil += copysize;
	}
}
