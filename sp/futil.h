#ifndef FUTIL_H
#define FUTIL_H
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

typedef struct pid_fd pid_fd;
typedef int (*app_t)(FILE *, FILE *);

struct prog_t {
	bool changed;
	FILE *output;
	char *name;
	int flush_count;
	int read_count;
	pid_t children[5];
};


void futil_wait(struct prog_t *prog, pid_t pid);
void futil_write(struct prog_t *prog, size_t n, char buf[n]);
void futil_flush(struct prog_t *prog);
void futil_exec(struct prog_t *prog, char *argv[]);
pid_t futil_spawn2(struct prog_t *prog, int fds[2], char *argv[]);
pid_t futil_spawn(struct prog_t *prog, int fds[1], char *argv[]);
void futil_die(struct prog_t *prog, const char *message);
void futil_die_errno(struct prog_t *prog);
ssize_t futil_read(struct prog_t *prog, size_t n, char buf[n], int fd);
void futil_shutdown(struct prog_t *prog);
ssize_t futil_forward_object(struct prog_t *prog, size_t n, char buf[n], ssize_t cur, int fd);
ssize_t futil_forward_object_sized(struct prog_t *prog, size_t n, char buf[n], ssize_t cur, int fd, int *written);

#endif /* FUTIL_H */
