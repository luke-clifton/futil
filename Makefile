CFLAGS = -Wall -pedantic -lskarnet

SRCS=$(wildcard *.c)

BINS=$(addprefix bin/, $(SRCS:.c=))

all: bin $(BINS)

bin/%: %.c
	$(CC) -o $@ $^ $(CFLAGS)

bin:
	mkdir -p bin

clean:
	rm -Rf bin
