CFLAGS = -Wall -pedantic -lskarnet

SRCS=$(wildcard *.c)

BINS=$(addprefix bin/, $(SRCS:.c=))

all: bin $(BINS)

bin/%: %.c
	gcc -o $@ $^ $(CFLAGS)

bin:
	mkdir -p bin

clean:
	rm -Rf bin
