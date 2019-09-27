CFLAGS = -Wall -pedantic

SRCS=$(wildcard *.c)

BINS=$(SRCS:.c=)
%: %.c
	gcc -o $@ $^ $(CFLAGS)


all: $(BINS)

clean:
	rm -- $(BINS)
