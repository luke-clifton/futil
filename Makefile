CFLAGS = -Wall -pedantic -lskarnet -lfutil -lm

SRCS=$(wildcard *.c)
LIBS_SRCS=$(wildcard lib/*.c)

BINS=$(addprefix bin/, $(SRCS:.c=))
LIBS=$(LIBS_SRCS:.c=.a)

all: $(BINS)

$(LIBS): lib/%.a: lib/%.c
	gcc -Wall -pedantic -Iheaders -c -o $@ $<

$(BINS): bin/%: %.c $(LIBS) headers/*.h | bin
	$(CC) -Iheaders -Llib -o $@ $< $(CFLAGS)

bin:
	mkdir -p bin

clean:
	rm -Rf bin $(LIBS)

