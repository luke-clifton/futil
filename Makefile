CFLAGS = -Wall

SRC=$(wildcard *.c)

all: $(SRC)
	gcc -o $@ $^ $(CFLAGS)
