redo-ifchange "$2.c"
redo-ifchange "futil.o"

gcc -Wall -MD -MF "$2.d" -o "$3" "$2.c" futil.o
read DEPS <"$2.d"
redo-ifchange ${DEPS#*:}
