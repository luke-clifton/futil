redo-ifchange "$2.c"

gcc -MD -MF "$2.d" -o "$3" -c "$2.c"
read DEPS <"$2.d"
redo-ifchange ${DEPS#*:}

