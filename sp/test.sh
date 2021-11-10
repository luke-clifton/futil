redo
set -e

expect() {
	output="$1"
	shift
	diff <(printf -- "$output" | xxd) <("$@" | xxd)
}

expect ' 1\0 2\0 3\0' list ' 1' ' 2' ' 3'
expect '1\0p\n' cons 1 echo p
expect '1\n\x002\n\0' pipeline list 1 12 '' flatmap wc -c
expect '-\0--\0---\0----\0' take 4 fix cons - format '-%s\0'
expect '1\x00b\x002\x00c\x00' pipeline list 1 2 3 4 '' zip2 list b c
expect '1\x00b\x002\x00c\x00' pipeline list 1 2 '' zip2 list b c d e
expect '1\x00b\x002\x00c\x00' pipeline list 1 2 '' zip2 list b c
expect '1\x002\x003\x00' pipeline list 1 2 3 '' flatmaps a : printf '%s' a
expect '1\x002\x003\x00' pipeline list 1 2 3 '' format '%s\0'
expect '12\x0034\x00' pipeline list 1 2 3 4 '' flatmaps a b : printf '%s' a b
expect '-\0-\0' pipeline fix cons - '' head -c 4
expect '1\n1\n2\n3\n5\n8\n13\n' unlines take 7 fix cons 1 cons 1 calc read peek + p reset
expect '\0' lift true
expect '\n'  lift echo
