redo
set -e

expect() {
	output="$1"
	shift
	if ! diff <(printf -- "$output") <("$@")
	then
		printf 'expected: %s\ngot     :\n' "$output"
		"$@" | xxd
		false
	fi
}

expect ' 1\0 2\0 3\0' list ' 1' ' 2' ' 3'
expect '1\0p\n' cons 1 echo p
expect '1\n\x002\n\0' pipeline list 1 12 '' flatmap wc -c
expect '-\0--\0---\0----\0' take 4 fix cons - format '-%s\0'
