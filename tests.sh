
assert() {
	diff "$2" "$3" && printf '%-40s passed\n' "$1" || (printf "%-40s failed\n" "$1" && exit 1)
}

assert "nil == ''" <(nil) <(printf '')
assert "array0" <(array) <(true)
assert "array1" <(array 1) <(printf '1\0')
assert "array2" <(array 1 2) <(printf '1\0002\000')
assert "concat0" <(concat array) <(printf '')
assert "concat1" <(concat array 1) <(printf 1)
assert "concat2" <(concat array 1 2) <(printf 12)
assert "concat bad terminate" <(concat printf '1\0002') <(printf 12)
assert "cons is array" <(cons a cons b nil) <(array a b)
assert "map f [] == []" <(array | map echo) <(array)
assert "map printf" <(array 1 2 3 | map printf "%s-") <(array 1- 2- 3-)
assert "length (map f x) = length x" <(array 1 2 3 | map echo | length) <(array 1 2 3 | length)
assert "drop" <(drop 1 cons a nil) <(nil)
assert "drop 2" <(drop 2 array 1 2 3 4) <(array 3 4)
assert "drop 0" <(drop 0 array 1 2 3) <(array 1 2 3)
assert "take" <(take 1 cons a nil) <(cons a nil)
assert "take 2" <(take 2 array 1 2 3) <(array 1 2)
assert "take 0" <(take 0 array 1 2 3) <(nil)
assert "filter" <(array 1 2 3 10 11 12 | filter grep 2) <(array 2 12)
