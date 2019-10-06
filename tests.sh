
assert() {
	diff "$2" "$3" && printf '%-40s passed\n' "$1" || (printf "%-40s failed\n" "$1" && exit 1)
}

assert "nil == ''" <(nil) <(printf '')
assert "list0" <(list) <(true)
assert "list1" <(list 1) <(printf '1\0')
assert "list2" <(list 1 2) <(printf '1\0002\000')
assert "concat0" <(concat list) <(printf '')
assert "concat1" <(concat list 1) <(printf 1)
assert "concat2" <(concat list 1 2) <(printf 12)
assert "concat bad terminate" <(concat printf '1\0002') <(printf 12)
assert "cons is list" <(cons a cons b nil) <(list a b)
assert "map f [] == []" <(list | map echo) <(list)
assert "map printf" <(list 1 2 3 | map printf "%s-") <(list 1- 2- 3-)
assert "length (map f x) = length x" <(list 1 2 3 | map echo | length) <(list 1 2 3 | length)
assert "drop" <(drop 1 cons a nil) <(nil)
assert "drop 2" <(drop 2 list 1 2 3 4) <(list 3 4)
assert "drop 0" <(drop 0 list 1 2 3) <(list 1 2 3)
assert "take" <(take 1 cons a nil) <(cons a nil)
assert "take 2" <(take 2 list 1 2 3) <(list 1 2)
assert "take 0" <(take 0 list 1 2 3) <(nil)
assert "filter" <(list 1 2 3 10 11 12 | filter grep 2) <(list 2 12)
