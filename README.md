# EXPERIMENTAL!

A functional take on some shell utilities.

Values are any string of bytes that can be used as a command argument (that
is, any string of bytes not containing a `\0` character.

We use `\0` for terminating items in lists when reading from file descriptors.
We thus get lazy infinite lists. Compatible with the output of `find -print0`
and with the input of `xargs -0`.


## The "types" of values.

`String`: any non '\0' containing sequence of bytes. Can appear as arguments
          or on stdin.

`List`: A (potentially infinite) list of `String` types. Represented as
        consecutive '\0' terminated `String` elements.

Some commands assume extra structure on the various inputs, for example,
some assume that a `String` looks like a number, or operate on elements
of a list two at a time (mapTuple).

Unfortunately, we don't have a type checker, so it is up to you to to ensure
the types match up. In some instances we can't even do run-time type checking
due to the limited nature of our encoding scheme (`\0` terminated).

