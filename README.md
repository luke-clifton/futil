A functional take on some shell utilities.

Arrays are '\0' separated (not terminated!) strings.

(this means no empty lists though... hmmm)

The issue was originally implementing concatMap seemed easier if we didn't have
to worry about terminating nul which would translate into a double nul if we
had to assume missing terminators. I think going back to null terminated
seems easiest, but it will mean we have to double write things in concatMap.
