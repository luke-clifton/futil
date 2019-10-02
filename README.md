A functional take on some shell utilities.

map :: (String -> IO [String]) -> [String] -> IO [String]

pure :: String -> IO [String]

Each of these utilities can be thought of as a function which takes
one or more string arguments (the command line arguments), a function argument
(trailing command line arguments), and a list of strings (the stdinput).

They each return an IO [String] or a IO ()

For functions which don't take a function argument, we can treat the arguments
as a list or vararg.

Sometimes it can make sense for a function to ignore it's input or output.
Such functions are suffixed with an _

    concatMap :: (String -> IO [String]) -> [String] -> IO [String]
    concatMap_ :: (String -> IO [String]) -> [String] -> IO ()


    cons :: String -> [String] -> IO [String]
    cons_ :: String -> IO [String]

If a function takes a function as an argument, it must have a fixed number
of String arguments before it. We avoid the use of special markers such as
`--` except in very specific circumstances (see lambda function).

    withCurrentDirectory :: String -> ([String] -> IO [String]) -> IO [String]


