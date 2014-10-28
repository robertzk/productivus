productivus
===========

A suite of productivity tools for R -- debugging, visualization, inline editing, etc.

Debug interactively
-----------

Imagine you wrote something like

```R
very_long_expression <- lapply(some_long_list, some_long_function)
```

You can bring up that expression in your favorite text editor and execute it from
R by using the `ed` function, which matches a regular expression backwards through
your R history to find the find instance:

```R
ed("sion <-") # Matches "very_long_expression <- " and will bring up the
 # above code to edit in your favorite editor and execute after editing.
```

Ruby-like block-accepting functions
--------

Ruby has the notion of passing a "block" to a function: just a bunch of
code you wish you could be run within the function, but it should be up to
the function to decide how. You can now do this in R.

```R
plus_with_yield <- with_block(function(x, y) { x + y + yield() })
stopifnot(plus_with_yield(1, 2, { 3 + 4 }) == 10)
```


