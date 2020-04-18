
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipe

This package proposes an alternative to the pipe from ‘magrittr’. It’s
named the same and passes all the ‘magrittr’ test so can easily be a
drop-in replacement, its main advantages is that it is implemented more
consistently and allows definition of new pipes. It also solves a few
issues of the original pipe.

Install with :

``` r
remotes::install_github("pipe")
```

## What’s different from *magrittr* ?

The main differences are:

  - Pipes are object of class `pipe` and of class either `standard_pipe`
    or `compound_pipe`, none of them is special implementation-wise

  - Every step of the chain is executed in the same environment, as can
    be seen by the use of `%B>%` above

  - The new pipe `%B>%` allows browsing the pipe chain step by step

calling `iris %>% head %B>% dim %>% length` will open place you right at
the browser call below :

``` r
#> Called from: (function (.) 
#> {
#>     on.exit(rm(.))
#>     . <- head(.)
#>     browser()
#>     . <- dim(.)
#>     . <- length(.)
#>     .
#> })(iris)
```

  - The new pipe `%S>%` allows the use of *rlang*’s `!!!` operator to
    splice dots in any function

<!-- end list -->

``` r
library(pipe)
c(a = 1, b = 2) %S>% data.frame()
#>   .
#> a 1
#> b 2
```

  - additional pipes can be defined easily by calling `new_pipe()`,
    which is used internally to create pipes included in the package

  - `:::`, `::` and `$` get a special treatment to solve a common issue

<!-- end list -->

``` r
iris %>% base::dim
#> [1] 150   5
iris %>% base:::dim
#> [1] 150   5
x <- list(y = dim)
iris %>% x$y
#> [1] 150   5
```

  - It fails explicitly in some cases rather than allowing strange
    behavior silently

<!-- end list -->

``` r
iris %>% head %<>% dim
#> Error: A compound pipe should only be used at the start of the chain
. %<>% head
#> Error in . %<>% head: You can't start a functional sequence on a compound operator
```

  - It’s slightly slower than *magrittr* (see *fast\_pipe* if speed is
    an issue\!)

<!-- end list -->

``` r
microbenchmark::microbenchmark(
  magrittr = {`%>%` <- magrittr::`%>%`
  1 %>% identity %>% identity() %>% (identity) %>% {identity(.)}},
  pipe = {`%>%` <- pipe::`%>%`
  1 %>% identity %>% identity() %>% (identity) %>% {identity(.)}},
  times=10000
)
#> Unit: microseconds
#>      expr   min     lq     mean median    uq     max neval cld
#>  magrittr  93.0 118.00 215.5137  152.3 240.9  5113.1 10000  a 
#>      pipe 128.9 157.15 289.4841  204.4 330.4 11374.4 10000   b
```

## Defining new pipes

Pipes are defined by the function `new_pipe()` which accepts arguments
`format_fun` and `compound_fun`.

`format_fun` is the function that changes the quoted rhs of a pipe into
the quoted expression that will give a new value to the dot. For
instance in `iris %>% head()` its input will be `quote(head())` and it’s
output `quote(. <- head(.))`.

`compound_fun` is used to define new types of compound pipes, we won’t
detail it as it’s very specific, but see last example, used to define
`%<>%`.

We enumerate all the pipes of the package below :

``` r
`%>%` <- new_pipe(
  function(call){
  call <- insert_dot(call)
  bquote(. <- .(call))
})

`%T>%`<- new_pipe(
  function(call){
  call <- insert_dot(call)
  bquote(local(.(call)))
})

`%$%`<- new_pipe(
  function(call){
  bquote(. <- with(.,.(call)))
})

`%S>%`<- new_pipe(
  function(call){
    call <- insert_dot(call)
    bquote(. <- eval(rlang::expr(.(call))))
  })


`%B>%` <- new_pipe(
  function(call){
  call <- insert_dot(call)
  c(quote(browser()),
    bquote(. <- .(call)))
})

`%<>%` <- new_pipe(
  function(call){
    call <- insert_dot(call)
    bquote(. <- .(call))
  },
  function(lhs, res){
    substitute(lhs <- res)
  })
```

## A few notes

This is just an experiment, it’s likely to change. I’ve also been
working on the package *fastpipe* which is a closer equivalent to
*magrittr* focused on being more robust and faster.
