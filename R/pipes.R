#' Pipes
#'
#' Equivalents to magrittr's pipes, we also provide a `%S>%` to splice
#' any input and a `%B>%` pipe to debug the full pipe chain from its call.
#'
#' @param lhs A value or a dot (`.`).
#' @param rhs A function call using pipe semantics of the relevant pipe.
#' @name pipes
NULL

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%>%` <- new_pipe(
  function(call){
  call <- insert_dot(call)
  bquote(. <- .(call))
})


# %B>%

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%B>%` <- new_pipe(
  function(call){
  call <- insert_dot(call)
  c(quote(browser()),
    bquote(. <- .(call)))
})

# %<>%

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%<>%` <- new_pipe(
  function(call){
    call <- insert_dot(call)
    bquote(. <- .(call))
  },
  function(lhs, res){
    substitute(lhs <- res)
  })

# %T>%

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%T>%`<- new_pipe(
  function(call){
  call <- insert_dot(call)
  bquote(local(.(call)))
})

# %$%

#' @export
#' @rdname pipes
#' @inheritParams pipes
`%$%`<- new_pipe(
  function(call){
  bquote(. <- with(.,.(call)))
})

#' @export
#' @rdname pipes
#' @inheritParams  pipes
`%S>%`<- new_pipe(
  function(call){
    call <- insert_dot(call)
    bquote(. <- eval(rlang::expr(.(call))))
  })
