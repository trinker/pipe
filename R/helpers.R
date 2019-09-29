#' Insert dot at the relevant place
#'
#' Helper function to use in the `format_fun` argument of `new_pipe()`.
#'
#' Note that *magrittr* behaves as if `special_case` is `FALSE`. We think setting
#' it to `TRUE` is very unlikely to break any code or give an unexpected behavior
#' so we set it by default to `TRUE` and that's the way our default pipes use it.
#'
#' @param expr the quoted rhs to edit
#' @param special_cases wether to change `package::fun` to `package::fun()`,
#' `package:::fun` to `package:::fun()` and `env$fun` to `env$fun()`
#'
#' @value the altered quoted expression
#' @export
insert_dot <- function(expr, special_cases = TRUE) {
  if(is.symbol(expr) || expr[[1]] == quote(`(`)) {
    # if a symbol or an expression inside parentheses, make it a call with dot arg
    expr <- as.call(c(expr, quote(`.`)))
  } else if(length(expr) ==1) {
    # if a call without arg, give it a dot arg
    expr <- as.call(c(expr[[1]], quote(`.`)))
  } else if(special_cases && (
    expr[[1]] == quote(`$`) ||
    expr[[1]] == quote(`::`) ||
    expr[[1]] == quote(`:::`))) {
    # deal with special cases of infix operators
    expr <- as.call(c(expr, quote(`.`)))
  } else if (expr[[1]] != quote(`{`) &&
             all(sapply(expr[-1], `!=`, quote(`.`))) &&
             all(sapply(expr[-1], `!=`, quote(`!!!.`)))) {
    # if a call with args but no dot in arg, insert one first
    expr <- as.call(c(expr[[1]], quote(`.`), as.list(expr[-1])))
  }
  expr
}


#' Check if object is a pipe
#'
#' Functions to check if an object is a pipe or a .
#' @param x An object
#' @export
is_pipe <- function(x) inherits(x, "pipe")


#' @export
#' @inheritParams is_pipe
#' @rdname is_pipe
is_compound_pipe <- function(x) {inherits(x, "compound_pipe")}

#' @export
#' @inheritParams is_pipe
#' @rdname is_pipe
is_standard_pipe <- function(x) {inherits(x, "standard_pipe")}

pipe_op <- function(lhs, rhs) {
  fc <- flatten_chain(match.call())
  if(fc$fs && !is.null(fc$compound))
    stop("You can't start a functional sequence on a compound operator")
  # return functional sequence if relevant
  if(fc$fs) return(fc$fun)

  # compute result
  res <- eval.parent(substitute(FUN(LHS), list(LHS = fc$lhs, FUN = fc$fun)))

  # execute compound function if relevant
  if(!is.null(fc$compound)) {
    call <- eval(substitute(
      FUN(LHS, RES),
      list(FUN = fc$compound, LHS = fc$lhs, RES = res)))
    eval.parent(call)
  } else res
}

