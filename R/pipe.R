#' Define a new pipe
#'
#' All pipes of the package, including `%>%` and `%<>%`, are defined using this
#' general approach.
#'
#' @param format_fun a function taking an argument `call`, which will be fed the quoted
#'   rhs, and should return the quoted expression of what the pipe should do.
#' @param compound_fun either `NULL` or a function taking arguments `lhs` and `res`
#'   which are respectively the quoted first element of the pipe chain and the result of
#'   the pipe chain, and should return a quoted expression to executre in the
#'   parent environment.
#'
#' This pipe constructir is best understood by examples below and by the use of the
#' `%B>%` pipe.
#'
#' @examples
#' # let's build a standard pipe (it's the code used to create `%>%`)
#' `%>>>%` <- new_pipe(
#'   function(call){
#'     # add explicit dots at the right place in rhs
#'     call <- insert_dot(call)
#'     # the new dot should be equal to the call
#'     bquote(. <- .(call))
#'   })
#' iris %>>>% head() %>>>% dim()
#' # let's build a compound pipe (it's the code used to create `%>%`)
#' `%<>>>%` <- new_pipe(
#'   function(call){
#'     call <- insert_dot(call)
#'     bquote(. <- .(call))
#'   },
#'   function(lhs, res){
#'     substitute(lhs <- res)
#'   })
#' x <- iris
#' x %<>>>% head() %>>>% dim()
#' x
#' @export
new_pipe <- function(format_fun, compound_fun = NULL) {
  if(!is.function(format_fun) || !isTRUE(all.equal(
    formals(format_fun), as.pairlist(alist(call=)))))
    stop("`format_fun` must be a function using a unique argument named `call`")

  if(!is.null(compound_fun) && (!is.function(compound_fun) || !isTRUE(all.equal(
    formals(compound_fun), as.pairlist(alist(lhs=, res=))))))
    stop("`compound_fun` must be NULL or a function using arguments `lhs` and `res`")

  # copy the pipe
  p <- pipe_op

  # assign the relevant class
  class(p) <- c(
    "pipe",
    if(is.null(compound_fun)) "standard_pipe" else "compound_pipe")

  # assign the format_fun
  attr(p, "format_fun") <- format_fun
  attr(p, "compound_fun") <- compound_fun
  p
}
