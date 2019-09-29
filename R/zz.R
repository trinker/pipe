#' Move pipe at the end of the search path()
#'
#' many packages reexport magrittr's pipe, in order to use this package we can
#' use this function to place `pipe()` to the end of the search path so its pipes
#' will mask the others.
#'
#' @return
#' @export
#'
#' @examples
pipe_first <- function(){
  detach("package:pipe")
  library(pipe, warn.conflicts = FALSE, quietly = TRUE )
}
