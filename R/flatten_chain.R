
format_rhs <- function(call, pipe){
  format_fun <- attr(pipe, "format_fun")
  format_fun(call)
}

# create a flat sequence of calls from a pipe chain
flatten_chain<- function(call, env = parent.frame(2)){
  ind <- numeric(0)
  calls <- list()
  subcall <- call
  # initialise bools
  compound <- NULL
  fs <- FALSE
  repeat {
    # split subcall
    op <- eval(subcall[[1]], env)
    lhs <- subcall[[2]]
    rhs <- subcall[[3]]

    # if compound pipe found, check if well used, add a last call,
    # toggle the compound bool, and the fs bool if relevant, and break
    if(is_compound_pipe(op)) {
      if(length(lhs)==3 && is_pipe(eval(lhs[[1]])))
        stop("A compound pipe should only be used at the start of the chain",
             call. = FALSE)
      calls <- c(format_rhs(rhs, op), calls)
      if(lhs == quote(.)) fs <- TRUE
      compound <- attr(op,"compound_fun")
      break
    }

    # if there are no more pipes, add a last call and break
    if(!is_pipe(op)) {
      lhs <- subcall
      break
    }

    # else add a call
    calls <- c(format_rhs(rhs, op), calls)

    # if the lhs is not properly subsettable, check if it's a dot,
    # add a last call and break
    if(length(lhs) !=3) {
      if(lhs == quote(.)) fs <- TRUE
      break
    }
    # go deeper
    ind <- c(ind,2)
    subcall <- call[[ind]]
  }

  calls <- c(quote(on.exit(rm(.))), calls, quote(.))
  c(lhs = lhs,
    fun = rlang::new_function(alist(.=), rlang::call2("{", !!!calls),env = env),
    compound = compound,
    fs = fs)
}
