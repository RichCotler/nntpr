#
#' nntpr.quit Function
#'
#' This function disconnects the NNTP session associated with a connection object.
#' The 'goodby' message or a warning that there's nothing to quit from is stored
#' in nntpr.private and is returned to caller.
#'
#' @export
nntpr.quit <- function() {
  retmessage <- NULL

  if (!is.null(nntpr.private$gsvconn)) {
    py$svconn <- nntpr.private$gsvconn
    retmessage <- py_eval("svconn.quit()")
  } else {
    retmessage <- c("nothing to quit from")
  }

  nntpr.private$gretmessage <- retmessage
  return(retmessage)
}
