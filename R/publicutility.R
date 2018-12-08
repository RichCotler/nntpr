#
#' publicutility Function
#'
#' This function provides a public method to
#'  interface with private utility functions.
#' Returns whatever the private function returns.
#'
#' Valid utility to call:
#' \itemize{
#' \item help - return a message with valid utility to call values
#' \item retmessage - return nntpr.private$gretmessage
#' \item artinfo - return 2 item list with first-last article info from last listarticles call
#' \item groupinfo - return vector with Group information vector (response, count, first, last, name)
#' \item exectime - return string with last stored system.time information collected on nntplib xhdr and xover calls
#'}
#' @param utility_to_call the function you want to invoke
#' @export
publicutility <- function(utility_to_call) {
  retvariable <- NULL
  retmessage <- nntpr.private$gretmessage  # store previous contents to return if not changed

  if (is.null(utility_to_call)) {
    retmessage <- c("Error: Utility to call not specified.")
  } else {
    valid_utils <- c("help", "retmessage", "artinfo", "groupinfo", "exectime")
    if (!is.element(utility_to_call, valid_utils)) {
      retmessage <- str_c("Error: ", utility_to_call, " is not a valid utility to call value.")
    } else if (utility_to_call == "retmessage") {
      retvariable <- getretmessage()
    } else if (utility_to_call == "artinfo") {
      retvariable <- getartinfo()
    } else if (utility_to_call == "groupinfo") {
      retvariable <- getgroupinfo()
    } else if (utility_to_call == "exectime") {
      retvariable <- getexectime()
    } else {
      retvariable <- str_c("Valid utility to call values: ", paste(valid_utils,
                                                                   collapse = ", "), ".")  # fall through to help function
    }
  }

  nntpr.private$gretmessage <- retmessage
  return(retvariable)
}
