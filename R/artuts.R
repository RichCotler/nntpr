#
#' artuts Function
#'
#' This is a convenience function that returns a list
#' with article level information from the nttpr.private private utilities
#' for return message, execution time stats, and article info
#' with starting and ending article numbers and their respective article dates.
#' @export
artuts <- function() {

    retlist <- as.list(publicutility("retmessage"))
    artvector <- unlist(publicutility("artinfo"))
    retlist <- list.append(retlist, c("first article", artvector[1], artvector[2]))
    retlist <- list.append(retlist, c("last article", artvector[3], artvector[4]))
    retlist <- list.append(retlist, publicutility("exectime"))
    retlist <- list.append(retlist, publicutility("listfieldnames"))
    return(retlist)

}

