# 
#' listgroups Function
#'
#' This function retrieves a list of groups (with an optional search wildcard string)
#' and returns a group list to the caller.
#'
#' @param wildcard optional usenet group name search wildcard
#' @export
listgroups <- function(wildcard = NULL) {
    py$svconn <- nntpr.private$gsvconn
    retmessage <- NULL
    retgrplist <- NULL
    
    if (!is.null(wildcard)) {
        # get sorted filtered or unfiltered group tuples
        fullcmd <- str_c("wildlist = sorted(svconn.descriptions('", wildcard, "')[1])")
    } else {
        fullcmd <- str_c("wildlist = sorted(svconn.list()[1])")
    }
    nntpr.private$gexectime <- system.time(grplistresult <- py_run_string(fullcmd))
    retgrplist <- unique(list.flatten(py$wildlist))
    retmessage <- str_c("Call returned ", as.character(length(retgrplist)), " entries in ", 
        getclocktime(), " seconds.")
    
    nntpr.private$gretmessage <- retmessage
    return(retgrplist)
}
