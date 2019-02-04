# 
#' listgroups Function
#'
#' This function retrieves a list of groups (with an optional search wildcard string)
#' and returns a group list to the caller.
#'
#' @param wildcard optional usenet group name search wildcard
#' @export
listgroups <- function(wildcard = NULL) {
    retmessage <- NULL
    retgrplist <- NULL
    
    if (!is.null(wildcard)) {
        # get sorted filtered or unfiltered group tuples
        fullcmd <- str_c("sorted(svconn.descriptions('", wildcard, "')[1])")
    } else {
        fullcmd <- str_c("sorted(svconn.list()[1])")
    }
    
    nntpr.private$gexectime <- system.time(wildlist <- call2python(fullcmd))
    
    if (!is.null(wildlist)) {
        retgrplist <- unique(list.flatten(wildlist))
        retmessage <- str_c("Call returned ", as.character(length(retgrplist)), 
            " entries in ", getclocktime(), " seconds.")
    }
    
    nntpr.private$gretmessage <- retmessage
    return(retgrplist)
}

