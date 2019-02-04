# 
#' group Function
#'
#' This function gets group information and sets the group for nntplib methods operating against articles.
#' Group information vector (response, article count, first article number, last article number, group name)
#' is stored in nntpr.private and is returned to caller.
#'
#' @param groupname usenet group name
#' @export
group <- function(groupname) {
    nntpr.private$ggroupinfovector <- NULL
    
    cmd2run <- str_c("svconn.group('", groupname, "')")
    groupinfo <- call2python(cmd2run)
    if (!is.null(groupinfo)) {
        nntpr.private$ggroupinfovector <- unlist(str_split(groupinfo[1], 
            " "))
    }
    
    return(nntpr.private$ggroupinfovector)
}

