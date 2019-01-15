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
    py$svconn <- nntpr.private$gsvconn
    
    py_run_string(str_c("groupinfo = svconn.group('", groupname, "')"))
    nntpr.private$ggroupinfovector <- unlist(str_split(py$groupinfo[1], " "))
    return(nntpr.private$ggroupinfovector)
    
}
