# 
#' getanarticle Function
#'
#' This function retrieves a single article stat, header and body,
#' header, or body (depending on request type parameter)
#' from the group set by the last group() call.
#' The function returns 4 item list with:
#' \itemize{
#' \item request summary with req number, article number, and message id
#' \item article number string
#' \item message id
#' \item vector containing article number/id, header, body, or header and body
#' }
#' or and error message string if applicable.
#'
#' @param article_string article number to retrieve
#' @param request_type the nntplib method to execute (article, head, body)
#' @export
getanarticle <- function(article_string, request_type) {
    retmessage <- NULL
    retlist <- NULL
    
    groupinfo <- publicutility("groupinfo")
    valid_request_types <- c("article", "body", "head", "stat")
    if (!is.element(request_type, valid_request_types)) {
        retmessage <- str_c("Error: ", request_type, " is not a valid request type value.")
    } else if (is.null(groupinfo)) {
        retmessage <- c("Error: Group not set, run group(<group name>) first.")
    }
    
    if (is.null(retmessage)) {
        retlist <- call2python(str_c("svconn.", request_type, "('", article_string, 
            "')"))
        
        if (str_sub(nntpr.private$gretmessage, 1, 5) != "Error") {
            retmessage <- str_c("Returning ", request_type, " for ", groupinfo[[5]], 
                " article ", article_string)
        }
    } else {
        message(retmessage)
    }
    
    
    nntpr.private$gretmessage <- retmessage
    return(retlist)
}

