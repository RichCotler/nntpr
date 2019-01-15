# 
#' set_post_names Function
#'
#' This function stores the name, email box name, and email domain
#' in nntpr.private for use by posttext in constructing the
#' NNTP message headers.
#' Calling posttext before using this to set the names
#' will return an error condition.
#' @param from_name sender name displayed in the NNTP From header
#' @param email_name sender email box name (before the @) for the NNTP From header
#' @param email_domain sender email address domain (after the @) for the NNTP From and Message-ID headers
#' @export
set_post_names <- function(from_name, email_name, email_domain) {
    
    nntpr.private$from.name <- from_name
    nntpr.private$from.email.name <- email_name
    nntpr.private$from.email.domain <- email_domain
    nntpr.private$gretmessage <- NULL
    
    example_from <- str_c("From: ", nntpr.private$from.name, " <", nntpr.private$from.email.name, 
        "@", nntpr.private$from.email.domain, ">")
    return(example_from)
    
}

