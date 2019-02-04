#
#' listartheader Function
#'
#' This function gets a list of article headers from newsgroup set by last group call.
#' If an error is encountered, the error message is stored in nntpr.private, and NULL is returned
#' to the caller, otherwise the dates of the first article and last article
#' in the request are stored in nntpr.private and the article list returned to the caller.
#'
#' @param first_article_string article number (string)
#' @param last_article_string number (string)
#' @param filter_term_string string (optional, case insensitive)
#' @export
listartheader <- function(first_article_string, last_article_string, filter_term_string = NULL) {
    retmessage <- NULL
    retlist <- NULL
    first_art_info <- NULL
    last_art_info <- NULL

    retmessage <- validate_list_call(first_article_string, last_article_string)

    if (is.null(retmessage)) {
        # if no simple error, continue
        adjvector <- adjust_article_range(first_article_string, last_article_string)
        first_article_string <- adjvector[[1]]
        last_article_string <- adjvector[[2]]
        last_art_info <- unlist(execute_listhdr_call("xhdr", "date", str_c(last_article_string,
            "-", last_article_string)))
        first_art_info <- unlist(execute_listhdr_call("xhdr", "date", str_c(first_article_string,
            "-", first_article_string)))

        nntpr.private$glastartinfo <- last_art_info
        nntpr.private$gfirstartinfo <- first_art_info

        retlist <- execute_listhdr_call("xover", first_article_string,
            last_article_string, filter_term_string)
    }

    if (!is.null(retmessage)) {
        if (str_sub(retmessage, 1, 5) != "Error") {
            retmessage <- str_c("Call returned ", as.character(length(retlist)),
                " articles in ", getclocktime(), " seconds.")
        }
    }

    nntpr.private$gretmessage <- retmessage
    return(retlist)
}
