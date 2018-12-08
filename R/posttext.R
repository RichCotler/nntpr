#
#' posttext Function
#'
#' This function posts a text message
#' with whatever header information you have stored
#' in nntpr.private, and the list of groups, the subject,
#' and text body passed in function parameters
#' @param group_list values for the Newsgroups header
#' @param post_subject value for the Subject header
#' @param message_body_vector vector containing lines of message text to post
#' @export
posttext <- function(group_list, post_subject, message_body_vector) {
  py$svconn <- nntpr.private$gsvconn
  retmessage <- NULL

  if (is.null(nntpr.private$from.name)) {
    retmessage <- c("Error: set_post_name must be run before posttext, post unsucessful")
  } else {
    message_id <- UUIDgenerate()

    vector2post <- set_from_header()
    vector2post[[2]] <- str_c("Newsgroups: ", paste(unlist(group_list), collapse = ", "))
    vector2post[[3]] <- c("Content-Type: text/plain")
    vector2post[[4]] <- str_c("Subject: ", post_subject)
    vector2post[[5]] <- set_messageid_header(message_id)
    vector2post <- append(vector2post, message_body_vector, length(vector2post))
    vector2post <- append(vector2post, nntpr.private$gposteof, length(vector2post))

    lineswritten <- write_temptext(message_id, vector2post)

    tryresult <- try(py_run_string(str_c("postfile = open('", set_temptext_filename(message_id),
                                         "', 'r')")))
    if (str_sub(tryresult, 1, 5) != "Error") {
      tryresult <- try(py_eval("svconn.post(postfile)"))
      retmessage <- tryresult
      if (str_sub(tryresult, 1, 5) != "Error") {
        tmpbool <- delete_temptext(message_id)
        retmessage <- str_c(retmessage, " - posted a ", as.character(lineswritten),
                            " line text file to message id <", message_id, ">.")
      }
    }
  }

  nntpr.private$gretmessage <- retmessage
  return(retmessage)
}
