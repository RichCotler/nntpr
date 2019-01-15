# 
#' connect Function
#'
#' This function creates an NNTP connection object.
#' An NNTP connection object is and either the welcome message or an error message
#' is stored in nntpr.private.
#'
#' @param nntp_server dsn name or ip address of target NNTP server
#' @param nntp_port connect port of NNTP server
#' @param user_name user login name
#' @param user_password user login password
#' @param read_only sets read only session True or False
#'
#' @export
connect <- function(nntp_server, nntp_port, user_name, user_password, read_only) {
    retmessage <- NULL
    svconn <- NULL
    
    tryresult <- try(py_run_string(str_c("svconn = r.nntplib.NNTP('", nntp_server, 
        "', '", nntp_port, "', '", user_name, "', '", user_password, "', ", read_only, 
        ")")), FALSE)
    
    if (str_sub(tryresult, 1, 5) != "Error") {
        py_run_string("welcmsg = svconn.getwelcome()")
        svconn <- py$svconn
        retmessage <- py$welcmsg
    } else {
        retmessage <- tryresult
    }
    
    nntpr.private$gsvconn <- svconn
    nntpr.private$gretmessage <- retmessage
    
    return(retmessage)
}
