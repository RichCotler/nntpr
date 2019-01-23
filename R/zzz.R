#
#' nntpr Function
#'
#' This function initializes the R reticulate environment for the rest of nntpr
#' @keywords internal
#' @import reticulate
#' @importFrom plyr llply
#' @importFrom purrr reduce
#' @import stringr
#' @import rlist
#' @import uuid
#' @import readr
#' @import R.utils
#' @import stats
#' @import htmltools
#' @import htmlwidgets
#' @import timevis
#' @import tm
#' @import wfindr
#' @import wordcloud2
#' @docType package
#' @name nntpr
#' @export
nntplib <- NULL
nntpr.private <- new.env()
nntpr.private$gsvconn <- NULL
nntpr.private$ggroupinfovector <- NULL
nntpr.private$gretmessage <- NULL
nntpr.private$gfirstartinfo <- NULL
nntpr.private$glastartinfo <- NULL
nntpr.private$gfrom.name <- NULL
nntpr.private$gfrom.email.name <- NULL
nntpr.private$gfrom.email.domain <- NULL
nntpr.private$gexectime <- NULL

nntpr.private$gposteof <- "\\xc3\\xa9."  # eof sequence to indicate end of text post

.onLoad <- function(libname, pkgname) {
    nntplib <<- reticulate::import("nntplib")

}
