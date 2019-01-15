#
#' privateutility Function
#'
#' This function provides private utility functions
#'  used by other nntpr functions
#' \itemize{
#' \item getretmessage - global retmessage retrieval
#' \item getartinfo - return list with first and last article information stored by listartheader and listheaderfield
#' \item getgroupinfo - return Group information vector (response, count, first, last, name) stored by group
#' \item getexectime - return last stored execution time info (proc.time) from group list, xover, and xhdr calls
#' \item getclocktime - return a string with the rounded elapsed time from getexectime
#' \item write_temptext - writes fully constructed text message vector to temporary file for posting
#' \item delete_temptext - cleans up temporary text file after posting
#' \item set_temptext_filename - returns temporary text file using message_id UUID
#' \item set_from_header - returns From header line constructed from stored name, email name, email domain values
#' \item set_messageid_header - returns Message-ID header line constructed from supplied message_id and stored email domain
#' \item count_lines - returns the numnber of lines in the supplied text file (with path)
#' \item validate_list_call - returns an error message if a list function is called without a group set or with first > last article numbers
#' \item validate_header_field - returns an error message if an invalid header field name is requested
#' \item adjust_article_range - returns a vector with original first and last article numbers or adjusted value(s) if a requested number falls outside of the group's actual range
#' \item execute_listhdr_call - executes validated nntplib xhdr or xover call with appropriate passed parameters
#' \item is.wholenumber - tests a variable to see if it is numeric and is an integer
#' \item propsamplesize - calculates proportional sample size for systematic sampling of newsgroup article information
#' \item samplegroup - creates a sample set data frame of article numbers, postdates, counts for group volume plotting across group dates
#' }
#' @keywords internal
#'
# stub function for collection
privateutility <- function() {

}

# returns contents of nntpr.private$gretmessage to caller
getretmessage <- function() {
    retmessage <- nntpr.private$gretmessage
    return(retmessage)
}

# returns a list with listarticles first and last article information to caller
getartinfo <- function() {
    artinfolist <- list(nntpr.private$gfirstartinfo, nntpr.private$glastartinfo)
    return(artinfolist)
}

# returns Group information vector (response, count, first, last, name) to caller
getgroupinfo <- function() {
    groupinfovector <- nntpr.private$ggroupinfovector
    return(groupinfovector)
}

# returns last stored execution time info saved by xhdr, xover, and group list
# calls as proc.time
getexectime <- function() {
    execproctime <- nntpr.private$gexectime
    return(execproctime)
}

# returns a string with the rounded elapsed time from getexectime
getclocktime <- function() {
    clocktimestring <- as.character(round(publicutility("exectime")[[3]], digits = 3))
    return(clocktimestring)
}

# writes text message to be posted to a temporary file named using message_id
# UUID returns temporary file line count
write_temptext <- function(message_id, message_vector) {
    posttempfile <- set_temptext_filename(message_id)
    tmpout <- write_lines(message_vector, path = posttempfile)
    lineswritten <- count_lines(posttempfile)
    return(lineswritten)
}

# cleans up temporary text message file after posting
delete_temptext <- function(message_id) {
    resbool <- file.remove(set_temptext_filename(message_id))
    return(resbool)
}

# formats and returns temporary text message file name using message_id UUID
set_temptext_filename <- function(message_id) {
    tmp_filename <- str_c("./", message_id, ".tmp.txt")
    return(tmp_filename)
}

# constructs and returns From header line using name, email name, and email
# domain in nntpr.private
set_from_header <- function() {
    from_header <- str_c("From: ", nntpr.private$from.name, " <", nntpr.private$from.email.name,
        "@", nntpr.private$from.email.domain, ">")
    return(from_header)
}

# constructs and returns Message-ID header line using supplied message_id UUID
# and email domain from nntpr.private
set_messageid_header <- function(message_id) {
    messageid_header <- str_c("Message-ID: <", message_id, "@", nntpr.private$from.email.domain,
        ">\n")
    return(messageid_header)
}

# returns the number of lines in the text file at the supplied path
count_lines <- function(filename) {
    text_lines <- countLines(filename)
    return(text_lines)
}

# returns an error message if no group is set or if first article number > last
# article number
validate_list_call <- function(first_article_string, last_article_string, timeout_seconds) {
    retmessage <- NULL

    if (is.null(nntpr.private$ggroupinfovector)) {
        retmessage <- c("Error: Group not set, run group(<group name>) first.")
    } else if (!is.wholenumber(timeout_seconds)) {
        retmessage <- str_c("Error: ", as.character(timeout_seconds), " is not a valid timeout seconds value.")
    } else if (as.numeric(first_article_string) > as.numeric(last_article_string)) {
        retmessage <- c("Error: First article number cannot be greater than last article number.")
    }
    return(retmessage)
}

# return an error message if an invalid header field value is requested
validate_header_field <- function(header_field_name) {
    retmessage <- NULL

    valid_header_fields <- c("path", "from", "subject", "newsgroups", "user-agent",
        "message-id", "x-no-archive", "lines", "x-complaints-to", "nntp-posting-date",
        "organization", "bytes", "date", "x-received-bytes", "x-received-body-crc")

    if (!is.element(header_field_name, valid_header_fields)) {
        retmessage <- str_c("Error: ", header_field_name, " is not a valid header field.")
    }
    return(retmessage)
}

# returns a vector with first and last article numbers, retaining the initial
# value or adjusted value if a number falls outside of the actual range for the
# group
adjust_article_range <- function(first_article_string, last_article_string) {

    if (as.numeric(first_article_string) < as.numeric(nntpr.private$ggroupinfovector[3])) {
        first_article_string <- nntpr.private$ggroupinfovector[3]
    }
    if (as.numeric(last_article_string) > as.numeric(nntpr.private$ggroupinfovector[4])) {
        last_article_string <- nntpr.private$ggroupinfovector[4]
    }

    return(c(first_article_string, last_article_string))

}

# run a call to xhdr or xover depending on requested function if xhdr, call_parm1
# is header field name, call_parm2 is article_range if xover, call_parm1 is
# first_article_string, call_parm2 is last_article_string
execute_listhdr_call <- function(requested_function, call_parm1, call_parm2, filter_term_string = NULL) {
    retvariable <- NULL

    valid_functions <- c("xhdr", "xover")
    if (!is.element(requested_function, valid_functions)) {
        retvariable <- str_c("Error: ", requested_function, " is not a valid listhdr function.")
    } else {
        cmdhdr <- c("articlelist = ")
        callstr <- str_c("svconn.", requested_function, "('", call_parm1, "', '",
            call_parm2, "')[1]")
        if (!is.null(filter_term_string)) {
            fullcmd <- str_c(cmdhdr, "filter(lambda x: '", str_to_upper(filter_term_string),
                "' in str.upper(x[1]), ", callstr, ")")
        } else {
            fullcmd <- str_c(cmdhdr, callstr)
        }
        callresult <- py_run_string(fullcmd, FALSE)
        retvariable <- py$articlelist
    }

    return(retvariable)
}

# function based on example in R Documentation for ingeter {base} with addition
# of numeric check to bypass calculations if the variable passed is not numeric
is.wholenumber <- function(variable_to_test) {
    returnbool <- NULL
    if (is.numeric(variable_to_test)) {
        returnbool <- abs(variable_to_test - round(variable_to_test)) <= 0
    } else {
        returnbool <- FALSE
    }
    return(returnbool)
}

# function returning proportional sample size (based on surveysystem.com formula)
# using group size, a confidence interval of 10, and confidence level of 95%.
# groupsize - total number of articles in the newsgroup
# marginoferror - confidence interval (c), % / 100
# ppct - sample size needed (p)
# confidencelvl - confidence level %
propsamplesize <- function(groupsize, marginoferror, ppct = 0.5, confidencelvl = 95) {
    zsqr <- round(qnorm(0.5 + confidencelvl/200), 2)^2  # calculated Z squared
    samplesize <- (zsqr * ppct * (1 - ppct))/marginoferror^2
    propsamplesize <- round(samplesize/(1 + ((samplesize - 1)/groupsize)), 0)
    return(propsamplesize)
}

# function returning a data frame of article numbers and normalized post dates,
# and for convenience an article count since previous sample point for a
# systematic sampling of articles from the oldest to newest available posts of
# the target newsgroup.  using optional parameter runmode, which can be NULL
# (default) or "test".  a null runmode calls propsamplesize() with a margin of
# error set to 5%.  runmode test calls propsamplesize() with a margin of error
# set to 20%, which creates a smaller sample set.  The sample set (ss) is
# collected and processed as lists, then molded into the returned data frame.
samplegroup <- function(runmode = NULL) {
    groupinfo <- nntpr.private$ggroupinfovector
    daysvector <- c("Mon, ", "Tue, ", "Wed, ", "Thu, ", "Fri, ", "Sat, ", "Sun, ")
    if (!is.null(runmode)) {
        marginoferror <- 0.2
    } else {
        marginoferror <- 0.05
    }
    propsssize <- propsamplesize(as.numeric(groupinfo[2]), marginoferror)

    # choose sample points based on intervals of article numbers
    sampleinterval <- round(as.numeric(groupinfo[2])/propsssize)  # article count / # samples
    sspropminus <- propsssize - 1
    samplemulti <- c(0, seq(1:sspropminus))
    samplepoints <- c(rep(as.numeric(groupinfo[3]), propsssize))
    sampleintvec <- rep(sampleinterval, propsssize)
    samplearticles <- samplepoints + (sampleintvec * samplemulti)
    propsssize <- propsssize + 1
    samplearticles[propsssize] <- as.numeric(groupinfo[4])
    sslist <- list(article = samplearticles)

    # collect and normalize article post dates for the sample set
    sslist$articledatetime <- lapply(1:propsssize, function(x) {
        listheaderfield("date", as.character(sslist$article[x]), as.character(sslist$article[x]),
            0)[[1]][[2]]
    })
    # change GMT abbrev to TZ offset number and convert to date class
    sslist$articledatetime <- lapply(1:propsssize, function(x) {
        if (any(str_detect(sslist$articledatetime[x], daysvector))) {
            strptime(str_replace(sslist$articledatetime[x], "GMT", "+0000"), format = "%a, %d %b %Y %R:%S %z")
        } else {
            strptime(str_replace(sslist$articledatetime[x], "GMT", "+0000"), format = "%d %b %Y %R:%S %z")
        }
    })

    # verify the articles count between sample points
    sslist$articlecount <- lapply(1:propsssize, function(x) {
        if (x == 1) {
            0
        } else {
            as.numeric(sslist$article[x]) - as.numeric(sslist$article[x - 1])
        }
    })

    ssdataframe <- data.frame(article = sslist$article, articledatetime = Reduce(c,
        sslist$articledatetime), articlecount = Reduce(c, sslist$articlecount))
    return(ssdataframe)

}

