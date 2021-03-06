# 
#' privateutility Function
#'
#' This function provides private utility functions
#'  used by other nntpr functions
#' \itemize{
#' \item adjust_article_range - returns a vector with original first and last article numbers or adjusted value(s) if a requested number falls outside of the group's actual range
#' \item call2python - invoke reticulate functions to run python commands with exception handling
#' \item count_lines - returns the number of lines in the supplied text file (with path)
#' \item delete_temptext - cleans up temporary text file after posting
#' \item execute_listhdr_call - executes validated nntplib xhdr or xover call with appropriate passed parameters
#' \item getartinfo - return list with first and last article information stored by listartheader and listheaderfield
#' \item getclocktime - return a string with the rounded elapsed time from getexectime
#' \item getexectime - return last stored execution time info (proc.time) from group list, xover, and xhdr calls
#' \item getgroupinfo - return Group information vector (response, count, first, last, name) stored by group
#' \item getretmessage - global retmessage retrieval
#' \item is.wholenumber - tests a variable to see if it is numeric and is an integer
#' \item propsamplesize - calculates proportional sample size for systematic sampling of newsgroup article information
#' \item samplegroup - creates a sample set data frame of article numbers, postdates, counts for group volume plotting across group dates
#' \item sampleheaderfield - called from inside list apply loop, calls header retrieval with error handling and retry logic
#' \item samplesubject - creates a sample set data frame of article subjects for a set of article numbers for processing to produce a term frequency wordcloud
#' \item set_from_header - returns From header line constructed from stored name, email name, email domain values
#' \item set_messageid_header - returns Message-ID header line constructed from supplied message_id and stored email domain
#' \item set_temptext_filename - returns temporary text file using message_id UUID
#' \item subjects2wordcloud - takes the subject fields in the groupdatevolume sample and produces a wordcloud on term freq
#' \item validate_header_field - returns an error message if an invalid header field name is requested
#' \item validate_list_call - returns an error message if a list function is called without a group set or with first > last article numbers
#' \item write_temptext - writes fully constructed text message vector to temporary file for posting
#' }
#' @keywords internal
#'
# stub function for collection
privateutility <- function() {
}

# Returns a vector with first and last article numbers, retaining the
# initial value or adjusted value if a number falls outside of the
# actual range for the group
adjust_article_range <- function(first_article_string, last_article_string) {
    
    if (as.numeric(first_article_string) < as.numeric(nntpr.private$ggroupinfovector[3])) {
        first_article_string <- nntpr.private$ggroupinfovector[3]
    }
    if (as.numeric(last_article_string) > as.numeric(nntpr.private$ggroupinfovector[4])) {
        last_article_string <- nntpr.private$ggroupinfovector[4]
    }
    
    return(c(first_article_string, last_article_string))
    
}

# Wrap call to python with exception handling
call2python <- function(cmd2run) {
    callheader <- "try:\n   respfrompy = "
    calltrailer <- "\nexcept Exception as err:\n   respfrompy = err\n"
    callresponse <- NULL
    retmessage <- NULL
    
    callstring <- str_c(callheader, cmd2run, calltrailer)
    
    respdict <- py_run_string(callstring, local = TRUE)
    
    # see if the python command caught nntplib exception
    if ("err" %in% names(respdict)) {
        retmessage <- str_c("Error encountered: ", respdict$err, "; class: ", 
            class(respdict$respfrompy)[[1]])
        message(retmessage)
        message("cmd: ", cmd2run)
        if (class(respdict$respfrompy)[[1]] != "nntplib.NNTPTemporaryError") {
            message("You should rerun connect and group functions before continuing.")
            nntpr.quit()
        }
    } else {
        retmessage <- str_c("Call returned ", as.character(length(respdict$respfrompy)), 
            " item(s).")
        callresponse <- respdict$respfrompy
    }
    nntpr.private$gretmessage <- retmessage
    return(callresponse)
}

# Returns the number of lines in the text file at the supplied path
count_lines <- function(filename) {
    text_lines <- countLines(filename)
    return(text_lines)
}

# Cleans up temporary text message file after posting
delete_temptext <- function(message_id) {
    resbool <- file.remove(set_temptext_filename(message_id))
    return(resbool)
}

# Runs a call to xhdr or xover depending on requested function if xhdr,
# call_parm is header field name, call_parm2 is article_range if xover,
# call_parm1 is first_article_string, call_parm2 is last_article_string
execute_listhdr_call <- function(requested_function, call_parm1, call_parm2, 
    filter_term_string = NULL) {
    retvariable <- NULL
    
    valid_functions <- c("xhdr", "xover")
    if (!is.element(requested_function, valid_functions)) {
        retvariable <- str_c("Error: ", requested_function, " is not a valid listhdr function.")
    } else {
        callstr <- str_c("svconn.", requested_function, "('", call_parm1, 
            "', '", call_parm2, "')[1]")
        if (!is.null(filter_term_string)) {
            fullcmd <- str_c("filter(lambda x: '", str_to_upper(filter_term_string), 
                "' in str.upper(x[1]), ", callstr, ")")
        } else {
            fullcmd <- callstr
        }
        retvariable <- call2python(fullcmd)
    }
    
    return(retvariable)
}

# Returns a list with listarticles first and last article information
# to caller
getartinfo <- function() {
    artinfolist <- list(nntpr.private$gfirstartinfo, nntpr.private$glastartinfo)
    return(artinfolist)
}

# Returns a string with the rounded elapsed time from getexectime
getclocktime <- function() {
    clocktimestring <- as.character(round(publicutility("exectime")[[3]], 
        digits = 3))
    return(clocktimestring)
}

# Returns last stored execution time info saved by xhdr, xover, and
# group list calls as proc.time
getexectime <- function() {
    execproctime <- nntpr.private$gexectime
    return(execproctime)
}

# Returns Group information vector (response, count, first, last, name)
# to caller
getgroupinfo <- function() {
    groupinfovector <- nntpr.private$ggroupinfovector
    return(groupinfovector)
}

# Returns contents of nntpr.private$gretmessage to caller
getretmessage <- function() {
    retmessage <- nntpr.private$gretmessage
    return(retmessage)
}

# Function based on example in R Documentation for integer {base} with
# addition of numeric check to bypass calculations if the variable
# passed is not numeric
is.wholenumber <- function(variable_to_test) {
    returnbool <- NULL
    if (is.numeric(variable_to_test)) {
        returnbool <- abs(variable_to_test - round(variable_to_test)) <= 
            0
    } else {
        returnbool <- FALSE
    }
    return(returnbool)
}

# Function returning proportional sample size (based on
# surveysystem.com formula) using group size, a confidence interval of
# 10, and confidence level of 95%.  groupsize - total number of
# articles in the newsgroup marginoferror - confidence interval (c), %
# / 100 ppct - sample size needed (p) confidencelvl - confidence level
# %
propsamplesize <- function(groupsize, marginoferror, ppct = 0.5, confidencelvl = 95) {
    zsqr <- round(qnorm(0.5 + confidencelvl/200), 2)^2  # calculated Z squared
    samplesize <- (zsqr * ppct * (1 - ppct))/marginoferror^2
    propsamplesize <- round(samplesize/(1 + ((samplesize - 1)/groupsize)), 
        0)
    return(propsamplesize)
}

# Function returning a data frame of article numbers, normalized post
# dates, and an article count since previous sample point for a
# sampling of articles from the oldest to newest available posts of the
# target newsgroup.  If testmode is FALSE (default), a confidence
# interval of 5% is provided to the proportional sample size
# calculation function (propsamplesize).  If testmode is requested
# (testmode = TRUE), a confidence interval of 20% is provided to the
# sample size calculation, which returns a significantly smaller sample
# size.  The article numbers of the sample points are calculated based
# on newsgroup size, starting article number, and sample interval
# (newsgroup size / sample size). The post dates are cleaned up, and
# the number of posts between sample points is calculated.  These are
# formed into a data frame and returned to the caller.
samplegroup <- function(testmode) {
    groupinfo <- nntpr.private$ggroupinfovector
    daysvector <- c("Mon, ", "Tue, ", "Wed, ", "Thu, ", "Fri, ", "Sat, ", 
        "Sun, ")
    if (testmode) {
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
    options(error = recover)
    
    cat("collecting sample set post dates...", "\n")
    sslist$articledatetime <- llply(1:propsssize, function(x) {
        sampleheaderfield(sslist$article[x])
    }, .progress = "text")
    # change GMT abbrev to TZ offset number and convert to date class
    cat("formatting sample set post dates...", "\n")
    sslist$articledatetime <- llply(1:propsssize, function(x) {
        if (any(str_detect(sslist$articledatetime[x], daysvector))) {
            strptime(str_replace(sslist$articledatetime[x], "GMT", "+0000"), 
                format = "%a, %d %b %Y %R:%S %z")
        } else {
            strptime(str_replace(sslist$articledatetime[x], "GMT", "+0000"), 
                format = "%d %b %Y %R:%S %z")
        }
    }, .progress = "text")
    
    # verify the articles count between sample points
    sslist$articlecount <- llply(1:propsssize, function(x) {
        if (x == 1) {
            0
        } else {
            as.numeric(sslist$article[x]) - as.numeric(sslist$article[x - 
                1])
        }
    })
    
    ssdataframe <- data.frame(article = sslist$article, articledatetime = Reduce(c, 
        sslist$articledatetime), articlecount = Reduce(c, sslist$articlecount))
    return(ssdataframe)
    
}

# Function to handle collection of header fields for article from the
# sample set, with error handling and retry logic.  This is for the
# situation where a call to get the header field has a problem and
# doesn't return a proper field, causing a subscript out of bounds
# error.  Subsequent retries of the same article header often clear the
# issue.  The article number to sample is passed from the caller The
# date field is default (subjectfield = FALSE) The subject field is
# returned if called with subjectfield = TRUE
sampleheaderfield <- function(article, subjectfield = FALSE) {
    if (subjectfield) {
        hdrfield <- "subject"
    } else {
        hdrfield <- "date"
    }
    articleheaderfield <- NULL
    maxRetries <- 10
    retryCount <- 0
    
    while (retryCount < maxRetries) {
        rawheaderfield <- listheaderfield(hdrfield, as.character(article), 
            as.character(article))
        if (!is.atomic(rawheaderfield)) {
            articleheaderfield <- rawheaderfield[[1]][[2]]
            retryCount <- maxRetries
        }
        retryCount <- retryCount + 1
    }
    
    return(articleheaderfield)
}

# Function returning a vector of article subjects based on the article
# numbers of the sample points created by the samplegroup function
# call.
samplesubject <- function(samplearticles) {
    
    # collect subject entries for the sample set
    cat("collecting sample set subject fields...", "\n")
    sslist <- list(article = samplearticles)
    sslist$subject <- llply(1:length(samplearticles), function(x) {
        sampleheaderfield(sslist$article[x], subjectfield = TRUE)
    }, .progress = "text")
    
    articlesubjects <- Reduce(c, sslist$subject)
    return(articlesubjects)
    
}

# Constructs and returns From header line using name, email name, and
# email domain in nntpr.private
set_from_header <- function() {
    from_header <- str_c("From: ", nntpr.private$from.name, " <", nntpr.private$from.email.name, 
        "@", nntpr.private$from.email.domain, ">")
    return(from_header)
}

# constructs and returns Message-ID header line using supplied
# message_id UUID and email domain from nntpr.private
set_messageid_header <- function(message_id) {
    messageid_header <- str_c("Message-ID: <", message_id, "@", nntpr.private$from.email.domain, 
        ">\n")
    return(messageid_header)
}

# Formats and returns temporary text message file name using message_id
# UUID
set_temptext_filename <- function(message_id) {
    tmp_filename <- str_c("./", message_id, ".tmp.txt")
    return(tmp_filename)
}

# Function processing the subject fields collected in the
# groupdatevolume sampling, returning a wordcloud2 plot of term
# frequency.  The wordcloud2 minSize setting is passed from the caller.
# The subject fields are cleansed of special characters, then processed
# using tm's termFreq function. tm::termFreq converts everything to
# lower case, removes punctuation, numbers, stopwords, and any word not
# found in the wfindr::words.eng dictionary vector (263000+ words).
# While the filter against the dictionary may severely constrain the
# resulting words to plot, it is implemented as a balance to frequent
# non-language elements prevalent in the subject fields of some
# newsgroups.
subjects2wordcloud <- function(subjectvector, minSize) {
    # remove special characters before attempting to process subject fields
    subjectvector <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", subjectvector)
    
    termfreqvector <- termFreq(subjectvector, control = list(removePunctuation = TRUE, 
        removeNumbers = TRUE, stopwords = TRUE, dictionary = wfindr::words.eng))
    
    termfreqdataframe <- data.frame(word = names(termfreqvector), freq = termfreqvector)
    
    return(wordcloud2(data = termfreqdataframe, minSize = minSize, rotateRatio = 0.4, 
        color = "random-dark"))
}

# Returns an error message if an invalid header field value is
# requested or returns the vector of valid field names if a utility
# list request (utlistrequest) value is provided
validate_header_field <- function(header_field_name) {
    retmessage <- NULL
    
    valid_header_fields <- c("path", "from", "subject", "newsgroups", "user-agent", 
        "message-id", "x-no-archive", "lines", "x-complaints-to", "nntp-posting-date", 
        "organization", "bytes", "date", "x-received-bytes", "x-received-body-crc")
    
    if (header_field_name == "utlistrequest") {
        retmessage <- valid_header_fields
    } else {
        if (!is.element(header_field_name, valid_header_fields)) {
            retmessage <- str_c("Error: ", header_field_name, " is not a valid header field.")
        }
    }
    
    return(retmessage)
}

# Returns an error message if no group is set or if first article
# number > last article number
validate_list_call <- function(first_article_string, last_article_string) {
    retmessage <- NULL
    
    if (is.null(nntpr.private$ggroupinfovector)) {
        retmessage <- c("Error: Group not set, run group(<group name>) first.")
    } else if (as.numeric(first_article_string) > as.numeric(last_article_string)) {
        retmessage <- c("Error: First article number cannot be greater than last article number.")
    }
    
    if (!is.null(retmessage)) {
        message(retmessage)
    }
    
    return(retmessage)
}

# Writes text message to be posted to a temporary file named using
# message_id UUID returns temporary file line count
write_temptext <- function(message_id, message_vector) {
    posttempfile <- set_temptext_filename(message_id)
    tmpout <- write_lines(message_vector, path = posttempfile)
    lineswritten <- count_lines(posttempfile)
    return(lineswritten)
}

