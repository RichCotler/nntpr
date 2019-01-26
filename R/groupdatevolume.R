#
#' groupdatevolume Function
#'
#' This function provides a timevis timeline plot, providing an overview of a newsgroup's
#' average posts per day, ranked across quartiles, for a series of date ranges from first to
#' last available article.  An optional wordcloud of the sampled article’s subject fields can be requested.
#'
#' NOTE:
#' The execution time for this function can vary greatly, depending on the size of the newsgroup,
#' network speeds, and news server load.  Text progress bars are displayed in the R console for the process
#' steps most influenced by these factors.
#'
#' Process:
#' A sample size proportional to the number of articles available in the newsgroup is calculated
#' based on a formula derived from surveysystem.com.
#' The number of articles in the newsgroup and the requested confidence interval are variables supplied to the
#' calculation function (propsamplesize), and confidence level is set at 95%.
#' The default margin of error supplied to the calculation is 5%.
#' If testmode is requested (testmode = TRUE), a margin of error (confidence interval) of 20%
#' is used, which provides a smaller sample size to facilitate testing before committing to a full sample run.
#'
#' The calculated sample size determines the number of article dates retrieved from usenet at mostly equal
#' article count distances.  The number of days and number of articles between the collected sample dates is
#' calculated, and an average number of articles posted per day for each sample period is based on those aggregates.
#'
#' The data is then sorted by posts per day and assigned color codes across the sample's posts per day quartiles.
#' The sorted and categorized data is passed to the timevis package to create an object containing an interactive
#' time line report widget - \strong{Average Posts Per Day for Sample Periods}.
#'
#' If a wordcloud is requested (wordcloud = TRUE), the subject lines for each of the sample point articles are
#' collected.  They’re cleansed and run against a 263,000+ English word dictionary provided by the wfindr
#' package.  What remains is sent to a wordcloud2 call, producing an object containing a term frequency
#' wordcloud plot widget.
#'
#' A list containing the report widget object and wordcloud widget object (or NULL if no wordcloud was requested)
#' is returned to the caller.
#'
#' @param testmode optional parameter, set to TRUE to request a smaller sample set for testing purposes
#' @param wordcloud optional parameter, set to TRUE to request a wordcloud2 plot of words from sampled subject fields
#' @param minSize optional parameter, wordcloud2 setting overide with default of 3. Ignored unless wordcloud = TRUE.
#' @export
groupdatevolume <- function(testmode = FALSE, wordcloud = FALSE, minSize = 3) {

    options(stringsAsFactors = FALSE)
    wordcloud_plot_widget = NULL
    groupinfo <- nntpr.private$ggroupinfovector

    # if a bad minSize override is requested, just flip it back to default value of 3
    if(!is.wholenumber(minSize)) { minSize <- 3 }

    # collect basic sample points and article counts
    samples.df <- samplegroup(testmode)

    # rollup by date, calculate date diffs and posts per day
    samples.df$articledate <- as.Date(samples.df$articledatetime)
    rollups.df <- aggregate(samples.df["articlecount"], by = samples.df["articledate"],
        sum)
    propsssize <- nrow(rollups.df)
    cat("aggregating period days and posts per day...", "\n")
    rollups.df$perioddays <- Reduce(c, llply(1:propsssize, function(x) {
        if (x == 1) {
            0
        } else {
            max(1, round(as.numeric(difftime(rollups.df$articledate[x], rollups.df$articledate[x -
                1], units = "days")), 0))
        }
    }, .progress = "text"))
    rollups.df$postsperday <- Reduce(c, llply(1:propsssize, function(x) {
        if (x == 1) {
            0
        } else {
            round(rollups.df$articlecount[x]/rollups.df$perioddays[[x]], 0)
        }
    }, .progress = "text"))
    rollups.df$prevarticledate <- c(rollups.df$articledate[1], rollups.df$articledate[1:nrow(rollups.df) -
        1])

    # setup posts per day display colors for the quartile ranges
    bg <- "background-color: "
    colors_qt <- c("LightGray", "DodgerBlue", "LightGreen", "Orange", "Red")
    qtvalues <- quantile(rollups.df$postsperday)
    groups.df <- data.frame(id = colors_qt, content = names(qtvalues), style = str_c(rep(bg,
        5), colors_qt))

    rollups.df$group <- rep(colors_qt[5], nrow(rollups.df))
    for (idx in 4:1) {
        rollups.df$group[rollups.df$postsperday <= qtvalues[[idx]]] <- colors_qt[idx]
    }
    rollups.df$style <- str_c(rep(bg, nrow(rollups.df)), rollups.df$group)

    df2plot <- data.frame(start = rollups.df$prevarticledate, end = rollups.df$articledate,
        content = format(rollups.df$postsperday, big.mark = ","), group = rollups.df$group,
        style = rollups.df$style)

    timeline_plot_widget <- timevis(df2plot[order(df2plot$content), ], groups = groups.df) %>%
        setOptions(list(min = df2plot$start[1], max = Sys.Date(), type = "range")) %>%
        setGroups(groups.df[5:1, ])

    timeline_mod_taglist <- tagList(includeCSS(system.file("css/style.css", package = "nntpr")),
        tags$h2("Average Posts Per Day for Sample Periods", align = "center"), tags$h3(groupinfo[5],
            align = "center"), tags$h6("quartile", align = "left"))
    timeline_plot_widget <- prependContent(timeline_plot_widget, timeline_mod_taglist)

    if (wordcloud) {
        subjectvector <- samplesubject(samples.df$article)
        wordcloud_plot_widget <- subjects2wordcloud(subjectvector, minSize)
    }

    return(list(timeline_plot_widget, wordcloud_plot_widget))
}

