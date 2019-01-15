# 
#' groupdatevolume Function
#'
#' This function provides a timevis timeline plot, providing an overview of
#' a newsgroup's average posts per day, ranked across quartiles, for a series of
#' date ranges from first to last available article. A sample size is calculated, determining
#' the number of article dates retrieved from usenet at mostly equal article count distances.
#' The date ranges plotted represent the resulting sample periods, with a calculated average
#' number of posts per day for that sample period.
#'
#' groupdatevolume calls a private function, samplegroup, to collect
#' the article post dates for a randomly generated sample set of article numbers,
#' returning a data frame of the sample set data carrying article number, post date, and for convenience
#' an article count since previous sample point.
#'
#' The data is then sorted by posts per day and assigned color codes across the sample's
#' posts per day quartiles.  That is passed to the timevis package to create an interactive
#' time line plot.  The time line plot object is returned to the caller.
#'
#' The default margin of error used to calculate sample size (confidence interval) is 5%, but calling
#' groupdatevolume with 'test' as a parameter will change that to 20%.  This returns a sample size
#' one quarter of the default size.
#'
#' @param runmode optional parameter, set to 'test' request a smaller sample set for testing purposes
#' @export
groupdatevolume <- function(runmode = NULL) {
    
    options(stringsAsFactors = FALSE)
    # collect basic sample points and article counts
    if (!is.null(runmode)) {
        runmode <- "test"
    }
    samples.df <- samplegroup(runmode)
    
    # rollup by date, calculate date diffs and posts per day
    samples.df$articledate <- as.Date(samples.df$articledatetime)
    rollups.df <- aggregate(samples.df["articlecount"], by = samples.df["articledate"], 
        sum)
    propsssize <- nrow(rollups.df)
    rollups.df$perioddays <- Reduce(c, lapply(1:propsssize, function(x) {
        if (x == 1) {
            0
        } else {
            max(1, round(as.numeric(difftime(rollups.df$articledate[x], rollups.df$articledate[x - 
                1], units = "days")), 0))
        }
    }))
    rollups.df$postsperday <- Reduce(c, lapply(1:propsssize, function(x) {
        if (x == 1) {
            0
        } else {
            round(rollups.df$articlecount[x]/rollups.df$perioddays[[x]], 0)
        }
    }))
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
    
    timeline_plot_widget <- prependContent(timeline_plot_widget, includeCSS(system.file("css/style.css", 
        package = "nntpr")))
    
    return(timeline_plot_widget)
    
}
