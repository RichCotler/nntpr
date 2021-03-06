% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groupdatevolume.R
\name{groupdatevolume}
\alias{groupdatevolume}
\title{groupdatevolume Function}
\usage{
groupdatevolume(testmode = FALSE, wordcloud = FALSE, minSize = 3)
}
\arguments{
\item{testmode}{optional parameter, set to TRUE to request a smaller sample set for testing purposes}

\item{wordcloud}{optional parameter, set to TRUE to request a wordcloud2 plot of words from sampled subject fields}

\item{minSize}{optional parameter, wordcloud2 setting overide with default of 3. Ignored unless wordcloud = TRUE.}
}
\description{
This function provides a timevis timeline plot, providing an overview of a newsgroup's
average posts per day, ranked across quartiles, for a series of date ranges from first to
last available article.  An optional wordcloud of the sampled article’s subject fields can be requested.
}
\details{
NOTE:
The execution time for this function can vary greatly, depending on the size of the newsgroup,
network speeds, and news server load.  Text progress bars are displayed in the R console for the process
steps most influenced by these factors.

Process:
A sample size proportional to the number of articles available in the newsgroup is calculated
based on a formula derived from surveysystem.com.
The number of articles in the newsgroup and the requested confidence interval are variables supplied to the
calculation function (propsamplesize), and confidence level is set at 95%.
The default margin of error supplied to the calculation is 5%.
If testmode is requested (testmode = TRUE), a margin of error (confidence interval) of 20%
is used, which provides a smaller sample size to facilitate testing before committing to a full sample run.

The calculated sample size determines the number of article dates retrieved from usenet at mostly equal
article count distances.  The number of days and number of articles between the collected sample dates is
calculated, and an average number of articles posted per day for each sample period is based on those aggregates.

The data is then sorted by posts per day and assigned color codes across the sample's posts per day quartiles.
The sorted and categorized data is passed to the timevis package to create an object containing an interactive
time line report widget - \strong{Average Posts Per Day for Sample Periods}.

If a wordcloud is requested (wordcloud = TRUE), the subject lines for each of the sample point articles are
collected.  They’re cleansed and run against a 263,000+ English word dictionary provided by the wfindr
package.  What remains is sent to a wordcloud2 call, producing an object containing a term frequency
wordcloud plot widget.

A list containing the report widget object and wordcloud widget object (or NULL if no wordcloud was requested)
is returned to the caller.
}
