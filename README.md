# nntpr

#### Rich Cotler
#### January, 2019

The nntpr R package is an in-progress project that I started with some
basic personal objectives in mind:

* learn how to develop R packages
* develop some simple nntp utility and analysis functions
* learn how to integrate python code and objects with R

The python integration was a side effect of my choice of working with nntp. I didn't run across any native NNTP protocol libraries or methods in R, but base python has an nntplib that fit the bill nicely.  I know I could have written the equivalent of nntpr completely in python, but that wouldn't have satisfied my first objective.

The application runs in R Studio It is written in
R (developed using version 3.5.1) and requires installtion of the following CRAN libraries:

    R.utils,
    htmltools,
    htmlwidgets,
    purrr,
    readr,
    reticulate,
    rlist,
    stats,
    stringr,
    timevis,
    uuid


This is a work in progress.  More functionality and documentation as time permits.
