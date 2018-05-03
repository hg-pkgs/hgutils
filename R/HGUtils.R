# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

devtools::use_package("dplyr","Imports")
devtools::use_package("numbers","Imports")
devtools::use_package("magrittr","Imports")
devtools::use_package("tibble","Imports")

#' Installs and loads specified packages
#'
#' @param packages A character list of package names
#' @param install Whether to install packages when missing, defaults to TRUE
#'
#' @return NULL
#'
#' @examples install_load_packages(c("ggplot2","dplyr"))
#' @export
install_load_packages = function(packages,install = TRUE){
  for(package in packages){
    if(!require(package, character.only = TRUE) & install){
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

#' Loads (and optionally install) commonly used packages
#'
#' @param install Whether to install a package if missing
#'
#' @return NULL
#'
#' @export
load_common_packages = function(install = T){
install_load_packages(c("numbers","magrittr","colorspace","RColorBrewer","grid","gridExtra","readxl","writexl","shinydashboard",
                        "shinyBS","shiny","shinyjs","shinyWidgets","shinydashboard","shinyBS","shiny","shinyjs","rms","devtools",
                        "ggthemes","stringr","reshape2","gridGraphics","tidyverse", "scales"),install = install)
}

#' Clears the workspace and sets the working directory automatically to the Dropbox folder
#'
#' @param folder String to specify subfolder name
#'
#' @return NULL
#'
#' @examples startup("source_webinterface/")
#' @export
startup = function(folder = "source_webinterface/")
{
  rm(list = ls(pos = .GlobalEnv), envir = .GlobalEnv)
  gc()
  graphics.off()

  if (grepl("^[A-z]:[\\/][^\\.]*$", folder))
  {
    if (dir.exists(folder)) setwd(folder) else warning(paste0("Working directory not set, cannot find ",folder))
  } else
  {
    results = working_dirs %>% filter(usr == Sys.info()["user"])
    if (results %>% nrow == 1){
      d = paste0(results$location,folder)
      if (dir.exists(d)) {setwd(d); print(paste0("Setting the working directory at: ",d))} else
        {warning("Could not find the working directory")}
    } else {
      warning("Could not find the working directory")
    }

  }
}

#' Set the breaks for a graph in nice positions.
#'
#' @param limits The limits of the axis. May be a list of 2 elements with lower and upper bounds, or a
#'               single digit (the upperbound, the lowerbound is then assumed to be 0).
#' @param N The unit stepsize; The eventual interval size will be multiples of the divisors of N. Defaults to 10
#' @param max_breaks Maximum amount of steps, defaults to 10
#' @param int_only Whether only integer divisors of N may be used for interval sizes, default to TRUE
#' @param strict Whether only multiples of N can be used, defaults to FALSE
#' @param ... Additional parameters, use "prnt=TRUE" to print to limits
#'
#' @return A list of maximum max_breaks+1 elements with break elements.
#'
#' @examples get_breaks(24, N=12, max_breaks=15)
#' @export
get_breaks = function(limits, N=10, max_breaks=10, int_only=TRUE, strict=FALSE, ...) #checken van 181-600, N=12, 10 max
{
  args = list(...)
  if(length(limits)==1) {xmin=0;xmax=limits} else {xmin=limits[1]; xmax=limits[2]}
  if("prnt" %in% names(args) && args$prnt==T) print(paste0("Range: [",xmin," - ",xmax,"]"))
  xmax = xmax-xmin
  lower_powers = function(x) (xmax/(max_breaks*x)) %>% log10 %>% ceiling %>% ifelse(int_only,0,.)
  upper_powers = function(x) (xmax/x) %>% log10 %>% floor
  intervals = sapply(if(!strict) divisors(N) else N*1:(xmax/N),
                     function(x) x*10**(lower_powers(x) : upper_powers(x))) %>% unlist %>% unique %>% sort
  selected = intervals[xmax/intervals <= max_breaks][1]
  seq(0,floor(xmax/selected)*selected,selected)+ceiling(xmin/selected)*selected
}

#' Get estimate of timepoints for a given survival probability
#'
#' @param sfit A survfit object
#' @param survival The survival probability for which a timepoint estimate is needed. Default is 0.5 (median survival)
#'
#' @return A named list or matrix with elements surv (estimate), lower and upper (confidence interval). The attribute "survival" is set
#' to the argument survival
#' @export
#'
#' @examples fit = cph(Surv(time=time, event = status==2) ~ age + sex, data=lung, x = T, y=T, surv=T)
#' sfit = survfit(fit)
#' sfit2 = survfit(fit, newdata=lung[1:20,])
#'
#' get_survival_estimate(sfit) #get median survival for all data points in the dataset.
#' get_survival_estimate(sfit2) #get median survival for patients 1-20 seperately
get_survival_estimate = function(sfit, survival=0.5)
{
  if ("survfit" %nin% class(sfit))
    stop("sfit must be a survfit object")

  d = dim(sfit$surv)[2]
  results = if (is.null(d)){
    sfit %>% {sapply(c("surv","lower","upper"), function(x) .$time[.[[x]] < survival][1])} %>% as.list
  } else {
    sfit %>% {sapply(1:d, function(i) sapply(c("surv","lower","upper"), function(x) .$time[.[[x]][,i] < survival][1]))} %>% t
  }
  attr(results,"survival") = survival
  results
}

#' Specifies the size of a grid which is as square as possible to fit N objects.
#'
#' @description It will always be a square or or have one row/column more than columns/rows
#'
#' @param N Number of objects
#' @param moreRows Whether there should be more rows than columns if the grid is not square.
#'
#' @return A named list with elements rows and columns specifying the size of the optimal grid.
#'
#' @examples get_square_grid(5)
#' @export
get_square_grid = function(N, moreRows=TRUE) {N %>% sqrt %>% ceiling %>% {list(rows=ifelse(moreRows,.,(N/.) %>% ceiling),
                                                                           columns=ifelse(moreRows,(N/.) %>% ceiling,.))}}

#' Removes any NA from a list
#'
#' @param LIST A list which may contain NA elements
#'
#' @return A list without NA elements
#'
#' @examples rmNA(c(1,NA,5,6))
#' @export
rmNA = function(LIST) {LIST[!is.na(LIST)]}

#' Rounds a number to a specified amount of digits and returns the string value
#'
#' @param dbl The number to be rounded.
#' @param digits The number of digits the number needs to be rounded to.
#'
#' @return A string value of the number rounded to the specified amount of digits.
#'
#' @examples rndDbl(1.26564,digits = 2)
#' @export
rndDbl = function(dbl, digits=3) {sprintf(paste0("%.",digits,"f"),round(dbl, digits))}
