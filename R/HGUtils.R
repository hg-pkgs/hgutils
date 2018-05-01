# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Installs and loads specified packages
#'
#' @param packages A character list of package names
#' @param install Whether to install packages when missing, defaults to TRUE
#'
#' @return NULL
#'
#' @examples install_load_packages(c("ggplot2","dplyr"))
install_load_packages = function(packages,install = TRUE){
  for(package in packages){
    if(!require(package, character.only = TRUE) & install){
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}
install_load_packages(c("devtools","ggplot2","dplyr","numbers","roxygen2"))

#' Sets the working directory automatically to Dropbox folder
#'
#' @param folder String to specify subfolder name
#'
#' @return NULL
#'
#' @examples startup("source_webinterface/")
startup = function(folder = "source_webinterface/"){
  rm(list = ls(pos = .GlobalEnv), envir = .GlobalEnv)
  gc()
  if (Sys.info()["user"]=="hgvandenboorn"){
    setwd(paste0("C:/Users/hgvandenboorn/Dropbox/",folder))
  } else if (Sys.info()["user"]=="H.G. van den Boorn") {
    setwd(paste0("S:/Dropbox/Dropbox/",folder))
  }
}

#' Set the breaks for a graph in nice positions
#'
#' @param limits The limits of the axis. May be a list of 2 elements with lower and upper bounds, or a
#'               single digit (the upperbound, the lowerbound is then assumed to be 0).
#' @param N The `unit` stepsize; The eventual interval size will be multiples of the divisors of N.
#' @param max_breaks Maximum amount of steps
#' @param int_only Whether only integer divisors of N may be used for interval sizes
#'
#' @return A list of maximum `max_breaks` elements with break elements.
#'
#' @examples get_breaks(24, N=12, max_breaks=15)
get_breaks = function(limits, N=10, max_breaks=10, int_only=TRUE) #checken van 181-600, N=12, 10 max
{
  if(length(limits)==1) {xmin=0;xmax=limits} else {xmin=limits[1]; xmax=limits[2]}
  xmax = xmax-xmin
  lower_powers = function(x) (xmax/(max_breaks*x)) %>% log10 %>% ceiling %>% ifelse(int_only,0,.)
  upper_powers = function(x) (xmax/x) %>% log10 %>% floor
  intervals = sapply(divisors(N), function(x) x*10**(lower_powers(x) : upper_powers(x))) %>% unlist %>% unique %>% sort
  selected = intervals[xmax/intervals <= max_breaks][1]
  seq(0,floor(xmax/selected)*selected,selected)+ceiling(xmin/selected)*selected
}

#' Specifies the size of a grid which is as square as possible to fit N objects
#'
#' @param N Number of objects
#'
#' @return A named list with elements `rows` and `columns` specifying the size of the optimal grid.
#'
#' @examples get_cr(5)
get_cr = function(N) {N %>% sqrt %>% ceiling %>% c((N/.) %>% ceiling, .) %>% setNames(c("rows","columns"))}

#' Removes any NA from a list
#'
#' @param LIST A list which may contain NA elements
#'
#' @return A list without NA elements
#'
#' @examples rmNA(c(1,NA,5,6))
rmNA = function(LIST) {LIST[!is.na(LIST)]}

#' Rounds a number to a specified amount of digits and returns the string value
#'
#' @param dbl The number to be rounded.
#' @param digits The number of digits the number needs to be rounded to.
#'
#' @return A string value of the number rounded to the specified amount of digits.
#'
#' @examples rndDbl(1.26564,digits = 2)
rndDbl = function(dbl, digits=3) {sprintf(paste0("%.",digits,"f"),round(dbl, digits))}
