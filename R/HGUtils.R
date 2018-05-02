# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

devtools::use_package("dplyr","Depends")
devtools::use_package("numbers","Depends")
devtools::use_package("magrittr","Depends")

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
install_load_packages(c("numbers","magrittr","colorspace","RColorBrewer","grid","gridExtra","readxl","writexl","shinydashboard","shinyBS",
                        "shiny","shinyjs","shinyWidgets","shinydashboard","shinyBS","shiny","shinyjs","rms","devtools",
                        "ggthemes","stringr","reshape2","gridGraphics","tidyverse"),install = install)
}

#' Clears the workspace and sets the working directory automatically to the Dropbox folder
#'
#' @param folder String to specify subfolder name
#'
#' @return NULL
#'
#' @examples startup("source_webinterface/")
#' @export
startup = function(folder = "source_webinterface/"){
  rm(list = ls(pos = .GlobalEnv), envir = .GlobalEnv)
  gc()
  if (Sys.info()["user"]=="hgvandenboorn"){
    setwd(paste0("C:/Users/hgvandenboorn/Dropbox/",folder))
    Sys.setenv(JAVA_HOME="C:\\Program Files (x86)\\Java\\jre1.8.0_161")
  } else if (Sys.info()["user"]=="H.G. van den Boorn") {
    setwd(paste0("S:/Dropbox/Dropbox/",folder))
    Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_66/")
  }
}

#' Set the breaks for a graph in nice positions
#'
#' @param limits The limits of the axis. May be a list of 2 elements with lower and upper bounds, or a
#'               single digit (the upperbound, the lowerbound is then assumed to be 0).
#' @param N The unit stepsize; The eventual interval size will be multiples of the divisors of N.
#' @param max_breaks Maximum amount of steps
#' @param int_only Whether only integer divisors of N may be used for interval sizes
#' @param ... Additional parameters, use "prnt=TRUE" to print to limits
#'
#' @return A list of maximum max_breaks elements with break elements.
#'
#' @examples get_breaks(24, N=12, max_breaks=15)
#' @export
get_breaks = function(limits, N=10, max_breaks=10, int_only=TRUE, ...) #checken van 181-600, N=12, 10 max
{
  args = list(...)
  if(length(limits)==1) {xmin=0;xmax=limits} else {xmin=limits[1]; xmax=limits[2]}
  if("prnt" %in% names(args) && args$prnt==T) print(paste0("Range: [",xmin," - ",xmax,"]"))
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
#' @return A named list with elements rows and columns specifying the size of the optimal grid.
#'
#' @examples get_square_grid(5)
#' @export
get_square_grid = function(N) {N %>% sqrt %>% ceiling %>% c((N/.) %>% ceiling, .) %>% setNames(c("rows","columns"))}

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
