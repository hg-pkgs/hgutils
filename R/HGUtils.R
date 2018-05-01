# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

install_load_packages = function(packages,install = TRUE){
  for(package in packages){
    if(!require(package, character.only = TRUE) & install){
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}
install_load_packages(c("devtools","ggplot2","dplyr","numbers"))

startup = function(){
  rm(list = ls()); gc()
  if (Sys.info()["user"]=="hgvandenboorn"){
    setwd("C:/Users/hgvandenboorn/Dropbox/source_webinterface/")
  } else if (Sys.info()["user"]=="H.G. van den Boorn") {
    setwd("S:/Dropbox/Dropbox/source_webinterface")
  }
}

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

#specifies grid (as square as possible) to fit N objects
get_cr = function(N) N %>% sqrt %>% ceiling %>% c((N/.) %>% ceiling, .) %>% setNames(c("rows","columns"))

rmNA = function(A) A[!is.na(A)]
r = function(x, digits=3) sprintf(paste0("%.",digits,"f"),round(x, digits))
