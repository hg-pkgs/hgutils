# HGUtils

The goal of HGUtils is to provide the user with a selection of handy utility functions

## Installation

You can install HGUtils from github with:


``` r
# install.packages("devtools")
devtools::install_github("hvdboorn/HGUtils")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
startup("folder") #clears workspace and plot, and loads the folder for the current user.
load_common_packages(install=T) #loads and installs common packages such as ggplot and dplyr
install_load_packages(c("gtable","rms")) #loads and installs user-defined packages

ggplot() + scale_x_continuous(breaks=get_breaks) #creates some nice breaks for plotting

fit = cph(Surv(time=time, event = status==2) ~ age + sex, data=lung, x = T, y=T, surv=T)
sfit = survfit(fit)
get_survival_estimate(sfit) #get median survival for all data points in the dataset.

get_square_grid(6) #returns the size of a nice grid to place 6 objects in, as square as possible

rmNA(c(1,2,NA)) #removes NA from a list
rndDbl(3.14159, digits=3) #rounds a number
```
