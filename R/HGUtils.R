# Some useful keyboard shortcuts for package authoring: Build and Reload Package: 'Ctrl + Shift + B' Check Package: 'Ctrl + Shift + E' Test Package:
# 'Ctrl + Shift + T'

#' Clears the workspace and sets the working directory to specified folder.
#'
#' @param folder String to specify folder name. Registered users can indicate subfolders, others needs to specify complete
#' working directory paths. Use \code{is_registered()} to check whether user registration is enabled
#'
#' @return NULL
#'
#' @examples \dontrun{startup('project_folder/')}
#' @export
#' @importFrom magrittr %>%
#' @family initialization functions
startup = function(folder = NULL) {
    rm(list = ls(pos = .GlobalEnv), envir = .GlobalEnv)
    gc()
    graphics.off()

    results = .is_registered()
    has_results = results %>% nrow == 1

    if (!has_results) {
        if (!is.null(folder) && dir.exists(folder)) {
            setwd(folder)
            print(paste0("Setting the working directory at: '", folder, "'"), quote = F)
        } else warning(paste0("[UNREGISTERED] Directory does not exist: '", ifelse(is.null(folder), "NULL", folder), "'"))
    } else {
        dir = ifelse(!is.null(folder) && dir.exists(folder), folder, paste0(results$location, folder))
        if (dir.exists(dir)) {
            setwd(dir)
            print(paste0("Setting the working directory at: '", dir, "'"), quote = F)
        } else warning(paste0("[REGISTERED] Directory does not exist: '", dir, "'"))
    }
}

#' Installs and loads specified packages
#'
#' @param load_packages Whether to load the selected packages
#' @param install_packages Whether to install the selected packages
#' @param ... A list of package names
#'
#' @return NULL
#'
#' @examples \dontrun{
#' install_load_packages(c('ggplot2','dplyr'))
#' install_load_packages('ggplot2','dplyr',install=FALSE)}
#' @export
#' @family initialization functions
install_load_packages = function(..., load_packages = TRUE, install_packages = TRUE) {
    if (!load_packages & !install_packages)
        warning("Function not executed: not installing or loading any packages. Please set 'load_packages=TRUE' or 'install_packages=TRUE'")
    packages = unlist(list(...))
    for (package in packages) {
        if (!require(package, character.only = TRUE) & install_packages) {
            install.packages(package, dependencies = TRUE)
            if (load_packages)
                library(package, character.only = TRUE)
        }
    }
}

#' Loads (and optionally install) commonly used packages
#'
#' @param load_packages Whether to load the selected packages
#' @param install_packages Whether to install the selected packages
#'
#' @return NULL
#' @export
#' @family initialization functions
load_common_packages = function(load_packages = TRUE, install_packages = TRUE) {
    install_load_packages("tidyverse", "numbers", "magrittr", "colorspace", "RColorBrewer", "grid", "gridExtra", "readxl", "writexl", "devtools", "ggthemes",
        "stringr", "reshape2", "gridGraphics", "scales", "formatR", load_packages = load_packages, install_packages = install_packages)
}


#' Internal functio to check whether the current user has the working directory registered.
#'
#' @param verbose Whether to print the user name and registered working directory.
#' @param return_data Whether to return a boolean or a tibble containing the found user.
#'
#' @return Either a boolean (if return_data is FALSE) or a tibble containing fields 'desc' (computer description), 'usr' (computer user name)
#' and 'location' (working directory base).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
.is_registered = function(verbose = FALSE, return_data = TRUE) {
    current_usr = Sys.info()["user"]
    results = working_dirs %>% filter(usr == current_usr)
    registered = nrow(results) == 1

    if (verbose) {
        if (!registered)
            print(paste0("User '", current_usr, "' is not registered."), quote = F) else print(paste0("User '", current_usr, "' is registered. Default working directory is '", results$location[1], "'"), quote = F)
    }

    if (return_data)
        return(results) else return(registered)
}

#' Checks whether the current user has the working directory registered.
#'
#' @param verbose Whether to print the user name and registered working directory.
#'
#' @return Boolean indicating whether the user is registered or not.
#'
#' @examples is_registered()
#' @export
#' @family initialization functions
is_registered = function(verbose = FALSE) {
    .is_registered(verbose = verbose, return_data = FALSE)
}

#' Nice plotting axis breaks
#' @description Set the breaks for a graph in nice positions.
#'
#' @param limits The limits of the axis. May be a list of 2 elements with lower and upper bounds, or a
#'               single digit (the upperbound, the lowerbound is then assumed to be 0).
#' @param N The unit stepsize; The eventual interval size will be multiples of the divisors of N. Defaults to 10
#' @param max_breaks Maximum amount of steps, defaults to 10
#' @param int_only Whether only integer divisors of N may be used for interval sizes, default to TRUE
#' @param strict Whether only multiples of N can be used, defaults to FALSE
#' @param ... Additional parameters, use 'prnt=TRUE' to print to limits
#'
#' @return A list of maximum \code{max_breaks+1} elements with intervals.
#'
#' @examples get_breaks(24, N=12, max_breaks=15)
#' @export
#' @importFrom magrittr %>%
#' @importFrom numbers divisors
#' @family break functions
get_breaks = function(limits, N = 10, max_breaks = 10, int_only = TRUE, strict = FALSE, ...) {
    args = list(...)
    if (length(limits) == 1) {
        xmin = 0
        xmax = limits
    } else {
        xmin = limits[1]
        xmax = limits[2]
    }
    if ("prnt" %in% names(args) && args$prnt == T)
        print(paste0("Range: [", xmin, " - ", xmax, "]"))
    xmax = xmax - xmin
    lower_powers = function(x) (xmax/(max_breaks * x)) %>% log10 %>% ceiling %>% ifelse(int_only, 0, .)
    upper_powers = function(x) (xmax/x) %>% log10 %>% floor
    intervals = sapply(if (!strict)
        divisors(N) else N * 1:(xmax/N), function(x) x * 10^(lower_powers(x):upper_powers(x))) %>% unlist %>% unique %>% sort
    selected = intervals[xmax/intervals <= max_breaks][1]
    seq(0, floor(xmax/selected) * selected, selected) + ceiling(xmin/selected) * selected
}

#' Nice plotting axis breaks
#' @description This makes usage easier in plot functions as the limits may not be always be known before plotting.
#' @param ... See \code{\link{get_breaks}} for possible parameters.
#'
#' @return A \code{\link{get_breaks}} function with filled-in parameters which expects limits.
#'
#' @export
#' @examples ggplot() + scale_x_continuous(breaks = plot_breaks(N=12, max_breaks=15))
#' @family break functions
plot_breaks = function(...) {
    function(x) get_breaks(x, ...)
}

#' Get estimate of timepoints for a given survival probability
#'
#' @param sfit A survfit object
#' @param survival The survival probability for which a timepoint estimate is needed. Default is 0.5 (median survival)
#'
#' @return A named list or matrix with elements surv (estimate), lower and upper (confidence interval). The attribute 'survival' is set
#' to the argument survival
#' @export
#'
#' @examples fit = cph(Surv(time=time, event = status==2) ~ age + sex, data=lung, x = T, y=T, surv=T)
#' sfit = survfit(fit)
#' sfit2 = survfit(fit, newdata=lung[1:20,])
#'
#' get_survival_estimate(sfit) #get median survival for all data points in the dataset.
#' get_survival_estimate(sfit2) #get median survival for patients 1-20 seperately
#' @importFrom magrittr %>%
get_survival_estimate = function(sfit, survival = 0.5) {
    if ("survfit" %nin% class(sfit))
        stop("sfit must be a survfit object")

    d = dim(sfit$surv)[2]
    results = if (is.null(d)) {
        sfit %>% {
            sapply(c("surv", "lower", "upper"), function(x) .$time[.[[x]] < survival][1])
        } %>% as.list
    } else {
        sfit %>% {
            sapply(1:d, function(i) sapply(c("surv", "lower", "upper"), function(x) .$time[.[[x]][, i] < survival][1]))
        } %>% t
    }
    attr(results, "survival") = survival
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
#' @importFrom magrittr %>%
get_square_grid = function(N, moreRows = TRUE) {
    N %>% sqrt %>% ceiling %>% {
        list(rows = ifelse(moreRows, ., (N/.) %>% ceiling), columns = ifelse(moreRows, (N/.) %>% ceiling, .))
    }
}

#' Removes any NA from a list
#'
#' @param LIST A list which may contain NA elements
#'
#' @return A list without NA elements
#'
#' @examples rmNA(c(1,NA,5,6))
#' @export
rmNA = function(LIST) {
    LIST[!is.na(LIST)]
}

#' Rounds a number to a specified amount of digits and returns the string value
#'
#' @param dbl The number to be rounded.
#' @param digits The number of digits the number needs to be rounded to.
#'
#' @return A string value of the number rounded to the specified amount of digits.
#'
#' @examples rndDbl(1.26564,digits = 2)
#' @export
rndDbl = function(dbl, digits = 3) {
    sprintf(paste0("%.", digits, "f"), round(dbl, digits))
}
