#' Clears the workspace and sets the working directory to specified folder.
#'
#' @param folder String to specify folder name. Registered users can indicate subfolders, others needs to specify complete
#' working directory paths. Use \code{is_registered()} to check whether user registration is enabled
#' @param rm Whether to remove objects in the current environment. Defaults to TRUE.
#'
#' @return NULL
#'
#' @examples \dontrun{startup('project_folder/')}
#' @export
#' @importFrom magrittr %>%
#' @importFrom grDevices graphics.off
#' @family initialization functions
startup = function(folder = NULL, rm = TRUE) {
    if (rm) {
        rm(list = ls(pos = .GlobalEnv), envir = .GlobalEnv)
        gc()
        graphics.off()
    }

    results = .is_registered()
    has_results = results %>% nrow == 1

    if (!has_results) {
        if (!is.null(folder) && dir.exists(folder)) {
            setwd(folder)
            message(paste0("Setting the working directory at: '", folder, "'"), quote = F)
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
#' @param force_install Whether to install packages even if they are installed already
#' @param ... A list of package names
#'
#' @return NULL
#'
#' @examples \dontrun{
#' install_load_packages(c('magrittr','dplyr'))
#' install_load_packages('magrittr','dplyr',install_packages=FALSE)}
#' @export
#' @family initialization functions
#' @importFrom utils install.packages
install_load_packages = function(..., install_packages = TRUE, load_packages = TRUE, force_install = FALSE) {
    if (!load_packages & !install_packages)
        warning("Function not executed: not installing or loading any packages. Set load_packages=TRUE or install_packages=TRUE")
    packages = unlist(list(...))
    for (package in packages) {
        package_exists = suppressWarnings(require(package, character.only = TRUE, quietly = TRUE))

        if (!package_exists && install_packages || force_install) {
            suppressWarnings(install.packages(package, dependencies = TRUE, verbose = FALSE, quiet = TRUE))
            message(sprintf("- Installed '%s'", package))
        }

        if (load_packages) {
          suppressPackageStartupMessages(
            library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
            )
          message(sprintf("Loaded '%s'", package))
        }
    }
}

#' Loads (and optionally install) commonly used packages
#'
#' @param load_packages Whether to load the selected packages
#' @param install_packages Whether to install the selected packages
#' @param force_install Whether to install packages even if they are installed already
#'
#' @return NULL
#' @export
#' @family initialization functions
load_common_packages = function(load_packages = TRUE, install_packages = TRUE, force_install = FALSE) {
    install_load_packages("devtools", "utils", "readxl", "writexl", "grid", "gridExtra", "gridGraphics",
                          "reshape2", "scales", "ggplot2", "stringr", "formatR", "tibble", "magrittr","dplyr",
                          load_packages = load_packages, install_packages = install_packages, force_install = force_install)
}


#' Internal function to check whether the current user has the working directory registered.
#'
#' @param verbose Whether to print the user name and registered working directory.
#' @param return_data Whether to return a boolean or a tibble containing the found user.
#'
#' @return Either a boolean (if return_data is FALSE) or a tibble containing fields 'desc' (computer description),
#' 'usr' (computer user name) and 'location' (working directory base).
.is_registered = function(verbose = FALSE, return_data = TRUE) {
    current_usr = Sys.info()["user"]
    results = working_dirs[working_dirs$usr == current_usr, ]
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
#' @param include_upper Whether the resulting upperbound should go past the upper limit. Defaults to TRUE.
#'
#' @return A list of maximum \code{max_breaks+1} elements with intervals.
#'
#' @examples get_breaks(24, N=12, max_breaks=15)
#' @export
#' @importFrom magrittr %>%
#' @importFrom numbers divisors
#' @family break functions
get_breaks = function(limits, N = 10, max_breaks = 10, int_only = TRUE, strict = FALSE, include_upper=TRUE, ...) {
    if (length(limits) == 1) {
        xmin = 0
        xmax = limits
    } else {
        xmin = limits[1]
        xmax = limits[2]
    }

    options = list(prnt = FALSE) %>% retrieve_ellipsis(...)
    if (options$prnt)
        print(paste0("input range: [", xmin, " - ", xmax, "]"))


    xmax = xmax - xmin
    lower_powers = function(X) (xmax/(max_breaks * X)) %>% log10 %>% ceiling %>% ifelse(int_only, 0, .)
    upper_powers = function(X) (xmax/X) %>% log10 %>% floor
    intervals = sapply(if (!strict)
        divisors(N) else N * 1:(xmax/N), function(X) X * 10^(lower_powers(X):upper_powers(X))) %>% unlist %>% unique %>% sort
    selected = intervals[xmax/intervals <= max_breaks][1]
    sq = seq(0, ifelse(include_upper,ceiling(xmax/selected),floor(xmax/selected)) * selected, selected) + ceiling(xmin/selected) * selected

    if (options$prnt) {
        print(paste0("Selected: ", selected, ". Sequence: "))
        print(sq)
    }

    return(sq)
}

#' Nice plotting axis breaks
#'
#' @description This makes usage easier in plot functions as the limits may not be always be known before plotting.
#'
#' @return A \code{\link{get_breaks}} function with filled-in parameters which expects limits.
#'
#' @export
#' @examples
#' \dontrun{library(ggplot2)
#' ggplot() + scale_x_continuous(breaks = plot_breaks(N=12, max_breaks=15))}
#' @family break functions
#' @inheritDotParams get_breaks
#' @importFrom ggplot2 ggplot scale_x_continuous
plot_breaks = function(...) {
    function(X) get_breaks(X, ...)
}

#' Seperate values
#' @description Seperates real numbers from one another with a minimum distance, bounded by lower and upper values and constraint to be as
#' close as possible to their original values.
#'
#' @param X A sorted numerical vector of real numbers.
#' @param distance The minimum distance between subsequent numbers
#' @param min The minimum value of a number
#' @param max The maximum value of a number
#'
#' @return A numerical vector with the same length as x, with numbers bounded by min and max, close to their original values and
#'         with the minimum allowed distance between subsequent values.
#' @export
#'
#' @examples seperate_values(c(0.3,0.4,0.41), distance = 0.05, min = 0, max = 1)
#' @importFrom limSolve lsei
seperate_values = function(X, distance = 0.05, min = 0, max = 1) {
    if (!is.vector(X) || !is.numeric(X) || length(X) <= 1)
        stop("Argument 'X' must be a numerical vector of real numbers with |X| > 1.")
    if (max < min)
        stop("Argument 'max' must be larger than 'min', but 'min'=%s and 'max'=%s.", val_str(min), val_str(max))
    if ((max - min)/distance < length(X))
        stop(paste0("With the specified distance, there is space between min and max of ", (max - min)/distance, " elements", ", however x contains ",
            length(X), " elements. Choose a larger distance or a wider range."))
    if (is.unsorted(X))
        stop("x must be sorted.")

    N = length(X)
    upper = matrix(nrow = 2 * N, ncol = N, 0)
    for (i in 1:N) {
        upper[(i * 2 - 1):(i * 2), i] = c(1, -1)
    }  #constraint for limits [min-max]
    lower = matrix(nrow = N - 1, ncol = N, 0)
    for (i in 1:(N - 1)) {
        lower[i, i:(i + 1)] = c(-1, 1)
    }  #constraint for distances between elements
    H = c(rep(c(min, -max), N), rep(distance, N - 1))  #solution vectors

    # constraint on limits, spacing and distance to original value
    return(lsei(A = diag(N), B = X, G = rbind(upper, lower), H = H, type = 2)$X)
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
#' @examples rnd_dbl(1.26564,digits = 2)
#' @export
rnd_dbl = function(dbl, digits = 3) {
    sprintf(paste0("%.", digits, "f"), round(dbl, digits))
}

#' Update default parameters with ellipsis
#'
#' @param default A named list of default values for parameters
#' @param ... Optional parameters to override the default parameters.
#'
#' @return The updated list of parameters with possible new values.
#' @export
#'
#' @examples foo = function(...) {
#'   default = list(a=1)
#'   retrieve_ellipsis(default, ...)
#' }
retrieve_ellipsis = function(default, ...) {
    supplied = list(...)
    match = intersect(names(default), names(supplied))

    default[names(supplied)] = supplied
    default
}

#' Retrieves generic function implementation
#'
#' @param generic A string with the name of the generic function.
#'
#' @return A vector of class names for which argument 'generic' provides an implementation.
#' @export
#'
#' @examples impls('print')
#' @importFrom magrittr %>%
#' @importFrom stringr str_match
#' @importFrom utils methods
impls = function(generic) {
    methods(generic) %>% sapply(. %>% {
        str_match(., "^.*\\.(.*)$")[, 2]
    }) %>% unname %>% .[. != "default"]
}

#' Creates a nice string representation of a variable.
#'
#' @param x The variable for which a string representation is created.
#' @param show_class Whether to show the class of 'x'. Defaults to FALSE.
#'
#' @return A character vector with the string representation of 'x'.
#' @export
#' @examples val_str(c(1,2,3))
val_str = function(x, show_class = FALSE) {
    text = if (length(x) == 0L) {
        "{}"
    } else if (length(x) == 1) {
        sprintf("'%s'", x)
    } else if (is.atomic(x)) {
        sprintf("['%s']", paste0(sort(x), collapse = "','"))
    } else {
        sprintf("{'%s'}", paste0(x, collapse = "','"))
    }

    if (show_class)
        sprintf("%s (class: %s)", text, class(x)) else text
}

#' Creates a list of necessary imports for the DESCRIPTION file.
#'
#' @param skip_prompt Whether to skip the prompt or to edit the DECRIPTION file directly.
#'
#' @return NULL
#' @export
#'
#' @examples set_DESCRIPTION_imports()
#' @importFrom magrittr %>%
#' @importFrom stringr str_match str_replace str_replace_all str_split str_subset
#' @importFrom utils read.delim packageVersion
set_DESCRIPTION_imports = function(skip_prompt = FALSE) {
    if (!dir.exists("R/") || !file.exists("DESCRIPTION")) {
        warning("Working directory not set to an R project folder.")
        return()
    }

    depen = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE) %>%
            sapply(. %>% read.delim(sep = "\n", stringsAsFactors = FALSE) %>% unlist %>%
            str_match(., paste0("^#\'[ ]+@import(?:.*?From)? ([^ ]*).*$|",
                                "^.*(?:library|require)\\((.*?) *[,\\)].*$|",
                                "^.*?[^[:alpha:]](.*?)::[:]?.*$|",
                                "^.*install_load_packages\\((?:c\\()?(.*?)\\).*$")) %>% .[, -1] %>% rmNA) %>%
            unlist %>% str_split(",") %>% unlist %>% str_replace_all("[\\'[:space:]]","") %>%
            str_subset("^[a-zA-Z0-9\\.]*$") %>% unique %>% sort

    depen = depen[sapply(depen, function(x) suppressWarnings(suppressPackageStartupMessages(
      require(x, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)))) %>% unname]

    pack_version = depen %>% sapply(. %>% packageVersion %>% format) %>% paste0(depen," (>= ",.,")")

    message(sprintf("The following packages have been found in the source code:\n%s", paste0("* ", pack_version, collapse = "\n")))

    if (skip_prompt || readline("\n\nReplace DESCRIPTION imports (Y/N)? ") == "Y") {
      readLines("DESCRIPTION") %>% paste0(collapse = "\n") %>%
        str_replace("(R \\(.*?\\))",sprintf("R (>= %s)",getRversion())) %>%
        str_replace("(?s)(Imports: )(.*?)(\n[[:alpha:]]+:)",sprintf("\\1\n  %s\\3",paste0(pack_version,collapse=",\n  "))) %>%
        writeLines("DESCRIPTION")
    } else {
        warning("DESCRIPTION was not adjusted.")
    }
}
