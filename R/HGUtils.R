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
#' @param ... List of package names
#'
#' @return NULL
#'
#' @examples \dontrun{
#' use_packages(c('magrittr','dplyr'))
#' use_packages('magrittr','dplyr',install_packages=FALSE)}
#' @export
#' @family initialization functions
#' @importFrom utils install.packages
#' @importFrom cli rule symbol
#' @importFrom crayon green red make_style
use_packages = function(..., install_packages = TRUE, load_packages = TRUE, force_install = FALSE) {
  if (!load_packages & !install_packages)
    {warning("Function not executed: not installing or loading any packages. Set load_packages=TRUE or install_packages=TRUE"); return()}

  #-- Check for extra arguments in '...' -------
  packages = list(...)
  packages = setdiff(packages, packages[names(packages) != ""])
  settings = update_settings(list(show_title=TRUE), ...)

  #-- Check for invalid package names in '...' -------
  packages = sort(unlist(packages))
  invalid_names = packages[!is_valid_pkgname(packages)]
  if(length(invalid_names) > 0)
    stop(sprintf("The argument '...' contains the following invalid package names: %s.", invalid_names))

  if(settings$show_title)
  cat(rule(left = "Loading packages", line = "bar4"),"\n")

  max_n = nchar(packages) %>% max
  blue = make_style("dodgerblue4")
  for (package in packages) {
    cat(symbol$arrow_right," ",package, rep(" ",max_n+2-nchar(package)),sep = "")
    package_exists = suppressWarnings(require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

    succesful = TRUE
    if (!package_exists && install_packages || force_install) {
      cat(blue(symbol$continue, " Installing...","\n"), sep = "")
      suppressMessages(install.packages(package, verbose = FALSE, quiet = TRUE))

      succesful = suppressWarnings(require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
      if (succesful) cat(green(symbol$tick, " Succesfully installed.","\n"), sep = "") else
        cat(red(symbol$cross, " Installation failed.","\n"), sep = "")
    }

    if (load_packages && succesful) {
      suppressPackageStartupMessages(library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
      cat(green(symbol$tick,"Loaded","\n"),sep = "")#cat_bullet("  Loaded.", bullet = "tick", bullet_col = "green", col="green")
    }
  }
}

#' Validate a package name
#' @description Naming rule obtained from \emph{'Writing R Extensions'} manual.
#' @param pkg The name of a package. May be a vector or a single string.
#'
#' @return A boolean indicating whether the package name is valid.
#' @export
#' @references \href{https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#The-DESCRIPTION-file}{'Writing R Extensions'} manual.
#' @examples
#' is_valid_pkgname("hgutils") #valid
#' is_valid_pkgname("ggplot2") #valid
#' is_valid_pkgname("pkg2.-1") #invalid
#' @importFrom stringr str_detect
#' @family initialization functions
is_valid_pkgname = function(pkg) {
  if (!is.character(pkg))
    stop(sprintf("Argument 'pkg' must be of class 'character', but is %s.",frmt(pkg, TRUE)))
  str_detect(pkg,"^[[:alpha:]][[:alnum:]\\.]*[[:alnum:]]$")
}

#' Load commonly used packages
#' @description \code{load_common_packages} loads (and optionally install) commonly used packages.
#' A list of used packages is available from \code{common_packages}.
#' @inheritDotParams use_packages
#'
#' @return NULL
#' @export
#' @family initialization functions
use_common_packages = function(...) {
  use_packages(list_common_packages(), ...)
}

#' A list of common packages
#'
#' @return A vector of package names
#' @export
#' @rdname load_common_packages
list_common_packages = function()
{
  c("devtools", "utils", "readxl", "writexl", "grid", "gridExtra", "gridGraphics", "cli",
  "reshape2", "scales", "ggplot2", "stringi", "stringr", "formatR", "tibble", "magrittr","dplyr","roxygen2")
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
#' @param limits The limits of the axis. May be a vector of 2 elements with lower and upper bounds, or a
#'               single digit (the upperbound, the lowerbound is then assumed to be 0).
#' @param N The unit stepsize; The eventual interval size will be multiples of the divisors of N. Defaults to 10.
#' @param max_breaks Maximum amount of steps, defaults to 10.
#' @param int_only Whether only integer divisors of 'N' may be used for interval sizes, defaults to TRUE.
#' @param strict Whether only multiples of N can be used, defaults to FALSE.
#' @param ... Additional parameters.
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

  options = list(prnt = FALSE) %>% update_settings(...)
  if (options$prnt)
      cat(paste0("input range: [", xmin, " - ", xmax, "]"))

  xmax = xmax - xmin
  lower_powers = function(X) (xmax/(max_breaks * X)) %>% log10 %>% ceiling %>% ifelse(int_only, 0, .)
  upper_powers = function(X) (xmax/X) %>% log10 %>% floor
  intervals = sapply(if (!strict)
      divisors(N) else N * 1:(xmax/N), function(X) X * 10^(lower_powers(X):upper_powers(X))) %>% unlist %>% unique %>% sort
  selected = intervals[xmax/intervals <= max_breaks][1]
  #replace 0 by -xmin*selected
  sq = seq(0, ifelse(include_upper,ceiling(xmax/selected),floor(xmax/selected)) * selected, selected) + ceiling(xmin/selected) * selected

  if (options$prnt)
    cat(paste0(sprintf("\nSelected: %s.\nSequence: %s.",selected,frmt(sq))))

  sq
}

#' Nice plotting axis breaks
#'
#' @description This makes usage easier in \pkg{ggplot2} as the limits may not be always be known before plotting.
#'
#' @return A \code{\link{get_breaks}} function with filled-in parameters which expects limits.
#'
#' @export
#' @examples \dontrun{ggplot() + scale_x_continuous(breaks = ggplot_breaks(N=12, max_breaks=15))}
#' @family axis break functions
#' @inheritDotParams get_breaks
ggplot_breaks = function(...) {
  function(X) get_breaks(X, ...)
}

#' Separate values
#' @description Separates real numbers from one another with a minimum distance, bounded by lower and upper values and constraint to be as
#' close as possible to their original values.
#'
#' @param X A numerical vector of real numbers.
#' @param distance The minimum distance between subsequent numbers. Must be a scalar or vector of size |X|.
#' @param min,max The lower and upper bounds
#' @details The output vector has the following properties. For all elements \code{e_i}, \code{min <= e_i <= max}.
#' For the distance \code{D} between \code{e_i} and \code{e_(i+1)}, \code{D >= max(d_i, d_(i+1))}. And finally, the distance
#' between \code{e_i} and \code{X_i} is minimized for all \code{e_i}.
#'
#' @return A numerical vector with the same length as \code{X}, with numbers bounded by min and max, close to their original values and
#'         with the minimum allowed distance between subsequent values.
#' @export
#'
#' @examples separate_values(c(0.3,0.4,0.41), distance = 0.05, min = 0, max = 1)
#' @importFrom limSolve lsei
separate_values = function(X, distance = 0.05, min = 0, max = 1) {
  if (!is.vector(X) || !is.numeric(X))
      stop(sprintf("Argument 'X' must be a numerical vector of real numbers, but is %s.",frmt(X)))
  if (!is.numeric(distance))
      stop(sprintf("Argument 'distance' must be numeric, but is %s.", frmt(distance)))
  if (max <= min)
      stop(sprintf("Argument 'max' must be strictly larger than 'min', but 'min'=%s and 'max'=%s.", frmt(min), frmt(max)))
  if (!length(distance) %in% c(1,length(X)))
      stop(sprintf("Argument 'distance' must be of length 1 or |X|, but is of length %s.", frmt(length(distance))))

  N = length(X)
  if (length(distance) == 1) distance = rep(distance, N)
  distance = distance[order(X)]
  X = X[order(X)]
  distance = if(N>=2) pmax(distance[1:(N-1)], distance[2:N]) else numeric(0)

  if ((max - min) < sum(distance))
    stop(sprintf(paste0("The total distance constraint is %s, but the space between 'min' and 'max' is only %s.",
                 "\nExtend either the bounds or limit the distance constraint."),
                 frmt(sum(distance)), frmt(max-min)))

  #constraint for limits [min-max]
  upper = matrix(nrow = 2 * N, ncol = N, 0)
  for (i in 1:N) {
      upper[(i * 2 - 1):(i * 2), i] = c(1, -1)
  }

  if(N > 1) {
    #constraint for distances between elements
    lower = matrix(nrow = N - 1, ncol = N, 0)
    for (i in 1:(N - 1)) {
        lower[i, i:(i + 1)] = c(-1, 1)
    }
  } else {lower = NULL}
  H = c(rep(c(min, -max), N), distance)  #solution vectors

  # constraint on limits, spacing and distance to original value
  lsei(A = diag(N), B = X, G = rbind(upper, lower), H = H, type = 2)$X
}

#' Specifies a square grid which fits N objects.
#'
#' @description The resulting grid will be of size \code{(a*a)} or \code{(a*(a+1))} where \code{a} is an integer.
#' It will therefore always be a square or or have one row/column more than columns/rows
#'
#' @param N Number of objects
#' @param moreRows Whether there should be more rows than columns if the grid is not square. Defaults to more rows.
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

#' Removes any NA from a vector
#'
#' @param vec A vector which may contain NA elements
#'
#' @return A vector without NA elements
#'
#' @examples rmNA(c(1,NA,5,6))
#' @export
rmNA = function(vec) {
  vec[!is.na(vec)]
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

#' Update default function settings
#' @description Uses ellipsis parameter to update a list of default settings.
#'
#' @param default A named list of default values for settings.
#' @param ... Optional settings. to override the default settings.
#'
#' @return The updated list of settings. with possible new values.
#' @export
#'
#' @examples
#' foo = function(...) {
#'   default = list(a=1)
#'   update_settings(default, ...)
#' }
#'
#' foo(a=2, b=3)
update_settings = function(default, ...) {
  supplied = list(...)
  supplied = supplied[names(supplied)!=""]
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
#' @examples impls = generic_implementations('plot')
#' @importFrom magrittr %>%
#' @importFrom stringr str_match
#' @importFrom utils methods
#' @importFrom methods existsMethod
generic_implementations = function(generic) {
  if (!is.character(generic) || !length(generic)==1 || !existsMethod(generic))
    stop(sprintf("Argument 'generic' is not a valid generic function."))
  methods(generic) %>% sapply(. %>% {str_match(., "^.*\\.(.*)$")[, 2]}) %>%
    unname %>% rmNA %>% .[. != "default"]
}

#' Creates a nice string representation of a variable.
#'
#' @param x The variable for which a string representation is created.
#' @param show_class Whether to show the class of 'x'. Defaults to FALSE.
#'
#' @return A character vector with the string representation of 'x'.
#' @export
#' @examples frmt(c(1,2,3))
frmt = function(x, show_class = FALSE) {
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
#' @param skip_prompt Whether to skip the prompt or to edit the DESCRIPTION file directly.
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{set_package_imports(skip_prompt=TRUE)}
#' @importFrom magrittr %>%
#' @importFrom stringr str_match str_replace str_replace_all str_split
#' @importFrom utils read.delim packageVersion menu
#' @importFrom cli rule cat_bullet cat_rule
set_package_imports = function(skip_prompt = FALSE) {
  if (!dir.exists("R/") || !file.exists("DESCRIPTION"))
      stop("Working directory not set to an R project folder.")
  desc = readLines("DESCRIPTION") %>% paste0(collapse = "\n")
  package_name = desc %>% {str_match(., "Package:[ ]*(.*?)\n(?:.*\n)+Version:[ ]*(.*?)\n")[-1]} %>% paste(collapse = " ")
  existing_imports = str_match(desc,"Imports: ((?:.*\n)+)RoxygenNote:")[,2] %>%
    str_replace_all("[ \n]|(\\(.*?\\))","") %>% {strsplit(.,",")[[1]]}

  depen = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE) %>%
          sapply(. %>% read.delim(sep = "\n", stringsAsFactors = FALSE) %>% unlist %>%
          str_match(., paste0("^#\'[ ]+@import(?:.*?From)? ([^ ]*).*$|",
                              "^.*(?:library|require)\\((.*?) *[,\\)].*$|",
                              "^.*?[^[:alpha:]](.*?)::[:]?.*$|",
                              "^.*use_packages\\((?:c\\()?(.*?)\\).*$")) %>% .[, -1] %>% rmNA) %>%
          unlist %>% str_split(",") %>% unlist %>% str_replace_all("[\\'[:space:]]","") %>% unique %>%
          {.[is_valid_pkgname(.)]} %>% sort

  depen = depen[sapply(depen, function(x) suppressWarnings(suppressPackageStartupMessages(
          require(x, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)))) %>% unname]

  pack_version = depen %>% sapply(. %>% packageVersion %>% format) %>% paste0(depen," (>= ",.,")")
  RVersion = sprintf("R (>= %s)",getRversion())

  cat_rule(left = "Found packages", right=package_name, line = "bar4", col = "dodgerblue4",line_col = "black")
  cat("\n")
  cat_bullet(RVersion, background_col = "dodgerblue4", bullet_col = "white", col="white", bullet = "continue")
  cat_bullet(pack_version, col = "dodgerblue4", bullet_col = "black", bullet = "tick")

  if (skip_prompt || menu(c("Yes","No"),title="\nReplace DESCRIPTION imports?") == 1) {
      desc %>%
      str_replace("(R \\(.*?\\))",RVersion) %>%
      str_replace("(?s)(Imports: )(.*?)(\n[[:alpha:]]+:)",sprintf("\\1\n  %s\\3",paste0(pack_version,collapse=",\n  "))) %>%
      writeLines("DESCRIPTION")

    cat("\n")
    cat_rule(center="DESCRIPTION successfully updated.",line="bar1",
               col="white", background_col = "green")
    cat("\n")
    cat_rule(left="Installing/loading dependencies", right=package_name, col="dodgerblue4", line="bar4")
    use_packages(depen, show_title=FALSE)
  } else warning("DESCRIPTION was not adjusted.")
}
