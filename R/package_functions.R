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
  stfu = . %>% suppressPackageStartupMessages %>% suppressMessages %>% suppressWarnings
  for (package in packages) {
    package_exists = stfu(require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
    col = if(package_exists) crayon::black else blue
    cat(col(symbol$arrow_right," ",package), rep(" ",max_n+2-nchar(package)),sep = "")

    succesful = TRUE
    if (!package_exists && install_packages || force_install) {
      cat(blue("\n   ",symbol$continue, "Installing...","\n"), sep = "")
      capture.output({stfu(install.packages(package, verbose = FALSE, quiet = TRUE))})

      capture.output({succesful = stfu(require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))})
      if (!succesful) cat(red("  ",symbol$cross, "Installation failed","\n   "), sep = "") else
        cat(green("   ",symbol$tick, "Installation succesful","\n    "), sep = "")
    }

    if (load_packages && succesful) {
      capture.output(stfu(library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)))
      cat(green(symbol$tick,"Loaded","\n"),sep = "")
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
#'   settings = update_settings(default, ...)
#' }
#'
#' foo(a=2, b=3)
update_settings = function(default, ...) {
  supplied = list(...)
  supplied = supplied[names(supplied)!=""]
  match = intersect(names(default), names(supplied))
  additional = setdiff(names(supplied), names(default))
  if (length(additional) > 0)
    warning(sprintf("Unspecified parameters: %s. Must be one of %s.", frmt(additional), frmt(names(default))))

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
#' @param ... Additional parameters.
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{set_package_imports(skip_prompt=TRUE)}
#' @importFrom magrittr %>%
#' @importFrom stringr str_match str_replace str_replace_all str_split
#' @importFrom utils read.delim packageVersion menu
#' @importFrom cli rule cat_bullet cat_rule
set_package_imports = function(...) {
  settings = update_settings(list(skip_prompt=FALSE, use_version_numbers=TRUE), ...)

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

  pack_version = if(settings$use_version_numbers) depen %>% sapply(. %>% packageVersion %>% format) %>% paste0(depen," (>= ",.,")") else depen
  RVersion = sprintf("R (>= %s)",getRversion())

  cat_rule(left = "Found packages", right=package_name, line = "bar4", col = "dodgerblue4",line_col = "black")
  cat("\n")
  cat_bullet(RVersion, background_col = "dodgerblue4", bullet_col = "white", col="white", bullet = "continue")
  cat_bullet(pack_version, col = "dodgerblue4", bullet_col = "black", bullet = "tick")

  if (settings$skip_prompt || menu(c("Yes","No"),title="\nReplace DESCRIPTION imports?") == 1) {
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
