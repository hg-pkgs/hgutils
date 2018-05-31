#' Compares two vectors of version numbers
#' @description Vectorised version of \code{\link[utils]{compareVersion}}.
#' @inherit utils::compareVersion
#' @seealso \code{\link[utils]{compareVersion}}
#' @export
#' @examples .vec_compareVersion(a=c("1.0","7.2-0"), b=c("1.0-1","7.1-12"))
#' @importFrom utils compareVersion
.vec_compareVersion = function(a, b)
{
  if (length(a)!=length(b) || !length(a)>=1)
    stop(sprintf("Argument 'a' and 'b' must be of the same positive length, but |a|=%s and |b|=%s", length(a), length(b)))

  1:length(a) %>% sapply(. %>% {compareVersion(a[.],b[.])})
}

#' Prepare packages for usage
#'
#' @description Utility function to load and optionally install packages if they are missing. When the function terminates,
#' packages are installed (if necessary), upgraded to the latest version (if necessary) and loaded.
#'
#' @param load_packages Whether to load the selected packages
#' @param install_packages Whether to install the selected packages
#' @param force_install Whether to install packages even if they are installed already
#' @param ... List of package names and other options.
#' @param upgrade Whether to upgrade outdated packages. Defaults to FALSE.
#'
#' @return NULL
#'
#' @examples \dontrun{
#' #package names can be given as a vector or one-by-one
#' use_packages(c('magrittr','dplyr'))
#' use_packages('magrittr','dplyr',install_packages=FALSE)}
#' @export
#' @family developer functions
#' @importFrom utils install.packages capture.output old.packages update.packages
#' @importFrom cli rule symbol
#' @importFrom crayon green red yellow make_style
#' @importFrom dplyr mutate filter
use_packages = function(..., install_packages = TRUE, load_packages = TRUE, force_install = FALSE, upgrade=FALSE) {
  if (!load_packages & !install_packages)
  {warning("Function 'use_packages' not executed: Set argument 'load_packages' and/or 'install_packages' to TRUE."); return()}

  #-- Check for extra arguments in '...' -------
  packages = list(...)
  packages = setdiff(packages, packages[names(packages) != ""])
  settings = update_settings(list(show_title=TRUE), ...)

  #-- Check for invalid package names in '...' -------
  packages = sort(unlist(packages))
  invalid_names = packages[!valid_pkgname(packages)]
  if(length(invalid_names) > 0)
    stop(sprintf("The argument '...' contains the following invalid package names: %s.", invalid_names))

  #-- Define constants -------
  max_n = nchar(packages) %>% max
  blue = make_style("dodgerblue4")
  stfu = . %>% capture.output(type = "message") %>% capture.output(type="output") %>% invisible %>%
         suppressPackageStartupMessages %>% suppressMessages %>% suppressWarnings
  spc = paste0(rep(" ",5),collapse = "")
  outdated_pkgs = old.packages() %>%
             data.frame(stringsAsFactors=FALSE) %>%
             {mutate(., "Installed" = sapply(.$Package, function(x) packageVersion(x) %>% format))} %>%
             filter(.vec_compareVersion(Installed, ReposVer) < 0) %>%
             filter(Package %in% packages)

  if(settings$show_title)
    cat(rule(left = "Loading packages", line = "bar4"),"\n")
  for (package in packages) {
    stfu({package_exists = require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
    cat(blue(symbol$arrow_right," ",package), rep(" ",max_n+2-nchar(package)),sep = "")

    can_load = TRUE
    if (!package_exists && install_packages || force_install) {
      cat("\n")
      cat(blue(spc,symbol$continue, "Installing...","\n"), sep = "")
      stfu({install.packages(package, verbose = FALSE, quiet = TRUE)})

      stfu({can_load = require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      if (!can_load) cat(red(spc,symbol$cross, "Installation failed","\n",spc), sep = "") else
        cat(green(spc,symbol$tick, "Installation succesful","\n",spc), sep = "")
    }
    if (upgrade && package %in% outdated_pkgs$Package) #upgrade package
    {
      sel = outdated_pkgs[outdated_pkgs$Package==package,]
      cat("\n")
      cat(yellow(spc,symbol$continue, sprintf("Upgrading from version [%s] to [%s]...",
                                              sel$Installed, sel$ReposVer),"\n"), sep = "")
      stfu({update.packages(oldPkgs=package, ask=FALSE, verbose = FALSE, quiet = TRUE)})

      current_ver = packageVersion(package)
      if (compareVersion(current_ver, sel$ReposVer) < 0) {
        cat(red(spc,symbol$cross, sprintf("Upgrade failed. Proceeding with version [%s].",current_ver),"\n",spc), sep = "")
      } else {
        cat(green(spc,symbol$tick, sprintf("Upgrade to version [%s] succesful.",current_ver),"\n",spc), sep = "")
      }
    }

    if (load_packages && can_load) {
      stfu({library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      cat(green(symbol$tick,"Loaded","\n"),sep = "")
    }
  }
}

#' Validate a package name
#' @description Naming rule obtained from \emph{'Writing R Extensions'} manual.
#' @param pkg The name of a package. Can be a vector of strings with size of at least 1.
#' If missing, the function returns the regex to validate the names.
#'
#' @return A boolean indicating whether the package name is valid.
#' @export
#' @references \href{https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#The-DESCRIPTION-file}{'Writing R Extensions'} manual.
#' @examples
#' valid_pkgname("hgutils") #valid
#' valid_pkgname("ggplot2") #valid
#' valid_pkgname("pkg2.-1") #invalid
#' @importFrom stringr str_detect
#' @family developer functions
valid_pkgname = function(pkg) {
  regex = "[[:alpha:]][[:alnum:]\\.]*[[:alnum:]]"

  if (missing(pkg))
    return(regex)
  if (!is.character(pkg))
    stop(sprintf("Argument 'pkg' must be of class 'character', but is %s.",frmt(pkg, TRUE)))

  str_detect(pkg,paste0("^",regex,"$"))
}

#' Load commonly used packages
#' @description Utility function to load and install frequently used packages.
#' @inheritDotParams use_packages
#' @details \code{load_common_packages} is a wrapper function for \code{\link{use_packages}}. The supplied package names
#' are available from \code{list_common_packages}.
#'
#' @return NULL
#' @export
#' @family developer functions
use_common_packages = function(...) {
  use_packages(list_common_packages(), ...)
}

#' @export
#' @rdname use_common_packages
list_common_packages = function()
{
  c("devtools", "utils", "readxl", "writexl", "grid", "gridExtra", "gridGraphics", "cli", "installr",
    "reshape2", "scales", "ggplot2", "stringi", "stringr", "formatR", "tibble", "magrittr","dplyr","roxygen2")
}

#' Update default function settings
#' @description Uses ellipsis parameter to update a list of default settings.
#'
#' @param default A named list of default values for settings.
#' @param ... Optional settings to override the default settings.
#'
#' @return The updated list of settings with possible new values.
#' @export
#' @family developer functions
#' @examples
#' foo = function(...) {
#'   default = list(a=1)
#'   settings = update_settings(default, ...)
#' }
#'
#' \dontrun{foo(a=2, b=3)}
update_settings = function(default, ...) {
  supplied = list(...)
  supplied = supplied[names(supplied)!=""]
  match = intersect(names(default), names(supplied))
  additional = setdiff(names(supplied), names(default))
  if (length(additional) > 0)
    warning(sprintf("Ignoring unspecified parameters: %s. Must be one of %s.", frmt(additional), frmt(names(default))))

  default[names(supplied)] = supplied
  default
}

#' Retrieves generic function implementations
#'
#' @description Obtains a list of classes for which the supplied generic function has an implementation.
#'
#' @param generic The name of the generic function.
#' @param ... Optional settings
#'
#' @return A vector with class names for which argument '\code{generic}' has an implementation.
#' @export
#'
#' @examples
#' #get a list of classes which have an implementation for graphics::plot
#' impls = generic_implementations('plot')
#'
#' @note Removes the default generic implementation
#' @importFrom magrittr %>%
#' @importFrom stringr str_match
#' @importFrom utils methods
#' @importFrom methods existsMethod
#' @family developer functions
generic_implementations = function(generic, ...) {
  impls = methods(generic)
  if (!is.character(generic) || !length(generic)==1 || length(impls)==0)
    stop(sprintf("Argument 'generic' is not a valid generic function."))
  settings = update_settings(list(remove_default=TRUE), ...)

  impls %>% sapply(. %>% {str_match(., "^.*\\.(.*)$")[, 2]}) %>% unname %>% rmNA %>%
    if(settings$remove_default) .[. != "default"] else .
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
#' @family developer functions
set_package_imports = function(...) {
  settings = list(skip_prompt=TRUE, use_version_numbers=TRUE, rversion = format(getRversion()))
  settings = update_settings(settings, ...)

  if (!dir.exists("R/") || !file.exists("DESCRIPTION"))
    stop("Working directory not set to an R project folder.")
  desc = readLines("DESCRIPTION") %>% paste0(collapse = "\n")
  package_name = desc %>% {str_match(., "Package:[ ]*(.*?)\n(?:.*\n)+Version:[ ]*(.*?)\n")[-1]} %>% paste(collapse = " ")
  existing_imports = str_match(desc,"Imports:((?:.*\n)+?).*?:")[,2] %>%
    str_replace_all("[ \n]|(?:\\(.*?\\))","") %>% {strsplit(.,",")[[1]]}

  depen = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE) %>%
          sapply(. %>% read.delim(sep = "\n", stringsAsFactors = FALSE, quote="") %>% unlist %>%
                 str_match(., paste0(paste0("#\'[ ]*@import(?:From)?[ ]+(",valid_pkgname(),")|"),
                 paste0("[^#]*?(?:library|require)\\((",valid_pkgname(),")[ ]*[,\\)]|"),
                 paste0("[^#]*?\\((",valid_pkgname(),")::[:]?[^(:)]+|"),
                 "[^#]*?use_packages\\((?:c\\()?([[:alnum:] ,\"\\'\\.]*?)\\).*")) %>% .[, -1] %>% rmNA) %>%
          unlist %>% str_split(",") %>% unlist %>% str_replace_all("[\\'[:space:]\"]","") %>%
          unique %>% {.[valid_pkgname(.)]} %>% sort

  depen = depen[sapply(depen, function(x) suppressWarnings(suppressPackageStartupMessages(
    require(x, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)))) %>% unname]

  pack_version = if(settings$use_version_numbers) depen %>% sapply(. %>% packageVersion %>% format) %>%
                                                  paste0(depen," (>= ",.,")") else depen
  RVersion = sprintf("R (>= %s)",settings$rversion)

  cat_rule(left = "Analyzing package usage", right=package_name, line = "bar4",
           line_col = "dodgerblue4", col="dodgerblue4")
  cat_bullet("R version: ",RVersion, background_col = "dodgerblue4", bullet_col = "white", col="white", bullet = "continue")
  cat_bullet(pack_version, col = "dodgerblue4", bullet_col = "black", bullet = "tick")

  if (settings$skip_prompt || menu(c("Yes","No"),title="\nReplace DESCRIPTION imports?") == 1) {
    desc %>% str_replace("R \\(.*?\\)",RVersion) %>%
             str_replace("(Imports:)(?:.*\n)+?(\n.*?:)",sprintf("\\1\n  %s\\2",paste0(pack_version,collapse=",\n  "))) %>%
             writeLines("DESCRIPTION")

    cat("\n")
    cat_rule(center="DESCRIPTION successfully updated",line="bar1",
             col="white", background_col = "green")
    cat("\n")
    cat_rule(left="Installing/loading dependencies", right=package_name, col="dodgerblue4", line="bar4")
    use_packages(depen, show_title=FALSE)
    cat("\n")
    cat_rule(center="DONE",line="bar1", col="white", background_col = "green")
  } else warning("DESCRIPTION was not adjusted.")
}
