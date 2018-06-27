#' Make packages ready for usage
#'
#' @description Utility function to load and optionally install packages if they are missing. When the function terminates,
#' packages are installed (if necessary), upgraded to the latest version (if necessary) and loaded.
#'
#' @param install_packages Whether to install the selected packages.
#' @param force_install Whether to install packages even if they are installed already.
#' @param upgrade Whether to upgrade outdated packages. Defaults to \code{FALSE}.
#' @param ... List of additional package names.
#'
#' @details
#' \code{load_packages} optionally installs, upgrades and attaches packages to the work space for a list of specified packages.
#' \code{use_common_packages} is a convenient utility which does the same for a prespecified list of common package names
#' defined in \code{list_common_packages}. The dots parameter is passed on to \code{load_packages}.
#'
#' \code{load_package_collection} loads a collection of useful packages, identified by a collection name. This is used to
#' load similar packages for specific programming tasks. The possible collections are stated in \code{list_package_collections}
#'
#'
#' @return Returns invisibly a list with additional package information and results of installing/upgrading and loading.
#' @seealso \code{\link{load_package_collection}} for loading packages collections.
#' \code{\link[utils]{install.packages}} for installation of new packages,
#' \code{\link[utils]{update.packages}} for updating outdated packages,
#' \code{\link[base]{library}} for load and attaching packages.
#'
#' @examples \dontrun{
#' # Package names can be given as a vector or one-by-one
#' load_packages(c('magrittr','dplyr'))
#' load_packages('magrittr','dplyr',install_packages=FALSE)
#'
#' # These are equivalent
#' load_common_packages()
#' load_packages(list_common_packages())
#'
#' #load package collection "processing"
#' #installs/loads dplyr, lubridate, magrittr, mice, stringr, tibble and utils
#' load_package_collection("processing")}
#' @export
#' @family developer functions
#'
#' @importFrom utils install.packages capture.output old.packages update.packages compareVersion installed.packages
#' @importFrom cli rule symbol cat_bullet
#' @importFrom crayon green red yellow make_style underline
#' @importFrom dplyr mutate filter
#' @importFrom magrittr %>% %<>%
#' @importFrom stringr str_wrap
load_packages = function(..., install_packages = TRUE, force_install = FALSE, upgrade=FALSE) {

  #-- Check for extra arguments in '...' -------
  packages = list(...) %>% unlist
  duplicates = .pkg_duplicated(packages)

  #-- Check for invalid package names in '...' -------
  packages = packages %>% unique %>% sort
  invalid_names = packages[!valid_pkgname(packages)]
  if(length(invalid_names) > 0)
    stop(sprintf("The argument '...' contains the following invalid package names: %s.", invalid_names))

  #-- Define constants -------
  blue = make_style("dodgerblue4")
  spaces = paste0(rep(" ",100),collapse = "")
  SUCCESS=      "Loaded succesfully:   "
  UPGRADED=     "Upgraded succesfully: "
  UPGRADE_FAIL= "Upgraded failed:      "
  FAILED =      "Loading failed:       "
  REDUNDANT =   "Redundant packages:   "
  DUPLICATED =  "Duplicate packages:   "
  exdent = nchar(SUCCESS) + 2
  redundant = redundant_packages(packages)
  success = c(); fail=c(); upgraded=c(); upgrade_fail=c()
  progressbar = progressbar(format="\u258f[\u2589][][\u2581]\u2595",refresh = 0.3, width = 20, n_iterations = length(packages))

  inst = installed.packages()
  outdated_pkgs = old.packages(instPkgs = inst[row.names(inst) %in% packages,, drop=FALSE]) %>% data.frame(stringsAsFactors=FALSE)
  outdated_pkgs$Installed = sapply(outdated_pkgs$Package, function(x) packageVersion(x) %>% format)
  outdated_pkgs %<>% filter(numeric_version(.$Installed) < numeric_version(.$ReposVer))

  name = paste("hgutils", packageVersion("hgutils"))
  cat(rule(left = sprintf("Loading packages (total: %s packages)",length(packages)), right = blue(name), line = "bar4"),"\n")

  data_acc = data.frame(package=character(),action=character(),result=logical()); acc_i = 1
  for (p in 1:length(packages)) {
    package = packages[p]

    progressbar = update(progressbar, p)
    cat("\r",render(progressbar, show_iteration = TRUE),"loading",package,spaces)
    stfu({package_exists = require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})

    will_install = !package_exists && install_packages || force_install
    will_upgrade = upgrade && package %in% outdated_pkgs$Package

    can_load = TRUE
    added_res = FALSE
    if(!package_exists && !install_packages) fail = c(fail, package)

    if (will_install) {
      progressbar = update(progressbar, p)
      cat("\r",render(progressbar, show_iteration = TRUE),"installing",package,spaces)
      stfu({install.packages(package, verbose = FALSE, quiet = TRUE)})

      stfu({can_load = require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      if (!can_load) fail = c(fail, package)

      data_acc = rbind(data_acc, data.frame(package=package, action="INSTALL", result=can_load, stringsAsFactors = FALSE))
      added_res = TRUE
    }

    if (upgrade && package %in% outdated_pkgs$Package) #upgrade package
    {
      sel = outdated_pkgs[outdated_pkgs$Package==package,]
      progressbar = update(progressbar, p)
      cat("\r",render(progressbar, show_iteration = TRUE),"upgrading",package,spaces)
      stfu({update.packages(oldPkgs=package, ask=FALSE, verbose = FALSE, quiet = TRUE)})

      current_ver = format(packageVersion(package))
      if (compareVersion(current_ver, sel$ReposVer) < 0) {upgrade_fail=c(upgrade_fail, package)} else {upgraded=c(upgraded, package)}

      data_acc = rbind(data_acc, data.frame(package=package, action="UPGRADE",
                                            result=compareVersion(current_ver, sel$ReposVer) < 0, stringsAsFactors = FALSE))
      added_res = TRUE
    }

    if (can_load) {
      progressbar = update(progressbar, p)
      cat("\r",render(progressbar, show_iteration = TRUE),"loading",package,spaces)
      stfu({library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      success = c(success,package)

      if(!added_res)
        data_acc = rbind(data_acc, data.frame(package=package, action="LOAD", result=TRUE, stringsAsFactors = FALSE))
    }
  }

  ## Output status ####################
  pkg_success = str_wrap(paste(success,collapse = ", "),width=80,exdent=exdent)
  pkg_failed = str_wrap(paste(red(underline(fail)),collapse = ", "),width=200,exdent=exdent)
  pkg_upgrade = str_wrap(paste(upgraded,collapse = ", "),width=80,exdent=exdent)
  pkg_upgrade_failed = str_wrap(paste(red(underline(upgrade_fail)),collapse = ", "),width=200,exdent=exdent)
  pkg_dupl = str_wrap(paste(underline(names(duplicates)),collapse = ", "),width=120,exdent=exdent)

  cat("\r",spaces,"\n")
  if(length(success) > 0) cat_bullet(green(SUCCESS),pkg_success,"\n",bullet = "tick", bullet_col = "green")
  if(length(upgraded) > 0) cat_bullet(green(UPGRADED),pkg_upgrade,"\n",bullet = "tick", bullet_col = "green")
  if(length(upgrade_fail) > 0) cat_bullet(red(UPGRADE_FAIL),pkg_upgrade_failed,"\n", bullet = "cross", bullet_col = "red")
  if(length(fail) > 0) cat_bullet(red(FAILED),pkg_failed,"\n", bullet = "cross", bullet_col = "red")

  if(length(duplicates) > 0) cat_bullet(yellow(DUPLICATED),pkg_dupl, "\n", bullet = "warning", bullet_col = "yellow")
  if(length(redundant) > 0) {
    txt = sapply(names(redundant), function(x) paste0(underline(x), " (loaded by ", frmt(redundant[[x]]), ")"))
    spaces = paste0(rep(" ",nchar(REDUNDANT)+2),collapse = "")
    cat_bullet(yellow(REDUNDANT), paste0(txt,collapse = paste0("\n",spaces)),bullet_col = "yellow", bullet = "warning")
  }

  invisible(list(packages=packages, actions=data_acc, outdated=outdated_pkgs))
}

#' List package collections
#' @export
#' @rdname load_packages
list_package_collections = function() {
  list(
    "data_import" = c("readxl","writexl","foreign","utils","haven","sas7bdat","Hmisc"),
    "image_import" = c("png","bmp","rtiff","rgdal"),
    "ggplot" = c("ggthemes","ggmap","colorspace","reshape2","RColorBrewer","Cairo"),
    "grid" = c("gridExtra","gridGraphics"),
    "survival" = c("rms","mice"),
    "processing" = c("magrittr","dplyr","stringr","lubridate","tibble","utils","mice", "Hmisc"),
    "shiny" = c("shiny","shinydashboard","shinyBS","shinyjs","plotly","shinycssloaders","shinyalert","shinythemes"),
    "development" = c("devtools","roxygen2","testthat","utils","rhub","cli","crayon")
  )
}

#' @param collection_name One or multiple collection names. Must be in \code{"data_import","image_import","ggplot",
#' "grid","survival","processing","shiny","development"}.
#' @param ... List of additional package names.
#'
#' @export
#' @rdname load_packages
#' @importFrom crayon bold
load_package_collection = function(collection_name = names(list_package_collections()), ...)
{
  col_names = unique(match.arg(collection_name, several.ok = TRUE))
  pkg_cols = list_package_collections()

  pkgs = sapply(col_names,function(x) pkg_cols[x]) %>% unlist %>% unique %>% sort

  cat_bullet(sprintf("Importing collection: %s",paste0(bold(col_names),collapse = ", ")), bullet = "arrow_right")
  load_packages(pkgs, ...)
}

#' @export
#' @rdname load_packages
list_common_packages = function()
{
  c("devtools", "utils", "readxl", "writexl", "gridExtra", "gridGraphics",
    "reshape2", "scales", "ggplot2", "stringr", "formatR", "tibble", "magrittr","dplyr","roxygen2")
}

#' @export
#' @rdname load_packages
#' @inheritParams load_packages
load_common_packages = function(...) {
  load_packages(list_common_packages(), ...)
}

#' Validate a package name
#' @description Naming rule obtained from \emph{'Writing R Extensions'} manual.
#' The corresponding regular expression used for verifying the package name is \code{"[[:alpha:]][[:alnum:]\\.]*[[:alnum:]]"}.
#' @param pkg A character vector containing package names. Can be a vector of strings with size of at least 1.
#'
#' @return A named logical indicating whether the package name is valid.
#' @export
#' @references \href{https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#The-DESCRIPTION-file}{'Writing R Extensions'} manual.
#' @examples
#' valid_pkgname("hgutils") # valid
#' valid_pkgname("ggplot2") # valid
#' valid_pkgname("pkg2.-1") # invalid
#' @importFrom stringr str_detect
#' @importFrom magrittr %>% set_names
#' @family developer functions
valid_pkgname = function(pkg) {
  regex = "[[:alpha:]][[:alnum:]\\.]*[[:alnum:]]"

  if (!is.character(pkg))
    stop(sprintf("Argument 'pkg' must be of class 'character', but is %s.", frmt(pkg, TRUE)))

  str_detect(pkg,paste0("^",regex,"$")) %>% set_names(pkg)
}

#' Update default function settings
#' @description Uses ellipsis parameter to update a list of default settings.
#'
#' @param default A named list of default values for settings.
#' @param ... Optional settings values to override the default settings.
#'
#' @return The updated list of settings with updated values.
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

#' Retrieve generic function implementations
#' @description Obtains a list of classes for which the supplied generic function has an implementation.
#'
#' @param generic The name of the generic function.
#' @param remove_default Whether to keep the default generic implementation in the result.
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
generic_implementations = function(generic, remove_default = TRUE) {
  stopifnot(length(generic) == 1)
  impls = methods(generic)
  if (!is.character(generic) || length(impls)==0)
    stop(sprintf("Argument 'generic' is not a valid generic function."))

  impls %>% sapply(. %>% {str_match(., "^.*\\.(.*)$")[, 2]}) %>% unname %>% rm_na %>%
    if(remove_default) .[. != "default"] else .
}

#' Set imports for \emph{DESCRIPTION} file
#' @description Update the \emph{DESCRIPTION} file with all imported packages stated in the source code.
#'
#' @param skip_prompt Whether to skip the confirmation prompt to change the \emph{DESCRIPTION} file. Defaults to \code{FALSE}.
#' @param update Whether the \emph{DESCRIPTION} file should be updated. Defaults to \code{TRUE}.
#' @param use_version_numbers Whether package version numbers should be included in the \emph{DESCRIPTION} file. Defaults to \code{TRUE}.
#' @param rversion What version of R to be used in the \emph{DESCRIPTION} file.
#' Can be \code{DEPENDENCIES_VERSION} for the latest version in the package dependencies,
#' \code{LATEST_VERSION} for the current R version or any valid version number.
#'
#' @return Invisibly returns a list with the current R version,
#' the R version obtained from dependencies and packages names (including version numbers).
#' @export
#'
#' @examples \dontrun{crossref_description(skip_prompt=TRUE)}
#' @importFrom magrittr %>%
#' @importFrom stringr str_match str_replace str_replace_all str_split
#' @importFrom utils read.delim packageVersion menu packageDescription
#' @importFrom cli rule cat_bullet cat_rule
#' @importFrom dplyr last
#' @importFrom stats update
#'
#' @family developer functions
crossref_description = function(skip_prompt=FALSE, update=TRUE, use_version_numbers=TRUE, rversion = "DEPENDENCIES_VERSION") {
  if (!dir.exists("R/") || !file.exists("DESCRIPTION"))
    stop("Working directory not set to an R project folder.")

  rversion_const = c("DEPENDENCIES_VERSION","LATEST_VERSION")
  if (!rversion %in% rversion_const && !str_detect(rversion,"[[:digit:]]+([\\.-][[:digit:]]+)+"))
    stop(sprintf("Argument 'rversion' must be either a valid version number or one of %s.",frmt(rversion_const)))

  desc = readLines("DESCRIPTION") %>% paste0(collapse = "\n")
  package_name = desc %>% {str_match(., "Package:[ ]*(.*?)\n(?:.*\n)+Version:[ ]*(.*?)\n")[-1]} %>% paste(collapse = " ")
  existing_imports = str_match(desc,"Imports:((?:.*\n)+?).*?:")[,2] %>%
    str_replace_all("[ \n]|(?:\\(.*?\\))","") %>% {strsplit(.,",")[[1]]}

  pkg_name_regex = "[[:alpha:]][[:alnum:]\\.]*[[:alnum:]]"
  depen = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE) %>%
          sapply(. %>% read.delim(sep = "\n", stringsAsFactors = FALSE, quote="") %>% unlist %>%
                 str_match(., paste0("#'[ ]*@import[ ]+([[:alnum:] \\.]*)$|",
                           paste0("#\'[ ]*@importFrom[ ]+(",pkg_name_regex,")|"),
                 paste0("[^#]*?(?:library|require)\\((",pkg_name_regex,")[ ]*[,\\)]|"),
                 paste0("[^#]*?\\((",pkg_name_regex,")::[:]?[^(:)]+|"),
                 "[^#]*?load_packages\\((?:c\\()?([[:alnum:] ,\"\\'\\.]*?)\\).*")) %>% .[, -1] %>% rm_na) %>%
          unlist %>% str_split("[ ,]") %>% unlist %>% str_replace_all("[\\'\"]","") %>%
          unique %>% {.[valid_pkgname(.)]} %>% sort

  depen = depen[sapply(depen, function(x) suppressWarnings(suppressPackageStartupMessages(
    require(x, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)))) %>% unname]

  pack_version = depen %>% sapply(. %>% packageVersion %>% format) %>% paste0(depen," (>= ",.,")")
  pkgs = if(use_version_numbers) pack_version else depen

  #-- Determine R versions -------------------------------
  current_r = format(getRversion())
  dependencies_r = do.call(rbind, sapply(depen, function(x) packageDescription(x)$Depends)) %>% .[,1] %>%
                   unname %>% {str_match(.,"R \\(>= (.*?)\\)")[,-1]} %>% rm_na %>% numeric_version %>% max %>% format

  rversion = ifelse(identical(rversion, "LATEST_VERSION"), current_r,
             ifelse(identical(rversion, "DEPENDENCIES_VERSION"), dependencies_r, format(rversion)))
  RVersion = sprintf("R (>= %s)",rversion)

  cat_rule(left = "Analyzing package usage", right=package_name, line = "bar4",
           line_col = "dodgerblue4", col="dodgerblue4")
  cat_bullet("R version: ",RVersion, background_col = "dodgerblue4", bullet_col = "white", col="white", bullet = "continue")
  cat_bullet(pkgs, col = "dodgerblue4", bullet_col = "black", bullet = "continue")

  if (update && (skip_prompt || menu(c("Yes","No"),title="\nReplace DESCRIPTION imports?") == 1)) {
    desc %>% str_replace("R \\(.*?\\)",RVersion) %>%
             str_replace("(Imports:)(?:(?:.*\n)+?)(.*?:)",sprintf("\\1\n  %s\n\\2",paste0(pkgs,collapse=",\n  "))) %>%
             writeLines("DESCRIPTION")
    cat("\n")
    cat_rule(center="DESCRIPTION successfully updated",line="bar1", col="white", background_col = "green")
  } else
  cat_bullet("DESCRIPTION was not adjusted.",background_col = "red4", col="white", bullet_col = "white", bullet="warning")

  cat("\n")
  # cat_rule(left="Installing/loading dependencies", right=package_name, col="dodgerblue4", line="bar4")
  load_packages(depen)
  cat("\nDone.")

  invisible(list(current_r_version=current_r, dependencies_r_version=dependencies_r,
            packages=depen, packages_version=pack_version))
}

#' Find duplicated packages names
#'
#' @param pkgs A list of packages names
#'
#' @return A named list of duplicated names and number of occurrences
#' @importFrom magrittr %>% set_names
.pkg_duplicated = function(pkgs)
{
  unique(pkgs[duplicated(pkgs)]) %>% set_names(., .) %>% lapply(. %>% {sum(pkgs==.)})
}

#' Find redundant packages
#'
#' @param packages A list of package names
#'
#' @return A named list of packages names, where each value is a vector of packages already loading the corresponding package.
#' @details Certain packages have a direct dependency on other packages. In that case it is unnecessary to attach the latter packages.
#' This function finds those packages and returns them in a named list. For each named item, the name is imported by the value in the list.
#' @export
#'
#' @examples
#' \dontrun{
#' #grid does not have be loaded since gridGraphics already does so.
#' redundant_packages("gridGraphics","grid")
#' }
#' @importFrom magrittr %>% set_names
#' @importFrom stringr str_detect
redundant_packages = function(packages){
  packages = unique(packages)
  redundant = packages %>% set_names(., .) %>%
              lapply(. %>% {
                sapply(packages, function(other) {
                  desc = packageDescription(other)
                  if("Depends" %in% names(desc) && str_detect(desc$Depends, .)) other else NULL
                  }) %>% unlist %>% unique
              })
  redundant[!sapply(redundant, is.null)]
}
