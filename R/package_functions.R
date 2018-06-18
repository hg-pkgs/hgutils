#' Make packages ready for usage
#'
#' @description Utility function to load and optionally install packages if they are missing. When the function terminates,
#' packages are installed (if necessary), upgraded to the latest version (if necessary) and loaded.
#'
#' @param install_packages Whether to install the selected packages.
#' @param force_install Whether to install packages even if they are installed already.
#' @param upgrade Whether to upgrade outdated packages. Defaults to \code{FALSE}.
#' @param ... List of package names and other options.
#'
#' @details
#' \code{load_packages} optionally installs, upgrades and attaches packages to the work space for a list of specified packages.
#' \code{use_common_packages} is a convenient utility which does the same for a prespecified list of common package names
#' defined in \code{list_common_packages}. The dots parameter is passed on to \code{load_packages}.
#'
#' \code{load_package_collection} loads a collection of useful packages, identified by a collection name. This is used to
#' load similar packages for specific programming tasks.
#' The possible collections are:
#' \itemize{
#'   \item \code{data_import}:  readxl, writexl, foreign, utils, haven
#'   \item \code{image_import}: png, bmp, rtiff, rgdal
#'   \item \code{ggplot}: ggplot2, ggthemes, ggmap, colorspace, reshape2, RColorBrewer, Cairo
#'   \item \code{grid}: grid, gridExtra, gridGraphics
#'   \item \code{survival}: survival, Hmisc, rms, mice
#'   \item \code{processing}: magrittr, dplyr, stringr, lubridate, tibble, utils, mice
#'   \item \code{shiny}: shiny, shinydashboard, shinyBS, shinyjs, plotly, shinycssloaders, shinyalert, shinythemes
#'   \item \code{development}: devtools, roxygen2, testthat, utils, rhub
#' }
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
#' @importFrom utils install.packages capture.output old.packages update.packages compareVersion
#' @importFrom cli rule symbol cat_bullet
#' @importFrom crayon green red yellow make_style
#' @importFrom dplyr mutate filter
#' @importFrom magrittr %>% %<>%
load_packages = function(..., install_packages = TRUE, force_install = FALSE, upgrade=FALSE) {

  #-- Check for extra arguments in '...' -------
  packages = list(...)
  packages = setdiff(packages, packages[names(packages) != ""])
  settings = update_settings(list(show_title=TRUE), ...)

  #-- Check for invalid package names in '...' -------
  packages = sort(unique(unlist(packages)))
  invalid_names = packages[!valid_pkgname(packages)]
  if(length(invalid_names) > 0)
    stop(sprintf("The argument '...' contains the following invalid package names: %s.", invalid_names))

  #-- Define constants -------
  max_n = nchar(packages) %>% max
  blue = make_style("dodgerblue4")
  stfu = . %>% capture.output(type = "message") %>% capture.output(type="output") %>% invisible %>%
         suppressPackageStartupMessages %>% suppressMessages %>% suppressWarnings
  spc = paste0(rep(" ",5),collapse = "")
  outdated_pkgs = old.packages() %>% data.frame(stringsAsFactors=FALSE)
  outdated_pkgs$Installed = sapply(outdated_pkgs$Package, function(x) packageVersion(x) %>% format)
  outdated_pkgs %<>% filter(numeric_version(.$Installed) < numeric_version(.$ReposVer)) %>%
                  filter(.$Package %in% packages)

  if(settings$show_title)
    cat(rule(left = sprintf("Loading packages (total: %s packages)",length(packages)), line = "bar4"),"\n")

  data_acc = data.frame(package=character(),action=character(),result=logical()); acc_i = 1
  for (package in packages) {
    stfu({package_exists = require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
    cat(blue(symbol$arrow_right," ",package), rep(" ",max_n+2-nchar(package)),sep = "")

    can_load = TRUE
    added_res = FALSE
    if (!package_exists && install_packages || force_install) {
      cat("\n")
      cat(blue(spc,symbol$continue, "Installing...","\n"), sep = "")
      stfu({install.packages(package, verbose = FALSE, quiet = TRUE)})

      stfu({can_load = require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      if (!can_load) cat(red(spc,symbol$cross, "Installation failed","\n",spc), sep = "") else
        cat(green(spc,symbol$tick, "Installation succesful","\n",spc), sep = "")

      data_acc = rbind(data_acc, data.frame(package=package, action="INSTALL", result=can_load, stringsAsFactors = FALSE))
      added_res = TRUE
    }
    if (upgrade && package %in% outdated_pkgs$Package) #upgrade package
    {
      sel = outdated_pkgs[outdated_pkgs$Package==package,]
      cat("\n")
      cat(yellow(spc,symbol$continue, sprintf("Upgrading from version [%s] to [%s]...",
                                              sel$Installed, sel$ReposVer),"\n"), sep = "")
      stfu({update.packages(oldPkgs=package, ask=FALSE, verbose = FALSE, quiet = TRUE)})

      current_ver = format(packageVersion(package))
      if (compareVersion(current_ver, sel$ReposVer) < 0) {
        cat(red(spc,symbol$cross, sprintf("Upgrade failed. Proceeding with version [%s].",current_ver),"\n",spc), sep = "")
      } else {
        cat(green(spc,symbol$tick, sprintf("Upgrade to version [%s] succesful.",current_ver),"\n",spc), sep = "")
      }

      data_acc = rbind(data_acc, data.frame(package=package, action="UPGRADE",
                                            result=compareVersion(current_ver, sel$ReposVer) < 0, stringsAsFactors = FALSE))
      added_res = TRUE
    }

    if (can_load) {
      stfu({library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      cat(green(symbol$tick,"Loaded","\n"),sep = "")

      if(!added_res)
        data_acc = rbind(data_acc, data.frame(package=package, action="LOAD", result=TRUE, stringsAsFactors = FALSE))
    }
  }

  invisible(list(packages=packages, actions=data_acc, outdated=outdated_pkgs))
}

#' @param collection_name One or multiple collection names. Must be in \code{"data_import","image_import","ggplot",
#' "grid","survival","processing","shiny","development"}
#'
#' @export
#' @rdname load_packages
load_package_collection = function(collection_name = c("data_import","image_import","ggplot","grid",
                                                       "survival","processing","shiny","development"))
{
  collections = match.arg(collection_name, several.ok = TRUE)

  pkg_collections = data.frame(name="data_import", packages=I(list(c("readxl","writexl","foreign","utils","haven"))), stringsAsFactors = FALSE) %>%
    rbind(list("image_import", I(list(c("png","bmp","rtiff","rgdal"))))) %>%
    rbind(list("ggplot", I(list(c("ggplot2","ggthemes","ggmap","colorspace","reshape2","RColorBrewer","Cairo"))))) %>%
    rbind(list("grid", I(list(c("grid","gridExtra","gridGraphics"))))) %>%
    rbind(list("survival", I(list(c("survival","Hmisc","rms","mice"))))) %>%
    rbind(list("processing",I(list(c("magrittr","dplyr","stringr","lubridate","tibble","utils","mice"))))) %>%
    rbind(list("shiny",I(list(c("shiny","shinydashboard","shinyBS","shinyjs","plotly","shinycssloaders","shinyalert","shinythemes"))))) %>%
    rbind(list("development",I(list(c("devtools","roxygen2","testthat","utils","rhub")))))

  for(collection in collections)
  {
    cat_bullet(sprintf("Importing packages: %s",pkg_collections$name[pkg_collections$name==collection]),
              background_col = "dodgerblue4", col = "white", bullet_col = "white", bullet = "arrow_right")
    load_packages(pkg_collections[pkg_collections$name==collection, "packages"][[1]])
    cat("\n")
    .get_examples(collection)
  }
}

#' Displays sample functions for various genres of functions.
#'
#' @param collection The collection name. See \code{\link{load_package_collection}}.
#'
#' @return NULL
#' @importFrom crayon bgMagenta white
.get_examples = function(collection)
{
  if (collection == "survival")
    cli::cat_bullet(paste("Consider using",white(bgMagenta(" hgutils::time_estimate ")),
                           "to obtain a time estimate for a given survival probability.\n"))
  if (collection == "ggplot")
    cli::cat_bullet(paste("Consider using",white(bgMagenta(" hgutils::plot_breaks ")),
                          "for nice and customized axis breaks.\n"))

  if (collection == "development")
    cli::cat_bullet(paste("Consider using:\n",
                          "-  ",white(bgMagenta("hgutils::update_settings ")),"to use the elipsis parameter to specify function settings.\n",
                          "-  ",white(bgMagenta("hgutils::crossref_description ")),"to automatically specify imports in the DESCRIPTION file.\n",
                          "-  ",white(bgMagenta("hgutils::generic_implmentations ")),"to find implementations of generic functions.\n"))
}

#' @export
#' @rdname load_packages
#' @inheritParams load_packages
load_common_packages = function(...) {
  load_packages(list_common_packages(), ...)
}

#' @export
#' @rdname load_packages
list_common_packages = function()
{
  c("devtools", "utils", "readxl", "writexl", "grid", "gridExtra", "gridGraphics", "cli", "installr",
    "reshape2", "scales", "ggplot2", "stringi", "stringr", "formatR", "tibble", "magrittr","dplyr","roxygen2")
}

#' Validate a package name
#' @description Naming rule obtained from \emph{'Writing R Extensions'} manual.
#' @param pkg A character vector containing package names. Can be a vector of strings with size of at least 1.
#' If \code{pkg} is missing, the function returns the regex to validate the names.
#'
#' @return A named logical indicating whether the package name is valid or the validation regex when \code{pkg} is missing.
#' @export
#' @references \href{https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#The-DESCRIPTION-file}{'Writing R Extensions'} manual.
#' @examples
#' valid_pkgname("hgutils") # valid
#' valid_pkgname("ggplot2") # valid
#' valid_pkgname("pkg2.-1") # invalid
#' valid_pkgname()          # returns package name regex
#' @importFrom stringr str_detect
#' @importFrom magrittr %>% set_names
#' @family developer functions
valid_pkgname = function(pkg) {
  regex = "[[:alpha:]][[:alnum:]\\.]*[[:alnum:]]"

  if (missing(pkg))
    return(regex)
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
#' @param ... Optional settings. Set \code{remove_default=FALSE} to keep the default implementation.
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

  impls %>% sapply(. %>% {str_match(., "^.*\\.(.*)$")[, 2]}) %>% unname %>% rm_na %>%
    if(settings$remove_default) .[. != "default"] else .
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

  depen = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE) %>%
          sapply(. %>% read.delim(sep = "\n", stringsAsFactors = FALSE, quote="") %>% unlist %>%
                 str_match(., paste0("#'[ ]*@import[ ]+([[:alnum:] \\.]*)$|",
                           paste0("#\'[ ]*@importFrom[ ]+(",valid_pkgname(),")|"),
                 paste0("[^#]*?(?:library|require)\\((",valid_pkgname(),")[ ]*[,\\)]|"),
                 paste0("[^#]*?\\((",valid_pkgname(),")::[:]?[^(:)]+|"),
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
  cat_rule(left="Installing/loading dependencies", right=package_name, col="dodgerblue4", line="bar4")
  load_packages(depen, show_title=FALSE)
  cat("\n")
  cat_rule(center="DONE",line="bar1", col="white", background_col = "green")

  invisible(list(current_r_version=current_r, dependencies_r_version=dependencies_r,
            packages=depen, packages_version=pack_version))
}

