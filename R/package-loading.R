#' Load and install packages
#'
#' @description Utility function to load and optionally install packages if they are missing. When the function terminates,
#' packages are installed (if necessary), upgraded to the latest version (if necessary) and loaded.
#'
#' @param install_packages whether to install the selected packages.
#' @param force_install whether to install packages even if they are installed already.
#' @param upgrade whether to upgrade outdated packages. Defaults to \code{FALSE}.
#' @param ... list of additional package names.
#' @param collection_name One or multiple collection names. Must be in \code{"data_import","image_import","ggplot",
#' "grid","survival","processing","shiny","development"}.
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
#' @importFrom utils install.packages old.packages update.packages compareVersion installed.packages
#' @importFrom crayon underline
#' @importFrom magrittr %>% %<>%
load_packages = function(..., install_packages = TRUE, force_install = FALSE, upgrade=FALSE) {
  start = Sys.time()
  #-- Check for extra arguments in '...' -------
  packages = list(...) %>% unlist
  duplicates = .pkg_duplicated(packages)

  #-- Check for invalid package names in '...' -------
  packages = packages %>% unique %>% sort
  invalid_names = packages[!valid_pkgname(packages)]
  if(length(invalid_names) > 0)
    stop(sprintf("The argument '...' contains the following invalid package names: %s.", invalid_names))

  #-- Define constants -------
  bull = .bullets()
  show_progress = "iteration"
  spaces = paste0(rep(" ",80),collapse = "")
  SUCCESS=         "Loaded succesfully:   "
  UPGRADED=        "Upgraded succesfully: "
  UPGRADE_FAIL=    "Upgrading failed:     "
  FAILED =         "Loading failed:       "
  REDUNDANT =      "Redundant packages:   "
  DUPLICATED =     "Duplicate packages:   "
  CONSIDER_UPGR =  "Consider upgrading:   "
  exdent = nchar(SUCCESS) + 3
  redundant = redundant_packages(packages)
  success = c(); fail=c(); upgraded=c(); upgrade_fail=c()
  n_packages = length(packages)
  prog = if(length(setdiff(packages,installed.packages()[,"Package"])) > 0){
    progressbar(format=">[*][][ ]<",refresh = 1/24, width = min(max(10,n_packages),20), n_iterations = n_packages)
  } else {
    spinner(refresh = 1/24)
  }

  #-- show title -------
  left = sprintf("Loading packages (total: %s package%s)",length(packages), ifelse(length(packages)>1,"s",""))
  cat(.get_title_bar(left),"\n")

  cat("\r",render(prog, progress=0)," Retrieving package info...",spaces, sep = "")

  inst = installed.packages()
  outdated_pkgs = old.packages(instPkgs = inst[inst[,"Package"] %in% packages,, drop=FALSE]) %>% data.frame(stringsAsFactors=FALSE)
  outdated_pkgs$Installed = sapply(outdated_pkgs$Package, function(x) format(packageVersion(x))) #other installed is old
  outdated_pkgs %<>% {.[package_version(.$Installed) < package_version(.$ReposVer),]}
  consider_upgrade = outdated_pkgs$Package
  data_acc = data.frame(package=character(),action=character(),result=logical()); acc_i = 1
  for (p in 1:length(packages)) {
    package = packages[p]

    cat("\r",render(prog, p, show_progress)," loading ",package,"...",spaces, sep = "")
    stfu({package_exists = require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})

    will_install = !package_exists && install_packages || force_install
    will_upgrade = upgrade && package %in% consider_upgrade

    can_load = TRUE
    added_res = FALSE
    if(!package_exists && !install_packages) fail = c(fail, package)

    if (will_install) {
      cat("\r",render(prog, p, show_progress)," installing ",package,"...",spaces, sep = "")
      stfu({install.packages(package, verbose = FALSE, quiet = TRUE)})

      stfu({can_load = require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      if (!can_load) fail = c(fail, package)

      data_acc = rbind(data_acc, data.frame(package=package, action="INSTALL", result=can_load, stringsAsFactors = FALSE))
      added_res = TRUE
    }

    if (will_upgrade) #upgrade package
    {
      sel = outdated_pkgs[outdated_pkgs$Package==package,]
      cat("\r",render(prog, p, show_progress)," upgrading ",package,"...",spaces, sep = "")
      stfu({install.packages(package, verbose = FALSE, quiet = TRUE)})

      current_ver = format(packageVersion(package))
      if (compareVersion(current_ver, sel$Installed) <= 0) {upgrade_fail=c(upgrade_fail, package)} else {upgraded=c(upgraded, package)}

      data_acc = rbind(data_acc, data.frame(package=package, action="UPGRADE",
                                            result=compareVersion(current_ver, sel$ReposVer) < 0, stringsAsFactors = FALSE))
      added_res = TRUE
    }

    if (can_load) {
      cat("\r",render(prog, p, show_progress)," loading ",package,"...",spaces, sep = "")
      stfu({library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      success = c(success,package)

      if(!added_res)
        data_acc = rbind(data_acc, data.frame(package=package, action="LOAD", result=TRUE, stringsAsFactors = FALSE))
    }
  }
  cat("\r",spaces,"\r")

  ## Output status ####################
  pkg_success = wrap_text_table(success, exdent)
  pkg_upgrade = wrap_text_table(upgraded, exdent)
  pkg_upgrade_failed = wrap_text_table(upgrade_fail, exdent) %>% str_replace_all("(\\w+)",.cfail(underline("\\1")))
  pkg_failed = wrap_text_table(fail, exdent) %>% str_replace_all("(\\w+)",.cfail(underline("\\1")))
  pkg_dupl = wrap_text_table(names(duplicates), exdent) %>% str_replace_all("(\\w+)",.cwarn(underline("\\1")))
  pkg_cons = wrap_text_table(consider_upgrade, exdent) %>% str_replace_all("(\\w+)", .chint(underline("\\1")))

  if(length(success) > 0) cat(bull$succ,SUCCESS,pkg_success,"\n",sep = "")
  if(length(upgraded) > 0) cat(bull$succ,UPGRADED,pkg_upgrade,"\n",sep="")
  if(length(upgrade_fail) > 0) cat(bull$fail, .cfail(UPGRADE_FAIL),pkg_upgrade_failed,"\n",sep="")
  if(length(fail) > 0) cat(bull$fail, .cfail(FAILED),pkg_failed,"\n",sep="")
  if(length(duplicates) > 0) cat(bull$warn, DUPLICATED,pkg_dupl,"\n",sep="")

  if(length(redundant) > 0) {
    txt = sapply(names(redundant), function(x) paste0(.cwarn(underline(x)), " (loaded by ", frmt(redundant[[x]]), ")"))
    spaces = paste0(rep(" ",nchar(REDUNDANT)+3),collapse = "")
    cat(bull$warn, REDUNDANT, paste0(txt,collapse = paste0("\n",spaces)),"\n", sep="")
  }
  if(length(consider_upgrade) > 0) cat(bull$hint, CONSIDER_UPGR, pkg_cons,"\n",sep="")

  end = Sys.time()
  cat(sprintf("\n%sDone. %s\n", bull$mess, .cnumb(format_duration(start, end))))
  invisible(list(packages=packages, actions=data_acc, outdated=outdated_pkgs))
}

#' List package collections
#' @export
#' @rdname load_packages
list_package_collections = function() {
  list(
    "data_import" = c("readxl","writexl","foreign","utils","haven","sas7bdat","Hmisc"),
    "image_import" = c("png","bmp","rtiff","rgdal"),
    "ggplot" = c("ggthemes","ggmap","colorspace","reshape2","RColorBrewer","Cairo","grDevices"),
    "grid" = c("gridExtra","gridGraphics","grDevices"),
    "survival" = c("rms","mice"),
    "processing" = c("magrittr","dplyr","stringr","lubridate","tibble","utils","mice", "Hmisc","tidyr"),
    "shiny" = c("shiny","shinydashboard","shinyBS","shinyjs","plotly","shinycssloaders","shinyalert","shinythemes"),
    "development" = c("devtools","roxygen2","testthat","utils","rhub","cli","crayon")
  )
}

#' @export
#' @rdname load_packages
#' @importFrom magrittr %>%
load_package_collection = function(collection_name = names(list_package_collections()), ...)
{
  col_names = unique(match.arg(collection_name, several.ok = TRUE))
  pkg_cols = list_package_collections()
  pkgs = sapply(col_names,function(x) pkg_cols[x]) %>% unlist %>% unique %>% sort

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
load_common_packages = function(...) {
  load_packages(list_common_packages(), ...)
}
