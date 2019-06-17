#' Load and install packages
#'
#' @description Utility function to load and optionally install packages if they are missing. When the function terminates,
#' packages are installed (if necessary) and loaded. Upgradeable packages are shown.
#'
#' @param ... list of package names.
#' @param install_packages whether to install the selected packages.
#' @param force_install whether to install packages even if they are installed already.
#' @param show_outdated_packages whether to show a list of packages which are outdated.
#' @param default_loading_method load according to the default R method using only \code{library()}
#' @param return_library_statements makes this function only return a string containing \code{library()} statements which can be paste into an R script.
#'
#' @details
#' \code{load_packages} optionally installs, upgrades and attaches packages to the work space for a list of specified packages.
#'
#'
#' @return Returns invisibly a list with additional package information and results of installing/upgrading and loading.
#' @seealso
#' \code{\link[utils]{install.packages}} for installation of new packages,
#' \code{\link[utils]{update.packages}} for updating outdated packages,
#' \code{\link[base]{library}} for load and attaching packages.
#'
#' @examples \dontrun{
#' # Package names given one-by-one or in a vector
#' load_packages(c('magrittr', 'dplyr'))
#' load_packages('magrittr', 'dplyr')
#'
#' # Package names may be unquoted
#' load_packages(magrittr, dplyr)
#' load_packages('magrittr','dplyr', install_packages=FALSE)
#' }
#'
#' @export
#' @family developer functions
#'
#' @importFrom utils install.packages old.packages update.packages compareVersion installed.packages
#' @importFrom crayon underline
#' @importFrom magrittr set_rownames
load_packages = function(..., install_packages = TRUE, force_install = FALSE, show_outdated_packages=TRUE,
                         default_loading_method=FALSE, return_library_statements=FALSE) {
  oldw <- getOption("warn")
  options(warn = -1)
  start = Sys.time()
  #-- Check for extra arguments in '...' -------
  packages = list(...) %>% unlist
  duplicates = .pkg_duplicated(packages)

  #-- Check for invalid package names in '...' -------
  packages = packages %>% unique %>% sort
  invalid_names = packages[!valid_pkgname(packages)]
  if(length(invalid_names) > 0)
    stop(sprintf("The argument '...' contains the following invalid package names: %s.", paste0(invalid_names,collapse = ", ")))

  found = sapply(packages, function(x) length(find.package(x, quiet = TRUE)) > 0)
  installed = names(found)[found]
  not_installed = names(found)[!found]

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

  if(return_library_statements || default_loading_method) {
    text = paste0("library(", installed,")", collapse = "; ")
    ni = wrap_text_table(not_installed, exdent) %>% str_replace_all("(\\w+)",.cwarn(underline("\\1")))

    if(length(not_installed) > 0)
      cat(bull$warn, "Skipped packages:     ", ni, "\n",sep="")
    options(warn = oldw)
    if (return_library_statements) {
      return(text)
    } else {
      eval(parse(text = text))
      return(invisible())
    }
  }

  #-- show title -------
  left = sprintf("Loading packages (total: %s package%s)",length(packages), ifelse(length(packages)>1,"s",""))
  cat(.get_title_bar(left),"\n")

  cat("\r",render(prog, progress=0)," Retrieving package info...",spaces, sep = "")

  consider_upgrade = c()
  if(show_outdated_packages) {
    inst = installed.packages() %>% set_rownames(NULL)
    inst = inst[order(package_version(inst[,"Version"]),decreasing = TRUE),]
    current_versions = lapply(installed, function(x) {vers = inst[inst[,"Package"]==x,];
    if(is.null(nrow(vers))) {
      vers
    } else if (nrow(vers)==0){
      NULL
    } else {
      vers[1,]
    }}) %>% do.call(rbind,.)

    if(!is.null(current_versions) && nrow(current_versions) > 0) {
      outdated_pkgs = old.packages(instPkgs = current_versions) %>% data.frame(stringsAsFactors=FALSE)
      outdated_pkgs$Installed = sapply(outdated_pkgs$Package, function(x) format(packageVersion(x))) #other installed is old
      outdated_pkgs %<>% {.[package_version(.$Installed) < package_version(.$ReposVer),]}
      consider_upgrade = outdated_pkgs$Package
    }
  }

  for (p in 1:length(packages)) {
    package = packages[p]

    cat("\r",render(prog, p, show_progress)," loading ",package,"...",spaces, sep = "")

    will_install = !(package %in% installed) && install_packages || force_install
    #will_upgrade = upgrade && package %in% consider_upgrade

    can_load = TRUE
    if(!(package %in% installed) && !install_packages)
      fail = c(fail, package)

    if (will_install) {
      cat("\r",render(prog, p, show_progress)," installing ",package,"...",spaces, sep = "")
      stfu({install.packages(package, verbose = FALSE, quiet = TRUE)})

      stfu({can_load = require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      if (!can_load) fail = c(fail, package)
    }

    # if (will_upgrade) #upgrade package
    # {
    #   sel = outdated_pkgs[outdated_pkgs$Package==package,]
    #   cat("\r",render(prog, p, show_progress)," upgrading ",package,"...",spaces, sep = "")
    #   stfu({detach(paste0("package:",package),unload = TRUE);
    #     update.packages(oldPkgs=package, verbose = FALSE, quiet = TRUE,ask = FALSE)})
    #
    #   current_ver = format(packageVersion(package))
    #   if (compareVersion(current_ver, sel$Installed) <= 0) {upgrade_fail=c(upgrade_fail, package)} else {upgraded=c(upgraded, package)}
    #
    #   data_acc = rbind(data_acc, data.frame(package=package, action="UPGRADE",
    #                                         result=compareVersion(current_ver, sel$ReposVer) < 0, stringsAsFactors = FALSE))
    #   added_res = TRUE
    # }

    if (can_load) {
      cat("\r",render(prog, p, show_progress)," loading ",package,"...",spaces, sep = "")
      stfu({library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      success = c(success,package)
    }
  }
  cat("\r",spaces,"\r")

  ## Output status ####################
  pkg_success = wrap_text_table(success, exdent)
  #pkg_upgrade = wrap_text_table(upgraded, exdent)
  #pkg_upgrade_failed = wrap_text_table(upgrade_fail, exdent) %>% str_replace_all("(\\w+)",.cfail(underline("\\1")))
  pkg_failed = wrap_text_table(fail, exdent) %>% str_replace_all("(\\w+)",.cfail(underline("\\1")))
  pkg_dupl = wrap_text_table(names(duplicates), exdent) %>% str_replace_all("(\\w+)",.cwarn(underline("\\1")))
  pkg_cons = wrap_text_table(consider_upgrade, exdent) %>% str_replace_all("(\\w+)", .chint(underline("\\1")))

  if(length(success) > 0) cat(bull$succ,SUCCESS,pkg_success,"\n",sep = "")
  #if(length(upgraded) > 0) cat(bull$succ,UPGRADED,pkg_upgrade,"\n",sep="")
  #if(length(upgrade_fail) > 0) cat(bull$fail, .cfail(UPGRADE_FAIL),pkg_upgrade_failed,"\n",sep="")
  if(length(fail) > 0) cat(bull$fail, .cfail(FAILED),pkg_failed,"\n",sep="")
  if(length(duplicates) > 0) cat(bull$warn, DUPLICATED,pkg_dupl,"\n",sep="")

  if(length(redundant) > 0) {
    txt = sapply(names(redundant), function(x) paste0(.cwarn(underline(x)), " (loaded by ", frmt(redundant[[x]]), ")"))
    spaces = paste0(rep(" ",nchar(REDUNDANT)+3),collapse = "")
    cat(bull$warn, REDUNDANT, paste0(txt,collapse = paste0("\n",spaces)),"\n", sep="")
  }
  if(length(consider_upgrade) > 0) cat(bull$hint, CONSIDER_UPGR, pkg_cons,"\n",sep="")

  end = Sys.time()
  cat(sprintf("\n%sDone. %s\n", bull$info, .cnumb(format_duration(start, end))))
  options(warn=oldw)
}

#' @param ... list of package names.
#' @param collection_name One or multiple collection names. Must be in \code{"data_import","image_import","ggplot",
#' "grid","survival","processing","shiny","development"}.
#' @export
load_package_collection = function(collection_name = names(list_package_collections()), ...) {
  .Deprecated("load_packages")
  col_names = unique(match.arg(collection_name, several.ok = TRUE))
  pkg_cols = list_package_collections()
  pkgs = sapply(col_names,function(x) pkg_cols[x]) %>% unlist %>% unique %>% sort

  load_packages(pkgs, ...)
}

#' List package collections
#' @export
#' @rdname load_package_collection
list_package_collections = function() {
  .Deprecated("load_packages")
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
#' @rdname load_package_collection
list_common_packages = function() {
  .Deprecated("load_packages")
  c("devtools", "utils", "readxl", "writexl", "gridExtra", "gridGraphics",
    "reshape2", "scales", "ggplot2", "stringr", "formatR", "tibble", "magrittr","dplyr","roxygen2")
}

#' @export
#' @rdname load_package_collection
load_common_packages = function(...) {
  .Deprecated("load_packages")
  load_packages(list_common_packages(), ...)
}
