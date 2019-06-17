#' Cleans R for use
#' @description Clears workspace, deletes all objects from global environment,
#' clears graphics and (optionally) sets working directory.
#'
#' @param folder folder name to set the current working directory.
#' @param verbose whether to print informative messages during cleaning.
#' @param removeObjects whether to remove objects from the workspace.
#' @param runGarbageCollection whether to run the garbage collection.
#' @param clearGraphics whether to clear the graphics from the R studio plots screen.
#'
#' @return NULL
#'
#' @examples \dontrun{startup()}
#' @export
#' @importFrom crayon red blue
#' @importFrom grDevices graphics.off dev.list
#' @family initialization functions
startup = function(removeObjects=TRUE, runGarbageCollection=TRUE, clearGraphics=TRUE, folder = NULL, verbose=TRUE) {
  if (removeObjects) {
    objects = ls(pos = .GlobalEnv)
    rm(list = objects, envir = .GlobalEnv)
    if(verbose) cat(blue(" \u25ba"), "Removed", length(objects),
                    ifelse(length(objects)==1, "object", "objects"),
                    "from the global environment.\n")
  }

  if(runGarbageCollection) {
    gc()
  }

  if(clearGraphics){
    n_devices = length(dev.list())
    if(n_devices > 0) {
      graphics.off()
      if(verbose) cat(blue(" \u25ba"), "Cleared",n_devices,"graphical devices.\n")
    } else {
      if(verbose) cat(blue(" \u25ba"), "No graphical devices are in use.\n")
    }
  }

  if (!is.null(folder)) {
    ifelse(dir.exists(folder), setwd(folder), warning("Argument 'folder' does not refer to an existing directory."))
  }
  cat(green(" \u25ba"), "Done.\n")
}

#' Round number
#' @description Rounds a number to a specified amount of digits and returns the string value.
#' @param dbl number to be rounded.
#' @param digits number of digits the number needs to be rounded to (defaults to \code{3}).
#'
#' @return A string value of the number rounded to the specified amount of digits.
#'
#' @examples rnd_dbl(1.26564,digits = 2)
#' @export
rnd_dbl = function(dbl, digits = 3) {
  sprintf(paste0("%.", digits, "f"), round(dbl, digits))
}

#' Format time duration
#'
#' @param start,end date-time objects as obtained via \code{\link[base]{Sys.time}}
#'
#' @return A string representation of the duration.
#' @export
#'
#' @importFrom magrittr multiply_by
format_duration = function(start, end=Sys.time()) {
  ms = difftime(end, start, units="secs") %>% as.double %>% multiply_by(1000) %>% round
  if (ms < 1000) return(sprintf("[%s ms.]", rnd_dbl(ms, 0)))
  if (ms < 60000) return(sprintf("[%s sec.]", rnd_dbl(ms/1000, 2)))
  if (ms < 86400000) return(sprintf("[~%s min.]", rnd_dbl(ms/60000, 1)))
  return(sprintf("[~%s days]", rnd_dbl(ms/86400000, 1)))
}

#' Format variable value
#'
#' @description Creates a nice string representation of a variable value.
#'
#' @param x variable for which a string representation is created.
#' @param show_class whether to show the class of \code{x}. Defaults to \code{FALSE}.
#' @param use_quotes whether to use single quotation marks (default: \code{TRUE}).
#'
#' @return A character vector with the string representation of \code{x}.
#' @export
#' @examples frmt(c(1,2,3))
frmt = function(x, show_class = FALSE, use_quotes=TRUE) {
  text = if (length(x) == 0L) {
    "{}"
  } else if (length(x) == 1) {
    sprintf(ifelse(use_quotes,"'%s'","%s"), x)
  } else if (is.atomic(x)) {
    sprintf(ifelse(use_quotes,"['%s']","[%s]"), paste0(sort(x), collapse = ifelse(use_quotes,"','",",")))
  } else {
    sprintf(ifelse(use_quotes,"{'%s'}","{%s}"), paste0(x, collapse = ifelse(use_quotes,"','",",")))
  }

  if (show_class)
    sprintf("%s (class: %s)", text, class(x)) else text
}

#' Adds comma's to separate thousands in numbers
#'
#' @param n a real number
#'
#' @return A string with the number and thousands separated by comma's.
#' @export
#'
#' @examples sep_thousands(13243.33) #13,243.33
#' @importFrom stringr str_replace_all
sep_thousands = function(n) {
  str_replace_all(n, "(\\d{1,3})(?=(?:\\d{3})+\\b)","\\1,")
}

#' Remove \code{NA}
#'
#' @param x vector containing possible \code{NA} values.
#'
#' @return Vector without \code{NA}
#' @export
#' @family NA functions
#' @examples
#' rm_na(c(1,2,NA,54))
rm_na = function(x) {
  x[!is.na(x)]
}

#' Remove empty rows
#'
#' @param dataframe \code{data.frame} object.
#'
#' @return A \code{data.frame} with rows removed that only contain \code{NA}.
#' @export
#' @family NA functions
#' @examples
#' data <- rbind(c(1,2,3), c(1, NA, 4), c(4,6,7), c(NA, NA, NA), c(4, 8, NA))
#' rm_empty_rows(data)
rm_empty_rows = function(dataframe) {
  dataframe[rowSums(is.na(dataframe)) != ncol(dataframe),]
}

#' S.T.F.U.: Stop Text From turning Up
#'
#' @param expr expression to evaluate in silence.
#'
#' @return Returns invisibly the result of \code{expr}.
#' @section Warning:
#' Make sure to call this function \strong{always} directly on the expression and never indirectly e.g. via pipes.
#' Example: \code{stfu(expr)} is correct, but \code{expr \%>\% stfu} will not hide the output. However, the \code{expr} argument itself may contain pipes.
#'
#' @export
#'
#' @examples stfu(print("hi"))
stfu = function(expr) {
  sink(ifelse(.Platform$OS.type=="windows", "NUL", "/dev/null"))
  invisible(tryCatch(suppressWarnings(suppressMessages(expr)), finally = sink()))
}

#' Creates a title bar
#'
#' @param left The text on the left side of the title bar, may be \code{NULL}
#' @keywords internal
#' @return A string vector of a title bar of 80 characters.
.get_title_bar = function(left = NULL) {
  pack_name = ifelse(requireNamespace("methods", quietly = TRUE), methods::getPackageName(), "hgutils")

  name = pack_name %>% paste(packageVersion(.))
  if (is.null(left)) {
    paste(paste0(rep("=",80-nchar(name)-4), collapse = ""), .cinfo(name), "==")
  } else {
    paste("==", left, paste0(rep("=",80-nchar(left)-nchar(name)-8),collapse = ""), .cinfo(name), "==")
  }
}
