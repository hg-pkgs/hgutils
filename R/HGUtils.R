#' Cleans R for use
#' @description Clears workspace, deletes all objects from global environment, clears graphics and (optionally) sets working directory.
#'
#' @param clean whether to remove objects in the global environment, run garbage collection and to clear graphics. Defaults to \code{TRUE}.
#' @param folder folder name to set the current working directory.
#'
#' @return NULL
#'
#' @examples \dontrun{startup()}
#' @export
#' @importFrom magrittr %>%
#' @importFrom grDevices graphics.off
#' @family initialization functions
startup = function(clean = TRUE, folder = NULL) {
  if (clean)
  {
    rm(list = ls(pos = .GlobalEnv), envir = .GlobalEnv)
    gc()
    graphics.off()
  }

  if (!is.null(folder))
  {
    ifelse(dir.exists(folder), setwd(folder), warning("Argument 'folder' does not refer to an existing directory."))
  }
}

#' Create nice axis breaks for plots
#' @description Set the breaks for a graph in nice positions.
#' @details \code{get_breaks} is the base function and creates a vector of breaks \code{ggplot_breaks} is a wrapper and
#' makes usage easier in \pkg{ggplot2}. The limits of the axis may not be known beforehand,
#' but \code{ggplot_breaks} receives it from \code{ggplot} and then creates nice breaks.
#'
#' @param limits axis limits. May be either a vector of 2 elements with lower and upper bounds, or a
#'               single number (which is the upper bound, the lower bound is then assumed to be 0).
#' @param N step size. The eventual intervals will be multiples of the divisors of \code{N} or
#' multiples of \code{N} when \code{multiples_only} is \code{TRUE}. Defaults to 10.
#' @param max_breaks maximum amount of breaks, defaults to 10.
#' @param int_only whether only integer divisors of \code{N} may be used as breaks, defaults to \code{TRUE}.
#' @param multiples_only whether only multiples of \code{N} can be used as breaks, defaults to \code{FALSE}.
#' @param include_bounds whether the resulting breaks should encompass \code{min} and \code{max}. Defaults to \code{TRUE}.
#'
#' @return A sorted numerical vector with breaks of length \code{|max_breaks|+1} when \code{include_bounds} is \code{TRUE}
#' and of size \code{|max_breaks|} otherwise.
#'
#' @examples
#' get_breaks(24, N=12, max_breaks=15)
#'
#' \dontrun{
#' ggplot() + scale_x_continuous(breaks = ggplot_breaks(N=12, max_breaks=15))}
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom numbers divisors
#' @family break functions
get_breaks = function(limits, N=10, max_breaks=10, int_only=TRUE, multiples_only=FALSE, include_bounds=TRUE) {
  if (!is.vector(limits) || length(limits) > 2 || !is.numeric(limits))
    stop("Argument 'limits' must be a scalar or numeric vector.")
  if (length(limits) == 1) {xmin = 0; xmax = limits} else {xmin = limits[1]; xmax = limits[2]}
  if (xmax < xmin)
    stop("In argument 'limits', 'xmax' must be at least as large as 'xmin'.")
  xmax = xmax - xmin

  lp = function(d) (xmax/(max_breaks*d)) %>% log10 %>% ceiling %>% ifelse(int_only, max(., 0), .)
  up = function(d) (xmax/d)              %>% log10 %>% floor   %>% ifelse(int_only, max(., 0), .)
  intervals = {if(multiples_only) N else divisors(N)} %>% sapply(. %>% {. * 10^(lp(.):up(.))}) %>% unlist %>% unique %>% sort
  selected = intervals[xmax/intervals <= max_breaks][1]

  sq = seq(0, ceiling(xmax/selected)*selected, selected) + floor(xmin/selected)*selected
  if (!include_bounds) sq = sq[sq>=xmin & sq<=(xmin+xmax)]
  sq
}

#' @inheritDotParams get_breaks
#' @export
#' @rdname get_breaks
ggplot_breaks = function(...) {
  function(X) get_breaks(X, ...)
}

#' Separate values
#' @description Separates real numbers from one another that are to close to each other. In the resulting set,
#' the values are separated by a minimum distance, bounded by lower and upper limits and are constraint to be as
#' close as possible to their original values.
#'
#' @param X numerical vector of real numbers.
#' @param distance minimum distance between subsequent numbers. Must be a scalar or vector of size \code{|X|}.
#' @param min,max lower and upper limits.
#' @details This function can be used for example to separate labels that are too close to one another.
#' The resulting vector will create enough space, such that the labels do not overlap anymore, yet are still close to their original values.
#'
#' The output vector has the following properties. For all elements \code{e_i}, \code{min <= e_i <= max}.
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
#' @description The resulting grid will be of size \code{a*a} or \code{a*(a+1)} where \code{a} is an integer.
#' It will therefore always be a square or or have one row/column more than columns/rows.
#'
#' @param N number of objects.
#' @param moreRows whether there should be more rows than columns if the resulting grid is not square. Defaults to more rows (\code{TRUE}).
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

#' Remove \code{NA}
#'
#' @param x vector containing possible \code{NA} values.
#'
#' @return Vector without \code{NA}
#' @export
#'
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
#'
#' @examples
#' data <- rbind(c(1,2,3), c(1, NA, 4), c(4,6,7), c(NA, NA, NA), c(4, 8, NA))
#' rm_empty_rows(data)
rm_empty_rows = function(dataframe) {
  dataframe[rowSums(is.na(dataframe)) != ncol(dataframe),]
}

#' Discretize continuous numbers
#'
#' @param x vector of numbers.
#' @param min_size minimum size of bins at the edges. Any bins smaller than this size are combined.
#' @inheritDotParams get_breaks -limits -include_bounds
#' @details The function \code{get_breaks} is called to create the boundaries between groups.
#' It is called on default with \code{limits = range(x)} and with \code{include_bounds = FALSE}.
#' This behavior may be overridden with the \code{...} argument, although it is advised not to do so to avoid empty groups.
#'
#' \code{NA} values are preserved in the result.
#'
#' @return A factor with the same length as \code{x}, with labels indicating bins.
#' @export
#'
#' @examples
#' ages = round(rnorm(1000,50,10)); ages[1] = NA
#' discretize_numbers(ages)
#' @importFrom magrittr %>%
#' @importFrom dplyr last
discretize_numbers = function(x, min_size = 1, ...) {
  if (!is.numeric(x) & !is.logical(x))
    stop(sprintf("Argument 'x' must be a numeric or logical vector but is of type %s.", frmt(class(x))))

  if(length(unique(x)) <= 3)
    return(factor(x))

  breaks_args = list(...)
  if (!"limits" %in% names(breaks_args)) breaks_args = c(breaks_args, list(limits=range(x,na.rm = TRUE)))
  if (!"include_bounds" %in% names(breaks_args)) breaks_args = c(breaks_args, list(include_bounds=FALSE))
  br = do.call(get_breaks, breaks_args) %>%
       setdiff(., .[sapply(., function(y) sum(x < y, na.rm = TRUE) < min_size)]) %>%
       setdiff(., .[sapply(., function(y) sum(x >= y, na.rm = TRUE) < min_size)])

  labels = c(paste0("<",br[1]), paste0(br[-length(br)], "-",br[-1]), paste0(">=",last(br)))
  cut(x, breaks=c(-Inf,br,Inf), right=FALSE, labels = labels)
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

#' Creates a text table
#'
#' @param compact whether to take only the necessary space (\code{TRUE}) or to fill out the table_width (\code{FALSE}).
#' @inheritParams wrap_text_table
#'
#' @return A vector of strings per row, forming together a table.
#' @export
#' @examples cat(create_text_table(LETTERS),sep = "\n")
#' @importFrom stringr str_pad
#' @seealso \code{\link{get_square_grid}}.
create_text_table = function(string, table_width = 80, compact = TRUE) {
  max_width = max(nchar(string))+2
  n_cols = min(get_square_grid(length(string))$columns, floor(table_width/max_width))

  data = c(string, rep(NA, ceiling(length(string)/n_cols)*n_cols - length(string)))
  mat = matrix(data = data, ncol = n_cols, byrow = TRUE)
  if(!compact) max_width = floor(table_width/n_cols)
  apply(mat, c(1,2), function(x) str_pad(x, max_width, side = "right")) %>% apply(1, function(x) paste0(rm_na(x),collapse = ""))
}

#' Wrap string table
#'
#' @param min_size minimal size where a table is constructed, otherwise elements are concatenated with ', '.
#' @param table_width table character width.
#' @inheritParams stringr::str_wrap
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all str_wrap
#'
#' @return A character vector of a wrapped table where rows are separated by the newline character.
#' @examples cat(wrap_text_table(LETTERS, exdent=0))
#' @seealso \code{\link[stringr]{str_wrap}}, \code{\link{get_square_grid}}.
wrap_text_table = function(string, exdent, min_size = 9, table_width = 80-exdent) {
  if (length(string) >= min_size) {
    tab = create_text_table(string, table_width = table_width)
    str_wrap(paste(tab %>% str_replace_all(" ","@_@"),collapse = "\n"), width=1, exdent=exdent) %>% str_replace_all("@_@"," ")
  } else {
    str_wrap(paste(string, collapse = ", "), width=80-exdent, exdent=exdent)
  }
}
