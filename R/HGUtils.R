#' Cleans R for use
#' @description Clears workspace, deletes all objects from global environment, clears graphics and (optionally) sets working directory.
#'
#' @param clean Removes objects in the global environment, runs garbage collection and clears graphics. Defaults to \code{TRUE}.
#' @param folder Folder name to set the current working directory.
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
#' @param limits The limits of the axis. May be a vector of 2 elements with lower and upper bounds, or a
#'               single digit (which is the upper bound, the lower bound is then assumed to be 0).
#' @param N The step size. The eventual intervals will be multiples of the divisors of \code{N} or
#' multiples of \code{N} when \code{multiples_only} is \code{TRUE}. Defaults to 10.
#' @param max_breaks Maximum amount of breaks, defaults to 10.
#' @param int_only Whether only integer divisors of \code{N} may be used for interval sizes, defaults to \code{TRUE}.
#' @param multiples_only Whether only multiples of \code{N} can be used, defaults to \code{FALSE}.
#' @param include_bounds Whether the resulting breaks should encompass \code{min} and \code{max}. Defaults to \code{TRUE}.
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
#' @description Separates real numbers from one another with a minimum distance, bounded by lower and upper values and constraint to be as
#' close as possible to their original values.
#'
#' @param X A numerical vector of real numbers.
#' @param distance The minimum distance between subsequent numbers. Must be a scalar or vector of size \code{|X|}.
#' @param min,max The lower and upper bounds.
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
#' It will therefore always be a square or or have one row/column more than columns/rows.
#'
#' @param N Number of objects
#' @param moreRows Whether there should be more rows than columns if the grid is not square. Defaults to more rows (\code{TRUE}).
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

#' Remove \code{NA}
#' @description Removes any \code{NA} element from a vector.
#' @param vec A vector which may contain \code{NA} elements.
#'
#' @return A vector without \code{NA} elements.
#'
#' @examples rmNA(c(1,NA,5,6))
#' @export
rmNA = function(vec) {
  vec[!is.na(vec)]
}

#' Round number
#' @description Rounds a number to a specified amount of digits and returns the string value.
#' @param dbl The number to be rounded.
#' @param digits The number of digits the number needs to be rounded to (defaults to \code{3}).
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
#' @param x The variable for which a string representation is created.
#' @param show_class Whether to show the class of \code{x}. Defaults to \code{FALSE}.
#' @param quotes Whether to use single quotation marks (default: \code{TRUE}).
#'
#' @return A character vector with the string representation of \code{x}.
#' @export
#' @examples frmt(c(1,2,3))
frmt = function(x, show_class = FALSE, quotes=TRUE) {
  text = if (length(x) == 0L) {
    "{}"
  } else if (length(x) == 1) {
    sprintf(ifelse(quotes,"'%s'","%s"), x)
  } else if (is.atomic(x)) {
    sprintf(ifelse(quotes,"['%s']","[%s]"), paste0(sort(x), collapse = ifelse(quotes,"','",",")))
  } else {
    sprintf(ifelse(quotes,"{'%s'}","{%s}"), paste0(x, collapse = ifelse(quotes,"','",",")))
  }

  if (show_class)
    sprintf("%s (class: %s)", text, class(x)) else text
}
