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
#' @return A sorted numerical vector with breaks of length \code{|max_breaks|+2} when \code{include_bounds} is \code{TRUE}
#' and of size \code{|max_breaks|} otherwise.
#'
#' @examples
#' get_breaks(24, N=12, max_breaks=15)
#'
#' \dontrun{
#' ggplot() + scale_x_continuous(breaks = ggplot_breaks(N=12, max_breaks=15))}
#'
#' @export
#' @family break functions
#' @export
get_breaks = function(limits, N=10, max_breaks=10, int_only=TRUE, multiples_only=FALSE, include_bounds=TRUE) {
  if (!is.vector(limits) || length(limits) > 2 || !is.numeric(limits))
    stop("Argument 'limits' must be a scalar or numeric vector.")
  if (length(limits) == 1) {xmin = 0; xmax = limits} else {xmin = limits[1]; xmax = limits[2]}
  if (xmax < xmin)
    stop("In argument 'limits', 'xmax' must be at least as large as 'xmin'.")
  stopifnot(N >= 1)
  xmax = xmax - xmin

  lp = function(d) (xmax/(max_breaks*d)) %>% log10 %>% ceiling %>% ifelse(int_only, max(., 0), .)
  up = function(d) (xmax/d)              %>% log10 %>% floor   %>% ifelse(int_only, max(., 0), .)
  intervals = {if(multiples_only) N else (1:N)[(N%%1:N) == 0]} %>% sapply(. %>% {. * 10^(lp(.):up(.))}) %>% unlist %>% unique %>% sort
  selected = intervals[xmax/intervals <= max_breaks][1]

  #sq = seq(0, floor(xmax/selected + 1)*selected, selected) + ceil(xmin/selected - 1)*selected
  compl = seq(xmin-selected,xmax+xmin+selected, selected) - xmin%%selected
  lb = max(compl[xmin >= compl])
  ub = min(compl[(xmax+xmin) <= compl])
  sq = compl[compl >= lb & compl <= ub]

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
#' @examples \dontrun{separate_values(c(0.3,0.4,0.41), distance = 0.05, min = 0, max = 1)}
separate_values = function(X, distance = 0.05, min = 0, max = 1) {
  if(!requireNamespace("limSolve", quietly = TRUE)) {
    cat(.bullets()$warn,"Package ",underline("limSolve")," is required to run 'separate_values'. However, it is not installed.\n\n",sep = "")
    res = readline("Would you like to install it (Y/N)? ")
    if (str_detect(res, "[Yy]")) {
      load_packages("limSolve")
    }

    if(!requireNamespace("limSolve", quietly = TRUE)) {
      cat(.bullets()$fail,"Please install 'limSolve' before calling 'separate_values'.",sep = "")
      invisible(return())
    }
  }

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
    upper[(i*2 - 1):(i*2), i] = c(1, -1)
  }

  if(N > 1) {
    #constraint for distances between elements
    lower = matrix(nrow = N - 1, ncol = N, 0)
    for (i in 1:(N - 1)) {
      lower[i, i:(i+1)] = c(-1, 1)
    }
  } else {lower = NULL}
  H = c(rep(c(min, -max), N), distance)  #solution vectors

  # constraint on limits, spacing and distance to original value
  limSolve::lsei(A = diag(N), B = X, G = rbind(upper, lower), H = H, type = 2)$X
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
get_square_grid = function(N, moreRows = TRUE) {
  N %>% sqrt %>% ceiling %>% {
    list(rows = ifelse(moreRows, ., (N/.) %>% ceiling), columns = ifelse(moreRows, (N/.) %>% ceiling, .))
  }
}
