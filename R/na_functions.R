#' Remove \code{NA}
#' @description Removes any \code{NA} element from a vector.
#'
#' @param x A vector which may contain \code{NA} elements.
#' @return A vector without \code{NA} elements.
#'
#' @examples rm_na(c(1,NA,5,6))
#' @export
rm_na = function(x) {
  x[!is.na(x)]
}

#' Ignore \code{NA} during computations
#'
#' Provides a variety of \code{base} and \code{stats} function, whose default implementation returns \code{NA} if any input value is \code{NA}.
#' For convenience, these utility functions are provided which ignores \code{NA} and executes the original implementations, see table.
#' \tabular{ll}{
#'   \code{table_na}            \tab \code{\link[base]{table}}    \cr
#'   \code{mean_na}             \tab \code{\link[base]{mean}}     \cr
#'   \code{min_na}              \tab \code{\link[base]{min}}      \cr
#'   \code{max_na}              \tab \code{\link[base]{max}}      \cr
#'   \code{pmin_na}             \tab \code{\link[base]{pmin}}     \cr
#'   \code{pmax_na}             \tab \code{\link[base]{pmax}}     \cr
#'   \code{pmin_na.int}         \tab \code{\link[base]{pmin.int}} \cr
#'   \code{pmax_na.int}         \tab \code{\link[base]{pmax.int}} \cr
#'   \code{sum_na}              \tab \code{\link[base]{sum}}      \cr
#'   \code{sd_na}               \tab \code{\link[stats]{sd}}      \cr
#'   \code{var_na}              \tab \code{\link[stats]{var}}     \cr
#'   \code{median_na}           \tab \code{\link[stats]{median}}  \cr
#'   \code{quantile_na}         \tab \code{\link[stats]{quantile}}\cr
#'   \code{range_na}            \tab \code{\link[base]{range}}    \cr
#' }
#' @export
#' @usage NULL
#' @seealso \code{\link{rm_na}} to remove \code{NA} values from a vector.
#'
#' @examples #create a list with random numbers and 25 NA values
#' table_na(c(floor(runif(1000,min=0,max=10)), rep(NA,25)))
#'
#' mean_na(c(1,5,NA,6)) #returns 4
#' sd_na(c(1,5,NA,6)) #returns 2.645751
#' var_na(c(1,5,NA,6)) #returns 7
#' median_na(c(1,5,NA,6)) #returns 5
#' quantile_na(c(1,5,NA,6))
#' sum_na(c(1,5,NA,6)) #returns 12
#' min_na(c(1,5,NA,6)) #returns 1
#' max_na(c(1,5,NA,6)) #returns 6
#' pmin_na(1:5, 2) #returns c(1,2,2,2,2)
#' pmax_na(1:5, 2) #returns c(2,2,3,4,5)
#' range_na(c(1,5,NA,6)) #returns c(1,6)
table_na = function(...) {table(..., useNA = "always")}
#' @export
#' @rdname table_na
#' @usage NULL
mean_na = function(x, trim = 0,...){mean(x, ..., trim=trim, na.rm = TRUE)}
#' @export
#' @rdname table_na
#' @usage NULL
#' @importFrom stats sd
sd_na = function(x){sd(x, na.rm = TRUE)}
#' @rdname table_na
#' @export
#' @usage NULL
var_na = function(x){sd_na(x)^2}
#' @export
#' @rdname table_na
#' @usage NULL
#' @importFrom stats median
median_na = function(x, ...){median(x, na.rm = TRUE, ...)}
#' @export
#' @rdname table_na
#' @usage NULL
#' @importFrom stats quantile
quantile_na = function(x, probs = seq(0, 1, 0.25), names = TRUE, type = 7, ...){
  quantile(x, na.rm = TRUE, probs=probs, names=names, type=type, ...)
}
#' @export
#' @rdname table_na
#' @usage NULL
range_na = function(...) {range(..., na.rm = TRUE)}
#' @export
#' @rdname table_na
#' @usage NULL
sum_na = function(...) {sum(..., na.rm = TRUE)}
#' @export
#' @rdname table_na
#' @usage NULL
min_na = function(...){min(..., na.rm = TRUE)}
#' @export
#' @rdname table_na
#' @usage NULL
max_na = function(...) max(..., na.rm = TRUE)
#' @export
#' @rdname table_na
#' @usage NULL
pmin_na = function(...) pmin(..., na.rm = TRUE)
#' @export
#' @rdname table_na
#' @usage NULL
pmax_na = function(...) pmax(..., na.rm = TRUE)
#' @export
#' @rdname table_na
#' @usage NULL
pmax_na.int = function(...) pmax.int(..., na.rm = TRUE)
#' @export
#' @rdname table_na
#' @usage NULL
pmin_na.int = function(...) pmin.int(..., na.rm = TRUE)
