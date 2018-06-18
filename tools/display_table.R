# Initialization ----------------------------------------------------------

library(hgutils)
use_package_genre("processing")
startup()
df = iris; df[is.na(ampute(iris, prop = 0.2)$amp)] = NA


# Create Table ------------------------------------------------------------

create_contigency_table = function(df, x, max_size = 5)
{
  sub = df[,x]
  if (is.numeric(sub)){
    if (length(unique(sub)) <= max_size)
      sub = factor(as.character(sub))
    else
      sub = discretize_numbers(sub, min_size=10, int_only=FALSE)
  }
  if (is.character(sub))
    sub = factor(sub)
  if (!is.factor(sub))
    stop("Argument 'x' must be a factor, numeric or character.")

  pct = percentage_table(sub)
  pct_format = paste0(pct$frequencies," (",rnd_dbl(pct$percentages*100,digits = 1),"%)") %>%
  {set_names(., names(pct$frequencies))} %>% {c(.[length(.)], .[-length(.)])}

  nm = names(pct_format); nm[is.na(nm)] = "Missing"; names(pct_format) = nm

  tbl = matrix(c(names(pct_format), pct_format), ncol = 2)
  tbl[,1] = paste0("  ",tbl[,1])

  rbind(c(x,NA),tbl)
}

tbl = do.call(rbind, lapply(names(df), function(x) create_contigency_table(df, x)))

# #min, q1, median, q3, max
# quantile(ages,probs = c(0,0.25,0.5,0.75,1))
# #min, q1,...,median,..., max
# quantile(ages,probs = get_breaks(c(0,1), max_breaks=10,include_bounds = FALSE, int_only = FALSE))
#
# quantile_na(ages,c(0.25,0.5,0.75))
# mean_na(ages); sd_na(ages)


#' Creates a formatted percentage table
#'
#' @param x A vector
#' @param n_digits The number of digits to which the percentages are rounded
#'
#' @return An object of class \code{percentage_table}, containing the percentage table, and original frequencies and numeric percentages.
#' @export
#'
#' @examples percentage_table(iris$Species)
percentage_table = function(..., n_digits=2) {
  freq = table_na(...)
  pct = prop.table(freq, 2)
  frmt = matrix(c(names(freq),freq,paste0("(",rnd_dbl(pct*100,2),"%)")),ncol = 3)
  frmt = cbind(frmt[,1], sprintf(paste0("%-",
                         max_na(nchar(frmt[,2]))+1,"s%",
                         max_na(nchar(frmt[,3]))+1,"s"), frmt[,2], frmt[,3]))
  result = list(percentage_table = frmt, frequencies = freq, percentages = pct)
  class(result) = "percentage_table"
  result
}

#' Print a formatted percentage table
#'
#' @param x An object of class \code{percentage_table}
#' @param ... unused
#'
#' @return NULL
#' @export
#'
#' @examples print(percentage_table(iris$Species))
print.percentage_table = function(x, ...) {
  cat(paste0(sprintf(paste0("%-",max_na(nchar(x$percentage_table[,1]))+2,"s%s"), x$percentage_table[,1], x$percentage_table[,2]), collapse = "\n"))
}
