#' Table one
#'
#' @param df \code{data.frame}.
#' @param x column vector name in \code{df}.
#' @param max_size maximum size of unique elements in the numeric variable \code{x} before the values are clustered.
#' @param n_digits The number of digits to which the percentages are rounded.
#' @inheritDotParams get_breaks
#'
#' @return A dataframe containing the contingency tables for each of the variables in \code{df}.
#' @export
create_table_one = function(df) {
  do.call(rbind, lapply(names(df), function(x) create_contigency_table(df, x))) %>% as.data.frame
}

#' @return A matrix with distinct (factor) labels and corresponding counts and percentages.
#' @export
#' @rdname create_table_one
create_contigency_table = function(df, x, max_size = 8, ...) {
  sub = df[,x]
  if (is.numeric(sub)){
    if (length(unique(sub)) <= max_size)
      sub = factor(as.character(sub))
    else
      sub = discretize_numbers(sub, min_size=10, max_breaks = max_size, int_only=FALSE, ...)
  }
  if (is.character(sub) || is.logical(sub))
    sub = factor(sub)
  if (!is.factor(sub))
    stop(sprintf("Column '%s' must be a factor, numeric, logical or character, but is of type %s.", x, frmt(class(sub))))

  pct = percentage_table(sub)
  pct_format = paste0(pct$frequencies," (",rnd_dbl(pct$percentages*100,digits = 1),"%)") %>%
  {set_names(., names(pct$frequencies))} %>% {c(.[length(.)], .[-length(.)])}

  nm = names(pct_format); nm[is.na(nm)] = "Missing"; names(pct_format) = nm

  tbl = matrix(c(names(pct_format), pct_format), ncol = 2)
  tbl[,1] = paste0("  ",tbl[,1])

  rbind(c(x,NA),tbl)
}


#' @export
#' @rdname create_table_one
percentage_table = function(x, n_digits=2) {
  freq = table(x, useNA = "always")
  pct = if(length(dim(freq))==1) prop.table(freq) else prop.table(freq, 2)
  frmt = matrix(c(names(freq),freq,paste0("(",rnd_dbl(pct*100,2),"%)")),ncol = 3)
  frmt = cbind(frmt[,1], sprintf(paste0("%-",
                                        max(nchar(frmt[,2]), na.rm = TRUE)+1,"s%",
                                        max(nchar(frmt[,3]), na.rm = TRUE)+1,"s"), frmt[,2], frmt[,3]))
  result = list(percentage_table = frmt, frequencies = freq, percentages = pct)
  class(result) = "percentage_table"
  result
}

#' Print a formatted percentage table
#'
#' @param x An object of class \code{percentage_table}
#' @param ... further arguments passed to or from other methods.
#'
#' @return NULL
#' @export
#'
#' @examples print(percentage_table(iris$Species))
print.percentage_table = function(x, ...) {
  cat(paste0(sprintf(paste0("%-",max(nchar(x$percentage_table[,1]), na.rm = TRUE)+2,"s%s"),
                     x$percentage_table[,1], x$percentage_table[,2]), collapse = "\n"))
}

#' Discretize continuous numbers
#'
#' @param x vector of numbers.
#' @param min_size minimum size of bins at the edges. Any bins smaller than this size are combined.
#' @inheritDotParams get_breaks -limits -include_bounds
#' @details The function \code{get_breaks} is called to create the boundaries between groups.
#' It is called on default with \code{limits = range(x)} and with \code{include_bounds = FALSE}.
#' This behaviour may be overridden with the \code{...} argument, although it is advised not to do so to avoid empty groups.
#'
#' \code{NA} values are preserved in the result.
#'
#' @return A factor with the same length as \code{x}, with labels indicating bins.
#' @export
#'
#' @examples
#' ages = round(rnorm(1000,50,10)); ages[1] = NA
#' discretize_numbers(ages)
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

  labels = c(paste0("<",br[1]), paste0(br[-length(br)], "-",br[-1]), paste0(">=",br[length(br)]))
  cut(x, breaks=c(-Inf,br,Inf), right=FALSE, labels = labels)
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
  max_width = max(nchar(string))+3
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
