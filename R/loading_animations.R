#' Creates an animated progress bar
#'
#' @param format Character vector containing format of the resulting progress bar. See 'details' for more information.
#' @param width The width in characters of the progress bar.
#' @param n_iterations Optional parameter, specifies the number of total iterations. When updating the progress bar it
#' is then sufficient to specify the current iteration number.
#' @param refresh The refresh rate in milliseconds of the animation.
#' @param ... Further arguments passed to or from other methods.
#' @param object An animated object (such as a progress bar).
#' @param progress_ratio The proportion progress the bar should be set at.
#' @param progress_iter The current iteration number if \code{n_iterations} is not \code{NULL}.
#' @param show_percentage show percentage progress to the right of the progress bar.
#' @param show_iteration show progress as iteration number to the right of the progress bar.
#' @export
#' @details The format of the progress bar is given by a character vector. It consists of 5 parts:
#' \enumerate{
#'   \item the left border of the progress bar consisting of 0 or more characters.
#'   \item a pair of square brackets containing a single character which represents the loaded area.
#'   \item a pair of square brackets containing 0 or more characters. These are animated on the border between the loaded and unloaded area.
#'   \item a pair of square brackets containing a single character which represents the unloaded area.
#'   \item the right border of the progress bar consisting of 0 or more characters.
#' }
#' The format follows the following regular expression: \code{^.*?[.?][.*?][.?].*$}
#' @examples \dontrun{
#' #simple progressbar
#' bar = progressbar(format = "[[|][|/-\\][ ]]")
#' #fancy progressbar using UTF-8 codes
#' bar2 = progressbar(format="\u25ba[\u2589][\u2580\u2584][\u3000]\u25c4")
#' n_operations = 1000
#'
#' for(i in 1:n_operations) {
#'   bar = update(bar, i/n_operations)
#'   cat("\r", render(bar))
#'   Sys.sleep(0.01)
#' }}
progressbar = function(format="[[|][|/-\\][ ]]", width = 25, refresh = 200, n_iterations = NULL) {
  stopifnot(width > 0)
  if(!width%%1 == 0) stop("Argument 'width' must be an integer.")
  stopifnot(refresh > 0)

  pattern = "^(.*?)\\[(.?)\\]\\[(.*?)\\]\\[(.?)\\](.*)$"
  if (!str_detect(format, pattern))
    stop("Argument 'format' is invalid. See documentation for more details.")
  matches = str_match(format, pattern)[-1]
  matches[is.na(matches)] = ""
  if(matches[2]=="" && matches[4]=="")
    stop("Argument 'format' is invalid: either the loaded or unloaded symbol must be provided. See documentation for more details.")

  progressbar = list(width=width, start=matches[1], ls=matches[2], anim=str_split(matches[3],"")[[1]],
                us=matches[4], end=matches[5], speed=refresh, index=1,
                time=Sys.time(), progress = 0, n_iterations = n_iterations, iteration = NULL)
  class(progressbar) = "progressbar"
  progressbar
}

#' @export
#' @rdname progressbar
update.progressbar = function(object, progress_ratio = NULL, progress_iter = NULL, ...) {
  progressbar = object
  if(!is.null(progress_iter) && !is.null(progress_ratio)) {
    stop("Only one of 'progress_ratio' or 'progress_iter' can be specified.")
  }
  if(!is.null(progressbar$n_iterations) && !is.null(progress_iter)) {
    progressbar$iteration = progress_iter
    progress = progress_iter/progressbar$n_iterations
  } else if(!is.null(progress_ratio) && progress_ratio >= 0 && progress_ratio <= 1) {
    progress = progress_ratio
  } else {
    stop("Either argument 'progress_ratio' or 'progress_iter' must be specified.")
  }
  stopifnot(progress >= 0 && progress <= 1)
  progressbar$progress = progress

  if (Sys.time() >= progressbar$time+progressbar$speed/1000)
  {
    progressbar$time = Sys.time()
    progressbar$index = (progressbar$index %% length(progressbar$anim)) + 1
  }
  progressbar
}

#' @export
#' @rdname progressbar
render = function(object, ...) {
  UseMethod("render", object)
}

#' @rdname progressbar
#' @export
render.progressbar = function(object, show_percentage = FALSE, show_iteration = FALSE, ...) {
  progressbar = object
  progress_text = ""
  if(show_percentage && show_iteration)
    stop("Either 'show_percentage' or 'show_iteration' can be TRUE, not both.")
  if(show_iteration) {
    stopifnot(!is.null(progressbar$iteration))
    progress_text = sprintf(" [%s/%s]", progressbar$iteration, progressbar$n_iterations)
  } else if(show_percentage) {
    progress_text = paste0(" ",round(progressbar$progress*100),"%")
  }

  loaded_width = round(progressbar$width*progressbar$progress)
  animation_width = if(length(progressbar$anim) > 0) min(max(progressbar$width-loaded_width, 0), 1) else 0
  loaded = paste0(rep(progressbar$ls,loaded_width), collapse = "")
  unloaded = paste0(rep(progressbar$us, max(progressbar$width-loaded_width-animation_width,0)), collapse = "")
  animation = paste0(rep(progressbar$anim[progressbar$index],animation_width), collapse = "")
  paste0(progressbar$start, loaded, animation, unloaded, progressbar$end, progress_text)
}
