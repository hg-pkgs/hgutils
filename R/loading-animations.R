#' Creates an animated progress bar
#'
#' @param format character vector containing the format of the animation. See 'details' for more information.
#' @param width progress bar width.
#' @param n_iterations optional parameter, specifies the number of total iterations. When updating the progress bar it
#' is then sufficient to specify the current iteration number.
#' @param refresh refresh rate in milliseconds of the animation.
#' @param ... further arguments passed to or from other methods.
#' @param object animated progress bar.
#' @param progress either the iteration number (if n_iterations is set), or the progress fraction (in [0,1]).
#' @param show_progress how to show the progress. Either not to show it (default),
#' show a percentage or if \code{n_iterations} is set to show the number of iterations.
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
#' # simple progressbar
#' bar = progressbar(format = "[[|][|/-\\][ ]]")
#' # fancy progressbar using UTF-8 codes
#' n_operations = 1000
#' bar2 = progressbar(format="\u25ba[\u2589][\u2580\u2584][\u3000]\u25c4", n_iterations=n_operations)
#'
#' for(i in 1:n_operations) {
#'   cat("\r", render(bar),sep="")
#'   Sys.sleep(0.01)
#' }}
progressbar = function(format="[[|][|/-\\][ ]]", width = 20, refresh = 200, n_iterations = NULL) {
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
                time=Sys.time(), progress = 0)
  class(progressbar) = c("fraction_progressbar","progressbar","progress_animation")

  if(!is.null(n_iterations)) {
    progressbar = c(progressbar, list(n_iterations = n_iterations, iteration = 0))
    class(progressbar) = c("iteration_progressbar","progressbar","progress_animation")
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
render.fraction_progressbar = function(object, progress, show_progress=c("nothing", "percentage"), ...) {
  progressbar = object
  if (Sys.time() >= progressbar$time+progressbar$speed/1000)
  {
    progressbar$time = Sys.time()
    progressbar$index = (progressbar$index %% length(progressbar$anim)) + 1
  }
  progressbar$progress = progress
  eval.parent(substitute(object<-progressbar))

  render.progressbar(object, show_progress = match.arg(show_progress), ...)
}

#' @rdname progressbar
#' @export
render.iteration_progressbar = function(object, progress, show_progress=c("nothing", "percentage", "iteration"), ...) {
  progressbar = object
  if (Sys.time() >= progressbar$time+progressbar$speed/1000)
  {
    progressbar$time = Sys.time()
    progressbar$index = (progressbar$index %% length(progressbar$anim)) + 1
  }
  progressbar$iteration = progress
  progressbar$progress = progress/progressbar$n_iterations
  eval.parent(substitute(object<-progressbar))

  render.progressbar(object, show_progress = match.arg(show_progress), ...)
}

#' @rdname progressbar
#' @export
render.progressbar = function(object, show_progress=c("nothing", "percentage", "iteration"), ...) {
  progressbar = object

  show_pr = match.arg(show_progress)

  if(is.null(progressbar$progress))
    stop("Progress must be specified.")
  if(is.null(progressbar$n_iterations) && show_pr=="iteration")
    stop("The total number of iterations has not been specified.")
  if(progressbar$progress < 0 || progressbar$progress > 1)
    stop("Progress must be in [0,1].")

  progress_text = ""
  progress_text = if(show_pr=="iteration") {
      stopifnot(!is.null(progressbar$iteration))
      sprintf(" [%s/%s]", progressbar$iteration, progressbar$n_iterations)
  } else if(show_pr=="percentage") {
      progress_text = paste0(" ",round(progressbar$progress*100),"%")
  } else {
      ""
  }

  loaded_width = round(progressbar$width*progressbar$progress)
  animation_width = if(length(progressbar$anim) > 0) min(max(progressbar$width-loaded_width, 0), 1) else 0
  loaded = paste0(rep(progressbar$ls,loaded_width), collapse = "")
  unloaded = paste0(rep(progressbar$us, max(progressbar$width-loaded_width-animation_width,0)), collapse = "")
  animation = paste0(rep(progressbar$anim[progressbar$index],animation_width), collapse = "")
  paste0(progressbar$start, loaded, animation, unloaded, progressbar$end, progress_text)
}

#' Creates an animated spinner
#'
#' @param format character vector containing the format of the animation. See 'details' for more information.
#' @param refresh refresh rate in milliseconds of the animation.
#' @param ... further arguments passed to or from other methods.
#' @param object animated spinner.
#' @export
#' @details The format of the spinner simply consists of the characters in order which the spinner cycles through.
#' @examples \dontrun{
#' sp = spinner("|/-\\")
#' n_operations = 100
#'
#' for(i in 1:n_operations) {
#'   cat("\r", render(sp),sep="")
#'   Sys.sleep(0.01)
#' }}
spinner = function(format="|/-\\", refresh = 200) {
  stopifnot(refresh > 0)

  spinner = list(anim=str_split(format,"")[[1]], speed=refresh, index=1, time=Sys.time())
  class(spinner) = c("spinner","progress_animation")
  spinner
}

#' @rdname spinner
#' @export
render.spinner = function(object, ...) {
  spinner = object

  if (Sys.time() >= spinner$time+spinner$speed/1000)
  {
    spinner$time = Sys.time()
    spinner$index = (spinner$index %% length(spinner$anim)) + 1
  }
  eval.parent(substitute(object<-spinner))

  spinner$anim[spinner$index]
}
