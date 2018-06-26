#' Creates an animated progress bar
#'
#' @param format Character vector containing format of the resulting progress bar. See 'details' for more information.
#' @param width The width in characters of the progress bar.
#' @param refresh The refresh rate in milliseconds of the animation.
#' @param x A \code{progressbar}.
#' @param object A \code{progressbar}.
#' @param progress Fraction indicating the progression of the progress bar. Must lie in \code{[0,1]}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
#'
#' @examples \dontrun{
#' bar = create_progressbar(format = "[[|][|/-\\][ ]]")
#' n_operations = 1000
#'
#' for(i in 1:n_operations) {
#'   bar = update(bar, i/n_operations)
#'   cat("\r", render(bar))
#'   Sys.sleep(0.01)
#' }}
create_progressbar = function(format="[[|][|/-\\][ ]]", width = 25, refresh = 200) {
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
  class(progressbar) = "progressbar"
  progressbar
}

#' @rdname create_progressbar
#' @export
update.progressbar = function(object, progress, ...) {
  stopifnot(progress >= 0 && progress <= 1)

  progressbar = object
  progressbar$progress = progress
  if (Sys.time() >= progressbar$time+progressbar$speed/1000)
  {
    progressbar$time = Sys.time()
    progressbar$index = (progressbar$index %% length(progressbar$anim)) + 1
  }
  progressbar
}

#' Render an animated object
#'
#' @param x An animated object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A character vector.
#' @export
render = function(x, ...) {
  UseMethod("render", x)
}

#' @rdname create_progressbar
#' @export
render.progressbar = function(x, show_percentage,...) {
  progressbar = x
  loaded_width = round(progressbar$width*progressbar$progress)
  animation_width = if(length(progressbar$anim) > 0) min(max(progressbar$width-loaded_width, 0), 1) else 0
  loaded = paste0(rep(progressbar$ls,loaded_width), collapse = "")
  unloaded = paste0(rep(progressbar$us, max(progressbar$width-loaded_width-animation_width,0)), collapse = "")
  animation = paste0(rep(progressbar$anim[progressbar$index],animation_width), collapse = "")
  pct = ifelse(show_percentage, paste0(" ",round(progressbar$progress*100),"%"))
  paste0(progressbar$start, loaded, animation, unloaded, progressbar$end, pct)
}
