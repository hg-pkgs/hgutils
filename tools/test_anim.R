library(hgutils)
load_package_collection()

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
  class(progressbar) = c("progressbar","progress_animation")
  progressbar
}

render = function(object, ...) {
  UseMethod("render", object)
}

#' @rdname progressbar
#' @export
render.progressbar = function(object, progress, show_progress=c("nothing", "percentage", "iteration"), ...) {
  progressbar = object
  show_pr <<- match.arg(show_progress)

  if(is.null(progress))
    stop("Progress must be specified.")
  if(is.null(progressbar$n_iterations) && show_pr=="iteration")
    stop("The total number of iterations has not been specified.")
  if(!is.null(progressbar$n_iterations))
    {progressbar$iteration = progress; progress=progress/progressbar$n_iterations}
  if(progress < 0 || progress > 1)
    stop("Progress must be in [0,1].")
  progressbar$progress = progress

  if (Sys.time() >= progressbar$time+progressbar$speed/1000)
  {
    progressbar$time = Sys.time()
    progressbar$index = (progressbar$index %% length(progressbar$anim)) + 1
  }

  progress_text = ""
  progress_text = if(show_pr=="iteration") {
                    stopifnot(!is.null(progressbar$iteration))
                    progress_text = sprintf(" [%s/%s]", progressbar$iteration, progressbar$n_iterations)
                  } else if(show_pr=="percentage") {
                    progress_text = paste0(" ",round(progressbar$progress*100),"%")
                  } else {
                    ""
                  }

  eval.parent(substitute(object<-progressbar))

  loaded_width = round(progressbar$width*progressbar$progress)
  animation_width = if(length(progressbar$anim) > 0) min(max(progressbar$width-loaded_width, 0), 1) else 0
  loaded = paste0(rep(progressbar$ls,loaded_width), collapse = "")
  unloaded = paste0(rep(progressbar$us, max(progressbar$width-loaded_width-animation_width,0)), collapse = "")
  animation = paste0(rep(progressbar$anim[progressbar$index],animation_width), collapse = "")
  paste0(progressbar$start, loaded, animation, unloaded, progressbar$end, progress_text)
}

spinner = function(format="|/-\\", refresh = 200) {
  stopifnot(refresh > 0)

  spinner = list(anim=str_split(format,"")[[1]], speed=refresh, index=1, time=Sys.time())
  class(spinner) = c("spinner","progress_animation")
  spinner
}

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

sp = spinner("|/-\\")
n_operations = 100

for(i in 1:n_operations) {
  sp = update(sp)
  cat("\r", render(sp),sep = "")
  Sys.sleep(0.01)
}
