#' Time point estimation
#'
#' @description Get estimate of time points for a given survival probability.
#'
#' @param ... further arguments passed to or from other methods.
#' @param survival vector of survival probabilities for which the time points are estimated.
#' @param fit \code{\link[=coxph]{survival fit}} object.
#'
#' @return A named list or matrix with elements \code{surv} (estimate), \code{lower} and \code{upper} (confidence interval).
#' The attribute \code{survival} is added to the result and set to the argument survival probability.
#'
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' fit = rms::cph(Surv(time=time, event=status) ~ age + sex,
#'   data=lung, x=TRUE, y=TRUE, surv=TRUE)
#'
#' #get population time estimates
#' pop = time_estimate(fit)
#'
#' #get median survival for patients 1-20 seperately.
#' sub = time_estimate(fit, survival=seq(0.1,0.9,0.1), newdata=lung[1:20,])
#'
#' fit2 = rms::cph(Surv(time=time, event=status) ~ age + sex,
#'   data=lung, x=FALSE, y=FALSE, surv=TRUE) #no x nor y
#' pop2 = time_estimate(fit2, survival=seq(0.1,0.9,0.01)) #get cox.ph estimate
#' }
time_estimate = function(fit, survival, ...) {
    UseMethod("time_estimate")
}

#' @describeIn time_estimate for \code{survRes} objects.
#' @export
time_estimate.survRes = function(fit, survival = 0.5, ...) {
    d = dim(fit$surv)[2]
    results = if (is.null(d)) {
      lapply(survival, function(s) c(survival=s, {sapply(c("surv", "lower", "upper"),
                       function(x) fit$time[fit[[x]] < s][1])})) %>%
      do.call(rbind,.)
    } else {
      lapply(1:d, function(i) {lapply(survival,
                  function(s) c(id=i,survival=s,{sapply(c("surv", "lower", "upper"),
                  function(x) fit$time[fit[[x]][, i] < s][1])})) %>%
      do.call(rbind,.)}) %>% {do.call(rbind, .)}
    }
    results = data.frame(results)
    names(results)[names(results)=="surv"] = "time"
    attr(results, "survival") = survival
    results
}

#' @describeIn time_estimate for \code{\link[survival]{survfit}} objects.
#' @export
time_estimate.survfit = function(fit, survival = 0.5, ...) {
    class(fit) = c("survRes", class(fit))
    time_estimate(fit, survival = survival)
}

#' @describeIn time_estimate for \code{\link[rms]{survest.cph}} objects.
#' @export
time_estimate.list = function(fit, survival = 0.5, ...) {
    if (!all(c("time", "surv", "lower", "upper") %in% names(fit)))
        stop("fit must be a valid survest result.")

    class(fit) = c("survRes", class(fit))
    time_estimate(fit, survival = survival)
}

#' @param newdata \code{data.frame} containing predictors for which predictors are desired. See \code{\link[survival]{survfit.coxph}}.
#' @describeIn time_estimate A \code{\link[survival]{coxph}} object.
#' @export
time_estimate.coxph = function(fit, survival = 0.5, newdata = NULL, ...) {
    if (!requireNamespace("rms", quietly = TRUE) || !requireNamespace("survival", quietly = TRUE)) {
      stop("Packages \"rms\" and \"survival\" are needed for this function to work. Please install it.", call. = FALSE)
    }
    if (is.null(newdata)) newdata = fit$means

    SF = if ("x" %in% names(fit) && "y" %in% names(fit)) {
        survival::survfit(fit, newdata = newdata)
    } else if ("surv" %in% names(fit)) {
        se = suppressWarnings(rms::survest(fit, times = fit$time[-1], newdata = newdata))
        if (!is.null(dim(se$surv)))
        {
            for (n in c("surv", "lower", "upper")) se[[n]] = t(se[[n]])
        }  #make it consistent with survfit by transposing data
        se
    } else {
      stop("Argument 'fit' must specify either both x and y or surv. Rerun fit with x=TRUE and y=TRUE or surv=TRUE.")
    }

    time_estimate(SF, survival = survival)
}
