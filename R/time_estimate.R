#' Survival time estimation
#'
#' @description Get estimate of timepoints for a given survival probability.
#'
#' @param ... unused
#' @param survival the survival probability for which the timepoint is estimated.
#' @param fit A survival fit object
#' @param newdata A dataframe containing predictors for which predictors are desired. See \code{\link[survival]{survfit.coxph}}.
#'
#' @return A named list or matrix with elements surv (estimate), lower and upper (confidence interval). The attribute 'survival' is added to the
#' result and set to the argument survival probability
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr rename
#' @importFrom survival survfit
#' @importFrom rms survest
#' @export
#' @examples
#' library(rms)
#' fit = cph(Surv(time=time, event=status) ~ age + sex, data=lung,
#'   x=TRUE, y=TRUE, surv=TRUE)
#' sFit = survfit(fit)
#' sFit2 = survfit(fit, newdata=lung[1:20,])
#'
#' time_estimate(sFit) #get median survival for all data points in the dataset.
#' time_estimate(sFit2) #get median survival for patients 1-20 seperately
time_estimate = function(fit, survival, ...) {
    UseMethod("time_estimate")
}

#' @describeIn time_estimate for survRes objects
#' @export
time_estimate.survRes = function(fit, survival = 0.5, ...) {
    d = dim(fit$surv)[2]
    results = if (is.null(d)) {
        fit %>% {
            sapply(c("surv", "lower", "upper"), function(x) .$time[.[[x]] < survival][1])
        } %>% as.list
    } else {
        fit %>% {
            sapply(1:d, function(i) sapply(c("surv", "lower", "upper"), function(x) .$time[.[[x]][, i] < survival][1]))
        } %>% t
    }
    results = data.frame(results) %>% rename(time = "surv")
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
    if (!("list" %in% class(fit) && all(c("time", "surv", "lower", "upper") %in% names(fit))))
        stop("fit must be a valid survest result.")

    class(fit) = c("survRes", class(fit))
    time_estimate(fit, survival = survival)
}

#' @describeIn time_estimate A \code{\link[survival]{coxph}} object.
#' @export
time_estimate.coxph = function(fit, survival = 0.5, newdata = NULL, ...) {
    if (is.null(newdata)) {
        newdata = fit$means
    }

    SF = if ("x" %in% names(fit) && "y" %in% names(fit)) {
        survfit(fit, newdata = newdata)
    } else if ("surv" %in% names(fit)) {
        se = suppressWarnings(survest(fit, times = fit$time[-1], newdata = newdata))
        if (!is.null(dim(se$surv)))
            {
                for (n in c("surv", "lower", "upper")) se[[n]] = t(se[[n]])
            }  #make it consistent with survfit by transposing data
        se
    } else {
        stop("fit must specify either both x and y or surv. Rerun fit with x=TRUE and y=TRUE or surv=TRUE.")
    }

    time_estimate(SF, survival = survival)
}
