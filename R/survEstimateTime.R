#' Survival time estimation
#'
#' @description Get estimate of timepoints for a given survival probability.
#'
#' @param ... unused
#' @param survival the survival probability for which the timepoint is estimated.
#'
#' @return A named list or matrix with elements surv (estimate), lower and upper (confidence interval). The attribute 'survival' is added to the
#' result and set to the argument survival probability
#'
#' @examples library(rms)
#' fit = cph(Surv(time=time, event=status) ~ age + sex, data=lung, x=TRUE, y=TRUE, surv=TRUE)
#' sFit = survfit(fit)
#' sFit2 = survfit(fit, newdata=lung[1:20,])
#'
#' survEstimateTime(sFit) #get median survival for all data points in the dataset.
#' survEstimateTime(sFit2) #get median survival for patients 1-20 seperately
#' @importFrom magrittr %>%
#' @export
survEstimateTime = function(fit, survival, ...) {
    UseMethod("survEstimateTime")
}

#' Survival time estimation for survRes objects
#' @param sRes A \code{sRes} object
#' @param survival the survival probability for which the timepoint is estimated.
#' @importFrom dplyr rename
survEstimateTime.survRes = function(sRes, survival = 0.5) {
    d = dim(sRes$surv)[2]
    results = if (is.null(d)) {
        sRes %>% {
            sapply(c("surv", "lower", "upper"), function(x) .$time[.[[x]] < survival][1])
        } %>% as.list
    } else {
        sRes %>% {
            sapply(1:d, function(i) sapply(c("surv", "lower", "upper"), function(x) .$time[.[[x]][, i] < survival][1]))
        } %>% t
    }
    results = data.frame(results) %>% rename(time = "surv")
    attr(results, "survival") = survival
    results
}

#' @param sFit A \code{\link[survival]{survfit} object}
#' @describeIn survEstimateTime for survfit objects.
#' @export
survEstimateTime.survfit = function(sFit, survival = 0.5) {
    if (!("survfit" %in% class(sFit)))
        stop("sFit must be a valid survfit object.")

    class(sFit) = c("survRes", class(sFit))
    survEstimateTime(sFit, survival = survival)
}

#' @param sEst A \code{\link[rms]{survest.cph} object}
#' @describeIn survEstimateTime for survest objects.
#' @export
survEstimateTime.list = function(sEst, survival = 0.5) {
    if (!("list" %in% class(sEst) && all(c("time", "surv", "lower", "upper") %in% names(sEst))))
        stop("sEst must be a valid survest result.")

    class(sEst) = c("survRes", class(sEst))
    survEstimateTime(sEst, survival = survival)
}

#' @param fit A \code{\link[survival]{coxph}} object
#'
#' @param newdata A dataframe containing predictors for which predictors are desired. See \code{\link[survival]{survfit.coxph}}.
#'
#' @describeIn survEstimateTime A \code{\link[survival]{coxph}} object.
#' @export
#' @importFrom survival survfit
#' @importFrom rms survest
survEstimateTime.coxph = function(fit, survival = 0.5, newdata = NULL) {
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

    survEstimateTime(SF, survival = survival)
}
