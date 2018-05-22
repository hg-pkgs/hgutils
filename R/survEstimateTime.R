#' Survival time estimation
#'
#' @description Get estimate of timepoints for a given survival probability.
#'
#' @param ... unused
#' @param survival the survival probability for which the timepoint is estimated.
#' @param fit A survival fit object
#'
#' @return A named list or matrix with elements surv (estimate), lower and upper (confidence interval). The attribute 'survival' is added to the
#' result and set to the argument survival probability
#'
#' @examples library(rms)
#' fit = cph(Surv(time=time, event=status) ~ age + sex, data=lung, x=TRUE, y=TRUE, surv=TRUE)
#' sFit = survfit(fit)
#' sFit2 = survfit(fit, newdata=lung[1:20,])
#'
#' survTimeEstimate(sFit) #get median survival for all data points in the dataset.
#' survTimeEstimate(sFit2) #get median survival for patients 1-20 seperately
#' @importFrom magrittr %>%
#' @export
survTimeEstimate = function(fit, survival, ...) {
    UseMethod("survTimeEstimate")
}

#' Survival time estimation for survRes objects
#'
#' @importFrom dplyr rename
#' @describeIn survTimeEstimate for survRes objects
#' @method survTimeEstimate survRes
survTimeEstimate.survRes = function(fit, survival = 0.5, ...) {
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

#' @describeIn survTimeEstimate for \code{\link[survival]{survfit}} objects.
#' @export
#' @method survTimeEstimate survfit
survTimeEstimate.survfit = function(fit, survival = 0.5, ...) {
    if (!("survfit" %in% class(fit))) 
        stop("fit must be a valid survfit object.")
    
    class(fit) = c("survRes", class(fit))
    survTimeEstimate(fit, survival = survival)
}

#' @describeIn survTimeEstimate for \code{\link[rms]{survest.cph}} objects.
#' @export
#' @method survTimeEstimate list
survTimeEstimate.list = function(fit, survival = 0.5, ...) {
    if (!("list" %in% class(fit) && all(c("time", "surv", "lower", "upper") %in% names(fit)))) 
        stop("fit must be a valid survest result.")
    
    class(fit) = c("survRes", class(fit))
    survTimeEstimate(fit, survival = survival)
}

#' @param newdata A dataframe containing predictors for which predictors are desired. See \code{\link[survival]{survfit.coxph}}.
#'
#' @describeIn survTimeEstimate A \code{\link[survival]{coxph}} object.
#' @export
#' @importFrom survival survfit
#' @importFrom rms survest
#' @method survTimeEstimate coxph
survTimeEstimate.coxph = function(fit, survival = 0.5, newdata = NULL, ...) {
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
    
    survTimeEstimate(SF, survival = survival)
}
