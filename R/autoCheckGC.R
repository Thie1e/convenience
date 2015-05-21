#' Granger-Causality test with automatic choice of lag structure
#'
#' This function takes an xts formatted bivariate time series as input, automatically
#' determines the amount of lags to be used in the Granger-Causality test
#' using an information criterion
#' after estimating a VAR and then checks for Granger-Causality in both
#' directions. Uses lmtest::grangertest() for the Granger-Causality testing.
#' @param x A bivariate time series of class xts
#' @param convertToWeekly If TRUE converts the time series to weekly using
#' xts::to.weekly(). Default FALSE
#' @param lagCriterion The information criterion to be used when deciding on
#' the optimal amount of lags based on the estimated VAR. "AIC" by default.
#' Other possible values are "HQ", "SC", and "FPE".
#' Uses vars::VARselect() to estimate the VAR.
#' @param useMinLags If TRUE, uses the minimum amount of lags that any of the
#' information criteria suggested. Overrides "lagCriterion". FALSE by default.
#' @param ... Other parameters to be passed on to other functions, for example
#' to vars::VARselect().
#' @keywords VAR Granger-Causality
#' @export
#' @examples
#' autoCheckGC()

autoCheckGC <- function(x = NULL, convertToWeekly = F,
                        lagCriterion = "AIC", useMinLags = F, ...){
      require(vars)
      require(xts)
      require(lmtest)
      stopifnot("xts" %in% class(x))
      if (sum(is.na(x)) > 0) stop("NAs in data not allowed")
      stopifnot(lagCriterion %in% c("AIC", "HQ", "SC", "FPE"))
      varnames <- colnames(x)
      if (convertToWeekly){
            x1 <- to.weekly(x[, 1])[, 4]
            x2<- to.weekly(x[, 2])[, 4]
            x <- merge(x1, x2)
            colnames(x) <- varnames
            rm(x1, x2)
      }
      lags <- VARselect(x)$selection
      lagCriterion <- paste(lagCriterion, "(n)", sep = "")
      if (useMinLags){
            lags <- min(lags)
      } else lags <- lags[lagCriterion]
      gc1 <- grangertest(x = x[, 1], y = x[, 2], order = lags)
      gc2 <- grangertest(x = x[, 2], y = x[, 1], order = lags)
      message(paste("Hypothesis 1:", varnames[1], "does not help predict",
                    varnames[2], "can be rejected with a p-value of",
                    round(gc1[2, "Pr(>F)"], 3), "using", lags, "lags."))
      message(paste("Hypothesis 2:", varnames[2], "does not help predict",
                    varnames[1], "can be rejected with a p-value of",
                    round(gc2[2, "Pr(>F)"], 3), "using", lags, "lags"))
}
