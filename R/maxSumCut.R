#' Determine the optimal cutoff to maximize the sum of all observations
#'
#' Given a vector of obsevations and a vector of probabilities / predictions,
#' this function determines the optimal cutoff for binary classification
#' to maximize the sum of the given observations. If multiple cutoffs
#' lead to the maximum sum, the average of these cutoffs is returned.
#'
#' Returns the determined optimal cutoff, a numeric vector of length 1.
#' @param obs A numeric vector of observations
#' @param preds A numeric vector of predictions or probabilities in
#' the interval [0,1]
#' @param stepsize The interval at which cutoffs are created and examined
#' @keywords cutoff threshold classification
#' @export

maxSumCut <- function(obs, preds, stepsize = 0.02, summaryFunc = "mean") {
    if (max(preds) > 1 | min(preds) < 0) {
        stop("Predictions / Probabilities should be in [0,1]")
    }

    cuts <- seq(0, 1, stepsize)
    sums <- sapply(cuts, function(cut) sum(obs * (preds >= cut)))
    results <- data.frame(Cutoff = cuts, Sum = sums)
    bestCutoff <- with(results, Cutoff[which(Sum == max(Sum, na.rm = T))])

    # If multiple cutoffs lead to the optimal result
    if (summaryFunc == "mean") {
        bestCutoff <- mean(bestCutoff)
    } else if (summaryFunc == "min") {
        bestCutoff <- min(bestCutoff)
    } else if (summaryFunc == "max") {
        bestCutoff <- max(bestCutoff)
    }

    return(bestCutoff)
}
