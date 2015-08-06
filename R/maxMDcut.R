#' Determine the optimal cutoff to maximize the Mean divided by Standard Deviation
#'
#' Given a vector of obsevations and a vector of probabilities / predictions,
#' this function determines the optimal cutoff for binary classification
#' to maximize the mean of the given observations divided by the standard
#' deviation of the observations. If multiple cutoffs
#' lead to the maximum, the minimum, maximum or average of these cutoffs is returned.
#'
#' Returns the determined optimal cutoff, a numeric vector of length 1.
#' @param obs A numeric vector of observations
#' @param preds A numeric vector of predictions or probabilities in
#' the interval [0,1]
#' @param stepsize The interval at which cutoffs are created and examined
#' @param summaryFunc If multiple cutoffs lead to the optimal solution,
#' return the "mean", "min" or "max" of those cutoffs
#' @keywords cutoff threshold classification
#' @export

maxMDcut <- function(obs, preds, stepsize = 0.02, summaryFunc = "mean") {
    if (max(preds) > 1 | min(preds) < 0) {
        stop("Predictions / Probabilities should be in [0,1]")
    }
    stopifnot(summaryFunc %in% c("max", "min", "mean"))

    cuts <- seq(0, 1, stepsize)
    MeanOverSD <- sapply(cuts, function(cut) {
        mean(obs * (preds >= cut)) / sd(obs * (preds >= cut))
    })

    results <- data.frame(Cutoff = cuts, MeanOverSD = MeanOverSD)
    bestCutoff <- with(results, Cutoff[which(MeanOverSD == max(MeanOverSD))])

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
