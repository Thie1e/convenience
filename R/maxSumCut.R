#' Determine the optimal cutoff to maximize the sum of all obsevations
#'
#' Given a vector of obsevations and a vector of probabilities / predictions,
#' this function determines the optimal cutoff for binary classification
#' to maximize the sum of the given observations.
#' @param obs A numeric vector of observations
#' @param preds A numeric vector of predictions or probabilities in
#' the interval [0,1]
#' @keywords cutoff threshold classification
#' @export

maxSumCut <- function(obs, preds, step = 0.02) {
    if (max(preds) > 1 | min(preds) < 0) {
        stop("Predictions / Probabilities should be in [0,1]")
    }

    cuts <- seq(0, 1, step)
    profit <- sapply(cuts, function(cut) sum(obs * (preds >= cut)))
    results <- data.frame(Cutoff = cuts, Profit = profit)
    bestCutoff <- with(results, Cutoff[which(Profit == max(Profit))])
    # Falls es für mehrere Cutoffs das optimale Ergebnis gibt
    bestCutoff <- mean(bestCutoff)
    return(bestCutoff)
}
