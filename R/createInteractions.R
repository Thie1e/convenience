#' Create all possible interactions (product, sum, difference)
#'
#' This function takes a data frame as input and returns all possible
#' interaction terms, sums, differences or sums after taking the absolute
#' value of every observation. Normalization before the calculations are
#' carried out is possible. The function does only return the newly created
#' terms, not the input data.
#' @param x A data frame with column names
#' @param type "*" for all possible products, "-" for all possible differences,
#' "+" for all possible sums and "absSum" for all possible sums after taking the
#' absolute value of the observations.
#' @param normalize If TRUE, normalize all columns before carrying out any
#' calculations. TRUE by default.
#' @param excludeFactors If TRUE, excludes all factor variables from the calculations.
#' Otherwise, a high number of variables may be created if the data includes
#' a lot of levels. TRUE by default.
#' @keywords interactions data.frame
#' @export
#' @examples
#' createInteractions()

createInteractions <- function(x, type = NULL, normalize = T, excludeFactors = T){
    if ("data.table" %in% class(x)) stop("Data.table not supported")
    if (is.null(colnames(x))) stop("A data frame with colnames is expected")
    if (is.null(type)) stop("type is missing with no default")
    if (any(is.na(x))) stop("NAs not allowed")

    if (normalize){
        excludedFac <- which(unlist(lapply(x, class)) == "factor")
        SDs <- unlist(lapply(x, sd))
        # For columns without variance
        if (any(SDs == 0)){
            warning("Columns with zero variance found")
            zeroVarCols <- which(SDs == 0)
        } else {
            zeroVarCols <- c()
        }
        x[, -unique(c(excludedFac, zeroVarCols))] <-
            scale(x[, -unique(c(excludedFac, zeroVarCols))])
    }

    if (excludeFactors){
        factorIndices <- which(unlist(lapply(x, class)) == "factor")
        if (length(factorIndices > 0)) x <- x[, -factorIndices]
    }

    if (type == "*"){
        if (any(unlist(lapply(x, is.factor))) && !excludeFactors){
            warning("Including factors may produce a high number of variables")
        }
        # don't return original columns
        colClasses <- sapply(x, class)
        nonFacCols <- sum(colClasses != "factor")
        nLevels <- lapply(x[colClasses == "factor"], levels)
        # Per Variable model.matrix creates one level less ("dummy trap")
        nLevels <- lapply(nLevels, function(x) length(x) - 1)
        nLevels <- sum(unlist(nLevels))
        return(model.matrix(~ .^2-1, x)[, -(1:(nonFacCols + nLevels))])
    }

    if (type == "-"){
        if (any(unlist(lapply(x, is.factor))) && !excludeFactors){
            excludedFac <- which(unlist(lapply(x, class)) == "factor")
            warning("Factors excluded from calculation (differences)")
        }
        colNames <- colnames(x)
        # (don't need last column)
        colNames <- colNames[-length(colNames)]
        diffs <- data.frame(matrix(NA, nrow = nrow(x), ncol = 0))
        for (col in seq_along(colNames)){
            colNum <- which(colnames(x) == colNames[col])
            excludedCols <- 1:colNum
            newnames <- colnames(x)[-excludedCols]
            newnames <- paste(colNames[col], newnames, sep = "_MINUS_")
            tempdat <- x[-excludedCols]
            tempdat <- lapply(tempdat, function(y) x[colNum] - y)
            tempdat <- data.frame(tempdat)
            colnames(tempdat) <- newnames
            diffs <- cbind(diffs, tempdat)
        }
        return(diffs)
    }

    if (type == "+"){
        if (any(unlist(lapply(x, is.factor))) && !excludeFactors){
            excludedFac <- which(unlist(lapply(x, class)) == "factor")
            warning("Factors excluded from calculation (sum)")
        }
        colNames <- colnames(x)
        # (don't need last column)
        colNames <- colNames[-length(colNames)]
        sums <- data.frame(matrix(NA, nrow = nrow(x), ncol = 0))
        for (col in seq_along(colNames)){
            colNum <- which(colnames(x) == colNames[col])
            excludedCols <- 1:colNum
            newnames <- colnames(x)[-excludedCols]
            newnames <- paste(colNames[col], newnames, sep = "_PLUS_")
            tempdat <- x[-excludedCols]
            tempdat <- lapply(tempdat, function(y) x[colNum] + y)
            tempdat <- data.frame(tempdat)
            colnames(tempdat) <- newnames
            sums <- cbind(sums, tempdat)
        }
        return(sums)
    }

    if (type == "absSum"){
        if (any(unlist(lapply(x, is.factor))) && !excludeFactors){
            excludedFac <- which(unlist(lapply(x, class)) == "factor")
            warning("Factors excluded from calculation (sum)")
        }
        colNames <- colnames(x)
        # (don't need last column)
        colNames <- colNames[-length(colNames)]
        sums <- data.frame(matrix(NA, nrow = nrow(x), ncol = 0))
        for (col in seq_along(colNames)){
            colNum <- which(colnames(x) == colNames[col])
            excludedCols <- 1:colNum
            newnames <- colnames(x)[-excludedCols]
            newnames <- paste("ABS:", colNames[col], "_PLUS_", newnames, sep = "")
            tempdat <- x[-excludedCols]
            tempdat <- lapply(tempdat, function(y) abs(x[colNum]) + abs(y))
            tempdat <- data.frame(tempdat)
            colnames(tempdat) <- newnames
            sums <- cbind(sums, tempdat)
        }
        return(sums)
    }
}
