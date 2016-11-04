#' Wrapper for as.numeric()
#'
#' This function is just a shortcut to as.numeric().
#' @param x An object to convert to numeric
#' @keywords as.numeric
#' @export
#' @examples
#' str(num("2"))

num <- function(x) as.numeric(x)
