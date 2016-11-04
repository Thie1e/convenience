#' Return the First and Last Rows or Elements of an Object
#'
#' This function shows the first and last n rows/elements of an object by
#' combining utils::head and utils::tail. It is supposed to be used only while
#' working interactively. Returns NULL invisibly. Also works with tibbles, but returns
#' a data.frame in that case.
#' @param x An object (usually a data.frame, matrix or vector)
#' @param n A single positive integer. The number of rows/elements to print of the
#' head and tail of the object.
#' @keywords head tail
#' @examples
#' ht(iris)

#' @export
ht <- function(x, n, ...) UseMethod("ht", x)

#' @export
ht.default <- function (x, n = 5, ...){
    stopifnot (n > 0)
    if (is.atomic(x)) {
        c(head(x, n), tail(x, n))
    } else {
        rbind(head(x, n), tail(x, n))
    }
}

#' @export
ht.tbl_df <- function (x, n = 5, ...){
    stopifnot (n > 0)
    data.frame(rbind(head(x, n), tail(x, n)))
}

#' @export
ht.data.frame <- function (x, n = 5, ...){
    stopifnot (n > 0)
    if (is.atomic(x)) {
        c(head(x, n), tail(x, n))
    } else {
        rbind(head(x, n), tail(x, n))
    }
}
