#' Print the First and Last Part of an Object
#'
#' This function shows the first and last n rows/elements of an object by
#' combining utils::head and utils::tail. It is supposed to be used only while
#' working interactively. Returns tail(x, n).
#' @param x An object
#' @param n A single positive integer. The number of rows/elements to print of the
#' head and tail of the object.
#' @keywords head tail
#' @export
#' @examples
#' set.seed(4321)
#' Head:
#' Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#' 1          5.1         3.5          1.4         0.2  setosa
#' 2          4.9         3.0          1.4         0.2  setosa
#' 3          4.7         3.2          1.3         0.2  setosa
#' 4          4.6         3.1          1.5         0.2  setosa
#' 5          5.0         3.6          1.4         0.2  setosa
#' Tail:
#'       Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
#' 146          6.7         3.0          5.2         2.3 virginica
#' 147          6.3         2.5          5.0         1.9 virginica
#' 148          6.5         3.0          5.2         2.0 virginica
#' 149          6.2         3.4          5.4         2.3 virginica
#' 150          5.9         3.0          5.1         1.8 virginica

ht <- function (x, n = 5L, ...){
      stopifnot (n > 0)
      message("Head:")
      print(head(x, n))
      message("Tail:")
      print(tail(x, n))
}
