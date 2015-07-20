#' Randomly draw rows or elements from an object
#'
#' Randomly draw n rows/elements from an object. Can be useful as an alternative
#' to head(), tail() or ht(). May also be used to quickly draw random samples.
#' @param x An object
#' @param n Single positive integer. The number of rows/elements to be randomly
#' drawn.
#' @keywords sample
#' @export
#' @examples
#' set.seed(4321)
#' draw(iris, 6L)
#' ###      Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#' ###  51           7.0         3.2          4.7         1.4 versicolor
#' ###  136          7.7         3.0          6.1         2.3  virginica
#' ###  61           5.0         2.0          3.5         1.0 versicolor
#' ###  7            4.6         3.4          1.4         0.3     setosa
#' ###  112          6.4         2.7          5.3         1.9  virginica
#' ###  109          6.7         2.5          5.8         1.8  virginica

draw <- function(x, n = 10L, replace = F, ...){
      stopifnot(n >= 0)
      dimension <- dim(x)
      if (is.null(dimension)){
            # Vector
            return(x[sample(seq_along(x), size = n, replace = replace)])
      } else {
            # Matrix
            return(x[sample(1:dimension[1], size = n, replace = replace), ])
      }
}