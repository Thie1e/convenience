#' Shuffle / Permutate rows or elements of a data object
#'
#' Randomly shuffle the rows/elements of an object.
#' If x is a matrix or data frame, this is equivalent to
#' draw(x, n = nrow(x)) or x[sample(1:nrow(x), size = nrow(x)), ].
#' @param x An object
#' @keywords sample draw
#' @export
#' @examples
#' set.seed(4321)
#' head(shuffle(iris))
#' # Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#' # 51           7.0         3.2          4.7         1.4 versicolor
#' # 136          7.7         3.0          6.1         2.3  virginica
#' # 61           5.0         2.0          3.5         1.0 versicolor
#' # 7            4.6         3.4          1.4         0.3     setosa
#' # 112          6.4         2.7          5.3         1.9  virginica
#' # 109          6.7         2.5          5.8         1.8  virginica

shuffle <- function(x){
    dimension <- dim(x)
    if (is.null(dimension)){
        # Vector
        return(x[sample(seq_along(x), size = length(x), replace = F)])
    } else {
        # Matrix / data frame
        return(x[sample(1:dimension[1], size = nrow(x), replace = F), ])
    }
}
