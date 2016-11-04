#' Return the column names of all variables in the workspace
#'
#' This function returns the column names of all variables in the global
#' environment
#' @param objects (optional) A character vector of all objects for which
#' column names should be returned. If NULL (the default) the function
#' fetches all variable names from the global environment.
#' @keywords colnames
#' @export
#' @examples
#' getColnames()

getColnames <- function(objects = NULL){
      if (is.null(objects)){
            getEnv <- globalenv()
            objects <- ls(getEnv)
      } else {
            getEnv <- -1
      }
      sapply(objects, FUN = function(x) colnames(get(x, envir = getEnv)))
}
