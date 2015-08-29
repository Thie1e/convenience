#' Return the names and sizes in Mb of a number of objects
#'
#' This function returns a data frame with the sizes in Megabytes of a number of
#' objects. By default returns the sizes of the objects in the global environment.
#' @param objects A character vector of all objects for which the size should be
#' returned. By default, fetches the names of all objects in the global environment.
#' @keywords object.size
#' @export
#' @examples
#' showMb()

showMb <- function(objects = ls(envir = .GlobalEnv)){
      sizes <- lapply(objects, function(x) object.size(get(x)))
      if (length(sizes) == 0) {
          message("No objects found, environment empty")
          return(NULL)
      }
      indices <- order(unlist(sizes), decreasing = T)
      sizes <- sapply(sizes, function(x) format(x, units = "Mb"))
      return(data.frame(Object = objects[indices],
                        Size = format(sizes[indices], units = "Mb"),
                        stringsAsFactors = F))
}
