#' Remove all objects from an environment
#'
#' This function removes all objects from the global environment (default)
#' or from a specified environment. Returns the result of the garbage
#' collection, if triggered, invisibly.
#' @param env The environment from which all objects are to be removed
#' @param garbageCollection If TRUE, garbage collection via gc() is
#' performed after clearing the environment
#' @keywords rm
#' @export
#' @examples
#' clear()

clear <- function(env = ".GlobalEnv", garbageCollection = T) {
    rm(list = ls(name = env), pos = env)
    if (garbageCollection) invisible(gc())
}
