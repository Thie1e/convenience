#' Remove all objects from an environment
#'
#' This function removes all objects from the global environment (default)
#' or from a specified environment.
#' @param env The environment from which all objects are to be removed
#' @keywords rm
#' @export
#' @examples
#' clear()

clear <- function(env = ".GlobalEnv") {
    rm(list = ls(name = env), pos = env)
}
