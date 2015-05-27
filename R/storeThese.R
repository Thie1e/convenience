#' Store a list of objects out of memory
#'
#' This function stores a list of objects from the global environment
#' in a cache on disk,
#' i.e. out of memory, by using SOAR::Store() and removing the objects from the
#' global environment. The only difference to SOAR::Store() is that storeThese()
#' creates a "SOARcache" directory as a subdirectory of
#' the current working directory to store the objects so the cache directory does
#' not have to be specified.
#'
#' Automatically runs garbage collection after storing the objects.
#' Returns the result of the garbage collection.
#' @param objects A character vector of objects in the global environment that
#' will be stored
#' @keywords SOAR Store memory
#' @export
#' @examples
#' storeThese()

storeThese <- function(objects = NULL){
      require(SOAR)
      oldLC <- Sys.getenv("R_LOCAL_CACHE", unset = ".R_Cache")
      Sys.setenv(R_LOCAL_CACHE ="SOARcache")
      Store(list = objects)
      Sys.setenv(R_LOCAL_CACHE = oldLC)
      rm(list = objects, pos = .GlobalEnv)
      gc()
}
