#' Store all objects out of memory which are larger than a threshold
#'
#' This function stores all objects from the global environment
#' that are larger than the threshold in a cache on disk,
#' i.e. out of memory, by using SOAR::Store() and removing the objects from the
#' global environment.
#' By default the function creates a "SOARcache" directory as a subdirectory of
#' the current working directory to store the objects.
#' Automatically runs garbage collection after storing the objects.
#' Returns the result of the garbage collection.
#' @param thresh (Megabytes) All objects that are larger than "thresh" will be
#' cached out of memory.
#' @param dir The directory where the cache is created. By default /SOARcache
#' as a subdirectory of the current working directory.
#' @keywords SOAR
#' @export
#' @examples
#' storeAll()

storeAll <- function(thresh = 50, dir = "SOARcache"){
      require(SOAR)
      oldLC <- Sys.getenv("R_LOCAL_CACHE", unset = ".R_Cache")
      Sys.setenv(R_LOCAL_CACHE = dir)
      sizesDF <- showMb()
      if (is.null(sizesDF)) stop("Abort, no objects found")
      sizes <- strsplit(sizesDF$Size, split = " ")
      sizes <- num(sapply(sizes, "[[", 1))
      storeThese <- sizesDF$Object[which(sizes >= thresh)]
      message(paste("Storing:", paste(storeThese, collapse = " ")))
      message(paste("Total size of stored objects:",
                    sum(sizes[which(sizes >= thresh)]), "Mb"))
      Store(list = storeThese)
      Sys.setenv(R_LOCAL_CACHE = oldLC)
      rm(list = storeThese, pos = .GlobalEnv)
      gc()
}
