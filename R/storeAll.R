#' Store all objects out of memory which are larger than a threshold
#'
#' This function stores all objects from the global environment
#' that are larger than the threshold in a cache on disk,
#' i.e. out of memory, using SOAR::Store().
#' By default the function creates a "SOARcache" directory as a subdirectory of
#' the current working directory to store the objects.
#' Automatically runs garbage collection after storing the objects.
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
      sizes <- strsplit(sizesDF$Size, split = " ")
      sizes <- num(sapply(sizes, "[[", 1))
      storeThese <- sizesDF$Object[which(sizes >= thresh)]
      message(paste("Storing:", paste(storeThese, collapse = " ")))
      message(paste("Total size:", sum(sizes), "Mb"))
      Store(list = storeThese)
      Sys.setenv(R_LOCAL_CACHE = oldLC)
      gc()
}
