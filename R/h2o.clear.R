#' Remove objects from an H2O connection (server)
#'
#' This function removes all or a specified type of objects from an H2O connection.
#' @param servername An H2O connection object
#' @param clear "all" to remove all objects, "models" to remove all model
#' objects, "noModel" to remove all non-model objects
#' @keywords H2O h2o.rm
#' @export
#' @examples
#' h2o.clear(h2oServer, "models")

h2o.clear <- function(servername, clear = "all") {
    stopifnot(clear %in% c("all", "models", "noModel"))
    stopifnot(class(servername) == "H2OConnection")

    ls_h2o <- h2o.ls(servername)
    ls_h2o <- as.character(ls_h2o$key)

    if (length(ls_h2o) < 1) stop("No objects at specified H2O connection")

    if (clear == "all") {
        sapply(ls_h2o, function(x) {
            try(h2o.rm(conn = servername, ids = as.character(x)), silent = T)
        })
    }

    if (clear == "models") {
        modelIndices <- str_detect(ls_h2o, "DeepLearning|GBM|K-means|GLM|DRF|
                                             NaiveBayes|PCA")
        sapply(ls_h2o[modelIndices], function(x) {
            try(h2o.rm(conn = servername, ids = as.character(x)), silent = T)
        })
    }

    if (clear == "noModels") {
        modelIndices <- str_detect(ls_h2o, "DeepLearning|GBM|K-means|GLM|DRF|
                                             NaiveBayes|PCA")
        sapply(ls_h2o[!modelIndices], function(x) {
            try(h2o.rm(conn = servername, ids = as.character(x)), silent = T)
        })
    }

    # Check if all objects could be removed
    ls_h2o <- h2o.ls(servername)
    ls_h2o <- as.character(ls_h2o$key)
    if (length(ls_h2o) > 0) message("h2o.clear: Not all objects could be removed")
}
