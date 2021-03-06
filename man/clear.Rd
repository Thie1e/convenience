% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clear.R
\name{clear}
\alias{clear}
\title{Remove all objects from an environment}
\usage{
clear(env = ".GlobalEnv", garbageCollection = T)
}
\arguments{
\item{env}{The environment from which all objects are to be removed}

\item{garbageCollection}{If TRUE, garbage collection via gc() is
performed after clearing the environment}
}
\description{
This function removes all objects from the global environment (default)
or from a specified environment. Returns the result of the garbage
collection, if triggered, invisibly.
}
\examples{
clear()
}
\keyword{rm}

