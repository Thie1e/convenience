[![Travis-CI Build Status](https://travis-ci.org/Thie1e/convenience.svg?branch=master)](https://travis-ci.org/Thie1e/convenience) [![Coverage Status](https://img.shields.io/codecov/c/github/thie1e/convenience/master.svg)](https://codecov.io/github/thie1e/convenience?branch=master) [![HitCount](https://hitt.herokuapp.com/thie1e/convenience.svg)](https://github.com/thie1e/convenience) [![codecov](https://codecov.io/github/thie1e/convenience/branch/master/graphs/badge.svg)](https://codecov.io/github/thie1e/convenience) 


# convenience
A small package of diverse functions, mainly for more convenient working in R

# For personal use

## Functions
- autoCheckGC: Given two time series checks for Granger Causality and automatically
chooses the appropriate number of lags based on a specified information criterion
- char: short for as.character()
- clear: This function removes all objects from the global environment (default)
or from a specified environment and by default runs garbage collection afterwards
- draw: Randomly draw rows or elements from an object
- getColnames: Return the column names of all variables in the workspace
- h2o.clear: Remove objects from an H2O connection (server)
- ht: This function returns the first and last n rows/elements of an object by
combining head() and tail()
- num: short for as.numeric()
- showMb: Return the names and sizes in Mb of the objects in an environment, by default
the global environment
- shuffle: Shuffle rows or elements of a data object
- storeAll: Store all objects out of memory which are larger than a threshold and
run garbage collection
- storeThese: Store a list of objects out of memory and run garbage collection

## Installation
    library(devtools)
    install_github('thie1e/convenience')
