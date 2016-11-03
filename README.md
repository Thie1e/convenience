[![Travis-CI Build Status](https://travis-ci.org/Thie1e/convenience.svg?branch=master)](https://travis-ci.org/Thie1e/convenience)

# convenience
A small package of diverse functions, mainly for more convenient working in R

# For personal use

## Functions
- autoCheckGC: Given two time series checks for Granger Causality and automatically
chooses the appropriate number of lags based on a specified information criterion
- cha: short for as.character()
- clear: This function removes all objects from the global environment (default)
or from a specified environment and by default runs garbage collection afterwards
- createInteractions: Create all possible interactions (product, sum, difference)
- draw: Randomly draw rows or elements from an object
- getColnames: Return the column names of all variables in the workspace
- h2o.clear: Remove objects from an H2O connection (server)
- ht: This function shows the first and last n rows/elements of an object by
combining head() and tail()
- maxMDcut: Determine the optimal cutoff to maximize the Mean divided by Standard Deviation
- maxSumCut: Determine the optimal cutoff to maximize the sum of all observations
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
