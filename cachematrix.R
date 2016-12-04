## ASSIGMENT CONTEXT ##

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:

    ## 1.) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

    ## 2.) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
    ## above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
    ## should retrieve the inverse from the cache.

## Below are two functions that are used to create a special object that stores a numeric vector and cache's its mean.

    ## The first function, makeVector creates a special "vector", which is really a list containing a function to:

        ## 1.set the value of the vector
        ## 2.get the value of the vector
        ## 3.set the value of the mean
        ## 4.get the value of the mean

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## The following function calculates the mean of the special "vector" created with
## the above function. However, it first checks to see if the mean has already been
## calculated. If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the mean in
## the cache via the setmean function.

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

## 1.) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## 2.) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return matrix where the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

## Testing use of my workings:
## Navigate to wd with assignment detail
getwd()
setwd('..') ## move up 1 folder
getwd()
setwd("Wk3")

source("cachematrix.R")
gt_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
gt_matrix$get()

gt_matrix$getInverse()

cacheSolve(gt_matrix)

cacheSolve(gt_matrix)

gt_matrix$getInverse()

gt_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))

gt_matrix$get()

gt_matrix$getInverse()

cacheSolve(gt_matrix)

cacheSolve(my_matrix)

gt_matrix$getInverse()
