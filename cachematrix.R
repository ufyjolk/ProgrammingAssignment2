# R Programming MOOC by JHU @ coursera
# (Peer-assesed) Programming Assignment No. 2
# This is the script to be assessed.
# BM (ufyjolk) -- June 20th 2015.

## This script contains two functions: makeCacheMatrix() and casheSolve().
## The overall goal of this script is to calculate and cache an inverted matrix.
## The script keeps the calculated value in cache and when given a task 
## to calculate the value, it first checks if the value already exists in cache.
## Thanks to this functionality, it has a potential to prevent doing this time-consuming
## task repeatedly, when the appropriate value has already been calculated.

## The function makeCacheMatrix() takes as its input a matrix
## and on the basis of it creates an object with four functions:
## [1] set() function sets the value of the original matrix,
## [2] get() function returns the value of the original matrix,
## [3] setCache() function sets the value of cached inverted matrix,
## [4] getCache() function returns the value of cached inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setCache <- function(cache) c <<- cache
        getCache <- function() c
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}


## The function cacheSolve() takes as an input an object created by
## the makeCacheMatrix() funtion and
## [1]   checks if the inverse of the matrix has already been calculated
##       by calling the object's getCache() function,
## [2.1] if the cache is empty, it calculates the appropriate value
##       and stores it in the object using its setCache() funtion,
## [2.2] if the inverse has already been calculated, it displays
##       an information telling the user that the value is in cache
##       and then gets it using the object's getCache() funtion,
## [3]   it returns the inverted matrix.

cacheSolve <- function(x, ...) {
        c <- x$getCache()
        if(!is.null(c)) {
                message("getting cached data")
                return(c)
        }
        data <- x$get()
        c <- solve(data, ...)
        x$setCache(c)
        c
}

## END.