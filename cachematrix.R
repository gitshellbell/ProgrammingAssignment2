## This pair of functions computes and caches the inverse of a matrix, assuming the matrix input is an invertible matrix.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing functions that
## do each of the following:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(solve) {
                        m <<- solve
        }
                        
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks to see if the inverse matrix has already been cached and if it has been cached,
## the cached inverse matrix will be retrieved and returned. If the inverse has not been cached,
## the inverse will be computed using r function solve(x) and then cached and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinv()  ## Checking to see if the inverse has already been cached
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)   ## This is the r function computing the inverse of the matrix
        x$setinv(m)
        m
}
