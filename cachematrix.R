## These functions support caching the inverse of a matrix.
## By using them against matrices, if the inverse is calculated, it will be retained
## in memory and recallable without expensive recalculation.

## makeCacheMatrix is applied to a matrix to create a set of functions
## that support caching its inverse after it's been calculated once, and returning that
## cached inverse thereafter.
makeCacheMatrix <- function(x = matrix()) {
        cached_matrix_inverse <- NULL
        set <- function(val) {
                x <<- val
                cached_matrix_inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(matrix_inverse) cached_matrix_inverse <<- matrix_inverse
        get_inverse <- function() cached_matrix_inverse
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## cacheSolve can be used against any invertible matrix to get its inverse.
## If its inverse has never been calculated before, it will be cached, so any
## subsequent calls for it will simply fetch from the cached value instead of 
## re-incurring the expensive cost.
cacheSolve <- function(x, ...) {
        ptm <- proc.time()
        matrix_inverse <- x$get_inverse()
        if(!is.null(matrix_inverse)) {
                message("getting cached data")
                #return(proc.time() - ptm)
                return(matrix_inverse)
        }
        data <- x$get()
        matrix_inverse <- solve(data, ...)
        x$set_inverse(matrix_inverse)
        #print(proc.time() - ptm)
        matrix_inverse
}
