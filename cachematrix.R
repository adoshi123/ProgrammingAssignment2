## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        get <- function() x
        inverse_set <- function(inv) inverse <<- inv
        inverse_get <- function() inverse
        list(set = set,
             get = get,
             inverse_set = inverse_set,
             inverse_get = inverse_get)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$inverse_get()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$inverse_set(inv)
        inverse
}
