## MakeCacheMatrix() and cacheSolve() work together to calculate the inverse 
## of a matrix (must be invertible) and cache that information for future access

## makeCacheMatrix() creates a list that contains two data objects (x = matrix,
## i = inverse matrix) and four functions for setting and retrieving this
## information

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)   
}


## cacheSolve() takes a makeCacheMatrix object (matrix) as an argument and 
## returns either the newly calculated inverse matrix or a cached inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get_inverse()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$set_inverse(i)
    i
}
