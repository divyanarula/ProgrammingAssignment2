## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix - A special matrix object that can cache its inverse
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse of matrix
##  4. get the value of the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
    
    ## begins by setting the inverse to NULL as a placeholder for a future value
    inv <- NULL
    
    ## fucntion to set the matrix x, to a new matrix y, and resets the inverse inv to NULL 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## returns the matrix, x
    get <- function() x
    
    ## sets the inverse inv to inverse 
    setinverse <- function(inverse) inv <<- inverse
    
    ##returns the inverse inv
    getinverse <- function() inv
    
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve - This function computes the inverse of the special "matrix" returned 
##              by makeCacheMatrix above. If the inverse has already been calculated 
##              (and the matrix has not changed), then the cacheSolve should retrieve 
##              the inverse from the cache.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
