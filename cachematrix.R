## Since matrix inversion is usually a costly computation, the functions in this
## file cache the value of the matrix inverse so that when we need it again, it
## can be looked up in the cache rather than recomputed.


## makeCacheMatrix: This function creates a special "matrix" object that can cache
## its inverse. The first function, makeVector creates a special "matrix" is
## really a list containing a function to
##
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the inverse 
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL

    ## Function to set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Function to get the value of the matrix
    get <- function() x
    
    ## Function to set the value of the inverse
    setmatrix <- function(solve) m <<- solve
    
    ## Function to get the value of the inverse
    getmatrix <- function() m
    
    ##  List containing the above functions
    list(set = set, get = get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves the inverse
## from the cache.
##  NOTE: This function makes the assumption that the matrix is invertable

cacheSolve <- function(x = matrix(), ...) {
    
    ## Get the cached value of the matrix inverse    
    m <- x$getmatrix()

    ## If the casched inverse value is not null, return the inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## otherwise, 
    
    ## Get the value of the matrix
    matrix <- x$get()
    
    ## Compute the inverse,  ** assumes that the matrix is invertible  **
    m <- solve(matrix, ...)
    
    ## Save the  invarse value in the cache
    x$setmatrix(m)
    
    ##  Return the inverse
    m
}
