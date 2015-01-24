## MakeCacheMatrix creates a special 'matrix' from its argument which can save 
## its inverse when calculated. cacheSolve takes this matrix and returns the 
## inverse. If the cached inverse is available it is returned otherwise it is 
## calculated.

## This function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set stores 'x' from the input matrix 'y', and sets 
    ## the inverse 'm' to NULL (Same as calling makeCacheMatrix).
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get returns the matrix x.
    get <- function() x
    
    ## setinverse stores the input as 'm'
    setinverse <- function(inverse) m <<- inverse
    
    ## getinverse returns 'm'
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special 'matrix' returned by 
## makeCacheMatrix above. If the inverse has already been calculated the cached
## inverse is returned otherwise its calculated:

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## set 'm' to the cached inverse, if 'm' is not null, return it.
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    
    ## if 'm' is null, get the matrix and calculate the inverse
    data <- x$get()
    m <- solve(data, ...)
    
    ## store the inverse for future use.
    x$setinverse(m)
    
    m
}
