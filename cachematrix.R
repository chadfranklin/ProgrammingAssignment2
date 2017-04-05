## The following functions allow an inverse of a matrix to be cached

## This function creates a special "matrix" object that can cache its inverse.
## m is the original matrix; i is the inverse matrix

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(x) {
        m <<- x
        i <<- NULL
    }
    get <- function() m
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## m is the original matrix; i is the inverse matrix

cacheSolve <- function(m, ...) {
    ## get inverse
    i <- m$getsolve()
    
    ## if inverse is already set (not NULL) then return inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## if inverse is not set (NULL), get matrix and solve
    data <- m$get()
    i <- solve(data)
    
    ## set inverse matrix on m special "matrix" and return inverse
    m$setsolve(i)
    i
}
