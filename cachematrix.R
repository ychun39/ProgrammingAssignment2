## These functions are used to calculate the inverse of an invertible matrix
## If the previous use of the functions was to calculate the same inverse, it
## will retrieve a cached version rather than recalculating the inverse.

## makeCahceMatrix takes one argument, x, a matrix. It creates a special list
## object that allows access to the set, get, setInv, and getInv functions. It 
## serves as a cache stince it stores the inverse of a matrix for the cacheSolve
## function below.

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInv <- function(solve) {
        I <<- solve(x)
    }
    
    getInv <- function() {
        I
    }
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve takes arguments x and ...; it retunrs the inverse of x
## If the inverse has already been calcuated and stored with makeCacheMatrix
## it will return the cached value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    I <- x$getInv()
    
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    
    data <- x$get()
    
    I <- solve(data)
    
    x$setInv(I)
    
    I
}
