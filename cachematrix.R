## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##      then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix()
## creates a special vector of functions to get and set matrix values
## and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
        ## initialize i to NULL
        i <- NULL
    
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
    
        ## get the value of the matrix
        get <- function() x
    
        ## set the value of the inverse
        setInverse <- function() i <<- solve
    
        ## get the value of the inverse
        getInverse <- function() i
    
        list(set = set,
            get = get,
            setInverse = setInverse,
            getInverse = getInverse)    
}


## cachesolve()
## returns a matrix that is the inverse of the input matrix
## by fetching from cache if available, or else by solving it as required

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## retrieve inversefrom the cache
        i <- x$getInverse()
        
        ## if the value is not null, 
        ## the inverse exists in the cache
        ## so return the cached inverse
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## the inverse in cache was NULL, 
        ## implying it was not cached
        ## so get the data matrix
        data <- x$get()
        
        ## and solve the inverse
        i <- solve(data, ...)
        
        ## then set the inverse in the cache
        x$setInverse(i)
        
        ## return the inverse
        i
}
