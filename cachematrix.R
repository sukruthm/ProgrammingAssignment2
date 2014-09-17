## Caching the inverse of a matrix.
## Finding the inverse of matrix can be a costly operation, to avoid 
## calculating inverse of a matrix multiple times we cache the result


## makeCacheMatrix function creates a cachable matrix. This special matrix 
## can cache the inverse of a matrix provided to it.
## The function has nested functions to:
## 1.set cached matrix
## 2.get cached matrix
## 3.set inverse matrix
## 4.get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {                 
                x <<- y
                inverse <<- NULL
        }
        get <- function() x                  
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function returns the inverse of a matrix
## parameters passed - cacheMatrix
##                     (cache matrix is obtained using makecacheMatrix())
## returns - Matrix inverse of the cacheMatrix
     
cacheSolve <- function(x) {
        inv <- x$getInverse()          # Checks if there exists cached data,        
                                       # If present returns cache value        
        if(!is.null(inv)) {            
                message("Found cached result. Returning cached result")
                return(inv)
        }                
        matrix <- x$get()              # if there is no cache entry, the                                  
        inv <- solve(matrix)           # inverse of the cache is calculated and
        x$setInverse(inv)              # the value is stored in the cache
        inv
}
