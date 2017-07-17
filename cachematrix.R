## These two functions take a square matrix object and calculates its inverse.  Once the inverse is calculated
## it is stored in a cache so that future calls for the solution matrix don't have to be recalculated.

## Takes a matrix object and attaches four functions: set, get, setsolve, and getsolve
## that allow the inverse matrix to be cached and recalled.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(inv) m <<- inv
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Takes the matrix object and functions created by makeCacheMatrix.  It check to see if an inverse matrix 
## has already been cached.  If it has, it returns the cache matrix with a message that it is cached data. 
## If the solution hasn't been cached, the function returns the inverse matrix and stores it to the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
