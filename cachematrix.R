## Matrix inversion cacheing functions
## 

## Creates a matrix which can cache it's inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function checks if the inverse matrix has been cached. 
## If so it returns the cached date. Otherwise it calculates it.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse() # Gets the cached inverse matrix 
        
        if (!is.null(m)) { # Checks if cached inverse matrix is present. If so it returns it
                message("Retrieving cached data")
                return(m)
        }
        
        data <- x$get() # Else process the inverse matrix
        
        m <- solve(data) # calculate the inverse matrix
        
        x$setinverse(m)
        
        m # Returns the data (last object in function is output)
        
}