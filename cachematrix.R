## Cachematrix functions store the values of a given matrix and its inverse matrix

## makeCacheMatrix generates a list with the values of the matrix and the inverse, 
## respectively get and getsolve. Set and setsolve function modify the values of
## x and its inverted s into makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve returns the inverted matrix of x, in case this value hasn't already been 
## cached and eventually stores the new value into makeCacheMatrix

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
