## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                                                    ## sets the value of matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                                                     ## get the value of matrix
        setsolve <- function(solve) m <<- solve()                               ## set the inverse matrix
        getsolve <- function() m                                                ## get the inverse matrix
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {                                                       ## call if the matrix mean is already calculated
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m                                                                       ## displys the final
}
