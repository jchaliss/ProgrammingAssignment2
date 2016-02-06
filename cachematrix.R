## Put comments here that give an overall description of what your
## functions do
# The makeCacheMatrix function stores the invertible matrix of choice.
# The cacheSolve function calculates the inverse of the matrix, but only
# does the calculation if cannot find any previously calculated inverse
# matrix.

## Write a short comment describing this function
# The makeCacheMatrix stores the invertible matrix and returns function
# handles of the functions to be performed with the matrix.
# The "set" & "get" function stores the input invertible
# matrix to the worksapce and retrieves it from the workspace.
# The "setinv" & "getinv" functions stores the inverted
# matrix to the worksapce and retrieves it from the workspace.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(t) {
                x <<- t
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
# The cacheSolve function first checks if the inverse of the matrix is 
# available in the workspace. If found, it is retrieved else the inverse 
# of the matrix is calculated.
cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
