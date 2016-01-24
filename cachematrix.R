## makeCacheMatrix(x) accepts a matrix as an argument and it returns a 'funky' copy of x
## that facilitates storing the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv ,
             getinv = getinv )
}


## cacheSolve(x) computes the inverse of x if it hasn't been computed before.  If it 
## has been computed before, then cacheSolve(x) simply accesses and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        x2 <- x$get()
        m <- solve(x2)
        x$setinv(m)
        m
}
