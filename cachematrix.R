## Matrix inversion is potentially a time-consuming computations that
## can benefit from caching rather than compute it repeatedly.
## If the contents of a matrix is not changing, it make sense to cache
## the inverse so that when we need it again, it can be looked up in 
## the cache rather than recomputed.
## Below are two functions that are used to create a special "matrix"
## object that stores the matrix and cache's its inverse.

## Usage example:
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h4 <- hilbert(4)
## h4_cached <- makeCacheMatrix(h4)  # store as special cache matrix
## sh4 <- cacheSolve(h4_cached)  # this time inverse is calculated
## round(sh4 %*% h4, 3)   # optional: check inverse is correct
## sh4 <- cacheSolve(h4_cached)  # this time inverse is from the cache
## round(sh4 %*% h4, 3)   # optional: check inverse is correct
## h4[1,1] <- h4[1,1]*10  # let's change h4
## h4_cached <- makeCacheMatrix(h4)  # matrix has changed, so update
## h4_cached$set(h4)  # alternative method to update
## sh4 <- cacheSolve(h4_cached)
## round(sh4 %*% h4, 3)   # optional: check inverse is correct
## sh4 <- cacheSolve(h4_cached)
## round(sh4 %*% h4, 3)   # optional: check inverse is correct


## makeCacheMatrix: This function creates a special "matrix" object 
## that can store a matrix and cache its inverse.
## it returns a list containing functions to
##    set the matrix
##    get the matrix
##    set the inverse of the matrix
##    get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m_inv <<- inv
        getinv <- function() m_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse has already
## been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.
## LIMITATION: assume the matrix supplied is always invertible.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached inverse")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setinv(m_inv)
        m_inv
}
