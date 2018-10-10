## These function enable the user to cache the inverse of a matrix so that it 
##      is available for future processing without requiring a recalculation.
##      It consists of 2 function: makeCacheMatrix, which allows the user to 
##      store and retrieve the matrix and its inverse, and cacheSolve, which 
##      returns either a cached version matrix inverse, if it exists, or a 
##      newly calculated inverse, otherwise.

## The makeCacheMatrix function allows the user to store and retrieve a matrix 
##      and its inverse. It is returns a list of 4 functions:
##
##          1. set - which stores a new matrix and removes the cached inverse,
##                  if it exists.
##          2. get - which returns the matrix
##          3. setinv - which caches the calculate inverse of the matrix
##          4. getinv - which returns the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # Update the matrix and clear the cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Return the matrix
    get <- function() x
    # Cache the supplied inverse of the matrix
    setinv <- function(i) inv <<- i
    # Return the cached inverse of the matrix
    getinv <- function() inv
    # Return a list of the available functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve function checks if an inverse of the matrix has been cached.
##      If so, it returns the cached inverse. If not, the function will 
##      calculate the inverse, store it in the cache and then return the 
##      calculated inverse.

cacheSolve <- function(x, ...) {
    # Retrieve the cached inverse.
    inv <- x$getinv()
    # If the inverse has been cached, return it.
    if (!is.null(inv)) {
        return(inv)
    }
    # Else, retrieve the matrix
    mat <- x$get()
    # Calculate the inverse
    inv <- solve(mat)
    # Cache the inverse for future use
    x$setinv(inv)
    # Return the calculated inverse
    inv
}
