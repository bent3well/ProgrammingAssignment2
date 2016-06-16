## These two functions cache the inverse of a matrix.
## The first function--makeCacheMatrix-- should be stored as
        ## 'x' before using it as a variable in the second
        ## function--cacheSolve.


## makeCacheMatrix stores 4 functions that relate to 
        ## caching a matrix

        ## setmatrix: stores the given matrix
        ## getmatrix: returns the matrix
        ## invertmatrix: inverts the matrix using solve()
        ## getinverse: returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmatrix <- function() x
        invertmatrix <- function(invert) inv <- invert
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             invertmatrix = invertmatrix, getinverse = getinverse)
        
}


## If the inverse is not NULL, the cached inverse
        ## will be returned. Otherwise, the inverse is 
        ## calculated by cacheSolve

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getmatrix()
        inv <- solve(data, ...)
        x$invertmatrix(inv)
        inv
}
