## As a part of Programming Assignment 2, functions makeCacheMatrix and cacheSolve
## are meant to compute the inverse of a matrix if it has not computed before.
## This is achieved by creating a special kind of matrix which is able to cache
## its inverse as well. Therefore, when the inverse function is called, the inverse 
## is only computed if it has not been computed before


## makeCacheMatrix is a function which receives a matrix(x) and returns a list with 
## several functions that can operate over the matrix parameter.
## Four operators/functions are supported over the matrix:
## - set: It sets the matrix(x) into a new one(new_mat)
## - setinv: It sets the inverse matrix(inv) into a new one(new_inv)
## - get: It returns the current matrix(x)
## - set: It returns the current stored inverse matrix(inv)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(new_mat) {
        x <<- new_mat
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(new_inv) inv <<- new_inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is a function which receives a matrix(x) and the additional parameters of the base function "solve"
## and returns the respective inverse. If the inverse has been computed before, it avoids from computing it again. 
cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(is.null(inv))
        {
            data <- x$get()
            inv <- solve(data, ...)
            x$setinv(inv)
        }
        return(inv)
        
}
