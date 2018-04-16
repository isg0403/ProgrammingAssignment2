## Author: John 

## This file contains two functions: makeCacheMatrix() and cacheSolve().
## The first function returns a structure similar to an object-oriented (OO) object,
## having an internal state and behaviour. The second function initiates the
## calculation and storage of a property of its argument matrix x, i.e. x's inverse.


## The function makeCacheMatrix() returns a list containing the 4 functions defined
## in its body: get, set, setinv and getinv. The returned list object also contains
## a reference to the parent environment in which these functions were defined,
## i.e. to the variables x and inv.
## Note that during execution of makeCacheMatrix() its local variables x and inv
## are set, however none of its 4 functions are actually executed.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    set <- function(new_x) {
        x <<- new_x         # <<- assigns values to variables in the parent environment
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}

## The function cacheSolve() expects as argument an object created by the makeCacheMatrix()
## function. It is expected (per the assignment description) that this object 
## contains an invertible matrix.

## cacheSolve(x) returns the inverse of the matrix stored in its argument x. 
## If the inverse has already been calculated it returns the cached matrix. 
## Otherwise it first calculates and stores the inverse in object x
## before returning it.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
