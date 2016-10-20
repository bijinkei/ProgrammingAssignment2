## Overall functions create a square matrix and then cache its inverse. 
## The two objects are intiated inside the functions and their values are assigned
## to the parent environment so that they are cached and can be retrieved by later
## code/functions through lexical scoping

## Intializes two objects: x (which is a matrix) and inv (which is given a value
## of NULL so that it can be used later on). SETTER/GETTERS then used to set the
## value of x as y in the parent environment and the value of inv as NULL in the
## parent environment. inv is set as NULL again so that if x is a different value
## and there is a value for inv in the cache, that value is reset so that it can
## be recalculated with the new x. Two more objects are then intialized to be used
## as the setter/getter for the inverse matrix. Then create new object by returning
## a list which names and assigns each of the functions within the makeCacheMatrix()
## function as an element within the list and returns it to the parent environment
## so that each element/function can be called by name later on.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Get the cached inverse matrix and if it is not NULL, then return that value
## If it is null, calculate the inverse for the matrix returned by the 
## makeCacheMatrix() function and return it to the parent environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
