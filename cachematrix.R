## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## makeCacheMatrix is a function that creates a matrix, stores a list
## of functions that act on that matrix, and binds those functions with
## their inherent variable values to the matrix object.
z<-
makeCacheMatrix <- function(x = matrix()) {
    # upon initialization of the matrix, set the inverse to NULL
    i <- NULL
    # this function is used to reset the value of the matrix and reset the inverse to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # simply return the value of the matrix
    get <- function() x
    # simply set the value of the inverse
    setinverse <- function(inverse) i <<- inverse
    # simply get the value of the inverse
    getinverse <- function() i
    # create a list of the functions bound to the matrix object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## this function takes a matrix object and first checks to see if an inverse
## has already been computed for that object.  If there is an inverse, then this
## inverse is returned. If not, then the inverse is computed and returned, and the inverse
## value is cached for future use.

cacheSolve <- function(x, ...) {
    # get the current value of the inverse
    i <- x$getinverse()
    # if the inverse exists, return it.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # if the inverse does not exist, get the matrix, compute the inverse, and cache the inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    # return the inverse value
    i
}
