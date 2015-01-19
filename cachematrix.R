## The purpose of writing the following functions is to cache costly
## calculations to save time. In these functions we try to define
## a matrix and cache its inverse for further use.

## The following function is the main function for this purpose
# it gets the matrix object and provide subset functions to set, get
# both matrix and its inverse. The variable "inverse" in this function
# is a global variable which keeps the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function is a wraper for "makeCacheMatrix"
 # which checks and returns the inverse matrix if existed or
 # calculates it if it not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)){
        print("getting cached data")
        return(inverse)}
        data <- x$get()
        inverse<- solve(data)
        x$setinverse(inverse)
        inverse
    
}
